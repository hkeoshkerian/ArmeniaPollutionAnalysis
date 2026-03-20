library(sf)
library(osmextract)
library(sfnetworks)
library(tidygraph)
library(igraph)
library(dplyr)
library(ggplot2)
library(terra)
library(tidyr)

# ── CONSTANTS ─────────────────────────────────────────────────────────────────
ROAD_BUFFER_M      <- 50
MAX_ROUTES         <- 25
MIN_ROUTE_LENGTH_M <- 2000
MAX_ROUTE_LENGTH_M <- 10000

# ── 1. LOAD ROADS ─────────────────────────────────────────────────────────────
roads_pbf   <- "C:/Users/mrealehatem/Downloads/armenia.pbf"
yerevan_shp <- "C:/Users/mrealehatem/OneDrive/AUA/Air pollution/Data/am_yerevan.shp"
pop_path    <- "C:/Users/mrealehatem/Documents/GitHub/ArmeniaPollutionAnalysis/WorldPopData/arm_age_data/pop.tif"

armenia_roads_oe <- oe_read(roads_pbf, layer = "lines",
                            extra_tags = c("lanes", "maxspeed"))
armenia_roads_oe <- armenia_roads_oe[!is.na(armenia_roads_oe$highway), ]

# ── 2. CROP TO YEREVAN ────────────────────────────────────────────────────────
yerevan <- st_read(yerevan_shp)
if (st_crs(armenia_roads_oe) != st_crs(yerevan))
  yerevan <- st_transform(yerevan, st_crs(armenia_roads_oe))
yerevan_roads <- st_intersection(armenia_roads_oe, yerevan)

# ── 3. SUBSET TO MAJOR ROADS ──────────────────────────────────────────────────
yerevan_roads$lanes <- as.numeric(yerevan_roads$lanes)
routes <- yerevan_roads %>%
  filter(lanes >= 4 | highway %in% c("primary", "trunk"))

# ── 4. PROJECT TO UTM 38N ─────────────────────────────────────────────────────
routes  <- st_transform(routes, 32638)
yerevan <- st_transform(yerevan, 32638)

# ── 5. BUILD SPATIAL NETWORK ──────────────────────────────────────────────────
net <- as_sfnetwork(routes, directed = FALSE) %>%
  activate("edges") %>%
  mutate(weight  = as.numeric(edge_length()),
         edge_id = row_number()) %>%
  activate("nodes") %>%
  filter(group_components() == 1)

# ── 6. EXTRACT EDGE SF + LENGTHS ─────────────────────────────────────────────
edge_sf          <- net %>% activate("edges") %>% st_as_sf()
edge_sf$length_m <- as.numeric(st_length(edge_sf))
total_length_m   <- sum(edge_sf$length_m)
n_edges          <- nrow(edge_sf)

# ── 7. ATTACH POPULATION TO EACH EDGE ────────────────────────────────────────
pop_rast <- rast(pop_path)
pop_rast <- project(pop_rast, "EPSG:32638")

edge_pop_vals <- terra::extract(pop_rast, vect(edge_sf),
                                fun = sum, na.rm = TRUE)

edge_sf$pop <- edge_pop_vals[, 2]
edge_sf$pop[is.na(edge_sf$pop)] <- 0

total_pop <- sum(edge_sf$pop)
message(sprintf("Total population across all edges: %.0f", total_pop))

# ── 8. PRE-COMPUTE CANDIDATE SHORTEST PATHS ───────────────────────────────────
g       <- igraph::as.igraph(net)
n_nodes <- igraph::vcount(g)

set.seed(42)
sample_size   <- min(n_nodes, 80)
sampled_nodes <- sample(igraph::V(g), sample_size)
node_pairs    <- combn(as.integer(sampled_nodes), 2)
n_pairs       <- ncol(node_pairs)
message(sprintf("Computing %d shortest paths...", n_pairs))

all_candidate_paths <- vector("list", n_pairs)
for (i in seq_len(n_pairs)) {
  sp <- igraph::shortest_paths(g,
                               from    = node_pairs[1, i],
                               to      = node_pairs[2, i],
                               output  = "epath",
                               weights = igraph::E(g)$weight)
  eids <- as.integer(sp$epath[[1]])
  if (length(eids) > 0) {
    all_candidate_paths[[i]] <- list(
      from     = node_pairs[1, i],
      to       = node_pairs[2, i],
      eids     = eids,
      length_m = sum(edge_sf$length_m[eids]),
      pop      = sum(edge_sf$pop[eids])
    )
  }
  if (i %% 500 == 0) message(sprintf("  %d / %d done...", i, n_pairs))
}
all_candidate_paths <- Filter(Negate(is.null), all_candidate_paths)
message(sprintf("%d valid paths pre-computed.", length(all_candidate_paths)))

# ── 9. GREEDY COVERAGE FUNCTION ───────────────────────────────────────────────
run_greedy <- function(candidate_paths, n_edges, edge_vals, max_routes, score_by = "pop") {
  covered <- rep(FALSE, n_edges)
  total   <- sum(edge_vals)
  
  for (r in seq_len(max_routes)) {
    if (all(covered)) break
    scores <- sapply(candidate_paths, function(p) {
      sum(edge_vals[p$eids[!covered[p$eids]]])
    })
    best_i <- which.max(scores)
    if (scores[best_i] == 0) break
    covered[candidate_paths[[best_i]]$eids] <- TRUE
  }
  
  100 * sum(edge_vals[covered]) / total
}

# ── 10. 2D SWEEP: route counts (15–30) × max length (5km–20km) ───────────────
route_counts   <- seq(15, 30, by = 2)
max_lengths_km <- seq(5, 20, by = 1)

pop_matrix    <- matrix(NA, nrow = length(max_lengths_km),
                        ncol = length(route_counts))
length_matrix <- matrix(NA, nrow = length(max_lengths_km),
                        ncol = length(route_counts))

for (li in seq_along(max_lengths_km)) {
  max_m <- max_lengths_km[li] * 1000
  
  cands <- Filter(function(p) p$length_m >= MIN_ROUTE_LENGTH_M &&
                    p$length_m <= max_m, all_candidate_paths)
  
  if (length(cands) == 0) next
  
  for (ri in seq_along(route_counts)) {
    pop_matrix[li, ri]    <- run_greedy(cands, n_edges,
                                        edge_sf$pop,      route_counts[ri], "pop")
    length_matrix[li, ri] <- run_greedy(cands, n_edges,
                                        edge_sf$length_m, route_counts[ri], "length")
  }
  message(sprintf("Max length %.1fkm done.", max_lengths_km[li]))
}

# ── 11. RESHAPE FOR GGPLOT ────────────────────────────────────────────────────
make_long <- function(mat, metric_label) {
  df <- as.data.frame(mat)
  colnames(df) <- route_counts
  df$max_km    <- max_lengths_km
  tidyr::pivot_longer(df, cols = -max_km,
                      names_to  = "n_routes",
                      values_to = "coverage_pct") %>%
    mutate(n_routes = as.integer(n_routes),
           metric   = metric_label)
}

heatmap_df <- bind_rows(
  make_long(pop_matrix,    "Population Coverage (%)"),
  make_long(length_matrix, "Network Length Coverage (%)")
)

# ── 12. HEATMAPS ──────────────────────────────────────────────────────────────
ggplot(heatmap_df, aes(x = n_routes, y = max_km, fill = coverage_pct)) +
  geom_tile(colour = "white", linewidth = 0.3) +
  geom_text(aes(label = sprintf("%.0f%%", coverage_pct)),
            size = 2.8, colour = "white", fontface = "bold") +
  # Mark budget-optimal point (25 routes, 10km max)
  annotate("rect", xmin = 24.5, xmax = 25.5, ymin = -Inf, ymax = Inf,
           colour = "red", fill = NA, linewidth = 1) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 9.5, ymax = 10.5,
           colour = "red", fill = NA, linewidth = 1) +
  scale_fill_gradientn(
    colours = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c"),
    name    = "Coverage (%)"
  ) +
  scale_x_continuous(breaks = route_counts) +
  scale_y_continuous(breaks = max_lengths_km,
                     labels = paste0(max_lengths_km, "km")) +
  facet_wrap(~metric, ncol = 1) +
  labs(
    title    = "Route Coverage Heatmap: Population vs. Network Length",
    subtitle = "Red box = budget-optimal (25 routes, 10km max). Yerevan primary/trunk roads.",
    x        = "Number of Routes",
    y        = "Max Route Length Constraint"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(),
        strip.text = element_text(face = "bold", size = 12))

# ── 13. FINAL BEST ROUTES (25 routes, 2–10 km, population-optimised) ─────────
best_cands <- Filter(function(p) p$length_m >= MIN_ROUTE_LENGTH_M &&
                       p$length_m <= MAX_ROUTE_LENGTH_M, all_candidate_paths)
covered    <- rep(FALSE, n_edges)
routes_out <- list()

for (r in seq_len(MAX_ROUTES)) {
  if (all(covered)) break
  scores <- sapply(best_cands, function(p) sum(edge_sf$pop[p$eids[!covered[p$eids]]]))
  best_i <- which.max(scores)
  if (scores[best_i] == 0) break
  covered[best_cands[[best_i]]$eids] <- TRUE
  ro <- best_cands[[best_i]]
  routes_out[[r]] <- list(
    route_id = r, from = ro$from, to = ro$to,
    edge_ids = ro$eids, length_m = ro$length_m, pop = ro$pop,
    new_pop  = scores[best_i]
  )
  message(sprintf("Route %d: %.1fkm | pop covered: %.0f new / %.0f total (%.1f%%)",
                  r, ro$length_m / 1000, scores[best_i],
                  sum(edge_sf$pop[covered]), 100 * sum(edge_sf$pop[covered]) / total_pop))
}

# ── 14. EXTRACT AND MAP FINAL ROUTES ─────────────────────────────────────────
route_geometries <- lapply(routes_out, function(ro) {
  st_sf(route_id = ro$route_id, length_m = ro$length_m, pop = ro$pop,
        geometry = st_union(edge_sf[ro$edge_ids, ]))
})
routes_final_sf <- do.call(rbind, route_geometries)

plot(st_geometry(yerevan), border = "black",
     main = "Population-Optimised Routes (2–10km, 25 routes)")
plot(st_geometry(routes_final_sf),
     col = rainbow(nrow(routes_final_sf))[routes_final_sf$route_id],
     lwd = 2.5, add = TRUE)
legend("bottomleft",
       legend = paste0("R", routes_final_sf$route_id,
                       " (", round(routes_final_sf$pop / 1000, 1), "k pop)"),
       col    = rainbow(nrow(routes_final_sf)), lwd = 2, cex = 0.45)

# ── 15. EXPORT: 25 routes × 2 directions = 50 rows ───────────────────────────

EXPORT_MIN_KM   <- 2
EXPORT_MAX_KM   <- 10
EXPORT_N_ROUTES <- 25
N_WAYPOINTS     <- 9

export_cands <- Filter(function(p) p$length_m >= EXPORT_MIN_KM * 1000 &&
                         p$length_m <= EXPORT_MAX_KM * 1000, all_candidate_paths)

covered_exp   <- rep(FALSE, n_edges)
export_routes <- list()

for (r in seq_len(EXPORT_N_ROUTES)) {
  if (all(covered_exp)) break
  scores <- sapply(export_cands, function(p) sum(edge_sf$pop[p$eids[!covered_exp[p$eids]]]))
  best_i <- which.max(scores)
  if (scores[best_i] == 0) break
  covered_exp[export_cands[[best_i]]$eids] <- TRUE
  ro <- export_cands[[best_i]]
  export_routes[[r]] <- list(
    route_id = r,
    edge_ids = ro$eids,
    length_m = ro$length_m,
    pop      = ro$pop
  )
}

# ── EXTRACT GEOMETRY, WAYPOINTS, AND REVERSE DIRECTION ───────────────────────
route_export_rows <- lapply(export_routes, function(ro) {
  
  route_line <- edge_sf[ro$edge_ids, ] %>%
    st_union() %>%
    st_line_merge()
  
  if (st_geometry_type(route_line)[1] == "MULTILINESTRING") {
    parts      <- st_cast(st_sf(geometry = route_line), "LINESTRING")
    parts$len  <- as.numeric(st_length(parts))
    route_line <- parts$geometry[which.max(parts$len)]
  }
  
  route_line     <- st_cast(route_line, "LINESTRING")
  route_length_m <- as.numeric(st_length(route_line))
  
  if (!is.finite(route_length_m) || route_length_m == 0) {
    warning(sprintf("Route %d has zero/invalid length, skipping.", ro$route_id))
    return(NULL)
  }
  
  n_total_pts <- N_WAYPOINTS + 2
  distances   <- seq(0, route_length_m, length.out = n_total_pts)```r
  library(sf)
  library(osmextract)
  library(sfnetworks)
  library(tidygraph)
  library(igraph)
  library(dplyr)
  library(ggplot2)
  library(terra)
  library(tidyr)
  
  # ── CONSTANTS ─────────────────────────────────────────────────────────────────
  ROAD_BUFFER_M      <- 50
  MAX_ROUTES         <- 25
  MIN_ROUTE_LENGTH_M <- 2000    # 2 km minimum
  MAX_ROUTE_LENGTH_M <- 10000   # 10 km maximum
  
  # ── 1. LOAD ROADS ─────────────────────────────────────────────────────────────
  roads_pbf   <- "C:/Users/mrealehatem/Downloads/armenia.pbf"
  yerevan_shp <- "C:/Users/mrealehatem/OneDrive/AUA/Air pollution/Data/am_yerevan.shp"
  pop_path    <- "C:/Users/mrealehatem/Documents/GitHub/ArmeniaPollutionAnalysis/WorldPopData/arm_age_data/pop.tif"
  
  armenia_roads_oe <- oe_read(roads_pbf, layer = "lines",
                              extra_tags = c("lanes", "maxspeed"))
  armenia_roads_oe <- armenia_roads_oe[!is.na(armenia_roads_oe$highway), ]
  
  # ── 2. CROP TO YEREVAN ────────────────────────────────────────────────────────
  yerevan <- st_read(yerevan_shp)
  if (st_crs(armenia_roads_oe) != st_crs(yerevan))
    yerevan <- st_transform(yerevan, st_crs(armenia_roads_oe))
  yerevan_roads <- st_intersection(armenia_roads_oe, yerevan)
  
  # ── 3. SUBSET TO MAJOR ROADS ──────────────────────────────────────────────────
  yerevan_roads$lanes <- as.numeric(yerevan_roads$lanes)
  routes <- yerevan_roads %>%
    filter(lanes >= 4 | highway %in% c("primary", "trunk"))
  
  # ── 4. PROJECT TO UTM 38N ────────────────────────────────────────────────────
  routes  <- st_transform(routes, 32638)
  yerevan <- st_transform(yerevan, 32638)
  
  # ── 5. BUILD SPATIAL NETWORK ──────────────────────────────────────────────────
  net <- as_sfnetwork(routes, directed = FALSE) %>%
    activate("edges") %>%
    mutate(weight  = as.numeric(edge_length()),
           edge_id = row_number()) %>%
    activate("nodes") %>%
    filter(group_components() == 1)
  
  # ── 6. EXTRACT EDGE SF + LENGTHS ─────────────────────────────────────────────
  edge_sf          <- net %>% activate("edges") %>% st_as_sf()
  edge_sf$length_m <- as.numeric(st_length(edge_sf))
  total_length_m   <- sum(edge_sf$length_m)
  n_edges          <- nrow(edge_sf)
  
  # ── 7. ATTACH POPULATION TO EACH EDGE ────────────────────────────────────────
  pop_rast <- rast(pop_path)
  pop_rast <- project(pop_rast, "EPSG:32638")
  
  edge_pop_vals <- terra::extract(pop_rast, vect(edge_sf),
                                  fun = sum, na.rm = TRUE)
  edge_sf$pop <- edge_pop_vals[, 2]
  edge_sf$pop[is.na(edge_sf$pop)] <- 0
  
  total_pop <- sum(edge_sf$pop)
  message(sprintf("Total population across all edges: %.0f", total_pop))
  
  # ── 8. PRE-COMPUTE CANDIDATE SHORTEST PATHS ──────────────────────────────────
  g       <- igraph::as.igraph(net)
  n_nodes <- igraph::vcount(g)
  
  set.seed(42)
  sample_size   <- min(n_nodes, 80)
  sampled_nodes <- sample(igraph::V(g), sample_size)
  node_pairs    <- combn(as.integer(sampled_nodes), 2)
  n_pairs       <- ncol(node_pairs)
  message(sprintf("Computing %d shortest paths...", n_pairs))
  
  all_candidate_paths <- vector("list", n_pairs)
  for (i in seq_len(n_pairs)) {
    sp <- igraph::shortest_paths(g,
                                 from    = node_pairs[1, i],
                                 to      = node_pairs[2, i],
                                 output  = "epath",
                                 weights = igraph::E(g)$weight)
    eids <- as.integer(sp$epath[])[1]
    if (length(eids) > 0) {
      all_candidate_paths[[i]] <- list(
        from     = node_pairs[1, i],
        to       = node_pairs[2, i],
        eids     = eids,
        length_m = sum(edge_sf$length_m[eids]),
        pop      = sum(edge_sf$pop[eids])
      )
    }
    if (i %% 500 == 0) message(sprintf("  %d / %d done...", i, n_pairs))
  }
  all_candidate_paths <- Filter(Negate(is.null), all_candidate_paths)
  message(sprintf("%d valid paths pre-computed.", length(all_candidate_paths)))
  
  # ── 9. GREEDY COVERAGE FUNCTION ───────────────────────────────────────────────
  run_greedy <- function(candidate_paths, n_edges, edge_vals, max_routes, score_by = "pop") {
    covered <- rep(FALSE, n_edges)
    total   <- sum(edge_vals)
    
    for (r in seq_len(max_routes)) {
      if (all(covered)) break
      scores <- sapply(candidate_paths, function(p) {
        sum(edge_vals[p$eids[!covered[p$eids]]])
      })
      best_i <- which.max(scores)
      if (scores[best_i] == 0) break
      covered[candidate_paths[[best_i]]$eids] <- TRUE
    }
    
    100 * sum(edge_vals[covered]) / total
  }
  
  # ── 10. 2D SWEEP: route counts (15–30) × max length (5km–20km) ───────────────
  route_counts   <- seq(15, 30, by = 2)
  max_lengths_km <- seq(5, 20, by = 1)
  
  pop_matrix    <- matrix(NA, nrow = length(max_lengths_km),
                          ncol = length(route_counts))
  length_matrix <- matrix(NA, nrow = length(max_lengths_km),
                          ncol = length(route_counts))
  
  for (li in seq_along(max_lengths_km)) {
    max_m <- max_lengths_km[li] * 1000
    
    cands <- Filter(function(p) p$length_m >= MIN_ROUTE_LENGTH_M &&
                      p$length_m <= max_m, all_candidate_paths)
    
    if (length(cands) == 0) next
    
    for (ri in seq_along(route_counts)) {
      pop_matrix[li, ri]    <- run_greedy(cands, n_edges,
                                          edge_sf$pop,      route_counts[ri], "pop")
      length_matrix[li, ri] <- run_greedy(cands, n_edges,
                                          edge_sf$length_m, route_counts[ri], "length")
    }
    message(sprintf("Max length %.1fkm done.", max_lengths_km[li]))
  }
  
  # ── 11. RESHAPE FOR GGPLOT ────────────────────────────────────────────────────
  make_long <- function(mat, metric_label) {
    df <- as.data.frame(mat)
    colnames(df) <- route_counts
    df$max_km    <- max_lengths_km
    tidyr::pivot_longer(df, cols = -max_km,
                        names_to  = "n_routes",
                        values_to = "coverage_pct") %>%
      mutate(n_routes = as.integer(n_routes),
             metric   = metric_label)
  }
  
  heatmap_df <- bind_rows(
    make_long(pop_matrix,    "Population Coverage (%)"),
    make_long(length_matrix, "Network Length Coverage (%)")
  )
  
  # ── 12. HEATMAPS ──────────────────────────────────────────────────────────────
  ggplot(heatmap_df, aes(x = n_routes, y = max_km, fill = coverage_pct)) +
    geom_tile(colour = "white", linewidth = 0.3) +
    geom_text(aes(label = sprintf("%.0f%%", coverage_pct)),
              size = 2.8, colour = "white", fontface = "bold") +
    # Mark budget-optimal point (25 routes, 10km max)
    annotate("rect", xmin = 24.5, xmax = 25.5, ymin = -Inf, ymax = Inf,
             colour = "red", fill = NA, linewidth = 1) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 9.5, ymax = 10.5,
             colour = "red", fill = NA, linewidth = 1) +
    scale_fill_gradientn(
      colours = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c"),
      name    = "Coverage (%)"
    ) +
    scale_x_continuous(breaks = route_counts) +
    scale_y_continuous(breaks = max_lengths_km,
                       labels = paste0(max_lengths_km, "km")) +
    facet_wrap(~metric, ncol = 1) +
    labs(
      title    = "Route Coverage Heatmap: Population vs. Network Length",
      subtitle = "Red box = budget-optimal (25 routes, 10km max). Yerevan primary/trunk roads.",
      x        = "Number of Routes",
      y        = "Max Route Length Constraint"
    ) +
    theme_minimal(base_size = 12) +
    theme(panel.grid = element_blank(),
          strip.text = element_text(face = "bold", size = 12))
  
  # ── 13. FINAL BEST ROUTES (25 routes, 2–10 km, population-optimised) ─────────
  best_cands <- Filter(function(p) p$length_m >= MIN_ROUTE_LENGTH_M &&
                         p$length_m <= MAX_ROUTE_LENGTH_M, all_candidate_paths)
  covered    <- rep(FALSE, n_edges)
  routes_out <- list()
  
  for (r in seq_len(MAX_ROUTES)) {
    if (all(covered)) break
    scores <- sapply(best_cands, function(p) sum(edge_sf$pop[p$eids[!covered[p$eids]]]))
    best_i <- which.max(scores)
    if (scores[best_i] == 0) break
    covered[best_cands[[best_i]]$eids] <- TRUE
    ro <- best_cands[[best_i]]
    routes_out[[r]] <- list(
      route_id = r, from = ro$from, to = ro$to,
      edge_ids = ro$eids, length_m = ro$length_m, pop = ro$pop,
      new_pop  = scores[best_i]
    )
    message(sprintf("Route %d: %.1fkm | pop covered: %.0f new / %.0f total (%.1f%%)",
                    r, ro$length_m / 1000, scores[best_i],
                    sum(edge_sf$pop[covered]), 100 * sum(edge_sf$pop[covered]) / total_pop))
  }
  
  # ── 14. EXTRACT AND MAP FINAL ROUTES ─────────────────────────────────────────
  route_geometries <- lapply(routes_out, function(ro) {
    st_sf(route_id = ro$route_id, length_m = ro$length_m, pop = ro$pop,
          geometry = st_union(edge_sf[ro$edge_ids, ]))
  })
  routes_final_sf <- do.call(rbind, route_geometries)
  
  plot(st_geometry(yerevan), border = "black",
       main = "Population-Optimised Routes (2–10km, 25 routes)")
  plot(st_geometry(routes_final_sf),
       col = rainbow(nrow(routes_final_sf))[routes_final_sf$route_id],
       lwd = 2.5, add = TRUE)
  legend("bottomleft",
         legend = paste0("R", routes_final_sf$route_id,
                         " (", round(routes_final_sf$pop / 1000, 1), "k pop)"),
         col = rainbow(nrow(routes_final_sf)), lwd = 2, cex = 0.45)
  
  # ── 15. EXPORT: 25 routes × 2 directions = 50 rows ───────────────────────────
  
  EXPORT_MIN_KM   <- 2
  EXPORT_MAX_KM   <- 12
  EXPORT_N_ROUTES <- 25
  N_WAYPOINTS     <- 10
  
  export_cands <- Filter(function(p) p$length_m >= EXPORT_MIN_KM * 1000 &&
                           p$length_m <= EXPORT_MAX_KM * 1000, all_candidate_paths)
  
  covered_exp   <- rep(FALSE, n_edges)
  export_routes <- list()
  
  for (r in seq_len(EXPORT_N_ROUTES)) {
    if (all(covered_exp)) break
    scores <- sapply(export_cands, function(p) sum(edge_sf$pop[p$eids[!covered_exp[p$eids]]]))
    best_i <- which.max(scores)
    if (scores[best_i] == 0) break
    covered_exp[export_cands[[best_i]]$eids] <- TRUE
    ro <- export_cands[[best_i]]
    export_routes[[r]] <- list(
      route_id = r,
      edge_ids = ro$eids,
      length_m = ro$length_m,
      pop      = ro$pop
    )
  }
  
  # ── EXTRACT GEOMETRY, WAYPOINTS, AND REVERSE ─────────────────────────────────
  route_export_rows <- lapply(export_routes, function(ro) {
    
    route_line <- edge_sf[ro$edge_ids, ] %>%
      st_union() %>%
      st_line_merge()
    
    if (st_geometry_type(route_line)[1] == "MULTILINESTRING") {   # ← fixed: [1] inside condition
      parts      <- st_cast(st_sf(geometry = route_line), "LINESTRING")
      parts$len  <- as.numeric(st_length(parts))
      route_line <- parts$geometry[which.max(parts$len)]
    }
    
    route_line     <- st_cast(route_line, "LINESTRING")
    route_length_m <- as.numeric(st_length(route_line))
    
    if (!is.finite(route_length_m) || route_length_m == 0) {
      warning(sprintf("Route %d has zero/invalid length, skipping.", ro$route_id))
      return(NULL)
    }
    
    n_total_pts <- N_WAYPOINTS + 2
    distances   <- seq(0, route_length_m, length.out = n_total_pts)
    sampled_pts <- st_line_sample(route_line, sample = distances / route_length_m)
    sampled_pts <- st_cast(sampled_pts, "POINT", warn = FALSE)
    
    if (length(sampled_pts) < n_total_pts) {
      warning(sprintf("Route %d returned only %d points (expected %d), skipping.",
                      ro$route_id, length(sampled_pts), n_total_pts))
      return(NULL)
    }
    
    sampled_pts <- st_transform(sampled_pts, 4326)
    coords      <- st_coordinates(sampled_pts)
    
    # ── Forward row ───────────────────────────────────────────────────────────
    row_fwd <- data.frame(
      route_id    = ro$route_id,
      direction   = "forward",
      length_km   = round(ro$length_m / 1000, 3),
      pop_covered = round(ro$pop, 0),
      route_wkt   = st_as_text(st_transform(route_line, 4326)),
      start_lat   = round(coords[1,            "Y"], 6),
      start_lon   = round(coords[1,            "X"], 6),
      end_lat     = round(coords[nrow(coords), "Y"], 6),
      end_lon     = round(coords[nrow(coords), "X"], 6),
      stringsAsFactors = FALSE
    )
    wp_fwd <- coords[2:(N_WAYPOINTS + 1), , drop = FALSE]
    for (w in seq_len(N_WAYPOINTS)) {
      row_fwd[[paste0("wp", w, "_lat")]] <- round(wp_fwd[w, "Y"], 6)
      row_fwd[[paste0("wp", w, "_lon")]] <- round(wp_fwd[w, "X"], 6)
    }
    
    # ── Reverse row ───────────────────────────────────────────────────────────
    coords_rev <- coords[nrow(coords):1, , drop = FALSE]
    
    row_rev <- data.frame(
      route_id    = ro$route_id,
      direction   = "reverse",
      length_km   = round(ro$length_m / 1000, 3),
      pop_covered = round(ro$pop, 0),
      route_wkt   = st_as_text(st_transform(st_reverse(route_line), 4326)),
      start_lat   = round(coords_rev[1,                "Y"], 6),
      start_lon   = round(coords_rev[1,                "X"], 6),
      end_lat     = round(coords_rev[nrow(coords_rev), "Y"], 6),
      end_lon     = round(coords_rev[nrow(coords_rev), "X"], 6),
      stringsAsFactors = FALSE
    )
    wp_rev <- coords_rev[2:(N_WAYPOINTS + 1), , drop = FALSE]
    for (w in seq_len(N_WAYPOINTS)) {
      row_rev[[paste0("wp", w, "_lat")]] <- round(wp_rev[w, "Y"], 6)
      row_rev[[paste0("wp", w, "_lon")]] <- round(wp_rev[w, "X"], 6)
    }
    
    rbind(row_fwd, row_rev)
  })
  
  route_export_rows <- Filter(Negate(is.null), route_export_rows)
  routes_export_df  <- do.call(rbind, route_export_rows)
  rownames(routes_export_df) <- NULL
  
  # ── PREVIEW ───────────────────────────────────────────────────────────────────
  cat(sprintf("Exported %d rows (%d routes x 2 directions, %.0fkm-%.0fkm)\n",
              nrow(routes_export_df), EXPORT_N_ROUTES, EXPORT_MIN_KM, EXPORT_MAX_KM))
  print(routes_export_df[, c("route_id", "direction", "length_km", "pop_covered",
                             "start_lat", "start_lon", "end_lat", "end_lon")])
  
  # ── PLOT EXPORTED ROUTES ──────────────────────────────────────────────────────
  plot(st_geometry(yerevan),
       border = "black", lwd = 1.5, col = "lightgrey",
       main   = sprintf("Population-Optimised Routes (n=%d, %.0fkm \u2013 %.0fkm)",
                        EXPORT_N_ROUTES, EXPORT_MIN_KM, EXPORT_MAX_KM),
       axes = FALSE, key.pos = NULL)
  
  plot(st_geometry(routes_final_sf),
       col = RColorBrewer::brewer.pal(9, "Set1")[routes_final_sf$route_id %% 9 + 1],
       lwd = 3.5, add = TRUE)
  
  text_coords       <- st_centroid(routes_final_sf)
  text_coords$label <- paste0(routes_final_sf$route_id,
                              "\n(", round(routes_final_sf$pop / 1000, 0), "k)")
  text(st_coordinates(text_coords),
       labels = text_coords$label, cex = 0.75, font = 2, col = "white")
  
  legend("bottomleft",
         legend = paste0("Route ", routes_final_sf$route_id,
                         " (", round(routes_final_sf$length_m / 1000, 1), "km)"),
         col    = RColorBrewer::brewer.pal(9, "Set1")[routes_final_sf$route_id %% 9 + 1],
         lwd = 3, cex = 0.7)
  
  coverage_pop_pct <- 100 * sum(routes_final_sf$pop) / total_pop
  coverage_len_pct <- 100 * sum(routes_final_sf$length_m) / total_length_m
  title(subtitle = sprintf("Pop: %.1f%% | Length: %.1f%% | Total pop: %.0f | Total length: %.0fm",
                           coverage_pop_pct, coverage_len_pct,
                           sum(routes_final_sf$pop), sum(routes_final_sf$length_m)),
        line = -2, cex.sub = 0.9, col.sub = "darkblue")
  
  # ── EXPORT TO CSV ─────────────────────────────────────────────────────────────
  export_path <- sprintf(
    "C:\\Users\\mrealehatem\\Documents\\GitHub\\ArmeniaPollutionAnalysis\\Route Network Optimizing\\yerevan_routes_%dkm_%dkm_%droutes_bidirectional.csv",
    EXPORT_MIN_KM, EXPORT_MAX_KM, EXPORT_N_ROUTES
  )
  write.csv(routes_export_df, export_path, row.names = FALSE)
  message(sprintf("Saved to: %s", export_path))
  
  
  # ── EXPORT PLOTS TO PNG ───────────────────────────────────────────────────────
  plot_dir <- "C:\\Users\\mrealehatem\\Documents\\GitHub\\ArmeniaPollutionAnalysis\\Route Network Optimizing"
  
  # ── Plot 1: Heatmap ───────────────────────────────────────────────────────────
  heatmap_path <- file.path(plot_dir, "heatmap_coverage.png")
  ggsave(
    filename = heatmap_path,
    plot = ggplot(heatmap_df, aes(x = n_routes, y = max_km, fill = coverage_pct)) +
      geom_tile(colour = "white", linewidth = 0.3) +
      geom_text(aes(label = sprintf("%.0f%%", coverage_pct)),
                size = 2.8, colour = "white", fontface = "bold") +
      annotate("rect", xmin = 24.5, xmax = 25.5, ymin = -Inf, ymax = Inf,
               colour = "red", fill = NA, linewidth = 1) +
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = 9.5, ymax = 10.5,
               colour = "red", fill = NA, linewidth = 1) +
      scale_fill_gradientn(
        colours = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c"),
        name    = "Coverage (%)"
      ) +
      scale_x_continuous(breaks = route_counts) +
      scale_y_continuous(breaks = max_lengths_km,
                         labels = paste0(max_lengths_km, "km")) +
      facet_wrap(~metric, ncol = 1) +
      labs(
        title    = "Route Coverage Heatmap: Population vs. Network Length",
        subtitle = sprintf("Red box = budget-optimal (25 routes, 10km max). Yerevan primary/trunk roads."),
        x        = "Number of Routes",
        y        = "Max Route Length Constraint"
      ) +
      theme_minimal(base_size = 12) +
      theme(panel.grid = element_blank(),
            strip.text = element_text(face = "bold", size = 12)),
    width  = 12,
    height = 14,
    dpi    = 200,
    units  = "in"
  )
  message(sprintf("Heatmap saved to: %s", heatmap_path))
  
  # ── Plot 2: Greedy selection map (Section 14) ─────────────────────────────────
  greedy_map_path <- file.path(plot_dir, "routes_greedy_map.png")
  png(greedy_map_path, width = 2400, height = 2400, res = 200)
  plot(st_geometry(yerevan), border = "black",
       main = "Population-Optimised Routes (2–10km, 25 routes)")
  plot(st_geometry(routes_final_sf),
       col = rainbow(nrow(routes_final_sf))[routes_final_sf$route_id],
       lwd = 2.5, add = TRUE)
  legend("bottomleft",
         legend = paste0("R", routes_final_sf$route_id,
                         " (", round(routes_final_sf$pop / 1000, 1), "k pop)"),
         col    = rainbow(nrow(routes_final_sf)), lwd = 2, cex = 0.45)
  dev.off()
  message(sprintf("Greedy map saved to: %s", greedy_map_path))
  
  # ── Plot 3: Final exported routes map (Section 15) ───────────────────────────
  export_map_path <- file.path(plot_dir,
                               sprintf("routes_export_map_%dkm_%dkm_%droutes.png",
                                       EXPORT_MIN_KM, EXPORT_MAX_KM, EXPORT_N_ROUTES))
  png(export_map_path, width = 2400, height = 2400, res = 200)
  plot(st_geometry(yerevan),
       border = "black", lwd = 1.5, col = "lightgrey",
       main   = sprintf("Population-Optimised Routes (n=%d, %dkm \u2013 %dkm)",
                        EXPORT_N_ROUTES, EXPORT_MIN_KM, EXPORT_MAX_KM),
       axes = FALSE, key.pos = NULL)
  plot(st_geometry(routes_final_sf),
       col = RColorBrewer::brewer.pal(9, "Set1")[routes_final_sf$route_id %% 9 + 1],
       lwd = 3.5, add = TRUE)
  text_coords       <- st_centroid(routes_final_sf)
  text_coords$label <- paste0(routes_final_sf$route_id,
                              "\n(", round(routes_final_sf$pop / 1000, 0), "k)")
  text(st_coordinates(text_coords),
       labels = text_coords$label, cex = 0.75, font = 2, col = "white")
  legend("bottomleft",
         legend = paste0("Route ", routes_final_sf$route_id,
                         " (", round(routes_final_sf$length_m / 1000, 1), "km)"),
         col    = RColorBrewer::brewer.pal(9, "Set1")[routes_final_sf$route_id %% 9 + 1],
         lwd = 3, cex = 0.7)
  coverage_pop_pct <- 100 * sum(routes_final_sf$pop) / total_pop
  coverage_len_pct <- 100 * sum(routes_final_sf$length_m) / total_length_m
  title(subtitle = sprintf("Pop: %.1f%% | Length: %.1f%% | Total pop: %.0f | Total length: %.0fm",
                           coverage_pop_pct, coverage_len_pct,
                           sum(routes_final_sf$pop), sum(routes_final_sf$length_m)),
        line = -2, cex.sub = 0.9, col.sub = "darkblue")
  dev.off()
  message(sprintf("Export map saved to: %s", export_map_path))
  
  
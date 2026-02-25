library(sf)
library(osmextract)
library(sfnetworks)
library(tidygraph)
library(igraph)
library(dplyr)
library(ggplot2)
library(terra)

# ── CONSTANTS ─────────────────────────────────────────────────────────────────
ROAD_BUFFER_M  <- 50     # buffer around each edge to sample population (metres)
MAX_ROUTES     <- 38     # budget-optimal number of routes

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

# ── 4. PROJECT TO UTM 38N FOR METRE-ACCURATE MEASUREMENTS ────────────────────
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
# Load population raster and reproject to match UTM 38N
pop_rast <- rast(pop_path)
pop_rast <- project(pop_rast, "EPSG:32638")

# Extract population cells directly intersected by each edge line
# fun = sum gives total population of all 100m cells the road passes through
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

# Build candidate paths — store length_m AND pop for each
# NOTE: filtering by max_length happens in the sweep loop (varies 4km–8km)
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
# score_by: "pop" or "length_m"
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

# ── 10. 2D SWEEP: route counts (20–40) × max length (4km–8km) ────────────────
route_counts   <- seq(20, 40, by = 2)
max_lengths_km <- seq(4, 8, by = 0.5)

# Initialise result matrices
pop_matrix    <- matrix(NA, nrow = length(max_lengths_km),
                        ncol = length(route_counts))
length_matrix <- matrix(NA, nrow = length(max_lengths_km),
                        ncol = length(route_counts))

for (li in seq_along(max_lengths_km)) {
  max_m <- max_lengths_km[li] * 1000
  
  # Filter candidate paths to those within the current length constraint
  cands <- Filter(function(p) p$length_m <= max_m, all_candidate_paths)
  
  if (length(cands) == 0) next
  
  for (ri in seq_along(route_counts)) {
    pop_matrix[li, ri] <- run_greedy(cands, n_edges,
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

library(tidyr)
heatmap_df <- bind_rows(
  make_long(pop_matrix,    "Population Coverage (%)"),
  make_long(length_matrix, "Network Length Coverage (%)")
)

# ── 12. HEATMAPS ──────────────────────────────────────────────────────────────
ggplot(heatmap_df, aes(x = n_routes, y = max_km, fill = coverage_pct)) +
  geom_tile(colour = "white", linewidth = 0.3) +
  geom_text(aes(label = sprintf("%.0f%%", coverage_pct)),
            size = 2.8, colour = "white", fontface = "bold") +
  # Mark budget-optimal point on both panels
  annotate("rect", xmin = 37.5, xmax = 38.5, ymin = -Inf, ymax = Inf,
           colour = "red", fill = NA, linewidth = 1) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 5.75, ymax = 6.25,
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
    subtitle = "Red box = budget-optimal (38 routes, 6km max). Yerevan primary/trunk roads.",
    x        = "Number of Routes",
    y        = "Max Route Length Constraint"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(),
        strip.text = element_text(face = "bold", size = 12))

# ── 13. FINAL BEST ROUTES (38 routes, 6km, population-optimised) ─────────────
best_cands  <- Filter(function(p) p$length_m <= 6000, all_candidate_paths)
covered     <- rep(FALSE, n_edges)
routes_out  <- list()

for (r in seq_len(MAX_ROUTES)) {
  if (all(covered)) break
  scores  <- sapply(best_cands, function(p) sum(edge_sf$pop[p$eids[!covered[p$eids]]]))
  best_i  <- which.max(scores)
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
     main = "Population-Optimised Routes (≤ 6km, 38 routes)")
plot(st_geometry(routes_final_sf),
     col = rainbow(nrow(routes_final_sf))[routes_final_sf$route_id],
     lwd = 2.5, add = TRUE)
legend("bottomleft",
       legend = paste0("R", routes_final_sf$route_id,
                       " (", round(routes_final_sf$pop / 1000, 1), "k pop)"),
       col    = rainbow(nrow(routes_final_sf)), lwd = 2, cex = 0.45)

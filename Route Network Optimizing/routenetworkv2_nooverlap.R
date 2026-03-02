library(sf)
library(osmextract)
library(sfnetworks)
library(tidygraph)
library(igraph)
library(dplyr)
library(ggplot2)
library(terra)
library(tidyr)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)
library(Matrix)             
library(slam)

# ── CONSTANTS ─────────────────────────────────────────────────────────────────
MAX_ROUTES         <- 38
MIN_ROUTE_LENGTH_M <- 3000

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

edge_pop_vals <- terra::extract(pop_rast, vect(edge_sf), fun = sum, na.rm = TRUE)
edge_sf$pop   <- edge_pop_vals[, 2]
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

# ── 8b. PRE-BUILD FULL SPARSE INCIDENCE MATRIX (once, for all candidates) ────
# A_full[e, p] = 1 if edge e is used by path p
# Built once here — subsetted cheaply inside run_ilp() for each sweep cell
message("Building full sparse incidence matrix...")
n_all <- length(all_candidate_paths)

# Collect (edge, path) index pairs for sparse matrix
row_idx <- unlist(lapply(seq_len(n_all), function(p) all_candidate_paths[[p]]$eids))
col_idx <- unlist(lapply(seq_len(n_all), function(p) rep(p, length(all_candidate_paths[[p]]$eids))))
A_full  <- sparseMatrix(i = row_idx, j = col_idx,
                        dims = c(n_edges, n_all), x = 1)
message("Sparse incidence matrix built.")

# ── 9. ILP SOLVER FUNCTION (sparse matrix + ROI directly, no ompr loop) ───────
# ── 9. ILP SOLVER FUNCTION ────────────────────────────────────────────────────
library(slam)   # ← add to library block at top too

run_ilp <- function(candidate_paths, n_edges, edge_vals, max_routes,
                    cand_idx = seq_along(candidate_paths)) {
  n_cands <- length(cand_idx)
  pop_vec <- sapply(candidate_paths, `[[`, "pop")
  total   <- sum(edge_vals)
  
  # Subset pre-built sparse incidence matrix to current candidates
  A_sub          <- A_full[, cand_idx, drop = FALSE]
  edge_use_count <- rowSums(A_sub)
  conflict_edges <- which(edge_use_count >= 2)
  A_conflict     <- A_sub[conflict_edges, , drop = FALSE]
  
  message(sprintf("  ILP: %d candidates, %d conflict edges", n_cands, length(conflict_edges)))
  
  # ── Convert to slam simple_triplet_matrix (required by ROI) ──────────────
  # Budget row: all 1s
  budget_stm <- slam::simple_triplet_matrix(
    i    = rep(1L, n_cands),
    j    = seq_len(n_cands),
    v    = rep(1, n_cands),
    nrow = 1L,
    ncol = n_cands
  )
  
  # Conflict rows: convert dgCMatrix → slam triplet
  cx <- as(A_conflict, "TsparseMatrix")   # COO format: i, j, x slots
  conflict_stm <- slam::simple_triplet_matrix(
    i    = as.integer(cx@i) + 1L,         # 0-indexed → 1-indexed
    j    = as.integer(cx@j) + 1L,
    v    = as.numeric(cx@x),
    nrow = length(conflict_edges),
    ncol = n_cands
  )
  
  # Stack budget + conflict into one constraint matrix
  A_roi <- rbind(budget_stm, conflict_stm)
  rhs   <- c(max_routes, rep(1, length(conflict_edges)))
  dir   <- rep("<=", length(rhs))
  
  # ── Build and solve ROI problem ───────────────────────────────────────────
  prob <- ROI::OP(
    objective   = ROI::L_objective(-pop_vec),   # negate: ROI minimises
    constraints = ROI::L_constraint(A_roi, dir = dir, rhs = rhs),
    bounds      = ROI::V_bound(
      li = seq_len(n_cands), ui = seq_len(n_cands),
      lb = rep(0, n_cands),  ub = rep(1, n_cands)),
    types       = rep("B", n_cands),
    maximum     = FALSE
  )
  
  result   <- ROI::ROI_solve(prob, solver = "glpk",
                             control = list(verbose = FALSE, presolve = TRUE,
                                            tm_limit = 120000))
  x_vals   <- result$solution
  selected <- which(x_vals > 0.5)
  
  covered <- rep(FALSE, n_edges)
  for (idx in selected) covered[candidate_paths[[idx]]$eids] <- TRUE
  
  list(
    coverage_pct = 100 * sum(edge_vals[covered]) / total,
    selected_idx = selected
  )
}


# ── 10. 2D SWEEP: route counts (20–40) × max length (4km–12km) ───────────────
route_counts   <- seq(20, 40, by = 2)
max_lengths_km <- seq(4, 12, by = 0.5)

pop_matrix    <- matrix(NA, nrow = length(max_lengths_km), ncol = length(route_counts))
length_matrix <- matrix(NA, nrow = length(max_lengths_km), ncol = length(route_counts))

for (li in seq_along(max_lengths_km)) {
  max_m <- max_lengths_km[li] * 1000
  
  # Get indices into all_candidate_paths (for fast matrix subsetting)
  cand_idx <- which(sapply(all_candidate_paths, function(p)
    p$length_m >= MIN_ROUTE_LENGTH_M && p$length_m <= max_m))
  
  if (length(cand_idx) == 0) next
  cands <- all_candidate_paths[cand_idx]
  
  for (ri in seq_along(route_counts)) {
    pop_matrix[li, ri]    <- run_ilp(cands, n_edges, edge_sf$pop,
                                     route_counts[ri], cand_idx)$coverage_pct
    length_matrix[li, ri] <- run_ilp(cands, n_edges, edge_sf$length_m,
                                     route_counts[ri], cand_idx)$coverage_pct
  }
  message(sprintf("Max length %.1fkm done.", max_lengths_km[li]))
}

# ── 11. RESHAPE FOR GGPLOT ────────────────────────────────────────────────────
make_long <- function(mat, metric_label) {
  df <- as.data.frame(mat)
  colnames(df) <- route_counts
  df$max_km    <- max_lengths_km
  pivot_longer(df, cols = -max_km,
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

# ── 13. FINAL BEST ROUTES via ILP (38 routes, 6km) ───────────────────────────
best_idx <- which(sapply(all_candidate_paths, function(p)
  p$length_m >= MIN_ROUTE_LENGTH_M && p$length_m <= 6000))
best_cands <- all_candidate_paths[best_idx]

message(sprintf("Running ILP on %d candidates for final route selection...",
                length(best_cands)))

ilp_result   <- run_ilp(best_cands, n_edges, edge_sf$pop, MAX_ROUTES, best_idx)
selected_idx <- ilp_result$selected_idx

routes_out <- lapply(seq_along(selected_idx), function(r) {
  ro <- best_cands[[selected_idx[r]]]
  list(route_id = r, from = ro$from, to = ro$to,
       edge_ids = ro$eids, length_m = ro$length_m, pop = ro$pop, new_pop = ro$pop)
})

message(sprintf("ILP selected %d routes. Population coverage: %.1f%%",
                length(routes_out), ilp_result$coverage_pct))

# ── 14. EXTRACT AND MAP FINAL ROUTES ─────────────────────────────────────────
route_geometries <- lapply(routes_out, function(ro) {
  st_sf(route_id = ro$route_id, length_m = ro$length_m, pop = ro$pop,
        geometry = st_union(edge_sf[ro$edge_ids, ]))
})
routes_final_sf <- do.call(rbind, route_geometries)

plot(st_geometry(yerevan), border = "black",
     main = "ILP Population-Optimised Routes (≤ 6km, no overlap)")
plot(st_geometry(routes_final_sf),
     col = rainbow(nrow(routes_final_sf))[routes_final_sf$route_id],
     lwd = 2.5, add = TRUE)
legend("bottomleft",
       legend = paste0("R", routes_final_sf$route_id,
                       " (", round(routes_final_sf$pop / 1000, 1), "k pop)"),
       col    = rainbow(nrow(routes_final_sf)), lwd = 2, cex = 0.45)

# ── ADJUSTABLE EXPORT PARAMETERS ─────────────────────────────────────────────
MIN_ROUTE_LENGTH_M <- 3000
EXPORT_MAX_KM      <- 10
EXPORT_N_ROUTES    <- 38
N_WAYPOINTS        <- 9

# ── EXPORT: Run ILP with export parameters ────────────────────────────────────
exp_idx <- which(sapply(all_candidate_paths, function(p)
  p$length_m >= MIN_ROUTE_LENGTH_M && p$length_m <= EXPORT_MAX_KM * 1000))
export_cands <- all_candidate_paths[exp_idx]

message(sprintf("Running ILP for export: %d candidates...", length(export_cands)))

export_ilp    <- run_ilp(export_cands, n_edges, edge_sf$pop, EXPORT_N_ROUTES, exp_idx)
export_routes <- lapply(seq_along(export_ilp$selected_idx), function(r) {
  ro <- export_cands[[export_ilp$selected_idx[r]]]
  list(route_id = r, edge_ids = ro$eids, length_m = ro$length_m, pop = ro$pop)
})

message(sprintf("ILP export: %d routes, %.1f%% population coverage",
                length(export_routes), export_ilp$coverage_pct))

# ── EXTRACT GEOMETRY AND COMPUTE WAYPOINTS (robust) ──────────────────────────
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
  
  row <- data.frame(
    route_id    = ro$route_id,
    length_km   = round(ro$length_m / 1000, 3),
    pop_covered = round(ro$pop, 0),
    route_wkt   = st_as_text(st_transform(route_line, 4326)),
    start_lat   = round(coords[1,            "Y"], 6),
    start_lon   = round(coords[1,            "X"], 6),
    end_lat     = round(coords[nrow(coords), "Y"], 6),
    end_lon     = round(coords[nrow(coords), "X"], 6),
    stringsAsFactors = FALSE
  )
  
  waypoint_rows <- coords[2:(N_WAYPOINTS + 1), , drop = FALSE]
  for (w in seq_len(N_WAYPOINTS)) {
    row[[paste0("wp", w, "_lat")]] <- round(waypoint_rows[w, "Y"], 6)
    row[[paste0("wp", w, "_lon")]] <- round(waypoint_rows[w, "X"], 6)
  }
  row
})

route_export_rows <- Filter(Negate(is.null), route_export_rows)
routes_export_df  <- do.call(rbind, route_export_rows)
rownames(routes_export_df) <- NULL

# ── PREVIEW ───────────────────────────────────────────────────────────────────
cat(sprintf("Exported %d routes (>= %.0fkm <= %.0fkm, ILP optimal, no overlap)\n",
            nrow(routes_export_df), MIN_ROUTE_LENGTH_M / 1000, EXPORT_MAX_KM))
print(routes_export_df[, c("route_id", "length_km", "pop_covered",
                           "start_lat", "start_lon", "end_lat", "end_lon",
                           "wp1_lat", "wp1_lon")])

# ── PLOT ──────────────────────────────────────────────────────────────────────
plot(st_geometry(yerevan),
     border = "black", lwd = 1.5, col = "lightgrey",
     main   = sprintf("ILP Routes (n=%d, %.0fkm to %.0fkm, no overlap)",
                      nrow(routes_export_df), MIN_ROUTE_LENGTH_M / 1000, EXPORT_MAX_KM),
     axes = FALSE)

plot(st_geometry(routes_final_sf),
     col = RColorBrewer::brewer.pal(9, "Set1")[routes_final_sf$route_id %% 9 + 1],
     lwd = 3.5, add = TRUE)

text_coords       <- st_centroid(routes_final_sf)
text_coords$label <- paste0(routes_final_sf$route_id,
                            " (", round(routes_final_sf$pop / 1000, 0), "k)")
text(st_coordinates(text_coords), labels = text_coords$label,
     cex = 0.75, font = 2, col = "black")

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
  "C:\\Users\\mrealehatem\\Documents\\GitHub\\ArmeniaPollutionAnalysis\\Route Network Optimizing\\yerevan_routes_%dkm_%dkm_%droutes_ILP.csv",
  as.integer(MIN_ROUTE_LENGTH_M / 1000), as.integer(EXPORT_MAX_KM), EXPORT_N_ROUTES
)
write.csv(routes_export_df, export_path, row.names = FALSE)
message(sprintf("Saved to: %s", export_path))

library(sf)
library(osmextract)
library(sfnetworks)
library(tidygraph)
library(igraph)
library(dplyr)
library(ggplot2)
library(terra)
library(tidyr)
library(ROI)
library(ROI.plugin.glpk)
library(slam)

# ── CONSTANTS ─────────────────────────────────────────────────────────────────
MAX_ROUTES  <- 38
SPEED_KMH   <- 30
SPEED_KMMIN <- SPEED_KMH / 60

# ── PENALTY FUNCTION (L in metres) ────────────────────────────────────────────
# Absolute travel time error (RSS of variance + rounding), in minutes.
# Acts as the penalty — high error routes are discouraged by lambda weighting.
penalty_fn <- function(L_m) {
  L_km       <- L_m / 1000
  travel_min <- L_km / SPEED_KMMIN
  sigma_var  <- 3.2 * sqrt(L_km)
  sigma_rnd  <- 0.289 / (SPEED_KMMIN * L_km)
  abs_err    <- sqrt(sigma_var^2 + sigma_rnd^2)
  pct_err    <- abs_err / travel_min
  list(penalty    = abs_err,
       abs_err    = abs_err,
       travel_min = travel_min,
       pct_err    = pct_err)
}

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

# ── 8. PRE-COMPUTE CANDIDATE PATHS (all pairs, no length filter) ──────────────
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
    L_m <- sum(edge_sf$length_m[eids])
    pen <- penalty_fn(L_m)
    all_candidate_paths[[i]] <- list(
      from       = node_pairs[1, i],
      to         = node_pairs[2, i],
      eids       = eids,
      length_m   = L_m,
      pop        = sum(edge_sf$pop[eids]),
      penalty    = pen$penalty,
      abs_err    = pen$abs_err,
      travel_min = pen$travel_min,
      pct_err    = pen$pct_err
    )
  }
  if (i %% 500 == 0) message(sprintf("  %d / %d done...", i, n_pairs))
}
all_candidate_paths <- Filter(Negate(is.null), all_candidate_paths)
message(sprintf("%d valid paths pre-computed.", length(all_candidate_paths)))

# ── 9A. GREEDY (marginal pop, penalised by abs error) ─────────────────────────
run_greedy <- function(candidate_paths, n_edges, edge_pops, edge_lengths,
                       max_routes, lambda = 0.5) {
  covered      <- rep(FALSE, n_edges)
  total_pop    <- sum(edge_pops)
  selected_idx <- integer(0)
  
  for (r in seq_len(max_routes)) {
    if (all(covered)) break
    
    scores <- sapply(candidate_paths, function(p) {
      marginal_pop <- sum(edge_pops[p$eids[!covered[p$eids]]])
      (marginal_pop / total_pop) - lambda * (p$penalty / max_routes)
    })
    
    best_i <- which.max(scores)
    if (scores[best_i] <= 0) break
    
    covered[candidate_paths[[best_i]]$eids] <- TRUE
    selected_idx <- c(selected_idx, best_i)
  }
  
  pct_errs <- sapply(candidate_paths[selected_idx], `[[`, "pct_err")
  list(
    pop_pct      = 100 * sum(edge_pops[covered])    / total_pop,
    km_pct       = 100 * sum(edge_lengths[covered]) / sum(edge_lengths),
    mean_pct_err = 100 * mean(pct_errs),
    selected_idx = selected_idx
  )
}

# ── 9B. SET-COVER ILP (exact, no length constraints) ─────────────────────────
# Maximise: sum(pop_e * y_e) / total_pop  -  lambda * sum(penalty_p * x_p) / K
# Subject to: sum(x_p) == K
#             y_e <= sum_{p containing e} x_p   for each active edge e

run_setcover_ilp <- function(candidate_paths, n_edges, edge_pops, edge_lengths,
                             max_routes, lambda = 0.5) {
  
  n_cands      <- length(candidate_paths)
  total_pop    <- sum(edge_pops)
  active_edges <- sort(unique(unlist(lapply(candidate_paths, `[[`, "eids"))))
  n_active     <- length(active_edges)
  
  pen_vec    <- sapply(candidate_paths, `[[`, "penalty")
  pop_active <- edge_pops[active_edges]
  
  # Objective: minimise  lambda*penalty*x  -  pop*y
  obj <- c(lambda * pen_vec / max_routes,   # x_p terms
           -pop_active / total_pop)          # y_e terms
  
  n_vars <- n_cands + n_active
  
  # Build sparse coverage constraint matrix
  cov_i <- integer(0); cov_j <- integer(0); cov_v <- numeric(0)
  for (ae in seq_len(n_active)) {
    e_global <- active_edges[ae]
    cov_i <- c(cov_i, ae);           cov_j <- c(cov_j, n_cands + ae); cov_v <- c(cov_v,  1)
    for (pi in seq_len(n_cands)) {
      if (e_global %in% candidate_paths[[pi]]$eids) {
        cov_i <- c(cov_i, ae);       cov_j <- c(cov_j, pi);           cov_v <- c(cov_v, -1)
      }
    }
  }
  
  all_i <- c(rep(1L, n_cands), as.integer(cov_i + 1L))
  all_j <- c(seq_len(n_cands), as.integer(cov_j))
  all_v <- c(rep(1, n_cands),  cov_v)
  
  con_stm <- slam::simple_triplet_matrix(
    i = all_i, j = all_j, v = all_v,
    nrow = 1L + n_active, ncol = n_vars
  )
  
  prob <- ROI::OP(
    objective   = ROI::L_objective(obj),
    constraints = ROI::L_constraint(con_stm,
                                    dir = c("==", rep("<=", n_active)),
                                    rhs = c(max_routes, rep(0, n_active))),
    bounds      = ROI::V_bound(li = seq_len(n_vars), ui = seq_len(n_vars),
                               lb = rep(0, n_vars),  ub = rep(1, n_vars)),
    types   = rep("B", n_vars),
    maximum = FALSE
  )
  
  result   <- ROI::ROI_solve(prob, solver = "glpk",
                             control = list(verbose = FALSE, presolve = TRUE,
                                            tm_limit = 120000))
  selected <- which(result$solution[seq_len(n_cands)] > 0.5)
  
  covered  <- rep(FALSE, n_edges)
  for (idx in selected) covered[candidate_paths[[idx]]$eids] <- TRUE
  
  pct_errs <- sapply(candidate_paths[selected], `[[`, "pct_err")
  list(
    pop_pct      = 100 * sum(edge_pops[covered])    / total_pop,
    km_pct       = 100 * sum(edge_lengths[covered]) / sum(edge_lengths),
    mean_pct_err = 100 * mean(pct_errs),
    selected_idx = selected
  )
}

# ── 10. 2D SWEEP: route counts × lambda ───────────────────────────────────────
# No length axis — length is governed by penalty. Sweep lambda instead.
route_counts <- seq(20, 40, by = 2)
lambdas      <- c(0, 0.1, 0.2, 0.3, 0.5, 0.75, 1.0, 1.5, 2.0)

pop_matrix <- matrix(NA, nrow = length(lambdas), ncol = length(route_counts))
len_matrix <- matrix(NA, nrow = length(lambdas), ncol = length(route_counts))
err_matrix <- matrix(NA, nrow = length(lambdas), ncol = length(route_counts))

for (li in seq_along(lambdas)) {
  for (ri in seq_along(route_counts)) {
    res                <- run_greedy(all_candidate_paths, n_edges,
                                     edge_sf$pop, edge_sf$length_m,
                                     route_counts[ri], lambda = lambdas[li])
    pop_matrix[li, ri] <- res$pop_pct
    len_matrix[li, ri] <- res$km_pct
    err_matrix[li, ri] <- res$mean_pct_err
  }
  message(sprintf("Lambda = %.2f done.", lambdas[li]))
}

# ── 11. RESHAPE FOR GGPLOT ────────────────────────────────────────────────────
make_long <- function(mat, col_vals, row_vals, metric_label) {
  df           <- as.data.frame(mat)
  colnames(df) <- col_vals
  df$lambda    <- row_vals
  tidyr::pivot_longer(df, cols = -lambda,
                      names_to  = "n_routes",
                      values_to = "value") %>%
    mutate(n_routes = as.integer(n_routes),
           metric   = metric_label)
}

lambda_idx_05 <- which(lambdas == 0.5)

heatmap_df <- bind_rows(
  make_long(pop_matrix, route_counts, lambdas, "Population Coverage (%)"),
  make_long(len_matrix, route_counts, lambdas, "Network Length Coverage (%)"),
  make_long(err_matrix, route_counts, lambdas, "Avg % Error in Travel Time")
) %>%
  mutate(
    metric = factor(metric, levels = c(
      "Population Coverage (%)",
      "Network Length Coverage (%)",
      "Avg % Error in Travel Time"
    )),
    cell_label = ifelse(
      metric == "Avg % Error in Travel Time",
      sprintf("%.1f%%", value),
      sprintf("%.0f%%", value)
    )
  ) %>%
  group_by(metric) %>%
  mutate(value_norm = (value - min(value, na.rm = TRUE)) /
           (max(value, na.rm = TRUE) - min(value, na.rm = TRUE))) %>%
  ungroup()

# ── 12. HEATMAPS ──────────────────────────────────────────────────────────────
ggplot(heatmap_df, aes(x = n_routes, y = factor(lambda), fill = value_norm)) +
  geom_tile(colour = "white", linewidth = 0.3) +
  geom_text(aes(label = cell_label),
            size = 2.8, colour = "white", fontface = "bold") +
  annotate("rect", xmin = 37.5, xmax = 38.5, ymin = -Inf, ymax = Inf,
           colour = "red", fill = NA, linewidth = 1) +
  annotate("rect", xmin = -Inf, xmax = Inf,
           ymin = lambda_idx_05 - 0.5, ymax = lambda_idx_05 + 0.5,
           colour = "red", fill = NA, linewidth = 1) +
  scale_fill_gradientn(
    colours = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c"),
    name    = "Relative\nValue"
  ) +
  scale_x_continuous(breaks = route_counts) +
  facet_wrap(~metric, ncol = 1) +
  labs(
    title    = "Route Optimisation: Coverage vs. Travel Time Error",
    subtitle = sprintf(
      "Red box = 38 routes, λ=0.5. No hard length constraints. v = %.0f km/h.",
      SPEED_KMH),
    x = "Number of Routes",
    y = "Penalty Weight (λ)"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(),
        strip.text = element_text(face = "bold", size = 12))

# ── 13. FINAL BEST ROUTES: set-cover ILP (38 routes, lambda = 0) ──────────────
LAMBDA_FINAL <- 0

message(sprintf("Running set-cover ILP: %d routes, lambda = %.1f ...",
                MAX_ROUTES, LAMBDA_FINAL))
ilp_result   <- run_setcover_ilp(all_candidate_paths, n_edges,
                                 edge_sf$pop, edge_sf$length_m,
                                 MAX_ROUTES, lambda = LAMBDA_FINAL)
selected_idx <- ilp_result$selected_idx
message(sprintf(
  "ILP: %d routes | Pop: %.1f%% | KM: %.1f%% | Mean err: %.1f%%",
  length(selected_idx), ilp_result$pop_pct,
  ilp_result$km_pct,    ilp_result$mean_pct_err))

routes_out <- lapply(seq_along(selected_idx), function(r) {
  ro <- all_candidate_paths[[selected_idx[r]]]
  list(route_id   = r, from = ro$from, to = ro$to,
       edge_ids   = ro$eids, length_m = ro$length_m, pop = ro$pop,
       abs_err    = ro$abs_err, travel_min = ro$travel_min, pct_err = ro$pct_err)
})

# ── 14. MAP FINAL ROUTES ──────────────────────────────────────────────────────
route_geometries <- lapply(routes_out, function(ro) {
  st_sf(route_id    = ro$route_id,
        length_km   = round(ro$length_m / 1000, 3),
        pop         = ro$pop,
        abs_err_min = round(ro$abs_err, 3),
        pct_err     = round(100 * ro$pct_err, 1),
        geometry    = st_union(edge_sf[ro$edge_ids, ]))
})
routes_final_sf <- do.call(rbind, route_geometries)

plot(st_geometry(yerevan), border = "black",
     main = "Set-Cover ILP Routes (38 routes, λ=0, pure coverage)")
plot(st_geometry(routes_final_sf),
     col = rainbow(nrow(routes_final_sf))[routes_final_sf$route_id],
     lwd = 2.5, add = TRUE)
legend("bottomleft",
       legend = paste0("R", routes_final_sf$route_id,
                       " (", round(routes_final_sf$pop / 1000, 1),
                       "k | ", routes_final_sf$pct_err, "% err)"),
       col = rainbow(nrow(routes_final_sf)), lwd = 2, cex = 0.4)

# ── EXPORT ────────────────────────────────────────────────────────────────────
EXPORT_N_ROUTES <- 38
EXPORT_LAMBDA   <- 0
N_WAYPOINTS     <- 9

# No need to re-solve — EXPORT_LAMBDA == LAMBDA_FINAL, reuse result
export_routes <- lapply(seq_along(selected_idx), function(r) {
  ro <- all_candidate_paths[[selected_idx[r]]]
  list(route_id   = r, edge_ids = ro$eids, length_m = ro$length_m,
       pop = ro$pop, abs_err = ro$abs_err,
       travel_min = ro$travel_min, pct_err = ro$pct_err)
})

# ── WAYPOINTS AND GEOMETRY ────────────────────────────────────────────────────
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
    warning(sprintf("Route %d: only %d points, skipping.", ro$route_id, length(sampled_pts)))
    return(NULL)
  }
  
  sampled_pts <- st_transform(sampled_pts, 4326)
  coords      <- st_coordinates(sampled_pts)
  
  row <- data.frame(
    route_id    = ro$route_id,
    length_km   = round(ro$length_m / 1000, 3),
    travel_min  = round(ro$travel_min, 2),
    abs_err_min = round(ro$abs_err, 3),
    pct_err     = round(100 * ro$pct_err, 2),
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

cat(sprintf("Exported %d routes (no length constraints, λ=0, pure coverage)\n",
            nrow(routes_export_df)))
print(routes_export_df[, c("route_id", "length_km", "travel_min",
                           "abs_err_min", "pct_err", "pop_covered",
                           "start_lat", "start_lon", "end_lat", "end_lon",
                           "wp1_lat", "wp1_lon")])

# ── EXPORT TO CSV ─────────────────────────────────────────────────────────────
export_path <- sprintf(
  "C:\\Users\\mrealehatem\\Documents\\GitHub\\ArmeniaPollutionAnalysis\\Route Network Optimizing\\yerevan_routes_%droutes_lam0_setcover.csv",
  EXPORT_N_ROUTES
)
write.csv(routes_export_df, export_path, row.names = FALSE)
message(sprintf("Saved to: %s", export_path))

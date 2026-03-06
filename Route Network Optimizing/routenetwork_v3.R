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
library(parallel)

# ── CONSTANTS ─────────────────────────────────────────────────────────────────
MAX_ROUTES  <- 38
SPEED_KMH   <- 30
SPEED_KMMIN <- SPEED_KMH / 60
N_CORES     <- max(1L, detectCores() - 1L)

# Coverage objective blend:
#   alpha = 1.0 → pure edge-length coverage (every metre counts equally)
#   alpha = 0.0 → pure population coverage (original behaviour)
#   alpha = 0.5 → equal blend
ALPHA <- 0.7

# ── PROGRESS BAR HELPER ───────────────────────────────────────────────────────
make_pb <- function(total, label = "", width = 40) {
  list(total = total, label = label, width = width, start = proc.time()["elapsed"])
}
tick_pb <- function(pb, i) {
  pct     <- i / pb$total
  filled  <- round(pct * pb$width)
  bar     <- paste0(strrep("=", filled), strrep("-", pb$width - filled))
  elapsed <- as.numeric(proc.time()["elapsed"] - pb$start)
  eta     <- if (pct > 0.01) elapsed / pct * (1 - pct) else NA
  eta_str <- if (!is.na(eta)) sprintf("ETA %ds", round(eta)) else "ETA --"
  cat(sprintf("\r  %s [%s] %3.0f%%  %s  (%.0fs elapsed)",
              pb$label, bar, 100 * pct, eta_str, elapsed))
  if (i == pb$total) cat("\n")
  flush.console()
}

# ── PENALTY FUNCTION ──────────────────────────────────────────────────────────
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
message("\n[1/8] Loading road network...")
roads_pbf   <- "C:/Users/mrealehatem/Downloads/armenia.pbf"
yerevan_shp <- sf::st_read("C:/Users/mrealehatem/Documents/GitHub/ArmeniaPollutionAnalysis/Yerevan Boundary/boundary.gpkg",
                           layer = "boundary-polygon-lvl4", quiet = TRUE)
pop_path    <- "C:/Users/mrealehatem/Documents/GitHub/ArmeniaPollutionAnalysis/WorldPopData/arm_age_data/pop.tif"

armenia_roads_oe <- oe_read(roads_pbf, layer = "lines",
                            extra_tags = c("lanes", "maxspeed"))
armenia_roads_oe <- armenia_roads_oe[!is.na(armenia_roads_oe$highway), ]
message(sprintf("    Loaded %d road features.", nrow(armenia_roads_oe)))

# ── 2. CROP TO YEREVAN ────────────────────────────────────────────────────────
message("\n[2/8] Cropping to Yerevan boundary...")
yerevan <- yerevan_shp
if (st_crs(armenia_roads_oe) != st_crs(yerevan))
  yerevan <- st_transform(yerevan, st_crs(armenia_roads_oe))
yerevan_roads <- st_intersection(armenia_roads_oe, yerevan)
message(sprintf("    %d road features within Yerevan.", nrow(yerevan_roads)))

# ── 3. SUBSET TO MAJOR ROADS ──────────────────────────────────────────────────
message("\n[3/8] Subsetting to major roads...")
yerevan_roads$lanes <- as.numeric(yerevan_roads$lanes)
routes <- yerevan_roads %>%
  filter(lanes >= 4 | highway %in% c("primary", "trunk")) %>%
  filter(st_geometry_type(.) %in% c("LINESTRING", "MULTILINESTRING"))
routes <- st_cast(routes, "LINESTRING")
message(sprintf("    %d major road segments retained.", nrow(routes)))

# ── 4. PROJECT TO UTM 38N ─────────────────────────────────────────────────────
message("\n[4/8] Projecting to UTM 38N...")
routes  <- st_transform(routes, 32638)
yerevan <- st_transform(yerevan, 32638)

# ── 5. BUILD SPATIAL NETWORK ──────────────────────────────────────────────────
message("\n[5/8] Building spatial network...")
net <- as_sfnetwork(routes, directed = FALSE) %>%
  activate("edges") %>%
  mutate(weight  = as.numeric(edge_length()),
         edge_id = row_number()) %>%
  activate("nodes") %>%
  filter(group_components() == 1)

edge_sf          <- net %>% activate("edges") %>% st_as_sf()
edge_sf$length_m <- as.numeric(st_length(edge_sf))
n_edges          <- nrow(edge_sf)
message(sprintf("    Network: %d nodes, %d edges.",
                igraph::vcount(igraph::as.igraph(net)), n_edges))

# ── 6. ATTACH POPULATION TO EDGES ────────────────────────────────────────────
message("\n[6/8] Extracting population per edge...")
pop_rast <- rast(pop_path)
pop_rast <- project(pop_rast, "EPSG:32638")

edge_pop_vals <- terra::extract(pop_rast, vect(edge_sf), fun = sum, na.rm = TRUE)
edge_sf$pop   <- edge_pop_vals[, 2]
edge_sf$pop[is.na(edge_sf$pop)] <- 0

total_pop    <- sum(edge_sf$pop)
total_length <- sum(edge_sf$length_m)
message(sprintf("    Total population: %.0f | Total network length: %.1f km",
                total_pop, total_length / 1000))

# ── 7. SPATIALLY STRATIFIED NODE SAMPLING ────────────────────────────────────
# Uniform random sampling clusters nodes in dense central areas, causing
# candidate paths to repeatedly cover the same corridors. Spatial stratification
# forces origin-destination pairs to span the full city extent.
message("\n[7/8] Computing candidate paths (spatially stratified sampling)...")

g       <- igraph::as.igraph(net)
node_sf <- net %>% activate("nodes") %>% st_as_sf()

bbox   <- st_bbox(node_sf)
grid_n <- 8L   # 8×8 grid = up to 64 cells; tune up for more peripheral coverage
grid_x <- seq(bbox["xmin"], bbox["xmax"], length.out = grid_n + 1)
grid_y <- seq(bbox["ymin"], bbox["ymax"], length.out = grid_n + 1)

sampled_nodes <- integer(0)
for (xi in seq_len(grid_n)) {
  for (yi in seq_len(grid_n)) {
    cell    <- st_as_sfc(st_bbox(c(xmin = grid_x[xi],   ymin = grid_y[yi],
                                   xmax = grid_x[xi+1], ymax = grid_y[yi+1]),
                                 crs = st_crs(node_sf)))
    in_cell <- which(lengths(st_within(node_sf, cell)) > 0)
    if (length(in_cell) > 0)
      sampled_nodes <- c(sampled_nodes, sample(in_cell, 1L))
  }
}
sampled_nodes <- unique(sampled_nodes)
message(sprintf("    %d spatially stratified nodes selected.", length(sampled_nodes)))

node_pairs <- combn(as.integer(sampled_nodes), 2)
n_pairs    <- ncol(node_pairs)
message(sprintf("    %d node pairs to evaluate.", n_pairs))

edge_lengths_vec <- edge_sf$length_m
edge_pops_vec    <- edge_sf$pop

chunk_size <- max(1L, ceiling(n_pairs / (N_CORES * 10)))
chunk_ids  <- split(seq_len(n_pairs), ceiling(seq_len(n_pairs) / chunk_size))
n_chunks   <- length(chunk_ids)

cl <- makeCluster(N_CORES)
clusterExport(cl, varlist = c("g", "node_pairs", "edge_lengths_vec",
                              "edge_pops_vec", "penalty_fn", "SPEED_KMMIN"),
              envir = environment())
clusterEvalQ(cl, library(igraph))

pb      <- make_pb(n_chunks, label = "Paths")
results <- vector("list", n_chunks)

for (ci in seq_len(n_chunks)) {
  idx_chunk     <- chunk_ids[[ci]]
  results[[ci]] <- parLapply(cl, idx_chunk, function(i) {
    sp <- igraph::shortest_paths(g,
                                 from    = node_pairs[1, i],
                                 to      = node_pairs[2, i],
                                 output  = "epath",
                                 weights = igraph::E(g)$weight)
    eids <- as.integer(sp$epath[[1]])
    if (length(eids) == 0) return(NULL)
    L_m <- sum(edge_lengths_vec[eids])
    pen <- penalty_fn(L_m)
    list(from       = node_pairs[1, i],
         to         = node_pairs[2, i],
         eids       = eids,
         length_m   = L_m,
         pop        = sum(edge_pops_vec[eids]),
         penalty    = pen$penalty,
         abs_err    = pen$abs_err,
         travel_min = pen$travel_min,
         pct_err    = pen$pct_err)
  })
  tick_pb(pb, ci)
}
stopCluster(cl)

all_candidate_paths <- Filter(Negate(is.null), unlist(results, recursive = FALSE))
message(sprintf("    %d valid paths computed.", length(all_candidate_paths)))

path_keys           <- sapply(all_candidate_paths,
                              function(p) paste(sort(unique(p$eids)), collapse = "-"))
all_candidate_paths <- all_candidate_paths[!duplicated(path_keys)]
message(sprintf("    %d paths after deduplication.", length(all_candidate_paths)))

# ── 8A. GREEDY SOLVER (for sweep) ────────────────────────────────────────────
# Objective blends length coverage and population coverage via ALPHA
run_greedy <- function(candidate_paths, n_edges, edge_pops, edge_lengths,
                       max_routes, lambda = 0.5, alpha = ALPHA) {
  covered      <- rep(FALSE, n_edges)
  total_pop    <- sum(edge_pops)
  total_length <- sum(edge_lengths)
  selected_idx <- integer(0)
  
  for (r in seq_len(max_routes)) {
    if (all(covered)) break
    scores <- sapply(candidate_paths, function(p) {
      new_e        <- p$eids[!covered[p$eids]]
      marginal_len <- sum(edge_lengths[new_e]) / total_length
      marginal_pop <- sum(edge_pops[new_e])    / total_pop
      # Blend: alpha weights length coverage, (1-alpha) weights population
      (alpha * marginal_len + (1 - alpha) * marginal_pop) -
        lambda * (p$penalty / max_routes)
    })
    best_i <- which.max(scores)
    if (scores[best_i] <= 0) break
    covered[candidate_paths[[best_i]]$eids] <- TRUE
    selected_idx <- c(selected_idx, best_i)
  }
  
  pct_errs <- sapply(candidate_paths[selected_idx], `[[`, "pct_err")
  list(
    pop_pct      = 100 * sum(edge_pops[covered])    / total_pop,
    km_pct       = 100 * sum(edge_lengths[covered]) / total_length,
    mean_pct_err = 100 * mean(pct_errs),
    selected_idx = selected_idx
  )
}

# ── 8B. SET-COVER ILP ─────────────────────────────────────────────────────────
# Objective: maximise alpha * (length covered / total_length)
#                  + (1-alpha) * (population covered / total_pop)
#                  - lambda * (travel time penalty)
#
# Each edge e contributes:
#   alpha * length_e / total_length   (length reward)
#   + (1-alpha) * pop_e / total_pop   (population reward)
# via its binary coverage indicator y_e.
# Low-population peripheral edges now have nonzero weight via the length term,
# so the solver is incentivised to spread routes across the full network.

run_setcover_ilp <- function(candidate_paths, n_edges, edge_pops, edge_lengths,
                             max_routes, lambda = 0.5, alpha = ALPHA,
                             max_overlap = 2L) {
  
  n_cands      <- length(candidate_paths)
  total_pop    <- sum(edge_pops)
  total_length <- sum(edge_lengths)
  active_edges <- sort(unique(unlist(lapply(candidate_paths,
                                            function(p) unique(p$eids)))))
  n_active     <- length(active_edges)
  
  message(sprintf(
    "    ILP size: %d routes × %d edges | α=%.2f | max_overlap=%d",
    n_cands, n_active, alpha, max_overlap))
  
  pen_vec       <- sapply(candidate_paths, `[[`, "penalty")
  len_active    <- edge_lengths[active_edges]
  pop_active    <- edge_pops[active_edges]
  
  # Per-edge coverage reward = blend of length and population
  edge_reward   <- alpha       * (len_active / total_length) +
    (1 - alpha) * (pop_active / total_pop)
  
  # Objective: [x_p costs] [y_e rewards]
  obj    <- c(lambda * pen_vec / max_routes, -edge_reward)
  n_vars <- n_cands + n_active
  
  unique_eids_per_path <- lapply(candidate_paths, function(p) unique(p$eids))
  ae_lookup <- match(unlist(unique_eids_per_path), active_edges)
  p_rep     <- rep(seq_len(n_cands),
                   times = sapply(unique_eids_per_path, length))
  valid     <- !is.na(ae_lookup)
  ae_lookup <- ae_lookup[valid]
  p_rep     <- p_rep[valid]
  
  # Row 1:              sum(x_p) == K
  # Rows 2..n+1:        y_e - sum(x_p covering e) <= 0   (coverage)
  # Rows n+2..2n+1:     sum(x_p covering e) <= max_overlap  (overlap limit)
  b1_i <- rep(1L, n_cands);  b1_j <- seq_len(n_cands);  b1_v <- rep(1, n_cands)
  
  b2_i <- c(ae_lookup + 1L,            ae_lookup + 1L)
  b2_j <- c(n_cands + ae_lookup,       p_rep)
  b2_v <- c(rep(1,  length(ae_lookup)), rep(-1, length(ae_lookup)))
  
  b3_raw_i <- ae_lookup;  b3_raw_j <- p_rep;  b3_raw_v <- rep(1, length(ae_lookup))
  b3_key   <- paste(b3_raw_i, b3_raw_j, sep = "_")
  b3_agg   <- tapply(b3_raw_v, b3_key, sum)
  b3_split <- strsplit(names(b3_agg), "_")
  b3_i     <- as.integer(sapply(b3_split, `[`, 1)) + 1L + n_active
  b3_j     <- as.integer(sapply(b3_split, `[`, 2))
  b3_v     <- as.numeric(b3_agg)
  
  all_i_raw <- c(b1_i, b2_i, b3_i)
  all_j_raw <- c(b1_j, b2_j, b3_j)
  all_v_raw <- c(b1_v, b2_v, b3_v)
  
  ij_key   <- paste(all_i_raw, all_j_raw, sep = "_")
  agg      <- tapply(all_v_raw, ij_key, sum)
  ij_split <- strsplit(names(agg), "_")
  all_i    <- as.integer(sapply(ij_split, `[`, 1))
  all_j    <- as.integer(sapply(ij_split, `[`, 2))
  all_v    <- as.numeric(agg)
  
  con_stm <- slam::simple_triplet_matrix(
    i = all_i, j = all_j, v = all_v,
    nrow = 1L + 2L * n_active, ncol = n_vars
  )
  
  prob <- ROI::OP(
    objective   = ROI::L_objective(obj),
    constraints = ROI::L_constraint(
      con_stm,
      dir = c("==", rep("<=", n_active), rep("<=", n_active)),
      rhs = c(max_routes, rep(0, n_active), rep(max_overlap, n_active))
    ),
    bounds = ROI::V_bound(
      li = seq_len(n_vars), ui = seq_len(n_vars),
      lb = rep(0, n_vars),  ub = rep(1, n_vars)
    ),
    types   = rep("B", n_vars),
    maximum = FALSE
  )
  
  message("    Handing off to GLPK solver...")
  result   <- ROI::ROI_solve(prob, solver = "glpk",
                             control = list(verbose  = TRUE,
                                            presolve = TRUE,
                                            tm_limit = 120000))
  
  if (result$status$code != 0L)
    warning(sprintf("Solver status: %s", result$status$msg))
  
  selected <- which(result$solution[seq_len(n_cands)] > 0.5)
  
  covered        <- rep(FALSE, n_edges)
  edge_use_count <- rep(0L, n_edges)
  for (idx in selected) {
    covered[candidate_paths[[idx]]$eids]        <- TRUE
    edge_use_count[candidate_paths[[idx]]$eids] <-
      edge_use_count[candidate_paths[[idx]]$eids] + 1L
  }
  
  overlap_stats <- edge_use_count[edge_use_count > 0]
  pct_errs      <- sapply(candidate_paths[selected], `[[`, "pct_err")
  
  list(
    pop_pct        = 100 * sum(edge_pops[covered])    / total_pop,
    km_pct         = 100 * sum(edge_lengths[covered]) / total_length,
    mean_pct_err   = 100 * mean(pct_errs),
    mean_edge_uses = mean(overlap_stats),
    max_edge_uses  = max(overlap_stats),
    selected_idx   = selected
  )
}

# ── 9. PARAMETER SWEEP (greedy) ───────────────────────────────────────────────
message("\n[8/8] Running parameter sweep (greedy)...")
route_counts <- seq(20, 40, by = 2)
lambdas      <- c(0, 0.1, 0.2, 0.3, 0.5, 0.75, 1.0, 1.5, 2.0)
n_sweep      <- length(lambdas) * length(route_counts)

pop_matrix <- matrix(NA, nrow = length(lambdas), ncol = length(route_counts))
len_matrix <- matrix(NA, nrow = length(lambdas), ncol = length(route_counts))
err_matrix <- matrix(NA, nrow = length(lambdas), ncol = length(route_counts))

pb_sweep <- make_pb(n_sweep, label = "Sweep ")
sweep_i  <- 0L

for (li in seq_along(lambdas)) {
  for (ri in seq_along(route_counts)) {
    res                <- run_greedy(all_candidate_paths, n_edges,
                                     edge_sf$pop, edge_sf$length_m,
                                     route_counts[ri], lambda = lambdas[li])
    pop_matrix[li, ri] <- res$pop_pct
    len_matrix[li, ri] <- res$km_pct
    err_matrix[li, ri] <- res$mean_pct_err
    sweep_i            <- sweep_i + 1L
    tick_pb(pb_sweep, sweep_i)
  }
}

# ── 10. RESHAPE FOR GGPLOT ────────────────────────────────────────────────────
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

# ── 11. HEATMAPS ──────────────────────────────────────────────────────────────
message("\nPlotting heatmaps...")
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
      "Sweep via greedy (α=%.1f). Final solve via ILP. Red box = 38 routes, λ=0.5. v=%.0f km/h.",
      ALPHA, SPEED_KMH),
    x = "Number of Routes",
    y = "Penalty Weight (λ)"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(),
        strip.text = element_text(face = "bold", size = 12))

# ── 12. FINAL ILP SOLVE ───────────────────────────────────────────────────────
LAMBDA_FINAL      <- 0
MAX_OVERLAP_FINAL <- 2L   # increase to 3 if solver returns infeasible

message(sprintf("\n[Final] Set-cover ILP: %d routes, λ=%.1f, α=%.2f, max_overlap=%d ...",
                MAX_ROUTES, LAMBDA_FINAL, ALPHA, MAX_OVERLAP_FINAL))
t0         <- proc.time()["elapsed"]
ilp_result <- run_setcover_ilp(all_candidate_paths, n_edges,
                               edge_sf$pop, edge_sf$length_m,
                               MAX_ROUTES,
                               lambda      = LAMBDA_FINAL,
                               alpha       = ALPHA,
                               max_overlap = MAX_OVERLAP_FINAL)
selected_idx <- ilp_result$selected_idx
message(sprintf("    Solved in %.0fs.", as.numeric(proc.time()["elapsed"] - t0)))
message(sprintf(
  "    %d routes | Pop: %.1f%% | KM: %.1f%% | Mean err: %.1f%% | Mean edge uses: %.2f | Max: %d",
  length(selected_idx), ilp_result$pop_pct, ilp_result$km_pct,
  ilp_result$mean_pct_err, ilp_result$mean_edge_uses, ilp_result$max_edge_uses))

# ── 13. BUILD ROUTE OBJECTS ───────────────────────────────────────────────────
routes_out <- lapply(seq_along(selected_idx), function(r) {
  ro <- all_candidate_paths[[selected_idx[r]]]
  list(route_id   = r, from = ro$from, to = ro$to,
       edge_ids   = ro$eids, length_m = ro$length_m, pop = ro$pop,
       abs_err    = ro$abs_err, travel_min = ro$travel_min, pct_err = ro$pct_err)
})

# ── 14. MAP FINAL ROUTES ──────────────────────────────────────────────────────
message("\nRendering final route map...")
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
     main = sprintf("ILP Routes (%d routes, λ=%.1f, α=%.1f, max_overlap=%d)",
                    MAX_ROUTES, LAMBDA_FINAL, ALPHA, MAX_OVERLAP_FINAL))
plot(st_geometry(routes_final_sf),
     col = rainbow(nrow(routes_final_sf))[routes_final_sf$route_id],
     lwd = 2.5, add = TRUE)
legend("bottomleft",
       legend = paste0("R", routes_final_sf$route_id,
                       " (", round(routes_final_sf$pop / 1000, 1),
                       "k | ", routes_final_sf$pct_err, "% err)"),
       col = rainbow(nrow(routes_final_sf)), lwd = 2, cex = 0.4)

# ── 15. EXPORT ────────────────────────────────────────────────────────────────
message("\nExporting routes...")
EXPORT_N_ROUTES <- 38
N_WAYPOINTS     <- 9

pb_exp            <- make_pb(length(routes_out), label = "Export")
route_export_rows <- lapply(seq_along(routes_out), function(ri) {
  ro <- routes_out[[ri]]
  
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
  
  tick_pb(pb_exp, ri)
  
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

print(routes_export_df[, c("route_id", "length_km", "travel_min",
                           "abs_err_min", "pct_err", "pop_covered",
                           "start_lat", "start_lon", "end_lat", "end_lon",
                           "wp1_lat", "wp1_lon")])

export_path <- sprintf(
  "C:\\Users\\mrealehatem\\Documents\\GitHub\\ArmeniaPollutionAnalysis\\Route Network Optimizing\\yerevan_routes_%droutes_lam%.0f_alpha%.0f_setcover.csv",
  EXPORT_N_ROUTES, LAMBDA_FINAL * 10, ALPHA * 10
)
write.csv(routes_export_df, export_path, row.names = FALSE)
message(sprintf("\nDone. Saved %d routes to:\n  %s",
                nrow(routes_export_df), export_path))

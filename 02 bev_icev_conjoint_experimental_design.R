############################################################
# Vehicle Choice Experiment Design Pipeline (BEV vs ICEV)
#
# This script constructs and optimizes a stated-choice
# experimental design for a BEV–ICEV vehicle preference study.
#
# The workflow includes:
#
# (1) Full-factorial generation of vehicle attribute profiles
#     based on Price, Powertrain, Operating Cost, Convenience,
#     Smart Cockpit, Reliability, and Cabin Comfort.
#
# (2) Application of realism constraints to remove implausible
#     attribute combinations in order to improve behavioral
#     validity of the experimental design.
#
# (3) D-optimal design selection using the Federov algorithm
#     to obtain an efficient subset of profiles that maximizes
#     statistical information for conditional logit estimation,
#     including key interaction effects with powertrain.
#
# (4) Construction of BEV vs ICEV choice pairs by combining
#     candidate profiles, removing dominance cases, and scoring
#     trade-off quality to ensure realistic and non-trivial
#     decision tasks.
#
# (5) Blocking of choice tasks to reduce respondent burden and
#     improve survey completion quality.
#
# (6) Export of finalized experimental design into both wide
#     and long formats, followed by automatic generation of a
#     Qualtrics-compatible survey (QSF) file with embedded HTML
#     choice tasks.
#
# Overall, this pipeline ensures a statistically efficient,
# behaviorally realistic, and survey-ready conjoint experiment
# design for analyzing heterogeneous preferences between BEV
# and ICEV alternatives.
############################################################

library(dplyr)

# ------------------------
# 1. Attribute levels
# ------------------------
Price <- c("RMB110k_130k", "RMB150k_170k", "RMB190k_210k")
Powertrain <- c("BEV", "ICEV")
OperatingCost <- c("RMB300", "RMB600", "RMB900")
Convenience <- c("Easy", "Moderate", "Difficult")
Cockpit <- c("Basic", "Intermediate", "Advanced")
Reliability <- c("High", "Medium", "Low")
Cabin <- c("Basic", "Improved", "Premium")

# ------------------------
# 2. Full factorial
# ------------------------
full_profiles <- expand.grid(
  Price = Price,
  Powertrain = Powertrain,
  OperatingCost = OperatingCost,
  Convenience = Convenience,
  Cockpit = Cockpit,
  Reliability = Reliability,
  Cabin = Cabin,
  stringsAsFactors = FALSE
)

# ------------------------
# 3. Realism constraints
# ------------------------
realistic_pool <- full_profiles %>%
  filter(
    !(Powertrain == "BEV" & OperatingCost == "RMB900"),
    !(Powertrain == "BEV" & Price == "RMB110k_130k" & Reliability == "High"),
    !(Powertrain == "ICEV" & Price == "RMB110k_130k" & Cockpit == "Advanced"),
    !(Powertrain == "ICEV" & Price == "RMB110k_130k" & Cabin == "Premium"),
    !(Powertrain == "ICEV" & Convenience == "Difficult"),
    !(Price == "RMB190k_210k" & Cockpit == "Basic" & Cabin == "Basic")
  )

# ------------------------
# 4. Basic diagnostics
# ------------------------
cat("Total candidate profiles:", nrow(realistic_pool), "\n")
cat("Powertrain distribution:\n")
print(table(realistic_pool$Powertrain))

cat("\nMain attribute distributions:\n")
cat("Price:\n"); print(table(realistic_pool$Price))
cat("OperatingCost:\n"); print(table(realistic_pool$OperatingCost))
cat("Convenience:\n"); print(table(realistic_pool$Convenience))
cat("Cockpit:\n"); print(table(realistic_pool$Cockpit))
cat("Cabin:\n"); print(table(realistic_pool$Cabin))
cat("Reliability:\n"); print(table(realistic_pool$Reliability))

# ------------------------
# 5. Check only the 3 core interactions
# ------------------------
cat("\nPowertrain x Cockpit:\n")
print(table(realistic_pool$Powertrain, realistic_pool$Cockpit))

cat("\nPowertrain x Reliability:\n")
print(table(realistic_pool$Powertrain, realistic_pool$Reliability))

cat("\nPowertrain x Convenience:\n")
print(table(realistic_pool$Powertrain, realistic_pool$Convenience))

cat("\nRow proportions:\n")
print(prop.table(table(realistic_pool$Powertrain, realistic_pool$Cockpit), 1))
print(prop.table(table(realistic_pool$Powertrain, realistic_pool$Reliability), 1))
print(prop.table(table(realistic_pool$Powertrain, realistic_pool$Convenience), 1))

# ------------------------
# 6. Random inspection
# ------------------------
set.seed(123)
print(realistic_pool %>% sample_n(30))

# ------------------------
# 7. Export candidate pool
# ------------------------
realistic_pool_export <- realistic_pool %>%
  mutate(profile_id = sprintf("P%03d", 1:n())) %>%
  select(profile_id, Price, Powertrain, OperatingCost, Convenience, Cockpit, Reliability, Cabin)

write.csv(realistic_pool_export, "realistic_candidate_pool_revised.csv", row.names = FALSE)




## reduce model with D-optimal

library(AlgDesign)

design_data <- realistic_pool %>%
  select(
    Price,
    Powertrain,
    OperatingCost,
    Convenience,
    Cockpit,
    Reliability,
    Cabin
  ) %>%
  mutate(across(everything(), as.factor))

d_formula <- ~
  Price + Powertrain + OperatingCost + Convenience + Cockpit + Reliability + Cabin +
  Powertrain:Cockpit +
  Powertrain:Reliability +
  Powertrain:Convenience

X_full <- model.matrix(d_formula, data = design_data)

cat("Full model matrix columns:\n")
print(ncol(X_full))

cat("\nFull model matrix rank:\n")
print(qr(X_full)$rank)

qr_obj <- qr(X_full)
independent_cols <- sort(qr_obj$pivot[seq_len(qr_obj$rank)])

X_reduced <- X_full[, independent_cols, drop = FALSE]

cat("\nReduced model matrix columns:\n")
print(ncol(X_reduced))

cat("\nReduced model matrix rank:\n")
print(qr(X_reduced)$rank)

cat("\nRetained columns:\n")
print(colnames(X_reduced))

X_design <- as.data.frame(X_reduced[, -1, drop = FALSE])

set.seed(123)

d_opt_result <- optFederov(
  ~ .,
  data = X_design,
  nTrials = 40,
  criterion = "D",
  nRepeats = 20
)

d_opt_profiles <- realistic_pool %>%
  slice(d_opt_result$rows) %>%
  mutate(design_id = sprintf("D%03d", 1:n()))

cat("\nSelected D-optimal profile count:\n")
print(nrow(d_opt_profiles))

cat("\nSelected row indices:\n")
print(d_opt_result$rows)

cat("\nPowertrain x Cockpit:\n")
print(table(d_opt_profiles$Powertrain, d_opt_profiles$Cockpit))

cat("\nPowertrain x Reliability:\n")
print(table(d_opt_profiles$Powertrain, d_opt_profiles$Reliability))

cat("\nPowertrain x Convenience:\n")
print(table(d_opt_profiles$Powertrain, d_opt_profiles$Convenience))

write.csv(
  d_opt_profiles,
  "d_optimal_profiles_40_reduced_model.csv",
  row.names = FALSE
)




# 分block和配对方案

library(dplyr)

# ------------------------
# 1. Load profiles
# ------------------------
if (exists("d_opt_profiles")) {
  profiles <- d_opt_profiles
} else {
  profiles <- read.csv(
    "d_optimal_profiles_40_reduced_model.csv",
    stringsAsFactors = FALSE
  )
}

# ------------------------
# 2. Ensure profile_id exists
# ------------------------
if (!("profile_id" %in% names(profiles))) {
  profiles <- profiles %>%
    mutate(profile_id = sprintf("P%03d", 1:n()))
}

profiles <- profiles %>%
  select(
    profile_id,
    Price,
    Powertrain,
    OperatingCost,
    Convenience,
    Cockpit,
    Reliability,
    Cabin
  )

# ------------------------
# 3. Convert levels to ordinal scores
# ------------------------
price_map <- c(
  "RMB110k_130k" = 1,
  "RMB150k_170k" = 2,
  "RMB190k_210k" = 3
)

cost_map <- c(
  "RMB300" = 1,
  "RMB600" = 2,
  "RMB900" = 3
)

conven_map <- c(
  "Difficult" = 1,
  "Moderate" = 2,
  "Easy" = 3
)

cockpit_map <- c(
  "Basic" = 1,
  "Intermediate" = 2,
  "Advanced" = 3
)

reliab_map <- c(
  "Low" = 1,
  "Medium" = 2,
  "High" = 3
)

cabin_map <- c(
  "Basic" = 1,
  "Improved" = 2,
  "Premium" = 3
)

profiles_num <- profiles %>%
  mutate(
    Price_num = unname(price_map[Price]),
    OperatingCost_num = unname(cost_map[OperatingCost]),
    Convenience_num = unname(conven_map[Convenience]),
    Cockpit_num = unname(cockpit_map[Cockpit]),
    Reliability_num = unname(reliab_map[Reliability]),
    Cabin_num = unname(cabin_map[Cabin])
  )

# ------------------------
# 4. Split BEV and ICEV
# ------------------------
bev <- profiles_num %>% filter(Powertrain == "BEV")
ice <- profiles_num %>% filter(Powertrain == "ICEV")

cat("BEV count:\n")
print(nrow(bev))
cat("ICEV count:\n")
print(nrow(ice))

cat("\nUnique BEV profile_id count:\n")
print(length(unique(bev$profile_id)))
cat("Unique ICEV profile_id count:\n")
print(length(unique(ice$profile_id)))

# ------------------------
# 5. Build all possible BEV vs ICEV pairs
# ------------------------
pair_candidates <- merge(
  bev,
  ice,
  by = NULL,
  suffixes = c("_BEV", "_ICEV")
)

# ------------------------
# 6. Dominance check
# ------------------------
check_dominance <- function(price_a, cost_a, conven_a, cockpit_a, reliab_a, cabin_a,
                            price_b, cost_b, conven_b, cockpit_b, reliab_b, cabin_b) {
  a_better_or_equal <- c(
    price_a <= price_b,
    cost_a <= cost_b,
    conven_a >= conven_b,
    cockpit_a >= cockpit_b,
    reliab_a >= reliab_b,
    cabin_a >= cabin_b
  )
  
  a_strictly_better <- c(
    price_a < price_b,
    cost_a < cost_b,
    conven_a > conven_b,
    cockpit_a > cockpit_b,
    reliab_a > reliab_b,
    cabin_a > cabin_b
  )
  
  b_better_or_equal <- c(
    price_b <= price_a,
    cost_b <= cost_a,
    conven_b >= conven_a,
    cockpit_b >= cockpit_a,
    reliab_b >= reliab_a,
    cabin_b >= cabin_a
  )
  
  b_strictly_better <- c(
    price_b < price_a,
    cost_b < cost_a,
    conven_b > conven_a,
    cockpit_b > cockpit_a,
    reliab_b > reliab_a,
    cabin_b > cabin_a
  )
  
  a_dominates <- all(a_better_or_equal) && any(a_strictly_better)
  b_dominates <- all(b_better_or_equal) && any(b_strictly_better)
  
  if (a_dominates) return("BEV_dominates")
  if (b_dominates) return("ICEV_dominates")
  return("No")
}

pair_candidates$dominance_flag <- mapply(
  check_dominance,
  pair_candidates$Price_num_BEV,
  pair_candidates$OperatingCost_num_BEV,
  pair_candidates$Convenience_num_BEV,
  pair_candidates$Cockpit_num_BEV,
  pair_candidates$Reliability_num_BEV,
  pair_candidates$Cabin_num_BEV,
  pair_candidates$Price_num_ICEV,
  pair_candidates$OperatingCost_num_ICEV,
  pair_candidates$Convenience_num_ICEV,
  pair_candidates$Cockpit_num_ICEV,
  pair_candidates$Reliability_num_ICEV,
  pair_candidates$Cabin_num_ICEV
)

cat("\nAll candidate pair dominance summary:\n")
print(table(pair_candidates$dominance_flag))

# ------------------------
# 7. Remove dominant pairs
# ------------------------
pair_candidates <- pair_candidates %>%
  filter(dominance_flag == "No")

cat("\nRemaining non-dominant candidate pairs:\n")
print(nrow(pair_candidates))

# ------------------------
# 8. Pair scoring function
#    Lower score = better pair
# ------------------------
pair_candidates <- pair_candidates %>%
  mutate(
    price_diff = abs(Price_num_BEV - Price_num_ICEV),
    cost_diff = abs(OperatingCost_num_BEV - OperatingCost_num_ICEV),
    reliab_diff = abs(Reliability_num_BEV - Reliability_num_ICEV),
    cabin_diff = abs(Cabin_num_BEV - Cabin_num_ICEV),
    
    bev_cockpit_adv = ifelse(Cockpit_num_BEV > Cockpit_num_ICEV, 1, 0),
    bev_cost_adv = ifelse(OperatingCost_num_BEV < OperatingCost_num_ICEV, 1, 0),
    ice_conven_adv = ifelse(Convenience_num_ICEV > Convenience_num_BEV, 1, 0),
    
    tradeoff_bonus =
      1.5 * bev_cockpit_adv +
      1.0 * bev_cost_adv +
      2.0 * ice_conven_adv,
    
    score =
      4.0 * price_diff +
      1.5 * cost_diff +
      2.0 * reliab_diff +
      2.0 * cabin_diff -
      tradeoff_bonus
  ) %>%
  arrange(score, profile_id_BEV, profile_id_ICEV)

# ------------------------
# 9. Greedy matching without replacement
# ------------------------
used_bev <- character(0)
used_ice <- character(0)
selected_pairs <- list()

for (i in seq_len(nrow(pair_candidates))) {
  row_i <- pair_candidates[i, ]
  
  bev_id <- row_i$profile_id_BEV
  ice_id <- row_i$profile_id_ICEV
  
  if (!(bev_id %in% used_bev) && !(ice_id %in% used_ice)) {
    selected_pairs[[length(selected_pairs) + 1]] <- row_i
    used_bev <- c(used_bev, bev_id)
    used_ice <- c(used_ice, ice_id)
  }
  
  if (length(selected_pairs) == min(nrow(bev), nrow(ice))) {
    break
  }
}

pair_tasks <- bind_rows(selected_pairs)

cat("\nNumber of selected non-dominant tasks:\n")
print(nrow(pair_tasks))

# ------------------------
# 10. Add task IDs and blocks
# ------------------------
pair_tasks <- pair_tasks %>%
  mutate(task_id = sprintf("T%02d", 1:n()))

set.seed(123)
block_order <- sample(pair_tasks$task_id, length(pair_tasks$task_id), replace = FALSE)

block_map <- data.frame(
  task_id = block_order,
  block_id = rep(c("Block_A", "Block_B"), length.out = length(block_order)),
  stringsAsFactors = FALSE
)

pair_tasks <- pair_tasks %>%
  left_join(block_map, by = "task_id") %>%
  select(task_id, block_id, everything())

# ------------------------
# 11. Final dominance re-check
# ------------------------
pair_tasks$dominance_flag_final <- mapply(
  check_dominance,
  pair_tasks$Price_num_BEV,
  pair_tasks$OperatingCost_num_BEV,
  pair_tasks$Convenience_num_BEV,
  pair_tasks$Cockpit_num_BEV,
  pair_tasks$Reliability_num_BEV,
  pair_tasks$Cabin_num_BEV,
  pair_tasks$Price_num_ICEV,
  pair_tasks$OperatingCost_num_ICEV,
  pair_tasks$Convenience_num_ICEV,
  pair_tasks$Cockpit_num_ICEV,
  pair_tasks$Reliability_num_ICEV,
  pair_tasks$Cabin_num_ICEV
)

cat("\nFinal selected task dominance summary:\n")
print(table(pair_tasks$dominance_flag_final))

# ------------------------
# 12. Build wide export
# ------------------------
tasks_wide <- pair_tasks %>%
  transmute(
    task_id,
    block_id,
    dominance_flag = dominance_flag_final,
    pair_score = score,
    
    A_profile_id = profile_id_BEV,
    A_Price = Price_BEV,
    A_Powertrain = Powertrain_BEV,
    A_OperatingCost = OperatingCost_BEV,
    A_Convenience = Convenience_BEV,
    A_Cockpit = Cockpit_BEV,
    A_Reliability = Reliability_BEV,
    A_Cabin = Cabin_BEV,
    
    B_profile_id = profile_id_ICEV,
    B_Price = Price_ICEV,
    B_Powertrain = Powertrain_ICEV,
    B_OperatingCost = OperatingCost_ICEV,
    B_Convenience = Convenience_ICEV,
    B_Cockpit = Cockpit_ICEV,
    B_Reliability = Reliability_ICEV,
    B_Cabin = Cabin_ICEV
  )

# ------------------------
# 13. Build long export
# ------------------------
tasks_long_A <- pair_tasks %>%
  transmute(
    task_id,
    block_id,
    dominance_flag = dominance_flag_final,
    alternative_id = "A",
    profile_id = profile_id_BEV,
    Price = Price_BEV,
    Powertrain = Powertrain_BEV,
    OperatingCost = OperatingCost_BEV,
    Convenience = Convenience_BEV,
    Cockpit = Cockpit_BEV,
    Reliability = Reliability_BEV,
    Cabin = Cabin_BEV
  )

tasks_long_B <- pair_tasks %>%
  transmute(
    task_id,
    block_id,
    dominance_flag = dominance_flag_final,
    alternative_id = "B",
    profile_id = profile_id_ICEV,
    Price = Price_ICEV,
    Powertrain = Powertrain_ICEV,
    OperatingCost = OperatingCost_ICEV,
    Convenience = Convenience_ICEV,
    Cockpit = Cockpit_ICEV,
    Reliability = Reliability_ICEV,
    Cabin = Cabin_ICEV
  )

tasks_long <- bind_rows(tasks_long_A, tasks_long_B) %>%
  arrange(task_id, alternative_id)

# ------------------------
# 14. Diagnostics
# ------------------------
cat("\nBlock sizes:\n")
print(table(tasks_wide$block_id))

cat("\nPreview of tasks_wide:\n")
print(tasks_wide)

# ------------------------
# 15. Export
# ------------------------
write.csv(tasks_wide, "choice_tasks_wide_no_dominance.csv", row.names = FALSE)
write.csv(tasks_long, "choice_tasks_long_no_dominance.csv", row.names = FALSE)

table(pair_candidates$dominance_flag)
nrow(pair_tasks)
table(pair_tasks$dominance_flag_final)




library(dplyr)
library(igraph)

# ------------------------
# 1. Load profiles
# ------------------------
if (exists("d_opt_profiles")) {
  profiles <- d_opt_profiles
} else {
  profiles <- read.csv(
    "d_optimal_profiles_40_reduced_model.csv",
    stringsAsFactors = FALSE
  )
}

# ------------------------
# 2. Ensure profile_id exists
# ------------------------
if (!("profile_id" %in% names(profiles))) {
  profiles <- profiles %>%
    mutate(profile_id = sprintf("P%03d", 1:n()))
}

profiles <- profiles %>%
  select(
    profile_id,
    Price,
    Powertrain,
    OperatingCost,
    Convenience,
    Cockpit,
    Reliability,
    Cabin
  )

# ------------------------
# 3. Convert levels to ordinal scores
# ------------------------
price_map <- c(
  "RMB110k_130k" = 1,
  "RMB150k_170k" = 2,
  "RMB190k_210k" = 3
)

cost_map <- c(
  "RMB300" = 1,
  "RMB600" = 2,
  "RMB900" = 3
)

conven_map <- c(
  "Difficult" = 1,
  "Moderate" = 2,
  "Easy" = 3
)

cockpit_map <- c(
  "Basic" = 1,
  "Intermediate" = 2,
  "Advanced" = 3
)

reliab_map <- c(
  "Low" = 1,
  "Medium" = 2,
  "High" = 3
)

cabin_map <- c(
  "Basic" = 1,
  "Improved" = 2,
  "Premium" = 3
)

profiles_num <- profiles %>%
  mutate(
    Price_num = unname(price_map[Price]),
    OperatingCost_num = unname(cost_map[OperatingCost]),
    Convenience_num = unname(conven_map[Convenience]),
    Cockpit_num = unname(cockpit_map[Cockpit]),
    Reliability_num = unname(reliab_map[Reliability]),
    Cabin_num = unname(cabin_map[Cabin])
  )

# ------------------------
# 4. Split BEV and ICEV
# ------------------------
bev <- profiles_num %>% filter(Powertrain == "BEV")
ice <- profiles_num %>% filter(Powertrain == "ICEV")

cat("BEV count:\n")
print(nrow(bev))
cat("ICEV count:\n")
print(nrow(ice))

# ------------------------
# 5. Build all BEV vs ICEV candidate pairs
# ------------------------
pair_candidates <- merge(
  bev,
  ice,
  by = NULL,
  suffixes = c("_BEV", "_ICEV")
)

# ------------------------
# 6. Dominance check
# ------------------------
check_dominance <- function(price_a, cost_a, conven_a, cockpit_a, reliab_a, cabin_a,
                            price_b, cost_b, conven_b, cockpit_b, reliab_b, cabin_b) {
  a_better_or_equal <- c(
    price_a <= price_b,
    cost_a <= cost_b,
    conven_a >= conven_b,
    cockpit_a >= cockpit_b,
    reliab_a >= reliab_b,
    cabin_a >= cabin_b
  )
  
  a_strictly_better <- c(
    price_a < price_b,
    cost_a < cost_b,
    conven_a > conven_b,
    cockpit_a > cockpit_b,
    reliab_a > reliab_b,
    cabin_a > cabin_b
  )
  
  b_better_or_equal <- c(
    price_b <= price_a,
    cost_b <= cost_a,
    conven_b >= conven_a,
    cockpit_b >= cockpit_a,
    reliab_b >= reliab_a,
    cabin_b >= cabin_a
  )
  
  b_strictly_better <- c(
    price_b < price_a,
    cost_b < cost_a,
    conven_b > conven_a,
    cockpit_b > cockpit_a,
    reliab_b > reliab_a,
    cabin_b > cabin_a
  )
  
  a_dominates <- all(a_better_or_equal) && any(a_strictly_better)
  b_dominates <- all(b_better_or_equal) && any(b_strictly_better)
  
  if (a_dominates) return("BEV_dominates")
  if (b_dominates) return("ICEV_dominates")
  return("No")
}

pair_candidates$dominance_flag <- mapply(
  check_dominance,
  pair_candidates$Price_num_BEV,
  pair_candidates$OperatingCost_num_BEV,
  pair_candidates$Convenience_num_BEV,
  pair_candidates$Cockpit_num_BEV,
  pair_candidates$Reliability_num_BEV,
  pair_candidates$Cabin_num_BEV,
  pair_candidates$Price_num_ICEV,
  pair_candidates$OperatingCost_num_ICEV,
  pair_candidates$Convenience_num_ICEV,
  pair_candidates$Cockpit_num_ICEV,
  pair_candidates$Reliability_num_ICEV,
  pair_candidates$Cabin_num_ICEV
)

cat("\nAll candidate pair dominance summary:\n")
print(table(pair_candidates$dominance_flag))

pair_candidates <- pair_candidates %>%
  filter(dominance_flag == "No")

cat("\nRemaining non-dominant candidate pairs:\n")
print(nrow(pair_candidates))

# ------------------------
# 7. Pair quality score
#    Lower score = better pair
# ------------------------
pair_candidates <- pair_candidates %>%
  mutate(
    price_diff = abs(Price_num_BEV - Price_num_ICEV),
    cost_diff = abs(OperatingCost_num_BEV - OperatingCost_num_ICEV),
    reliab_diff = abs(Reliability_num_BEV - Reliability_num_ICEV),
    cabin_diff = abs(Cabin_num_BEV - Cabin_num_ICEV),
    
    bev_cockpit_adv = ifelse(Cockpit_num_BEV > Cockpit_num_ICEV, 1, 0),
    bev_cost_adv = ifelse(OperatingCost_num_BEV < OperatingCost_num_ICEV, 1, 0),
    ice_conven_adv = ifelse(Convenience_num_ICEV > Convenience_num_BEV, 1, 0),
    
    tradeoff_bonus =
      1.5 * bev_cockpit_adv +
      1.0 * bev_cost_adv +
      2.0 * ice_conven_adv,
    
    score =
      4.0 * price_diff +
      1.5 * cost_diff +
      2.0 * reliab_diff +
      2.0 * cabin_diff -
      tradeoff_bonus
  )

# ------------------------
# 8. Build bipartite graph for maximum matching
# ------------------------
edge_df <- pair_candidates %>%
  transmute(
    from = paste0("BEV_", profile_id_BEV),
    to = paste0("ICEV_", profile_id_ICEV),
    score = score
  )

vertices_df <- data.frame(
  name = c(
    paste0("BEV_", bev$profile_id),
    paste0("ICEV_", ice$profile_id)
  ),
  type = c(
    rep(TRUE, nrow(bev)),
    rep(FALSE, nrow(ice))
  ),
  stringsAsFactors = FALSE
)

g <- graph_from_data_frame(
  d = edge_df,
  vertices = vertices_df,
  directed = FALSE
)

# ------------------------
# 9. Maximum cardinality matching
# ------------------------
m <- max_bipartite_match(g)

cat("\nMaximum matching size:\n")
print(m$matching_size)

vertex_names <- igraph::as_ids(V(g))
partner_names <- m$matching

cat("\nClass of partner_names:\n")
print(class(partner_names))

cat("\nLength of vertex_names:\n")
print(length(vertex_names))

cat("\nLength of partner_names:\n")
print(length(partner_names))

match_df <- data.frame(
  vertex = vertex_names,
  partner = partner_names,
  stringsAsFactors = FALSE
) %>%
  filter(!is.na(partner)) %>%
  filter(grepl("^BEV_", vertex)) %>%
  transmute(
    profile_id_BEV = sub("^BEV_", "", vertex),
    profile_id_ICEV = sub("^ICEV_", "", partner)
  )

cat("\nRecovered matched pairs:\n")
print(nrow(match_df))
print(match_df)

# ------------------------
# 10. Recover selected matched pairs with scores
# ------------------------
pair_tasks <- pair_candidates %>%
  inner_join(match_df, by = c("profile_id_BEV", "profile_id_ICEV")) %>%
  arrange(score, profile_id_BEV, profile_id_ICEV)

cat("\nNumber of selected non-dominant tasks from maximum matching:\n")
print(nrow(pair_tasks))

if (nrow(pair_tasks) == 0) {
  stop("pair_tasks is empty after joining recovered matching results.")
}

# ------------------------
# 11. Final dominance re-check
# ------------------------
pair_tasks$dominance_flag_final <- mapply(
  check_dominance,
  pair_tasks$Price_num_BEV,
  pair_tasks$OperatingCost_num_BEV,
  pair_tasks$Convenience_num_BEV,
  pair_tasks$Cockpit_num_BEV,
  pair_tasks$Reliability_num_BEV,
  pair_tasks$Cabin_num_BEV,
  pair_tasks$Price_num_ICEV,
  pair_tasks$OperatingCost_num_ICEV,
  pair_tasks$Convenience_num_ICEV,
  pair_tasks$Cockpit_num_ICEV,
  pair_tasks$Reliability_num_ICEV,
  pair_tasks$Cabin_num_ICEV
)

cat("\nFinal selected task dominance summary:\n")
print(table(pair_tasks$dominance_flag_final))

# ------------------------
# 12. Add task IDs and blocks
# ------------------------
pair_tasks <- pair_tasks %>%
  mutate(task_id = sprintf("T%02d", 1:n()))

set.seed(123)
block_order <- sample(pair_tasks$task_id, length(pair_tasks$task_id), replace = FALSE)

block_map <- data.frame(
  task_id = block_order,
  block_id = rep(c("Block_A", "Block_B"), length.out = length(block_order)),
  stringsAsFactors = FALSE
)

pair_tasks <- pair_tasks %>%
  left_join(block_map, by = "task_id") %>%
  select(task_id, block_id, everything())

# ------------------------
# 13. Export wide format
# ------------------------
tasks_wide <- pair_tasks %>%
  transmute(
    task_id,
    block_id,
    dominance_flag = dominance_flag_final,
    pair_score = score,
    
    A_profile_id = profile_id_BEV,
    A_Price = Price_BEV,
    A_Powertrain = Powertrain_BEV,
    A_OperatingCost = OperatingCost_BEV,
    A_Convenience = Convenience_BEV,
    A_Cockpit = Cockpit_BEV,
    A_Reliability = Reliability_BEV,
    A_Cabin = Cabin_BEV,
    
    B_profile_id = profile_id_ICEV,
    B_Price = Price_ICEV,
    B_Powertrain = Powertrain_ICEV,
    B_OperatingCost = OperatingCost_ICEV,
    B_Convenience = Convenience_ICEV,
    B_Cockpit = Cockpit_ICEV,
    B_Reliability = Reliability_ICEV,
    B_Cabin = Cabin_ICEV
  )

# ------------------------
# 14. Export long format
# ------------------------
tasks_long_A <- pair_tasks %>%
  transmute(
    task_id,
    block_id,
    dominance_flag = dominance_flag_final,
    alternative_id = "A",
    profile_id = profile_id_BEV,
    Price = Price_BEV,
    Powertrain = Powertrain_BEV,
    OperatingCost = OperatingCost_BEV,
    Convenience = Convenience_BEV,
    Cockpit = Cockpit_BEV,
    Reliability = Reliability_BEV,
    Cabin = Cabin_BEV
  )

tasks_long_B <- pair_tasks %>%
  transmute(
    task_id,
    block_id,
    dominance_flag = dominance_flag_final,
    alternative_id = "B",
    profile_id = profile_id_ICEV,
    Price = Price_ICEV,
    Powertrain = Powertrain_ICEV,
    OperatingCost = OperatingCost_ICEV,
    Convenience = Convenience_ICEV,
    Cockpit = Cockpit_ICEV,
    Reliability = Reliability_ICEV,
    Cabin = Cabin_ICEV
  )

tasks_long <- bind_rows(tasks_long_A, tasks_long_B) %>%
  arrange(task_id, alternative_id)

# ------------------------
# 15. Diagnostics
# ------------------------
cat("\nBlock sizes:\n")
print(table(tasks_wide$block_id))

cat("\nPreview of tasks_wide:\n")
print(tasks_wide)

# ------------------------
# 16. Save files
# ------------------------
write.csv(tasks_wide, "choice_tasks_wide_max_matching.csv", row.names = FALSE)
write.csv(tasks_long, "choice_tasks_long_max_matching.csv", row.names = FALSE)

m$matching_size
nrow(pair_tasks)
table(pair_tasks$dominance_flag_final)




library(jsonlite)
library(dplyr)
library(tidyr)

# ------------------------
# 1. File paths
# ------------------------
qsf_template_file <- "Survey_final.qsf"
tasks_csv_file <- "choice_tasks_long_max_matching.csv"
out_qsf_file <- "Survey_final_filled.qsf"

# ------------------------
# 2. Read inputs
# ------------------------
qsf_txt <- paste(readLines(qsf_template_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
qsf <- fromJSON(qsf_txt, simplifyVector = FALSE)

tasks_long <- read.csv(tasks_csv_file, stringsAsFactors = FALSE)

# ------------------------
# 3. Validate CSV columns
# ------------------------
required_cols <- c(
  "task_id", "block_id", "alternative_id", "profile_id",
  "Price", "Powertrain", "OperatingCost", "Convenience",
  "Cockpit", "Reliability", "Cabin"
)

missing_cols <- setdiff(required_cols, names(tasks_long))
if (length(missing_cols) > 0) {
  stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
}

# ------------------------
# 4. Reshape long -> wide
# ------------------------
tasks_wide <- tasks_long %>%
  filter(alternative_id %in% c("A", "B")) %>%
  arrange(block_id, task_id, alternative_id) %>%
  pivot_wider(
    id_cols = c(task_id, block_id),
    names_from = alternative_id,
    values_from = c(profile_id, Price, Powertrain, OperatingCost, Convenience, Cockpit, Reliability, Cabin),
    names_sep = "_"
  ) %>%
  arrange(block_id, task_id)

# ------------------------
# 5. Helpers
# ------------------------
html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}

make_task_html <- function(row) {
  paste0(
    "<p><strong>", html_escape(row[["task_id"]]), "</strong></p>",
    "<table style='width:100%; border-collapse:collapse; font-size:14px;'>",
    "<tr>",
    "<th style='border:1px solid #999; padding:8px; width:34%; background:#f2f2f2;'>Attribute</th>",
    "<th style='border:1px solid #999; padding:8px; width:33%; background:#f9f9f9;'>Vehicle A</th>",
    "<th style='border:1px solid #999; padding:8px; width:33%; background:#f9f9f9;'>Vehicle B</th>",
    "</tr>",
    "<tr><td style='border:1px solid #999; padding:8px;'>Price</td>",
    "<td style='border:1px solid #999; padding:8px;'>", html_escape(row[["Price_A"]]), "</td>",
    "<td style='border:1px solid #999; padding:8px;'>", html_escape(row[["Price_B"]]), "</td></tr>",
    "<tr><td style='border:1px solid #999; padding:8px;'>Powertrain</td>",
    "<td style='border:1px solid #999; padding:8px;'>", html_escape(row[["Powertrain_A"]]), "</td>",
    "<td style='border:1px solid #999; padding:8px;'>", html_escape(row[["Powertrain_B"]]), "</td></tr>",
    "<tr><td style='border:1px solid #999; padding:8px;'>Operating cost</td>",
    "<td style='border:1px solid #999; padding:8px;'>", html_escape(row[["OperatingCost_A"]]), "</td>",
    "<td style='border:1px solid #999; padding:8px;'>", html_escape(row[["OperatingCost_B"]]), "</td></tr>",
    "<tr><td style='border:1px solid #999; padding:8px;'>Convenience</td>",
    "<td style='border:1px solid #999; padding:8px;'>", html_escape(row[["Convenience_A"]]), "</td>",
    "<td style='border:1px solid #999; padding:8px;'>", html_escape(row[["Convenience_B"]]), "</td></tr>",
    "<tr><td style='border:1px solid #999; padding:8px;'>Cockpit</td>",
    "<td style='border:1px solid #999; padding:8px;'>", html_escape(row[["Cockpit_A"]]), "</td>",
    "<td style='border:1px solid #999; padding:8px;'>", html_escape(row[["Cockpit_B"]]), "</td></tr>",
    "<tr><td style='border:1px solid #999; padding:8px;'>Reliability</td>",
    "<td style='border:1px solid #999; padding:8px;'>", html_escape(row[["Reliability_A"]]), "</td>",
    "<td style='border:1px solid #999; padding:8px;'>", html_escape(row[["Reliability_B"]]), "</td></tr>",
    "<tr><td style='border:1px solid #999; padding:8px;'>Cabin</td>",
    "<td style='border:1px solid #999; padding:8px;'>", html_escape(row[["Cabin_A"]]), "</td>",
    "<td style='border:1px solid #999; padding:8px;'>", html_escape(row[["Cabin_B"]]), "</td></tr>",
    "</table>",
    "<p style='margin-top:12px;'><strong>Which vehicle would you choose?</strong></p>"
  )
}

make_mc_question <- function(survey_id, qid, question_text, data_tag) {
  list(
    SurveyID = survey_id,
    Element = "SQ",
    PrimaryAttribute = qid,
    SecondaryAttribute = paste("Task", data_tag),
    TertiaryAttribute = NULL,
    Payload = list(
      QuestionText = question_text,
      DefaultChoices = FALSE,
      DataExportTag = data_tag,
      QuestionType = "MC",
      Selector = "SAVR",
      SubSelector = "TX",
      DataVisibility = list(Private = FALSE, Hidden = FALSE),
      Configuration = list(QuestionDescriptionOption = "UseText"),
      QuestionDescription = data_tag,
      Choices = list(
        "1" = list(Display = "Vehicle A"),
        "2" = list(Display = "Vehicle B")
      ),
      ChoiceOrder = list("1", "2"),
      Validation = list(Settings = list(ForceResponse = "OFF", Type = "None")),
      GradingData = list(),
      Language = list(),
      NextChoiceId = 3,
      NextAnswerId = 1,
      QuestionID = qid
    )
  )
}

# ------------------------
# 6. Locate template elements
# ------------------------
survey_elements <- qsf$SurveyElements

bl_idx <- which(vapply(survey_elements, function(x) identical(x$Element, "BL"), logical(1)))[1]
fl_idx <- which(vapply(survey_elements, function(x) identical(x$Element, "FL"), logical(1)))[1]
qc_idx <- which(vapply(survey_elements, function(x) identical(x$Element, "QC"), logical(1)))[1]
sq_idxs <- which(vapply(survey_elements, function(x) identical(x$Element, "SQ"), logical(1)))

if (is.na(bl_idx) || is.na(fl_idx) || length(sq_idxs) == 0) {
  stop("Template QSF does not contain expected BL/FL/SQ elements.")
}

survey_id <- qsf$SurveyEntry$SurveyID
blocks <- survey_elements[[bl_idx]]$Payload
block_desc <- vapply(blocks, function(x) x$Description, character(1))

idx_block_a <- which(block_desc == "Block_A")
idx_block_b <- which(block_desc == "Block_B")

if (length(idx_block_a) != 1 || length(idx_block_b) != 1) {
  stop("Template QSF must contain Block_A and Block_B.")
}

# ------------------------
# 7. Preserve the first SQ from template, replace the rest
# ------------------------
existing_sq <- survey_elements[sq_idxs]
first_sq <- existing_sq[[1]]
first_sq$SurveyID <- survey_id

new_sq_elements <- list(first_sq)

next_q_num <- 2L
make_qid <- function() {
  qid <- paste0("QID", next_q_num)
  next_q_num <<- next_q_num + 1L
  qid
}

block_a_qids <- character(0)
block_b_qids <- character(0)

for (i in seq_len(nrow(tasks_wide))) {
  row_i <- tasks_wide[i, ]
  qid <- make_qid()
  
  sq <- make_mc_question(
    survey_id = survey_id,
    qid = qid,
    question_text = make_task_html(row_i),
    data_tag = paste0("TASK_", row_i[["task_id"]])
  )
  
  new_sq_elements[[length(new_sq_elements) + 1]] <- sq
  
  if (identical(row_i[["block_id"]], "Block_A")) {
    block_a_qids <- c(block_a_qids, qid)
  } else if (identical(row_i[["block_id"]], "Block_B")) {
    block_b_qids <- c(block_b_qids, qid)
  }
}

# ------------------------
# 8. Update block contents
# ------------------------
blocks[[idx_block_a]]$BlockElements <- lapply(block_a_qids, function(qid) {
  list(Type = "Question", QuestionID = qid)
})

blocks[[idx_block_b]]$BlockElements <- lapply(block_b_qids, function(qid) {
  list(Type = "Question", QuestionID = qid)
})

survey_elements[[bl_idx]]$Payload <- blocks

# ------------------------
# 9. Update QC
# ------------------------
if (!is.na(qc_idx)) {
  survey_elements[[qc_idx]]$SecondaryAttribute <- as.character(length(new_sq_elements))
}

# ------------------------
# 10. Replace SQ section
# ------------------------
non_sq_elements <- survey_elements[-sq_idxs]
qsf$SurveyElements <- c(non_sq_elements, new_sq_elements)

# ------------------------
# 11. Write output
# ------------------------
write_json(
  qsf,
  out_qsf_file,
  pretty = TRUE,
  auto_unbox = TRUE,
  null = "null"
)

cat("Wrote", out_qsf_file, "\n")
cat("Tasks inserted:", nrow(tasks_wide), "\n")
cat("Block_A tasks:", length(block_a_qids), "\n")
cat("Block_B tasks:", length(block_b_qids), "\n")




# 对调题目 更平均
library(dplyr)
library(tidyr)

# ------------------------
# 1. Read current task file
# ------------------------
tasks_long <- read.csv("choice_tasks_long_max_matching.csv", stringsAsFactors = FALSE)

# ------------------------
# 2. Helper: summarize block distributions
# ------------------------
summarize_blocks <- function(df) {
  dims <- c("Price", "Convenience", "Cockpit", "Reliability")
  out <- list()
  
  for (d in dims) {
    tab <- df %>%
      count(block_id, .data[[d]]) %>%
      tidyr::pivot_wider(
        names_from = .data[[d]],
        values_from = n,
        values_fill = 0
      ) %>%
      mutate(dimension = d, .before = 1)
    out[[d]] <- tab
  }
  
  bind_rows(out)
}

cat("Before swap:\n")
print(summarize_blocks(tasks_long))

# ------------------------
# 3. Swap tasks across blocks
#    Recommended swaps:
#    T06 <-> T01
#    T15 <-> T04
# ------------------------
swap_map <- c(
  "T06" = "Block_B",
  "T01" = "Block_A",
  "T15" = "Block_B",
  "T04" = "Block_A"
)

tasks_long_balanced <- tasks_long %>%
  mutate(
    block_id = ifelse(task_id %in% names(swap_map),
                      unname(swap_map[task_id]),
                      block_id)
  )

cat("\nAfter swap:\n")
print(summarize_blocks(tasks_long_balanced))

# ------------------------
# 4. Check task counts by block
# ------------------------
cat("\nTask counts by block after swap:\n")
print(tasks_long_balanced %>%
        distinct(task_id, block_id) %>%
        count(block_id))

# ------------------------
# 5. Save updated CSV
# ------------------------
write.csv(
  tasks_long_balanced,
  "choice_tasks_long_max_matching_balanced.csv",
  row.names = FALSE
)

cat("\nUpdated file written to: choice_tasks_long_max_matching_balanced.csv\n")

library(jsonlite)
library(dplyr)
library(tidyr)

# ------------------------
# 1. File paths
# ------------------------
qsf_template_file <- "Survey_final.qsf"
tasks_csv_file <- "choice_tasks_long_max_matching_balanced.csv"
out_qsf_file <- "Survey_final_filled_V2.qsf"

# ------------------------
# 2. Read inputs
# ------------------------
qsf_txt <- paste(readLines(qsf_template_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
qsf <- fromJSON(qsf_txt, simplifyVector = FALSE)

tasks_long <- read.csv(tasks_csv_file, stringsAsFactors = FALSE)

# ------------------------
# 3. Validate CSV columns
# ------------------------
required_cols <- c(
  "task_id", "block_id", "alternative_id", "profile_id",
  "Price", "Powertrain", "OperatingCost", "Convenience",
  "Cockpit", "Reliability", "Cabin"
)

missing_cols <- setdiff(required_cols, names(tasks_long))
if (length(missing_cols) > 0) {
  stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
}

# ------------------------
# 4. Reshape long -> wide
# ------------------------
tasks_wide <- tasks_long %>%
  filter(alternative_id %in% c("A", "B")) %>%
  arrange(block_id, task_id, alternative_id) %>%
  pivot_wider(
    id_cols = c(task_id, block_id),
    names_from = alternative_id,
    values_from = c(profile_id, Price, Powertrain, OperatingCost, Convenience, Cockpit, Reliability, Cabin),
    names_sep = "_"
  ) %>%
  arrange(block_id, task_id)

# ------------------------
# 5. Helpers
# ------------------------
html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}

make_task_html <- function(row) {
  paste0(
    "<p><strong>", html_escape(row[["task_id"]]), "</strong></p>",
    "<table style='width:100%; border-collapse:collapse; font-size:14px;'>",
    "<tr>",
    "<th style='border:1px solid #999; padding:8px; width:34%; background:#f2f2f2;'>Attribute</th>",
    "<th style='border:1px solid #999; padding:8px; width:33%; background:#f9f9f9;'>Vehicle A</th>",
    "<th style='border:1px solid #999; padding:8px; width:33%; background:#f9f9f9;'>Vehicle B</th>",
    "</tr>",
    "<tr><td style='border:1px solid #999; padding:8px;'>Price</td>",
    "<td style='border:1px solid #999; padding:8px;'>", html_escape(row[["Price_A"]]), "</td>",
    "<td style='border:1px solid #999; padding:8px;'>", html_escape(row[["Price_B"]]), "</td></tr>",
    "<tr><td style='border:1px solid #999; padding:8px;'>Powertrain</td>",
    "<td style='border:1px solid #999; padding:8px;'>", html_escape(row[["Powertrain_A"]]), "</td>",
    "<td style='border:1px solid #999; padding:8px;'>", html_escape(row[["Powertrain_B"]]), "</td></tr>",
    "<tr><td style='border:1px solid #999; padding:8px;'>Operating cost</td>",
    "<td style='border:1px solid #999; padding:8px;'>", html_escape(row[["OperatingCost_A"]]), "</td>",
    "<td style='border:1px solid #999; padding:8px;'>", html_escape(row[["OperatingCost_B"]]), "</td></tr>",
    "<tr><td style='border:1px solid #999; padding:8px;'>Convenience</td>",
    "<td style='border:1px solid #999; padding:8px;'>", html_escape(row[["Convenience_A"]]), "</td>",
    "<td style='border:1px solid #999; padding:8px;'>", html_escape(row[["Convenience_B"]]), "</td></tr>",
    "<tr><td style='border:1px solid #999; padding:8px;'>Cockpit</td>",
    "<td style='border:1px solid #999; padding:8px;'>", html_escape(row[["Cockpit_A"]]), "</td>",
    "<td style='border:1px solid #999; padding:8px;'>", html_escape(row[["Cockpit_B"]]), "</td></tr>",
    "<tr><td style='border:1px solid #999; padding:8px;'>Reliability</td>",
    "<td style='border:1px solid #999; padding:8px;'>", html_escape(row[["Reliability_A"]]), "</td>",
    "<td style='border:1px solid #999; padding:8px;'>", html_escape(row[["Reliability_B"]]), "</td></tr>",
    "<tr><td style='border:1px solid #999; padding:8px;'>Cabin</td>",
    "<td style='border:1px solid #999; padding:8px;'>", html_escape(row[["Cabin_A"]]), "</td>",
    "<td style='border:1px solid #999; padding:8px;'>", html_escape(row[["Cabin_B"]]), "</td></tr>",
    "</table>",
    "<p style='margin-top:12px;'><strong>Which vehicle would you choose?</strong></p>"
  )
}

make_mc_question <- function(survey_id, qid, question_text, data_tag) {
  list(
    SurveyID = survey_id,
    Element = "SQ",
    PrimaryAttribute = qid,
    SecondaryAttribute = paste("Task", data_tag),
    TertiaryAttribute = NULL,
    Payload = list(
      QuestionText = question_text,
      DefaultChoices = FALSE,
      DataExportTag = data_tag,
      QuestionType = "MC",
      Selector = "SAVR",
      SubSelector = "TX",
      DataVisibility = list(Private = FALSE, Hidden = FALSE),
      Configuration = list(QuestionDescriptionOption = "UseText"),
      QuestionDescription = data_tag,
      Choices = list(
        "1" = list(Display = "Vehicle A"),
        "2" = list(Display = "Vehicle B")
      ),
      ChoiceOrder = list("1", "2"),
      Validation = list(Settings = list(ForceResponse = "OFF", Type = "None")),
      GradingData = list(),
      Language = list(),
      NextChoiceId = 3,
      NextAnswerId = 1,
      QuestionID = qid
    )
  )
}

# ------------------------
# 6. Locate template elements
# ------------------------
survey_elements <- qsf$SurveyElements

bl_idx <- which(vapply(survey_elements, function(x) identical(x$Element, "BL"), logical(1)))[1]
fl_idx <- which(vapply(survey_elements, function(x) identical(x$Element, "FL"), logical(1)))[1]
qc_idx <- which(vapply(survey_elements, function(x) identical(x$Element, "QC"), logical(1)))[1]
sq_idxs <- which(vapply(survey_elements, function(x) identical(x$Element, "SQ"), logical(1)))

if (is.na(bl_idx) || is.na(fl_idx) || length(sq_idxs) == 0) {
  stop("Template QSF does not contain expected BL/FL/SQ elements.")
}

survey_id <- qsf$SurveyEntry$SurveyID
blocks <- survey_elements[[bl_idx]]$Payload
block_desc <- vapply(blocks, function(x) x$Description, character(1))

idx_block_a <- which(block_desc == "Block_A")
idx_block_b <- which(block_desc == "Block_B")

if (length(idx_block_a) != 1 || length(idx_block_b) != 1) {
  stop("Template QSF must contain Block_A and Block_B.")
}

# ------------------------
# 7. Preserve the first SQ from template, replace the rest
# ------------------------
existing_sq <- survey_elements[sq_idxs]
first_sq <- existing_sq[[1]]
first_sq$SurveyID <- survey_id

new_sq_elements <- list(first_sq)

next_q_num <- 2L
make_qid <- function() {
  qid <- paste0("QID", next_q_num)
  next_q_num <<- next_q_num + 1L
  qid
}

block_a_qids <- character(0)
block_b_qids <- character(0)

for (i in seq_len(nrow(tasks_wide))) {
  row_i <- tasks_wide[i, ]
  qid <- make_qid()
  
  sq <- make_mc_question(
    survey_id = survey_id,
    qid = qid,
    question_text = make_task_html(row_i),
    data_tag = paste0("TASK_", row_i[["task_id"]])
  )
  
  new_sq_elements[[length(new_sq_elements) + 1]] <- sq
  
  if (identical(row_i[["block_id"]], "Block_A")) {
    block_a_qids <- c(block_a_qids, qid)
  } else if (identical(row_i[["block_id"]], "Block_B")) {
    block_b_qids <- c(block_b_qids, qid)
  }
}

# ------------------------
# 8. Update block contents
# ------------------------
blocks[[idx_block_a]]$BlockElements <- lapply(block_a_qids, function(qid) {
  list(Type = "Question", QuestionID = qid)
})

blocks[[idx_block_b]]$BlockElements <- lapply(block_b_qids, function(qid) {
  list(Type = "Question", QuestionID = qid)
})

survey_elements[[bl_idx]]$Payload <- blocks

# ------------------------
# 9. Update QC
# ------------------------
if (!is.na(qc_idx)) {
  survey_elements[[qc_idx]]$SecondaryAttribute <- as.character(length(new_sq_elements))
}

# ------------------------
# 10. Replace SQ section
# ------------------------
non_sq_elements <- survey_elements[-sq_idxs]
qsf$SurveyElements <- c(non_sq_elements, new_sq_elements)

# ------------------------
# 11. Write output
# ------------------------
write_json(
  qsf,
  out_qsf_file,
  pretty = TRUE,
  auto_unbox = TRUE,
  null = "null"
)





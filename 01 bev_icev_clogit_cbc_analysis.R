# ============================================================
#  Conjoint Analysis of Vehicle Choice Experiment
# ------------------------------------------------------------
# This script processes raw survey data, cleans and transforms
# respondent-level and choice-task-level variables, and merges
# them with experimental design attributes. It converts the
# dataset into long format suitable for discrete choice modeling.
#
# The workflow includes:
# 1) Data cleaning and respondent filtering
# 2) Translation of survey responses into English
# 3) Construction of unique respondent and choice set IDs
# 4) Reshaping data into alternative-level long format
# 5) Merging experimental design attributes
# 6) Variable type conversion and recoding
# 7) Preparation of modeling datasets for conditional logit (clogit)
#
# The final output is a structured dataset ready for:
# - Conditional logit models
# - Interaction and moderation analysis
# - Robustness checks
# - Heterogeneity (subgroup) analysis
# ============================================================

# ============================================================
#  Data cleaning and respondent filtering
# ============================================================
# Load required packages
library(dplyr)
library(tidyr)
library(readr)
library(stringr)

# -------------------------------
# 1 Read raw questionnaire data
# -------------------------------
questionnaire <- read_csv("questionnaire_data.csv")

# -------------------------------
# 2 Remove first two metadata rows
# -------------------------------
questionnaire <- questionnaire[-c(1,2), ]

# -------------------------------
# 3 Keep completed responses only (remove Survey Preview data)
# -------------------------------
questionnaire <- questionnaire %>%
  filter(Finished == "True", Status != "Survey Preview")


# -------------------------------
# 4 Remove irrelevant columns
# -------------------------------
cols_to_remove <- c(
  "StartDate","EndDate","Status","IPAddress","Progress","Duration (in seconds)",
  "RecordedDate","ResponseId","RecipientLastName","RecipientFirstName",
  "RecipientEmail","ExternalReference","LocationLatitude","LocationLongitude",
  "DistributionChannel","UserLanguage","Q2"
)
questionnaire <- questionnaire %>% select(-all_of(cols_to_remove))


# -------------------------------
# 5 Translate survey variables Q3–Q11 into English
# -------------------------------
questionnaire <- questionnaire %>%
  mutate(
    Q1 = case_when(Q8 == "是" ~ "Yes", Q1 == "否" ~ "No", 
                   Q1 == "不确定" ~ "No Sure",TRUE ~ Q1),
    Q3 = case_when(Q3 == "18-24岁" ~ "18-24 years",
                   Q3 == "25-29岁" ~ "25-29 years",
                   Q3 == "30-39岁" ~ "35-39 years",
                   Q3 == "40岁及以上" ~ "40+ years",
                   TRUE ~ Q3),
    Q4 = case_when(Q4 == "男" ~ "Male", Q4 == "女" ~ "Female", TRUE ~ Q4),
    Q5 = case_when(Q5 == "大专及以下" ~ "College or below",
                   Q5 == "本科" ~ "Bachelor",
                   Q5 == "硕士及以上" ~ "Master or above",
                   TRUE ~ Q5),
    Q6 = case_when(Q6 == "8000元及以下" ~ "< 8,000 RMVB",
                   Q6 == "8,001–15,000元" ~ "8,001-15,000 RMB",
                   Q6 == "15,001-25,000" ~ "15,001-15,000 RMB",
                   Q6 == "25,000元以上" ~ ">25,000 RMB",
                   
                   TRUE ~ Q6),
    Q7 = case_when(Q7 == "一线/新一线城市" ~ "Tier1/NewTier1 city",
                   Q7 == "二线城" ~ "Tier2 city",
                   Q7 == "三线及以下城市" ~ "Tier3 or lower city",
                   Q7 == "县城/乡镇" ~ "Villages",
                   TRUE ~ Q7),
    Q8 = case_when(Q8 == "是" ~ "Yes", Q8 == "否" ~ "No", TRUE ~ Q8),
    Q9 = case_when(Q9 == "是" ~ "Yes", Q9 == "否" ~ "No", TRUE ~ Q9),
    Q10 = case_when(Q10 == "10公里以内" ~ "<=10 km",
                    Q10 == "11 – 30公里" ~ "11-30 km",
                    Q10 == "30公里以上" ~ ">30 km",
                    Q10 == "基本无通勤" ~ "No commuting",
                    TRUE ~ Q10),
    Q11 = case_when(Q11 == "是" ~ "Yes", Q11 == "否" ~ "No", TRUE ~ Q11)
  )

# -------------------------------
#  Create unique respondent ID
# -------------------------------
questionnaire <- questionnaire %>%
  mutate(RespondentID = row_number())

# -------------------------------
# 7 Remove rows with all TA/TB choice items missing
# -------------------------------
ta_tb_cols <- grep("^T[AB]", names(questionnaire), value = TRUE)

questionnaire <- questionnaire %>%
  filter(rowSums(!is.na(select(., all_of(ta_tb_cols)))) > 0)

cat("Remaining respondents", nrow(questionnaire), "\n")

write_csv(questionnaire, "questionnaire_long_cleaned.csv")



# ============================================================
#  Construct choice-task long dataset from survey responses
# ============================================================

# -------------------------------
# 1. Load cleaned questionnaire data
# -------------------------------
questionnaire <- read_csv("questionnaire_long_cleaned.csv")

# -------------------------------
#  2. Load experimental design data
# -------------------------------
experiment_design <- read_csv("task_design.csv") %>%
  mutate(TaskID = paste(Block, Task, sep = "_"))

# 如果存在 ...1 列，则删除
experiment_design <- experiment_design %>%
  select(-any_of("...1"))

# -------------------------------
# 3. Extract actual choice tasks completed by respondents
# -------------------------------
chosen_tasks <- questionnaire %>%
  select(RespondentID, matches("^TA\\d+$|^TB\\d+$")) %>%
  pivot_longer(
    cols = -RespondentID,
    names_to = c("Block_prefix", "TaskNumber"),
    names_pattern = "(TA|TB)(\\d+)",
    values_to = "ChoiceRaw"
  ) %>%
  filter(ChoiceRaw %in% c("A", "B")) %>%
  mutate(
    TaskNumber = as.integer(TaskNumber),
    Block = paste0(Block_prefix, TaskNumber),
    Task = paste0("T", str_pad(TaskNumber, 2, pad = "0")),
    TaskID = paste(Block, Task, sep = "_")
  ) %>%
  select(RespondentID, Block, TaskNumber, Task, TaskID, ChoiceRaw)

# -------------------------------
# 4. Expand each choice task into two alternatives (A / B)
# -------------------------------
long_data <- chosen_tasks %>%
  tidyr::expand_grid(Alternative_code = c("A", "B")) %>%
  mutate(
    Choice = ifelse(Alternative_code == ChoiceRaw, 1, 0),
    Alternative = case_when(
      Alternative_code == "A" ~ "BEV",
      Alternative_code == "B" ~ "ICEV",
      TRUE ~ NA_character_
    )
  )

# -------------------------------
# 5. Merge experimental design attributes
# -------------------------------
long_data <- long_data %>%
  left_join(
    experiment_design %>%
      select(
        TaskID,
        Alternative,
        Purchase_Price,
        Powertrain,
        Monthly_Operating_Cost,
        Convenience,
        Smart_Cockpit,
        Reliability,
        Cabin_Comfort
      ),
    by = c("TaskID", "Alternative_code" = "Alternative")
  )

# -------------------------------
# 6. Data validation and consistency checks
# -------------------------------

table(long_data$Choice)

check_choice_set <- long_data %>%
  group_by(RespondentID, TaskID) %>%
  summarise(
    n_alt = n(),
    choice_sum = sum(Choice),
    .groups = "drop"
  )

check_choice_set %>%
  summarise(
    total_choice_sets = n(),
    min_alt = min(n_alt),
    max_alt = max(n_alt),
    min_choice_sum = min(choice_sum),
    max_choice_sum = max(choice_sum)
  )

long_data %>%
  summarise(
    missing_price = sum(is.na(Purchase_Price)),
    missing_powertrain = sum(is.na(Powertrain)),
    missing_cost = sum(is.na(Monthly_Operating_Cost)),
    missing_convenience = sum(is.na(Convenience)),
    missing_smart = sum(is.na(Smart_Cockpit)),
    missing_reliability = sum(is.na(Reliability)),
    missing_comfort = sum(is.na(Cabin_Comfort))
  )


long_data %>%
  select(
    RespondentID,
    TaskID,
    Alternative_code,
    Alternative,
    Choice,
    Purchase_Price,
    Powertrain,
    Monthly_Operating_Cost,
    Convenience,
    Smart_Cockpit,
    Reliability,
    Cabin_Comfort
  ) %>%
  head(20)

table(long_data$Choice)

check_choice_set %>%
  summarise(
    total_choice_sets = n(),
    min_alt = min(n_alt),
    max_alt = max(n_alt),
    min_choice_sum = min(choice_sum),
    max_choice_sum = max(choice_sum)
  )


long_data <- long_data %>%
  mutate(
    choice_set_id = paste(RespondentID, TaskID, sep = "_")
  )

# -------------------------------
# 7. Create choice set identifier
# -------------------------------
long_data <- long_data %>%
  mutate(
    Choice = as.logical(Choice),
    
    Alternative_code = factor(Alternative_code),
    Alternative = factor(Alternative),
    
    Powertrain = factor(Powertrain),
    Convenience = factor(Convenience),
    Smart_Cockpit = factor(Smart_Cockpit),
    Reliability = factor(Reliability),
    Cabin_Comfort = factor(Cabin_Comfort)
  )

# -------------------------------
# 8. Convert variable types
# -------------------------------
long_data <- long_data %>%
  mutate(
    Alternative_code = relevel(Alternative_code, ref = "B"),
    Alternative = relevel(Alternative, ref = "ICEV"),
    Powertrain = relevel(Powertrain, ref = "ICEV"),
    Convenience = relevel(Convenience, ref = "Difficult"),
    Smart_Cockpit = relevel(Smart_Cockpit, ref = "Basic"),
    Reliability = relevel(Reliability, ref = "Low"),
    Cabin_Comfort = relevel(Cabin_Comfort, ref = "Basic")
  )

# -------------------------------
# 9. Set reference categories
# -------------------------------
long_data <- long_data %>%
  mutate(

    Purchase_Price_mid = case_when(
      str_detect(Purchase_Price, "110,000") ~ 120000,
      str_detect(Purchase_Price, "150,000") ~ 160000,
      str_detect(Purchase_Price, "190,000") ~ 200000,
      TRUE ~ NA_real_
    ),
    

    Monthly_Cost_num = case_when(
      str_detect(Monthly_Operating_Cost, "300") ~ 300,
      str_detect(Monthly_Operating_Cost, "600") ~ 600,
      str_detect(Monthly_Operating_Cost, "900") ~ 900,
      TRUE ~ NA_real_
    ),
    
  
    price_10k = Purchase_Price_mid / 10000,
    cost_100 = Monthly_Cost_num / 100
  )

# -------------------------------
# 10. Convert price and cost variables into numeric form
# -------------------------------

long_data %>%
  group_by(choice_set_id) %>%
  summarise(
    n_alt = n(),
    choice_sum = sum(Choice),
    .groups = "drop"
  ) %>%
  summarise(
    total_choice_sets = n(),
    min_alt = min(n_alt),
    max_alt = max(n_alt),
    min_choice_sum = min(choice_sum),
    max_choice_sum = max(choice_sum)
  )

long_data %>%
  summarise(
    missing_price_mid = sum(is.na(Purchase_Price_mid)),
    missing_monthly_cost = sum(is.na(Monthly_Cost_num)),
    missing_price_10k = sum(is.na(price_10k)),
    missing_cost_100 = sum(is.na(cost_100))
  )

long_data %>%
  select(
    RespondentID,
    choice_set_id,
    TaskID,
    Alternative_code,
    Alternative,
    Choice,
    Purchase_Price,
    Purchase_Price_mid,
    price_10k,
    Monthly_Operating_Cost,
    Monthly_Cost_num,
    cost_100,
    Convenience,
    Smart_Cockpit,
    Reliability,
    Cabin_Comfort
  ) %>%
  head(20)

write_csv(long_data, "long_data_ready_for_mlogit.csv")



# ============================================================
# Descriptive Analysis of Conjoint Choice Data
# ============================================================


# Load packages
library(readr)
library(dplyr)
library(tidyr)
library(knitr)
library(writexl)

# ==============================
# 1. Read data
# ==============================

df <- read_csv("long_data_ready_for_mlogit.csv")

# Check structure
str(df)
glimpse(df)

# Make sure Choice is treated as logical / binary
df <- df %>%
  mutate(
    Choice_binary = case_when(
      Choice == TRUE ~ 1,
      Choice == 1 ~ 1,
      Choice == "TRUE" ~ 1,
      Choice == "True" ~ 1,
      Choice == "true" ~ 1,
      Choice == "chosen" ~ 1,
      TRUE ~ 0
    )
  )

# ==============================
#  Data Overview
# ==============================

data_overview <- tibble(
  Item = c(
    "Number of respondents",
    "Number of choice sets",
    "Alternatives per choice set",
    "Total alternative-level observations",
    "Missing values"
  ),
  Value = c(
    n_distinct(df$RespondentID),
    n_distinct(df$choice_set_id),
    paste(sort(unique(df %>% count(choice_set_id) %>% pull(n))), collapse = ", "),
    nrow(df),
    sum(is.na(df))
  )
)

kable(
  data_overview,
  caption = "Table Data structure of the conjoint experiment"
)

# Check whether each choice set has exactly two alternatives and one chosen alternative
choice_set_check <- df %>%
  group_by(choice_set_id) %>%
  summarise(
    n_alternatives = n(),
    n_chosen = sum(Choice_binary == 1),
    .groups = "drop"
  )

choice_set_summary <- choice_set_check %>%
  summarise(
    min_alternatives = min(n_alternatives),
    max_alternatives = max(n_alternatives),
    min_chosen = min(n_chosen),
    max_chosen = max(n_chosen)
  )

kable(
  choice_set_summary,
  caption = "Choice set consistency check"
)

# Tasks completed by each respondent
respondent_task_summary <- df %>%
  group_by(RespondentID) %>%
  summarise(
    completed_tasks = n_distinct(choice_set_id),
    .groups = "drop"
  ) %>%
  count(completed_tasks, name = "n_respondents")

kable(
  respondent_task_summary,
  caption = "Number of completed choice tasks per respondent"
)

# ==============================
# 5.2 Descriptive Choice Patterns
# Chosen vs Non-chosen Alternatives
# ==============================

# Attribute list
attributes_list <- c(
  "Powertrain",
  "Purchase_Price",
  "Monthly_Operating_Cost",
  "Convenience",
  "Smart_Cockpit",
  "Reliability",
  "Cabin_Comfort"
)

# Optional: nicer labels for paper tables
attribute_labels <- c(
  Powertrain = "Powertrain",
  Purchase_Price = "Purchase price",
  Monthly_Operating_Cost = "Monthly operating cost",
  Convenience = "Energy replenishment convenience",
  Smart_Cockpit = "Smart cockpit functions",
  Reliability = "Long-term reliability",
  Cabin_Comfort = "Cabin comfort"
)

# Function: chosen vs non-chosen distribution for one attribute
chosen_vs_nonchosen_table <- function(data, attribute) {
  
  data_temp <- data %>%
    mutate(
      Choice_Status = if_else(Choice_binary == 1, "Chosen", "Non_chosen"),
      Level = as.character(.data[[attribute]])
    )
  
  # Keep the original level order as it appears in the data
  level_order <- data_temp %>%
    distinct(Level) %>%
    pull(Level)
  
  tab <- data_temp %>%
    group_by(Choice_Status, Level) %>%
    summarise(
      n = n(),
      .groups = "drop"
    ) %>%
    group_by(Choice_Status) %>%
    mutate(
      percent = round(n / sum(n) * 100, 1)
    ) %>%
    ungroup() %>%
    pivot_wider(
      names_from = Choice_Status,
      values_from = c(n, percent),
      values_fill = list(n = 0, percent = 0)
    ) %>%
    mutate(
      Attribute = attribute_labels[[attribute]],
      Level = factor(Level, levels = level_order),
      difference_pp = round(percent_Chosen - percent_Non_chosen, 1)
    ) %>%
    arrange(Level) %>%
    select(
      Attribute,
      Level,
      chosen_n = n_Chosen,
      chosen_percent = percent_Chosen,
      non_chosen_n = n_Non_chosen,
      non_chosen_percent = percent_Non_chosen,
      difference_pp
    )
  
  return(tab)
}

# Generate Table for all attributes
table_5_2_chosen_vs_nonchosen <- bind_rows(
  lapply(attributes_list, function(att) {
    chosen_vs_nonchosen_table(df, att)
  })
)

# Print Table
kable(
  table_5_2_chosen_vs_nonchosen,
  caption = "Table Distribution of chosen and non-chosen alternatives by attribute level",
  digits = 1
)

# ==============================
# Optional: identify strongest descriptive differences
# ==============================

largest_descriptive_differences <- table_5_2_chosen_vs_nonchosen %>%
  mutate(abs_difference_pp = abs(difference_pp)) %>%
  arrange(desc(abs_difference_pp))

kable(
  largest_descriptive_differences,
  caption = "Attribute levels ranked by absolute chosen vs non-chosen percentage-point difference",
  digits = 1
)

# ==============================
# Export results
# ==============================

write_csv(
  table_5_2_chosen_vs_nonchosen,
  "Table_chosen_vs_nonchosen.csv"
)

write_csv(
  largest_descriptive_differences,
  "Table_largest_descriptive_differences.csv"
)

write_csv(
  data_overview,
  "Table_data_overview.csv"
)

write_csv(
  respondent_task_summary,
  "respondent_task_summary.csv"
)


# ==============================
#  Baseline Conditional Logit Model：dummy coding
# ==============================

library(survival)

# 2. Check variable structure
str(df)

# 3. Check key variables
table(df$Choice, useNA = "ifany")
table(df$Powertrain, useNA = "ifany")
table(df$Purchase_Price, useNA = "ifany")
table(df$Monthly_Operating_Cost, useNA = "ifany")
table(df$Convenience, useNA = "ifany")
table(df$Smart_Cockpit, useNA = "ifany")
table(df$Cabin_Comfort, useNA = "ifany")
table(df$Reliability, useNA = "ifany")

# 4. Convert categorical attributes to factors if needed
df$Powertrain <- factor(df$Powertrain)
df$Purchase_Price <- factor(df$Purchase_Price)
df$Monthly_Operating_Cost <- factor(df$Monthly_Operating_Cost)
df$Convenience <- factor(df$Convenience)
df$Smart_Cockpit <- factor(df$Smart_Cockpit)
df$Cabin_Comfort <- factor(df$Cabin_Comfort)
df$Reliability <- factor(df$Reliability)

# 5. Set reference levels

levels(df$Powertrain)
levels(df$Purchase_Price)
levels(df$Monthly_Operating_Cost)
levels(df$Convenience)
levels(df$Smart_Cockpit)
levels(df$Cabin_Comfort)
levels(df$Reliability)


df$Powertrain <- relevel(df$Powertrain, ref = "ICEV")

df$Powertrain <- relevel(df$Powertrain, ref = "ICEV")

df$Purchase_Price <- relevel(
  df$Purchase_Price,
  ref = "RMB 110,000–130,000"
)

df$Monthly_Operating_Cost <- relevel(
  df$Monthly_Operating_Cost,
  ref = "RMB 900/month"
)

df$Convenience <- relevel(
  df$Convenience,
  ref = "Difficult"
)

df$Smart_Cockpit <- relevel(
  df$Smart_Cockpit,
  ref = "Basic"
)

df$Cabin_Comfort <- relevel(
  df$Cabin_Comfort,
  ref = "Basic"
)

df$Reliability <- relevel(
  df$Reliability,
  ref = "Low"
)

#  Estimate baseline conditional logit model
baseline_clogit <- clogit(
  Choice ~ Powertrain +
    Purchase_Price +
    Monthly_Operating_Cost +
    Convenience +
    Smart_Cockpit +
    Cabin_Comfort +
    Reliability +
    strata(choice_set_id) +
    cluster(RespondentID),
  data = df,
  method = "efron"
)

# 7. Show model summary
summary(baseline_clogit)

# 8. Extract coefficients, robust standard errors, z-values, and p-values

baseline_summary <- summary(baseline_clogit)

coef_mat <- as.data.frame(baseline_summary$coefficients)

colnames(coef_mat)

if ("robust se" %in% colnames(coef_mat)) {
  se_col <- "robust se"
} else {
  se_col <- "se(coef)"
}


baseline_results <- data.frame(
  Variable = rownames(coef_mat),
  Coefficient = coef_mat[, "coef"],
  SE = coef_mat[, se_col],
  z_value = coef_mat[, "z"],
  p_value = coef_mat[, "Pr(>|z|)"],
  Odds_Ratio = exp(coef_mat[, "coef"])
)

baseline_results$Significance <- cut(
  baseline_results$p_value,
  breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
  labels = c("***", "**", "*", ".", "")
)

baseline_results

# 10. export results to CSV
write.csv(
  baseline_results,
  "baseline_clogit_results.csv",
  row.names = FALSE
)

# ==============================
# Powertrain Moderation Model：BEV × Attribute interactions
# ==============================
df$Convenience_int <- relevel(df$Convenience, ref = "Moderate")
# 1. Estimate interaction model
# This model tests:
# H4: Powertrain × Reliability
# H5: Powertrain × Smart Cockpit
# H6: Powertrain × Convenience

moderation_clogit <- clogit(
  Choice ~ 
    Purchase_Price +
    Monthly_Operating_Cost +
    Cabin_Comfort +
    Powertrain * Reliability +
    Powertrain * Smart_Cockpit +
    Powertrain * Convenience_int +
    strata(choice_set_id) +
    cluster(RespondentID),
  data = df,
  method = "efron"
)

# 2. Show model summary
moderation_summary <- summary(moderation_clogit)
moderation_summary

# 3. Extract coefficients, robust standard errors, z-values, and p-values

coef_mat_mod <- moderation_summary$coefficients

colnames(coef_mat_mod)

if ("robust se" %in% colnames(coef_mat_mod)) {
  se_col_mod <- "robust se"
} else {
  se_col_mod <- "se(coef)"
}

moderation_results <- data.frame(
  Variable = rownames(coef_mat_mod),
  Coefficient = coef_mat_mod[, "coef"],
  SE = coef_mat_mod[, se_col_mod],
  z_value = coef_mat_mod[, "z"],
  p_value = coef_mat_mod[, "Pr(>|z|)"],
  Odds_Ratio = exp(coef_mat_mod[, "coef"])
)

moderation_results$Significance <- cut(
  moderation_results$p_value,
  breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
  labels = c("***", "**", "*", ".", "")
)

moderation_results


# 4. Extract interaction terms only

interaction_terms <- moderation_results[
  grepl(":", moderation_results$Variable),
]

interaction_terms

# 5. Export full moderation model results

write.csv(
  moderation_results,
  "powertrain_moderation_model_results.csv",
  row.names = FALSE
)

# 6. Export interaction terms only

write.csv(
  interaction_terms,
  "powertrain_interaction_terms_only.csv",
  row.names = FALSE
)


# ==============================
# Robustness Checks
# Clean 2x2 model structure
# ==============================

library(survival)
library(fixest)
library(dplyr)
library(modelsummary)

# ------------------------------------------------
# 0. Make sure Choice is 0/1 numeric
# ------------------------------------------------

df <- df %>%
  mutate(
    Choice = as.integer(Choice)
  )

# ------------------------------------------------
# 1. Convenience coding
# ------------------------------------------------

# Baseline models keep original Convenience reference = Difficult
df$Convenience <- relevel(df$Convenience, ref = "Difficult")

# Interaction models use Moderate as reference
df$Convenience_int <- relevel(df$Convenience, ref = "Moderate")

# Create numeric BEV dummy
df$BEV_dummy <- ifelse(df$Powertrain == "BEV", 1, 0)

# Create numeric Easy dummy under interaction coding
df$Conv_Easy <- ifelse(df$Convenience_int == "Easy", 1, 0)

# Create manually estimable BEV × Easy interaction
df$BEV_Easy <- df$BEV_dummy * df$Conv_Easy

# Check design
table(df$Powertrain, df$Convenience_int, useNA = "ifany")
table(df$BEV_dummy, df$Conv_Easy, useNA = "ifany")
table(df$BEV_Easy, useNA = "ifany")

M1_baseline <- clogit(
  Choice ~ 
    Purchase_Price +
    Monthly_Operating_Cost +
    Cabin_Comfort +
    Powertrain +
    Reliability +
    Smart_Cockpit +
    Convenience +
    strata(choice_set_id) +
    cluster(RespondentID),
  data = df,
  method = "efron"
)

summary(M1_baseline)

M2_interaction_clean <- clogit(
  Choice ~ 
    Purchase_Price +
    Monthly_Operating_Cost +
    Cabin_Comfort +
    Powertrain * Reliability +
    Powertrain * Smart_Cockpit +
    Convenience_int +
    BEV_Easy +
    strata(choice_set_id) +
    cluster(RespondentID),
  data = df,
  method = "efron"
)

summary(M2_interaction_clean)

M3_fe_baseline <- feglm(
  Choice ~ 
    Purchase_Price +
    Monthly_Operating_Cost +
    Cabin_Comfort +
    Powertrain +
    Reliability +
    Smart_Cockpit +
    Convenience |
    choice_set_id,
  data = df,
  family = binomial("logit"),
  cluster = ~ RespondentID
)

summary(M3_fe_baseline)

M4_fe_moderation_clean <- feglm(
  Choice ~ 
    Purchase_Price +
    Monthly_Operating_Cost +
    Cabin_Comfort +
    Powertrain * Reliability +
    Powertrain * Smart_Cockpit +
    Convenience_int +
    BEV_Easy |
    choice_set_id,
  data = df,
  family = binomial("logit"),
  cluster = ~ RespondentID
)

summary(M4_fe_moderation_clean)

coef(M4_fe_moderation_clean)

modelsummary(
  list(
    "M1 Baseline" = M1_baseline,
    "M2 Interaction" = M2_interaction_clean,
    "M3 FE Baseline" = M3_fe_baseline,
    "M4 FE Interaction" = M4_fe_moderation_clean
  ),
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  stars = TRUE,
  coef_omit = "Purchase_Price|Cabin_Comfort",
  output = "Table_robustness_2x2.csv"
)







# ==============================
# 5.5 Robustness Checks
# Clean 2x2 model structure
# Respondent-level FE + Bias Correction
# ==============================

library(survival)
library(dplyr)
library(modelsummary)
library(bife)

# ------------------------------------------------
# 0. Make sure Choice is 0/1 numeric
# ------------------------------------------------

df <- df %>%
  mutate(
    Choice = as.integer(Choice),
    
    Purchase_Price = factor(Purchase_Price),
    Monthly_Operating_Cost = factor(Monthly_Operating_Cost),
    Cabin_Comfort = factor(Cabin_Comfort),
    Powertrain = factor(Powertrain),
    Reliability = factor(Reliability),
    Smart_Cockpit = factor(Smart_Cockpit),
    Convenience = factor(Convenience)
  )

# ------------------------------------------------
# 1. Convenience coding
# ------------------------------------------------

# Baseline models keep original Convenience reference = Difficult
df$Convenience <- relevel(df$Convenience, ref = "Difficult")

# Interaction models use Moderate as reference
df$Convenience_int <- relevel(df$Convenience, ref = "Moderate")

# Create numeric BEV dummy
df$BEV_dummy <- ifelse(df$Powertrain == "BEV", 1, 0)

# Create numeric Easy dummy under interaction coding
df$Conv_Easy <- ifelse(df$Convenience_int == "Easy", 1, 0)

# Create manually estimable BEV × Easy interaction
df$BEV_Easy <- df$BEV_dummy * df$Conv_Easy

# Check design
table(df$Powertrain, df$Convenience_int, useNA = "ifany")
table(df$BEV_dummy, df$Conv_Easy, useNA = "ifany")
table(df$BEV_Easy, useNA = "ifany")

# ------------------------------------------------
# M1: Baseline conditional logit
# No interaction, no respondent FE
# ------------------------------------------------

M1_baseline <- clogit(
  Choice ~ 
    Purchase_Price +
    Monthly_Operating_Cost +
    Cabin_Comfort +
    Powertrain +
    Reliability +
    Smart_Cockpit +
    Convenience +
    strata(choice_set_id) +
    cluster(RespondentID),
  data = df,
  method = "efron"
)

summary(M1_baseline)

# ------------------------------------------------
# M2: Interaction conditional logit
# Interaction terms, no respondent FE
# ------------------------------------------------

M2_interaction_clean <- clogit(
  Choice ~ 
    Purchase_Price +
    Monthly_Operating_Cost +
    Cabin_Comfort +
    Powertrain * Reliability +
    Powertrain * Smart_Cockpit +
    Convenience_int +
    BEV_Easy +
    strata(choice_set_id) +
    cluster(RespondentID),
  data = df,
  method = "efron"
)

summary(M2_interaction_clean)

# ------------------------------------------------
# M3: Respondent-level fixed-effects baseline model
# Using bife + bias correction
# ------------------------------------------------

M3_fe_baseline_raw <- bife(
  Choice ~ 
    Purchase_Price +
    Monthly_Operating_Cost +
    Cabin_Comfort +
    Powertrain +
    Reliability +
    Smart_Cockpit +
    Convenience |
    RespondentID,
  data = df,
  model = "logit"
)

summary(M3_fe_baseline_raw)

# Apply analytical bias correction
M3_fe_baseline <- bias_corr(M3_fe_baseline_raw)

summary(M3_fe_baseline)

# ------------------------------------------------
# M4: Respondent-level fixed-effects interaction model
# Using bife + bias correction
# ------------------------------------------------

M4_fe_moderation_raw <- bife(
  Choice ~ 
    Purchase_Price +
    Monthly_Operating_Cost +
    Cabin_Comfort +
    Powertrain * Reliability +
    Powertrain * Smart_Cockpit +
    Convenience_int +
    BEV_Easy |
    RespondentID,
  data = df,
  model = "logit"
)

summary(M4_fe_moderation_raw)

# Apply analytical bias correction
M4_fe_moderation_clean <- bias_corr(M4_fe_moderation_raw)

summary(M4_fe_moderation_clean)

coef(M4_fe_moderation_clean)

# ------------------------------------------------
# Export table
# ------------------------------------------------

modelsummary(
  list(
    "M1 Baseline" = M1_baseline,
    "M2 Interaction" = M2_interaction_clean,
    "M3 Respondent FE + BC" = M3_fe_baseline,
    "M4 Respondent FE Interaction + BC" = M4_fe_moderation_clean
  ),
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  stars = TRUE,
  coef_omit = "Purchase_Price|Cabin_Comfort",
  output = "Table_robustness_2x2.csv"
)


# ============================================================
# Full Script: Prepare for Heterogeneity Analysis
# Includes respondent-level variables (Age, Income, City tier, EV exp)

# This script builds the final analysis dataset used for
# heterogeneity and subgroup analysis in the conjoint experiment.
#
# The process includes:
# (1) Loading cleaned questionnaire data and experimental design
# (2) Extracting respondents’ actual choice tasks and reshaping
#     them into a long-format (alternative-level) dataset
# (3) Merging choice tasks with experimental design attributes
#     (e.g., price, powertrain, cost, and vehicle features)
# (4) Appending respondent-level characteristics (e.g., age,
#     income, city tier, EV experience, charging access)
# (5) Recoding categorical variables and setting reference levels
# (6) Transforming price and operating cost variables into numeric
#     formats for econometric modeling
# (7) Filtering incomplete observations and ensuring valid
#     two-alternative choice sets
#
# The final output (long_data_ready_for_heterogeneity.csv) is a
# clean individual-choice dataset ready for conditional logit
# subgroup and heterogeneity analysis.
# ============================================================

library(tidyverse)
library(stringr)

questionnaire <- read_csv("questionnaire_long_cleaned.csv")

respondent_vars <- questionnaire %>%
  select(RespondentID, Q3:Q11) %>%
  distinct(RespondentID, .keep_all = TRUE) %>%
  rename(
    Age_raw = Q3,
    Gender_raw = Q4,
    Education_raw = Q5,
    Income_raw = Q6,
    City_tier_raw = Q7,
    Car_ownership_raw = Q8,
    Charging_access_raw = Q9,
    Commute_distance_raw = Q10,
    EV_experience_raw = Q11
  ) %>%
  mutate(
    Age_group = case_when(
      Age_raw %in% c("18-24 years","25-29 years") ~ "18-29",
      Age_raw %in% c("35-39 years","40+ years") ~ "30+",
      TRUE ~ NA_character_
    ),
    Gender = case_when(
      Gender_raw == "Male" ~ "Male",
      Gender_raw == "Female" ~ "Female",
      TRUE ~ "Prefer not to say"
    ),
    Education_binary = case_when(
      Education_raw == "Master or above" ~ "Master or above",
      Education_raw %in% c("Bachelor","College or below") ~ "Bachelor or below",
      TRUE ~ NA_character_
    ),
    Income_binary = case_when(
      Income_raw %in% c("8,000元以下","8,001-15,000 RMB") ~ "≤15,000 RMB",
      Income_raw %in% c("15,001–25,000元",">25,000 RMB") ~ ">15,000 RMB",
      TRUE ~ NA_character_
    ),
    City_tier_binary = case_when(
      City_tier_raw %in% c("Tier1/NewTier1 city","Tier2 city") ~ "High-tier city",
      City_tier_raw %in% c("Tier3 or lower city","Villages") ~ "Lower-tier city",
      TRUE ~ NA_character_
    ),
    Car_ownership = case_when(
      Car_ownership_raw == "Yes" ~ "Car ownership",
      Car_ownership_raw == "No" ~ "No car ownership",
      TRUE ~ NA_character_
    ),
    Charging_access_binary = case_when(
      Charging_access_raw == "Yes" ~ "Home charging access",
      Charging_access_raw %in% c("No","Uncertain","不确定") ~ "No/uncertain charging access",
      TRUE ~ "No/uncertain charging access"
    ),
    Commute_binary = case_when(
      Commute_distance_raw %in% c("No commuting","<=10 km") ~ "No/short commute",
      Commute_distance_raw %in% c("11-30 km",">30 km") ~ "Medium/long commute",
      TRUE ~ NA_character_
    ),
    EV_experience = case_when(
      EV_experience_raw == "Yes" ~ "EV experience",
      EV_experience_raw == "No" ~ "No EV experience",
      TRUE ~ NA_character_
    )
  )


experiment_design <- read_csv("task_design.csv") %>%
  mutate(TaskID = paste(Block, Task, sep = "_")) %>%
  select(-any_of("...1"))


chosen_tasks <- questionnaire %>%
  select(RespondentID, matches("^TA\\d+$|^TB\\d+$")) %>%
  pivot_longer(
    cols = -RespondentID,
    names_to = c("Block_prefix","TaskNumber"),
    names_pattern = "(TA|TB)(\\d+)",
    values_to = "ChoiceRaw"
  ) %>%
  filter(ChoiceRaw %in% c("A","B")) %>%
  mutate(
    TaskNumber = as.integer(TaskNumber),
    Block = paste0(Block_prefix, TaskNumber),
    Task = paste0("T", str_pad(TaskNumber, 2, pad="0")),
    TaskID = paste(Block, Task, sep = "_")
  ) %>%
  select(RespondentID, Block, TaskNumber, Task, TaskID, ChoiceRaw)


long_data <- chosen_tasks %>%
  tidyr::expand_grid(Alternative_code = c("A","B")) %>%
  mutate(
    Choice = ifelse(Alternative_code == ChoiceRaw,1,0),
    Alternative = case_when(
      Alternative_code=="A" ~ "BEV",
      Alternative_code=="B" ~ "ICEV",
      TRUE ~ NA_character_
    )
  )


long_data <- long_data %>%
  left_join(
    experiment_design %>%
      select(TaskID, Alternative, Purchase_Price, Powertrain, Monthly_Operating_Cost,
             Convenience, Smart_Cockpit, Reliability, Cabin_Comfort),
    by = c("TaskID","Alternative_code"="Alternative")
  )


long_data <- long_data %>%
  left_join(respondent_vars, by="RespondentID")


long_data <- long_data %>%
  mutate(choice_set_id = paste(RespondentID, TaskID, sep="_"))


long_data <- long_data %>%
  mutate(
    Choice = as.logical(Choice),
    Alternative_code = factor(Alternative_code),
    Alternative = factor(Alternative),
    Powertrain = factor(Powertrain),
    Convenience = factor(Convenience),
    Smart_Cockpit = factor(Smart_Cockpit),
    Reliability = factor(Reliability),
    Cabin_Comfort = factor(Cabin_Comfort),
    Age_group = factor(Age_group),
    Gender = factor(Gender),
    Education_binary = factor(Education_binary),
    Income_binary = factor(Income_binary),
    City_tier_binary = factor(City_tier_binary),
    Car_ownership = factor(Car_ownership),
    Charging_access_binary = factor(Charging_access_binary),
    Commute_binary = factor(Commute_binary),
    EV_experience = factor(EV_experience)
  )


long_data <- long_data %>%
  mutate(
    Alternative_code = relevel(Alternative_code, ref="B"),
    Alternative = relevel(Alternative, ref="ICEV"),
    Powertrain = relevel(Powertrain, ref="ICEV"),
    Convenience = relevel(Convenience, ref="Difficult"),
    Smart_Cockpit = relevel(Smart_Cockpit, ref="Basic"),
    Reliability = relevel(Reliability, ref="Low"),
    Cabin_Comfort = relevel(Cabin_Comfort, ref="Basic"),
    Age_group = relevel(Age_group, ref="30+"),
    Education_binary = relevel(Education_binary, ref="Bachelor or below"),
    Income_binary = relevel(Income_binary, ref="≤15,000 RMB"),
    City_tier_binary = relevel(City_tier_binary, ref="Lower-tier city"),
    Charging_access_binary = relevel(Charging_access_binary, ref="No/uncertain charging access"),
    EV_experience = relevel(EV_experience, ref="No EV experience"),
    Commute_binary = relevel(Commute_binary, ref="No/short commute")
  )


long_data <- long_data %>%
  mutate(
    Purchase_Price_mid = case_when(
      str_detect(Purchase_Price,"110,000") ~ 120000,
      str_detect(Purchase_Price,"150,000") ~ 160000,
      str_detect(Purchase_Price,"190,000") ~ 200000,
      TRUE ~ NA_real_
    ),
    Monthly_Cost_num = case_when(
      str_detect(Monthly_Operating_Cost,"300") ~ 300,
      str_detect(Monthly_Operating_Cost,"600") ~ 600,
      str_detect(Monthly_Operating_Cost,"900") ~ 900,
      TRUE ~ NA_real_
    ),
    price_10k = Purchase_Price_mid/10000,
    cost_100 = Monthly_Cost_num/100
  )

long_data_56 <- long_data %>%
  filter(
    !is.na(Choice),
    !is.na(choice_set_id),
    !is.na(price_10k),
    !is.na(cost_100),
    !is.na(Powertrain),
    !is.na(Convenience),
    !is.na(Smart_Cockpit),
    !is.na(Reliability),
    !is.na(Cabin_Comfort),
    !is.na(EV_experience),
    !is.na(Charging_access_binary),
    !is.na(Income_binary),
    !is.na(Education_binary),
    !is.na(City_tier_binary)
  )

write_csv(long_data_56,"long_data_ready_for_heterogeneity.csv")



# ============================================================
# Heterogeneity Analysis / Subgroup Re-estimation
# Based on M2 interaction model
# ============================================================

library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(survival)
library(writexl)
library(readr)
library(knitr)

# ------------------------------------------------------------
# 0. Read dataset
# ------------------------------------------------------------

long_data_56 <- read_csv("long_data_ready_for_heterogeneity.csv")

# ------------------------------------------------------------
# 1. Helper function: safe relevel
# ------------------------------------------------------------

safe_relevel <- function(x, ref) {
  x <- factor(x)
  if (ref %in% levels(x)) {
    x <- relevel(x, ref = ref)
  }
  return(x)
}

# ------------------------------------------------------------
# 2. Prepare modelling data
# ------------------------------------------------------------

long_data_56 <- long_data_56 %>%
  mutate(
    # make sure Choice is numeric 0/1
    Choice_num = case_when(
      Choice == TRUE ~ 1,
      Choice == "TRUE" ~ 1,
      Choice == "True" ~ 1,
      Choice == "true" ~ 1,
      Choice == 1 ~ 1,
      TRUE ~ 0
    ),
    
    RespondentID = factor(RespondentID),
    choice_set_id = factor(choice_set_id),
    
    Purchase_Price = safe_relevel(Purchase_Price, "RMB 110,000–130,000"),
    Monthly_Operating_Cost = safe_relevel(Monthly_Operating_Cost, "RMB 900/month"),
    Cabin_Comfort = safe_relevel(Cabin_Comfort, "Basic"),
    Powertrain = safe_relevel(Powertrain, "ICEV"),
    Reliability = safe_relevel(Reliability, "Low"),
    Smart_Cockpit = safe_relevel(Smart_Cockpit, "Basic"),
    Convenience = safe_relevel(Convenience, "Difficult"),
    
    # same as  interaction coding
    Convenience_int = safe_relevel(Convenience, "Moderate"),
    
    BEV_dummy = ifelse(Powertrain == "BEV", 1, 0),
    Conv_Easy = ifelse(Convenience_int == "Easy", 1, 0),
    BEV_Easy = BEV_dummy * Conv_Easy,
    
    # respondent-level grouping variables
    Charging_access_binary = factor(Charging_access_binary),
    Income_binary = factor(Income_binary),
    EV_experience = factor(EV_experience),
    Car_ownership = factor(Car_ownership)
  )

# ------------------------------------------------------------
# 3. Keep valid 2-alternative choice sets only
# ------------------------------------------------------------

long_data_56_model <- long_data_56 %>%
  group_by(choice_set_id) %>%
  filter(
    n() == 2,
    sum(Choice_num == 1, na.rm = TRUE) == 1
  ) %>%
  ungroup() %>%
  filter(
    !is.na(Choice_num),
    !is.na(RespondentID),
    !is.na(choice_set_id),
    !is.na(Purchase_Price),
    !is.na(Monthly_Operating_Cost),
    !is.na(Cabin_Comfort),
    !is.na(Powertrain),
    !is.na(Reliability),
    !is.na(Smart_Cockpit),
    !is.na(Convenience_int),
    !is.na(BEV_Easy)
  )

# Basic data check
subgroup_sample_summary <- long_data_56_model %>%
  distinct(
    RespondentID,
    Charging_access_binary,
    Income_binary,
    EV_experience,
    Car_ownership
  ) %>%
  pivot_longer(
    cols = c(
      Charging_access_binary,
      Income_binary,
      EV_experience,
      Car_ownership
    ),
    names_to = "Subgroup_variable",
    values_to = "Subgroup_level"
  ) %>%
  count(Subgroup_variable, Subgroup_level, name = "n_respondents")

kable(
  subgroup_sample_summary,
  caption = "Table a Respondent counts by subgroup"
)

write_csv(
  subgroup_sample_summary,
  "Table_5_6a_subgroup_sample_summary.csv"
)

# ------------------------------------------------------------
# 4. Define the based model formula
# ------------------------------------------------------------
# This is the same structure as  M2_interaction_clean:
# Purchase price + monthly cost + cabin comfort +
# Powertrain × Reliability +
# Powertrain × Smart cockpit +
# Convenience_int +
# manually created BEV × Easy interaction

formula_56_base <- as.formula(
  Choice_num ~
    Purchase_Price +
    Monthly_Operating_Cost +
    Cabin_Comfort +
    Powertrain * Reliability +
    Powertrain * Smart_Cockpit +
    Convenience_int +
    BEV_Easy +
    strata(choice_set_id) +
    cluster(RespondentID)
)

# ------------------------------------------------------------
# 5. Estimate full-sample reference model
# ------------------------------------------------------------

M56_full_reference <- clogit(
  formula_56_base,
  data = long_data_56_model,
  method = "efron"
)

summary(M56_full_reference)

# ------------------------------------------------------------
# 6. Function to extract model results
# ------------------------------------------------------------

extract_clogit_results <- function(model,
                                   model_name,
                                   subgroup_variable = "Full sample",
                                   subgroup_level = "Full sample",
                                   data_used) {
  
  model_summary <- summary(model)
  coef_mat <- as.data.frame(model_summary$coefficients)
  
  if (nrow(coef_mat) == 0) {
    return(NULL)
  }
  
  se_col <- ifelse(
    "robust se" %in% colnames(coef_mat),
    "robust se",
    "se(coef)"
  )
  
  out <- data.frame(
    Model = model_name,
    Subgroup_variable = subgroup_variable,
    Subgroup_level = subgroup_level,
    n_respondents = n_distinct(data_used$RespondentID),
    n_choice_sets = n_distinct(data_used$choice_set_id),
    n_observations = nrow(data_used),
    Variable = rownames(coef_mat),
    Coefficient = coef_mat[, "coef"],
    SE = coef_mat[, se_col],
    z_value = coef_mat[, "z"],
    p_value = coef_mat[, "Pr(>|z|)"],
    Odds_Ratio = exp(coef_mat[, "coef"]),
    row.names = NULL
  )
  
  out <- out %>%
    mutate(
      Significance = case_when(
        p_value < 0.001 ~ "***",
        p_value < 0.01 ~ "**",
        p_value < 0.05 ~ "*",
        p_value < 0.1 ~ ".",
        TRUE ~ ""
      )
    )
  
  return(out)
}

# Extract full-sample model result
M56_full_results <- extract_clogit_results(
  model = M56_full_reference,
  model_name = "M56 full-sample reference",
  subgroup_variable = "Full sample",
  subgroup_level = "Full sample",
  data_used = long_data_56_model
)

# ------------------------------------------------------------
# 7. Function to estimate subgroup models
# ------------------------------------------------------------

estimate_subgroup_model <- function(data, subgroup_var, subgroup_level) {
  
  data_sub <- data %>%
    filter(.data[[subgroup_var]] == subgroup_level) %>%
    droplevels()
  
  n_resp <- n_distinct(data_sub$RespondentID)
  n_sets <- n_distinct(data_sub$choice_set_id)
  
  cat("\n--------------------------------------------------\n")
  cat("Estimating subgroup:", subgroup_var, "=", subgroup_level, "\n")
  cat("Respondents:", n_resp, "\n")
  cat("Choice sets:", n_sets, "\n")
  cat("Observations:", nrow(data_sub), "\n")
  
  if (n_resp < 10 | n_sets < 30) {
    warning(
      paste(
        "Subgroup too small:",
        subgroup_var,
        subgroup_level,
        "- model may be unstable."
      )
    )
  }
  
  model <- tryCatch(
    clogit(
      formula_56_base,
      data = data_sub,
      method = "efron"
    ),
    error = function(e) {
      message(
        "Model failed for ",
        subgroup_var,
        " = ",
        subgroup_level,
        ": ",
        e$message
      )
      return(NULL)
    }
  )
  
  if (is.null(model)) {
    return(NULL)
  }
  
  results <- extract_clogit_results(
    model = model,
    model_name = paste0("M56 subgroup: ", subgroup_var, " = ", subgroup_level),
    subgroup_variable = subgroup_var,
    subgroup_level = subgroup_level,
    data_used = data_sub
  )
  
  return(
    list(
      model = model,
      results = results,
      data = data_sub
    )
  )
}

# ------------------------------------------------------------
# 8. Define subgroup variables
# ------------------------------------------------------------

subgroup_variables_56 <- c(
  "Charging_access_binary",  # Q9
  "Income_binary",           # Q6
  "EV_experience",           # Q11
  "Car_ownership"            # Q8
)

# ------------------------------------------------------------
# 9. Estimate all subgroup models
# ------------------------------------------------------------

subgroup_model_outputs <- list()

for (var in subgroup_variables_56) {
  
  levels_var <- long_data_56_model %>%
    filter(!is.na(.data[[var]])) %>%
    pull(.data[[var]]) %>%
    factor() %>%
    levels()
  
  for (lev in levels_var) {
    
    result_name <- paste(var, lev, sep = "__")
    
    subgroup_model_outputs[[result_name]] <- estimate_subgroup_model(
      data = long_data_56_model,
      subgroup_var = var,
      subgroup_level = lev
    )
  }
}

# ------------------------------------------------------------
# 10. Combine subgroup results
# ------------------------------------------------------------

subgroup_results_56 <- subgroup_model_outputs %>%
  map("results") %>%
  compact() %>%
  bind_rows()

table_5_6_all_results <- bind_rows(
  M56_full_results,
  subgroup_results_56
)

# View full table
kable(
  table_5_6_all_results,
  caption = "Table b Subgroup conditional logit results",
  digits = 3
)

# ------------------------------------------------------------
# 11. Create key-coefficient comparison table
# ------------------------------------------------------------
# These are the most relevant terms for 5.6 interpretation.

key_terms_56 <- c(
  "PowertrainBEV",
  "Monthly_Operating_CostRMB 300/month",
  "Monthly_Operating_CostRMB 600/month",
  "Convenience_intEasy",
  "PowertrainBEV:ReliabilityMedium",
  "PowertrainBEV:ReliabilityHigh",
  "PowertrainBEV:Smart_CockpitIntermediate",
  "PowertrainBEV:Smart_CockpitAdvanced",
  "BEV_Easy"
)

table_5_6_key_results <- table_5_6_all_results %>%
  filter(Variable %in% key_terms_56) %>%
  mutate(
    Estimate_with_sig = paste0(
      round(Coefficient, 3),
      Significance,
      " (",
      round(SE, 3),
      ")"
    )
  ) %>%
  select(
    Subgroup_variable,
    Subgroup_level,
    n_respondents,
    n_choice_sets,
    Variable,
    Coefficient,
    SE,
    p_value,
    Odds_Ratio,
    Significance,
    Estimate_with_sig
  )

kable(
  table_5_6_key_results,
  caption = "Table c Key coefficient comparison across subgroups",
  digits = 3
)

# ------------------------------------------------------------
# 12. Wide-format table for easier paper writing
# ------------------------------------------------------------

table_5_6_key_wide <- table_5_6_key_results %>%
  mutate(
    Group = paste(Subgroup_variable, Subgroup_level, sep = ": ")
  ) %>%
  select(Group, Variable, Estimate_with_sig) %>%
  pivot_wider(
    names_from = Group,
    values_from = Estimate_with_sig
  )

kable(
  table_5_6_key_wide,
  caption = "Table d Key subgroup effects in wide format"
)

# ------------------------------------------------------------
# 13. Export results
# ------------------------------------------------------------

write_csv(
  table_5_6_all_results,
  "Table_b_all_subgroup_clogit_results.csv"
)

write_csv(
  table_5_6_key_results,
  "Table_c_key_subgroup_results.csv"
)

write_csv(
  table_5_6_key_wide,
  "Table_d_key_subgroup_results_wide.csv"
)

write_xlsx(
  list(
    "Subgroup sample summary" = subgroup_sample_summary,
    "All model results" = table_5_6_all_results,
    "Key subgroup results" = table_5_6_key_results,
    "Key results wide" = table_5_6_key_wide
  ),
  "Table_heterogeneity_results.xlsx"
)


library(dplyr)
library(readr)
library(forcats)
library(knitr)

data <- read_csv("long_data_ready_for_heterogeneity.csv")

respondents <- data %>%
  distinct(
    RespondentID,
    Age_raw,
    Gender,
    Education_raw,
    Income_raw,
    City_tier_raw,
    Car_ownership,
    EV_experience,
    Charging_access_binary,
    Commute_distance_raw
  ) %>%
  mutate(
    Age_raw = factor(
      Age_raw,
      levels = c("18-24 years", "25-29 years", "35-39 years", "40+ years")
    ),
    Gender = factor(
      Gender,
      levels = c("Female", "Male", "Prefer not to say")
    ),
    Education_raw = factor(
      Education_raw,
      levels = c("College or below", "Bachelor", "Master or above")
    ),
    Income_raw = factor(
      Income_raw,
      levels = c("8,000元以下", "8,001-15,000 RMB", "15,001–25,000元", ">25,000 RMB")
    ),
    City_tier_raw = factor(
      City_tier_raw,
      levels = c("Tier1/NewTier1 city", "Tier2 city", "Tier3 or lower city", "Villages")
    ),
    Car_ownership = factor(
      Car_ownership,
      levels = c("Car ownership", "No car ownership")
    ),
    EV_experience = factor(
      EV_experience,
      levels = c("EV experience", "No EV experience")
    ),
    Charging_access_binary = factor(
      Charging_access_binary,
      levels = c("Home charging access", "No/uncertain charging access")
    ),
    Commute_distance_raw = factor(
      Commute_distance_raw,
      levels = c("No commuting", "<=10 km", "11-30 km", ">30 km")
    )
  )

make_profile <- function(df, var, variable_label) {
  df %>%
    count({{ var }}, name = "N", .drop = FALSE) %>%
    mutate(
      Percent = round(N / sum(N) * 100, 1),
      Variable = variable_label,
      Category = as.character({{ var }})
    ) %>%
    select(Variable, Category, N, Percent)
}

respondent_profile <- bind_rows(
  make_profile(respondents, Age_raw, "Age"),
  make_profile(respondents, Gender, "Gender"),
  make_profile(respondents, Education_raw, "Education"),
  make_profile(respondents, Income_raw, "Monthly income"),
  make_profile(respondents, City_tier_raw, "City tier"),
  make_profile(respondents, Car_ownership, "Car ownership"),
  make_profile(respondents, EV_experience, "Prior EV experience"),
  make_profile(respondents, Charging_access_binary, "Home charging access"),
  make_profile(respondents, Commute_distance_raw, "Commute distance")
)

kable(
  respondent_profile,
  caption = "Respondent profile of valid survey participants",
  col.names = c("Variable", "Category", "N", "%")
)

write_csv(respondent_profile, "respondent_profile_table.csv")



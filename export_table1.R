# Export Table 1 to Excel
library(haven)
library(dplyr)
library(openxlsx)

setwd("/Users/mac/Documents/r作业")

# Load data
ajr_data <- read_dta("data/maketable1.dta")

# Prepare samples
base_sample <- ajr_data %>% filter(baseco == 1)
quartile_sample <- base_sample %>%
  filter(!is.na(extmort4)) %>%
  mutate(quartile = ntile(extmort4, 4))

# Helper functions
get_means <- function(df, vars) {
  df %>% summarise(across(all_of(vars), ~mean(.x, na.rm = TRUE)))
}
get_sds <- function(df, vars) {
  df %>% summarise(across(all_of(vars), ~sd(.x, na.rm = TRUE)))
}

# Calculate statistics
all_vars <- c("logpgp95", "loghjypl", "avexpr", "cons00a",
              "cons1", "democ00a", "euro1900", "logem4")

means_w <- get_means(ajr_data, all_vars)
sds_w <- get_sds(ajr_data, all_vars)
means_b <- get_means(base_sample, all_vars)
sds_b <- get_sds(base_sample, all_vars)
means_q <- quartile_sample %>% group_by(quartile) %>% get_means(all_vars)
sds_q <- quartile_sample %>% group_by(quartile) %>% get_sds(all_vars)

obs_w <- nrow(ajr_data)
obs_b <- nrow(base_sample)
obs_q <- quartile_sample %>% group_by(quartile) %>% count()

# Variable labels
var_labels <- c("Log GDP per capita (PPP) in 1995",
                "Log output per worker in 1988",
                "Average protection against expropriation risk",
                "Constraint on executive in 1900",
                "Constraint on executive at independence",
                "Democracy in 1900",
                "European settlements in 1900",
                "Log European settler mortality")

# Create export dataframe
table1_rows <- list()
for (i in 1:length(all_vars)) {
  # Mean row
  table1_rows[[2*i-1]] <- data.frame(
    Variable = var_labels[i],
    `Whole World` = means_w[[all_vars[i]]],
    `Base Sample` = means_b[[all_vars[i]]],
    Q1 = means_q[[all_vars[i]]][1],
    Q2 = means_q[[all_vars[i]]][2],
    Q3 = means_q[[all_vars[i]]][3],
    Q4 = means_q[[all_vars[i]]][4],
    check.names = FALSE
  )
  # Std Dev row
  table1_rows[[2*i]] <- data.frame(
    Variable = "  (Std. Dev.)",
    `Whole World` = sds_w[[all_vars[i]]],
    `Base Sample` = sds_b[[all_vars[i]]],
    Q1 = sds_q[[all_vars[i]]][1],
    Q2 = sds_q[[all_vars[i]]][2],
    Q3 = sds_q[[all_vars[i]]][3],
    Q4 = sds_q[[all_vars[i]]][4],
    check.names = FALSE
  )
}

# Add observation count row
table1_rows[[17]] <- data.frame(
  Variable = "Number of observations",
  `Whole World` = obs_w,
  `Base Sample` = obs_b,
  Q1 = obs_q$n[1],
  Q2 = obs_q$n[2],
  Q3 = obs_q$n[3],
  Q4 = obs_q$n[4],
  check.names = FALSE
)

# Combine all rows
table1_export <- bind_rows(table1_rows)

# Create output directory if needed
if (!dir.exists("output/tables")) {
  dir.create("output/tables", recursive = TRUE)
}

# Export to Excel
write.xlsx(table1_export, "output/tables/Table1_Descriptive_Statistics.xlsx", 
           rowNames = FALSE)

cat("✓ Table 1 exported to: output/tables/Table1_Descriptive_Statistics.xlsx\n")

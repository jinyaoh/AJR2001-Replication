# ============================================================================
# EXPORT ALL TABLES TO EXCEL
# ============================================================================
library(haven)
library(fixest)
library(modelsummary)
library(dplyr)
library(openxlsx)

setwd("/Users/mac/Documents/r作业")

# Create output directory
if (!dir.exists("output/tables")) {
  dir.create("output/tables", recursive = TRUE)
}

cat("\n============================================================\n")
cat("EXPORTING ALL TABLES TO EXCEL\n")
cat("============================================================\n\n")

# TABLE 2
ajr_dta <- read_dta("data/maketable2.dta")
base_sample <- ajr_dta %>% filter(baseco == 1)
model_list <- list(
  "(1)" = feols(logpgp95 ~ avexpr, data = ajr_dta, vcov = "hetero"),
  "(2)" = feols(logpgp95 ~ avexpr, data = base_sample, vcov = "hetero"),
  "(3)" = feols(logpgp95 ~ avexpr + lat_abst, data = ajr_dta, vcov = "hetero"),
  "(4)" = feols(logpgp95 ~ avexpr + lat_abst + africa + asia + other, data = ajr_dta, vcov = "hetero"),
  "(5)" = feols(logpgp95 ~ avexpr + lat_abst, data = base_sample, vcov = "hetero"),
  "(6)" = feols(logpgp95 ~ avexpr + lat_abst + africa + asia + other, data = base_sample, vcov = "hetero"),
  "(7)" = feols(loghjypl ~ avexpr, data = ajr_dta, vcov = "hetero"),
  "(8)" = feols(loghjypl ~ avexpr, data = base_sample, vcov = "hetero")
)
modelsummary(model_list, output = "output/tables/Table2_OLS.xlsx")
cat("✓ Table 2: output/tables/Table2_OLS.xlsx\n")

# TABLE 3 - Panel A
ajr_dta <- read_dta("data/maketable3.dta")
main_sample <- ajr_dta %>% filter(excolony == 1, !is.na(extmort4)) %>% mutate(euro1900 = euro1900 / 100)
lpgp_sample <- main_sample %>% filter(!is.na(logpgp95))
panel_A_models <- list(
  "(1)" = feols(avexpr ~ cons00a, data = main_sample),
  "(2)" = feols(avexpr ~ cons00a + lat_abst, data = main_sample),
  "(3)" = feols(avexpr ~ democ00a, data = main_sample),
  "(4)" = feols(avexpr ~ democ00a + lat_abst, data = main_sample),
  "(5)" = feols(avexpr ~ indtime + cons1, data = main_sample),
  "(6)" = feols(avexpr ~ indtime + cons1 + lat_abst, data = main_sample),
  "(7)" = feols(avexpr ~ euro1900, data = main_sample),
  "(8)" = feols(avexpr ~ euro1900 + lat_abst, data = main_sample),
  "(9)" = feols(avexpr ~ logem4, data = lpgp_sample),
  "(10)" = feols(avexpr ~ logem4 + lat_abst, data = lpgp_sample)
)
modelsummary(panel_A_models, output = "output/tables/Table3_PanelA_Determinants.xlsx")
cat("✓ Table 3 Panel A: output/tables/Table3_PanelA_Determinants.xlsx\n")

# TABLE 3 - Panel B
panel_B_models <- list(
  "(1)" = feols(cons00a ~ euro1900, data = lpgp_sample),
  "(2)" = feols(cons00a ~ euro1900 + lat_abst, data = lpgp_sample),
  "(3)" = feols(cons00a ~ logem4, data = main_sample),
  "(4)" = feols(cons00a ~ logem4 + lat_abst, data = main_sample),
  "(5)" = feols(democ00a ~ euro1900, data = lpgp_sample),
  "(6)" = feols(democ00a ~ euro1900 + lat_abst, data = lpgp_sample),
  "(7)" = feols(democ00a ~ logem4, data = lpgp_sample),
  "(8)" = feols(democ00a ~ logem4 + lat_abst, data = lpgp_sample),
  "(9)" = feols(euro1900 ~ logem4, data = lpgp_sample),
  "(10)" = feols(euro1900 ~ logem4 + lat_abst, data = lpgp_sample)
)
modelsummary(panel_B_models, output = "output/tables/Table3_PanelB_Early_Institutions.xlsx")
cat("✓ Table 3 Panel B: output/tables/Table3_PanelB_Early_Institutions.xlsx\n")

# TABLE 4 - Panel A
ajr_dta <- read_dta("data/maketable4.dta")
base_sample <- ajr_dta %>% filter(baseco == 1) %>% mutate(other_cont = if_else(shortnam %in% c("AUS", "MLT", "NZL"), 1, 0))
no_neo_europes_sample <- base_sample %>% filter(rich4 != 1)
no_africa_sample <- base_sample %>% filter(africa != 1)
iv_models <- list(
  "(1)" = feols(logpgp95 ~ 1 | avexpr ~ logem4, data = base_sample),
  "(2)" = feols(logpgp95 ~ lat_abst | avexpr ~ logem4, data = base_sample),
  "(3)" = feols(logpgp95 ~ 1 | avexpr ~ logem4, data = no_neo_europes_sample),
  "(4)" = feols(logpgp95 ~ lat_abst | avexpr ~ logem4, data = no_neo_europes_sample),
  "(5)" = feols(logpgp95 ~ 1 | avexpr ~ logem4, data = no_africa_sample),
  "(6)" = feols(logpgp95 ~ lat_abst | avexpr ~ logem4, data = no_africa_sample),
  "(7)" = feols(logpgp95 ~ africa + asia + other_cont | avexpr ~ logem4, data = base_sample),
  "(8)" = feols(logpgp95 ~ lat_abst + africa + asia + other_cont | avexpr ~ logem4, data = base_sample),
  "(9)" = feols(loghjypl ~ 1 | avexpr ~ logem4, data = base_sample)
)
modelsummary(iv_models, output = "output/tables/Table4_PanelA_2SLS.xlsx")
cat("✓ Table 4 Panel A: output/tables/Table4_PanelA_2SLS.xlsx\n")

# TABLE 5
ajr_dta <- read_dta("data/maketable5.dta")
base_sample <- ajr_dta %>% filter(baseco == 1)
brit_colonies_sample <- base_sample %>% filter(f_brit == 1)
iv_models_5 <- list(
  "(1)" = feols(logpgp95 ~ f_brit + f_french | avexpr ~ logem4, data = base_sample),
  "(2)" = feols(logpgp95 ~ lat_abst + f_brit + f_french | avexpr ~ logem4, data = base_sample),
  "(3)" = feols(logpgp95 ~ 1 | avexpr ~ logem4, data = brit_colonies_sample),
  "(4)" = feols(logpgp95 ~ lat_abst | avexpr ~ logem4, data = brit_colonies_sample),
  "(5)" = feols(logpgp95 ~ sjlofr | avexpr ~ logem4, data = base_sample),
  "(6)" = feols(logpgp95 ~ lat_abst + sjlofr | avexpr ~ logem4, data = base_sample),
  "(7)" = feols(logpgp95 ~ catho80 + muslim80 + no_cpm80 | avexpr ~ logem4, data = base_sample),
  "(8)" = feols(logpgp95 ~ lat_abst + catho80 + muslim80 + no_cpm80 | avexpr ~ logem4, data = base_sample)
)
modelsummary(iv_models_5, output = "output/tables/Table5_Additional_Controls.xlsx")
cat("✓ Table 5: output/tables/Table5_Additional_Controls.xlsx\n")

# TABLE 6
ajr_dta <- read_dta("data/maketable6.dta")
base_sample <- ajr_dta %>% filter(baseco == 1)
iv_models_6 <- list(
  "(1)" = feols(logpgp95 ~ lat_abst | avexpr ~ logem4, data = base_sample),
  "(2)" = feols(logpgp95 ~ lat_abst + temp1 + temp2 + temp3 + temp4 + temp5 | avexpr ~ logem4, data = base_sample),
  "(3)" = feols(logpgp95 ~ lat_abst + landlock | avexpr ~ logem4, data = base_sample),
  "(4)" = feols(logpgp95 ~ lat_abst + goldm + iron + silv + zinc + oilres | avexpr ~ logem4, data = base_sample)
)
modelsummary(iv_models_6, output = "output/tables/Table6_Geography_Resources.xlsx")
cat("✓ Table 6: output/tables/Table6_Geography_Resources.xlsx\n")

# TABLE 7
ajr_dta <- read_dta("data/maketable7.dta")
base_sample <- ajr_dta %>% filter(baseco == 1)
iv_models_7 <- list(
  "(1)" = feols(logpgp95 ~ lat_abst | avexpr ~ logem4, data = base_sample),
  "(2)" = feols(logpgp95 ~ lat_abst + malfal94 | avexpr ~ logem4, data = base_sample),
  "(3)" = feols(logpgp95 ~ lat_abst + leb95 | avexpr ~ logem4, data = base_sample),
  "(4)" = feols(logpgp95 ~ lat_abst + imr95 | avexpr ~ logem4, data = base_sample),
  "(5)" = feols(logpgp95 ~ lat_abst + yellow | avexpr ~ logem4, data = base_sample)
)
modelsummary(iv_models_7, output = "output/tables/Table7_Health_Geography.xlsx")
cat("✓ Table 7: output/tables/Table7_Health_Geography.xlsx\n")

# TABLE 8
ajr_dta <- read_dta("data/maketable8.dta")
base_sample <- ajr_dta %>% filter(baseco == 1)
panel_8_models <- list(
  "(1)" = feols(logpgp95 ~ 1 | avexpr ~ euro1900, data = base_sample),
  "(2)" = feols(logpgp95 ~ lat_abst | avexpr ~ euro1900, data = base_sample),
  "(3)" = feols(logpgp95 ~ 1 | avexpr ~ cons00a, data = base_sample),
  "(4)" = feols(logpgp95 ~ lat_abst | avexpr ~ cons00a, data = base_sample),
  "(5)" = feols(logpgp95 ~ 1 | avexpr ~ democ00a, data = base_sample),
  "(6)" = feols(logpgp95 ~ lat_abst | avexpr ~ democ00a, data = base_sample)
)
modelsummary(panel_8_models, output = "output/tables/Table8_Overidentification.xlsx")
cat("✓ Table 8: output/tables/Table8_Overidentification.xlsx\n")

cat("\n============================================================\n")
cat("✓ ALL TABLES EXPORTED TO output/tables/\n")
cat("============================================================\n")

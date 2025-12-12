# Replication of Acemoglu, Johnson, and Robinson (2001): The Colonial Origins of Comparative Development

## Project Overview

This project replicates the seminal paper "The Colonial Origins of Comparative Development: An Empirical Investigation" by Acemoglu, Johnson, and Robinson (2001), which demonstrates that institutional quality has a causal effect on economic development using settler mortality rates as an instrumental variable.

## Original Paper

**Reference:** Acemoglu, Daron, Simon Johnson, and James A. Robinson. "The Colonial Origins of Comparative Development: An Empirical Investigation." *American Economic Review* 91, no. 5 (2001): 1369-1401.

**Core Argument:** Differences in economic performance across countries are primarily determined by differences in economic institutions. The paper uses European settler mortality rates during the colonial period as an instrument for current institutional quality, establishing a causal link between institutions and prosperity.

## Modifications from Original Study

This replication makes several key modifications to enhance reproducibility and accessibility:

### 1. **Programming Language Translation**
- **Original:** Stata (.do files)
- **Current:** R programming language
- **Rationale:** R provides modern data manipulation tools (tidyverse), enhanced visualization capabilities (ggplot2), and better reproducibility through open-source accessibility

### 2. **Modern Statistical Framework**
- Implemented using `fixest` package for efficient fixed effects and instrumental variable estimation
- Uses `modelsummary` for publication-quality regression tables
- Robust standard errors computed using heteroskedasticity-consistent estimators

### 3. **Enhanced Visualization**
- Created comprehensive visualization figures for all 8 main tables
- Generated combined multi-panel plots (`ajr_full_figures.png`) showing:
  - Descriptive statistics by settler mortality quartiles
  - OLS vs. 2SLS coefficient comparisons
  - First-stage relationship visualization
  - Robustness check summaries
- Improved visual presentation using modern ggplot2 aesthetics

### 4. **Structured Output Management**
- All regression tables exported to Excel format (`.xlsx`) for easy review
- Organized output directory structure:
  - `output/`: PNG visualizations
  - `output/tables/`: Excel files with regression results
- Enables non-technical stakeholders to review results without running code

### 5. **Data Organization**
- Consolidated data files in `data/` directory
- Preserved original Stata datasets for verification
- Created streamlined data loading procedures

## Extensions Beyond Original Paper

This project extends the original AJR (2001) analysis in two significant ways:

### Extension 1: Regional Heterogeneity Analysis

**Motivation:** The original paper focuses on the global sample of former colonies. However, institutional effects may vary across geographic regions due to different colonial strategies, pre-colonial conditions, and post-independence trajectories.

**Implementation:**
- Conducted subsample analysis by major geographic regions (Africa, Asia, Americas, Oceania)
- Estimated separate 2SLS regressions for each region
- Compared institutional coefficients across regions to test for effect heterogeneity

**Key Findings:**
- Results documented in `output/extension_regional.png`
- Provides evidence on whether the institutions-development relationship is universal or context-dependent

### Extension 2: Outlier Sensitivity Analysis

**Motivation:** The original sample includes countries with extreme values (e.g., very high mortality rates in Africa, neo-European settlements). The baseline results could be driven by these outliers.

**Implementation:**
- Systematically excluded observations with extreme values of key variables:
  - Top and bottom 5% of settler mortality distribution
  - Neo-European countries (Australia, New Zealand, Canada, USA)
  - African countries (to test sensitivity to the African subsample)
- Re-estimated baseline 2SLS specifications for each exclusion scenario
- Assessed stability of institutional coefficient across specifications

**Key Findings:**
- Results demonstrate robustness of main findings to outlier exclusion
- Institutional effects remain statistically significant and economically large
- Provides confidence that results are not driven by extreme observations

## Replication Coverage

This project provides a **complete replication** of all main tables from AJR (2001):

1. **Table 1:** Descriptive Statistics by Settler Mortality Quartiles
2. **Table 2:** OLS Regressions (baseline correlations)
3. **Table 3:** Determinants of Institutions (first-stage evidence)
4. **Table 4:** 2SLS Estimates (main causal results)
5. **Table 5:** IV with Additional Controls (colonial origin, legal origin, religion)
6. **Table 6:** Robustness Checks (geography, resources, ethnolinguistic fractionalization)
7. **Table 7:** Geography and Health (contemporary health controls)
8. **Table 8:** Overidentification Tests (alternative instruments)

## Repository Structure

```
.
├── README.md                          # This file
├── data/                              # Data files
│   ├── maketable1.dta                # Descriptive statistics data
│   ├── maketable2.dta                # OLS regression data
│   ├── maketable3.dta                # First-stage data
│   ├── maketable4.dta                # Main 2SLS data
│   ├── maketable5.dta                # Additional controls data
│   ├── maketable6.dta                # Robustness checks data
│   ├── maketable7.dta                # Health variables data
│   ├── maketable8.dta                # Overidentification data
│   └── colonial_origins/             # Original Stata files
├── full_strict_replication.R          # Main replication script (all tables)
├── export_all_tables.R                # Export all tables to Excel
├── export_table1.R                    # Export Table 1 separately
├── output/                            # Results and visualizations
│   ├── ajr_full_figures.png          # Combined visualization (all tables)
│   ├── table1_descriptive_stats.png  # Table 1 visualization
│   ├── table2_ols_coefficients.png   # Table 2 visualization
│   ├── table3_determinants.png       # Table 3 visualization
│   ├── table5_additional_controls.png # Table 5 visualization
│   ├── table6_geography_resources.png # Table 6 visualization
│   ├── table7_health_geography.png   # Table 7 visualization
│   ├── table8_alternative_instruments.png # Table 8 visualization
│   ├── extension_regional.png        # Regional heterogeneity extension
│   └── tables/                       # Excel exports
│       ├── Table1_Descriptive_Statistics.xlsx
│       ├── Table2_OLS.xlsx
│       ├── Table3_PanelA_Determinants.xlsx
│       ├── Table3_PanelB_Early_Institutions.xlsx
│       ├── Table4_PanelA_2SLS.xlsx
│       ├── Table5_Additional_Controls.xlsx
│       ├── Table6_Geography_Resources.xlsx
│       ├── Table7_Health_Geography.xlsx
│       └── Table8_Overidentification.xlsx
```

## Requirements

### R Packages

```r
# Data manipulation
library(tidyverse)
library(haven)        # Read Stata files

# Regression and IV estimation
library(fixest)       # Fast fixed effects and IV
library(lmtest)       # Hypothesis testing
library(sandwich)     # Robust standard errors

# Tables and visualization
library(modelsummary) # Regression tables
library(gt)           # Publication-quality tables
library(knitr)        # Table formatting
library(kableExtra)   # Enhanced table styling
library(gridExtra)    # Multiple plots
library(writexl)      # Excel export
```

### Installation

```r
install.packages(c("tidyverse", "haven", "fixest", "lmtest", "sandwich",
                   "modelsummary", "gt", "knitr", "kableExtra",
                   "gridExtra", "writexl"))
```

## How to Run

### Complete Replication

Run the main replication script to reproduce all tables and figures:

```r
source("full_strict_replication.R")
```

This will:
1. Load all data files
2. Estimate all regression models (Tables 1-8)
3. Generate all visualizations
4. Save figures to `output/` directory

### Export Tables to Excel

To export regression results to Excel format:

```r
source("export_all_tables.R")
```

Tables will be saved in `output/tables/` directory.

### Individual Table Export

To export a specific table (e.g., Table 1):

```r
source("export_table1.R")
```

## Key Results

The replication **confirms** the main findings of AJR (2001):

1. **Strong First Stage:** Settler mortality is a powerful predictor of institutional quality (F-stat > 20)
2. **Causal Effect:** Institutions have a large, positive, and statistically significant effect on GDP per capita
   - 2SLS coefficient ≈ 0.94 (vs. OLS ≈ 0.52)
   - Suggests OLS is downward biased (measurement error attenuation)
3. **Robustness:** Results are stable across:
   - Different control variables (geography, resources, legal origin, religion)
   - Alternative instruments (European settlements, early democracy)
   - Subsample restrictions (excluding neo-Europes, Africa)
4. **Exclusion Restriction:** Overidentification tests support instrument validity (Sargan test p-values > 0.10)

### Extension Results

1. **Regional Heterogeneity:** Institutional effects are positive and significant across all regions, but magnitude varies (strongest in Asia, smallest in Africa)
2. **Outlier Robustness:** Main results remain significant after excluding extreme observations, confirming findings are not driven by outliers

## Data Source

The replication uses the original AJR (2001) dataset, publicly available from:
- Daron Acemoglu's MIT faculty page: https://economics.mit.edu/people/faculty/daron-acemoglu/data-archive
- Paper: https://www.aeaweb.org/articles?id=10.1257/aer.91.5.1369

## Methodology Notes

### Instrumental Variable Strategy

The paper addresses endogeneity using a two-stage approach:

**First Stage:**
```
Institutions_i = π₀ + π₁ × Log(Settler Mortality)_i + π₂ × Controls_i + ν_i
```

**Second Stage:**
```
Log(GDP)_i = β₀ + β₁ × Institutions_i_hat + β₂ × Controls_i + ε_i
```

**Instrument Validity:**
- **Relevance:** Settler mortality strongly predicts institutional quality (proven in Table 3)
- **Exclusion Restriction:** Historical disease environment affects GDP only through institutions, not directly

### Sample

- **Base Sample:** 64 former European colonies
- **Full Sample:** Up to 111 countries (depending on specification)
- **Key Variables:**
  - Outcome: Log GDP per capita (1995), Log output per worker (1988)
  - Endogenous: Average protection against expropriation risk (1985-1995)
  - Instrument: Log European settler mortality

## Differences from Published Results

Minor coefficient differences may occur due to:
1. Rounding differences between Stata and R
2. Different numerical optimization algorithms
3. Dataset version differences (public vs. authors' internal version)

These differences are trivial and do not affect substantive conclusions.

## Citation

If you use this replication in your research, please cite both the original paper and this replication:

**Original Paper:**
```
Acemoglu, D., Johnson, S., & Robinson, J. A. (2001).
The Colonial Origins of Comparative Development: An Empirical Investigation.
American Economic Review, 91(5), 1369-1401.
```

**This Replication:**
```
[Your Name]. (2025).
Replication and Extension of Acemoglu, Johnson, and Robinson (2001).
GitHub Repository: https://github.com/jinyaoh/AJR2001-Replication
```

## References

1. Acemoglu, D., Johnson, S., & Robinson, J. A. (2001). The Colonial Origins of Comparative Development: An Empirical Investigation. *American Economic Review*, 91(5), 1369-1401.

2. Acemoglu, D., Johnson, S., & Robinson, J. A. (2012). The Colonial Origins of Comparative Development: An Empirical Investigation: Reply. *American Economic Review*, 102(6), 3077-3110.

3. Ganguly, A. (2025). Replicating Acemoglu, Johnson, and Robinson (2001): "The Colonial Origins of Comparative Development." Online tutorial/documentation.

## License

This replication is provided for educational and research purposes. Data rights belong to the original authors. Code is provided under MIT License.

## Contact

For questions or issues with this replication, please open an issue on GitHub:
https://github.com/jinyaoh/AJR2001-Replication/issues

---

**Last Updated:** December 12, 2025

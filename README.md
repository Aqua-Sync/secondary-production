
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Global export of biomass and contaminants from rivers to land by aquatic insects

This page provides data and code for *Larsen et al.* *Global export of
biomass and contaminants from rivers to land by aquatic insects*.

All figures and tables in the manuscript can be recreated by running the
R scripts below. The scripts are named in order (e.g., 1, 2, 3…), so
that script 2 might not work without running script 1 first, and so
forth.

The scripts below are in the folders `code/emergence_modeling` or
`code/contaminants_modeling`:

| process | code_file | function | notes |
|:---|:---|:---|:---|
| emergence biomass modeling | 1_wrangle_data.R | Wrangles the raw literature-extracted secondary production and emergence data. Converts to common units in mgDM/m2/y. Checks for missing values and summarizes descriptive statistics. | NA |
| emergence biomass modeling | 2_estimate_aisp_from_acsp.R | Converts community production (acsp) to insect only production (aisp) | NA |
| emergence biomass modeling | 3_estimate_emergence_from_aisp.R | Converts aquatic insects secondary production (aisp) to emergence production. | NA |
| emergence biomass modeling | 4_estimate_emergence_from_aisp_taxa.R | Converts aquatic insects secondary production (aisp) to emergence production by taxonomic order. | NA |
| emergence biomass modeling | 5_wrangle_data_for_modeling.R | Standardizes (z-score) variables and adds attributes to standardized variables (i.e., center/scale). These are then used in subsequent models and code to back-transform temperature, precipitation, etc. | NA |
| emergence biomass modeling | 6_wrangle_data_for_modeling_taxa.R | Repeats 3.2_wrangle_data_for_modeling.R, but for each taxon. | NA |
| emergence biomass modeling | 7_prior_predictive.R | Simulates prior predictive for a model of emergence ~intercept. Compares to emergence = 1, which is the expected mean of emergence after scaling to the global mean of emergence. | NA |
| emergence biomass modeling | 8_fit_emergence_predictor_models.R | Fit GAMM models (n = 15 models) with various predictors to estimate annual insect emergence as a function of environmental predictors. Also fit model with taxon-specific predictions. | NA |
| emergence biomass modeling | 9_model_selection.R | Perform model selection on the 15 GAM models | NA |
| emergence biomass modeling | 10_fit_with_emergence_uncertainty.R | For the selected final model, re-fit using brm_multiple(), in which each “imputation” is a single draw of data from the posterior distribution of insect emergence. This incorporates the uncertainty in emergence into the final model that is used for predicting global patterns of biomass, nutrients, and contaminants. | NA |
| emergence biomass modeling | 11_check_and_plot_models.R | Conduct posterior predictive checks of GAM models and plot conditional effects. | NA |
| emergence biomass modeling | 12_make_vars.R | Wrangles data for predictor values of stream temp and precip for each of the \>900K hydrobasins. This is then used later to make predictions of emergence and contaminants in each hydrobasin. | NA |
| emergence biomass prediction | 13_predict_emergence_perm2.R | Summarizes global average of mass and nutrient emergence per square meter. | NA |
| emergence biomass prediction | 14_predict_emergence_perhybas.R | Predicts emergence of insect mass and nutrients (total flux per year) in each of the \>900K hydrobasins. | NA |
| emergence biomass prediction | 15_predict_emergence_global.R | Predicts global emergence of mass and nutrients per year by summing across iterations of predictions per hybas. | NA |
| emergence biomass prediction | 16_biomass_figures.R | Generates figures of biomass and nutrients used in the manuscript. | NA |
| emergence biomass prediction | 17_biomass_tables.R | Generates tables of biomass and nutrients used in the manuscript. | NA |
| emergence contaminants and PUFA prediction | 1_wrangle_data_contaminants.R | Wrangle contaminant and PUFA data for modeling (harmonize units, spelling, etc). | PUFA data are not contaminants, but they were extracted from the literature in the same data file as contaminants. Hence they are wrangled here in the “contaminants” section. |
| emergence contaminants and PUFA prediction | 2_plot_data_contaminants.R | Make exploratory plots and summaries of contaminants and PUFA data\> | PUFA data are not contaminants, but they were extracted from the literature in the same data file as contaminants. Hence they are wrangled here in the “contaminants” section. |
| emergence contaminants and PUFA prediction | 3_prior_predictive_contaminants.R | Check prior predictive distribution. | NA |
| emergence contaminants and PUFA prediction | 4_fit_models_contaminants.R | Fit Gamma regression between water concentrations and adult concentrations. Separate fits per contaminant. | NA |
| emergence contaminants and PUFA prediction | 5_check_models_contaminants.R | Check model fits. | NA |
| emergence contaminants and PUFA prediction | 6_predict_cides_flux_perhybas.R | Predict flux per hybas of “-cides” (fungicides, herbicides, and pesticides). | NA |
| emergence contaminants and PUFA prediction | 7_predict_metal_flux_perhybas.R | Predict flux per hybas of metals | NA |
| emergence contaminants and PUFA prediction | 8_predict_cides_perm2.R | Predict flux per square meter of water of “-cides” (fungicides, herbicides, and pesticides). | NA |
| emergence contaminants and PUFA prediction | 9_predict_cides_flux_globally.R | Predict flux per year of “-cides” (fungicides, herbicides, and pesticides) globally. | NA |
| emergence contaminants and PUFA prediction | 10_predict_metal_flux_globally.R | Predict flux per year of metals globally | NA |
| emergence contaminants and PUFA prediction | 11_predict_mercury_flux_compare_to_brandt.R | Compare insect Hg mercury flux to salmong Hg flux from Brandt et al. (2024). | NA |
| emergence contaminants and PUFA prediction | 12_predict_PUFA_flux_compare_to_brandt.R | Compare insect Hg PUFA flux to salmong PUFA flux from Brandt et al. (2024). | NA |
| emergence contaminants and PUFA prediction | 13_predict_relative_flux.R | Predict the relative flux of particular contaminants vs nutrients nutrients per HYBAS. | NA |
| emergence contaminants and PUFA prediction | 14_relative_importance_modeling.R | Estimate the relative importance of contaminant concentrations vs. biomass in explaining flux of contaminants | NA |
| emergence contaminants and PUFA prediction | 15_contaminant_figures | Generates figures of contaminants and PUFA used in the manuscript. | NA |
| emergence contaminants and PUFA prediction | 16_contaminant_tables | Generates tables of contaminants and PUFA used in the manuscript. | NA |

## Packages

| package      | version   |
|:-------------|:----------|
| VGAM         | 1.1-14    |
| bit64        | 4.8.0     |
| brms         | 2.23.1    |
| cowplot      | 1.2.0     |
| data.table   | 1.18.2.1  |
| directlabels | 2026.4.23 |
| dplyr        | 1.2.1     |
| foreign      | 0.8-91    |
| ggmap        | 4.0.2     |
| ggplot2      | 4.0.3     |
| ggrepel      | 0.9.8     |
| ggridges     | 0.5.7     |
| ggthemes     | 5.2.0     |
| ggview       | 0.2.2     |
| here         | 1.0.2     |
| isdbayes     | 0.1.0     |
| janitor      | 2.2.1     |
| knitr        | 1.51      |
| paletteer    | 1.7.0     |
| patchwork    | 1.3.2     |
| posterior    | 1.7.0     |
| readxl       | 1.4.5     |
| relaimpo     | 2.2-7     |
| renv         | 1.2.4     |
| rmarkdown    | 2.31      |
| scales       | 1.4.0     |
| sf           | 1.1-1     |
| stringi      | 1.8.7     |
| stringr      | 1.6.0     |
| svglite      | 2.2.2     |
| taxize       | 0.10.1    |
| tidybayes    | 3.0.7     |
| tidyr        | 1.3.2     |
| tidyverse    | 2.0.0     |
| viridis      | 0.6.5     |

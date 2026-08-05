
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Global export of biomass and contaminants from rivers to land by aquatic insects

This page provides data and code for *Larsen et al.* *Global export of
biomass and contaminants from rivers to land by aquatic insects*.

## Code

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

## Data

The table below describes each data file that is used/created by the
scripts above.

| Number | Data | Description | Use |
|---:|:---|:---|:---|
| 1 | ACSP_author_year.csv | Author and year information for extracted secondary and emergence production data. | Appends information to main data files described below. |
| 2 | ACSP_Data_ALL_ATTRIBUTES_PROCESSED_V3.csv | Literature-extracted production data (secondary and emergence) with environmental attributes from HydroBasin (e.g., temp, precip, etc.). | NA |
| 3 | ALL_SALMON_HYBAS-L12.rds | HYBAS codes for watersheds in Brandt et al. (2024). | To compare export of salmon in Brandt et al. (2024) to export of insects in the same locations. |
| 4 | AquaSync-Contaminant_transfer-2024-2-21_JMK(main_data_good_names).csv | Literature-extracted tissue and water concentration data of contaminants. | Raw (pre-wrangled) data used to model relationship between water concentrations and tissue concentrations. |
| 5 | AquaSync-Contaminant_transfer-2024-2-21_JMK(pufa).csv | Literature-extracted tissue concentraitons of PUFA. | Raw (pre-wrangled) data used to model PUFA concentrations in adult aquatic insects. |
| 6 | atlas.slim.rds | HYBAS codes that exclude desert or ice-covered basins. | Used to filter HYBAS predictions so that emergence isn’t predicted from ice-covered/dry basins. |
| 7 | attributes_by_id.rds | Environmental attributes for each extracted production value. | Used to append environmental attributes in wrangling. |
| 8 | cas_names.rds | Chemical Abstracts Service (CAS) registry numbers for each contaminant | Used to ensure consistent contaminant naming. |
| 9 | contaminants.rds | Wrangled contaminant tissue concentrations. | Cleaned version of data \#4 (AquaSync-Contaminant…good names).csv). Harmonized units, filtered to analyzed elements, quality-checked, etc. Used to model contaminant tissue concentrations. |
| 10 | data_to_predict.rds | Environmental parameters (e.g., temp, precip, etc.) for all ~ 900,000 HYBAS basins. | Used to predict export of biomass and elements from each basin based on posterior predictions of GAM models. |
| 11 | e_p_ratios.csv | Emergence:Production ratios from Gratton et al. (2009). | Used to model E:P ratios when converting secondary production to emergence production. |
| 12 | emergence_production.rds | Wrangled emergence production estimates. | Derived from data file \#2. |
| 13 | emergence_production_with_vars.rds | Same as data file \#12, but with environmental predictors added. | Used to model emergence production. |
| 14 | emergence_production_with_vars_taxa.rds | Same as data file \#13, but with estimates of taxon-specific emergence production rather than community production. | Used to model emergence production of individual taxa. |
| 15 | gratton_supplement_A.xlsx | Estimates from Gratton et al. (2009) of the proportion of total community secondary production that is insects. | Used to derive a prior proportion. |
| 16 | hybas_covariates.rds | Biome categories for all \>900,000 HYBAS basins | Used to estimate biome-specific export. |
| 17 | hybas_filtered.rds | HYBAS codes that exclude desert or ice-covered basins (NOTE: same as atlas.slim.rds? Could merge…) | Used to filter HYBAS predictions so that emergence isn’t predicted from ice-covered/dry basins. |
| 18 | hybas_regions.rds | Continent names for each HYBAS | Used to summarize export across continents. |
| 19 | hybas_regions_centroids.rds | Same as data \#18, but with lat/long centroids | Used to estimate latitudinal gradients in relative flux. |
| 20 | HYBAS_surface_area_REDIST.rds | Surface area of river water in each HYBAS. | Used to estimate total export per hybas. |
| 21 | hydrobasin_vars_rssa_short.rds | HYBAS environmental parameters. | Used to create data \#10 (data_to_predict.rds) |
| 22 | modeled_water.rds | Estimates of contaminant water concentrations in each HYBAS basin. | Used to predict contaminant export. |
| 23 | modeled_water_ids_mean.rds | Same as modeled_water.rds, but for pesticides only | Used to predict pesticide export. |
| 24 | pufa_data.rds | Wrangled concentraitons of PUFA concentrations in adult aquatic insects. | Used to model PUFA tissue concentrations. |
| 25 | PUFA_Data_ALL_ATTRIBUTES_PROCESSED_V1.csv | Raw (pre-wrangled) version of pufa_data.rds. | Similar to “AquaSync-Contaminant_transfer-2024-2-21_JMK(pufa).csv”, but with environmental attributes added. |
| 26 | secondary_prod.csv | Wrangled secondary production estimates from the raw data: ACSP_Data_ALL_ATTRIBUTES_PROCESSED_V3.csv | Used to model insect production as a fraction of community production. |
| 27 | secondary_prod_sd.rds | Similar to ‘secondary_prod.csv’ but with the posterior standard deviation of aquatic insect secondary produciton. | Used to simulate draws of secondary production, generating uncertainty. |
| 28 | secondary_prod_taxa.csv | Same as ‘secondary_prod.csv’, but for each taxon. | Used to model taxon-specific production. |
| 29 | secondary_prod_with_attributes.rds | Same as ‘secondary_prod.csv’, but with environmental attributes. | Used to re-append attributes to emergence estimates before fitting with GAMs. |

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

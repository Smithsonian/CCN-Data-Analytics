## CCN Blue Carbon Inventory

This repository contains the workflow for the CCN blue carbon data inventory analysis and publication. All soil core data used in this analysis is sourced from the [CCN Data Library](https://github.com/Smithsonian/CCN-Data-Library). This analysis is a follow up to the [original data inventory report](https://smithsonian.github.io/CCRCN-Pew-Project/) conducted in 2021.

***

**Contents**

- /data: Input data for the analysis. Largely, this contains spatial data derived from Coastal Change Analysis Program (CCAP) and National Wetlands Inventory (NWI) products which underlies inventory metrics.
- /report: Contains the output for the CCN Blue Carbon Data Inventory (BCDI), including the analysis report and associated bibliography.
- /scripts: Contains the scripts used to conduct this analysis.
- /figures: Contains the individual figures output from the analysis.

**To Reproduce this Analysis**

1. Run *wetland_area_summary.R* in the scripts folder. This script produces the underlying summary of land cover area for states overall and habitats within states. These two summary tables are output into the data folder for use in the inventory analysis.
2. Run *data_inventory_analysis.R* in the scripts folder. This sources the functions in *bcdi_functions.R* to calculate data inventory metrics for the CCN database. It then renders *bcdi_report.Rmd* to summarize these findings in a data visualization report.

## Coastal-Wetland-NGGI

This folder contains the public-facing literature review, data, and analysis which supports the EPA's coastal national greenhouse gas inventory. Methodology is based on the previous inventory conducted by [Lu et al 2017](https://github.com/Smithsonian/Coastal-Wetland-NGGI-Data-Public). This workflow is maintained by the [Coastal Carbon Network](https://serc.si.edu/coastalcarbon).

***

**Contents**

- /data: Contains the input data for the analysis. This includes the climate zone lookup table, the previously assembled table of reported values (2017 inventory), and the table of sediment accretion and carbon accumulation rates extracted from new literature for the 2022 inventory.
- /report: Contains the output for the 2022 NGGI inventory, including an updated emissions factors table, analysis report, and associated bibliography.
- /scripts: Contains the scripts used to conduct this analysis.

**To Reproduce this Analysis**

Run *nggi_data_analysis.R* in the scripts folder. This sources the functions in *nggi_utils.R* to perform the data analysis. It then renders *NGGI_2022_report.Rmd* to summarize these findings in a data visualization report.

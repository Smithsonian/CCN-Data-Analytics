The scripts folder contains the algorithmic workflow for calculating carbon accumulation rate. A few things are needed to replicate this workflow. 

First, a special version of the rplum package is needed. It can be downloaded from the Smithsonian GitHub. 

devtools::install_github("https://github.com/Smithsonian/rplum")

Second, one or more of the sub-directories in the Plum and Bacon output folders need to be removed manually from their directories. The workflow will only run Plum and Bacon (age-depth modeling workflows) if they have not already been completed. The code checks if a final file is present. If it is, it moves on. If you are double checking this workflow you may want to delete a few subfolders within to trigger the workflow to watch how it goes. The most extreme you could go is emptying the plum and bacon folders, but fair warning, re-running everything probably takes just over 24 hours. 

Third, you want to run each script in sequence. 

00_add_geography_to_meng_synthesis.R - This step accesses a version of the literature review (originated by Meng Lu back in 2017), and adds geographic attributes in a consistent way. 

01_dated_core_profiling.R - This step analyses all cores which have some type of dating information and makes a list of what methods are present, and prints a big list we can reference against when making sure the algorithm executed the correct workflows. 

02_pb210_visualization.R - This script organizes all of the information needed to calculate accretion rates using lead-210 data and visualizes profiles so that mistakes and anomalies in the database itself are more readily caught. The code consistently reports radium-226. Which is sometimes reported as is, and sometimes reported as either lead-214, bismuth-214 or both. It converts all total lead-210, excess lead-210 and radium-226 information into common unit (becquerels per kilogram). If sample level uncertainty was missing, we gap fill using an assumption of high uncertainty given the rest of the database (upper 95% distribution of the critical value [standard error / estimate]). In cases where only excess lead-210 (total lead-210 - radium-226) is reported, then we gap fill total lead-210 an assumption of average radium-226 informed from the database (total lead-210 = excess lead-210 + average radium-226). All profiles are visualized and saved as separate figures under 'outputs/figs/pb210'.

03_cs137_visualization.R - This script organizes accretion rate info from radiocesium dates. Study by study, site by site, and core by core, this algorithm iterates through the database. For each core radiocesium peaks are detected using a moving window approach. The most prominent peaks are assigned dates (in most cases 1963, but in cases of northern europe 1963 or 1986). If sample level uncertainty is missing, we gap fill using an assumption of high uncertainty given the rest of the database (upper 95% distribution of the critical value [standard error / estimate]). Peaks are scored to account for a level of confidence. They get one point for being significantly higher than background levels of radiation at the site level, and one point for being significantly different distribution relative to its shallower and deeper neighbor. A composite score of 0 to 3 indicates strength of the peak and separation from neighboring depth increments. For the rest of the workflow we filter out peaks with a score less than 2. Accretion rate means, min, and max values are estimated depending on the minimum and maximum potential depths of the peak (some studies date every other depth increment which makes for a wider depth of possibility). Age uncertainty is estimated using moment matching and approximating a normal distribution from a uniform distribution (Hobbs and Hooten, 2015).

04_historical_horizon_accretion_rates.R - This script organizes accretion rate info from miscellaneous historical horizons present in sediment, such as  pollen from introduced plants, occurrence of specific pollution events, or known changes in sediment quality that are visually apparent in a strata. These are varied and defined by original study authors. Here, similar to radiocesium dates, we estimate accretion rate means, min, and max values. Age uncertainty is estimated using moment matching and approximating a normal distribution from a uniform distribution (Hobbs and Hooten, 2015). 

05_c14_age_analysis.R - This script organizes radiocarbon information, filters out dates that are too young for our purposes, and consistently classifies organic versus carbonate sources. 

06_plum_bacon_workflow.R - This script triggers different age depth modeling workflows depending on data availability. If dry bulk density is missing, and fraction organic matter is present, then dry bulk density is gap filled as a function of fraction organic matter using the 'ideal mixing model' (Morris et al. 2016) and self packing densities of mineral and organic matter fit using the rest of the database. 

If dry bulk density is measured at a different resolution then radio isotopes, then they are resampled to match to radioisotope resolution using a depth weighted averaging. 

If historical horizons or radiocesium dates, or radiocarbon dates are present in addition to lead-210 information, then the rplum workflow is triggered with historical horizon and radiocarbon information added as auxiliary data. If only radiocarbon information is present then a separate rbacon workflow is run. 

For rplum workflows, we initially assume mean accretion rate of 5 years per centimeter and a mean memory of 0.66. Other priors are set to default settings.

If any radium-226 data is present, then plum is run with radium case set to 1. If no radium-226 data is present, then rplum is run with radium case set to 0.  

07_Plum_reruns.R - This script runs the same workflow, but only instances that did not arrive at a solution, with a different set of plum priors.

08_Plum_reruns_2.R - This script runs the same workflow, but only instances that did not arrive at a solution, with a different set of plum priors.

09_Plum_reruns_3.R - This script runs the same workflow, but only instances that did not arrive at a solution, with a different set of plum priors.

10_pb210_carbon_accumulation_calcs.R - This script iterates through the plum outputs, joins fraction organic matter density, dry bulk density, and fraction carbon data from the Coastal Carbon Library to the accretion rate data, gap fills dry bulk density, and fraction carbon in cases of missing data as functions of fraction organic matter using known relationships informed by the database and literature, and consistently calculates mean, and median carbon accumulation rate (grams carbon per square meter per year), as well as standard deviation, upper and lower 95% confidence intervals.  

11_cs137_carbon_accumulation_calcs.R - This script iterates through the cesium-226 and historical horizon accretion rate outputs, joins fraction organic matter density, dry bulk density, and fraction carbon data from the Coastal Carbon Library to the accretion rate data, gap fills dry bulk density, and fraction carbon in cases of missing data as functions of fraction organic matter using known relationships informed by the database and literature, and consistently calculates mean, and median carbon accumulation rate (grams carbon per square meter per year), as well as standard deviation, upper and lower 95% confidence intervals.  

12_method_intercomparisons.R - Joins algorithm based carbon accumulation rates to literature review values. Graphs relationships on a log scale. Calculates linear model statics.

13_state_level_emissions_factors.R - Calculates sate- habitat- and impact-level median and mean carbon accumulation rates for each method. Also calculates propegated uncertainty, and dataset size. Probability distributions for natural wetalnds are graphed as densities.  
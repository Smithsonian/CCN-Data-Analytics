This file contains a workflow for automatically calculating carbon accumulation rates from raw data within the Coastal Carbon Library

output - Folder contains outputs of the workflow.

scripts - Folder contains a sequence of scripts with which to run the work flow. 

ccn_dated_cores.csv - This csv file is produced by a script within the scripts folder. I used it as a reference to manually check which data points should be analyzed by this work flow and which versions of the algorithm should be triggered. 

pb210_manual_removals.csv - This csv file was created manually by inspecting all plum outputs and observing which graphs had anomalous or impossible solutions. These are graphs in which a single solution was not decided upon by the plum algorithm (the log-odds graph looked like it was jumping back and forth rather than than looking like a fuzzy ). Or results did not make physical sense. Observed values (boxes) did not overlap at all with the modeled values (blue stripes).   

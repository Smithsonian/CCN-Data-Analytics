
The files available in this directory provide the results of the GPS Imaging analysis of vertical time series available at the Nevada Geodetic Laboratory. Each file contains very similar information:

In Matlab: "VLM_Global_Imaged.mat"
variables in workspace (after "load VLM_Global_Imaged.mat; whos")
LONI - longitude (degrees)
LATI - latitude (degrees)
VU -  Vu (vertical rate in mm/yr)
SVU - Uncertainty in Vu (in mm/yr)
ZETA -  Nearest neighbor spatial variabilty in Vu (zeta, in mm/yr)

In GMT .grd format: "VuGlobal.grd"
variables in .grd (result of "gmt grdinfo VuGlobal.grd")
VuResampGlobal.grd: Title: 
VuResampGlobal.grd: Remark: 
VuResampGlobal.grd: Gridline node registration used [Cartesian grid]
VuResampGlobal.grd: Grid file format: nf = GMT netCDF format (32-bit float), COARDS, CF-1.5
VuResampGlobal.grd: x_min: 0 x_max: http://127.0.0.1:43437/graphics/plot_zoom_png?width=798&height=327360 x_inc: 0.25 name: x n_columns: 1441
VuResampGlobal.grd: y_min: -90 y_max: 90 y_inc: 0.25 name: y n_rows: 721
VuResampGlobal.grd: z_min: -20.0750007629 z_max: 41.2290000916 name: z
VuResampGlobal.grd: scale_factor: 1 add_offset: 0
VuResampGlobal.grd: format: netCDF-4 chunk_size: 131,145 shuffle: on deflation_level: 3

In netCDF format: "VLM_Global_Imaged.nc"
e.g., in Matlab try "ncdisp('VLM_Global_Imaged.nc')" to see contents

In text format: "VLM_Global_Imaged.txt"
column 1 - longitude (degrees)
column 2 - latitude (degrees)
column 2 - Vu (vertical rate in mm/yr) 
column 2 - Uncertainty in Vu (in mm/yr)
column 2 - Nearest neighbor spatial variabilty in Vu (zeta, in mm/yr)

The difference between uncertainty (SVU) and variabilty (ZETA) are discussed in Hammond et al., 2021 (see below for reference). 

References
===========

Global GPS Imaging analysis:
Hammond, W.C., G. Blewitt, C. Kreemer, 2016, GPS Imaging of vertical land motion in California and Nevada: Implications for Sierra Nevada uplift, Journal of Geophysical Research - Solid Earth, 121(10), p. 7681-7703, https://doi.org/10.1002/2016JB013458.

The GPS Imaging method:
Hammond, W.C., G. Blewitt, C. Kreemer, S. Nerem, 2021, Global vertical land motion for studies of sea level rise, J. Geophys. Res. - Solid Earth, 126(7), e2021JB022355, https://doi.org/10.1029/2021JB022355.

MIDAS robust trend estimator:
Blewitt, G., C. Kreemer, W.C. Hammond, J. Gazeaux, 2016, MIDAS Robust Trend Estimator for Accurate GPS Station Velocities Without Step Detection, Journal of Geophysical Research Solid Earth, 121, https://doi.org/10.1002/2015JB012552.

Nevada Geodetic Laboratory data products:
Blewitt, G., W.C. Hammond, C. Kreemer, 2018, Harnessing the GPS Data Explosion for Interdisciplinary Science, Eos, 99, https://doi.org/10.1029/2018EO104623.

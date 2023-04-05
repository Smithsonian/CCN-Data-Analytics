## Coastal Carbon Inventorying

## Script to Create Example Input Data

# activity data (how are you slicing the pie? And what does the pie encompass?)
activity_data <- data.frame(country = "Belize",
                            habitat = "mangrove", 
                            stratification = c("mainland", "offshore"),
                            area = c(372.04, 206.50))

# We identified and mapped a total of 578.54 km2 of mangrove ecosystem area as of
# 2020, with 372.04 km2
# located along the mainland coastal zone and 206.50 km2 distributed
# throughout the chain of islands and cayes. 

# c("healthy", "disturbed", "degraded"),

## emissions factors

# Tier 1

# t1 <- 

# need to create profiles

# country, habitat, stratification (may be several cols if nested), specific activity, emission factor

# stratifications based in per unit area of land use cat, 
# mineral v organic soil 
# vegetation type (species present)
# elevation, salinity, etc.
  
# convert mangrove area to hectares and multiply by the mean carbon stock  
total_Cstock <- 578.54 * 100 * 386
# now calculate CO2 equivalent
total_Cstock_co2 <- total_Cstock * 3.67

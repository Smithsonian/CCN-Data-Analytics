## Organizing citations for interface app, pull in citations for each country in 'Country Insights' and country reports 

#contact: cheneyr@si.edu

library(tidyverse)
library(dplyr)
library(readxl)
library(janitor)
library(RefManageR)
library(bib2df)

# List of needed citations 
# CCA, seagrass paper, seagrass individual sources, CCA study citations for cores used 

## 1. Read in citations and Country list ####

#country list 
ccn_countries <- read_csv("CCN_Official_Countries_and_Territories_Spelling.csv")

#CCA citations
library_citation <- as.data.frame(GetBibEntryWithDOI("https://doi.org/10.25573/serc.21565671.v6")) %>% 
  remove_rownames() %>% mutate(bibliography_id = "CCN_Data_Library")

ccn_studies <- read_csv("data/CCN_study_citations.csv")
ccn_cores <- read_csv("data/CCN_cores.csv") 


#get full country and ecosystem list 
country_territory_areas <- read_csv("seagrass_mapping/output/Marsh_mangrove_seagrass_territory_summary_tall.csv")
full_country_list <- country_territory_areas %>% 
  filter(!is.na(hectare)) %>% 
  select(country, ecosystem) %>% distinct()

 
## 2. Formatting Seagrass Sources ####

##Seagrass paper citation  AND individual sources
#spotfix citation formatting errors, match seagrass source to country 
seagrass <- read_xlsx("data/ERL_15_7_074041_suppdata.xlsx", sheet = 1) %>% 
  row_to_names(row_number = 11) %>% 
  rename(country = Sovereign, territory = Country) %>% 
  select(country, Source) %>%
  separate_longer_delim(cols = Source, delim = c(";")) %>%
  mutate(Source = case_when(grepl("no polygon", Source) ~ NA,
                            is.na(Source) ~ NA,
                            grepl("Skewes", Source) ~ "Skewes et al. 1999",
                            grepl("Kamal and Khan 2009", Source) ~ "Kamal and Khan 2009",
                            TRUE ~ str_squish(Source))) %>% filter(!is.na(Source))

#read in sources from supplementary data and paper citation                   <-- Need to FIX FORMATTING
seagrass_sources <- read_xlsx("data/ERL_15_7_074041_suppdata.xlsx", sheet = 2)

#join and spotfix, standardize country names 
seagrass_source_country <- full_join(seagrass, seagrass_sources) %>% 
  mutate(ecosystem = "seagrass",
         country = case_when(grepl("Gerakaris", Source) ~ "Greece",
                             grepl("Bahamas", country) ~ "Bahamas",
                             grepl("Verde", country) ~ "Cabo Verde",
                             grepl("Micronesia", country) ~ "Micronesia",
                             grepl("West Bank", country) ~ "Palestinian Territory",
                             grepl("Myanmar", country) ~ "Myanmar",
                             grepl("Tanzania", country) ~ "Tanzania",
                             grepl("Samoa", country) ~ "Samoa",
                             country == "Russia" ~ "Russian Federation",
                             country == "St. Kitts and Nevis" ~ "Saint Kitts and Nevis",
                             country == "St Lucia" ~ "Saint Lucia",
                             country == "St. Vincent and the Grenadines" ~ "Saint Vincent and the Grenadines",
                             TRUE ~ country), 
         Source = ifelse(grepl("seagrassspotter", Source), "Seagrass Spotter", Source)) %>% 
  filter(!is.na(country)) %>% 
  rename(bibliography_id = Source,
         reference = `Full reference`)


### -- Manually Editing Seagrass sources ####

#pull bibTex citations, format all to join to seagrass table 
UNEP <- data.frame(
  bibliography_id = "UNEP-WCMC and Short 2018",
  title = "Global distribution of seagrasses (version 6.0)",
  author = "UNEP-WCMC, FT Short",
  bibtype = "Misc", #dataset 
  doi = "",
  url = "https://wedocs.unep.org/bitstream/handle/20.500.11822/34031/GDS.pdf?sequence=1&isAllowed=y",
  publisher = "UN Environment Programme World Conservation Monitoring Centre",
  year = "2018"
)

natgeo_2000 <- data.frame(
  bibliography_id = "National Geographic Society 2000",
  title = "Coral World",
  author = "National Geographic Society",
  bibtype = "Misc", #map??
  doi = "",
  url = "",
  publisher = "",
  year = "2000"
)

green_and_short_2003 <- data.frame(
  bibliography_id = "Green and Short 2003",
  bibtype = "Book",
  title = "World Atlas of Seagrasses",
  author = "Green E.P. and Short F.T.",
  year = "2003",
  doi = "",
  url = "",
  publisher = "University of California Press, Berkeley, USA"
)

skewes_et_al_1999 <- data.frame(
  bibliography_id = "Skewes et al. 1999",
  title = "Survey and stock estimates of the shallow reef (0--15 m depth) and Shoal Area (15--50 m deep) marine resources and habitat mapping within the Timor Sea Mou 74Box. Vol 1: Stock estimates and stock status",
  author = "Skewes, TD and Dennis, DM and Jacobs, DR and Gordon, SR and Taranto, TJ and Haywood, M and Pitcher, CR and Smith, GP and Milton, D and Poiner, IR",
  bibtype = "Misc",
  doi = "",
  url = "",
  publisher = "CSIRO Report, 71p",
  year = "1999"
  )

coles_et_al_2009 <- as.data.frame(GetBibEntryWithDOI("https://doi.org/10.3354/meps08197")) %>% 
  remove_rownames() %>% 
  mutate(bibliography_id = "Coles et al. 2009")

mckenzie_et_al_2014a <- data.frame(GetBibEntryWithDOI("https://doi.org/10.1594/PANGAEA.826368")) %>% 
  remove_rownames() %>% 
  mutate(bibliography_id = "McKenzie et al. 2014a")

mckenzie_et_al_2014b <- data.frame(GetBibEntryWithDOI("10.1016/j.marpolbul.2014.07.019")) %>% 
  remove_rownames() %>% 
  mutate(bibliography_id = "McKenzie et al. 2014b")

mckenzie_et_al_2016b <- data.frame(GetBibEntryWithDOI("https://doi.org/10.1594/PANGAEA.858945")) %>% 
  remove_rownames() %>% 
  mutate(bibliography_id = "McKenzie et al. 2016b")

carter_et_al_2016 <- data.frame(
  bibliography_id = "Carter et al. 2016",
  title = "Seagrass mapping synthesis: A resource for coastal management in the Great Barrier Reef World Heritage Area",
  bibtype = "Misc",
  author = "Carter, A. B., McKenna, S. A., Rasheed, M. A., McKenzie L, Coles R. G",
  doi = "",
  url = "",
  publisher = "National Environmental Science Programme. Reef and Rainforest Research Centre Limited, Cairns (22 pp)",
  year = "2016")


carter_et_al_2014 <- data.frame(
  bibliography_id = "Carter et al. 2014",
  title = "Torres Strait Mapping: Seagrass Consolidation 2002-2014",
  bibtype = "Misc",
  author = "Carter AB, Taylor HA & Rasheed MA",
  doi = "",
  url = "",
  publisher = "JCU Publication, Report no. 14/55, Centre for Tropical Water & Aquatic Ecosystem Research, Cairns, 47 pp",
  year = "2014"
)


carter_and_rasheed_2016 <- data.frame(
  bibliography_id = "Carter and Rasheed 2016",
  title = "Assessment of Key Dugong and Turtle Seagrass Resources in Northwest Torres Strait",
  bibtype = "Misc",
  author = "Carter, A. B. and Rasheed, M. A.",
  doi = "",
  url = "",
  publisher = "Report to the National Environmental Science Programme and Torres Strait Regional Authority.Reef and Rainforest Research Centre Limited, Cairns, 41 pp",
  year = "2016"
)


coppo_et_al_2015 <- data.frame(
  bibliography_id = "Coppo et al. 2015",
  title = "Status of Coastal and Marine Assets in the Eastern Cape York region. TropWATER Report No 15/65",
  bibtype = "Misc",
  author = "Coppe, C., McKenzie, L.J., Brodie, J.",
  doi = "",
  url = "",
  publisher = "Centre for Tropical Water & Aquatic Ecosyystem Research, James Cook University, QLD",
  year = "2015"
)


saunders_et_al_2015 <- as.data.frame(GetBibEntryWithDOI("https://doi.org/10.1515/bot-2014-0060")) %>% 
  remove_rownames() %>% 
  mutate(bibliography_id = "Saunders et al. 2015")


kelly_and_chaffer_2012 <- data.frame(
  bibliography_id = "Kelly and Chaffer 2012",
  title = "Ecological Investigations to Support the Broadwater Masterplan. Report to Project Gold Coast City Council Report No GC1(2003)0",
  bibtype = "Misc",
  author = "Kelly, C. and Chaffer, K.",
  doi = "",
  url = "",
  publisher = "VDM Consulting (QLD) Pty Ltd, Southport, Queensland",
  year = "2012"
)


natura_2012 <- data.frame(
  bibliography_id = "Natura Consulting 2012",
  title = "Seagrass Health and Abundance Study (2012)",
  bibtype = "Misc",
  author = "Natura Consulting",
  doi = "",
  url = "",
  publisher = "Report prepared for the Gold Coast City Council",
  year = "2012"
)


hyland_et_al_1989 <- data.frame(
  bibliography_id = "Hyland et al. 1989",
  bibtype = "Book",
  title = "Distribution of seagrass in the Moreton region from Coolangatta to Noosa",
  author = "Hyland, S. J and Butler, C. T and Courtney, A. J and Queensland. Department of Primary Industries",
  year = "1989",
  doi = "",
  url = "https://nla.gov.au/nla.cat-vn4282217",
  publisher = "Dept. of Primary Industries, Queensland Government Brisbane 1989"
)

lucieer_et_al_2017 <- as.data.frame(GetBibEntryWithDOI("10.25959/5c06fc1beded6")) %>% 
  remove_rownames() %>% 
  mutate(bibliography_id = "Lucieer et al. 2017",
         year = "2017")


roelofs_et_al_2005 <- data.frame(
  bibliography_id = "Roelofs et al. 2005",
  title = "A survey of intertidal seagrass from Van Diemen Gulf to Castlereagh Bay, Northern Territory, and from Gove to Horn Island, Queensland",
  bibtype = "Misc",
  author = "Roelofs, A.J., Coles, R.G., Smit, N.",
  doi = "",
  url = "",
  publisher = "Report to the National Oceans Office. Department of Primary Industries & Fisheries, Cairns",
  year = "2005"
)

jimenez_2015 <- data.frame(
  bibliography_id = "Jimenez 2015",
  title = "Seagrass Monitoring Post-dredging Report: Ichthys Nearshore Environmental Monitoring Programme",
  bibtype = "Misc",
  author = "Roelofs, A.J., Coles, R.G., Smit, N.",
  doi = "",
  url = "",
  publisher = "Report to the National Oceans Office. Department of Primary Industries & Fisheries, Cairns",
  year = "2005"
)


williams_1994 <- as.data.frame(GetBibEntryWithDOI("https://doi.org/10.5479/si.00775630.406.1")) %>% 
  remove_rownames() %>% mutate(bibliography_id = "Williams 1994")


NSWparks <- data.frame(
  bibliography_id = "NSW Marine Parks Authority 2010",
  title = "Natural values of Lord Howe Island Marine Park",
  bibtype = "Misc",
  author = "NSW Marine Parks Authority",
  doi = "",
  url = "",
  publisher = "NSW Marine Parks Authority, Hurstville, NSW",
  year = "2010"
)


kamal_and_khan_2009 <- data.frame(
  bibliography_id = "Kamal and Khan 2009",
  title = "Coastal and estuarine resources of Bangladesh: management and conservation issues",
  bibtype = "Article",
  author = "Abu Hena Mostafa Kamal and M. Shah Alam Khan",
  journal = "Maejo International Journal of Science and Technology",
  volume = "3",
  pages = "313-342",
  year = "2009"
)


seagrass_spotter <- data.frame(
  bibliography_id = "Seagrass Spotter",
  title = "Seagrass Spotter",
  bibtype = "Misc",
  author = "Unsworth, R.K.F., Jones, B.L.H, Rogers, A., Cullen-Unsworth, L.C., Lilley, R.J",
  url = "https://seagrassspotter.org",
  year = "2024"
)

murdoch_et_al_2007 <- as.data.frame(GetBibEntryWithDOI("https://doi.org/10.3354/meps339123")) %>% 
  remove_rownames() %>% mutate(bibliography_id = "Murdoch et al. 2007")


christianen_et_al_2018 <- as.data.frame(GetBibEntryWithDOI("https://doi.org/10.1111/1365-2745.13021")) %>% 
  remove_rownames() %>% mutate(bibliography_id = "Christianen et al. 2018")


debrot_et_al_2014 <- data.frame(
  bibliography_id = "Debrot et al. 2014",
  title = "Habitat diversity and biodiversity of the benthic seascapes of St. Eustatius",
  bibtype = "Misc",
  author = "Debrot, Adolphe O and Houtepen, Eric and Meesters, Hubertus Wilhelmina Gerardus and van Beek, IJM and Timmer, Tania and Boman, Erik and de Graaf, Martin and Dijkman, EM and Hunting, Ellard R and Ballantine, David L",
  institution = "IMARES",
  year = "2014"
)

dutch_caribbean_alliance <- data.frame(
  bibliography_id = "Dutch Caribbean Nature Alliance 2014",
  title = "Sea grass area map of Saba",
  bibtype = "Misc",
  author = "Dutch Caribbean Nature Alliance",
  doi = "",
  url = "http://www.dcbd.nl/document/sea-grass-area-map-saba ",
  publisher = "",
  year = "2014"
)

deyanova_et_al_2013 <-  data.frame(
  bibliography_id = "Deyanova et al. 2013",
  title = "Seagrass Habitats Distribution and Ecological State Along the Southern Bulgarian Black Sea Coast",
  bibtype = "Misc",
  author = "Deyanova, D., Berov, D., Karamfilov, V.",
  doi = "",
  url = "",
  publisher = "In MARES 2020 International Conference “Marine Research Horizon 2020″. 17-20 September (2013), Varna, Bulgari",
  year = "2013"
)


UNEP_2007 <- data.frame(
  bibliography_id = "UNEP 2007",
  title = "Procedure for Determination of National and Regional Economic Values for Ecotone Goods and Services, 
  and Total Economic Values of Coastal Habitats in the Context of the UNEP/GEF Project Entitled:`Reversing Environmenal
  Degradation Trends in the South China Sea and Gulf of Thailand`",
  bibtype = "Misc",
  author = "United Nations Environmental Programme",
  doi = "",
  url = "",
  publisher = "South China Sea Knowledge Document No. 3. UNEP/GEF/SCS/Inf.3. United Nations Environment Programme, Bangkok, Thailand",
  year = "2007"
)


vibol_2008 <- data.frame(
  bibliography_id = "Vibol 2008",
  title = "National report on seagrass in the South China Sea - Cambodia, In National reports on seagrass in the South China Sea",
  bibtype = "Misc",
  author = "Vibol, O.",
  doi = "",
  url = "",
  publisher = "UNEP/GEF/SCS Technical Publication No. 12. ed. UNEP, p. 14. United Nations Environment Programme, Bangkok, Thailand",
  year = "2008"
)

CEC2016 <- data.frame(
  bibliography_id = "CEC 2016",
  title = "North America's Blue Carbon: Assessing Seagrass, Salt Marsh and Mangrove Distribution and Carbon Sinks",
  bibtype = "Misc",
  author = "Commission for Environmental Cooperation (CEC)",
  doi = "",
  url = "",
  publisher = "Commission for Environmental Cooperation (CEC), Montreal, Canada",
  year = "2016"
)
  
creed_et_al_2016 <- as.data.frame(GetBibEntryWithDOI("https://doi.org/10.1186/s41200-016-0067-9")) %>% 
  remove_rownames() %>% mutate(bibliography_id = "Creed et al. 2016")


dacosta_cottam_et_al_2009 <- data.frame(
  bibliography_id = "DaCosta-Cottam et al. 2009",
  title = "Cayman Islands National Biodiversity Action Plan (2009)",
  bibtype = "Misc",
  author = "DaCosta-Cottam, M., Olynik, J., Blumenthal, J., Godbeer, K.D., Gibb, J., Bothwell, J., Burton, F.J., Bradley, P.E.,
  Band, A., Austin, T., Bush, P., Johnson, B.J., Hurlston, L., Bishop, L., McCoy, C., Parsons, G., Kirkconnell, J., Halford, S., 
  Ebanks-Petrie, G.",
  doi = "",
  url = "",
  publisher = "Cayman Islands Government. Department of Environment, George Town, Grand Cayman",
  year = "2009"
)

zheng_et_al_2013 <- as.data.frame(GetBibEntryWithDOI("10.3724/SP.J.1003.2013.10038")) %>% 
  remove_rownames() %>% mutate(bibliography_id = "Zheng et al. 2013")


poonian_et_al_2016 <- as.data.frame(GetBibEntryWithDOI("https://doi.org/10.2989/1814232X.2016.1181103")) %>% 
  remove_rownames() %>% mutate(bibliography_id = "Poonian et al. 2016")


krupp_et_al_2009 <- data.frame(
  bibliography_id = "Krupp et al. 2009",
  title = "Growth dynamics and state of the seagrass Thalassia testudinum in the Gandoca-Manzanillo National Wildlife Refuge, Caribbean, Costa Rica",
  bibtype = "Article",
  author = "Krupp, Lucia S and Cort{\'e}s, Jorge and Wolff, Matthias",
  journal = "Revista de Biologìa Tropical",
  volume = "57",
  pages = "187--201",
  publisher = "http://creativecommons. org/licenses/by/3.0",
  year = "2009"
)
  

samper_villarreal_et_al_2018 <- as.data.frame(GetBibEntryWithDOI("https://doi.org/10.15517/rbt.v66i1.33260")) %>% 
  remove_rownames() %>% mutate(bibliography_id = "Samper-Villarreal et al. 2018") 


cerdeira_estrada_et_al_2008 <- data.frame(
  bibliography_id = "Cerdeira-Estrada et al. 2008",
  title = "Mapping of the spatial distribution of benthic habitats in the Gulf of Batabanó using Landsat-7 images",
  bibtype = "Article",
  author = "Cerdeira-Estrada, S., Lorenzo-Sánchez, S., Areces-Mallea, A., Martínez-Bayón, C.",
  journal = "Science Sea[online]",
  volume = "34 n.2",
  pages = "213-222",
  year = "2008"
)


ventura_diaz_et_al_2012 <- data.frame(
  bibliography_id = "Ventura Díaz and Rodríguez Cueto 2012",
  title = "Hábitats del golfo de Ana María identificados mediante el empleo de procesamiento digital de imágenes",
  bibtype = "Misc",  #magazine?
  author = "Ventura Díaz, Y., Rodríguez Cueto, Y.",
  publisher = "Revista de Investigaciones Marinas",
  volume = "32",
  pages = "1-8",
  year = "2012"
)


bostrom_et_al_2014 <- as.data.frame(GetBibEntryWithDOI("https://doi.org/10.1002/aqc.2424")) %>% 
  remove_rownames() %>% mutate(bibliography_id = "Boström et al. 2014")
  

steiner_and_willette_2010 <- data.frame(
  bibliography_id = "Steiner and Willette 2010",
  title = "Distribution and size of benthic marine habitats in Dominica, Lesser Antilles",
  bibtype = "Misc",
  author = "Claus Christoff Steiner, Sascha, Alexander Willette, Demian",
  doi ="",
  url = "https://revistas.ucr.ac.cr/index.php/rbt/article/view/5231",
  publisher = "Revista de Biologia Tropical",
  year = "2010"
)

de_mazières_2008 <- data.frame(
  bibtype = "phdthesis",
  bibliography_id = "de Mazières 2008",
  title = "Spatial Distribution of Reef Fish Communities: An Investigation of the Coral Coast, Fiji Islands",
  author = "De Mazi{\`e}res, Jeanne",
  year = "2008",
  school = "The University of the South Pacific Suva, Fiji"
)


harborne_et_al_2001 <- data.frame(
  bibliography_id = "Harborne et al. 2001",
  title = "Mamanuca coral reef conservation project - Fiji (2001): report summary",
  bibtype = "Misc",
  author = "Harborne, A., Solandt, J., Afzal, D., Andrews, M., Raines, P.",
  doi ="",
  url = "",
  publisher = "Coral Cay Conservation Ltd, London",
  year = "2001"
)

koshy_2001 <- data.frame(
  bibliography_id = "Koshy 2001",
  title = "An ecological and mapping study of the seagrass communities on Nukubuco Reef, Suva, Fiji",
  bibtype = "Misc", # MS Thesis?
  author = "Koshy, LP",
  doi ="",
  url = "",
  publisher = "Master of Science Thesis, School of Pure and Applied Sciences, University of the South Pacific",
  year = "2001"
)

phinn_et_al_2012 <- as.data.frame(GetBibEntryWithDOI("https://doi.org/10.1080/01431161.2011.633122")) %>% 
  remove_rownames() %>% mutate(bibliography_id = "Phinn et al. 2012")

roelfsema_et_al_2013b <- as.data.frame(GetBibEntryWithDOI("https://doi.org/10.1080/01431161.2013.800660")) %>% 
  remove_rownames() %>% mutate(bibliography_id = "Roelfsema et al. 2013b")

waycott_et_al_2011 <- data.frame(
  bibtype = "Misc",
  bibliography_id = "Waycott et al. 2011",
  title = "Vulnerability of mangroves, seagrasses and intertidal flats in the tropical Pacific to climate change",
  author = "Waycott, Michelle and McKenzie, Len J and Mellors, Jane E and Ellison, Joanna C and Sheaves, Marcus T and Collier, Catherine and Schwarz, Anne-Maree and Webb, Arthur and Johnson, Johanna E and Payri, Claude E",
  publisher = "Vulnerability of Tropical Pacific Fisheries and Aquaculture to Climate Change. Secretariat of the Pacific Community, Noumea, New Caledonia",
  pages = "297-368",
  year = "2011"
)

roelfsema_and_phinn_2010 <- as.data.frame(GetBibEntryWithDOI("https://doi.org/10.1117/1.3430107")) %>% 
  remove_rownames() %>% mutate(bibliography_id = "Roelfsema and Phinn 2010")

kolbe_2011 <- data.frame(
  bibliography_id = "Kolbe 2011",
  title = "Recording of seagrass populations in the Lower Saxony Wadden Sea using visual aerial image interpretation — 2008",
  bibtype = "Misc",
  author = "Koshy, LP",
  doi ="",
  url = "",
  publisher = "NLWKN, coastal waters and estuaries 4. Niedersächsische Landesbetrieb für Wasserwirtschaft, Küsten- und Naturschutz (NLWKN), Oldenburg",
  year = "2011"
)

ntiamoa_baidu_et_al_1998 <- as.data.frame(GetBibEntryWithDOI("https://doi.org/10.1111/j.1474-919X.1998.tb04545.x")) %>% 
  remove_rownames() %>% mutate(bibliography_id = "Ntiamoa-Baidu et al 1998")

gerakaris_et_al_2020 <- as.data.frame(GetBibEntryWithDOI("https://doi.org/10.1016/j.aquabot.2019.103151")) %>% 
  remove_rownames() %>% mutate(bibliography_id = "Gerakaris et al 2020")

baden_et_al_2003 <- as.data.frame(GetBibEntryWithDOI("https://doi.org/10.1579/0044-7447-32.5.374")) %>% 
  remove_rownames() %>% mutate(bibliography_id = "Baden et al. 2003")

nccos_2004 <- data.frame(
  bibliography_id = "NCCOS 2004",
  title = "Atlas of the Shallow-Water Benthic Habitats of American Samoa, Guam, and the Commonwealth of the Northern Mariana Islands",
  bibtype = "Misc", #technical report 
  author = "NCCOS",
  doi ="",
  url = "",
  publisher = "NOAA Technical Memorandum NOS NCCOS 8, Biogeography Team. NOAA National Centers for Coastal Ocean Science, Silver Spring, MD",
  year = "2004"
)

ballhorn_et_al_2014 <- data.frame(
  bibliography_id = "Ballhorn et al. 2014",
  title = "Establishing the baseline for seagrass and mangrove area cover in four Marine and Coastal Priority Protected Areas within the Meso-American Reef area: Punta de Manabique Wildlife Refuge, Guatemala",
  bibtype = "Misc",
  author = "Ballhorn, Uwe and Mott, Claudius and Atwood, EC and Siegert, F",
  doi ="",
  url = "",
  publisher = "Remote Sensing Solutions, München, Germany",
  year = "2014"
)


fong_1998 <- data.frame(
  bibliography_id = "Fong 1998",
  title = "Some aspects of the ecology of the seagrass  Zostera japonica in Hong Kong. p. 204",
  bibtype = "Misc",
  author = "Fong C.W.",
  doi ="",
  url = "",
  publisher = "University of Hong Kong, Hong Kong",
  year = "1998"
)

geevarghese_et_al_2018 <- as.data.frame(GetBibEntryWithDOI("https://doi.org/10.1016/j.ocecoaman.2017.10.032")) %>% 
  remove_rownames() %>% mutate(bibliography_id = "Geevarghese et al. 2018")

sjafrie_et_al_2018 <- data.frame(
  bibliography_id = "Sjafrie et al. 2018",
  title = "Status padang lamun Indonesia (2018)",
  bibtype = "Misc",
  author = "Sjafrie, N.D.M., Hernawan, U.E., Prayudha, B., Supriyadi, I.H., Iswari, M.Y., Rahmat, Anggraini, K., Rahmawati, S., Suyarso",
  doi ="",
  url = "",
  publisher = "Puslit Oseanografi - LIPI, Jakarta, Indonesia",
  year = "2018"
)


wilkes_et_al_2017 <- as.data.frame(GetBibEntryWithDOI("https://doi.org/10.1016/j.ecolind.2017.06.036")) %>% 
  remove_rownames() %>% mutate(bibliography_id = "Wilkes et al. 2017")


erftemeijer_and_shuail_2012 <- as.data.frame(GetBibEntryWithDOI("https://doi.org/10.1080/14634988.2012.668479")) %>% 
  remove_rownames() %>% mutate(bibliography_id = "Erftemeijer and Shuail 2012")
  

bujang_and_zakaria_2003 <-  data.frame(
  bibliography_id = "Bujang and Zakaria 2003",
  title = "The seagrasses of Malaysia, In World Atlas of Seagrasses. eds E. Green, F. Short, pp. 152-160. ",
  bibtype = "Misc",
  author = "Bujang, J.S., Zakaria, M.H.",
  doi ="",
  url = "",
  publisher = "University of California Press, Berkeley. USA",
  year = "2003"
)

orosco_et_al_2004 <- data.frame(
  bibliography_id = "Orosco and Amir Sharifudeen 2004",
  title = "The distribution of the seagrass, Halophila beccarii Aschers., in Terengganu River estuary, In KUSTEM Third Annual Seminar on Sustainability Science and Management (2004) : role of environmental science and technology in sustainable development of resources",
  bibtype = "Misc",
  author = "Orosco, C., Amir Sharifudeen, A.",
  editor = "M.Y. Norhayati, pp. 192-195",
  publisher = "Penerbit Kolej Universiti Sains dan Teknologi Malaysia, Primula Beach Resort, Kuala Terengganu, Terengganu",
  year = "2004"
)


turner_et_al_2000 <- data.frame(
  bibliography_id = "Turner et al. 2000",
  title = "The Mascarene Region, In Seas at the Millennium: An Environmental Evaluation",
  bibtype = "Misc",
  author = "Turner, J., Jago, C., Daby, D., Klaus, R.",
  doi ="",
  url = "",
  publisher = "ed. R. Sheppard, pp. 253–268. Elsevier, Amsterdam",
  year = "2000"
) 


ranjamani_and_marsh <- as.data.frame(GetBibEntryWithDOI("https://doi.org/10.1071/PC14908")) %>% 
  remove_rownames() %>% mutate(bibliography_id = "Rajamani and Marsh 2015")

turner_and_klaus <- as.data.frame(GetBibEntryWithDOI("https://doi.org/10.1098/rsta.2004.1489")) %>% 
  remove_rownames() %>% mutate(bibliography_id = "Turner and Klaus 2005")

mckenzie_and_rasheed <- as.data.frame(GetBibEntryWithDOI("https://doi.org/10.1594/PANGAEA.875721")) %>% 
  remove_rownames() %>% mutate(bibliography_id = "McKenzie and Rasheed 2006")

soe_htun_et_al_2015 <- data.frame(
  bibliography_id = "Soe-Htun et al. 2015",
  title = "Seagrass Conservation and Monitoring in Myanmar The biodiversity, distribution and coverage of seagrasses in the Tanintharyi and Rakhine",
  bibtype = "Misc",
  author = "Htun, U Soe and Maung, Antt and Mon, Salai and Soe, Thi and Ha, Soe and Aung, Myo and Lwin, Zau and Lunn",
  doi ="", url = "", publisher = "",
  year = "2015"
) 

anderson_et_al_2019 <- data.frame(
  bibliography_id = "Anderson et al. 2019",
  title = "Review of New Zealand’s key biogenic habitats. Prepared for the Ministry for the Environment. NIWA Client Report No: (2018)139WN",
  bibtype = "Misc",
  author = "Anderson, T.J., Morrison, M., MacDiarmid, A., Clark, M., D’Archino, R., Nelson, W., Tracey, D., Gordon, D., Read, G., Kettles, H., Morrisey, D., Wood, A., Anderson, O., Smith, A.M., Page, M., Paul-Burke, K., Schnabel, K., Wadhwa, S",
  doi ="", url = "", 
  publisher = "National Institute of Water & Atmospheric Research Ltd, Wellington, New Zealand",
  year = "2019"
) 

battista_et_al_2007 <- data.frame(
  bibliography_id = "Battista et al. 2007",
  title = "Shallow-Water Benthic Habitats of the Republic of Palau",
  bibtype = "Misc",
  author = "Battista, T., Costa, B., Anderson, S.",
  doi ="", url = "", 
  publisher = "NOAA Technical Memorandum. NOS NCCOS 59. Biogeography Branch, Silver Spring, MD",
  year = "2007"
  ) 

fortes_2008 <- data.frame(
  bibliography_id = "Fortes 2008",
  title = "National report on seagrass in the South China Sea – Philippines, In National reports on seagrass in the South China Sea",
  bibtype = "Misc",
  author = "Fortes, M.",
  doi ="", url = "", 
  publisher = "UNEP/GEF/SCS Technical Publication No. 12. ed. UNEP, p. 20. United Nations Environment Programme, Bangkok, Thailand",
  year = "2008"
) 

cunha_et_al_2014 <- as.data.frame(GetBibEntryWithDOI("https://doi.org/10.1016/j.aquabot.2011.08.007")) %>% 
  remove_rownames() %>% mutate(bibliography_id = "Cunha et al. 2014")
  

cuvillier_et_al_2017 <- as.data.frame(GetBibEntryWithDOI("https://doi.org/10.1016/j.ecss.2016.10.046")) %>% 
  remove_rownames() %>% mutate(bibliography_id = "Cuvillier et al. 2017")

hily_et_al_2010 <- as.data.frame(GetBibEntryWithDOI("https://doi.org/10.1016/j.marenvres.2015.03.004")) %>% 
  remove_rownames() %>% mutate(bibliography_id = "Hily et al. 2010")

jadot_2016 <- data.frame(
  bibliography_id = "Jadot 2016",
  title = "Environmental Conservation in Saint Barthélemy: Current knowledge and research reccommendations",
  bibtype = "Misc",
  author = "Jadot, C.",
  doi ="", url = "", 
  publisher = "Wildlife Conservation Society, Bronx, NY",
  year = "2016"
)

NOWPAP_CEARAC <- data.frame(
  bibliography_id = "NOWPAP CEARAC 2018",
  title = "Feasibility Study for Assessment of Seagrass Distribution in the NOWPAP Region",
  bibtype = "Misc",
  author = "NOWPAP CEARAC",
  doi ="", url = "", 
  publisher = "NOWPAP CEARAC, Toyama City, Japan",
  year = "2018"
)

alexandre_et_al_2017 <- as.data.frame(GetBibEntryWithDOI("https://doi.org/10.1016/j.aquabot.2017.06.008")) %>% 
  remove_rownames() %>% mutate(bibliography_id = "Alexandre et al 2017") 

cunha_and_araujo <- as.data.frame(GetBibEntryWithDOI("https://doi.org/10.1111/j.1365-2699.2009.02135.x")) %>% 
  remove_rownames() %>% mutate(bibliography_id = "Cunha and Araújo 2009") 

yaakub_et_al_2013 <- data.frame(
  bibliography_id = "Yaakub et al. 2013",
  title = "The diversity and distribution of seagrass in Singapore",
  bibtype = "Article",
  author = "Yaakub, S.M., Lim, R.L.F., Lim, W.L., Todd, P.A.",
  volume = "6",
  pages = "105-111",
  journal = "Nature in Singapore",
  year = "2018"
)

mckenzie_et_al_2016a <- as.data.frame(GetBibEntryWithDOI("https://doi.org/10.1594/PANGAEA.868773")) %>% 
  remove_rownames() %>% mutate(bibliography_id = "McKenzie et al. 2016a") 

roelfsema_et_al_2013a <- as.data.frame(GetBibEntryWithDOI("https://doi.org/10.1080/01431161.2013.800660")) %>% 
  remove_rownames() %>% mutate(bibliography_id = "Roelfsema et al. 2013a")

pernetta_1993 <- data.frame(
  bibliography_id = "Pernetta 1993",
  title = "Marine Protected Area Needs in the South Asian Seas Region. Volume 5: Sri Lanka. A Marine Conservation and Development Report",
  bibtype = "Misc",
  author = "Pernetta, J.C.",
  doi ="", url = "",
  publisher = "IUCN, Gland, Switzerland.",
  year = "1993"
)

weerakkody_and_suranjan <- data.frame(
  bibliography_id = "Weerakkody and Suranjan 2018",
  title = "Report on the survey of dugongs and seagrass of Gulf of Mannar 2016-2018",
  bibtype = "Misc",
  author = "Weerakkody, P., Suranjan, S.",
  doi ="", url = "",
  publisher = "Ocean Resources Conservation Association (ORCA), Galle, Sri Lanka",
  year = "2018"
)


huang_et_al_2015 <- data.frame(
  bibliography_id = "Huang et al. 2015",
  title = "Distribution of seagrass in the Moreton Region from Coolangatta to Noosa",
  bibtype = "Misc",
  author = "Hyland, S.J., Courtney, A.J., Butler, C.T.",
  doi ="", url = "",
  publisher = "Department of Primary Industries, Queensland Government, Brisbane",
  year = "2015"
)


lin_et_al_2005 <- data.frame(
  bibliography_id = "Lin et al. 2005",
  title = "Seagrasses of Tongsha Island, with descriptions of four new records to Taiwan",
  bibtype = "Misc",
  author = "Lin, H.-J., Hsieh, L.-Y., Liu, P.-J.",
  volume ="46", 
  pages = "163-168",
  publisher = "Botanical Bulletin Academia Sinica Taipei",
  year = "2005"
)

yang_et_al_2002 <- data.frame(
  bibliography_id = "Yang et al. 2002",
  title = "Taxonomy and Distribution of Seagrasses in Taiwan",
  bibtype = "Misc",
  author = "Yang, Y.-P., Fong, S.-C., Liu, H.-Y.",
  volume ="47", 
  pages = "54-61",
  publisher = "Taiwania",
  year = "2005"
)

boggs_et_al_2012 <- data.frame(
  bibliography_id = "Boggs et al. 2012",
  title = "Marine and Coastal Habitat Mapping in Timor Leste (North Coast) – Final Report. Project 1 of the Timor Leste Coastal-Marine Habitat Mapping, Tourism and Fisheries Development Project",
  bibtype = "Misc",
  author = "Boggs, G., Edyvane, K., de Carvalho, N., Penny, S., Rouwenhorst, J., Brocklehurst, P., Cowie, I., Barreto, C., Amaral, A., Monteiro, J., Pinto, P., Mau, R., Smit, N., Amaral, J., Fernandes, L.",
  doi ="", url = "",
  publisher = "Ministry of Agriculture & Fisheries, Government of Timor Leste, Dili, Timor Leste",
  year = "2012"
)


joyce_2013 <- data.frame(
  bibliography_id = "Joyce 2013",
  title = "Mapping and assessment of the coastal-marine habitats of Timor- Leste: South and Northwest Coasts",
  bibtype = "Misc",
  author = "Joyce, K.",
  doi ="", url = "",
  publisher = "Charles Darwin University, Darwin, NT",
  year = "2013"
)

ead_2017 <- data.frame(
  bibliography_id = "EAD 2017",
  title = "Abu Dhabi state of environment report",
  bibtype = "Misc", #report?
  author = "EAD",
  doi ="", url = "",
  publisher = "Environment Agency Abu Dhabi, Abu Dhabi, UAE",
  year = "2017"
)

sherman_and_debruyckere <- data.frame(
  bibliography_id = "Sherman and DeBruyckere 2018",
  title = "Eelgrass habitats of the U.S. West Coast. State of the Knowledge of Eelgrass Ecosystem Services and Eelgrass Extent",
  bibtype = "Misc", 
  author = "Sherman, K., DeBruyckere, L.A.",
  doi ="", url = "",
  publisher = "Pacific Marine and Estuarine Fish Habitat Partnership for The Nature Conservancy",
  year = "2017"
)

bae_systems_2007 <- data.frame(
  bibliography_id = "BAE Systems 2007",
  title = "Mapping of Benthic Habitats for The Main Eight Hawaiian Islands: Task Order 1 Project Completion Report",
  bibtype = "Misc", 
  author = "BAE Systems",
  doi ="", url = "",
  publisher = "BAE Systems Sensor Solutions Identification & Surveillance (S2 IS), Honolulu, HI",
  year = "2007"
)

luong_et_al_2012 <- as.data.frame(GetBibEntryWithDOI("https://doi.org/10.1117/12.977277")) %>% 
  remove_rownames() %>% mutate(bibliography_id = "Luong et al. 2012")

lorde_2011 <- data.frame(
  bibliography_id = "Lorde 2011",
  title = "An assessment of the economic impact of climate change on the coastal and marine sector in the British Virgin Islands",
  bibtype = "Misc", 
  author = "Lorde, T.",
  doi ="", url = "",
  publisher = "Economic Commission for Latin America and the Caribbean (ECLAC) Subregional Headquarters for the Caribbean, Port of Spain, Trinidad and Tobago",
  year = "2011"
)

tyllianakis_et_al_2019 <- as.data.frame(GetBibEntryWithDOI("https://doi.org/10.1016/j.scitotenv.2018.09.296")) %>% 
  remove_rownames() %>% mutate(bibliography_id = "Tyllianakis et al 2019")

mmrdc_2015 <- data.frame(
  bibliography_id = "Marine and Mangrove Research and Development Center 2015",
  title = "Report on the assessnebt and status of marine and coastal resources: coral and seagrass",
  bibtype = "Misc", 
  author = "Marine and Mangrove Research and Development Center",
  doi ="", url = "",
  publisher = "Department of Marine and Coastal Resources, Ministry of Natural Resources and Environment, Agricultural Cooperatuve Federation of Thailand, Thailand",
  year = "2015"
)


#3. Join Citations ##### 

seagrassestake2 <- rbind(
  UNEP, natgeo_2000, skewes_et_al_1999, green_and_short_2003, carter_et_al_2016, carter_et_al_2014, carter_and_rasheed_2016, 
  coppo_et_al_2015, kelly_and_chaffer_2012, natura_2012, hyland_et_al_1989, roelofs_et_al_2005, jimenez_2015, NSWparks, 
  dutch_caribbean_alliance, deyanova_et_al_2013, UNEP_2007, vibol_2008, CEC2016, dacosta_cottam_et_al_2009,
  steiner_and_willette_2010, harborne_et_al_2001, koshy_2001, kolbe_2011, nccos_2004, ballhorn_et_al_2014,
  fong_1998, sjafrie_et_al_2018,bujang_and_zakaria_2003, turner_et_al_2000, soe_htun_et_al_2015, anderson_et_al_2019,
  battista_et_al_2007, fortes_2008, jadot_2016, NOWPAP_CEARAC, pernetta_1993, weerakkody_and_suranjan, huang_et_al_2015,
  boggs_et_al_2012, joyce_2013, ead_2017, sherman_and_debruyckere, bae_systems_2007, lorde_2011, mmrdc_2015) %>% 
  full_join(coles_et_al_2009) %>% 
  full_join(mckenzie_et_al_2014a) %>% 
  full_join(mckenzie_et_al_2014b) %>% 
  full_join(mckenzie_et_al_2016b) %>% 
  full_join(mckenzie_et_al_2016a) %>% 
  full_join(saunders_et_al_2015) %>% 
  full_join(lucieer_et_al_2017) %>% 
  full_join(williams_1994) %>% 
  full_join(kamal_and_khan_2009) %>% 
  full_join(murdoch_et_al_2007) %>% 
  full_join(christianen_et_al_2018) %>% 
  full_join(debrot_et_al_2014) %>% 
  full_join(creed_et_al_2016) %>% 
  full_join(zheng_et_al_2013) %>% 
  full_join(poonian_et_al_2016) %>% 
  full_join(krupp_et_al_2009) %>% 
  full_join(samper_villarreal_et_al_2018) %>% 
  full_join(cerdeira_estrada_et_al_2008) %>% 
  full_join(ventura_diaz_et_al_2012) %>% 
  full_join(bostrom_et_al_2014) %>% 
  full_join(de_mazières_2008) %>% 
  full_join(phinn_et_al_2012) %>% 
  full_join(roelfsema_et_al_2013b) %>%
  full_join(roelfsema_et_al_2013a) %>% 
  full_join(waycott_et_al_2011) %>% 
  full_join(roelfsema_and_phinn_2010) %>% 
  full_join(ntiamoa_baidu_et_al_1998) %>% 
  full_join(baden_et_al_2003) %>% 
  full_join(gerakaris_et_al_2020) %>% 
  full_join(geevarghese_et_al_2018) %>% 
  full_join(wilkes_et_al_2017) %>% 
  full_join(erftemeijer_and_shuail_2012) %>% 
  full_join(orosco_et_al_2004) %>% 
  full_join(ranjamani_and_marsh) %>% 
  full_join(turner_and_klaus) %>% 
  full_join(mckenzie_and_rasheed) %>% 
  full_join(cunha_et_al_2014) %>% 
  full_join(cuvillier_et_al_2017) %>% 
  full_join(hily_et_al_2010) %>% 
  full_join(alexandre_et_al_2017) %>% 
  full_join(cunha_and_araujo) %>% 
  full_join(yaakub_et_al_2013) %>% 
  full_join(lin_et_al_2005) %>% 
  full_join(yang_et_al_2002) %>% 
  full_join(luong_et_al_2012) %>% 
  full_join(tyllianakis_et_al_2019) %>% 
  full_join(seagrass_spotter)

#join and spotfix 
FIXseagrass_citations <- seagrass_source_country %>% 
  select(-reference) %>% 
  full_join(seagrassestake2) %>% 
  filter(!bibliography_id == "Gerakaris et al. 2020") %>% #remove duplicate with formatting error
  distinct()

###

#Add country and habitat information to CCN Library citations
ccn_cores_country <- ccn_cores %>% 
  select(study_id, habitat, country) %>% distinct()


ccn_study_citations <- full_join(ccn_studies, ccn_cores_country) %>% 
  rename(ecosystem = habitat) %>%
  distinct()


#Len J McKenzie et al 2020 Environ. Res. Lett. 15 074041
# mckenzie_et_al <- tibble(
#   ecosystem = "seagrass",
#   Source = "McKenzie et al 2020",
#   `Full reference` = "Len J McKenzie et al 2020 Environ. Res. Lett. 15 074041")
mckenzie_et_al <- as.data.frame(GetBibEntryWithDOI("10.1088/1748-9326/ab7d06")) %>%
  remove_rownames() %>% 
  mutate(bibliography_id = "McKenzie_et_al_2020",
         ecosystem = "seagrass") 


# Marsh citation
worthington_et_al_2024 <- as.data.frame(GetBibEntryWithDOI("https://doi.org/10.1111/geb.13852")) %>% 
  remove_rownames() %>% 
  mutate(bibliography_id = "Worthington_et_al_2024",
         ecosystem = "marsh")


# Global Mangrove Watch citation 
gmw <- as.data.frame(GetBibEntryWithDOI("https://doi.org/10.3390/rs10101669")) %>% 
  remove_rownames() %>% 
  mutate(bibliography_id = "Bunting_et_al_2018",
         ecosystem = "mangrove")


add_citations <- bind_rows(mckenzie_et_al, worthington_et_al_2024, gmw, library_citation)


## Add marsh, mangrove, seagrass citations to each country 
## Add CCN library citations

country_index <- full_country_list %>% 
  filter(!ecosystem == "total") %>% 
  full_join(add_citations) %>% 
  full_join(FIXseagrass_citations) %>%
  mutate(volume = ifelse(!is.na(volume), as.numeric(volume), NA),
         year = as.numeric(year)) %>% 
  full_join(ccn_study_citations) %>% 
 # mutate(study_id = ifelse(is.na(study_id), bibliography_id, study_id),
        # bibliography_id = paste0(bibliography_id, "_", ecosystem, "_", country)) %>% 
  select(country, ecosystem, bibliography_id, study_id, everything())
  
##NEED TO FIX SPACES IN BIB ID NAMES 

#4. Write full citation list to csv ####
write_csv(country_index, "inventory_app_input/citations_by_country.csv")


#convert to bib? 
## getting errors with duplicate row_names/bibliography, same sources cited for multiple countries 
# warning message: duplicate rownames are not allowed - adding ecosystem and country to bib id fixes this 

country_Bib <- as.BibEntry(country_index %>%
                            # mutate(bibliography_id = paste0(bibliography_id, "_", ecosystem, "_", country)) %>% 
                             column_to_rownames(var = "bibliography_id"))

WriteBib(country_Bib, "inventory_app_input/citations_by_country.bib")


#write bib file without duplicate bib entries
bib_unique <- as.BibEntry(country_index %>% 
  select(-country, -ecosystem) %>% distinct() %>% 
  column_to_rownames(var = "bibliography_id"))

WriteBib(bib_unique, "inventory_app_input/unique_citations.bib")




markdown::renderMarkdown("TEST_country_insights.Rmd")




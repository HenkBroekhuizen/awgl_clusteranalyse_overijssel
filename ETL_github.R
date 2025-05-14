# title: Script for AWGL project
# Author: Henk Broekhuizen
# Date: 27/11/2024
# Updated by author: Henk Broekhuizen and Floor Kerkhof
# Date updated: 08/04/2025

#### Install and load needed packages if not installed yet ####
pkg_req = c("cbsodataR", "stringr", "sf", "tidyverse", "readxl", "openxlsx", "st", "terra", "exactextractr")

for (pkg in pkg_req) {
  if (system.file(package = pkg) == "") {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

#### Load required ggd functions ####
normalize <- function(x, scale.out = 100) {
  return(scale.out * (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

#### Initialize variables ####
data <- NULL
data_path_gecodes <- "I:/Repositories/__R_CODE__/Eenmalige projecten/AWGL_project_Overijssel_2024/brondata/WijkBuurtkaart_2022_v3/wijkenbuurten_2022_v3.gpkg"
data_path_kea <- "I:/Repositories/__R_CODE__/Eenmalige projecten/AWGL_project_Overijssel_2024/brondata/KEA_GroenGrijsPerBuurt_2023_03.xlsx"
data_path_no2 <- "I:/Repositories/__R_CODE__/Eenmalige projecten/AWGL_project_Overijssel_2024/brondata/02_Luchtkwaliteit_NO2/rivm_nsl_20240401_gm_NO22022.tif"
data_path_pm25 <- "I:/Repositories/__R_CODE__/Eenmalige projecten/AWGL_project_Overijssel_2024/brondata/03_Luchtkwaliteit_PM25/rivm_nsl_20240401_gm_PM252022.tif"
data_path_sound <- "I:/Repositories/__R_CODE__/Eenmalige projecten/AWGL_project_Overijssel_2024/brondata/01_Geluid_objectief/rivm_20220601_Geluid_lden_allebronnen_2020_v2.tif"
data_path_heat <- "I:/Repositories/__R_CODE__/Eenmalige projecten/AWGL_project_Overijssel_2024/brondata/Hitte/Hitte/Hittekaart gevoelstemperatuur Huidig.tif"
data_path_output <- "I:/Repositories/__R_CODE__/Eenmalige projecten/AWGL_project_Overijssel_2024/output etl/data.gpkg"
overijssel <- c("Almelo", "Borne", "Dinkelland", "Enschede", "Haaksbergen", "Hellendoorn", 
            "Hengelo", "Hof van Twente", "Losser", "Oldenzaal", "Rijssen-Holten", 
            "Tubbergen", "Twenterand", "Wierden", "Dalfsen", "Deventer", "Hardenberg", "Kampen", "Olst-Wijhe", 
            "Ommen", "Raalte", "Staphorst", "Steenwijkerland", 
            "Zwartewaterland", "Zwolle")
  
#### Extract data ####
##### Geocodes #####
# geocodes
geocodes <- st_read(
  dsn = data_path_gecodes,
  layer = "buurten"
  ) %>%
  select(geom, buurtcode, buurtnaam, gemeentecode, gemeentenaam, aantal_inwoners) %>% 
  filter(gemeentenaam %in% overijssel) %>% 
  mutate(aantal_inwoners = case_when(aantal_inwoners < 0 ~ 0, T ~ aantal_inwoners))

##### SMAP data #####
smap_data <- cbs_get_data(
  id = "50120NED",
  catalog = "RIVM",
  select = c("Marges", "Leeftijd","WijkenEnBuurten", "Perioden", "SoortRegio_2", 
             "Codering_3", "ErvarenGezondheidGoedZeerGoed_4", "VoldoetAanBeweegrichtlijn_5", 
             "ErnstigOvergewicht_10", "ErnstigZeerErnstigEenzaam_28"),
  Perioden = "2022JJ00",
  WijkenEnBuurten = has_substring("BU"),
  Marges = "MW00000",
  Leeftijd = "80200"
) %>% 
  select(-Marges, -Leeftijd, -Perioden, -SoortRegio_2, -Codering_3)

##### KWB data #####
kwb_data <- cbs_get_data(
  id = "85318NED",
  WijkenEnBuurten = has_substring("BU"),
  select = c("WijkenEnBuurten", "MateVanStedelijkheid_116",
             "HuishoudensMetEenLaagInkomen_78", "OpleidingsniveauLaag_64", "k_65JaarOfOuder_12",
             "AfstandTotHuisartsenpraktijk_106", "AfstandTotGroteSupermarkt_107", "AfstandTotSchool_109")
) %>% 
  mutate(
    voorzieningen = (AfstandTotHuisartsenpraktijk_106 + AfstandTotGroteSupermarkt_107 + AfstandTotSchool_109)/3
  ) %>% 
  select(-AfstandTotHuisartsenpraktijk_106, -AfstandTotGroteSupermarkt_107, -AfstandTotSchool_109)

##### Klimaateffectatlas #####
kea_data <- read_xlsx(data_path_kea) %>% 
  select(buurtcode, PercentageGroen)

#### Raster data ####
##### NO2 #####
no2 <- rast(data_path_no2)
##### PM2.5 #####
pm25 <- rast(data_path_pm25)
##### Sound #####
sound <- rast(data_path_sound)
##### Heat #####
heat <- rast(data_path_heat)

#### Join data ####
data <- geocodes %>% 
  left_join(smap_data, join_by(buurtcode == WijkenEnBuurten)) %>% 
  left_join(kwb_data, join_by(buurtcode == WijkenEnBuurten)) %>% 
  left_join(kea_data, join_by(buurtcode == buurtcode)) %>%
  mutate(
    no2_median = exact_extract(x = no2, y = geom, fun = "median"),
    pm25_median = exact_extract(x = pm25, y = geom, fun = "median"), 
    sound_median = exact_extract(x = sound, y = geom, fun = "median"),
    heat_median = exact_extract(x = heat, y = geom, fun = "median")
  )

# normalize
data <- data %>%
  mutate(across(where(is.numeric), normalize))

#### Save final data ####
st_write(data, data_path_output, append=F)

library(httr)
library(jsonlite)
library(tidyverse)

zip_direct <- fromJSON(txt = "https://services.arcgis.com/njFNhDsUCentVYJW/arcgis/rest/services/MDCOVID19_MASTER_ZIP_CODE_CASES/FeatureServer/0/query?where=ZIP_CODE%20%3D%20%2720912%27&outFields=*&outSR=4326&f=json")

zip_fields <- zip_direct$features$attributes

write_rds(zip_fields, "./data/zip_fields.rds")

zip_vax <- jsonlite::fromJSON("https://services.arcgis.com/njFNhDsUCentVYJW/arcgis/rest/services/MD_COVID_ZIPCODES_Vaccinations/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json")

zip_vax_data <- zip_vax$features$attributes

write_rds(zip_vax_data, "./data/zip_vax.rds")

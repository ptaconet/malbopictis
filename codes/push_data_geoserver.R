library(geosapi)
library(sf)

setwd(file.path("donnees","donnees_cartes"))

GSman <- GSManager$new(
  url = "https://geodata.bac-a-sable.inrae.fr/geoserver",
  user = "omees", pwd = "HHKcue51!HHKcue51!",
  logger = 'DEBUG'
)

couches <- list.files()
couches <- gsub("\\.gpkg","",couches)

for(i in 1:length(couches)){
  
ds = GSGeoPackageDataStore$new(name=couches[[i]],
                               description = "malbopictis layer",
                               enabled = TRUE,
                               database = paste0("/mnt/geoserver_geodata/",couches[[i]],".gpkg"))

created <- GSman$createDataStore("omees", ds)

uploaded <- GSman$uploadGeoPackage(
  ws = "omees", ds = couches[[i]],
  endpoint = "file", configure = "first", update = "overwrite",
  charset = "UTF-8", filename = paste0(couches[[i]],".gpkg")
)

}


created <- GSman$createStyle(file = "/home/ptaconet/contributions_diverses_projets_mivegec/malbopictis/donnees/styles_couches/omees_malbo_vege.sld", name = "omees-malbopictis-perc-vegetation")
created <- GSman$createStyle(file = "/home/ptaconet/contributions_diverses_projets_mivegec/malbopictis/donnees/styles_couches/omees_malbo_points_lacher.sld", name = "omees-malbopictis-points-lacher")
created <- GSman$createStyle(file = "/home/ptaconet/contributions_diverses_projets_mivegec/malbopictis/donnees/styles_couches/omees_malbo_zone_lacher.sld", name = "omees-malbopictis-zone-lacher")
created <- GSman$createStyle(file = "/home/ptaconet/contributions_diverses_projets_mivegec/malbopictis/donnees/styles_couches/omees_malbo_zone_suivie.sld", name = "omees-malbopictis-zone-suivie")
created <- GSman$createStyle(file = "/home/ptaconet/contributions_diverses_projets_mivegec/malbopictis/donnees/styles_couches/omees_malbo_limites_malbosc.sld", name = "omees-malbopictis-limites-malbosc")
created <- GSman$createStyle(file = "/home/ptaconet/contributions_diverses_projets_mivegec/malbopictis/donnees/styles_couches/omees_malbo_parcelles_typehabitat.sld", name = "omees-malbopictis-parcelles-typehabitat")


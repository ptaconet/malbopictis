library(readxl)
library(tidyverse)
library(ggmap)
library(sf)
library(patchwork)


##################
## Points de lacher
##################

df.lach = st_read('donnees/donnees_cartes/malbopictis_points_lacher.gpkg')

df.lach$lat = st_coordinates(df.lach)[,2]
df.lach$long = st_coordinates(df.lach)[,1]

df.lach_without_geom <- st_drop_geometry(df.lach)


##################
## Données PP
##################

PP_loc <- st_read("donnees/donnees_cartes/localisation_pp.gpkg")
PP_loc$Latitude = st_coordinates(PP_loc)[,2]
PP_loc$Longitude = st_coordinates(PP_loc)[,1]
PP_loc = st_drop_geometry(PP_loc)

df <- read_xlsx("donnees/suivi/BDD_TIS_PP_PRIVE_all.xlsx")

df <- df %>%
  filter(!is.na(date_releve), statut == "RAS") %>%
  mutate(date_instal=as.Date(date_instal), date_releve=as.Date(date_releve)) %>%
  rename(daterec = date_releve) %>%
  mutate(week = week(daterec), Mois_numeric = month(daterec), Year = year(daterec)) %>%
  mutate(effectif_jour = as.numeric(nb_oeufs)/as.numeric((daterec-date_instal))) %>%
  dplyr::select(daterec, week, Year, Mois_numeric, num_piege, effectif_jour) %>%
  mutate(num_piege = gsub("PP-MALMTP-","",  num_piege)) %>%
  left_join(PP_loc) %>%
  filter(!is.na(Latitude), !is.na(Longitude)) %>%
  mutate(zone = fct_relevel(zone, c("peripherie","centrale","tampon")))

##############################
## Données PP - taux fécondité
################################

# PP_loc <- st_read("donnees/donnees_cartes/localisation_pp.gpkg") %>%
#   mutate(Latitude = st_coordinates(.)[,2], Longitude = st_coordinates(.)[,1]) %>%
#   st_drop_geometry() %>%
#   dplyr::select(num_piege,zone, Latitude, Longitude) %>%
#   mutate(num_piege=ifelse(num_piege=="0014","014",num_piege))
# 
# df <- read_xlsx("donnees/suivi/BDD_TIS_PRIVE_011025.xlsx")
# 
# df <- df %>%
#   filter(!is.na(taux_fecond),!is.na(date_releve), statut == "RAS") %>%
#   mutate(date_instal=as.Date(as.numeric(date_instal), origin="1900-01-01"), date_releve=as.Date(as.numeric(date_releve),origin="1900-01-01")) %>%
#   mutate(date_instal = date_instal - 2, date_releve = date_releve - 2) %>%
#   rename(daterec = date_releve) %>%
#   mutate(week = week(daterec), Mois_numeric = month(daterec), Year = year(daterec)) %>%
#   mutate(effectif_jour = as.numeric(taux_fecond)*100) %>%
#   mutate(num_piege = gsub("PP-MALMTP-","",  num_piege)) %>%
#   left_join(PP_loc) %>%
#   mutate(zone = fct_relevel(zone, c("centrale","tampon","peripherie","autre"))) %>%
#   filter(!is.na(effectif_jour), !is.na(daterec))



### mapping
bbox_montpellier <- c(left = min(df$Longitude)-0.002, bottom = min(df$Latitude)-0.002, right = max(df$Longitude)+0.002, top = max(df$Latitude)+0.002)

ggmap::register_stadiamaps("46e02592-4347-4401-95bb-66873ec3a35b")

map_montpellier <- get_stadiamap(bbox_montpellier, maptype = "stamen_terrain", zoom = 16)


df_sf <- st_as_sf(df, crs = "OGC:CRS84", coords = c("Longitude", "Latitude"))

# # visualiser la localisation des pieges sur la carte
# traps_map <- ggmap(map_montpellier) +
#   coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
#   geom_sf(data = df_sf, aes(col=zone), size = 1, inherit.aes = FALSE)  +
#   theme_bw() +
#   ggtitle("Locations of the areas and traps") +
#   theme(axis.text.x=element_blank(),
#         axis.ticks.x=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank(),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank())
# 
# traps_map




# fonction pour créer les cartes 
fun_get_map <- function(df, temporal_grouping_column, map_type, legend_title,  col_to_plot = "effectif_jour_mn",hexagrid_cellsize = 150, hexagrid_trans = "sqrt"){
  
  # Define a function to fix the bbox to be in EPSG:3857
  ggmap_bbox <- function(map) {
    if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
    # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector,
    # and set the names to what sf::st_bbox expects:
    map_bbox <- setNames(unlist(attr(map, "bb")),
                         c("ymin", "xmin", "ymax", "xmax"))
    
    # Coonvert the bbox to an sf polygon, transform it to 3857,
    # and convert back to a bbox (convoluted, but it works)
    bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))
    
    # Overwrite the bbox of the ggmap object with the transformed coordinates
    attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
    attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
    attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
    attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
    map
  }
  
  # define theme
  my_theme <- function() {
    theme(
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.background = element_rect(fill = "transparent"), # necessary to avoid drawing panel outline
      panel.border = element_rect(colour = "grey", fill=NA, linewidth=1),
      panel.grid.major = element_blank(), # get rid of major grid
      panel.grid.minor = element_blank()
    )
  }
  
  
  if(map_type == "points"){
    
    df2 <- df %>%
      group_by(!!sym(temporal_grouping_column), num_piege, zone) %>%
      summarise(nb_rec = n(), effectif_jour_mn = mean(effectif_jour, na.rm = T), effectif_jour_sd = sd(effectif_jour, na.rm = T), effectif_jour_median = median(effectif_jour, na.rm = T), effectif_jour_max = max(effectif_jour, na.rm = T), effectif_jour_min = min(effectif_jour, na.rm = T), Latitude = mean(Latitude), Longitude = mean(Longitude))
      
    cbp1 <- c("#D55E00", "#E69F00", "#56B4E9", "#CC79A7")
    
    final_map <- ggmap(map_montpellier) +
      #{if(temporal_grouping_column=="Year")geom_point(data = df2,aes(x = Longitude, y = Latitude), color = "red", size = 0.02)}+
      geom_point(aes(x = Longitude, y = Latitude), data = df2, size = 0.5, color = "black") +
      geom_point(aes(x = Longitude, y = Latitude, size =  .data[[col_to_plot]], colour = zone), data = df2 %>% filter(effectif_jour_mn>0),  alpha = 0.7) +
      geom_point(data = df.lach_without_geom, aes(y = lat, x = long), color = "red", size = 0.7, shape=4) +
      scale_color_manual(values = cbp1) +
      scale_size_continuous(breaks = c(10,20,30,40,50,100), limits = c(0,100), range = c(1,7), name=legend_title, labels=c("1-10","10-20","20-30","30-40","40-50",">50")) +
      facet_wrap(temporal_grouping_column, nrow = 2) +
      #ggtitle(paste0("Aggregation by ", temporal_grouping_column)) +
      my_theme()
    #+ theme(legend.position="none")
    
    
  }
  
  if(map_type=="hexagrid"){
    
    df2 <- df %>%
      group_by(!!sym(temporal_grouping_column), num_piege, zone) %>%
      summarise(nb_rec = n(), effectif_jour_mn = mean(effectif_jour, na.rm = T), effectif_jour_sd = sd(effectif_jour, na.rm = T), effectif_jour_median = median(effectif_jour, na.rm = T), effectif_jour_max = max(effectif_jour, na.rm = T), effectif_jour_min = min(effectif_jour, na.rm = T), effectif_jour_sum = sum(effectif_jour, na.rm = T), Latitude = mean(Latitude), Longitude = mean(Longitude)) %>%
      st_as_sf(., crs = "OGC:CRS84", coords = c("Longitude", "Latitude")) %>%
      st_transform(3857) 
    df2$Longitude = st_coordinates(df2)[,1]
    df2$Latitude = st_coordinates(df2)[,2]
    
    # Use the function:
    map <- ggmap_bbox(map_montpellier)
    
    df_sf <- st_as_sf(df, crs = "OGC:CRS84", coords = c("Longitude", "Latitude")) %>%
      st_transform(3857)
    
    g = st_make_grid(df_sf , square=FALSE, cellsize = hexagrid_cellsize)
    nc2 = st_sf(geom=g)
    nc2$ID=1:length(g)
    
    a= nc2 %>%
      st_join(df_sf, join = st_intersects,left = TRUE) %>%
      filter(!is.na(Year)) %>%
      group_by(ID,!!sym(temporal_grouping_column)) %>%
      summarise(effectif_jour_mn = mean(effectif_jour, na.rm = T),effectif_jour_sd = sd(effectif_jour, na.rm = T), effectif_jour_median = median(effectif_jour, na.rm = T), effectif_jour_max = max(effectif_jour, na.rm = T),  effectif_jour_sum = sum(effectif_jour, na.rm = T), effectif_jour_min = min(effectif_jour, na.rm = T))
    
    df.lach_3857 <- st_transform(df.lach,3857)
    df.lach_3857$lat = st_coordinates(df.lach_3857)[,2]
    df.lach_3857$long = st_coordinates(df.lach_3857)[,1]
    df.lach_3857 <- st_drop_geometry(df.lach_3857)
    
    final_map <- ggmap(map) +
      coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
      geom_sf(data = a, aes(fill = .data[[col_to_plot]]), inherit.aes = FALSE,alpha = .7,) +
      scale_fill_gradient(low="lightyellow", high="red", trans = hexagrid_trans, name = legend_title) +
      geom_point(data = df.lach_3857, aes(y = lat, x = long), color = "red", size = 0.7, shape = 4) +
      #geom_point(aes(x = Longitude, y = Latitude, color = zone), data = df2, size = 0.3) +
      #{if(temporal_grouping_column=="Year")geom_sf(data = df_sf %>% st_transform(3857), color = "red", size = 0.01, inherit.aes = FALSE)} +
      facet_wrap(temporal_grouping_column, nrow = 2) +
      #ggtitle(paste0("Aggregation by ", temporal_grouping_column)) +
      my_theme()
    
    
  }
  
  return(final_map)
}


p1 <- fun_get_map(df, temporal_grouping_column = "daterec", map_type = "points", legend_title="nb eggs/day")       ## Pour l'argument temporal_grouping_column, possibilité de mettre "week", "Year", "Mois_numeric" ou toute autre colonne créée pour le besoin de l'aggrégation temporelle
p2 <- fun_get_map(df, temporal_grouping_column = "daterec", map_type = "hexagrid", legend_title="nb eggs/day")

p1 <- p1 + ggtitle("Malbopictis : nb oeuf/jours dans les 3 zones")
p2 <- p2 + ggtitle("Malbopictis : nb oeuf/jours dans les 3 zones")

ggsave(filename = 'plots_suivi/plot_oeufs_carte_point.png', plot = p1, width = 2000, height = 1000, unit ="px", dpi = 300, scale = 2)
ggsave(filename = 'plots_suivi/plot_oeufs_carte_grille.png', plot = p2, width = 2000, height = 1000, unit ="px", dpi = 300, scale = 2)



######################v
## Données BG
########################

BG_loc <- st_read("donnees/donnees_cartes/localisation_bg.gpkg") %>%
  st_drop_geometry() %>%
  dplyr::select(num_piege,zone) %>%
  rename(NumPP = num_piege)

df_BG <- read_xlsx("donnees/suivi/Malbopictis_BGS_BDD.xlsx")

df_BG_femelles <- df_BG %>%
  filter(!is.na(date_releve), statut == "RAS", date_instal!="NS", date_releve!="NS") %>%
  mutate(date_instal=as.Date(as.numeric(date_instal), origin="1900-01-01"), date_releve=as.Date(as.numeric(date_releve),origin="1900-01-01")) %>%
  mutate(date_instal = date_instal - 2, date_releve = date_releve - 2) %>%
  rename(daterec = date_releve, Latitude = Y, Longitude = X) %>%
  mutate(week = week(daterec), Mois_numeric = month(daterec), Year = year(daterec)) %>%
  mutate(effectif_jour = as.numeric(Femelles)/as.numeric((daterec-date_instal))) %>%
  mutate(Longitude=as.numeric(Longitude), Latitude=as.numeric(Latitude)) %>%
  dplyr::select(daterec, week, Year, Mois_numeric, num_piege, Latitude, Longitude, effectif_jour) %>%
  left_join(BG_loc, by = c("num_piege"="NumPP")) %>%
  mutate(zone = fct_relevel(zone, c("peripherie","centrale","tampon")))

df_BG_males <- df_BG %>%
  filter(!is.na(date_releve), statut == "RAS", date_instal!="NS", date_releve!="NS") %>%
  mutate(date_instal=as.Date(as.numeric(date_instal), origin="1900-01-01"), date_releve=as.Date(as.numeric(date_releve),origin="1900-01-01")) %>%
  mutate(date_instal = date_instal - 2, date_releve = date_releve - 2) %>%
  rename(daterec = date_releve, Latitude = Y, Longitude = X) %>%
  mutate(week = week(daterec), Mois_numeric = month(daterec), Year = year(daterec)) %>%
  mutate(effectif_jour = as.numeric(Males)/as.numeric((daterec-date_instal))) %>%
  mutate(Longitude=as.numeric(Longitude), Latitude=as.numeric(Latitude)) %>%
  dplyr::select(daterec, week, Year, Mois_numeric, num_piege, Latitude, Longitude, effectif_jour) %>%
  left_join(BG_loc, by = c("num_piege"="NumPP")) %>%
  mutate(zone = fct_relevel(zone, c("peripherie","centrale","tampon")))

df_BG_sex_ratio <- df_BG %>%
  filter(!is.na(date_releve), statut == "RAS", date_instal!="NS", date_releve!="NS") %>%
  mutate(date_instal=as.Date(as.numeric(date_instal), origin="1900-01-01"), date_releve=as.Date(as.numeric(date_releve),origin="1900-01-01")) %>%
  mutate(date_instal = date_instal - 2, date_releve = date_releve - 2) %>%
  rename(daterec = date_releve, Latitude = Y, Longitude = X) %>%
  mutate(week = week(daterec), Mois_numeric = month(daterec), Year = year(daterec)) %>%
  mutate(effectif_jour = (as.numeric(Males)/(as.numeric(Males)+as.numeric(Femelles))*100)) %>%
  mutate(Longitude=as.numeric(Longitude), Latitude=as.numeric(Latitude)) %>%
  dplyr::select(daterec, week, Year, Mois_numeric, num_piege, Latitude, Longitude, effectif_jour) %>%
  left_join(BG_loc, by = c("num_piege"="NumPP")) %>%
  mutate(zone = fct_relevel(zone, c("peripherie","centrale","tampon")))



p3 <- fun_get_map(df_BG_femelles, temporal_grouping_column = "daterec", map_type = "points", legend_title="nb adult females/day")
p4 <- fun_get_map(df_BG_males, temporal_grouping_column = "daterec", map_type = "points", legend_title="nb adult males/day")
p5 <- fun_get_map(df_BG_sex_ratio, temporal_grouping_column = "daterec", map_type = "points", legend_title="sex ratio % males")

p3 <- p3 +  ggtitle("Malbopictis : nb adultes femelles/jours dans les 3 zones")
p4 <- p4 +  ggtitle("Malbopictis : nb adultes mâles/jours dans les 3 zones")
p5 <- p5 +  ggtitle("Malbopictis : sex ratio (% males) dans les 3 zones")

ggsave(filename = 'plots_suivi/plot_adultes_carte_femelles.png', plot = p3, width = 2000, height = 1000, unit ="px", dpi = 300, scale = 2)
ggsave(filename = 'plots_suivi/plot_adultes_carte_males.png', plot = p4, width = 2000, height = 1000, unit ="px", dpi = 300, scale = 2)
ggsave(filename = 'plots_suivi/plot_adultes_carte_sexratio.png', plot = p5, width = 2000, height = 1000, unit ="px", dpi = 300, scale = 2)



## line plots

# p1 = ggplot(data = df_BG_femelles, aes(x = daterec, y = effectif_jour, color=zone)) +
#   geom_point() + 
#   geom_smooth(alpha = 0.1, se = T) +
#   theme_bw() + 
#   ggtitle("Nb femelles adultes /jour ~ temps") + 
#   ylim(c(0,100))


p1 <- df_BG_femelles %>%
  group_by(daterec,zone) %>%
  summarise(mean_adultes_femelles = mean(effectif_jour, na.rm = T), sd = sd(effectif_jour, na.rm = T)) %>% 
  ggplot(aes(x = daterec, y = mean_adultes_femelles, color=zone)) + 
  geom_line() + 
  geom_point() + 
  theme_bw()+ 
  ggtitle("Nb femelles adultes /jour ~ temps")


p2 <- df_BG_males %>%
  group_by(daterec,zone) %>%
  summarise(mean_adultes_femelles = mean(effectif_jour, na.rm = T), sd = sd(effectif_jour, na.rm = T)) %>% 
  ggplot(aes(x = daterec, y = mean_adultes_femelles, color=zone)) + 
  geom_line() + 
  geom_point() + 
  theme_bw()+ 
  ggtitle("Nb males adultes /jour ~ temps")

p3 <- df_BG_sex_ratio %>%
  group_by(daterec,zone) %>%
  summarise(mean_adultes_femelles = mean(effectif_jour, na.rm = T), sd = sd(effectif_jour, na.rm = T)) %>% 
  ggplot(aes(x = daterec, y = mean_adultes_femelles, color=zone)) + 
  geom_line() + 
  geom_point() + 
  theme_bw()+ 
  ggtitle("Sex ratio % males ~ temps")


p_x <- p1+p2+p3 + plot_layout(guides = "collect") & theme(legend.position = 'bottom')

ggsave(filename = 'plots_suivi/plot_adultes_resume.png', plot = p_x, width = 2000, height = 800, unit ="px", dpi = 300, scale = 2)


df_BG_males %>%
  filter(zone!="centrale") %>%
  ggplot(aes(x=reorder(num_piege,effectif_jour,na.rm = T), y = effectif_jour))  + 
  geom_boxplot() +
  ggtitle("abundance per trap") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + ylim(c(0,100))






### Points de lacher vs sterilité

df <- read_xlsx("donnees/suivi/BDD_TIS_PP_PRIVE_all.xlsx")


df2 <- df %>%
  filter(!is.na(date_releve), statut == "RAS", !is.na(taux_fecond)) %>%
  mutate(date_instal=as.Date(date_instal), date_releve=as.Date(date_releve)) %>%
  rename(daterec = date_releve) %>%
  mutate(week = week(daterec), Mois_numeric = month(daterec), Year = year(daterec)) %>%
  mutate(effectif_jour = 1-as.numeric(taux_fecond)) %>%
  dplyr::select(daterec, week, Year, Mois_numeric, num_piege, effectif_jour) %>%
  mutate(num_piege = gsub("PP-MALMTP-","",  num_piege)) %>%
  left_join(PP_loc) %>%
  filter(!is.na(Latitude), !is.na(Longitude)) %>%
  mutate(zone = fct_relevel(zone, c("peripherie","centrale","tampon"))) %>%
  filter(!is.na(effectif_jour), !is.na(daterec))

p2 <- fun_get_map(df2, temporal_grouping_column = "daterec", map_type = "hexagrid", legend_title="sterilité moyenne", hexagrid_cellsize = 350, col_to_plot = "effectif_jour_mn")
p2 <- p2 + ggtitle("Malbopictis : stérilité moyenne et points de lacher")
ggsave(filename = 'plots_suivi/plot_sterilite_moyenne_carte_grille.png', plot = p2, width = 2000, height = 1000, unit ="px", dpi = 300, scale = 2)

p2 <- fun_get_map(df2, temporal_grouping_column = "daterec", map_type = "hexagrid", legend_title="sterilité maximum", hexagrid_cellsize = 350, col_to_plot = "effectif_jour_max")
p2 <- p2 + ggtitle("Malbopictis : stérilité maximum et points de lacher")
ggsave(filename = 'plots_suivi/plot_sterilite_maximum_carte_grille.png', plot = p2, width = 2000, height = 1000, unit ="px", dpi = 300, scale = 2)

p2 <- fun_get_map(df2, temporal_grouping_column = "daterec", map_type = "hexagrid", legend_title="sterilité minimum", hexagrid_cellsize = 350, col_to_plot = "effectif_jour_min")
p2 <- p2 + ggtitle("Malbopictis : stérilité minimum et points de lacher")
ggsave(filename = 'plots_suivi/plot_sterilite_minimum_carte_grille.png', plot = p2, width = 2000, height = 1000, unit ="px", dpi = 300, scale = 2)

p2 <- fun_get_map(df2, temporal_grouping_column = "Year", map_type = "hexagrid", legend_title="sterilité moyenne", hexagrid_cellsize = 350, col_to_plot = "effectif_jour_mn")
p2 <- p2 + ggtitle("Malbopictis : stérilité moyenne et points de lacher")
ggsave(filename = 'plots_suivi/plot_sterilite_moyenne_saison_carte_grille.png', plot = p2, width = 2000, height = 1000, unit ="px", dpi = 300, scale = 2)



df2 <- df %>%
  filter(!is.na(date_releve), statut == "RAS", !is.na(taux_fecond)) %>%
  mutate(date_instal=as.Date(date_instal), date_releve=as.Date(date_releve)) %>%
  rename(daterec = date_releve) %>%
  mutate(week = week(daterec), Mois_numeric = month(daterec), Year = year(daterec)) %>%
  mutate(effectif_jour = as.numeric(nb_oeufs_compte)-as.numeric(nb_oeufs_fecond)) %>%
  dplyr::select(daterec, week, Year, Mois_numeric, num_piege, effectif_jour) %>%
  mutate(num_piege = gsub("PP-MALMTP-","",  num_piege)) %>%
  left_join(PP_loc) %>%
  filter(!is.na(Latitude), !is.na(Longitude)) %>%
  mutate(zone = fct_relevel(zone, c("peripherie","centrale","tampon"))) %>%
  filter(!is.na(effectif_jour), !is.na(daterec))

p2 <- fun_get_map(df2, temporal_grouping_column = "Year", map_type = "hexagrid", legend_title="Nb total oeufs steriles", hexagrid_cellsize = 350, col_to_plot = "effectif_jour_sum")
p2 <- p2 + ggtitle("Malbopictis : Nb total oeufs steriles et points de lacher")
ggsave(filename = 'plots_suivi/plot_sterilite_totale_saison_carte_grille.png', plot = p2, width = 2000, height = 1000, unit ="px", dpi = 300, scale = 2)

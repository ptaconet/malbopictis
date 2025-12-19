library(readxl)
library(tidyverse)
library(ggmap)
library(sf)
library(ggdensity)

PP_loc <- st_read("donnees/donnees_cartes/localisation_pp.gpkg") %>%
  st_drop_geometry() %>%
  dplyr::select(num_piege,zone) %>%
  rename(NumPP = num_piege)


df <- read_xlsx("/home/ptaconet/Téléchargements/BDD_TIS_PIEGES_.xlsx")


df <- df %>%
  filter(nom_commune == "MONTPELLIER") %>%
  filter(!is.na(date_releve), statut == "RAS") %>%
  mutate(date_instal=as.Date(date_instal), date_releve=as.Date(date_releve)) %>%
  rename(daterec = date_releve, Latitude = y, Longitude = x, Site = nom_commune, NumPP = num_piege) %>%
  mutate(week = week(daterec), Mois_numeric = month(daterec), Year = year(daterec)) %>%
  mutate(effectif_jour_PP = as.numeric(nb_oeufs)/as.numeric((daterec-date_instal))) %>%
  dplyr::select(daterec, week, Year, Mois_numeric, NumPP, Latitude, Longitude, Site, effectif_jour_PP) %>%
  mutate(NumPP = gsub("PP-MALMTP-","",  NumPP)) %>%
  left_join(PP_loc) %>%
  mutate(zone = fct_relevel(zone, c("centrale","tampon","peripherie","autre")))





## average by zone
lineplot <- df %>%
  group_by(daterec,zone) %>%
  summarise(mean_effectif_jour_PP=mean(effectif_jour_PP), sd_effectif_jour = sd(effectif_jour_PP)) %>%
  filter(!(zone=="autre")) %>%
  ggplot(aes(x=daterec, y = mean_effectif_jour_PP, group = zone, colour = zone)) + 
  geom_line() + 
  geom_point() + 
  ylim(0,30)

boxplot <- df %>%
  filter(!(zone=="autre")) %>%
  ggplot(aes(x=as.factor(daterec), y = effectif_jour_PP, fill = zone)) + 
  geom_boxplot()

lineplot + boxplot

## sans séparation des zones
lineplot <- df %>%
  group_by(daterec) %>%
  summarise(mean_effectif_jour_PP=mean(effectif_jour_PP), sd_effectif_jour = sd(effectif_jour_PP)) %>%
  ggplot(aes(x=daterec, y = mean_effectif_jour_PP)) + 
  geom_line() + 
  geom_point() + 
  ylim(0,30)

boxplot <- df %>%
  filter(!(zone=="autre")) %>%
  ggplot(aes(x=as.factor(daterec), y = effectif_jour_PP)) + 
  geom_boxplot()

lineplot + boxplot


### mapping
bbox_montpellier <- c(left = min(df$Longitude)-0.002, bottom = min(df$Latitude)-0.002, right = max(df$Longitude)+0.002, top = max(df$Latitude)+0.002)

ggmap::register_stadiamaps("46e02592-4347-4401-95bb-66873ec3a35b")

map_montpellier <- get_stadiamap(bbox_montpellier, maptype = "stamen_terrain", zoom = 14)


df_sf <- df %>%
  st_as_sf( crs = "OGC:CRS84", coords = c("Longitude", "Latitude"))

# function calculates angle with respect to polygon centroid.
# we need this to order the polygon correctly
calc_angle <- function(lon,lat) {
  cent_lon <- mean(lon)
  cent_lat <- mean(lat)
  ang <- atan2(lat - cent_lat, lon - cent_lon)
  
  return(ang)
}


bbox <-df %>%
  group_by(Site) %>%
  summarise(xmin = min(Longitude),ymin = min(Latitude), xmax=max(Longitude),  ymax = max(Latitude)) %>%
  gather(x,Longitude,c('xmin','xmax')) %>%
  gather(y,Latitude,c('ymin','ymax')) %>%
  st_as_sf(coords=c('Longitude','Latitude'),crs=4326,remove=F) %>%
  group_by(Site) %>%
  rename(lon = Longitude, lat = Latitude) %>%
  mutate(angle = calc_angle(lon,lat)) %>%
  arrange(angle) %>%
  summarise(do_union=FALSE) %>%
  st_cast('POLYGON')



cbp1 <- c("#D55E00", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

traps_map <- ggmap(map_montpellier) +
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = df_sf, aes(col=Site), size = 1, inherit.aes = FALSE)  +
  theme_bw() +
  scale_color_manual(values = cbp1) +
  ggtitle("Locations of the areas and traps") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())






fun_get_map <- function(df, site, temporal_grouping_column, map_type, spat_res = 1/2220){
  
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
  
  
  if(site == "BAYONNE"){
    
    map = map_bayonne
    df <- df %>% filter(Site == "BAYONNE")
    
  } else if (site == "PEROLS"){
    
    map = map_perols
    df <- df %>% filter(Site == "PEROLS")
    
  } else if (site == "SAINT-MEDARD-EN-JALLES"){
    
    map = map_stmedard
    df <- df %>% filter(Site == "SAINT-MEDARD-EN-JALLES")
    
  } else if (site == "MURVIEL-LES-MONTPELLIER"){
    
    map = map_murviel
    df <- df %>% filter(Site == "MURVIEL-LES-MONTPELLIER")
    
  } else if (site == "MONTPELLIER"){
    
    map = map_montpellier
    df <- df %>% filter(Site == "MONTPELLIER")
    
  } else if (site == "all"){
    map = map_allsites
  }
  
  
  if(map_type=="heatmap"){
    
    # df2 <- tidyr::uncount(df, effectif_jour_PP)
    
    df2 <- df %>%
      group_by(!!sym(temporal_grouping_column), NumPP) %>%
      summarise(nb_rec = n(), effectif_jour_PP_mn = trunc(mean(effectif_jour_PP, na.rm = T)), effectif_jour_PP_sd = sd(effectif_jour_PP, na.rm = T), Latitude = mean(Latitude), Longitude = mean(Longitude)) %>%
      tidyr::uncount(effectif_jour_PP_mn)
    
    
    final_map <- ggmap(map) +
      geom_hdr(aes(Longitude, Latitude, fill = after_stat(probs)), data = df2, alpha = .5, method = "kde") +
      geom_hdr_lines(aes(Longitude, Latitude), data = df2, method = "kde", linewidth = 0.2, show.legend=F) +
      scale_fill_brewer(name="Egg density", palette = "YlOrRd", labels=c("Lowest","","","Highest")) +
      {if(temporal_grouping_column=="Year")geom_point(data = df2,aes(x = Longitude, y = Latitude), color = "red", size = 0.02)}+
      facet_wrap(temporal_grouping_column, nrow = ifelse(temporal_grouping_column=="Mois",1,1)) +
      my_theme() +
      theme(legend.position="none") +
      ggtitle(paste0(site," - by ", temporal_grouping_column))
    
  }
  
  if(map_type=="idw"){
    
    df2 <- df %>%
      group_by(!!sym(temporal_grouping_column), NumPP) %>%
      summarise(nb_rec = n(), effectif_jour_PP_mn = mean(effectif_jour_PP, na.rm = T), effectif_jour_PP_sd = sd(effectif_jour_PP, na.rm = T), Latitude = mean(Latitude), Longitude = mean(Longitude))
    
    colnames(df2)[1] <- temporal_grouping_column
    
    unique_times <- unique(df[,temporal_grouping_column])[[1]]
    
    df_sf <- st_as_sf(df2, crs = "OGC:CRS84", coords = c("Longitude", "Latitude"))
    
    grd <- st_bbox(df_sf) %>%
      st_as_stars(dx = spat_res)
    
    grd2 <- st_simplify(st_buffer(st_convex_hull(st_union(st_geometry(df_sf))), dist = 15000), dTolerance = 5000)
    
    maps <- list()
    
    for(i in 1:length(unique_times)){
      
      df_sf2 <- df_sf %>% filter(!!sym(temporal_grouping_column) == unique_times[i])
      j <- gsta::idw(effectif_jour_PP_mn~1, df_sf2,grd)
      
      rext <- st_bbox(j)
      
      r <- raster(t(j[[1]]), xmn = rext[1], xmx = rext[3],
                  ymn = rext[2], ymx=rext[4],
                  crs = st_crs(i)$proj4string)
      
      r = mask(r, as(grd2, "Spatial"))
      
      r <-  as.data.frame(r, xy=TRUE) %>%
        filter(!is.na(layer))
      
      th_map<- ggmap(map) +
        geom_tile(data = r,  aes(x = x, y = y, fill = layer),  alpha = 0.8, size = 0.02) +
        scale_fill_gradient(low="yellow", high="red", limits = c(0,max(df2$effectif_jour_PP_mn))) +
        geom_point(data = df2,aes(x = Longitude, y = Latitude), color = "black", size = 0.05) +
        ggtitle(unique_times[i])
      
      maps[[i]] <- th_map
    }
    
    final_map <- wrap_plots(maps) +  plot_layout(nrow = 1, guides = "collect")
    
  }
  
  
  if(map_type=="rasterized"){
    
    df2 <- df %>%
      group_by(!!sym(temporal_grouping_column), NumPP) %>%
      summarise(nb_rec = n(), effectif_jour_PP_mn = mean(effectif_jour_PP, na.rm = T), effectif_jour_PP_sd = sd(effectif_jour_PP, na.rm = T), Latitude = mean(Latitude), Longitude = mean(Longitude))
    
    colnames(df2)[1] <- temporal_grouping_column
    
    unique_times <- unique(df[,temporal_grouping_column])[[1]]
    
    for(i in 1:length(unique_times)){
      
      df_th_year <- df2 %>% filter(!!sym(temporal_grouping_column) == unique_times[i])
      df_th_year_sp <- SpatialPointsDataFrame(df_th_year[,c("Longitude","Latitude")], df_th_year,proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
      
      r <- raster(ext = extent(df_th_year_sp), crs = crs(df_th_year_sp), res = spat_res)
      r <- extend(r, c(1,1))
      r <- raster::rasterize(df_th_year_sp, r, "effectif_jour_PP_mn", fun = mean)
      
      r <-  as.data.frame(r, xy=TRUE) %>%
        filter(!is.na(layer))
      
      th_map<- ggmap(map) +
        geom_tile(data = r,  aes(x = x, y = y, fill = layer),  alpha = 0.8, size = 0.02) +
        scale_fill_gradient(low="lightyellow", high="red", limits = c(0,max(df2$effectif_jour_PP_mn))) +
        geom_point(data = df2,aes(x = Longitude, y = Latitude), color = "red", size = 0.02) +
        ggtitle(unique_times[i])
      
      
      maps[[i]] <- th_map
      
    }
    
    final_map <- wrap_plots(maps) +  plot_layout(nrow = 1, guides = "collect")
    
  }
  
  if(map_type == "points"){
    
    df2 <- df %>%
      group_by(!!sym(temporal_grouping_column), NumPP) %>%
      summarise(nb_rec = n(), effectif_jour_PP_mn = mean(effectif_jour_PP, na.rm = T), effectif_jour_PP_sd = sd(effectif_jour_PP, na.rm = T), Latitude = mean(Latitude), Longitude = mean(Longitude))
    
    final_map <- ggmap(map) +
      #{if(temporal_grouping_column=="Year")geom_point(data = df2,aes(x = Longitude, y = Latitude), color = "red", size = 0.02)}+
      geom_point(aes(x = Longitude, y = Latitude), data = df2, size = 0.5, color = "black") +
      geom_point(aes(x = Longitude, y = Latitude, size = effectif_jour_PP_mn), data = df2 %>% filter(effectif_jour_PP_mn>0), colour = "darkred", alpha = 0.7) +
      scale_size_continuous(breaks = c(10,20,30,40,50,100), limits = c(0,100), range = c(1,10), name="Mean egg count / trap / day", labels=c("1-10","10-20","20-30","30-40","40-50",">50")) +
      facet_wrap(temporal_grouping_column, nrow = ifelse(temporal_grouping_column=="Mois",1,1)) +
      ggtitle(paste0(site," - by ", temporal_grouping_column)) +
      my_theme()
    #+ theme(legend.position="none")
    
    
  }
  
  if(map_type=="hexagrid"){
    
    # Use the function:
    map <- ggmap_bbox(map)
    
    df_sf <- st_as_sf(df, crs = "OGC:CRS84", coords = c("Longitude", "Latitude")) %>%
      st_transform(3857)
    
    g = st_make_grid(df_sf , square=FALSE, cellsize = 150)
    nc2 = st_sf(geom=g)
    nc2$ID=1:length(g)
    
    a= nc2 %>%
      st_join(df_sf, join = st_intersects,left = TRUE) %>%
      filter(!is.na(Year)) %>%
      group_by(ID,!!sym(temporal_grouping_column)) %>%
      summarise(effectif_jour_PP_mn = mean(effectif_jour_PP, na.rm = T))
    
    
    final_map <- ggmap(map) +
      coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
      geom_sf(data = a, aes(fill = effectif_jour_PP_mn), inherit.aes = FALSE,alpha = .7,) +
      scale_fill_gradient(low="lightyellow", high="red") + #, trans = "sqrt") +
      {if(temporal_grouping_column=="Year")geom_sf(data = df_sf %>% st_transform(3857), color = "red", size = 0.01, inherit.aes = FALSE)}+
      facet_wrap(temporal_grouping_column, nrow = ifelse(temporal_grouping_column=="Mois",2,1)) +
      ggtitle(paste0(site," - by ", temporal_grouping_column," - ", map_type)) +
      my_theme()
    
    
  }
  
  if (map_type=="hotspots_eachyear"){
    
    map <- ggmap_bbox(map)
    
    unique_times <- unique(df[,temporal_grouping_column])[[1]]
    
    if(temporal_grouping_column=="Mois"){
      unique_times <- c("janv","févr","mars","avril","mai","juin","juil","août","sept","oct","nov","déc")
    }
    if(temporal_grouping_column=="saison"){
      unique_times <- c("Winter","Summer")
    }
    
    df_sf <- st_as_sf(df, crs = "OGC:CRS84", coords = c("Longitude", "Latitude"))
    
    maps <- list()
    hotspots <- list()
    dfs_sf <- list()
    
    for(i in 1:length(unique_times)){
      
      df_th_year <- df %>%
        filter(!!sym(temporal_grouping_column) == unique_times[i]) %>%
        group_by(NumPP) %>%
        summarise(nb_rec = n(), effectif_jour_PP_mn = trunc(mean(effectif_jour_PP, na.rm = T)), effectif_jour_PP_sd = sd(effectif_jour_PP, na.rm = T), Latitude = mean(Latitude), Longitude = mean(Longitude))
      
      
      th_df_sf <- st_as_sf(df_th_year, crs = "OGC:CRS84", coords = c("Longitude", "Latitude")) %>%
        st_transform(32740)
      
      th_hotspot <- hotspot_gistar(th_df_sf, weights = effectif_jour_PP_mn, cell_size = 150, grid_type = "hex")
      
      th_hotspot <- th_hotspot %>%
        filter(gistar > 0)
      
      #th_hotspot <- ms_simplify(th_hotspot, keep = 0.1,keep_shapes = FALSE)
      
      
      hotspots[[i]] <- th_hotspot
      dfs_sf[[i]] <- th_df_sf
      
    }
    
    max_kde <- do.call("rbind", hotspots)
    max_kde <- max(max_kde$kde)
    
    for(i in 1:length(unique_times)){
      
      th_map <- ggmap(map) +
        coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
        geom_sf(data = hotspots[[i]] %>% st_transform(3857), mapping = aes(fill = kde), inherit.aes = FALSE, lwd = 0,alpha = .8) +
        scale_fill_gradient(low="lightyellow", high="red",limits = c(0,max_kde), name = "Egg density",  breaks=seq(0, max_kde, length.out = 4), labels = c("Lowest","","","Higest")) +
        {if(temporal_grouping_column=="Year")geom_sf(data = dfs_sf[[i]] %>% st_transform(3857), color = "red", size = 0.05, inherit.aes = FALSE)}+
        my_theme() +
        labs(subtitle = unique_times[i])
      
      
      maps[[i]] <- th_map
    }
    
    final_map <- wrap_plots(maps) +
      plot_layout(nrow = ifelse(temporal_grouping_column=="Mois",2,1), guides = "collect") +
      plot_annotation(title = paste0(site," - hotspots - ",temporal_grouping_column))
    
    
  }
  
  if(map_type == "hotspots_change"){
    
    map <- ggmap_bbox(map)
    
    # df_sf <- df %>%
    #   tidyr::uncount(effectif_jour_PP) %>%
    #   st_as_sf(crs = "OGC:CRS84", coords = c("Longitude", "Latitude")) %>%
    #   st_transform(32740)
    
    df_sf <- df %>%
      group_by(!!sym(temporal_grouping_column), NumPP) %>%
      summarise(nb_rec = n(), effectif_jour_PP_mn = trunc(mean(effectif_jour_PP, na.rm = T)), effectif_jour_PP_sd = sd(effectif_jour_PP, na.rm = T), Latitude = mean(Latitude), Longitude = mean(Longitude), daterec = mean(daterec)) %>%
      mutate(daterec = round(daterec,units="year")) %>%
      tidyr::uncount(effectif_jour_PP_mn) %>%
      st_as_sf(crs = "OGC:CRS84", coords = c("Longitude", "Latitude")) %>%
      st_transform(32740)
    
    
    cl = hotspot_classify(
      df_sf,
      cell_size = 150,
      time = "daterec",
      period = "1 year",
      grid_type = "rect"
    )
    
    cl <- cl %>%
      filter(hotspot_category!="no pattern") %>%
      st_transform(3857)
    
    final_map <- ggmap(map) +
      coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
      geom_sf(data = cl, aes(fill = hotspot_category), inherit.aes = FALSE) +
      my_theme() +
      geom_sf(data = st_as_sf(df, crs = "OGC:CRS84", coords = c("Longitude", "Latitude")) %>% st_transform(3857), color = "red", size = 0.1, inherit.aes = FALSE) +
      ggtitle(paste0(site, " - Evolution des hotspots entre ", min(df$Year), " et ",max(df$Year)))
    
  }
  
  return(final_map)
}


Sites <- data.frame(site = c("MONTPELLIER"))


pl1 <- Sites %>%
  mutate(pl_year_points = purrr::map(.$site,~fun_get_map(df,.,"daterec","points"))) 

pl2 <- Sites %>%
  mutate(pl_year_hexagrid = purrr::map(.$site,~fun_get_map(df,.,"daterec","hexagrid"))) 


library(patchwork)



pl1$pl_year_points[[1]] / pl2$pl_year_hexagrid[[1]]

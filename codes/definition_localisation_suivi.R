## cercles concentriques autour points de lacher
## carreaux de 2 ha
## % vegetation par carreau / cercle concentrique
## 5 pieges en zone lacher, 5 zone tampon, 

library(tidyverse)
library(sf)

malbosc <- st_read("donnees/donnees_cartes/malbopictis_malbosc.gpkg")

roi <- st_buffer(malbosc,1000)

parcelles <- st_read("donnees/donnees_environnementales/parcelles") %>%
  st_transform(2154) %>%
  st_crop(roi)

batiments <- st_read("donnees/donnees_environnementales/batiments_osm.geojson") %>%
  filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON")) %>%
  st_transform(2154) %>%
  st_crop(roi)

popfine <- st_read("donnees/donnees_environnementales/MMM_MMM_PopFine/MMM_MMM_PopFine.shp") %>%
  st_crop(roi)

vegetation <- read_sf("/vsizip//home/ptaconet/IDG_OMEES/data/mmm/MMM_MMM_VegFine.zip") %>%
  st_crop(roi)

parcs_jardins <-  st_read("donnees/donnees_environnementales/OSM_Metropole_parc_et_jardin/OSM_Metropole_parc_et_jardin.shp") %>%
  st_crop(roi)



## assigner pop à batiments

batiments_agg <- st_join(batiments, popfine, join = st_contains)

batiments_agg <- batiments_agg %>%
  group_by(geometry) %>%   # or use polygon ID if available
  summarise(
    total_pop = sum(pop_2021, na.rm = TRUE),
    n_points = n()
  )


## assigner pop à parcelles
parcelles_agg <- st_join(parcelles, popfine, join = st_contains)

parcelles_agg <- parcelles_agg %>%
  group_by(geometry) %>%   # or use polygon ID if available
  summarise(
    id = first(id),
    total_pop = sum(pop_2021, na.rm = TRUE),
    n_batiments = n(), 
  ) %>%
  mutate(surface = as.numeric(st_area(.)))



## function to calculate % vegetation

fun_calc_veg <- function(sf_object){

# 2. Calculate area of each parcel
sf_object$area_total <- st_area(sf_object)

# 3. Intersect parcels and vegetation
intersections <- st_intersection(sf_object, vegetation)

# 4. Calculate area of intersection
intersections$area_intersect <- st_area(intersections)

# 5. Summarize by parcel and vegetation type
veg_summary <- intersections %>%
  st_drop_geometry() %>%
  group_by(id, LIB_VEG_n) %>%  # replace with actual column names
  summarise(area_veg = sum(as.numeric(area_intersect)), .groups = "drop")

# 6. Join back total parcel area
veg_summary <- veg_summary %>%
  left_join(sf_object %>% st_drop_geometry() %>% select(id, area_total), by = "id") %>%
  mutate(
    veg_percent = area_veg / as.numeric(area_total) * 100
  )

veg_wide <- veg_summary %>%
  select(id, LIB_VEG_n, veg_percent) %>%
  tidyr::pivot_wider(names_from = LIB_VEG_n, values_from = veg_percent, values_fill = 0) %>%
  mutate(percentage_vegetation = rowSums(across(2:last_col()))) %>%
  janitor::clean_names(case = "snake")

return(veg_wide)

}



## percentage of vegetation for each parcel

veg_wide_parcelles <- fun_calc_veg(parcelles)

parcelles_info <- parcelles %>%
  left_join(st_drop_geometry(parcelles_agg)) %>%
  left_join(veg_wide_parcelles) 

parcelles_info <- parcelles_info %>%
  mutate(type_habitat = case_when((total_pop==0 | (total_pop>0 & total_pop <= 8 & surface >= 1000)) & percentage_vegetation < 80 ~ "inhabité (secondaire, tertiaire, voie pub.)",
                                  (total_pop==0 | (total_pop>0 & total_pop <= 8 & surface >= 1000)) & percentage_vegetation >= 80 ~ "inhabité (parc, végétation)",
                                  total_pop>0 & total_pop <= 8 & surface < 1000~ "individuel (pavillon)",
                                  total_pop>8 ~ "collectif (immeuble(s))"))

parcelles_info <- st_intersection(parcelles_info,roi)

st_write(parcelles_info,"donnees/donnees_cartes/malbopictis_parcelles_info.gpkg", append=FALSE )




## creer les polygones de suivi de 2 ha et calculer % vegetation dans chaque

# Create hex grid
hex_grid <- st_make_grid(
  roi,
  cellsize = 152,
  square = FALSE
)

# Convert to sf object
hex_sf <- st_sf(geometry = hex_grid)

# crop to aire de suivi
#hex_sf <- st_intersection(hex_sf,malbosc)

hex_sf$aire <- as.numeric(st_area(hex_sf))

hex_sf <- hex_sf %>%
  filter(aire>6000)

hex_sf$id <- seq(1,nrow(hex_sf))

# calcul vegetation
veg_hex <- fun_calc_veg(hex_sf)

hex_sf <- hex_sf %>%
  left_join(veg_hex) 

hex_sf <- st_intersection(hex_sf, roi)

st_write(hex_sf,"donnees/donnees_cartes/malbopictis_hexagones_suivi_2ha.gpkg", append = F)




## cercles concentriques autour de la zone protégée
# Centre point
zone_protegee <- st_read("donnees/donnees_cartes/malbopictis_zone_protegee.gpkg")

pt <- st_centroid(zone_protegee)

# Distances
distances <- seq(150, 1500, by = 150)

# Buffers
buffers <- lapply(distances, function(d) st_buffer(pt, dist = d))

# Create rings (annuli) by subtracting inner buffer from outer buffer
rings <- list()
for (i in seq_along(buffers)) {
  if (i == 1) {
    rings[[i]] <- buffers[[i]]
  } else {
    rings[[i]] <- st_difference(buffers[[i]], buffers[[i - 1]])
  }
}


geom_list <- lapply(rings, function(x) st_geometry(x)[[1]])

# Create the sf object
rings_sf <- st_sf(geometry = st_sfc(geom_list, crs = st_crs(pt)))


# Function to create a pie slice
create_slice <- function(center, radius, start_angle, end_angle, n_points = 100) {
  angles <- seq(start_angle, end_angle, length.out = n_points)
  x <- center[1] + radius * cos(angles)
  y <- center[2] + radius * sin(angles)
  coords <- rbind(center, cbind(x, y), center)  # close the polygon
  st_polygon(list(coords))
}

# Parameters
n_slices <- 10  # number of wedges
angle_step <- 2 * pi / n_slices
center <- as.numeric(st_coordinates(pt))

# Create slices
slices <- lapply(0:(n_slices - 1), function(i) {
  start_angle <- i * angle_step
  end_angle <- (i + 1) * angle_step
  st_sfc(create_slice(center, max(distances), start_angle, end_angle), crs = 2154)
})

geom_list <- lapply(slices, function(x) st_geometry(x)[[1]])

slices_sf <- st_sf(geometry = st_sfc(geom_list))


# Cross join and intersect
ring_id <- rep(1:length(rings), each = length(slices))
slice_id <- rep(1:length(slices), times = length(rings))
combo <- expand.grid(ring = 1:length(rings), slice = 1:length(slices))

wedges <- mapply(function(i, j) {
  st_intersection(rings[[i]], slices[[j]])
}, combo$ring, combo$slice, SIMPLIFY = FALSE)


geom_list <- lapply(wedges, function(x) st_geometry(x)[[1]])


wedges_sf <- st_sf(
  ring = ring_id,
  slice = slice_id,
  geometry = st_sfc(geom_list, crs = 2154)
)

# calcul vegetation

wedges_sf$id <- seq(1:nrow(wedges_sf))

veg_rings <- fun_calc_veg(wedges_sf)

wedges_sf <- veg_rings %>%
  left_join(wedges_sf) 

st_write(wedges_sf,"donnees/donnees_cartes/malbopictis_rings_150m.gpkg", append = F)


library(sf)
library(dplyr)
library(smoothr)

pp = st_read('donnees/donnees_cartes/localisation_pp.gpkg')

a=pp %>%
  group_by(zone) %>%
  summarise(geometry = st_convex_hull(st_union(geom))) %>%
  arrange(desc(st_area(.))) %>%
  mutate(geometry = smooth(geometry, method = "ksmooth", smoothness = .5)) %>%
  dplyr::filter(zone=="peripherie") %>%
  st_transform(2154)

## V2
zones = st_read('donnees/donnees_cartes/count_polygon_overlap.gpkg') %>%
  mutate(zone = case_when(aoeu %in% c(1,2) ~ "tampon", 
                          aoeu %in% c(3,4,5,6) ~ "centrale")) %>%
  group_by(zone) %>% 
  summarise(geometry = st_union(geom)) %>% 
  st_cast("MULTIPOLYGON") %>% 
  st_cast("POLYGON") %>%
  mutate(area = as.numeric(st_area(.))) %>%
  dplyr::filter(area>10000) %>%
  smoothr::fill_holes(3000) %>%
  mutate(geometry = smooth(geometry, method = "ksmooth", smoothness = 4)) %>%
  bind_rows(a) %>%
  arrange(desc(st_area(.)))

plot(zones)

st_write(zones,"donnees/donnees_cartes/zonage.gpkg", append = F, overwrite = F)

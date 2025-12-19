library(readxl)
library(tidyverse)
library(sf)

pp <- read_xlsx("donnees/suivi/BDD - MALBOPICTIS - BGS - R1.xlsx")

pp_loc <- pp %>%
  group_by(num_piege) %>%
  summarise(x=mean(as.numeric(x), na.rm  =T), y = mean(as.numeric(y), na.rm  = T))

pp_loc <- st_as_sf(pp_loc, coords = c("x","y"), crs = 4326)


points_lacher <- st_read("donnees/donnees_cartes/malbopictis_points_lacher.gpkg")


dist_pp_ptslacher <- st_distance(pp_loc,points_lacher)
dist_pp_ptslacher <- as.data.frame(dist_pp_ptslacher)
dist_pp_ptslacher <- dist_pp_ptslacher %>% mutate_all(as.numeric)
dist_pp_ptslacher$num_piege <- pp_loc$num_piege


# Étape 1 : sélectionner les colonnes de distances uniquement
distance_cols <- grep("^V[0-9]+$", names(dist_pp_ptslacher), value = TRUE)

# Étape 2 : calculer pour chaque piège :
# - nb de points < 50m
# - min distance au point le plus proche
dist_pp_ptslacher_class <- dist_pp_ptslacher %>%
  rowwise() %>%
  mutate(
    nb_points_150m = sum(c_across(all_of(distance_cols)) < 150, na.rm = TRUE),
    min_dist = min(c_across(all_of(distance_cols)), na.rm = TRUE)
  ) %>%
  ungroup()

# Étape 3 : classifier les pièges
dist_pp_ptslacher_class <- dist_pp_ptslacher_class %>%
  mutate(
    zone = case_when(
      nb_points_150m >= 4 ~ "centrale",
      nb_points_150m %in% 1:2 ~ "tampon",
      min_dist > 150 ~ "peripherie",   # si stricte >100m
      TRUE ~ "autre"
    )
  )

pp_ptslacher_class <- dist_pp_ptslacher_class %>%
  dplyr::select(num_piege, nb_points_150m, min_dist, zone) 

pp_loc <- left_join(pp_loc,pp_ptslacher_class)

st_write(pp_loc,"donnees/donnees_cartes/localisation_bg.gpkg", append=FALSE)





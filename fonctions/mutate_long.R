# 🌐 Fonction pour calculer la longueur d'une paroi via la diagonale de l'étendue 🌐
mutate.long <- function(polygon) {
  polygon_temp <- list()
  for(i in 1:nrow(polygon)){
    polygon %>% 
      slice(i) %>% 
      mutate(largeur = st_bbox(.)$xmax - st_bbox(.)$xmin, # Calcul la largeur de l'etendue de la paroi
             hauteur = st_bbox(.)$ymax - st_bbox(.)$ymin) %>% # Calcul la hauteur de l'etendue de la paroi
      mutate(long = sqrt(largeur^2+hauteur^2)) %>% # Calcule la diagonale du carre
      dplyr::select(-largeur, -hauteur) -> polygon_temp[[i]]
  }
  polygon_temp %>% 
    bind_rows -> polygon_long
  return(polygon_long)
}
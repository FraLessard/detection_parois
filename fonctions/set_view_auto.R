# 🌐 Fonction qui permet de zoomer sur le shapefile 🌐
set.view.auto <- function(map, 
                          shp) {
  # Extraction de l'etendue du shapefile
  bbox <- st_bbox(shp)
  
  # Calculer la largeur et la hauteur
  width <- bbox["xmax"] - bbox["xmin"]
  height <- bbox["ymax"] - bbox["ymin"]
  
  # Calculer la taille maximale
  size <- max(width, height)
  
  # Application de la fonction de régression log pour le niveau de zoom
  slope <- -1.436848  # Coefficient de régression
  intercept <- 8.347449  # Intercept
  
  # Calculer le niveau de zoom avec la fonction
  zoom_fct <- intercept + slope * log(size)
  
  # Limiter le niveau de zoom au valeur minimales et maximales de leaflet
  zoom_fct <- max(min(zoom_fct, 18), 2)
  
  setView(map,
          lng = mean(st_coordinates(shp)[, 1]),
          lat = mean(st_coordinates(shp)[, 2]),
          zoom = zoom_fct) -> zoomed_map
  
  return(zoomed_map)
}
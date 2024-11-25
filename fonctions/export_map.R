# 🌐 Fonction qui permet d'exporter une carte en TIFF géoréférencé 🌐
export.map <- function(carte_ggplot, 
                       bbox){
  # Exportation de la carte en PNG
  ggsave("./donnees_temporaires/carte_ggplot.png", 
         width = 10,
         height = 10,
         units = "in",
         plot = carte_ggplot, 
         device = "png")
  
  rast("./donnees_temporaires/carte_ggplot.png") -> carte_ggplot_raster
  
  # Convertir les pixels avec alpha 0 en NA (bande no.4)
  carte_ggplot_raster[carte_ggplot_raster[[4]] == 0] <- NA
  
  # Recadrer l'image pour enlever les zones transparentes
  carte_ggplot_raster %<>% 
    trim()
  
  # Mettre l'étendue dans le raster
  ext(carte_ggplot_raster) <- c(bbox$xmin, bbox$xmax, bbox$ymin, bbox$ymax)
  
  # Mettre le bon CRS dans le raster
  crs(carte_ggplot_raster) <- "EPSG:4326"
  
  # Faire pivoter en x et y la carte
  carte_ggplot_raster %<>% 
    flip(direction = c("vertical", "horizontal"))
  
  # Retirer la bande no 4 et seulement les trois première (RGB)
  carte_ggplot_raster[[1:3]] -> carte_ggplot_raster
  
  # Sauvegarder le résultat final sans les zones transparentes
  writeRaster(carte_ggplot_raster,
              datatype = "INT1U", # Format RGB
              filetype = "GTiff", # Format moins lourd que BIGTIFF
              paste0("./cartes/carte_georeferencee_",
                     length(list.files("./cartes")) + 1,
                     ".tiff"))
  
  # Retirer l'image temporaire sans géoréférencement
  file.remove("./donnees_temporaires/carte_temporaire.png")
}

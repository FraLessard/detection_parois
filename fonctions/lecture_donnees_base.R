# 🌐 Index LiDAR pour le telechargement et l'affichage 🌐
st_read("./donnees_base/URL_feuillet_ProDer.shp") %>% 
  st_transform(4326) %>% # Doit être en 4326 pour leaflet
  rowid_to_column("id") -> index

# 🌐 Donnees de filtre anthropique 🌐
st_read("./donnees_base/anthropique.shp") %>% 
  st_transform(4326) %>% 
  st_as_sf -> polygones_anthropiques
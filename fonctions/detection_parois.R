# 🌐 Fonction pour detecter les parois 🌐
detection.parois <- function(index, 
                             no_feuillet, 
                             session) {
  # Selection du feuillet
  index %>% 
    filter(feuillet == no_feuillet) -> index_select
  
  # Telechargement du mnt
  showNotification("Téléchargement du modèle numérique de terrain", type = "message", duration = NULL, session = session)
  downloader::download(url = paste0(index_select$lidar_url, "MNT_", index_select$feuillet, ".tif"), 
                       destfile = "./donnees_temporaires/mnt.tif", 
                       mode = "wb")
  
  rast("./donnees_temporaires/mnt.tif") -> mnt
  
  # Telechargement du mhc
  showNotification("Téléchargement du modèle de hauteur de canopée", type = "message", duration = NULL, session = session)
  downloader::download(url = paste0(index_select$lidar_url, "MHC_", index_select$feuillet, ".tif"), 
                       destfile = "./donnees_temporaires/mhc.tif", 
                       mode = "wb")
  
  rast("./donnees_temporaires/mhc.tif") -> mhc
  
  # Calcul du drop maximal parmis les 8 voisins (hauteur vertical de la paroi)
  showNotification("Calcul de la verticalité", type = "message", duration = NULL, session = session)
  mnt %>% 
    focal(w = 3, 
          fun = "min") -> min
  
  mnt-min -> drop
  
  # Extraction des polygones de parois
  showNotification("Extraction des parois et conversion en polygones", type = "message", duration = NULL, session = session)
  ifel(drop > 4 | (drop > 0.75 & mhc < 3), 1, NA) %>% # La premiere partie detecte les parois verticales (drop d'au moins 4 m) et l'autres les dalles (drop d'au moins 75 cm, mais une hauteur d'arbres de moins de 3 m)
    as.polygons() %>% # On converti le raster en polygone
    st_as_sf %>% # On converti en objet sf
    st_cast("POLYGON") %>% # Permet de convertir le polygone unique en polygones ne se touchant pas (parois uniques)
    mutate(area = st_area(.) %>% as.numeric()) %>% # Calculer la superficie
    mutate(maxd = exact_extract(drop, ., "max")) -> parois # Extracion du drop maximal
  
  # Pour filter les polygones par la surface et le drop max
  showNotification("Tri des parois trop petites", type = "message", duration = NULL, session = session)
  parois %<>% 
    filter(area > 25) %>% # On conserve toutes les parois de plus de 25 m carres en x/y
    filter(maxd > 3) %>% # On conserve tout ce qui possede un drop vertical de plus de 3 m
    filter(!(maxd < 4 & area < 250)) # On enleve tout ce qui possede un drop vertical de moins de 4 m et une superficie de moins de 250 m carre (On conserve les dalles)
  
  # Pour calculer les metriques
  showNotification("Calcul des métriques", type = "message", duration = NULL, session = session)
  exact_extract(mnt, parois, "max") -> max
  exact_extract(mnt, parois, "min") -> min
  parois %<>% 
    mutate(haut = max - min) %>% # Extraction de la difference entre l'elevation la plus basse et la plus haute que la paroi touche en x/y (le proxy le plus facile de la hauteur des voies)
    mutate(s4m = exact_extract(ifel(drop >= 4, 1, 0), parois, "sum")) %>% # Extraction du nombre de m carre avec un drop de plus de 4 m (vertical, pour calculer la proportion de dalle)
    mutate(vert = s4m/area*100) %>% # Extraction de la proportion de vertical dans la paroi en superficie x/y
    mutate(s10m = exact_extract(ifel(drop >= 10, 1, 0), parois, "sum")) %>% # Extraction du nombre de m carre avec un drop de plus de 10 m
    mutate(s20m = exact_extract(ifel(drop >= 20, 1, 0), parois, "sum")) %>% # Extraction du nombre de m carre avec un drop de plus de 20 m
    mutate(s30m = exact_extract(ifel(drop >= 30, 1, 0), parois, "sum")) %>%  # Extraction du nombre de m carre avec un drop de plus de 30 m
    mutate.long() # Pour extraire la longueur
    
  # Ecriture du jeu de donnees
  parois %>% 
    st_write(paste0("./parois/parois_", index_select$feuillet, ".shp"))
  
  showNotification(paste0("Détection des parois terminé pour le feuillet ", index_select$feuillet), type = "message", duration = NULL, session = session)
  
  # Supression des rasters temporaires
  list.files("./donnees_temporaires" , full.names = T) %>% 
    file.remove()
}
# 🌐 Fonction pour lire les parois detectees avec certains paramètres 🌐
lecture.parois <- function(variable_symbologie,
                           long_seuil,
                           maxd_seuil,
                           haut_seuil,
                           vert_seuil,
                           s10m_seuil,
                           s20m_seuil,
                           s30m_seuil,
                           cercle_filtre){
  
  variable_symbologie <- sym(variable_symbologie) # Pour mettre la variable de la symbologie en symbole R

  list.files("./parois", pattern = "\\.shp$", full.names = TRUE) %>% # Lectures des parois detectees
    map(st_read, quiet = TRUE) %>% # Lecture des shapefiles
    map(st_transform, 4326) %>% # Changement du systeme de coordonnes pour le lire dans leaflet
    bind_rows() %>% # Combinaison des fichiers dans un objet
    st_filter(cercle_filtre) %>% # selection des parois seulement a la distance voulue
    filter(long >= long_seuil) %>% 
    filter(maxd >= maxd_seuil) %>% 
    filter(haut >= haut_seuil) %>% 
    filter(vert >= vert_seuil) %>% 
    filter(s10m >= s10m_seuil) %>% 
    filter(s20m >= s20m_seuil) %>% 
    filter(s30m >= s30m_seuil) %>% 
    mutate(across(where(is.numeric), round)) %>% # On arrondis tous les champs numeriques sans decimales
    mutate(couleur = case_when(!!variable_symbologie < pull(., !!variable_symbologie) %>% quantile(0.50) ~ "#FF0000",
                               !!variable_symbologie < pull(., !!variable_symbologie) %>% quantile(0.65) ~ "#FF6D00",
                               !!variable_symbologie < pull(., !!variable_symbologie) %>% quantile(0.75) ~ "#FFDA00",
                               !!variable_symbologie < pull(., !!variable_symbologie) %>% quantile(0.85) ~ "#B6FF00",
                               !!variable_symbologie < pull(., !!variable_symbologie) %>% quantile(0.95) ~ "#48FF00",
                               !!variable_symbologie < pull(., !!variable_symbologie) %>% quantile(0.99) ~ "#009B62",
                               !!variable_symbologie < pull(., !!variable_symbologie) %>% quantile(0.999) ~ "#006D91",
                               !!variable_symbologie >= pull(., !!variable_symbologie) %>% quantile(0.999) ~ "#0000FF")) %>% # Pour avoir une couleur sur la carte selon la variable choisie
    group_by(couleur) %>% 
    mutate(label = paste0(min(!!variable_symbologie) %>% round(0),
                          " - ",
                          max(!!variable_symbologie) %>% round(0)) %>% as.character()) %>%  # Pour avoir une étiquette selon la variable choisie
    mutate(ordre = mean(!!variable_symbologie) %>% round(0)) %>% # Pour avoir un ordre du plus grand au plus petit pour trier la légende
    ungroup() %>% 
    mutate(ordre = factor(ordre, levels = sort(unique(ordre)))) %>% 
    rowid_to_column("id") -> parois # id unique de la paroi pour la retrouver dans la table
  
  return(parois)
}
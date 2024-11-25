# 🌐 Fonction qui permet de filtrer les parois pour chaque type d'affectation anthropique sélectionné 🌐
tri.anthropique <- function(parois, 
                            polygones_anthropiques,
                            liste_tri){
  if(!is.null(liste_tri)){
    # Permet de conserver uniquement les types d'affectation anthropique selectionne
    polygones_anthropiques %>% 
      filter(type %in% liste_tri) -> polygones_anthropiques_tri
    
    # Permet de trouve quel id sont des parois dans les types d'affectation anthropique selectionne
    parois %>% 
      st_filter(polygones_anthropiques_tri) %>% 
      pull(id) -> anthropique_id
    
    # On retire ces id
    parois %>% 
      filter(!(id %in% anthropique_id)) -> parois_select
    
    return(parois_select)
  } else {
    return(parois)
  }
}
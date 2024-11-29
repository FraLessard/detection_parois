# 🌈🌈🌈🌈🌈🌈🌈🌈🌈🌈🌈🌈🌈🌈🌈🌈🌈🌈🌈🌈🌈🌈🌈🌈🌈🌈🌈🌈🌈🌈🌈🌈🌈🌈🌈🌈🌈🌈🌈
# Crée par Francis Lessard

# Crée le 2024-11-02
# Mis à jour le 2024-11-16

# Ce script permet de :
# - Détecter les parois partout au Québec avec des données LiDAR
# - Les valeurs suivantes sont extraites pour toutes les parois :
#                           1. Longueur (m)
#                           2. Verticalité maximale (m)
#                           3. Hauteur (m)
#                           4. Proportion de la paroi étant verticale (%)
#                           5. Superficie en verticalité de 10 m et + (m²)
#                           6. Superficie en verticalité de 20 m et + (m²)
#                           7. Superficie en verticalité de 30 m et + (m²)
# - Permet un affichage dynamique des parois selon des paramètres désirés de l'utilisateur
# - Ajuste la symbologie (couleur) des parois selon ce qui est lu en entrée
# - Permet d'exporter des cartes TIFF géoréférencés (à lire avec Avenza Maps) pour l'exploration
#
# Marche à suivre pour l'utilisation :
# 1. Installer les pakages nécessaires (à faire une seule fois) en faisant rouler
# les lignes de code (enlever le "#" devant avant) et une fois es packages intallées,
# remettre le "#". Pour rouler une ligne de code, il faut la/les sélectionner et
# peser sur "ctrl" + "enter" en même temps. Il est aussi possible d'installer 
# les packages avec le bouton "Install" du panneau inférieur droite, onglet 
# "Packages". Un package deviendra une Library, c'est la même affaire.
# 2. Cliquer sur "Run App" en haut à droite du panneau supérieur gauche.
# 3. Have fun !
# 🌈🌈🌈🌈🌈🌈🌈🌈🌈🌈🌈🌈🌈🌈🌈🌈🌈🌈🌈🌈🌈🌈🌈🌈🌈🌈🌈🌈🌈🌈🌈🌈🌈🌈🌈🌈🌈🌈🌈

# 1. 🟡🟡 Installer les packages nécessaires (à faire une seule fois) 🟡🟡 ----
# install.packages("shiny")
# install.packages("shinyjs")
# install.packages("shinyBS")
# install.packages("tidyverse")
# install.packages("magrittr")
# install.packages("sf")
# install.packages("terra")
# install.packages("exactextractr")
# install.packages("leaflet")
# install.packages("leaflet.extras")
# install.packages("downloader")
# install.packages("devtools")
# install.packages("httr")
# devtools::install_github('Chrisjb/basemapR')






# 2. 🟡🟡 Charger les librairies nécessaires 🟡🟡 ----
library(shiny)
library(shinyjs)
library(shinyBS)
library(tidyverse)
library(magrittr)
library(sf)
library(terra)
library(exactextractr)
library(leaflet)
library(leaflet.extras)
library(downloader)
library(httr)
library(basemapR)






# 3. 🟡🟡 Parametres 🟡🟡 ----
# Repertoire de travail
rstudioapi::getSourceEditorContext()$path %>%
  dirname() %>% 
  setwd()

# Pour mettre un temps assez long de téléchargement avant qu'il arrête
options(timeout=10^10) 





# 4. 🟡🟡 Lecture des fonctions 🟡🟡 ----
rstudioapi::getSourceEditorContext()$path %>%  
  dirname %>% 
  paste0(., "/fonctions") %>% 
  list.files(full.names = TRUE) %>% 
  map(source)





# 5. 🟡🟡 Application Shiny 🟡🟡 ----
ui <- fluidPage(
  useShinyjs(),  # pour gérer l'interactivité
  titlePanel("Application de détection de parois"),
  # Ajouter le CSS pour rendre la carte fixe
  tags$style(HTML("
    .sidebar-panel {
      height: 100vh;  /* Assurez-vous que la barre latérale occupe toute la hauteur de l'écran */
      overflow-y: auto; /* Active le défilement vertical */
  ")),
  sidebarLayout(
    
    # 🔵 Barre latérale 🔵
    sidebarPanel(
      bsCollapse(
        open = "Détection de nouvelles parois",
        bsCollapsePanel("Paramètres de lecture et d'affichage des parois",
                        div(class = "sidebar-panel",
                            # Paramètres
                            numericInput("latitude", "Latitude", 46.78831),
                            numericInput("longitude", "Longitude", -71.31259),
                            sliderInput("distance", "Zone d'Analyse (km)", 
                                        min = 0, max = 200, value = 25),
                            checkboxGroupInput("anthropique", 
                                               "Retirer les affectations anthropiques", 
                                               choices = c("Carrière" = "carriere", 
                                                           "Route" = "route", 
                                                           "Zone urbanisée" = "urbain"), 
                                               selected = c("carriere", "route", "urbain")),
                            radioButtons("variable_symbologie", "Variable de Symbologie", 
                                         choices = c("Longueur (m)" = "long",
                                                     "Verticalité maximale (m)" = "maxd",
                                                     "Hauteur (m)" = "haut",
                                                     "Proportion de la paroi étant verticale (%)" = "vert",
                                                     "Superficie en verticalité de 10 m et + (m²)" = "s10m",
                                                     "Superficie en verticalité de 20 m et + (m²)" = "s20m",
                                                     "Superficie en verticalité de 30 m et + (m²)" = "s30m"),
                                         selected = "maxd"),
                            sliderInput("long_seuil", "Longueur (m)", 
                                        min = 0, max = 1000, value = 20),
                            sliderInput("maxd_seuil", "Verticalité maximale (m)", 
                                        min = 0, max = 100, value = 8),
                            sliderInput("haut_seuil", "Hauteur (m)", 
                                        min = 0, max = 200, value = 15),
                            sliderInput("vert_seuil", "Proportion de la paroi étant verticale (%)", 
                                        min = 0, max = 100, value = 0),
                            sliderInput("s10m_seuil", "Superficie en verticalité de 10 m et + (m²)", 
                                        min = 0, max = 500, value = 0),
                            sliderInput("s20m_seuil", "Superficie en verticalité de 20 m et + (m²)", 
                                        min = 0, max = 250, value = 0),
                            sliderInput("s30m_seuil", "Superficie en verticalité de 30 m et + (m²)", 
                                        min = 0, max = 100, value = 0))),
        bsCollapsePanel("Détection de nouvelles parois",
                        div(class = "sidebar-panel",
                            # Paramètres
                            textInput("feuillet_selection", "Feuillet à traiter", "21M04NO"),
                            actionButton("btn_detection", "Détecter/télécharger les parois dans le feuillet")))       
      )
    ),
    
    # 🔵🔵🔵🔵🔵🔵🔵🔵🔵🔵🔵🔵
    
    # 🔵 Interface principale 🔵
    mainPanel(
      # Ajouter une ligne pour mettre les boutons sur la même ligne
      fluidRow(
        # Bouton pour afficher la carte
        column(4, actionButton("btn_affichage", "Afficher les parois")),
        # Bouton pour exporter la carte en TIFF géoréférence
        column(4, actionButton("export_map", "Exporter en TIFF géoréférencé"))
      ),
      # Utilisation de fluidRow et column pour ajuster la carte
      fluidRow(
        column(12,
               leafletOutput("map", height = "600px")  # Carte de taille fixe
        )
      )
    )
    
    # 🔵🔵🔵🔵🔵🔵🔵🔵🔵🔵🔵🔵
    
  )
)

# Serveur
server <- function(input, output, session) {
  
  # 🟢 = Blocs de création de la carte
  # 🟠 = Blocs d'événement réactifs
  # 🟣 = Blocs de action actionButton
  
  # 🟢 Initialsation de la carte 🟢
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addProviderTiles(providers$Esri.WorldImagery) %>%
      addPolygons(data = index,
                  color = "black",
                  weight = 2,
                  fillOpacity = 0,
                  layerId = ~feuillet,
                  popup = ~as.character(feuillet)) %>%
      addPolygons(data = index %>% filter(feuillet %in% c(feuillets_detectes_github %>% pull(feuillet) %>% unique())),
                  color = "purple",
                  weight = 4,
                  fillOpacity = 0,
                  group = "feuillets_detectes_github",
                  options = pathOptions(interactive = FALSE))
  })
  
  # 🟢🟢🟢🟢🟢🟢🟢🟢🟢🟢🟢🟢
  
  # 🟢 Mise a jour de la carte avec le point et le cercle de recherche 🟢
  observe({
    req(cercle_reactive())
    leafletProxy("map") %>%
      clearGroup("recherche") %>% 
      addCircleMarkers(lng = input$longitude,
                       lat = input$latitude,
                       group = "recherche",
                       color = "turquoise",
                       radius = 0.5,
                       weight = 0.5,
                       fillOpacity = 1) %>%
      addPolygons(data = cercle_reactive(),
                  color = "turquoise",
                  weight = 2,
                  fillOpacity = 0,
                  group = "recherche",
                  options = pathOptions(interactive = FALSE))
  })
  
  # 🟢🟢🟢🟢🟢🟢🟢🟢🟢🟢🟢🟢
  
  # 🟢 Mise a jour de la carte avec les feuillets deja detectes 🟢
  observe({
    if(is.null(feuillets_detectes_reactive())){
      leafletProxy("map") %>%
        clearGroup("feuillets_detectes")
    } else {
      leafletProxy("map") %>%
        clearGroup("feuillets_detectes") %>% 
        addPolygons(data = index %>% filter(feuillet %in% feuillets_detectes_reactive()),
                    color = "lightgreen",
                    weight = 2,
                    fillOpacity = 0,
                    group = "feuillets_detectes",
                    options = pathOptions(interactive = FALSE))
    }
  })
  
  # 🟢🟢🟢🟢🟢🟢🟢🟢🟢🟢🟢🟢
  
  # 🟢 Ajout des contrôles des feuillets pour assurer l'ordre d'affichage 🟢
  observe({
    leafletProxy("map") %>%
      addLayersControl(overlayGroups = c("feuillets_detectes",
                                         "feuillets_detectes_github" ),  # Liste les groupes dans l'ordre
                       options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  # 🟢🟢🟢🟢🟢🟢🟢🟢🟢🟢🟢🟢
  
  # 🟢 Mise a jour de la carte avec les parois 🟢
  observe({
    # On fait rouler le bloc de code seulement s'ily a des parois de détectées
    req(parois_reactive())
    leafletProxy("map") %>%
      clearGroup("parois") %>% 
      clearControls() %>% 
      set.view.auto(parois_reactive()) %>% 
      addPolygons(data = parois_reactive(),
                  fillColor = ~couleur,
                  fillOpacity = 0.25,
                  color = ~couleur,
                  weight = 2,
                  opacity = 1,
                  group = "parois",
                  popup = ~paste("Longueur:", long, " m", "<br>",
                                 "Verticalité maximale:", maxd, " m", "<br>",
                                 "Hauteur:", haut, " m", "<br>",
                                 "Proportion de la paroi étant verticale:", vert, " %", "<br>",
                                 "Proportion de la paroi en dalle:", 100-vert, " %", "<br>",
                                 "Superficie en verticalité de 10 m et +:", s10m, " m²", "<br>",
                                 "Superficie en verticalité de 20 m et +:", s20m, " m²", "<br>",
                                 "Superficie en verticalité de 30 m et +:", s30m, " m²")) %>%
      addLegend("bottomright",
        colors = parois_reactive() %>% arrange(ordre) %>% pull(couleur) %>% unique,  # Utilise directement les codes de couleur
        labels = parois_reactive() %>% arrange(ordre) %>% pull(label) %>% unique,  # Affiche les codes de couleur comme labels
        title = "Légende",
        opacity = 1)
  })
  
  # 🟢🟢🟢🟢🟢🟢🟢🟢🟢🟢🟢🟢
  
  # 🟠 Observer les coordonnées du clic sur la carte pour centrer l'affichage des parois 🟠
  observeEvent(input$map_click, {
    click <- input$map_click
    updateNumericInput(session, "latitude", value = click$lat)
    updateNumericInput(session, "longitude", value = click$lng)
  })
  
  # 🟠🟠🟠🟠🟠🟠🟠🟠🟠🟠🟠🟠
  
  # 🟠 Observer un changement dans la position et le rayon de recherche pour afficher la zone de lecture sur la carte 🟠
  cercle_reactive <- reactive({
    st_sfc(st_point(c(input$longitude, input$latitude)), crs = 4326) %>%
      st_buffer(input$distance*1000) -> cercle
    
    cercle
  })
  
  # 🟠🟠🟠🟠🟠🟠🟠🟠🟠🟠🟠🟠
  
  # 🟠 Observer les clics sur les polygones pour mettre à jour le feuillet où les parois seront détectées 🟠
  observeEvent(input$map_shape_click, {
    clicked_feuillet <- input$map_shape_click$id
    # Mettre à jour la sélection du feuillet dans l'interface
    updateTextInput(session, "feuillet_selection", value = clicked_feuillet)
  })  
  
  # 🟠🟠🟠🟠🟠🟠🟠🟠🟠🟠🟠🟠
  
  # 🟠 Faire une liste des feuillets deja detectes 🟠
  feuillets_detectes_reactive <- reactiveFileReader(
    intervalMillis = 2000,  # Vérifie les changements toutes les 2 seconde
    session = NULL,         # Session actuelle (optionnel, peut être laissé NULL)
    filePath = "./parois",  # Répertoire à surveiller
    readFunc = function(path) {
      list.files(path, pattern = "\\.shp$") %>% 
        gsub(".shp", "", .) %>% 
        gsub("parois_", "", .)
    }
  )
  
  # 🟠🟠🟠🟠🟠🟠🟠🟠🟠🟠🟠🟠
  
  # 🟣 Lire les parois avec en fonction des paramètres de l'utilisateur 🟣
  # PROB, SI ON LIT A UN ENDROIT OÙ IL N'Y A PAS DE PAROIS DETECTÉES, ÇA BUG
  parois_reactive <- eventReactive(input$btn_affichage, {
    # Lecture des parois deja detectees
    if(list.files("./parois") %>% length() != 0){
      showNotification("Lecture des parois déjà detectées", type = "message", duration = 15, session = session)
      # Lecture des parois
      lecture.parois(variable_symbologie = input$variable_symbologie,
                     long_seuil = input$long_seuil,
                     maxd_seuil = input$maxd_seuil,
                     haut_seuil = input$haut_seuil,
                     vert_seuil = input$vert_seuil,
                     s10m_seuil = input$s10m_seuil,
                     s20m_seuil = input$s20m_seuil,
                     s30m_seuil = input$s30m_seuil,
                     index = index,
                     cercle_filtre = cercle_reactive()) %>% 
        tri.anthropique(polygones_anthropiques = polygones_anthropiques, 
                        liste_tri = input$anthropique) -> parois # Pour trier les affectation anthropiques
      # Pour retourner les parois
      parois
    } else {
      showNotification("Il n'y a aucune paroi de detectée pour l'instant", type = "message", duration = 15, session = session)
      # Pour retourner une valeur nulle
      NULL
    }
  })
    
  # 🟣🟣🟣🟣🟣🟣🟣🟣🟣🟣🟣🟣
  
  # 🟣 Détecter les parois dans un nouveau secteur 🟣
  observeEvent(input$btn_detection, {
    if(any(feuillets_detectes_reactive() == input$feuillet_selection)){
      showNotification("Les parois ont déjà été détectées pour ce feuillet", type = "error", duration = 15, session = session)
    } else if(any(feuillets_detectes_github %>% pull(feuillet) %>% unique() == input$feuillet_selection)){
      feuillets_detectes_github %>% 
        filter(feuillet == input$feuillet_selection) -> feuillets_detectes_github_a_telecharger
      
      map2(feuillets_detectes_github_a_telecharger$download_url,
           feuillets_detectes_github_a_telecharger$dest,
           ~ download.file(url = .x,
                           destfile = .y,
                           mode = "wb",
                           method = "curl"))
      
      showNotification(paste0("Feuillet téléchargé depuis github pour le feuillet ", input$feuillet_selection), type = "message", duration = 60, session = session)
    } else {
      detection.parois(index = index,
                       no_feuillet = input$feuillet_selection, 
                       session = shiny::getDefaultReactiveDomain())
      showNotification(paste0("Détection des parois terminé pour le feuillet ", input$feuillet_selection), type = "message", duration = 60, session = session)
      
    }
  })
  
  # 🟣🟣🟣🟣🟣🟣🟣🟣🟣🟣🟣🟣
  
  # 🟣 Exporter la carte en TIFF géoréférencé 🟣 
  observeEvent(input$export_map, {
    showNotification("Exportation de la carte en tif géoréférencé", type = "message", duration = 15, session = session)
    # Limites de la carte visible
    bounds <- input$map_bounds
    # Création de l'emprise en tant qu'objet sf
    bbox <- st_bbox(c(xmin = bounds$west, xmax = bounds$east,
                       ymin = bounds$south, ymax = bounds$north),
                     crs = st_crs(4326))
    # Création de la carte avec ggplot, sans titre et avec les parois
    ggplot() +
      base_map(bbox, basemap = "google-satellite", increase_zoom = 3, nolabels = T) +
      geom_sf(data = parois_reactive(), color = parois_reactive()$couleur, fill = parois_reactive()$couleur) + # Ajoute les parois
      coord_sf(xlim = c(bounds$west, bounds$east), # Limites de la carte
               ylim = c(bounds$south, bounds$north), 
               expand = FALSE) +
      theme_void() + # Supprime les axes, étiquettes et grilles
      theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) -> carte_ggplot
    
    # Pour exporter la carte en tiff géoréférencé
    carte_ggplot %>% 
      export.map(bbox)
    
    # Notification d'export
    showNotification("Carte exportée en tif géoréférencé", type = "message", duration = 15, session = session)
  })
  # 🟣🟣🟣🟣🟣🟣🟣🟣🟣🟣🟣🟣
}

# Exécuter l'application
shinyApp(ui = ui, server = server)
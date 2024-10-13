# Chargement des librairies
install.packages('sf')
library(shiny)
library(shinydashboard)
library(shinythemes)
library(leaflet)
library(ggplot2)
library(plotly)
library(DT)
library(shinyjs)
library(httr)
library(jsonlite)
library(shinymanager)
library(webshot)
library(sf)

# Création des utilisateurs pour l'authentification
credentials <- data.frame(
  user = c("admin", "user"),
  password = c("adminpass", "userpass"),
  stringsAsFactors = FALSE
)

# Fonction pour récupérer les données à partir de l'API
get_logements_data <- function() {
  
  # Création d'un vecteur avec les codes postaux du Rhône
  codes_postaux_69 <- c(
    69001, 69002, 69003, 69004, 69005, 69006, 69007, 69008, 69009,
    69100, 69110, 69120, 69140, 69160, 69170, 69190, 69200,
    69210, 69220, 69230, 69240, 69260, 69310, 69320, 69330,
    69340, 69350, 69360, 69370, 69380, 69390, 69400, 69410,
    69420, 69430, 69440, 69450, 69460, 69500, 69510, 69520,
    69530, 69540, 69560, 69570, 69580, 69590, 69600, 69610,
    69620, 69630, 69640, 69650, 69660, 69670, 69680, 69690
  )
  
  logement_existant <- data.frame()
  logement_neuf <- data.frame()
  
  # Boucle pour chaque code postal (logements existants)
  for(valeur in codes_postaux_69){
    base_url <- "https://data.ademe.fr/data-fair/api/v1/datasets/dpe-v2-logements-existants/lines"
    params <- list(
      page = 1,
      size = 10000,
      select = "N°DPE,Code_postal_(BAN),Etiquette_DPE,Date_réception_DPE,Type_bâtiment,Coordonnée_cartographique_X_(BAN),Coordonnée_cartographique_Y_(BAN),Coût_total_5_usages,Coût_chauffage,Coût_éclairage,Coût_ECS,Coût_refroidissement,Coût_auxiliaires,Type_énergie_n°1",
      q = valeur,
      q_fields = "Code_postal_(BAN)",
      qs = "Date_réception_DPE:[2023-01-01 TO 2023-01-02]"
    )
    
    url_encoded <- modify_url(base_url, query = params)
    response <- GET(url_encoded)
    content <- fromJSON(rawToChar(response$content), flatten = FALSE)
    
    df <- content$result
    if (!is.null(df)) {
      logement_existant <- rbind(logement_existant, as.data.frame(df))
    }
  }
  
  # Boucle pour chaque code postal (logements neufs)
  for(valeur in codes_postaux_69){
    base_url <- "https://data.ademe.fr/data-fair/api/v1/datasets/dpe-v2-logements-neufs/lines"
    params <- list(
      page = 1,
      size = 10000,
      select = "N°DPE,Code_postal_(BAN),Etiquette_DPE,Date_réception_DPE,Type_bâtiment,Coordonnée_cartographique_X_(BAN),Coordonnée_cartographique_Y_(BAN),Coût_total_5_usages,Coût_chauffage,Coût_éclairage,Coût_ECS,Coût_refroidissement,Coût_auxiliaires,Type_énergie_n°1",
      q = valeur,
      q_fields = "Code_postal_(BAN)",
      qs = "Date_réception_DPE:[2023-01-01 TO 2023-01-02]"
    )
    
    url_encoded <- modify_url(base_url, query = params)
    response <- GET(url_encoded)
    content <- fromJSON(rawToChar(response$content), flatten = FALSE)
    
    df <- content$result
    if (!is.null(df)) {
      logement_neuf <- rbind(logement_neuf, as.data.frame(df))
    }
  }
  
  logement_neuf$Logement = "Neuf"
  logement_existant$Logement = "Ancien"
  
  # Conversion des dates et types
  logement_neuf$Année_construction = Sys.Date()
  logement_neuf$Année_construction = format(logement_neuf$Année_construction, "%Y")
  logement_neuf$Année_construction = as.numeric(logement_neuf$Année_construction)
  
  # Conversion des coordonnées en sf et en WGS84
  if ("Coordonnée_cartographique_X_(BAN)" %in% colnames(logement_neuf) &&
      "Coordonnée_cartographique_Y_(BAN)" %in% colnames(logement_neuf)) {
    
    logements_neuf_sf <- st_as_sf(logement_neuf, coords = c("Coordonnée_cartographique_X_(BAN)", "Coordonnée_cartographique_Y_(BAN)"), crs = 2154) # Lambert-93
    logements_neuf_geo <- st_transform(logements_neuf_sf, crs = 4326) # WGS84
    
    # Extraire les coordonnées
    logement_neuf$latitude <- st_coordinates(logements_neuf_geo)[,2]
    logement_neuf$longitude <- st_coordinates(logements_neuf_geo)[,1]
  }
  
  # Idem pour logement_existant
  if ("Coordonnée_cartographique_X_(BAN)" %in% colnames(logement_existant) &&
      "Coordonnée_cartographique_Y_(BAN)" %in% colnames(logement_existant)) {
    
    logements_existant_sf <- st_as_sf(logement_existant, coords = c("Coordonnée_cartographique_X_(BAN)", "Coordonnée_cartographique_Y_(BAN)"), crs = 2154)
    logements_existant_geo <- st_transform(logements_existant_sf, crs = 4326)
    
    # Extraire les coordonnées
    logement_existant$latitude <- st_coordinates(logements_existant_geo)[,2]
    logement_existant$longitude <- st_coordinates(logements_existant_geo)[,1]
  }
  
  # Fusionner les données
  colonnes_communes = intersect(colnames(logement_neuf), colnames(logement_existant))
  logements = rbind(logement_neuf[, colonnes_communes], logement_existant[, colonnes_communes])
  
  return(logements)
}

# UI de l'application
ui <- dashboardPage(
  dashboardHeader(title = "Application DPE"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Contexte", tabName = "contexte", icon = icon("info-circle")),
      menuItem("Visualisations", tabName = "visualisations", icon = icon("chart-bar")),
      menuItem("Régression", tabName = "regression", icon = icon("chart-line")), # Nouvel onglet
      menuItem("Cartographie", tabName = "cartographie", icon = icon("map")),
      menuItem("Paramètres", tabName = "parametres", icon = icon("sliders-h"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
    tabItems(
      # Onglet Contexte
      tabItem(tabName = "contexte",
              h2("Présentation des données disponibles"),
              sliderInput("cost_slider", "Filtrer par Coût total :", 
                          min = 0, max = 10000, value = c(0, 10000)),  # Ajuster la plage selon les données
              DTOutput("table_donnees"),
              downloadButton("export_data", "Exporter les données en CSV")
      ),
      # Onglet Visualisations
      tabItem(tabName = "visualisations",
              h2("Graphiques interactifs"),
              selectInput("graph_type", 
                          label = "Choisissez le type de graphique", 
                          choices = c("Nuage de points" = "scatter", 
                                      "Histogramme" = "histogram", 
                                      "Boîte à moustache" = "boxplot", 
                                      "Diagramme" = "barplot")),
              selectInput("x_var", "Sélectionner la variable X", choices = c("Etiquette_DPE", "Type_énergie_n°1","Type_bâtiment")),
              selectInput("y_var", "Sélectionner la variable Y", choices = c("Coût_ECS", "Coût_refroidissement","Coût_total_5_usages", "Coût_auxiliaires","Coût_chauffage","Coût_éclairage")),
              plotlyOutput("plot_graph"),
              radioButtons("theme", "Choisir le thème",
                           choices = c("light", "dark"), inline = TRUE)
      ),
      # Onglet Régression
      tabItem(tabName = "regression",
              h2("Analyse de régression"),
              # Ajoutez vos éléments d'interface pour la régression ici
              # Par exemple, des sélecteurs pour les variables, un bouton pour générer la régression, etc.
      ),
      # Onglet Cartographie
      tabItem(tabName = "cartographie",
              h2("Carte des logements"),
              leafletOutput("map")
      ),
      # Onglet Paramètres
      tabItem(tabName = "parametres",
              h2("Paramètres de l'application"),
              textInput("api_url", "URL de l'API :", "https://data.ademe.fr/data-fair/api/v1"),
              actionButton("update", "Mettre à jour")
      )
    )
  )
)

# Serveur de l'application
server <- function(input, output) {
  
  # Afficher les données dans un tableau
  output$table_donnees <- renderDT({
    req(input$cost_slider)
    data <- get_logements_data()
    # Filtrer par coût total
    data <- data[data$Coût_total_5_usages >= input$cost_slider[1] & data$Coût_total_5_usages <= input$cost_slider[2], ]
    datatable(data)
  })
  
  # Graphique interactif
  output$plot_graph <- renderPlotly({
    req(input$x_var, input$y_var)
    data <- get_logements_data()
    if (input$graph_type == "scatter") {
      ggplot(data, aes_string(x = input$x_var, y = input$y_var)) +
        geom_point() +
        theme_minimal()
    } else if (input$graph_type == "histogram") {
      ggplot(data, aes_string(x = input$x_var)) +
        geom_histogram(binwidth = 1) +
        theme_minimal()
    } else if (input$graph_type == "boxplot") {
      ggplot(data, aes_string(x = input$x_var, y = input$y_var)) +
        geom_boxplot() +
        theme_minimal()
    } else if (input$graph_type == "barplot") {
      ggplot(data, aes_string(x = input$x_var)) +
        geom_bar() +
        theme_minimal()
    }
  })
  
  # Carte Leaflet
  output$map <- renderLeaflet({
    logements <- get_logements_data()
    leaflet(data = logements) %>%
      addTiles() %>%
      addCircleMarkers(~longitude, ~latitude,
                       color = ~ifelse(Logement == "Neuf", "blue", "red"),
                       popup = ~paste("Type:", Logement, "<br>", "Coordonnées:", latitude, longitude))
  })
  
  # Exporter les données en CSV
  output$export_data <- downloadHandler(
    filename = function() { 
      paste("logements_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(get_logements_data(), file, row.names = FALSE)
    }
  )
  
  # Mettre à jour les paramètres
  observeEvent(input$update, {
    # Ici vous pouvez ajouter une logique pour mettre à jour les paramètres
  })
}

# Lancement de l'application
shinyApp(ui, server)

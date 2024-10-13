# Chargement des librairies
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
library(shinymanager)  # Pour l'authentification

# Création des utilisateurs pour l'authentification
credentials <- data.frame(
  user = c("admin", "user"), # Ajouter plus d'utilisateurs ici
  password = c("adminpass", "userpass"), # Mots de passe correspondants
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
              DTOutput("table_donnees")
      ),
      # Onglet Visualisations
      tabItem(tabName = "visualisations",
              h2("Graphiques interactifs"),
              selectInput("x_var", "Sélectionner la variable X", choices = c("Coût_total_5_usages", "Coût_chauffage")),
              selectInput("y_var", "Sélectionner la variable Y", choices = c("Coût_ECS", "Coût_refroidissement")),
              plotlyOutput("scatterplot"),
              radioButtons("theme", "Choisir le thème",
                           choices = c("Default", "Cerulean", "Journal", "Flatly")),
              downloadButton("downloadGraph", "Exporter le graphique")
      ),
      # Onglet Cartographie
      tabItem(tabName = "cartographie",
              h2("Carte interactive des logements"),
              leafletOutput("map"),
              checkboxGroupInput("logement_type", "Type de logement",
                                 choices = c("Neuf", "Ancien")),
              sliderInput("annee", "Année de construction", 
                          min = 1900, max = 2023, value = c(2000, 2023)),
              downloadButton("downloadData", "Exporter les données filtrées")
      ),
      # Onglet Paramètres pour rafraîchir les données via l'API
      tabItem(tabName = "parametres",
              actionButton("refresh_data", "Rafraîchir les données")
      )
    )
  )
)

# Serveur de l'application
server <- function(input, output, session) {
  
  # Authentification
  res_auth <- secure_server(check_credentials = check_credentials(credentials))
  
  # Chargement des données initiales (logements existants + neufs)
  logements_data <- reactiveVal(get_logements_data())
  
  # Présentation des données dans l'onglet Contexte
  output$table_donnees <- renderDT({
    datatable(logements_data())
  })
  
  # Graphique interactif dans l'onglet Visualisations
  output$scatterplot <- renderPlotly({
    x_var <- input$x_var
    y_var <- input$y_var
    plot_ly(data = logements_data(), x = ~get(x_var), y = ~get(y_var), type = "scatter", mode = "markers") %>%
      layout(title = paste("Nuage de points entre", x_var, "et", y_var))
  })
  
  # Carte interactive dans l'onglet Cartographie
  output$map <- renderLeaflet({
    leaflet(data = logements_data()) %>%
      addTiles() %>%
      addCircleMarkers(~Coordonnée_cartographique_X_(BAN), ~Coordonnée_cartographique_Y_(BAN),
                       color = ~ifelse(Logement == "Neuf", "blue", "red"),
                       popup = ~paste("Type:", Logement))
  })
  
  # Bouton pour rafraîchir les données
  observeEvent(input$refresh_data, {
    new_data <- get_logements_data()
    logements_data(new_data)
  })
}

# Lancer l'application
shinyApp(ui, server)

# Installation et chargement des bibliothèques nécessaires
install.packages(c("httr", "jsonlite", "shiny", "shinydashboard", "leaflet", "plotly", "DT", "shinyWidgets"))
library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)
library(DT)
library(shinyWidgets)
library(httr)
library(jsonlite)

# Fonction pour charger les données via l'API
load_logement_data <- function() {
  codes_postaux_69 <- c(
    69001, 69002, 69003, 69004, 69005, 69006, 69007, 69008, 69009,
    69100, 69110, 69120, 69140, 69160, 69170, 69190, 69200,
    69210, 69220, 69230, 69240, 69260, 69310, 69320, 69330,
    69340, 69350, 69360, 69370, 69380, 69390, 69400, 69410,
    69420, 69430, 69440, 69450, 69460, 69500, 69510, 69520,
    69530, 69540, 69560, 69570, 69580, 69590, 69600, 69610,
    69620, 69630, 69640, 69650, 69660, 69670, 69680, 69690
  )
  
  # Initialisation des dataframes vides
  logement_existant <- data.frame()
  logement_neuf <- data.frame()
  
  # Charger les données pour les logements existants
  for (valeur in codes_postaux_69) {
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
    
    if (status_code(response) == 200) {
      content <- fromJSON(rawToChar(response$content), flatten = FALSE)
      df <- content$result
      if (!is.null(df)) {
        logement_existant <- rbind(logement_existant, as.data.frame(df))
      } else {
        showNotification(paste("Pas de données pour le code postal", valeur), type = "warning")
      }
    } else {
      showNotification(paste("Erreur API pour le code postal", valeur), type = "error")
    }
  }
  
  # Charger les données pour les logements neufs
  for (valeur in codes_postaux_69) {
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
    
    if (status_code(response) == 200) {
      content <- fromJSON(rawToChar(response$content), flatten = FALSE)
      df <- content$result
      if (!is.null(df)) {
        logement_neuf <- rbind(logement_neuf, as.data.frame(df))
      } else {
        showNotification(paste("Pas de données pour le code postal", valeur), type = "warning")
      }
    } else {
      showNotification(paste("Erreur API pour le code postal", valeur), type = "error")
    }
  }
  
  # Ajout de la colonne "Logement" et fusion des datasets
  logement_neuf$Logement <- "Neuf"
  logement_existant$Logement <- "Ancien"
  
  colnames_neuf <- colnames(logement_neuf)
  colnames_existant <- colnames(logement_existant)
  colonnes_communes <- intersect(colnames_neuf, colnames_existant)
  
  logements <- rbind(logement_neuf[, colonnes_communes], logement_existant[, colonnes_communes])
  
  # Vérification des coordonnées manquantes
  if (any(is.na(logements$`Coordonnée_cartographique_X_(BAN)`) | is.na(logements$`Coordonnée_cartographique_Y_(BAN)`))) {
    showNotification("Certaines coordonnées sont manquantes dans les données.", type = "warning")
  }
  
  return(logements)
}

# Interface utilisateur (UI)
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Application Logements"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Cartographie", tabName = "cartographie", icon = icon("map")),
      menuItem("Contexte", tabName = "contexte", icon = icon("info-circle")),
      menuItem("Analyse", tabName = "analyse", icon = icon("chart-line")),
      menuItem("Paramètres", tabName = "parametres", icon = icon("cogs"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      tabItem(tabName = "cartographie",
              h2("Carte interactive"),
              leafletOutput("map"),
              selectInput("type_batiment", "Type de bâtiment", choices = NULL),
              checkboxGroupInput("etiquette_dpe", "Etiquette DPE", choices = NULL)
      ),
      
      tabItem(tabName = "contexte",
              h2("Présentation des données"),
              DTOutput("data_table"),
              sliderInput("score_slider", "Filtrer par score DPE", min = 0, max = 100, value = c(0, 100)),
              plotlyOutput("boxplot"),
              downloadButton("export_csv", "Exporter les données")
      ),
      
      tabItem(tabName = "analyse",
              h2("Analyse des corrélations"),
              selectInput("var_x", "Variable X", choices = NULL),
              selectInput("var_y", "Variable Y", choices = NULL),
              plotlyOutput("scatterplot"),
              textOutput("correlation"),
              actionButton("regression", "Afficher la régression linéaire")
      ),
      
      tabItem(tabName = "parametres",
              h2("Personnalisation de l'application"),
              selectInput("theme", "Choisir un thème", choices = c("blue", "black", "green")),
              actionButton("rafraichir", "Rafraîchir les données"),
              passwordInput("password", "Mot de passe"),
              actionButton("connexion", "Connexion")
      )
    )
  )
)

# Serveur
server <- function(input, output, session) {
  # Charger les données au démarrage
  logements <- reactive({
    load_logement_data()
  })
  
  # Contexte - Tableau et boxplot
  output$data_table <- renderDT({
    datatable(logements())
  })
  
  output$boxplot <- renderPlotly({
    plot_ly(logements(), y = ~Coût_chauffage, type = "box")
  })
  
  # Cartographie - Leaflet Map
  output$map <- renderLeaflet({
    validate(
      need(nrow(logements()) > 0, "Pas de données disponibles pour la cartographie."),
      need(!any(is.na(logements()$`Coordonnée_cartographique_X_(BAN)`) | 
                  is.na(logements()$`Coordonnée_cartographique_Y_(BAN)`)),
           "Coordonnées manquantes.")
    )
    
    leaflet(logements()) %>%
      addTiles() %>%
      setView(lng = 1.8883, lat = 46.6034, zoom = 6) %>%
      addMarkers(lng = ~`Coordonnée_cartographique_X_(BAN)`,
                 lat = ~`Coordonnée_cartographique_Y_(BAN)`,
                 popup = ~paste("Coût Total:", `Coût_total_5_usages`, "<br> Type de bâtiment:", `Type_bâtiment`))
  })
  
  # Analyse - Scatter plot et régression
  output$scatterplot <- renderPlotly({
    plot_ly(logements(), x = ~logements()[[input$var_x]], y = ~logements()[[input$var_y]], mode = "markers")
  })
  
  output$correlation <- renderText({
    corr <- cor(logements()[[input$var_x]], logements()[[input$var_y]], use = "complete.obs")
    paste("Coefficient de corrélation: ", round(corr, 2))
  })
  
  observeEvent(input$regression, {
    output$scatterplot <- renderPlotly({
      plot_ly(logements(), x = ~logements()[[input$var_x]], y = ~logements()[[input$var_y]], mode = "markers") %>%
        add_lines(x = ~logements()[[input$var_x]], y = fitted(lm(logements()[[input$var_y]] ~ logements()[[input$var_x]])))
    })
  })
  
  # Paramètres - Rafraîchir le thème
  observeEvent(input$rafraichir, {
    updateSelectInput(session, "theme", selected = input$theme)
  })
  
  # Mise à jour des filtres de l'UI
  observe({
    updateSelectInput(session, "type_batiment", choices = unique(logements()$`Type_bâtiment`))
    updateCheckboxGroupInput(session, "etiquette_dpe", choices = unique(logements()$`Etiquette_DPE`))
  })
  
  # Exportation CSV
  output$export_csv <- downloadHandler(
    filename = function() {
      paste("logements_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(logements(), file)
    }
  )
}

# Lancer l'application
shinyApp(ui, server)

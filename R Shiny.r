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
library(shinymanager)
library(webshot)

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
                           choices = c("Default", "Rouge", "Orange", "Gris")),
              downloadButton("downloadGraph", "Exporter le graphique en PNG")
      ),
      # Onglet Régression
      tabItem(tabName = "regression",
              h2("Régression Linéaire"),
              selectInput("x_var_reg", "Sélectionner la variable X", choices = c("Coût_ECS", "Coût_refroidissement","Coût_total_5_usages", "Coût_auxiliaires","Coût_chauffage","Coût_éclairage")),
              selectInput("y_var_reg", "Sélectionner la variable Y", choices = c("Coût_ECS", "Coût_refroidissement","Coût_total_5_usages", "Coût_auxiliaires","Coût_chauffage","Coût_éclairage")),
              textOutput("correlation"),
              plotOutput("regression_plot")
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
  
  # Chargement des données initiales
  logements_data <- reactiveVal(get_logements_data())
  
  # Afficher la table des données dans l'onglet Contexte
  output$table_donnees <- renderDT({
    # Filtrage selon le slider
    filtered_data <- logements_data()[logements_data()$Coût_total_5_usages >= input$cost_slider[1] & 
                                        logements_data()$Coût_total_5_usages <= input$cost_slider[2], ]
    
    datatable(
      filtered_data,
      options = list(scrollX = TRUE, pageLength = 10)
    )
  })
  
  # Export des données filtrées en CSV
  output$export_data <- downloadHandler(
    filename = function() {
      paste("donnees_filtrees_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Récupérer les données filtrées
      filtered_data <- logements_data()[logements_data()$Coût_total_5_usages >= input$cost_slider[1] & 
                                          logements_data()$Coût_total_5_usages <= input$cost_slider[2], ]
      write.csv(filtered_data, file, row.names = FALSE)
    }
  )
  
  # Graphiques dans l'onglet Visualisations
  output$plot_graph <- renderPlotly({
    req(logements_data())  # Vérifier que les données sont disponibles
    
    data <- logements_data()  # Données des logements
    
    # Sélection du type de graphique choisi
    graph_type <- input$graph_type
    
    # Créer le graphique en fonction du choix de l'utilisateur
    plot <- NULL
    if (graph_type == "scatter") {
      # Nuage de points
      plot <- plot_ly(data, x = ~get(input$x_var), y = ~get(input$y_var), type = 'scatter', mode = 'markers') %>%
        layout(title = "Nuage de points")
      
    } else if (graph_type == "histogram") {
      # Histogramme de la variable X
      plot <- plot_ly(data, x = ~get(input$x_var), type = 'histogram') %>%
        layout(title = paste("Histogramme de", input$x_var))
      
    } else if (graph_type == "boxplot") {
      # Boîte à moustache (boxplot) de la variable Y
      plot <- plot_ly(data, y = ~get(input$y_var), type = 'box') %>%
        layout(title = paste("Boîte à moustache de", input$y_var))
      
    } else if (graph_type == "barplot") {
      # Diagramme en barres
      plot <- plot_ly(data, x = ~get(input$x_var), type = 'bar') %>%
        layout(title = paste("Diagramme de", input$x_var))
    }
    
    # Appliquer le thème choisi
    if (input$theme == "Rouge") {
      plot <- plot %>% layout(paper_bgcolor = "#ff5733", plot_bgcolor = "#ff5733")
    } else if (input$theme == "Orange") {
      plot <- plot %>% layout(paper_bgcolor = "#ffa833", plot_bgcolor = "#ffa833")
    } else if (input$theme == "Gris") {
      plot <- plot %>% layout(paper_bgcolor = "#F5F5F5", plot_bgcolor = "#F5F5F5")
    }
    
    plot
  })
  
  # Télécharger le graphique au format PNG
  output$downloadGraph <- downloadHandler(
    filename = function() {
      paste("graphique", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      webshot("temp_plot.html", file = file)  # Utilisation de webshot pour enregistrer le graphique
    },
    contentType = "image/png"
  )
  
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
  
  # Fonction pour exporter les données filtrées
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("logements_data", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(logements_data(), file, row.names = FALSE)
    }
  )
  
  # Calculer le coefficient de corrélation et afficher le nuage de points avec la droite de régression
  output$correlation <- renderText({
    req(logements_data())  # Vérifier que les données sont disponibles
    data <- logements_data()
    
    x_var <- input$x_var_reg
    y_var <- input$y_var_reg
    
    # Calculer le coefficient de corrélation
    correlation_value <- cor(data[[x_var]], data[[y_var]], use = "complete.obs")
    
    paste("Coefficient de corrélation (", x_var, " vs ", y_var, ") : ", round(correlation_value, 2))
  })
  
  output$regression_plot <- renderPlot({
    req(logements_data())  # Vérifier que les données sont disponibles
    data <- logements_data()
    
    x_var <- input$x_var_reg
    y_var <- input$y_var_reg
    
    # Créer le modèle de régression linéaire
    model <- lm(data[[y_var]] ~ data[[x_var]], data = data)
    
    # Tracer le nuage de points et la droite de régression
    ggplot(data, aes_string(x = x_var, y = y_var)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "blue") +
      labs(title = "Nuage de points avec droite de régression",
           x = x_var,
           y = y_var)
  })
}

# Lancer l'application
shinyApp(ui, server)

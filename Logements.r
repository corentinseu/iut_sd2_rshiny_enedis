library(httr)
library(jsonlite)

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

# Initialiser un dataframe vide pour stocker tous les résultats
logement_existant <- data.frame()

# Loop through each postal code in the 'code_postaux' dataframe
for(valeur in codes_postaux_69){
  base_url <- "https://data.ademe.fr/data-fair/api/v1/datasets/dpe-v2-logements-existants/lines"
  
  # Paramètres de la requête
  params <- list(
    page = 1,
    size = 10000,  # Limite de taille définie à 10 000
    select = "N°DPE,Code_postal_(BAN),Etiquette_DPE,Date_réception_DPE,Type_bâtiment,Coordonnée_cartographique_X_(BAN),Coordonnée_cartographique_Y_(BAN),Coût_total_5_usages,Coût_chauffage,Coût_éclairage,Coût_ECS,Coût_refroidissement,Coût_auxiliaires,Type_énergie_n°1",
    q = valeur,
    q_fields = "Code_postal_(BAN)",
    qs = "Date_réception_DPE:[2023-01-01 TO 2023-01-02]"
  )
  
  # Encodage des paramètres
  url_encoded <- modify_url(base_url, query = params)
  print(url_encoded)
  
  # Effectuer la requête
  response <- GET(url_encoded)
  
  # Afficher le statut de la réponse
  print(status_code(response))
  
  # Convertir le contenu brut en JSON
  content <- fromJSON(rawToChar(response$content), flatten = FALSE)
  
  # Vérifier si le contenu est non nul et ajouter les données à df1
  df <- content$result
  if (!is.null(df)) {
    print(dim(df))
    
    # Convertir en data.frame et ajouter à logement_existant
    logement_existant <- rbind(logement_existant, as.data.frame(df))
  } else {
    print(paste("No data returned for postal code:", valeur))
  }
}

# Afficher les dimensions finales de logement_existant
print(dim(logement_existant))

# Visualiser le dataframe complet
View(logement_existant)

logement_neuf <- data.frame()

# Loop through each postal code in the 'code_postaux' dataframe
for(valeur in codes_postaux_69){
  base_url <- "https://data.ademe.fr/data-fair/api/v1/datasets/dpe-v2-logements-neufs/lines"
  
  # Paramètres de la requête
  params <- list(
    page = 1,
    size = 10000,  # Limite de taille définie à 10 000
    select = "N°DPE,Code_postal_(BAN),Etiquette_DPE,Date_réception_DPE,Type_bâtiment,Coordonnée_cartographique_X_(BAN),Coordonnée_cartographique_Y_(BAN),Coût_total_5_usages,Coût_chauffage,Coût_éclairage,Coût_ECS,Coût_refroidissement,Coût_auxiliaires,Type_énergie_n°1",
    q = valeur,
    q_fields = "Code_postal_(BAN)",
    qs = "Date_réception_DPE:[2023-01-01 TO 2023-01-02]"
  )
  
  # Encodage des paramètres
  url_encoded <- modify_url(base_url, query = params)
  print(url_encoded)
  
  # Effectuer la requête
  response <- GET(url_encoded)
  
  # Afficher le statut de la réponse
  print(status_code(response))
  
  # Convertir le contenu brut en JSON
  content <- fromJSON(rawToChar(response$content), flatten = FALSE)
  
  # Vérifier si le contenu est non nul et ajouter les données à df1
  df <- content$result
  if (!is.null(df)) {
    print(dim(df))
    
    # Convertir en data.frame et ajouter à logement_neuf
    logement_neuf <- rbind(logement_neuf, as.data.frame(df))
  } else {
    print(paste("No data returned for postal code:", valeur))
  }
}

# Afficher les dimensions finales de logement_neuf
print(dim(logement_neuf))

# Visualiser le dataframe complet
View(logement_neuf)

logement_neuf$Logement = "Neuf"
logement_existant$Logement = "Ancien"

logement_neuf$Année_construction = Sys.Date()
class(logement_neuf$Année_construction)
logement_neuf$Année_construction = format(logement_neuf$Année_construction, "%Y")
class(logement_neuf$Année_construction)
logement_neuf$Année_construction = as.numeric(logement_neuf$Année_construction)

colnames_neuf = colnames(logement_neuf)
colnames_existant = colnames(logement_existant)
colonnes_communes = intersect(colnames_neuf,colnames_existant)
logements = rbind(logement_neuf[ ,colonnes_communes] ,
           logement_existant[ ,colonnes_communes])
dim(logements) #verif
table(logements$Logement) #verif
View(logements)

write.csv(logements, file = "C:/Users/okanm/Documents/merged_data.csv", row.names = FALSE)

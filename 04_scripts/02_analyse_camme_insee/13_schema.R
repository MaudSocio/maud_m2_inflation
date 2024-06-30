# Installer et charger le package visNetwork
if (!require(visNetwork)) {
  install.packages("visNetwork")
  library(visNetwork)
}

# Définir les noeuds
nodes <- data.frame(id = 1:6, 
                    label = c("Données d'Etienne Ollion\n1970-2022 / 1990-2021\nRegex pour inflation et prix",
                              "Données d'Europresse 1\nJusqu'à 2023\nArticles avec 'inflation' et 'prix'",
                              "Données Europresse 2\n2020-2023\n'Inflation' dans le titre",
                              "CCA-Inflation\n1970-2022 / 1990-2021\nRegex exhaustive",
                              "CSIP\n1970-2023 / 1990-2023\n'inflation' et 'prix' co-occurrents",
                              "CT-Inflation\n2020-2023\nInflation en titre"),
                    group = c("Source", "Source", "Source", "Corpus", "Corpus", "Corpus"),
                    color = c("#e66101", "#fdb863", "#b2abd2", "#5e3c99", "#5e3c99", "#5e3c99"),
                    shape = "box")

# Définir les arêtes
edges <- data.frame(from = c(1, 1, 2, 3),
                    to = c(4, 5, 5, 6),
                    color = c("#000000", "#000000", "#000000", "#000000"))

# Créer le graphique
visNetwork(nodes, edges, width = "100%") %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visGroups(groupname = "Source", shape = "icon", icon = list(code = "f1c0", color = "#f0a30a")) %>%
  visGroups(groupname = "Corpus", shape = "icon", icon = list(code = "f1c3", color = "#7F7F7F")) %>%
  visLegend(enabled = TRUE)

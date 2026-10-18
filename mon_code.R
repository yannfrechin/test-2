# =============================================================
# Analyse des sorties de pêche de loisir
# 1. Charger les données
# 2. Les nettoyer
# 3. Enregistrer la base propre
# 4. Générer un rapport HTML par région
# =============================================================
# Avant de lancer : ouvre le projet RStudio, et vérifie que le
# dossier contient : analyse.R, rapport_region.Rmd, donnees/panel_peche.csv
# Lance tout le script avec Cmd + Shift + Entrée.

# Packages (à installer une seule fois, dans la console) :
# install.packages(c("dplyr", "ggplot2", "knitr", "rmarkdown"))
library(dplyr)

# -------------------------------------------------------------
# 1. CHARGER LES DONNÉES
# -------------------------------------------------------------
# na.strings : les cases vides deviennent des NA
donnees <- read.csv("donnees/panel_peche.csv", na.strings = c("", " ", "NA"))
donnees_brutes <- donnees          # copie de la base d'origine

names(donnees)                     # les noms des colonnes
nrow(donnees)                      # le nombre de lignes au départ
str(donnees)                       # le type de chaque colonne

# -------------------------------------------------------------
# 2. NETTOYER
# -------------------------------------------------------------

# --- Étape 1 : enlever les cases vides ---
colSums(is.na(donnees))            # combien de cases vides par colonne
donnees <- na.omit(donnees)        # enlève les lignes avec une case vide
cat("Après les cases vides :", nrow(donnees), "lignes\n")

# --- Étape 2 : corriger ce qui est mal écrit ---
table(donnees$espece)              # on voit les fautes : "Bar", "BAR", "maquerau"...

donnees$espece <- tolower(donnees$espece)               # tout en minuscules
donnees$espece <- trimws(donnees$espece)                # enlève les espaces au début et à la fin
donnees$espece <- gsub("  ", " ", donnees$espece)       # remplace les doubles espaces par un seul
donnees$espece[donnees$espece == "maquerau"]   <- "maquereau"
donnees$espece[donnees$espece == "lieu-jaune"] <- "lieu jaune"

table(donnees$espece)              # on vérifie : plus de fautes

# --- Étape 3 : enlever les doublons ---
sum(duplicated(donnees))           # combien de lignes en double
donnees <- distinct(donnees)       # on les enlève
cat("Après les doublons :", nrow(donnees), "lignes\n")

# --- Étape 4 : enlever les valeurs aberrantes ---
summary(donnees$duree_h)           # regarder Min. et Max.
summary(donnees$nb_captures)
summary(donnees$nb_relaches)
summary(donnees$poids_kg)

# On garde seulement les valeurs possibles :
# une sortie dure entre 0 et 12 heures, moins de 100 poissons gardés ou relâchés,
# un poids total entre 0 et 100 kg
donnees <- filter(donnees,
                  duree_h > 0, duree_h <= 12,
                  nb_captures < 100,
                  nb_relaches < 100,
                  poids_kg >= 0, poids_kg < 100)

# --- Vérification ---
cat("Avant :", nrow(donnees_brutes), "lignes - Après :", nrow(donnees), "lignes\n")
summary(donnees)

# -------------------------------------------------------------
# 3. ENREGISTRER LA BASE PROPRE
# -------------------------------------------------------------
write.csv(donnees, "donnees/panel_peche_propre.csv", row.names = FALSE)

# -------------------------------------------------------------
# 4. UN RAPPORT PAR RÉGION
# -------------------------------------------------------------
# Un seul rapport, pour une région :
rmarkdown::render("rapport_region.Rmd",
                  params = list(region = "Bretagne"),
                  output_file = "rapport_Bretagne.html",
                  output_dir = "rapports",
                  envir = new.env())   # le rapport travaille dans son propre espace

# Tous les rapports d'un coup (une région après l'autre) :
regions <- unique(donnees$region)

for (r in regions) {
  rmarkdown::render("rapport_region.Rmd",
                    params = list(region = r),
                    output_file = paste0("rapport_", r, ".html"),
                    output_dir = "rapports",
                    envir = new.env())
}
# Les rapports sont dans le dossier "rapports" : ouvre-les avec ton navigateur.
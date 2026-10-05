# =============================================================================
# 01 — Chargement du SIM mensuel Météo-France (SWI, SSWI et SPI multi-échelles)
# -----------------------------------------------------------------------------
# Source : data.gouv.fr, jeu « Données changement climatique - SIM mensuelle »
#          (Météo-France, modèle SIM2, mailles de 8 km : 9 892 au total, dont
#          environ 8 980 en métropole, les autres couvrant les bassins amont
#          à l'étranger).
#          Un fichier par année ; celui de l'année en cours est recalculé
#          chaque mois.
# Ce script :
#   1. trouve le fichier de l'année via l'API data.gouv ;
#   2. le télécharge s'il est absent ou plus ancien que sur le serveur ;
#   3. garde, pour l'année demandée, les colonnes utiles : coordonnées, mois,
#      bilan hydrique mensuel (pluie, température, ETP, évaporation, pluies
#      efficaces, drainage, ruissellement), SWI, et SSWI / SPI sur 1 / 3 / 6 /
#      12 mois (toutes les mailles du fichier, y compris hors de France : le
#      tri est fait ensuite par rattacher(), dans fonctions_cartes.R) ;
#   4. enregistre data/sim_mensuel_<annee>.rds pour le rapport Quarto.
#
# Dépendances : httr2, jsonlite, data.table, dplyr
# =============================================================================

# Chargement des packages (sans les messages de démarrage)
suppressPackageStartupMessages({
  library(httr2)       # requêtes HTTP
  library(jsonlite)    # lecture du JSON de l'API
  library(data.table)  # lecture et manipulation rapides
  library(dplyr)       # manipulation de la table des ressources
})

# --- Paramètres --------------------------------------------------------------

annee <- 2026   # année à charger

# Dossier du script (si lancé depuis RStudio), sinon répertoire de travail
racine <- tryCatch(dirname(rstudioapi::getSourceEditorContext()$path),
                   error = function(e) getwd())
# Un script jamais enregistré renvoie un chemin vide : on arrête
if (racine == "") stop("Enregistrez le script avant de l'exécuter (chemin du script vide).")

# Dossier data/ pour les téléchargements et les sorties
dossier <- file.path(racine, "data")
dir.create(dossier, showWarnings = FALSE)

# Slug du jeu (accepté par l'API au même titre que l'identifiant)
url_api <- "https://www.data.gouv.fr/api/1/datasets/donnees-changement-climatique-sim-mensuelle/"

# --- Ressources ----------------------------------------------------------------

# Interrogation de l'API (3 essais max) et conversion du JSON en liste R
reponse <- request(url_api) |>
  req_user_agent("R/DRAAF-HdF SRISE") |>
  req_retry(max_tries = 3) |>
  req_perform() |>
  resp_body_string() |>
  fromJSON(simplifyVector = TRUE)

# Table des fichiers disponibles, avec la date de modification convertie en date-heure UTC
ressources <- as_tibble(reponse$resources) |>
  select(title, url, last_modified, filesize, format) |>
  mutate(last_modified = as.POSIXct(last_modified, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"))

# Un fichier par année ("MENS_SIM2_2026"), celui de l'année en cours étant
# recalculé chaque mois. La page annonce des lots pluriannuels : on garde un
# repli sur un titre contenant la décennie, puis sur le CSV le plus récent.

# On ne garde que les ressources au format csv
csv <- ressources |> filter(grepl("csv", format, ignore.case = TRUE) | grepl("\\.csv", url))
# Choix 1 : le fichier portant exactement le nom de l'année
cible <- csv |> filter(title == sprintf("MENS_SIM2_%d", annee))
# Choix 2 : un fichier dont le titre contient la décennie (ex. 2020)
if (nrow(cible) == 0) cible <- csv |> filter(grepl(as.character(annee - annee %% 10), title))
# Choix 3 : à défaut, le csv le plus récent
if (nrow(cible) == 0) cible <- csv |> arrange(desc(last_modified)) |> slice(1)
# S'il reste plusieurs candidats, on prend le plus récemment modifié
cible <- cible |> arrange(desc(last_modified)) |> slice(1)
message("Ressource retenue : ", cible$title, " (modifiée le ", format(cible$last_modified, "%d/%m/%Y"), ")")

# --- Téléchargement conditionnel ---------------------------------------------------

# Chemin du fichier local
fichier <- file.path(dossier, sprintf("MENS_SIM2_%d.csv.gz", annee))

# Le fichier local est "à jour" s'il existe et n'est pas plus ancien que celui du serveur
a_jour <- file.exists(fichier) && !is.na(cible$last_modified) &&
  file.mtime(fichier) >= cible$last_modified

if (!a_jour) {
  # Téléchargement avec barre de progression
  message("Téléchargement…")
  request(cible$url) |>
    req_user_agent("R/DRAAF-HdF SRISE") |>
    req_retry(max_tries = 3) |>
    req_progress() |>
    req_perform(path = fichier)
  # On donne au fichier local la date du serveur, pour la comparaison au prochain lancement
  Sys.setFileTime(fichier, cible$last_modified)
} else message("Fichier local à jour, pas de téléchargement.")

# --- Lecture -----------------------------------------------------------------------

# Lecture de l'en-tête seul pour connaître les colonnes présentes
entete   <- names(fread(fichier, nrows = 0, sep = ";"))

# Repérage des colonnes par leur nom (qui peut varier selon les fichiers)
col_date <- grep("^DATE|^MOIS|^AAAAMM", entete, value = TRUE)[1]   # colonne de date
col_swi  <- grep("^SWI", entete, value = TRUE)[1]                  # indice d'humidité des sols
col_ind  <- grep("^(SSWI|SPI)[0-9]+", entete, value = TRUE)   # indices standardisés multi-échelles

# Bilan hydrique mensuel (valeurs absolues) : pluie, température, ETP,
# évaporation réelle, pluies efficaces, drainage, ruissellement
# (nom utilisé dans le script = nom de colonne dans le fichier)
col_bilan <- c(PRECIP = grep("^PRETOT", entete, value = TRUE)[1], T = "T", ETP = "ETP",
               EVAP = "EVAP", PE = "PE", DRAINC = "DRAINC", RUNC = "RUNC")
# On écarte les variables absentes du fichier
col_bilan <- col_bilan[!is.na(col_bilan) & col_bilan %in% entete]

# Arrêt si une colonne indispensable manque (on affiche les colonnes trouvées pour diagnostiquer)
if (is.na(col_date) || is.na(col_swi) || length(col_ind) == 0) {
  stop("Colonnes attendues introuvables. Colonnes du fichier : ", paste(entete, collapse = ", "))
}

# Liste finale des colonnes à lire
cols <- c("LAMBX", "LAMBY", col_date, col_swi, unname(col_bilan), col_ind)
message("Colonnes retenues : ", paste(cols, collapse = ", "))

# Lecture des seules colonnes utiles (date lue en texte)
sim <- fread(fichier, sep = ";", select = cols, colClasses = list(character = col_date))

# Renommage : noms standard pour la date, le SWI et les variables du bilan
setnames(sim, c(col_date, col_swi), c("DATE", "SWI"))
setnames(sim, unname(col_bilan), names(col_bilan))
# SSWI3_MENS -> SSWI_3, SPI12_MENS -> SPI_12 : nom de l'indice + "_" + échelle
setnames(sim, col_ind, sub("^(SSWI|SPI)([0-9]+).*$", "\\1_\\2", col_ind))

# Création du mois au format "AAAA-MM" à partir de DATE (format AAAAMM)
sim[, mois := paste0(substr(DATE, 1, 4), "-", substr(DATE, 5, 6))]
# On ne garde que l'année demandée, puis on supprime DATE
sim <- sim[substr(DATE, 1, 4) == as.character(annee)][, DATE := NULL]
# Coordonnées et mois placés en premières colonnes
setcolorder(sim, c("LAMBX", "LAMBY", "mois"))

# Contrôle : nombre de mailles et étendue des mois disponibles
cat("Année", annee, ":", uniqueN(sim$LAMBX * 1e6 + sim$LAMBY), "mailles,",
    "mois", paste(range(sim$mois), collapse = " → "), "\n")
# Contrôle : moyenne nationale des indices par mois
print(sim[, lapply(.SD, function(v) round(mean(v, na.rm = TRUE), 2)), by = mois,
          .SDcols = patterns("^SWI|^SSWI|^SPI")])

# Enregistrement du résultat pour le rapport Quarto
saveRDS(sim, file.path(dossier, sprintf("sim_mensuel_%d.rds", annee)))

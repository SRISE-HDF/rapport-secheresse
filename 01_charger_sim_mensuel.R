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
#   4. enregistre data/sim_mensuel_<annee>.rds pour le rapport Quarto ;
#   5. [GitHub Actions] dépose le .rds sur le stockage S3 d'Onyxia (SSP Cloud).
#
# Deux modes de fonctionnement :
#   - en local (RStudio) : comportement d'origine, rien n'est envoyé sur S3 ;
#   - sur GitHub Actions : la variable d'environnement S3_BUCKET est définie,
#     le script compare la version du serveur à celle déjà déposée sur S3
#     (fichier témoin maj_sim_<annee>.txt) et s'arrête si rien n'a changé.
#
# Dépendances : httr2, jsonlite, data.table, dplyr (+ aws.s3 pour le mode S3)
# =============================================================================

# Chargement des packages (sans les messages de démarrage)
suppressPackageStartupMessages({
  library(httr2)       # requêtes HTTP
  library(jsonlite)    # lecture du JSON de l'API
  library(data.table)  # lecture et manipulation rapides
  library(dplyr)       # manipulation de la table des ressources
})

# --- Paramètres --------------------------------------------------------------

# Année à charger : variable d'environnement ANNEE si elle existe,
# sinon l'année en cours (évite de modifier le script chaque 1er janvier)
annee <- as.integer(Sys.getenv("ANNEE", format(Sys.Date(), "%Y")))

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

# --- Stockage S3 Onyxia (actif seulement si S3_BUCKET est défini) -------------

s3_actif <- nzchar(Sys.getenv("S3_BUCKET"))
if (s3_actif) {
  bucket  <- Sys.getenv("S3_BUCKET")                 # sur SSP Cloud : votre identifiant
  prefixe <- Sys.getenv("S3_PREFIX", "meteo/")        # « sous-dossier » dans le bucket
  # region = "" est indispensable avec MinIO ; l'adresse vient de AWS_S3_ENDPOINT
  s3_args <- list(bucket = bucket, region = "",
                  base_url = Sys.getenv("AWS_S3_ENDPOINT", "minio.lab.sspcloud.fr"))
  objet_maj <- paste0(prefixe, sprintf("maj_sim_%d.txt", annee))   # fichier témoin
  message("Mode S3 : dépôt dans s3://", bucket, "/", prefixe)
}

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

# --- Mode S3 : arrêt anticipé si S3 a déjà la version du serveur --------------
# Sur GitHub, le dossier data/ est vide à chaque lancement : la comparaison
# des dates se fait donc avec le fichier témoin déposé sur S3.

if (s3_actif && !is.na(cible$last_modified)) {
  maj_s3 <- tryCatch({
    if (do.call(aws.s3::object_exists, c(list(object = objet_maj), s3_args))) {
      brut <- do.call(aws.s3::get_object, c(list(object = objet_maj), s3_args))
      as.POSIXct(trimws(rawToChar(brut)), tz = "UTC")
    } else NA
  }, error = function(e) NA)
  
  if (!is.na(maj_s3) && maj_s3 >= cible$last_modified) {
    message("S3 contient déjà la version du ", format(maj_s3, "%d/%m/%Y %H:%M"),
            " UTC : rien à faire.")
    quit(save = "no", status = 0)
  }
}

# --- Téléchargement conditionnel ---------------------------------------------------

# Chemin du fichier local
fichier <- file.path(dossier, sprintf("MENS_SIM2_%d.csv.gz", annee))

# Le fichier local est "à jour" s'il existe et n'est pas plus ancien que celui du serveur
a_jour <- file.exists(fichier) && !is.na(cible$last_modified) &&
  file.mtime(fichier) >= cible$last_modified

if (!a_jour) {
  message("Téléchargement…")
  req <- request(cible$url) |>
    req_user_agent("R/DRAAF-HdF SRISE") |>
    req_retry(max_tries = 3)
  # Barre de progression seulement en session interactive (évite d'encombrer les logs GitHub)
  if (interactive()) req <- req_progress(req)
  req_perform(req, path = fichier)
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

# Garde-fou (utile en janvier, quand le fichier de l'année n'est pas encore publié)
if (nrow(sim) == 0) stop("Aucune donnée pour ", annee, " dans « ", cible$title, " ».")

# Coordonnées et mois placés en premières colonnes
setcolorder(sim, c("LAMBX", "LAMBY", "mois"))

# Contrôle : nombre de mailles et étendue des mois disponibles
cat("Année", annee, ":", uniqueN(sim$LAMBX * 1e6 + sim$LAMBY), "mailles,",
    "mois", paste(range(sim$mois), collapse = " → "), "\n")
# Contrôle : moyenne nationale des indices par mois
print(sim[, lapply(.SD, function(v) round(mean(v, na.rm = TRUE), 2)), by = mois,
          .SDcols = patterns("^SWI|^SSWI|^SPI")])

# Enregistrement du résultat pour le rapport Quarto
fichier_rds <- file.path(dossier, sprintf("sim_mensuel_%d.rds", annee))
saveRDS(sim, fichier_rds)

# --- Mode S3 : dépôt du résultat et du fichier témoin ---------------------------

if (s3_actif) {
  ok <- do.call(aws.s3::put_object,
                c(list(file = fichier_rds, object = paste0(prefixe, basename(fichier_rds))), s3_args))
  if (!isTRUE(ok)) stop("Échec de l'envoi de ", basename(fichier_rds), " sur S3.")
  
  # Fichier témoin : date de la version serveur traitée (relu au prochain lancement)
  temoin <- tempfile(fileext = ".txt")
  writeLines(format(cible$last_modified, "%Y-%m-%d %H:%M:%S", tz = "UTC"), temoin)
  do.call(aws.s3::put_object, c(list(file = temoin, object = objet_maj), s3_args))
  
  message("Déposé sur S3 : ", bucket, "/", prefixe, basename(fichier_rds))
}
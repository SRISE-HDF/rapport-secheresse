# =============================================================================
# 00 — Normales mensuelles 1991-2020 du bilan hydrique (SIM2 mensuel)
# -----------------------------------------------------------------------------
# À lancer une fois (puis seulement si la période de référence change bien sûr ;).
# Pour chaque maille et chaque mois calendaire, moyenne 1991-2020 de :
#   PRECIP (pluie), ETP, EVAP (évaporation réelle), PE (pluies efficaces),
#   SWI, T (température).
# Les fichiers annuels MENS_SIM2_AAAA sont téléchargés un par un depuis
# data.gouv, réduits aux colonnes utiles et mis en cache dans data/normales/ 
# le script est relançable et reprend là où il s'est arrêté.
# Sortie : data/normales_1991_2020.rds
#
# Dépendances: httr2, jsonlite, data.table, dplyr, purrr
# =============================================================================

# Chargement des packages (sans les messages de démarrage)
suppressPackageStartupMessages({
  library(httr2)       # requêtes HTTP
  library(jsonlite)    # lecture du JSON de l'API
  library(data.table)  # lecture et agrégation rapides
  library(dplyr)       # manipulation de la table des ressources
  library(purrr)       # boucle sur les années (map)
})

# --- Paramètres --------------------------------------------------------------

ref_debut <- 1991     # première année de la période de référence
ref_fin   <- 2020     # dernière année de la période de référence
garder_csv <- FALSE   # supprimer les csv bruts une fois traités

# Les fichiers seront générés à l'endroit où est enregistré ce programme (comme ça c'est portable), sinon répertoire de travail
racine <- tryCatch(dirname(rstudioapi::getSourceEditorContext()$path),
                   error = function(e) getwd())
# Un script jamais enregistré renvoie un chemin vide : on arrête
if (racine == "") stop("Enregistrez le script avant de l'exécuter (chemin du script vide).")

# Dossiers de travail: data/ pour les sorties, data/normales/ pour le cache
dossier <- file.path(racine, "data")
dos_ref <- file.path(dossier, "normales")
dir.create(dos_ref, recursive = TRUE, showWarnings = FALSE)

# Adresse de l'API data.gouv décrivant le jeu de données SIM2 mensuel
url_api <- "https://www.data.gouv.fr/api/1/datasets/donnees-changement-climatique-sim-mensuelle/"

# --- Ressources ----------------------------------------------------------------

# Interrogation de l'API (3 essais max) et conversion du JSON en liste R
reponse <- request(url_api) |>
  req_user_agent("R/DRAAF-HdF SRISE") |>
  req_retry(max_tries = 3) |>
  req_perform() |>
  resp_body_string() |>
  fromJSON(simplifyVector = TRUE)

# Table des fichiers disponibles: on ne garde que le titre et l'URL
ressources <- as_tibble(reponse$resources) |> select(title, url)

# --- Une année: téléchargement, réduction, cache ---------------------------------------

# Correspondance : nom utilisé dans le script = nom de colonne dans les fichiers SIM2
variables <- c(PRECIP = "PRETOTM", T = "T", ETP = "ETP", EVAP = "EVAP", PE = "PE", SWI = "SWI")

# Renvoie les données mensuelles d'une année (depuis le cache ou par téléchargement)
charger_annee <- function(annee_sel) {
  
  # 1. Si l'année est déjà en cache, on la relit directement
  cache <- file.path(dos_ref, sprintf("mens_%d.rds", annee_sel))
  if (file.exists(cache)) {
    d <- readRDS(cache)
    if (nrow(d) > 0) return(d)          # un cache vide (bug antérieur) est reconstruit
  }
  
  # 2. Recherche de la ressource correspondant à l'année
  res <- ressources |> filter(title == sprintf("MENS_SIM2_%d", annee_sel))
  if (nrow(res) == 0) stop("Pas de ressource MENS_SIM2_", annee_sel)
  
  # 3. Téléchargement du fichier csv compressé dans data/
  csv <- file.path(dossier, sprintf("MENS_SIM2_%d.csv.gz", annee_sel))
  message(annee_sel, " : téléchargement…")
  request(res$url) |>
    req_user_agent("R/DRAAF-HdF SRISE") |>
    req_retry(max_tries = 3) |>
    req_perform(path = csv)
  
  # 4. Lecture de l'en-tête seul pour connaître les colonnes présentes
  entete <- names(fread(csv, nrows = 0, sep = ";"))
  
  # 5. Lecture des seules colonnes utiles (DATE lue en texte)
  # PRETOTM peut manquer sur les vieux fichiers : on le reconstitue
  cols <- intersect(c("LAMBX", "LAMBY", "DATE", unname(variables), "PRENEI", "PRELIQ"), entete)
  d <- fread(csv, sep = ";", select = cols, colClasses = list(character = "DATE"))
  
  # Pluie totale = neige + pluie liquide si PRETOTM est absent
  if (!"PRETOTM" %in% names(d) && all(c("PRENEI", "PRELIQ") %in% names(d))) {
    d[, PRETOTM := PRENEI + PRELIQ]
  }
  
  # 6. On ne conserve que les coordonnées, la date et les variables d'intérêt
  d <- d[, c("LAMBX", "LAMBY", "DATE", intersect(unname(variables), names(d))), with = FALSE]
  
  # 7. Renommage des colonnes avec les noms du script (PRETOTM -> PRECIP, etc.)
  setnames(d, unname(variables)[unname(variables) %in% names(d)],
           names(variables)[unname(variables) %in% names(d)])
  
  # 8. Extraction de l'année et du mois depuis DATE (format AAAAMM), puis suppression de DATE
  d[, `:=`(an = as.integer(substr(DATE, 1, 4)), mois_num = as.integer(substr(DATE, 5, 6)))]
  d[, DATE := NULL]
  
  # 9. Sécurité : on ne garde que les lignes de l'année demandée
  d <- d[an == annee_sel]
  if (nrow(d) == 0) stop("Aucune ligne pour ", annee_sel, " dans ", basename(csv))
  
  # 10. Mise en cache, puis suppression du csv brut si demandé
  saveRDS(d, cache)
  if (!garder_csv) file.remove(csv)
  d
}

# --- Constitution des normales ------------------------------------------------------------

# Chargement de toutes les années de référence et empilement dans une seule table
ref <- map(ref_debut:ref_fin, charger_annee) |> rbindlist(fill = TRUE)

# Message de contrôle : nombre de lignes et de mailles
cat("Référence :", ref_debut, "-", ref_fin, "|", nrow(ref), "lignes,",
    uniqueN(ref[, .(LAMBX, LAMBY)]), "mailles\n")

# Moyenne de chaque variable par maille et par mois calendaire (NA ignorés)
normales <- ref[, lapply(.SD, mean, na.rm = TRUE),
                by = .(LAMBX, LAMBY, mois_num),
                .SDcols = intersect(names(variables), names(ref))]

# Préfixe "N_" sur les variables pour les distinguer des valeurs observées
setnames(normales, setdiff(names(normales), c("LAMBX", "LAMBY", "mois_num")),
         paste0("N_", setdiff(names(normales), c("LAMBX", "LAMBY", "mois_num"))))

# Enregistrement du résultat final
saveRDS(normales, file.path(dossier, sprintf("normales_%d_%d.rds", ref_debut, ref_fin)))

# Contrôle : normales nationales par mois
print(normales[, lapply(.SD, function(v) round(mean(v, na.rm = TRUE), 1)), by = mois_num,
               .SDcols = patterns("^N_")][order(mois_num)])

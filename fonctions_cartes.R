# =============================================================================
# Fonctions partagées : légende Météo-France, géométries, cartes, séries
# Chargé par le rapport Quarto (source("fonctions_cartes.R"))
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(sf)
  library(dplyr)
  library(ggplot2)
  library(patchwork)
  library(terra)
})

# --- Légende Météo-France (classes par durée de retour) ------------------------------

legende_mf <- list(
  breaks  = c(-Inf, qnorm(0.04), qnorm(0.10), qnorm(0.20),
              qnorm(0.80), qnorm(0.90), qnorm(0.96), Inf),
  labels  = c("Exceptionnellement sec", "Inhabituellement sec", "Plus sec que la normale",
              "Autour de la normale", "Plus humide que la normale",
              "Inhabituellement humide", "Exceptionnellement humide"),
  # couleurs relevées sur les cartes Météo-France
  couleurs = c("#FF3C00", "#FFA000", "#FFD200", "#ECFFAD", "#96FA00", "#0FD200", "#008700"),
  seuils = c("Dr25", "Dr10", "Dr5", "Dr5", "Dr10", "Dr25")   # repères aux frontières de classes
)
names(legende_mf$couleurs) <- legende_mf$labels
classer <- function(x) cut(x, breaks = legende_mf$breaks, labels = legende_mf$labels)

# Cas limite : la valeur est à moins de `marge` d'un seuil de classe ; la classe
# affichée peut alors basculer au prochain recalcul de Météo-France
cas_limite <- function(x, marge = 0.05) {
  seuils <- legende_mf$breaks[is.finite(legende_mf$breaks)]
  vapply(x, function(v) !is.na(v) && min(abs(v - seuils)) < marge, logical(1))
}

mois_fr <- c("janvier", "février", "mars", "avril", "mai", "juin", "juillet",
             "août", "septembre", "octobre", "novembre", "décembre")
libelle_mois <- function(m) paste(mois_fr[as.integer(substr(m, 6, 7))], substr(m, 1, 4))

# Fenêtre couverte par un SSWI sur k mois se terminant au mois m ("AAAA-MM") :
# "juin à août 2026", ou "septembre 2025 à août 2026" si l'année change
fenetre_mois <- function(m, k) {
  fin   <- as.Date(paste0(m, "-01"))
  debut <- seq(fin, by = paste0("-", k - 1, " months"), length.out = 2)[2]
  if (k == 1) return(libelle_mois(m))
  mf <- mois_fr[as.integer(format(fin, "%m"))]; md <- mois_fr[as.integer(format(debut, "%m"))]
  if (format(fin, "%Y") == format(debut, "%Y")) sprintf("%s à %s %s", md, mf, format(fin, "%Y"))
  else sprintf("%s %s à %s %s", md, format(debut, "%Y"), mf, format(fin, "%Y"))
}

# --- Géométries -------------------------------------------------------------------------

# Mailles SIM : centres en Lambert II étendu (hm) -> carrés de 8 km en Lambert-93
mailles_sf <- function(sim) {
  unique(sim[, .(LAMBX, LAMBY)]) |>
    st_as_sf(coords = c("LAMBX", "LAMBY"), crs = 27572, remove = FALSE) |>
    mutate(geometry = geometry * 100) |> st_set_crs(27572) |>
    st_buffer(4000, endCapStyle = "SQUARE") |>
    st_transform(2154)
}

# Contours ADMIN EXPRESS (IGN) simplifiés et mis en cache dans data/
charger_admin <- function(dossier, tolerance_m = 300) {
  cache <- file.path(dossier, "admin_express.rds")
  if (file.exists(cache)) return(readRDS(cache))  # Permet de ne charger qu'une seule fois le fichier s'il existe déjà
  args_wfs   <- names(formals(happign::get_wfs))
  arg_filtre <- if ("ecql_filter" %in% args_wfs) "ecql_filter" else "query"
  get_admin  <- function(layer, filtre) {
    rlang::exec(happign::get_wfs, x = NULL, layer = layer, !!arg_filtre := filtre) |>
      st_transform(2154) |> st_make_valid() |>
      st_simplify(dTolerance = tolerance_m, preserveTopology = TRUE)
  }
  regions <- get_admin("ADMINEXPRESS-COG.LATEST:region", "code_insee > '10'") |> # On écarte des DROM
    select(code_reg = code_insee, nom_reg = nom_officiel) |> arrange(nom_reg)
  deps <- get_admin("ADMINEXPRESS-COG.LATEST:departement", "code_insee < '96'") |>
    select(code_dep = code_insee, nom_dep = nom_officiel, code_reg = code_insee_de_la_region) |>
    arrange(code_dep)
  admin <- list(regions = regions, deps = deps)
  saveRDS(admin, cache)
  admin
}

# Communes (ADMIN EXPRESS), simplifiées, chargées et mises en cache région par
# région dans data/communes/ : un rendu limité à quelques régions ne
# télécharge que celles-là ; les suivantes s'ajoutent au fil des rendus.
charger_communes <- function(dossier, regions, tolerance_m = 100) {
  dos <- file.path(dossier, "communes")
  dir.create(dos, showWarnings = FALSE)
  args_wfs   <- names(formals(happign::get_wfs))
  arg_filtre <- if ("ecql_filter" %in% args_wfs) "ecql_filter" else "query"
  lapply(regions$code_reg, function(cr) {
    cache <- file.path(dos, sprintf("communes_%s.rds", cr))
    if (file.exists(cache)) return(readRDS(cache))
    message("Téléchargement des communes de la région ", cr, "…")
    com <- rlang::exec(happign::get_wfs, x = NULL, layer = "ADMINEXPRESS-COG.LATEST:commune",
                       !!arg_filtre := sprintf("code_insee_de_la_region = '%s'", cr)) |>
      st_transform(2154) |> st_make_valid() |>
      st_simplify(dTolerance = tolerance_m, preserveTopology = TRUE) |>
      select(code_com = code_insee, nom_com = nom_officiel,
             code_dep = code_insee_du_departement) |>
      mutate(code_reg = cr)
    saveRDS(com, cache)
    com
  }) |> bind_rows()
}

# Sélection de régions à partir du paramètre texte u YAML : "toutes", ou codes /
# noms séparés par des virgules ("32", "32,11", "Hauts-de-France, Normandie")
selectionner_regions <- function(regions, choix) {
  choix <- trimws(unlist(strsplit(as.character(choix), ",")))
  if (length(choix) == 0 || tolower(choix[1]) %in% c("toutes", "all", "")) return(regions)
  ok <- regions$code_reg %in% choix | tolower(regions$nom_reg) %in% tolower(choix)
  if (!any(ok)) stop("Aucune région ne correspond à : ", paste(choix, collapse = ", "),
                     "\nCodes disponibles : ", paste(regions$code_reg, regions$nom_reg, sep = " = ", collapse = " ; "))
  regions[ok, ]
}

# Chaque commune prend la valeur de la maille contenant son centre (rendu Météo-France)
rabattre_communes <- function(communes, mailles_val) {
  idx <- st_nearest_feature(st_point_on_surface(communes), mailles_val)
  communes |> mutate(valeur = mailles_val$valeur[idx], classe = mailles_val$classe[idx])
}

# Rattachement de chaque maille à un département (centre au plus proche).
# La grille SIM2 compte 9 892 mailles, dont environ 900 entièrement à l'étranger
# On garde toute maille dont le carré de 8 km touche le territoire, donc aussi
# les mailles à cheval sur une frontière ou sur le littoral.
rattacher <- function(mailles, deps) {
  en_france <- lengths(st_intersects(mailles, deps)) > 0
  if (any(!en_france)) {
    message(sum(!en_france), " mailles entièrement hors de France écartées (",
            sum(en_france), " conservées)")
  }
  mailles <- mailles[en_france, ]
  idx <- st_nearest_feature(st_centroid(mailles), deps)
  mailles$code_dep <- deps$code_dep[idx]
  mailles$nom_dep  <- deps$nom_dep[idx]
  mailles$code_reg <- deps$code_reg[idx]
  mailles
}

# --- Habillage commun ---------------------------------------------------------------------

theme_carte <- function() {
  theme_minimal(base_size = 12) +
    theme(panel.grid = element_blank(), legend.position = "bottom",
          legend.title = element_text(size = 9, colour = "grey40"),
          plot.title.position = "plot")
}
# Barre de légende façon Météo-France : sept cases jointes, repères de durée de
# retour (Dr) au-dessus des frontières, libellés des classes en biais dessous.
legende_barre <- function(taille_texte = 3) {
  n <- length(legende_mf$labels)
  cases <- data.frame(xmin = 0:(n - 1), xmax = 1:n, classe = legende_mf$labels)
  seuils <- data.frame(x = 1:(n - 1), lab = legende_mf$seuils)
  ggplot() +
    geom_rect(data = cases, aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = 1, fill = classe),
              colour = "black", linewidth = 0.3) +
    geom_text(data = seuils, aes(x = x, y = 1.35, label = lab), size = taille_texte) +
    geom_text(data = cases, aes(x = (xmin + xmax) / 2, y = -0.25, label = classe),
              angle = 30, hjust = 1, vjust = 1, size = taille_texte) +
    scale_fill_manual(values = legende_mf$couleurs, guide = "none") +
    coord_cartesian(xlim = c(-0.6, n + 0.2), ylim = c(-3.6, 1.9), expand = FALSE, clip = "off") +
    theme_void()
}

# Assemble une carte (sans sa légende ggplot) et la barre Météo-France
avec_legende <- function(g, hauteur_legende = 0.24) {
  (g + theme(legend.position = "none")) / legende_barre() +
    patchwork::plot_layout(heights = c(1, hauteur_legende)) &
    theme(plot.caption = element_text(margin = margin(t = 14), size = 8.5, colour = "grey40"))
}

# Échelle de couleurs des cartes (la légende ggplot est masquée, remplacée par la barre)
echelle_mf <- function() {
  scale_fill_manual(values = legende_mf$couleurs, drop = FALSE, na.value = "white", guide = "none")
}
source_txt <- "Source: Météo-France, SIM2 mensuel (maille 8 km) — meteo.data.gouv.fr, limites IGN ADMIN EXPRESS,\n DRAAF Hauts-de-France (Srise de Lille)"

# --- Champ interpolé (rendu "interpole") ------------------------------------------------------

# Champ interpolé national, calculé une fois : les centres des mailles SIM2 sont
# rasterisés sur la grille d'origine de 8 km. Les quelques cellules sans valeur
# nécessaires à la continuité du champ sont complétées à partir des voisines, puis
# la grille est interpolée à la résolution d'affichage (1 km par défaut).
# Aucun filtre de lissage n'est appliqué après l'interpolation.
# La résolution de l'information d'origine reste 8 km.
# `interpolation` règle le rééchantillonnage :
#   "bilineaire" : interpolation linéaire entre cellules voisines (par défaut)
#   "cubique"    : interpolation cubique, plus souple visuellement
#   "proche"     : plus proche voisin ; pas de transition entre valeurs.
# Pour conserver strictement les mailles SIM2 de 8 km sans rééchantillonnage,
# utiliser le rendu "mailles" plutôt que le rendu "interpole".
champ_interpole <- function(mailles_val, resolution_m = 1000, interpolation = "bilineaire") {
  methode <- switch(interpolation,
                    bilineaire = "bilinear",
                    cubique    = "cubic",
                    proche     = "near",
                    stop("interpolation : valeurs admises bilineaire, cubique, proche"))
  pts <- st_centroid(mailles_val) |> select(valeur)
 
  # Création d'un raster avec des mailles de 8 km
  r8  <- rast(vect(pts), resolution = 8000)
  
  # Attribution des valeurs SIM2 aux mailles du raster
  # et ajout d'une bordure de 4 mailles autour du raster 
  r8  <- extend(rasterize(vect(pts), r8, field = "valeur"), 4)
  
  # Remplissage des mailles sans valeur (NA)
  # à partir de la moyenne des mailles voisines (3 passages permettent amplement de remplir les petits espaces en bordure maritime surtout)
  for (i in 1:3) {
    r8 <- focal(
      r8,
      w = 3,                    # w = 3 signifie qu'on examine un voisinage de 3 × 3 cellules autour de chaque cellule
      fun = "mean",             # La valeur manquante est remplacée par une moyenne locale
      na.policy = "only",       # le calcul est effectué uniquement pour les cellules qui valent NA
      na.rm = TRUE
    )
  }
  
  # Création de la nouvelle grille, ici des mailles de 1 km
  rf <- rast(
    ext(r8),
    resolution = resolution_m,
    crs = crs(r8)
  )
  
  # Calcul des valeurs de la nouvelle grille par interpolation
  # (bilinéaire, cubique ou plus proche voisin)
  resample(r8, rf, method = methode)
}

# Pour l'affichage en phase avec le choix de l'utilisateur 
# Découpe du champ interpolé sur une emprise et classement en polygones: on obtient donc un raster qui épouse le contour administratif.
decouper_champ <- function(champ, emprise) {
  rl <- crop(champ, vect(emprise), snap = "out") |> 
    mask(vect(emprise))
  as.polygons(classify(rl, rcl = cbind(head(legende_mf$breaks, -1), tail(legende_mf$breaks, -1), 1:7)),
              dissolve = TRUE) |>
    st_as_sf() |>
    rename(cls = 1) |>
    mutate(classe = factor(legende_mf$labels[cls], levels = legende_mf$labels))
}

# --- Cartes -------------------------------------------------------------------------------

# Couche à dessiner pour une emprise, selon le rendu :
#   "mailles"  : carrés de 8 km découpés au contour
#   "communes" : communes coloriées par la maille de leur centre (fond = communes_val)
#   "interpole" : champ interpolé (fond = champ)
couche_rendu <- function(rendu, emprise, mailles_val, fond = NULL) {
  switch(rendu,                                    # switch = if et else if
         mailles  = suppressWarnings(st_intersection(mailles_val |> select(valeur, classe), emprise |> select())),
         communes = fond[st_intersects(st_point_on_surface(fond), st_union(emprise), sparse = FALSE)[, 1], ],
         interpole = decouper_champ(fond, emprise),
         stop("Rendu inconnu : ", rendu))
}

libelle_rendu <- c(mailles = "mailles 8 km", communes = "valeurs rabattues sur les communes",
                   interpole = "champ interpolé")

# Carte d'une région : couche + limites départementales
carte_region <- function(mailles_val, region, deps, titre, sous_titre, rendu = "mailles", fond = NULL) {
  couche <- couche_rendu(rendu, region, mailles_val, fond)
  g <- ggplot() +
    geom_sf(data = couche, aes(fill = classe),
            colour = if (rendu == "communes") "grey55" else NA, linewidth = 0.08) +
    geom_sf(data = deps, fill = NA, colour = "grey15", linewidth = 0.4) +
    geom_sf(data = region, fill = NA, colour = "black", linewidth = 0.7) +
    echelle_mf() + coord_sf(datum = NA) +
    labs(title = titre, subtitle = paste(sous_titre, "—", libelle_rendu[rendu])) +
    theme_carte()
  avec_legende(g) + patchwork::plot_annotation(caption = source_txt)
}

# Carte d'un département
# `assembler = FALSE` renvoie la carte seule (sans barre de légende), pour
# composer plusieurs cartes côte à côte avec une légende commune
carte_departement <- function(mailles_val, dep, titre, sous_titre, rendu = "mailles", fond = NULL,
                              assembler = TRUE) {
  couche <- couche_rendu(rendu, dep, mailles_val, fond)
  g <- ggplot() +
    geom_sf(data = couche, aes(fill = classe),
            colour = if (rendu == "communes") "grey45" else NA, linewidth = 0.12) +
    geom_sf(data = dep, fill = NA, colour = "black", linewidth = 0.7) +
    echelle_mf() + coord_sf(datum = NA) +
    labs(title = titre, subtitle = paste(sous_titre, "—", libelle_rendu[rendu])) +
    theme_carte()
  if (!assembler) return(g)
  avec_legende(g) + patchwork::plot_annotation(caption = source_txt)
}

# Deux cartes côte à côte (ex. SSWI et SPI d'un département) avec une seule barre.
# Les cartes ne gardent que leur étiquette courte (`etiquettes`) ; le titre et le
# sous-titre communs sont portés par la figure assemblée.
cartes_cote_a_cote <- function(g1, g2, titre, sous_titre, etiquettes = c("SSWI", "SPI"),
                               hauteur_legende = 0.28) {
  g1 <- g1 + labs(title = etiquettes[1], subtitle = NULL) + theme(plot.title = element_text(hjust = 0.5, size = 12))
  g2 <- g2 + labs(title = etiquettes[2], subtitle = NULL) + theme(plot.title = element_text(hjust = 0.5, size = 12))
  ((g1 | g2) / legende_barre()) +
    patchwork::plot_layout(heights = c(1, hauteur_legende)) +
    patchwork::plot_annotation(title = titre, subtitle = sous_titre, caption = source_txt,
                               theme = theme(plot.title = element_text(size = 15, face = "plain"),
                                             plot.subtitle = element_text(size = 11, colour = "grey30"),
                                             plot.caption = element_text(margin = margin(t = 10), size = 8.5, colour = "grey40")))
}

# Série mensuelle d'une entité (moyenne des mailles), fond = classes.
# Si `serie` a une colonne `indic`, une courbe par indicateur (SSWI, SPI).
serie_entite <- function(serie, titre, sous_titre) {
  bandes <- data.frame(ymin = head(legende_mf$breaks, -1), ymax = tail(legende_mf$breaks, -1),
                       classe = legende_mf$labels) |>
    mutate(ymin = pmax(ymin, -3), ymax = pmin(ymax, 3))
  multi <- "indic" %in% names(serie)
  g <- ggplot(serie, aes(date, valeur)) +
    geom_rect(data = bandes, aes(xmin = as.Date(-Inf), xmax = as.Date(Inf),
                                 ymin = ymin, ymax = ymax, fill = classe),
              inherit.aes = FALSE, alpha = 0.4) +
    scale_fill_manual(values = legende_mf$couleurs, guide = "none") +
    geom_hline(yintercept = 0, colour = "grey50")
  if (multi) {
    g <- g +
      geom_line(aes(colour = indic, linetype = indic), linewidth = 1) +
      geom_point(aes(colour = indic), size = 2) +
      scale_colour_manual(values = c(SSWI = "black", SPI = "#1d5fa5"), name = NULL) +
      scale_linetype_manual(values = c(SSWI = "solid", SPI = "longdash"), name = NULL)
  } else {
    g <- g + geom_line(colour = "black", linewidth = 1) + geom_point(colour = "black", size = 2)
  }
  g +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    coord_cartesian(ylim = c(-3, 3)) +
    labs(title = titre, subtitle = sous_titre, x = NULL, y = "Indice standardisé", caption = source_txt) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(), plot.title.position = "plot",
          legend.position = if (multi) "top" else "none")
}

# --- Comparaison des départements d'une région -------------------------------------------

# Barres horizontales : moyenne par département, colorée selon la classe
graphe_departements <- function(tab, titre, sous_titre) {
  tab <- tab |> arrange(valeur) |> mutate(nom_dep = factor(nom_dep, levels = nom_dep))
  # Bornes adaptées aux données : au moins ±3, élargies si une valeur dépasse,
  # avec une marge pour l'étiquette (aucune barre n'est supprimée)
  x_min <- min(-3, floor(min(tab$valeur, na.rm = TRUE) * 2) / 2) - 0.4
  x_max <- max( 3, ceiling(max(tab$valeur, na.rm = TRUE) * 2) / 2) + 0.4
  g <- ggplot(tab, aes(x = valeur, y = nom_dep, fill = classe)) +
    geom_vline(xintercept = legende_mf$breaks[2:7], colour = "grey70", linetype = "dashed") +
    geom_vline(xintercept = 0, colour = "grey40") +
    geom_col(width = 0.7, colour = "grey30", linewidth = 0.2) +
    geom_text(aes(label = sprintf("%.2f%s", valeur, ifelse(cas_limite(valeur), " *", "")),
                  x = valeur + ifelse(valeur < 0, -0.08, 0.08),
                  hjust = ifelse(valeur < 0, 1, 0)), size = 3.5) +
    scale_fill_manual(values = legende_mf$couleurs, limits = legende_mf$labels, drop = FALSE, guide = "none") +
    coord_cartesian(xlim = c(x_min, x_max)) +
    scale_x_continuous(breaks = seq(floor(x_min), ceiling(x_max))) +
    labs(title = titre,
         subtitle = paste0(sous_titre, if (any(cas_limite(tab$valeur)))
           "\n* moyenne à moins de 0,05 d'un seuil : classe susceptible de basculer" else ""),
         x = "Indice moyen des mailles", y = NULL) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
          plot.title.position = "plot")
  # Même barre de légende que sous les cartes : les sept classes, toujours affichées
  avec_legende(g, 0.28) + patchwork::plot_annotation(caption = source_txt)
}

# Part des mailles de chaque département dans chaque classe (en %)
parts_classes <- function(mailles_val) {
  mailles_val |> st_drop_geometry() |>
    count(code_dep, classe, .drop = FALSE) |>
    group_by(code_dep) |> mutate(part = round(100 * n / sum(n))) |> ungroup() |>
    select(-n) |>
    tidyr::pivot_wider(names_from = classe, values_from = part, values_fill = 0)
}

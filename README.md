# Sécheresse des sols et des précipitations

Ce projet produit un **rapport HTML sur la sécheresse** à partir des données mensuelles SIM2 de Météo-France.

Deux indicateurs sont utilisés :

- **SSWI** : humidité des sols ;
- **SPI** : précipitations.

Les données Météo-France sont disponibles sur une **maille de 8 km** et pour des périodes de **1, 3, 6 ou 12 mois**.

## Utilisation

Lancer les scripts dans cet ordre :

1. `00_constituer_normales.R`  
   À lancer une seule fois pour calculer les normales 1991-2020 du bilan hydrique.

2. `01_charger_sim_mensuel.R`  
   Télécharge et prépare les dernières données mensuelles Météo-France.

3. `02_rapport_secheresse.qmd`  
   Génère le rapport HTML.

Le fichier `fonctions_cartes.R` contient les fonctions utilisées pour les cartes et les graphiques.

## Paramètres du rapport

Les principaux paramètres se trouvent au début du fichier `.qmd` :

```yaml
annee: 2026
indicateurs: "SSWI,SPI"
echelle: 3
mois: "dernier"
regions: "toutes"
rendu: "interpole"
interpolation: "bilineaire"
resolution_m: 1000
```

### Période

`echelle` peut être `1`, `3`, `6` ou `12` mois.

`mois: "dernier"` utilise le dernier mois disponible.

Pour choisir un mois précis :

```yaml
mois: "2026-08"
```

### Régions

Toute la France :

```yaml
regions: "toutes"
```

Hauts-de-France uniquement :

```yaml
regions: "32"
```

## Cartes

Les données SSWI et SPI utilisées dans le rapport proviennent du modèle **SIM2 de Météo-France**. Elles sont disponibles sur une grille régulière composée de **mailles d'environ 8 km × 8 km**.

Le paramètre `rendu` permet de choisir la façon dont ces données sont représentées sur les cartes.

### Afficher les mailles d'origine

```yaml
rendu: "mailles"
```

Dans ce mode, les valeurs sont représentées directement sur les **mailles SIM2 de 8 km**.

Aucune interpolation spatiale n'est réalisée : la carte reste donc au plus proche de la grille utilisée pour les données d'origine.

Ce rendu permet notamment de bien visualiser la résolution spatiale réelle des données Météo-France.

### Afficher un champ interpolé

```yaml
rendu: "interpole"
interpolation: "bilineaire"
```

Dans ce mode, les valeurs des mailles de 8 km sont utilisées pour construire une **surface continue** destinée à la représentation cartographique.

Une grille plus fine est créée pour l'affichage. Sa résolution est définie avec le paramètre `resolution_m`, exprimé en mètres :

```yaml
resolution_m: 1000
```

correspond à une grille d'affichage de **1 km**, tandis que :

```yaml
resolution_m: 500
```

correspond à une grille d'affichage de **500 m**.

Les valeurs de cette grille sont estimées à partir des valeurs des mailles SIM2 voisines, selon la méthode d'interpolation choisie.

Diminuer `resolution_m` augmente le nombre de cellules et donne un affichage plus fin. Par exemple, sur une largeur de 8 km, on obtient théoriquement 8 × 8 = 64 cellules de 1 km, contre 16 × 16 = 256 cellules de 500 m. Le nombre de cellules à traiter est donc environ quatre fois plus élevé à 500 m qu'à 1 km.

**Important :** une grille d'affichage plus fine ne crée pas une information météorologique plus précise. Que `resolution_m` soit fixé à 1000, 500 ou une autre valeur, la résolution de l'information d'origine reste celle de SIM2, soit environ **8 km**. La résolution choisie concerne uniquement la représentation cartographique.

### Choix de la méthode d'interpolation

Trois méthodes sont proposées.

#### `bilineaire`

```yaml
interpolation: "bilineaire"
```

La valeur d'une cellule est estimée à partir des cellules voisines. Les transitions entre deux valeurs sont progressives.

C'est la méthode utilisée **par défaut dans le rapport** : elle permet d'obtenir une représentation continue tout en restant relativement simple et progressive.

#### `cubique`

```yaml
interpolation: "cubique"
```

L'interpolation cubique utilise un voisinage plus large pour produire des transitions plus douces.

Le résultat est généralement plus lisse visuellement que l'interpolation bilinéaire. En contrepartie, cette méthode peut produire localement des valeurs légèrement supérieures ou inférieures à celles des mailles d'origine.

#### `proche`

```yaml
interpolation: "proche"
```

Chaque cellule de la grille d'affichage reprend la valeur de la maille la plus proche.

Il n'y a donc pas de transition progressive entre deux valeurs. Ce mode conserve davantage l'aspect en blocs de la grille d'origine.

### En pratique

Pour conserver strictement la représentation des données SIM2 :

```yaml
rendu: "mailles"
```

Pour obtenir une carte plus continue et plus facile à lire, avec une grille d'affichage de 1 km :

```yaml
rendu: "interpole"
interpolation: "bilineaire"
resolution_m: 1000
```

Pour tester un affichage plus fin à 500 m, il suffit de modifier :

```yaml
resolution_m: 500
```

sans changer le reste du programme.

L'interpolation est uniquement utilisée pour **améliorer la représentation cartographique**. Elle ne modifie ni la résolution réelle des données Météo-France, ni les valeurs SSWI/SPI utilisées pour les statistiques, les moyennes régionales ou départementales et les graphiques du rapport.

## Fichiers produits

Les données préparées sont enregistrées dans `data/`.

Le rapport final est produit au format **HTML**.

## À savoir

Les limites administratives IGN sont téléchargées automatiquement lors de la première utilisation, puis conservées dans `data/`.

Météo-France peut mettre à jour les valeurs des mois récents lors de la publication d'un nouveau fichier mensuel.

## Principaux packages R

`data.table`, `sf`, `terra`, `dplyr`, `tidyr`, `ggplot2`, `patchwork`, `happign`, `httr2` et `jsonlite`.

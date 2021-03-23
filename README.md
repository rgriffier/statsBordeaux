statsBordeaux package
================
Romain GRIFFIER
23/03/2021

L'objectif de ce package est de regrouper des fonctions et méthodes qui permettent de décrire un jeu de données.
Ce package s'appuit :

-   Sur les fonctions de `base::` de R pour la partie statistique.
-   Sur `ggplot2::` pour la partie graphique.
-   Sur `rmarkdown::` pour le reporting.

Installation
============

Pour installer le package, il faut avoir au prélable installé `devtools`.
Une fois fait, le pakage s'installe grace à la fonction `devtools::install_github()`.

``` r
# install.packages("devtools")
devtools::install_github("rgriffier/statsBordeaux")
```

Mise à jour du package
======================

Le package n'étant pas hébergé sur le CRAN, la mise à jour du package ne peut pas se faire à partir de la fonction classique `update.packages()`. Pour le mettre à jour, vous devez utiliser la fonction `statsBordeaux::updateStatsBordeaux()`.

``` r
statsBordeaux::updateStatsBordeaux()
```

Utilisation du package
======================

Chargement du package
---------------------

``` r
library(statsBordeaux, warn.conflicts = FALSE)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
## tableaux de sortie en français
options(lang.value = 'FR')
```

Chargement des données
----------------------

Ce tutoriel se base sur le dataset `mtcars` inclu dans R.

### Chargement du jeu de données

Dans ce jeu de données, les variables qualitatives (`vs` et `am`) sont codées en 0/1.

``` r
## chargement du jeu de donnée
data(mtcars)

## affichage de la structure du data.frame
str(mtcars)
```

    ## 'data.frame':    32 obs. of  11 variables:
    ##  $ mpg : num  21 21 22.8 21.4 18.7 18.1 14.3 24.4 22.8 19.2 ...
    ##  $ cyl : num  6 6 4 6 8 6 8 4 4 6 ...
    ##  $ disp: num  160 160 108 258 360 ...
    ##  $ hp  : num  110 110 93 110 175 105 245 62 95 123 ...
    ##  $ drat: num  3.9 3.9 3.85 3.08 3.15 2.76 3.21 3.69 3.92 3.92 ...
    ##  $ wt  : num  2.62 2.88 2.32 3.21 3.44 ...
    ##  $ qsec: num  16.5 17 18.6 19.4 17 ...
    ##  $ vs  : num  0 0 1 1 0 1 0 1 1 1 ...
    ##  $ am  : num  1 1 1 0 0 0 0 0 0 0 ...
    ##  $ gear: num  4 4 4 3 3 3 3 4 4 4 ...
    ##  $ carb: num  4 4 1 1 2 1 4 2 2 4 ...

### Chargement des labels des modalitées des différentes variables qualitatives

Pour les variables qualitatives, on charge un data.frame de 3 colonnes :
- `Variable` : nom de la variable, tel que renseigné dans le dataset.
- `Modality` : nombre représentant la modalité, tel que renseigné dans le dataset.
- `Label` : label de la modalité, tel que souhaité pour le rendu des résultats.

``` r
## création du data.frame contenant les labels des différentes modalitées des variables qualitatives
var_QL <- data.frame(Variable = c("vs", "vs", "am", "am"), 
                     Modality = c(0, 1, 0, 1),
                     Label = c("V-shaped", "Straight", "Automatic", "Manual"))
var_QL
```

    ##   Variable Modality     Label
    ## 1       vs        0  V-shaped
    ## 2       vs        1  Straight
    ## 3       am        0 Automatic
    ## 4       am        1    Manual

Data management
---------------

### ÉTAPE 1 : vérification du contenu du data.frame contenant les données à analyser

On vérifie que le data.frame du jeux de données ne contient que des nombre grace à la fonction `statsBordeaux::checkNotDigitInDataframe()`.
Dans le cas où la fonction `statsBordeaux::checkNotDigitInDataframe()` renvoie `FALSE`, il est possible de récupérer un data.frame contenant la position des cellules contenant des éléments non-numériques en ajoutant le paramètre `returnError = TRUE`. La position de ces cellules est donnée au format Excel (ex : A2).

``` r
## vérification que le jeu de donnée ne contient que des chiffres
statsBordeaux::checkNotDigitInDataframe(data = mtcars)
```

### ÉTAPE 2 : labélisation des modalité des variables qualiatives

Comme le data.frame d'entrée ne contient que des nombres, il faut labéliser les modalités des variables qualitatives.
On effectue cette labélisation grâce à la fonction `statsBordeaux::setLabelToFactorLevels()` à partir du data.frame `labels` créé précédemment.
Cette fonction gère à la fois :

-   La conversion des variables présentes dans le data.frame `labels` en tant que `factor`.
-   La labélisation des différentes modalitées de ces variables.

La fonction vérifie que les variables du fichier des labels sont bien dans le data.frame des données. Le cas échant, elle retourne une erreur identifiant le ou les variables présentes dans le data.frame des labels mais absente du data.frame des données.

``` r
## association à chaque modalité des variables qualitatives du label correspondant et conversion de ces variables en factor
labelledData <- statsBordeaux::setLabelToFactorLevels(mtcars, var_QL)
head(labelledData)
```

    ##                    mpg cyl disp  hp drat    wt  qsec       vs        am gear
    ## Mazda RX4         21.0   6  160 110 3.90 2.620 16.46 V-shaped    Manual    4
    ## Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02 V-shaped    Manual    4
    ## Datsun 710        22.8   4  108  93 3.85 2.320 18.61 Straight    Manual    4
    ## Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44 Straight Automatic    3
    ## Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02 V-shaped Automatic    3
    ## Valiant           18.1   6  225 105 2.76 3.460 20.22 Straight Automatic    3
    ##                   carb
    ## Mazda RX4            4
    ## Mazda RX4 Wag        4
    ## Datsun 710           1
    ## Hornet 4 Drive       1
    ## Hornet Sportabout    2
    ## Valiant              1

### ÉTAPE 3 (optionnelle) : labellisation des variables

Il est possible d'associer à chaque variable du data.frame contenant le jeu de donnée un label (afin d'améliorer la sortie des résultats) grace à la fonction `statsBordeaux::setLabelToVariable()`. Le label des variables est contenu dans un attribut `var_label`.

``` r
## création du data.frame contenant le label de chaques variables
var_label <- data.frame(Variable = c("mpg", "cyl", "disp", "hp", "drat", "wt",
                                     "qsec", "vs", "am", "gear", "carb"),
                        Label = c("Miles/(US) gallon", "Number of cylinders", "Displacement (cu.in.)",
                                  "Gross horsepower ", "Rear axle ratio", "Weight (1000 lbs)",
                                  "1/4 mile time", "Engine", "Transmission", "Number of forward gears",
                                  "Number of carburetors"))

## labélisation des variables du data.frame
labelledData <- statsBordeaux::setLabelToVariable(data = labelledData,
                                                  varLabel = var_label)

## fonction pour récupérer les labels d'un data.frame labellisé
statsBordeaux::getVarLabel(data = labelledData)
```

    ##                       mpg                       cyl                      disp 
    ##       "Miles/(US) gallon"     "Number of cylinders"   "Displacement (cu.in.)" 
    ##                        hp                      drat                        wt 
    ##       "Gross horsepower "         "Rear axle ratio"       "Weight (1000 lbs)" 
    ##                      qsec                        vs                        am 
    ##           "1/4 mile time"                  "Engine"            "Transmission" 
    ##                      gear                      carb 
    ## "Number of forward gears"   "Number of carburetors"

Il est également possible de labéliser une variable avec la fonction `statsBordeaux::addLabelToVariable()`.

``` r
## labélisation manuelle d'une variable créé avec dplyr
labelledData <- labelledData %>%
  dplyr::mutate(wt_kg = wt/0.4536) %>%
  statsBordeaux::addLabelToVariable(wt_kg = 'Weight (1000 Kg)')

## fonction pour récupérer les labels d'un data.frame labellisé
statsBordeaux::getVarLabel(data = labelledData)
```

    ##                       mpg                       cyl                      disp 
    ##       "Miles/(US) gallon"     "Number of cylinders"   "Displacement (cu.in.)" 
    ##                        hp                      drat                        wt 
    ##       "Gross horsepower "         "Rear axle ratio"       "Weight (1000 lbs)" 
    ##                      qsec                        vs                        am 
    ##           "1/4 mile time"                  "Engine"            "Transmission" 
    ##                      gear                      carb                     wt_kg 
    ## "Number of forward gears"   "Number of carburetors"        "Weight (1000 Kg)"

### ÉTAPE 4 : vérification de la structure finale du fichier à analyser

On vérifie la structure du data.frame contenant le dataset.
Toutes les variables à analyser doivent être de classe `numeric` (`integer` ou `double`) ou de classe `factor`.

``` r
## description des méta-données
list_variableFormat <- statsBordeaux::describeMetadata(data = labelledData)
list_variableFormat
```

    ##      VAR               VAR_LABEL DATA_TYPE LEVELS_NB             LEVELS
    ## 1    mpg       Miles/(US) gallon   numeric        NA               <NA>
    ## 2    cyl     Number of cylinders   numeric        NA               <NA>
    ## 3   disp   Displacement (cu.in.)   numeric        NA               <NA>
    ## 4     hp       Gross horsepower    numeric        NA               <NA>
    ## 5   drat         Rear axle ratio   numeric        NA               <NA>
    ## 6     wt       Weight (1000 lbs)   numeric        NA               <NA>
    ## 7   qsec           1/4 mile time   numeric        NA               <NA>
    ## 8     vs                  Engine    factor         2 V-shaped, Straight
    ## 9     am            Transmission    factor         2  Automatic, Manual
    ## 10  gear Number of forward gears   numeric        NA               <NA>
    ## 11  carb   Number of carburetors   numeric        NA               <NA>
    ## 12 wt_kg        Weight (1000 Kg)   numeric        NA               <NA>
    ##    N_AVAILABLE PROP_AVAILABLE N_MISSING PROP_MISSING N_INF PROP_INF UNIQUE
    ## 1           32              1         0            0     0        0     25
    ## 2           32              1         0            0     0        0      3
    ## 3           32              1         0            0     0        0     27
    ## 4           32              1         0            0     0        0     22
    ## 5           32              1         0            0     0        0     22
    ## 6           32              1         0            0     0        0     29
    ## 7           32              1         0            0     0        0     30
    ## 8           32              1         0            0    NA       NA      2
    ## 9           32              1         0            0    NA       NA      2
    ## 10          32              1         0            0     0        0      3
    ## 11          32              1         0            0     0        0      6
    ## 12          32              1         0            0     0        0     29

Statistiques descriptives
-------------------------

### Statistiques descriptives globales

#### Utilisation de la fonction pipe `%>%`

La fonction `statsBordeaux::describeDataFrame()` permet de décrire les variables qualitatives et quantitatives d'un data.frame. Elle peut s'utiliser avec les pipes `%>%` (package `dplyr`).

``` r
# chargement de dplyr
library(dplyr)

# statistique descriptive sur l'ensemble du data.frame
description <- labelledData %>%
  statsBordeaux::describeDataFrame()

description[c(1:5, 36:39), ]
```

    ##             Variable             Modality          Description
    ## 1  Miles/(US) gallon                 <NA>                 <NA>
    ## 2               <NA>                 <NA>             N (d.m.)
    ## 3               <NA>                 <NA> Moyenne (Écart-type)
    ## 4               <NA>                 <NA>    Médiane [Q1 ; Q3]
    ## 5               <NA>                 <NA>            Min ; Max
    ## 36            Engine                 <NA>                 <NA>
    ## 37              <NA> Toutes les modalités         N (d.m. ; %)
    ## 38              <NA>             V-shaped                N (%)
    ## 39              <NA>             Straight                N (%)
    ##                     All
    ## 1                  <NA>
    ## 2                32 (0)
    ## 3        20,091 (6,027)
    ## 4  19,2 [15,425 ; 22,8]
    ## 5           10,4 ; 33,9
    ## 36                 <NA>
    ## 37           32 (0 ; 0)
    ## 38           18 (56,25)
    ## 39           14 (43,75)

De la même manière, il est possible de réaliser une description graphique des données avec la fonction `statsBordeaux::getGraphicalDescription()`

``` r
# statistique graphique sur l'ensemble du data.frame
graphicDescription <- labelledData %>%
  statsBordeaux::getGraphicalDescription()

## Variable quantitative : BOX PLOT
graphicDescription[[1]]
```

    ## $mpg

![](README_files/figure-markdown_github/unnamed-chunk-12-1.png)

``` r
## Variable qualitative : BAR PLOT
graphicDescription[[8]]
```

    ## $vs

![](README_files/figure-markdown_github/unnamed-chunk-12-2.png)

#### Utilisateurs avancés, utilisation pas à pas

On créé un tableau de sortie vierge à partir de la fonction `statsBordeaux::createOutput()`. C'est ce dernier qui va contenir la description de nos variables.
On réalise les statistiques descriptives des données avec les fonctions `statsBordeaux::statsQT()` et `statsBordeaux::statsQL()` (en fonction de la nature quantitative ou qualitative de la variable à décrire).

``` r
## création du tableau de sortie vierge
description <-  statsBordeaux::createOutput()

## boucle qui va traiter chaque colonne du data.frame les unes après les autres
for(i in 1:ncol(labelledData)){
  # cas où la colonne en cours est de type quantitative
  if(is.numeric(labelledData[, i])){
    description <- statsBordeaux::statsQT(output = description,
                                          data = labelledData,
                                          variable = colnames(labelledData)[i])
  }
  # cas où la colonne en cours est de type qualitative
  else if(is.factor(labelledData[, i])){
    description <- statsBordeaux::statsQL(output = description,
                                          data = labelledData,
                                          variable = colnames(labelledData)[i])
  }
  # autre cas non pris en charge
  else {
    print(paste0("Variable '", colnames(labelledData[i]), "' non décrite (", class(labelledData[, i]), ")"))
  }
}

head(description, n = 5)
```

    ##            Variable Modality          Description                  All
    ## 1 Miles/(US) gallon     <NA>                 <NA>                 <NA>
    ## 2              <NA>     <NA>             N (d.m.)               32 (0)
    ## 3              <NA>     <NA> Moyenne (Écart-type)       20,091 (6,027)
    ## 4              <NA>     <NA>    Médiane [Q1 ; Q3] 19,2 [15,425 ; 22,8]
    ## 5              <NA>     <NA>            Min ; Max          10,4 ; 33,9

### Statistiques descriptives en fonction d'une variable qualitative de groupe

#### Utilisation de la fonction pipe `%>%`

La fonction `statsBordeaux::describeDataFrame()` permet également de réaliser les statistiques en fonction d'unevariable qualitative de groupe. Il suffit pour celà de renseigner le paramètre `group`. On peut dans ce cas rajouter le paramètre `all = TRUE` afin afficher en plus une description globale des variables.

``` r
# statistique comparative sur l'ensemble du data.frame
comp <- labelledData %>%
  statsBordeaux::describeDataFrame(group = "vs",
                                   all = TRUE)

comp[c(1:5, 36:39), ]
```

    ##             Variable             Modality          Description
    ## 1  Miles/(US) gallon                 <NA>                 <NA>
    ## 2               <NA>                 <NA>             N (d.m.)
    ## 3               <NA>                 <NA> Moyenne (Écart-type)
    ## 4               <NA>                 <NA>    Médiane [Q1 ; Q3]
    ## 5               <NA>                 <NA>            Min ; Max
    ## 36      Transmission                 <NA>                 <NA>
    ## 37              <NA> Toutes les modalités         N (d.m. ; %)
    ## 38              <NA>            Automatic                N (%)
    ## 39              <NA>               Manual                N (%)
    ##                     All                V-shaped             Straight
    ## 1                  <NA>                    <NA>                 <NA>
    ## 2                32 (0)                  18 (0)               14 (0)
    ## 3        20,091 (6,027)          16,617 (3,861)       24,557 (5,379)
    ## 4  19,2 [15,425 ; 22,8] 15,65 [14,775 ; 19,075] 22,8 [21,4 ; 29,625]
    ## 5           10,4 ; 33,9               10,4 ; 26          17,8 ; 33,9
    ## 36                 <NA>                    <NA>                 <NA>
    ## 37           32 (0 ; 0)              18 (0 ; 0)           14 (0 ; 0)
    ## 38          19 (59,375)             12 (66,667)               7 (50)
    ## 39          13 (40,625)              6 (33,333)               7 (50)

De la même manière, il est possible de réaliser une description graphique des données en fonction d'une variable qualitative de groupe en rajoutant le paramètre `group` à la fonction `statsBordeaux::getGraphicalDescription()`

``` r
# statistique graphique sur l'ensemble du data.frame
graphicDescription <- labelledData %>%
  statsBordeaux::getGraphicalDescription(group = "vs")

## Variable quantitative : BOX PLOT
graphicDescription[[1]]
```

    ## $mpg

![](README_files/figure-markdown_github/unnamed-chunk-15-1.png)

``` r
## Variable qualitative : BAR PLOT
graphicDescription[[9]]
```

    ## $gear

![](README_files/figure-markdown_github/unnamed-chunk-15-2.png)

#### Utilisateurs avancés, utilisation pas à pas

On réalise les statistiques descriptives des données en fonction d'une variable qualitative de comparaison en rajoutant un paramètre `group` aux fonction `statsBordeaux::statsQL()` et `statsBordeaux::statsQT()`. On peut dans ce cas rajouter le paramètre `all = TRUE` afin afficher une description globale des variables.

``` r
## création du tableau de sortie vierge
comparaison <-  statsBordeaux::createOutput()

## on défini la variable de groupe
group <- "vs"

## boucle qui va traiter chaque colonne du data.frame les unes après les autres
for(i in 1:ncol(labelledData)){
  # on ne réalise pas la comparaison de la variable group avec elle-même
  if(colnames(labelledData)[i] != group){
    # cas où la colonne en cours est de type quantitative
    if(is.numeric(labelledData[, i])){
      comparaison <- statsBordeaux::statsQT(output = comparaison,
                                            data = labelledData,
                                            variable = colnames(labelledData)[i],
                                            group = group,
                                            all = TRUE)
    }
    # cas où la colonne en cours est de type qualitative
    else if(is.factor(labelledData[, i])){
      comparaison <- statsBordeaux::statsQL(output = comparaison,
                                            data = labelledData,
                                            variable = colnames(labelledData)[i],
                                            group = group,
                                            all = TRUE)
    }
    # autre cas non pris en charge
    else {
      print(paste0("Variable '", colnames(labelledData[i]), "' non décrite (", class(labelledData[, i]), ")"))
    }
  }
}

head(comparaison, n = 5)
```

    ##            Variable Modality          Description                  All
    ## 1 Miles/(US) gallon     <NA>                 <NA>                 <NA>
    ## 2              <NA>     <NA>             N (d.m.)               32 (0)
    ## 3              <NA>     <NA> Moyenne (Écart-type)       20,091 (6,027)
    ## 4              <NA>     <NA>    Médiane [Q1 ; Q3] 19,2 [15,425 ; 22,8]
    ## 5              <NA>     <NA>            Min ; Max          10,4 ; 33,9
    ##                  V-shaped             Straight
    ## 1                    <NA>                 <NA>
    ## 2                  18 (0)               14 (0)
    ## 3          16,617 (3,861)       24,557 (5,379)
    ## 4 15,65 [14,775 ; 19,075] 22,8 [21,4 ; 29,625]
    ## 5               10,4 ; 26          17,8 ; 33,9

Statistiques comparatives
-------------------------

-   Comparaison de fréquences :
    -   Si tous les effectifs théoriques sont supérieurs ou égaux à 5, un Chi² est réalisé.
    -   Si un des effectifs théoriques est inférieur à 5 mais qu'ils sont tous supérieurs ou égaux à 3, un Chi² avec correction de Yates est réalisé.
    -   Sinon un test de Fisher est réalisé.
-   Comparaison de paramètre quantitatifs :
    -   Cas où la variable de groupe est composée de deux modalitées :
        -   Si tous les effectifs sont supérieurs ou égaux à 30, l'hypothèse de normalité est considérée comme vraie. Un test de Student (ou un test de Welsh en fonction de l'égalité des variance entre les groupes) est réalisé. Dans le cas d'absence de variabilité au sein d'un ou des deux groupes, aucun test n'est réalisé.
        -   Si un des effectifs est inférieur à 30, l'hypothèse de normalité n'est pas considérée comme vraie. Un test de Wilcoxon est réalisé.
    -   Cas où la variable de groupe est composée de plus de deux modalitées :
        -   Si tous les effectifs sont supérieurs ou égaux à 30, l'hypothèse de normalité est considérée vraie. Un test d'ANOVA est réalisé.
        -   Si un des effectifs est inférieur à 30, l'hypothèse de normalité n'est pas considérée vraie. Un test non-paramétrique de Kruskal-Wallis est réalisé.

#### Utilisation de la fonction pipe `%>%`

On réalise les statistiques comparatives des données en fonction d'une variable qualitative de comparaison en associant au paramètre `group` le paramètre `p_value = TRUE` dans la fonction `statsBordeaux::describeDataFrame()`.

``` r
# statistique comparative sur l'ensemble du data.frame
comp <- labelledData %>%
  statsBordeaux::describeDataFrame(group = "vs",
                                   p_value = TRUE)
comp[c(1:5, 36:39), ]
```

    ##             Variable             Modality          Description
    ## 1  Miles/(US) gallon                 <NA>                 <NA>
    ## 2               <NA>                 <NA>             N (d.m.)
    ## 3               <NA>                 <NA> Moyenne (Écart-type)
    ## 4               <NA>                 <NA>    Médiane [Q1 ; Q3]
    ## 5               <NA>                 <NA>            Min ; Max
    ## 36      Transmission                 <NA>                 <NA>
    ## 37              <NA> Toutes les modalités         N (d.m. ; %)
    ## 38              <NA>            Automatic                N (%)
    ## 39              <NA>               Manual                N (%)
    ##                   V-shaped             Straight             Test p-value
    ## 1                     <NA>                 <NA> Test de Wilcoxon  <0,001
    ## 2                   18 (0)               14 (0)             <NA>    <NA>
    ## 3           16,617 (3,861)       24,557 (5,379)             <NA>    <NA>
    ## 4  15,65 [14,775 ; 19,075] 22,8 [21,4 ; 29,625]             <NA>    <NA>
    ## 5                10,4 ; 26          17,8 ; 33,9             <NA>    <NA>
    ## 36                    <NA>                 <NA>     Test du Chi²   0,341
    ## 37              18 (0 ; 0)           14 (0 ; 0)             <NA>    <NA>
    ## 38             12 (66,667)               7 (50)             <NA>    <NA>
    ## 39              6 (33,333)               7 (50)             <NA>    <NA>

#### Utilisateurs avancés, utilisation pas à pas

On réalise les statistiques comparatives des données en fonction d'une variable qualitative de comparaison en associant au paramètre `group` le paramètre `p_value = TRUE` dans les fonctions `statsBordeaux::statsQL()` et `statsBordeaux::statsQT()`. Le test réalisé est choisi en fonction des conditions d'application des différents tests.

``` r
## création du tableau de sortie vierge
statsComparatives <- statsBordeaux::createOutput()

## on défini la variable de groupe
group <- "vs"

## boucle qui va traiter chaque colonne du data.frame les unes après les autres
for(i in 1:ncol(labelledData)){
  # on ne réalise pas la comparaison de la variable group avec elle-même
  if(colnames(labelledData)[i] != group){
    # cas où la colonne en cours est de type quantitative
    if(is.numeric(labelledData[, i])){
      statsComparatives <- statsBordeaux::statsQT(output = statsComparatives,
                                                  data = labelledData,
                                                  variable = colnames(labelledData)[i],
                                                  group = group,
                                                  p_value = TRUE)
    }
    # cas où la colonne en cours est de type qualitative
    else if(is.factor(labelledData[, i])){
      statsComparatives <- statsBordeaux::statsQL(output = statsComparatives,
                                                  data = labelledData,
                                                  variable = colnames(labelledData)[i],
                                                  group = group,
                                                  p_value = TRUE)
    }
    # autre cas non pris en charge
    else {
      paste0(colnames(labelledData)[i], " n'a pas été décrite du fait de son type (", class(labelledData[, i]), ")")
    }
  }
}

head(statsComparatives, n = 5)
```

    ##            Variable Modality          Description                V-shaped
    ## 1 Miles/(US) gallon     <NA>                 <NA>                    <NA>
    ## 2              <NA>     <NA>             N (d.m.)                  18 (0)
    ## 3              <NA>     <NA> Moyenne (Écart-type)          16,617 (3,861)
    ## 4              <NA>     <NA>    Médiane [Q1 ; Q3] 15,65 [14,775 ; 19,075]
    ## 5              <NA>     <NA>            Min ; Max               10,4 ; 26
    ##               Straight             Test p-value
    ## 1                 <NA> Test de Wilcoxon  <0,001
    ## 2               14 (0)             <NA>    <NA>
    ## 3       24,557 (5,379)             <NA>    <NA>
    ## 4 22,8 [21,4 ; 29,625]             <NA>    <NA>
    ## 5          17,8 ; 33,9             <NA>    <NA>

Reporting avec `rmarkdown`
--------------------------

Dans un projet R Markdown `Knit to HTML`, vous pouvez ajouter facilement un tableau de donnée avec la fonction `statsBordeaux::addKable()`. Cette dernière génère un **tableau HTML**. La gestion des titre et des numéro de tableau se fait au niveau du document et non dans la fonction `statsBordeaux::addKable()`.

La génération de .pdf en LaTeX n'est actuellement pas prise en charge.

``` r
statsBordeaux::addKable(description)
```

Fonctions avancées
------------------

### Tests de normalité

Il est souvent interessant de s'interesser à la distribution des variables quantitatives afin de connaitre la meilleure façon de les décrire (utilisation de la moyenne ou médiane par exemple) et les test statistiques à appliquer.

La distribution que l'on va le plus souvent vouloir évaluer est la distribution normale.
La fonction `statsBordeaux::checkNormality()` permet d'évaluer la normalité d'une variable de manière graphique et à l'aide de deux test statistiques (*Kolmogorov-Smirnov* ou *Shapiro-Wilk*).

``` r
## évaluation graphique de la distribution de la variable 'mpg'
statsBordeaux::checkNormality(data = labelledData, variable = "mpg")
```

    ## [[1]]
    ## [[1]]$mpg

<img src="README_files/figure-markdown_github/unnamed-chunk-20-1.png" style="display: block; margin: auto;" />

``` r
## évaluation graphique et avec un test de Shapiro-Wilk de la distribution de la variable 'mpg'
statsBordeaux::checkNormality(data = labelledData, variable = "mpg", p_value = TRUE, method = 'Shapiro')
```

    ## [[1]]
    ## [[1]]$mpg

<img src="README_files/figure-markdown_github/unnamed-chunk-20-2.png" style="display: block; margin: auto;" />

``` r
## évaluation graphique et avec un test de Shapiro-Wilk de la distribution de la variable 'mpg' dans chacun des sous-groupe de 'vs'
statsBordeaux::checkNormality(data = labelledData, variable = "mpg", group = "vs", p_value = TRUE, method = 'Shapiro')
```

    ## [[1]]
    ## [[1]]$mpg

<img src="README_files/figure-markdown_github/unnamed-chunk-20-3.png" style="display: block; margin: auto;" />

### Gestion des 'non-applicables'

La gestion des 'non-applicables' fait intervenir les fonctions `statsBordeaux::setNonApp()` et `statsBordeaux::manageNotApplicable()`:

-   La fonction `statsBordeaux::setNonApp()` permet de taguer les données non applicables en fonction d'une condition.
-   La fonction `statsBordeaux::manageNotApplicable()` permet de gérer les tags générés à l'étape précédente.
    -   Elle prend 2 paramètres d'entrée :
        -   `df`, un data.frame, contenant le jeu de donnée à décrire.
        -   `notApplicableChar`, un vecteur de longueur 1 contenant la façon dont sont représentés les 'non-applicables' dans les données.
    -   Elle renvoie une liste de deux éléments :
        1.  Le data.frame d'entrée duquel ont été ôtés les 'non-applicable'
        2.  Une liste de vecteurs logiques :
            -   `TRUE` signifie que la ligne en cours, pour la variable considérée, doit être comptabilisée.
            -   `FALSE` signifie que la ligne en cours, pour la variable considérée, ne doit pas être comptabilitée ('non-applicable').

``` r
data(mtcars)

var_QL <- data.frame(Variable = c("vs", "vs", "am", "am"),
                     Modality = c(0, 1, 0, 1),
                     Label = c("V-shaped", "Straight", "Automatic", "Manual"))

var_label <- data.frame(Variable = c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb"),
                            Label = c("Miles/(US) gallon", "Number of cylinders", "Displacement (cu.in.)", "Gross horsepower ",
                                      "Rear axle ratio", "Weight (1000 lbs)", "1/4 mile time", "Engine", "Transmission",
                                      "Number of forward gears", "Number of carburetors"))

## on créé une colonne qui contient le nombre de vitesse dans le cas d'une transmission manuelle
## en cas de transmission automatique, une donée manquante est attendue, on tague alors la données comme 'NonApp'
mtcars <- mtcars %>%
  dplyr::mutate(
    nSpeed = c(5, 6, 5, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 6, 6, 6, NA, NA, NA, NA, NA, 5, NA, 6, 5, 6, 6, 5)
  ) %>%
  setNonApp('nSpeed', condition = function(.){.$am == 0})

## on utilise la fonction manageNotApplicable() pour gérer les non-applicable,
## représentés ici par des 'NonApp'
applicable <- statsBordeaux::manageNotApplicable(data = mtcars)

## on récupère le data.frame sans les 'non-applicables'
mtcars <- applicable[[1]]

## on récupère la liste de vecteurs logiques contenant les lignes qui doivent être analysées.
applicable <- applicable[[2]]

## labellisation automatique des variable et des modalités des variables qualitatives
labelledData <- statsBordeaux::setLabelToFactorLevels(data = mtcars, var_QL)
labelledData <- statsBordeaux::setLabelToVariable(data = labelledData, varLabel = var_label)

# labellisation manuelle de la variable créée
labelledData <- labelledData %>%
  dplyr::mutate(nSpeed = dplyr::recode_factor(nSpeed, "5" = "5 vitesses", "6" = "6 vitesses")) %>%
  statsBordeaux::addLabelToVariable(nSpeed = 'Nombre de vitesses (boite manuelle')

##----------------

descriptionNonAvailable <- labelledData %>% 
  statsBordeaux::describeDataFrame(applicable = applicable)

descriptionNonAvailable[c(40:44, 55:57), ]
```

    ##                   Variable             Modality  Description            All
    ## 40            Transmission                 <NA>         <NA>           <NA>
    ## 41                    <NA> Toutes les modalités N (d.m. ; %)     32 (0 ; 0)
    ## 42                    <NA>            Automatic        N (%)    19 (59,375)
    ## 43                    <NA>               Manual        N (%)    13 (40,625)
    ## 44 Number of forward gears                 <NA>         <NA>           <NA>
    ## 55                    <NA> Toutes les modalités N (d.m. ; %) 12 (1 ; 7,692)
    ## 56                    <NA>           5 vitesses        N (%)     5 (41,667)
    ## 57                    <NA>           6 vitesses        N (%)     7 (58,333)

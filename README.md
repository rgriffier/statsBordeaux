Stats bordeaux package
================
Romain GRIFFIER
26/01/2020

L'objectif de ce package est de regrouper des fonctions et méthodes qui permettent de décrire un jeu de données.
Ce package s'appuit :

-   Sur les fonctions de `base::` de R pour la partie statistique.
-   Sur `ggplot2::` pour la partie graphique.
-   Sur `rmarkdown::` et `officer::` pour le reporting.

Installation
============

Pour installer le package, il faut avoir au prélable installé `devtools`.
Une fois fait, le pakage s'installe grace à la fonction `devtools::install_github()`.

``` r
# install.packages("devtools")
# devtools::install_github("rgriffier/statsBordeaux")
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
```

    ## Warning: replacing previous import 'flextable::footnote' by
    ## 'kableExtra::footnote' when loading 'statsBordeaux'

    ## Warning: replacing previous import 'dplyr::group_rows' by
    ## 'kableExtra::group_rows' when loading 'statsBordeaux'

    ## Warning: replacing previous import 'flextable::as_image' by
    ## 'kableExtra::as_image' when loading 'statsBordeaux'

    ## Warning: replacing previous import 'dplyr::rename' by 'reshape::rename' when
    ## loading 'statsBordeaux'

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
labels <- data.frame(Variable = c("vs", "vs", "am", "am"), 
                     Modality = c(0, 1, 0, 1),
                     Label = c("V-shaped", "Straight", "Automatic", "Manual"))
labels
```

<script data-pagedtable-source type="application/json">
{"columns":[{"label":["Variable"],"name":[1],"type":["fctr"],"align":["left"]},{"label":["Modality"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["Label"],"name":[3],"type":["fctr"],"align":["left"]}],"data":[{"1":"vs","2":"0","3":"V-shaped"},{"1":"vs","2":"1","3":"Straight"},{"1":"am","2":"0","3":"Automatic"},{"1":"am","2":"1","3":"Manual"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

Data management
---------------

### ÉTAPE 1 : vérification du contenu du data.frame contenant les données à analyser

On vérifie que le data.frame du jeux de données ne contient que des nombre grace à la fonction `statsBordeaux::checkNotDigitInDataframe()`.
Dans le cas où la fonction `statsBordeaux::checkNotDigitInDataframe()` renvoie `FALSE`, il est possible de récupérer un data.frame contenant la position des cellules contenant des éléments non-numériques en ajoutant le paramètre `returnError = TRUE`. La position de ces cellules est donnée au format Excel (ex : A2).

``` r
## vérification que le jeu de donnée ne contient que des chiffres
statsBordeaux::checkNotDigitInDataframe(mtcars)
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
labelledData <- statsBordeaux::setLabelToFactorLevels(mtcars, labels)
head(labelledData)
```

<script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["mpg"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["cyl"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["disp"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["hp"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["drat"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["wt"],"name":[6],"type":["dbl"],"align":["right"]},{"label":["qsec"],"name":[7],"type":["dbl"],"align":["right"]},{"label":["vs"],"name":[8],"type":["fctr"],"align":["left"]},{"label":["am"],"name":[9],"type":["fctr"],"align":["left"]},{"label":["gear"],"name":[10],"type":["dbl"],"align":["right"]},{"label":["carb"],"name":[11],"type":["dbl"],"align":["right"]}],"data":[{"1":"21.0","2":"6","3":"160","4":"110","5":"3.90","6":"2.620","7":"16.46","8":"V-shaped","9":"Manual","10":"4","11":"4","_rn_":"Mazda RX4"},{"1":"21.0","2":"6","3":"160","4":"110","5":"3.90","6":"2.875","7":"17.02","8":"V-shaped","9":"Manual","10":"4","11":"4","_rn_":"Mazda RX4 Wag"},{"1":"22.8","2":"4","3":"108","4":"93","5":"3.85","6":"2.320","7":"18.61","8":"Straight","9":"Manual","10":"4","11":"1","_rn_":"Datsun 710"},{"1":"21.4","2":"6","3":"258","4":"110","5":"3.08","6":"3.215","7":"19.44","8":"Straight","9":"Automatic","10":"3","11":"1","_rn_":"Hornet 4 Drive"},{"1":"18.7","2":"8","3":"360","4":"175","5":"3.15","6":"3.440","7":"17.02","8":"V-shaped","9":"Automatic","10":"3","11":"2","_rn_":"Hornet Sportabout"},{"1":"18.1","2":"6","3":"225","4":"105","5":"2.76","6":"3.460","7":"20.22","8":"Straight","9":"Automatic","10":"3","11":"1","_rn_":"Valiant"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

### ÉTAPE 3 (optionnelle) : labellisation des variables

Il est possible d'associer à chaque variable du data.frame contenant le jeu de donnée un label (afin d'améliorer la sortie des résultats) grace à la fonction `statsBordeaux::setLabelToVariable()`. Le label des variables est contenu dans un attribut `var_label`.

``` r
## création du data.frame contenant le label de chaques variables
labelVariable <- data.frame(Variable = c("mpg", "cyl", "disp", "hp", "drat", "wt",
                                         "qsec", "vs", "am", "gear", "carb"),
                            Label = c("Miles/(US) gallon", "Number of cylinders", "Displacement (cu.in.)",
                                      "Gross horsepower ", "Rear axle ratio", "Weight (1000 lbs)",
                                      "1/4 mile time", "Engine", "Transmission", "Number of forward gears",
                                      "Number of carburetors"))

## labélisation des variables du data.frame
labelledData <- statsBordeaux::setLabelToVariable(labelledData, labelVariable)

## fonction pour récupérer les labels d'un data.frame labellisé
statsBordeaux::getVarLabel(labelledData)
```

    ##                       mpg                       cyl                      disp 
    ##       "Miles/(US) gallon"     "Number of cylinders"   "Displacement (cu.in.)" 
    ##                        hp                      drat                        wt 
    ##       "Gross horsepower "         "Rear axle ratio"       "Weight (1000 lbs)" 
    ##                      qsec                        vs                        am 
    ##           "1/4 mile time"                  "Engine"            "Transmission" 
    ##                      gear                      carb 
    ## "Number of forward gears"   "Number of carburetors"

``` r
## attribut stockant le label d'une variable
nomVariable <- attributes(labelledData[, 1])$var_label
```

### ÉTAPE 4 : vérification de la structure finale du fichier à analyser

On vérifie la structure du data.frame contenant le dataset.
Toutes les variables à analyser doivent être de classe `numeric` (`integer` ou `double`) ou de classe `factor`.

``` r
## description des méta-données
list_variableFormat <- statsBordeaux::describeMetadata(labelledData)
list_variableFormat
```

<script data-pagedtable-source type="application/json">
{"columns":[{"label":["VAR"],"name":[1],"type":["fctr"],"align":["left"]},{"label":["VAR_LABEL"],"name":[2],"type":["fctr"],"align":["left"]},{"label":["DATA_TYPE"],"name":[3],"type":["fctr"],"align":["left"]},{"label":["LEVELS_NB"],"name":[4],"type":["int"],"align":["right"]},{"label":["LEVELS"],"name":[5],"type":["chr"],"align":["left"]},{"label":["N_AVAILABLE"],"name":[6],"type":["int"],"align":["right"]},{"label":["PROP_AVAILABLE"],"name":[7],"type":["dbl"],"align":["right"]},{"label":["N_MISSING"],"name":[8],"type":["int"],"align":["right"]},{"label":["PROP_MISSING"],"name":[9],"type":["dbl"],"align":["right"]},{"label":["N_INF"],"name":[10],"type":["int"],"align":["right"]},{"label":["PROP_INF"],"name":[11],"type":["dbl"],"align":["right"]},{"label":["UNIQUE"],"name":[12],"type":["int"],"align":["right"]}],"data":[{"1":"mpg","2":"Miles/(US) gallon","3":"numeric","4":"NA","5":"NA","6":"32","7":"1","8":"0","9":"0","10":"0","11":"0","12":"25"},{"1":"cyl","2":"Number of cylinders","3":"numeric","4":"NA","5":"NA","6":"32","7":"1","8":"0","9":"0","10":"0","11":"0","12":"3"},{"1":"disp","2":"Displacement (cu.in.)","3":"numeric","4":"NA","5":"NA","6":"32","7":"1","8":"0","9":"0","10":"0","11":"0","12":"27"},{"1":"hp","2":"Gross horsepower","3":"numeric","4":"NA","5":"NA","6":"32","7":"1","8":"0","9":"0","10":"0","11":"0","12":"22"},{"1":"drat","2":"Rear axle ratio","3":"numeric","4":"NA","5":"NA","6":"32","7":"1","8":"0","9":"0","10":"0","11":"0","12":"22"},{"1":"wt","2":"Weight (1000 lbs)","3":"numeric","4":"NA","5":"NA","6":"32","7":"1","8":"0","9":"0","10":"0","11":"0","12":"29"},{"1":"qsec","2":"1/4 mile time","3":"numeric","4":"NA","5":"NA","6":"32","7":"1","8":"0","9":"0","10":"0","11":"0","12":"30"},{"1":"vs","2":"Engine","3":"factor","4":"2","5":"V-shaped, Straight","6":"32","7":"1","8":"0","9":"0","10":"NA","11":"NA","12":"2"},{"1":"am","2":"Transmission","3":"factor","4":"2","5":"Automatic, Manual","6":"32","7":"1","8":"0","9":"0","10":"NA","11":"NA","12":"2"},{"1":"gear","2":"Number of forward gears","3":"numeric","4":"NA","5":"NA","6":"32","7":"1","8":"0","9":"0","10":"0","11":"0","12":"3"},{"1":"carb","2":"Number of carburetors","3":"numeric","4":"NA","5":"NA","6":"32","7":"1","8":"0","9":"0","10":"0","11":"0","12":"6"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

Statistiques descriptives
-------------------------

### Statistiques descriptives globales

#### Utilisation de la fonction pipe `%>%`

La fonction `statsBordeaux::describeDataFrame()` permet de décrire les variables qualitatives et quantitatives d'un data.frame. Elle peut s'utiliser avec les pipes `%>%` (package `dplyr`).

``` r
# chargement de dplyr
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
# statistique descriptive sur l'ensemble du data.frame
description <- labelledData %>%
  statsBordeaux::describeDataFrame()

description[c(1:5, 36:39), ]
```

<script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["Variable"],"name":[1],"type":["chr"],"align":["left"]},{"label":["Modality"],"name":[2],"type":["chr"],"align":["left"]},{"label":["Description"],"name":[3],"type":["chr"],"align":["left"]},{"label":["All"],"name":[4],"type":["chr"],"align":["left"]}],"data":[{"1":"Miles/(US) gallon","2":"NA","3":"NA","4":"NA","_rn_":"1"},{"1":"NA","2":"NA","3":"N (m.d.)","4":"32 (0)","_rn_":"2"},{"1":"NA","2":"NA","3":"Mean (SD)","4":"20,091 (6,027)","_rn_":"3"},{"1":"NA","2":"NA","3":"Median [Q1 ; Q3]","4":"19,2 [15,425 ; 22,8]","_rn_":"4"},{"1":"NA","2":"NA","3":"Min ; Max","4":"10,4 ; 33,9","_rn_":"5"},{"1":"Engine","2":"NA","3":"NA","4":"NA","_rn_":"36"},{"1":"NA","2":"All modalities","3":"N (m.d. ; %)","4":"32 (0 ; 0)","_rn_":"37"},{"1":"NA","2":"V-shaped","3":"N (%)","4":"18 (56,25)","_rn_":"38"},{"1":"NA","2":"Straight","3":"N (%)","4":"14 (43,75)","_rn_":"39"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

De la même manière, il est possible de réaliser une description graphique des données avec la fonction `statsBordeaux::getGraphicalDescription()`

``` r
# statistique graphique sur l'ensemble du data.frame
graphicDescription <- labelledData %>%
  statsBordeaux::getGraphicalDescription()

## Variable quantitative : BOX PLOT
graphicDescription[[1]]
```

    ## $mpg

![](README_files/figure-markdown_github/unnamed-chunk-11-1.png)

``` r
## Variable qualitative : BAR PLOT
graphicDescription[[8]]
```

    ## $vs

![](README_files/figure-markdown_github/unnamed-chunk-11-2.png)

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

<script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["Variable"],"name":[1],"type":["chr"],"align":["left"]},{"label":["Modality"],"name":[2],"type":["chr"],"align":["left"]},{"label":["Description"],"name":[3],"type":["chr"],"align":["left"]},{"label":["All"],"name":[4],"type":["chr"],"align":["left"]}],"data":[{"1":"Miles/(US) gallon","2":"NA","3":"NA","4":"NA","_rn_":"1"},{"1":"NA","2":"NA","3":"N (m.d.)","4":"32 (0)","_rn_":"2"},{"1":"NA","2":"NA","3":"Mean (SD)","4":"20,091 (6,027)","_rn_":"3"},{"1":"NA","2":"NA","3":"Median [Q1 ; Q3]","4":"19,2 [15,425 ; 22,8]","_rn_":"4"},{"1":"NA","2":"NA","3":"Min ; Max","4":"10,4 ; 33,9","_rn_":"5"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

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

<script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["Variable"],"name":[1],"type":["chr"],"align":["left"]},{"label":["Modality"],"name":[2],"type":["chr"],"align":["left"]},{"label":["Description"],"name":[3],"type":["chr"],"align":["left"]},{"label":["All"],"name":[4],"type":["chr"],"align":["left"]},{"label":["V-shaped"],"name":[5],"type":["chr"],"align":["left"]},{"label":["Straight"],"name":[6],"type":["chr"],"align":["left"]}],"data":[{"1":"Miles/(US) gallon","2":"NA","3":"NA","4":"NA","5":"NA","6":"NA","_rn_":"1"},{"1":"NA","2":"NA","3":"N (m.d.)","4":"32 (0)","5":"18 (0)","6":"14 (0)","_rn_":"2"},{"1":"NA","2":"NA","3":"Mean (SD)","4":"20,091 (6,027)","5":"16,617 (3,861)","6":"24,557 (5,379)","_rn_":"3"},{"1":"NA","2":"NA","3":"Median [Q1 ; Q3]","4":"19,2 [15,425 ; 22,8]","5":"15,65 [14,775 ; 19,075]","6":"22,8 [21,4 ; 29,625]","_rn_":"4"},{"1":"NA","2":"NA","3":"Min ; Max","4":"10,4 ; 33,9","5":"10,4 ; 26","6":"17,8 ; 33,9","_rn_":"5"},{"1":"Transmission","2":"NA","3":"NA","4":"NA","5":"NA","6":"NA","_rn_":"36"},{"1":"NA","2":"All modalities","3":"N (m.d. ; %)","4":"32 (0 ; 0)","5":"18 (0 ; 0)","6":"14 (0 ; 0)","_rn_":"37"},{"1":"NA","2":"Automatic","3":"N (%)","4":"19 (59,375)","5":"12 (66,667)","6":"7 (50)","_rn_":"38"},{"1":"NA","2":"Manual","3":"N (%)","4":"13 (40,625)","5":"6 (33,333)","6":"7 (50)","_rn_":"39"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

De la même manière, il est possible de réaliser une description graphique des données en fonction d'une variable qualitative de groupe en rajoutant le paramètre `group` à la fonction `statsBordeaux::getGraphicalDescription()`

``` r
# statistique graphique sur l'ensemble du data.frame
graphicDescription <- labelledData %>%
  statsBordeaux::getGraphicalDescription(group = "vs")

## Variable quantitative : BOX PLOT
graphicDescription[[1]]
```

    ## $mpg

![](README_files/figure-markdown_github/unnamed-chunk-14-1.png)

``` r
## Variable qualitative : BAR PLOT
graphicDescription[[9]]
```

    ## $am

![](README_files/figure-markdown_github/unnamed-chunk-14-2.png) \#\#\#\# Utilisateurs avancés, utilisation pas à pas On réalise les statistiques descriptives des données en fonction d'une variable qualitative de comparaison en rajoutant un paramètre `group` aux fonction `statsBordeaux::statsQL()` et `statsBordeaux::statsQT()`. On peut dans ce cas rajouter le paramètre `all = TRUE` afin afficher une description globale des variables.

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

<script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["Variable"],"name":[1],"type":["chr"],"align":["left"]},{"label":["Modality"],"name":[2],"type":["chr"],"align":["left"]},{"label":["Description"],"name":[3],"type":["chr"],"align":["left"]},{"label":["All"],"name":[4],"type":["chr"],"align":["left"]},{"label":["V-shaped"],"name":[5],"type":["chr"],"align":["left"]},{"label":["Straight"],"name":[6],"type":["chr"],"align":["left"]}],"data":[{"1":"Miles/(US) gallon","2":"NA","3":"NA","4":"NA","5":"NA","6":"NA","_rn_":"1"},{"1":"NA","2":"NA","3":"N (m.d.)","4":"32 (0)","5":"18 (0)","6":"14 (0)","_rn_":"2"},{"1":"NA","2":"NA","3":"Mean (SD)","4":"20,091 (6,027)","5":"16,617 (3,861)","6":"24,557 (5,379)","_rn_":"3"},{"1":"NA","2":"NA","3":"Median [Q1 ; Q3]","4":"19,2 [15,425 ; 22,8]","5":"15,65 [14,775 ; 19,075]","6":"22,8 [21,4 ; 29,625]","_rn_":"4"},{"1":"NA","2":"NA","3":"Min ; Max","4":"10,4 ; 33,9","5":"10,4 ; 26","6":"17,8 ; 33,9","_rn_":"5"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

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
```

    ## Warning in wilcox.test.default(x = c(21, 21, 18.7, 14.3, 16.4, 17.3, 15.2, :
    ## cannot compute exact p-value with ties

    ## Warning in wilcox.test.default(x = c(6, 6, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, :
    ## cannot compute exact p-value with ties

    ## Warning in wilcox.test.default(x = c(160, 160, 360, 360, 275.8, 275.8, 275.8, :
    ## cannot compute exact p-value with ties

    ## Warning in wilcox.test.default(x = c(110, 110, 175, 245, 180, 180, 180, : cannot
    ## compute exact p-value with ties

    ## Warning in wilcox.test.default(x = c(3.9, 3.9, 3.15, 3.21, 3.07, 3.07, 3.07, :
    ## cannot compute exact p-value with ties

    ## Warning in wilcox.test.default(x = c(2.62, 2.875, 3.44, 3.57, 4.07, 3.73, :
    ## cannot compute exact p-value with ties

    ## Warning in wilcox.test.default(x = c(16.46, 17.02, 17.02, 15.84, 17.4, 17.6, :
    ## cannot compute exact p-value with ties

    ## Warning in wilcox.test.default(x = c(4, 4, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, :
    ## cannot compute exact p-value with ties

    ## Warning in wilcox.test.default(x = c(4, 4, 2, 4, 3, 3, 3, 4, 4, 4, 2, 2, :
    ## cannot compute exact p-value with ties

``` r
comp[c(1:5, 36:39), ]
```

<script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["Variable"],"name":[1],"type":["chr"],"align":["left"]},{"label":["Modality"],"name":[2],"type":["chr"],"align":["left"]},{"label":["Description"],"name":[3],"type":["chr"],"align":["left"]},{"label":["V-shaped"],"name":[4],"type":["chr"],"align":["left"]},{"label":["Straight"],"name":[5],"type":["chr"],"align":["left"]},{"label":["Test"],"name":[6],"type":["fctr"],"align":["left"]},{"label":["p-value"],"name":[7],"type":["fctr"],"align":["left"]}],"data":[{"1":"Miles/(US) gallon","2":"NA","3":"NA","4":"NA","5":"NA","6":"Wilcoxon rank sum test with continuity correction","7":"<0,001","_rn_":"1"},{"1":"NA","2":"NA","3":"N (m.d.)","4":"18 (0)","5":"14 (0)","6":"NA","7":"NA","_rn_":"2"},{"1":"NA","2":"NA","3":"Mean (SD)","4":"16,617 (3,861)","5":"24,557 (5,379)","6":"NA","7":"NA","_rn_":"3"},{"1":"NA","2":"NA","3":"Median [Q1 ; Q3]","4":"15,65 [14,775 ; 19,075]","5":"22,8 [21,4 ; 29,625]","6":"NA","7":"NA","_rn_":"4"},{"1":"NA","2":"NA","3":"Min ; Max","4":"10,4 ; 26","5":"17,8 ; 33,9","6":"NA","7":"NA","_rn_":"5"},{"1":"Transmission","2":"NA","3":"NA","4":"NA","5":"NA","6":"Pearson's Chi-squared test","7":"0,341","_rn_":"36"},{"1":"NA","2":"All modalities","3":"N (m.d. ; %)","4":"18 (0 ; 0)","5":"14 (0 ; 0)","6":"NA","7":"NA","_rn_":"37"},{"1":"NA","2":"Automatic","3":"N (%)","4":"12 (66,667)","5":"7 (50)","6":"NA","7":"NA","_rn_":"38"},{"1":"NA","2":"Manual","3":"N (%)","4":"6 (33,333)","5":"7 (50)","6":"NA","7":"NA","_rn_":"39"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

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
```

    ## Warning in wilcox.test.default(x = c(21, 21, 18.7, 14.3, 16.4, 17.3, 15.2, :
    ## cannot compute exact p-value with ties

    ## Warning in wilcox.test.default(x = c(6, 6, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, :
    ## cannot compute exact p-value with ties

    ## Warning in wilcox.test.default(x = c(160, 160, 360, 360, 275.8, 275.8, 275.8, :
    ## cannot compute exact p-value with ties

    ## Warning in wilcox.test.default(x = c(110, 110, 175, 245, 180, 180, 180, : cannot
    ## compute exact p-value with ties

    ## Warning in wilcox.test.default(x = c(3.9, 3.9, 3.15, 3.21, 3.07, 3.07, 3.07, :
    ## cannot compute exact p-value with ties

    ## Warning in wilcox.test.default(x = c(2.62, 2.875, 3.44, 3.57, 4.07, 3.73, :
    ## cannot compute exact p-value with ties

    ## Warning in wilcox.test.default(x = c(16.46, 17.02, 17.02, 15.84, 17.4, 17.6, :
    ## cannot compute exact p-value with ties

    ## Warning in wilcox.test.default(x = c(4, 4, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, :
    ## cannot compute exact p-value with ties

    ## Warning in wilcox.test.default(x = c(4, 4, 2, 4, 3, 3, 3, 4, 4, 4, 2, 2, :
    ## cannot compute exact p-value with ties

``` r
head(statsComparatives, n = 5)
```

<script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["Variable"],"name":[1],"type":["chr"],"align":["left"]},{"label":["Modality"],"name":[2],"type":["chr"],"align":["left"]},{"label":["Description"],"name":[3],"type":["chr"],"align":["left"]},{"label":["V-shaped"],"name":[4],"type":["chr"],"align":["left"]},{"label":["Straight"],"name":[5],"type":["chr"],"align":["left"]},{"label":["Test"],"name":[6],"type":["fctr"],"align":["left"]},{"label":["p-value"],"name":[7],"type":["fctr"],"align":["left"]}],"data":[{"1":"Miles/(US) gallon","2":"NA","3":"NA","4":"NA","5":"NA","6":"Wilcoxon rank sum test with continuity correction","7":"<0,001","_rn_":"1"},{"1":"NA","2":"NA","3":"N (m.d.)","4":"18 (0)","5":"14 (0)","6":"NA","7":"NA","_rn_":"2"},{"1":"NA","2":"NA","3":"Mean (SD)","4":"16,617 (3,861)","5":"24,557 (5,379)","6":"NA","7":"NA","_rn_":"3"},{"1":"NA","2":"NA","3":"Median [Q1 ; Q3]","4":"15,65 [14,775 ; 19,075]","5":"22,8 [21,4 ; 29,625]","6":"NA","7":"NA","_rn_":"4"},{"1":"NA","2":"NA","3":"Min ; Max","4":"10,4 ; 26","5":"17,8 ; 33,9","6":"NA","7":"NA","_rn_":"5"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

Reporting
---------

### Reporting avec `rmarkdown`

Dans un projet R Markdown `Knit to HTML`, vous pouvez ajouter facilement un tableau de donnée avec la fonction `statsBordeaux::addKable()`. Cette dernière génère un **tableau HTML**.
La gestion des titre et des numéro de tableau se fait au niveau du document et non dans la fonction `statsBordeaux::addKable()`.

La génération de .pdf en LaTeX n'est actuellement pas prise en charge.

``` r
statsBordeaux::addKable(description)
```

<table class="table table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto; border-top: 1px solid black;border-collapse: collapse;">
<thead>
<tr>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;text-align: center;border-bottom: 2px solid black; padding: 0px 10px 5px 10px;">
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;text-align: center;border-bottom: 2px solid black; padding: 0px 10px 5px 10px;">
All
</th>
</tr>
</thead>
<tbody style="border-bottom: 1px solid black; border-top: 1px solid black;border-collapse: collapse;">
<tr>
<td style="text-align:left;font-weight: bold;border:none; padding: 0px 0px;">
Miles/(US) gallon
</td>
<td style="text-align:left;text-align:right;font-weight: bold;border:none; padding: 0px 0px;">
</td>
</tr>
<tr>
<td style="text-align:left;border:none; padding: 0px 0px;">
<p style="text-indent:20px;">
N (m.d.)
</p>
</td>
<td style="text-align:left;text-align:right;border:none; padding: 0px 0px;">
32 (0)
</td>
</tr>
<tr>
<td style="text-align:left;border:none; padding: 0px 0px;">
<p style="text-indent:20px;">
Mean (SD)
</p>
</td>
<td style="text-align:left;text-align:right;border:none; padding: 0px 0px;">
20,091 (6,027)
</td>
</tr>
<tr>
<td style="text-align:left;border:none; padding: 0px 0px;">
<p style="text-indent:20px;">
Median \[Q1 ; Q3\]
</p>
</td>
<td style="text-align:left;text-align:right;border:none; padding: 0px 0px;">
19,2 \[15,425 ; 22,8\]
</td>
</tr>
<tr>
<td style="text-align:left;border:none; padding: 0px 0px;">
<p style="text-indent:20px;">
Min ; Max
</p>
</td>
<td style="text-align:left;text-align:right;border:none; padding: 0px 0px;">
10,4 ; 33,9
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;border:none; padding: 0px 0px;">
Number of cylinders
</td>
<td style="text-align:left;text-align:right;font-weight: bold;border:none; padding: 0px 0px;">
</td>
</tr>
<tr>
<td style="text-align:left;border:none; padding: 0px 0px;">
<p style="text-indent:20px;">
N (m.d.)
</p>
</td>
<td style="text-align:left;text-align:right;border:none; padding: 0px 0px;">
32 (0)
</td>
</tr>
<tr>
<td style="text-align:left;border:none; padding: 0px 0px;">
<p style="text-indent:20px;">
Mean (SD)
</p>
</td>
<td style="text-align:left;text-align:right;border:none; padding: 0px 0px;">
6,188 (1,786)
</td>
</tr>
<tr>
<td style="text-align:left;border:none; padding: 0px 0px;">
<p style="text-indent:20px;">
Median \[Q1 ; Q3\]
</p>
</td>
<td style="text-align:left;text-align:right;border:none; padding: 0px 0px;">
6 \[4 ; 8\]
</td>
</tr>
<tr>
<td style="text-align:left;border:none; padding: 0px 0px;">
<p style="text-indent:20px;">
Min ; Max
</p>
</td>
<td style="text-align:left;text-align:right;border:none; padding: 0px 0px;">
4 ; 8
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;border:none; padding: 0px 0px;">
Displacement (cu.in.)
</td>
<td style="text-align:left;text-align:right;font-weight: bold;border:none; padding: 0px 0px;">
</td>
</tr>
<tr>
<td style="text-align:left;border:none; padding: 0px 0px;">
<p style="text-indent:20px;">
N (m.d.)
</p>
</td>
<td style="text-align:left;text-align:right;border:none; padding: 0px 0px;">
32 (0)
</td>
</tr>
<tr>
<td style="text-align:left;border:none; padding: 0px 0px;">
<p style="text-indent:20px;">
Mean (SD)
</p>
</td>
<td style="text-align:left;text-align:right;border:none; padding: 0px 0px;">
230,722 (123,939)
</td>
</tr>
<tr>
<td style="text-align:left;border:none; padding: 0px 0px;">
<p style="text-indent:20px;">
Median \[Q1 ; Q3\]
</p>
</td>
<td style="text-align:left;text-align:right;border:none; padding: 0px 0px;">
196,3 \[120,825 ; 326\]
</td>
</tr>
<tr>
<td style="text-align:left;border:none; padding: 0px 0px;">
<p style="text-indent:20px;">
Min ; Max
</p>
</td>
<td style="text-align:left;text-align:right;border:none; padding: 0px 0px;">
71,1 ; 472
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;border:none; padding: 0px 0px;">
Gross horsepower
</td>
<td style="text-align:left;text-align:right;font-weight: bold;border:none; padding: 0px 0px;">
</td>
</tr>
<tr>
<td style="text-align:left;border:none; padding: 0px 0px;">
<p style="text-indent:20px;">
N (m.d.)
</p>
</td>
<td style="text-align:left;text-align:right;border:none; padding: 0px 0px;">
32 (0)
</td>
</tr>
<tr>
<td style="text-align:left;border:none; padding: 0px 0px;">
<p style="text-indent:20px;">
Mean (SD)
</p>
</td>
<td style="text-align:left;text-align:right;border:none; padding: 0px 0px;">
146,688 (68,563)
</td>
</tr>
<tr>
<td style="text-align:left;border:none; padding: 0px 0px;">
<p style="text-indent:20px;">
Median \[Q1 ; Q3\]
</p>
</td>
<td style="text-align:left;text-align:right;border:none; padding: 0px 0px;">
123 \[96,5 ; 180\]
</td>
</tr>
<tr>
<td style="text-align:left;border:none; padding: 0px 0px;">
<p style="text-indent:20px;">
Min ; Max
</p>
</td>
<td style="text-align:left;text-align:right;border:none; padding: 0px 0px;">
52 ; 335
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;border:none; padding: 0px 0px;">
Rear axle ratio
</td>
<td style="text-align:left;text-align:right;font-weight: bold;border:none; padding: 0px 0px;">
</td>
</tr>
<tr>
<td style="text-align:left;border:none; padding: 0px 0px;">
<p style="text-indent:20px;">
N (m.d.)
</p>
</td>
<td style="text-align:left;text-align:right;border:none; padding: 0px 0px;">
32 (0)
</td>
</tr>
<tr>
<td style="text-align:left;border:none; padding: 0px 0px;">
<p style="text-indent:20px;">
Mean (SD)
</p>
</td>
<td style="text-align:left;text-align:right;border:none; padding: 0px 0px;">
3,597 (0,535)
</td>
</tr>
<tr>
<td style="text-align:left;border:none; padding: 0px 0px;">
<p style="text-indent:20px;">
Median \[Q1 ; Q3\]
</p>
</td>
<td style="text-align:left;text-align:right;border:none; padding: 0px 0px;">
3,695 \[3,08 ; 3,92\]
</td>
</tr>
<tr>
<td style="text-align:left;border:none; padding: 0px 0px;">
<p style="text-indent:20px;">
Min ; Max
</p>
</td>
<td style="text-align:left;text-align:right;border:none; padding: 0px 0px;">
2,76 ; 4,93
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;border:none; padding: 0px 0px;">
Weight (1000 lbs)
</td>
<td style="text-align:left;text-align:right;font-weight: bold;border:none; padding: 0px 0px;">
</td>
</tr>
<tr>
<td style="text-align:left;border:none; padding: 0px 0px;">
<p style="text-indent:20px;">
N (m.d.)
</p>
</td>
<td style="text-align:left;text-align:right;border:none; padding: 0px 0px;">
32 (0)
</td>
</tr>
<tr>
<td style="text-align:left;border:none; padding: 0px 0px;">
<p style="text-indent:20px;">
Mean (SD)
</p>
</td>
<td style="text-align:left;text-align:right;border:none; padding: 0px 0px;">
3,217 (0,978)
</td>
</tr>
<tr>
<td style="text-align:left;border:none; padding: 0px 0px;">
<p style="text-indent:20px;">
Median \[Q1 ; Q3\]
</p>
</td>
<td style="text-align:left;text-align:right;border:none; padding: 0px 0px;">
3,325 \[2,581 ; 3,61\]
</td>
</tr>
<tr>
<td style="text-align:left;border:none; padding: 0px 0px;">
<p style="text-indent:20px;">
Min ; Max
</p>
</td>
<td style="text-align:left;text-align:right;border:none; padding: 0px 0px;">
1,513 ; 5,424
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;border:none; padding: 0px 0px;">
1/4 mile time
</td>
<td style="text-align:left;text-align:right;font-weight: bold;border:none; padding: 0px 0px;">
</td>
</tr>
<tr>
<td style="text-align:left;border:none; padding: 0px 0px;">
<p style="text-indent:20px;">
N (m.d.)
</p>
</td>
<td style="text-align:left;text-align:right;border:none; padding: 0px 0px;">
32 (0)
</td>
</tr>
<tr>
<td style="text-align:left;border:none; padding: 0px 0px;">
<p style="text-indent:20px;">
Mean (SD)
</p>
</td>
<td style="text-align:left;text-align:right;border:none; padding: 0px 0px;">
17,849 (1,787)
</td>
</tr>
<tr>
<td style="text-align:left;border:none; padding: 0px 0px;">
<p style="text-indent:20px;">
Median \[Q1 ; Q3\]
</p>
</td>
<td style="text-align:left;text-align:right;border:none; padding: 0px 0px;">
17,71 \[16,892 ; 18,9\]
</td>
</tr>
<tr>
<td style="text-align:left;border:none; padding: 0px 0px;">
<p style="text-indent:20px;">
Min ; Max
</p>
</td>
<td style="text-align:left;text-align:right;border:none; padding: 0px 0px;">
14,5 ; 22,9
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;border:none; padding: 0px 0px;">
Engine
</td>
<td style="text-align:left;text-align:right;font-weight: bold;border:none; padding: 0px 0px;">
</td>
</tr>
<tr>
<td style="text-align:left;border:none; padding: 0px 0px;">
<p style="text-indent:20px;">
N (m.d. ; %)
</p>
</td>
<td style="text-align:left;text-align:right;border:none; padding: 0px 0px;">
32 (0 ; 0)
</td>
</tr>
<tr>
<td style="text-align:left;border:none; padding: 0px 0px;">
<p style="text-indent:20px;">
V-shaped
</p>
</td>
<td style="text-align:left;text-align:right;border:none; padding: 0px 0px;">
18 (56,25)
</td>
</tr>
<tr>
<td style="text-align:left;border:none; padding: 0px 0px;">
<p style="text-indent:20px;">
Straight
</p>
</td>
<td style="text-align:left;text-align:right;border:none; padding: 0px 0px;">
14 (43,75)
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;border:none; padding: 0px 0px;">
Transmission
</td>
<td style="text-align:left;text-align:right;font-weight: bold;border:none; padding: 0px 0px;">
</td>
</tr>
<tr>
<td style="text-align:left;border:none; padding: 0px 0px;">
<p style="text-indent:20px;">
N (m.d. ; %)
</p>
</td>
<td style="text-align:left;text-align:right;border:none; padding: 0px 0px;">
32 (0 ; 0)
</td>
</tr>
<tr>
<td style="text-align:left;border:none; padding: 0px 0px;">
<p style="text-indent:20px;">
Automatic
</p>
</td>
<td style="text-align:left;text-align:right;border:none; padding: 0px 0px;">
19 (59,375)
</td>
</tr>
<tr>
<td style="text-align:left;border:none; padding: 0px 0px;">
<p style="text-indent:20px;">
Manual
</p>
</td>
<td style="text-align:left;text-align:right;border:none; padding: 0px 0px;">
13 (40,625)
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;border:none; padding: 0px 0px;">
Number of forward gears
</td>
<td style="text-align:left;text-align:right;font-weight: bold;border:none; padding: 0px 0px;">
</td>
</tr>
<tr>
<td style="text-align:left;border:none; padding: 0px 0px;">
<p style="text-indent:20px;">
N (m.d.)
</p>
</td>
<td style="text-align:left;text-align:right;border:none; padding: 0px 0px;">
32 (0)
</td>
</tr>
<tr>
<td style="text-align:left;border:none; padding: 0px 0px;">
<p style="text-indent:20px;">
Mean (SD)
</p>
</td>
<td style="text-align:left;text-align:right;border:none; padding: 0px 0px;">
3,688 (0,738)
</td>
</tr>
<tr>
<td style="text-align:left;border:none; padding: 0px 0px;">
<p style="text-indent:20px;">
Median \[Q1 ; Q3\]
</p>
</td>
<td style="text-align:left;text-align:right;border:none; padding: 0px 0px;">
4 \[3 ; 4\]
</td>
</tr>
<tr>
<td style="text-align:left;border:none; padding: 0px 0px;">
<p style="text-indent:20px;">
Min ; Max
</p>
</td>
<td style="text-align:left;text-align:right;border:none; padding: 0px 0px;">
3 ; 5
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;border:none; padding: 0px 0px;">
Number of carburetors
</td>
<td style="text-align:left;text-align:right;font-weight: bold;border:none; padding: 0px 0px;">
</td>
</tr>
<tr>
<td style="text-align:left;border:none; padding: 0px 0px;">
<p style="text-indent:20px;">
N (m.d.)
</p>
</td>
<td style="text-align:left;text-align:right;border:none; padding: 0px 0px;">
32 (0)
</td>
</tr>
<tr>
<td style="text-align:left;border:none; padding: 0px 0px;">
<p style="text-indent:20px;">
Mean (SD)
</p>
</td>
<td style="text-align:left;text-align:right;border:none; padding: 0px 0px;">
2,812 (1,615)
</td>
</tr>
<tr>
<td style="text-align:left;border:none; padding: 0px 0px;">
<p style="text-indent:20px;">
Median \[Q1 ; Q3\]
</p>
</td>
<td style="text-align:left;text-align:right;border:none; padding: 0px 0px;">
2 \[2 ; 4\]
</td>
</tr>
<tr>
<td style="text-align:left;border:none; padding: 0px 0px;">
<p style="text-indent:20px;">
Min ; Max
</p>
</td>
<td style="text-align:left;text-align:right;border:none; padding: 0px 0px;">
1 ; 8
</td>
</tr>
</tbody>
<tfoot>
<tr>
<td style="padding: 0; border: 0;" colspan="100%">
<span style="font-style: italic;">Note: </span> <sup></sup> Qualitative data are expressed as N (%)
</td>
</tr>
</tfoot>
</table>
### Reporting avec `officier`

Le reporting utilise le package `officier`.
Le reporting se base sur un template (document Word). Ce dernier peut contenir une page de garde et des styles. Ce sont les styles qui sont utilisées dans les différentes fonctions ci-dessous pour gérer la mise en page.
Les styles sont disponibles via la fonction `styles_info(doc)` dans la colonne `style_name` et dépendent du template utilisé.

``` r
## chargement du package officier
library(officer)

## création d'un document vierge
doc <- officer::read_docx()

## ajout d'une table des matières puis d'un saut de page
doc <- statsBordeaux::addTableOfContent(doc, title = "Table des matières", style = "Normal", level = 3)
doc <- statsBordeaux::addBreak(doc)

## ajout d'un titre et d'un sous-titre
doc <- statsBordeaux::addTextElement(doc, title = "Statistiques descriptives", style = "heading 1")
doc <- statsBordeaux::addTextElement(doc, title = "Statistiques descriptives du dataset mtcars ", style = "heading 2")
## ajout d'un tableau de sortie puis d'un saut de page
doc <- statsBordeaux::addTable(doc, table = description, title = "Description du dataset mtcars",
                               displayTestName = FALSE, modalitySize = 2, valueSize = 1.5)
doc <- statsBordeaux::addBreak(doc)

## ajout sous-titre puis d'un tableau de sortie
doc <- statsBordeaux::addTextElement(doc, title = "Statistiques descriptives du dataset mtcars en fonction de engine",
                                     style = "heading 2")
doc <- statsBordeaux::addTable(doc, table = comparaison, title = "Description du dataset mtcars en fonction de engine",
                               displayTestName = FALSE, modalitySize = 2, valueSize = 1.5)
## section en cours au format paysage
doc <- statsBordeaux::setLandscape(doc, add_break = TRUE)

## ajout titre puis d'un sous-titre puis d'un tableau de sortie (le tout au format paysage)
doc <- statsBordeaux::addTextElement(doc, title = "Statistiques comparatives", style = "heading 1")
doc <- statsBordeaux::addTextElement(doc, title = "Statistiques comparatives du dataset mtcars en fonction de engine",
                                     style = "heading 2")
doc <- statsBordeaux::addTable(doc, table = statsComparatives,
                               title = "Comparaison du dataset mtcars en fonction de engine",
                               displayTestName = TRUE, modalitySize = 2, valueSize = 1.5)
doc <- statsBordeaux::setLandscape(doc, add_break = FALSE)

## ajout d'une table des tableaux
doc <- statsBordeaux::addTableOfTables(doc, title = "Table des tableaux", style = "Normal")
## ajout d'une table des graphiques
doc <- statsBordeaux::addTableOfGraphics(doc, title = "Table des graphiques", style = "Normal")

## on sauvegarde le document dans le workspace
print(doc, target = 'stat.docx')
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

La gestion des 'non-applicables' fait intervenir la fonction `statsBordeaux::manageNotApplicable()`. Cette fonction prend 2 paramètres d'entrée :

-   `df`, un data.frame, contenant le jeu de donnée à décrire.
-   `notApplicableChar`, un vecteur de longueur 1 contenant la façon dont sont représentés les 'non-applicables' dans les données.

Cette fonction renvoie une liste de deux éléments :

1.  Le data.frame d'entrée duquel ont été ôtés les 'non-applicable'
2.  Une liste de vecteurs logiques :

-   `TRUE` signifie que la ligne en cours, pour la variable considérée, doit être comptabilisée.
-   `FALSE` signifie que la ligne en cours, pour la variable considérée, ne doit pas être comptabilitée ('non-applicable').

``` r
data(mtcars)

## on créé une colonne qui contient le nombre de vitesse dans le cas d'une transmission manuelle, sinon qui contient "NonApp"
mtcars$nSpeed <- c(5, 6, 5, "NonApp", "NonApp", "NonApp", "NonApp", "NonApp",
                   "NonApp", "NonApp", "NonApp", "NonApp", "NonApp", "NonApp",
                   "NonApp", "NonApp", "NonApp", 6, 6, 6, "NonApp", "NonApp",
                   "NonApp", "NonApp", "NonApp", 5, NA, 6, 5, 6, 6, 5)

## on utilise la fonction manageNotApplicable() pour gérer les non-applicable,
## représentés ici par des 'NonApp'
notApplicable <- statsBordeaux::manageNotApplicable(mtcars, "NonApp")

## on récupère le data.frame sans les 'non-applicables'
mtcars <- notApplicable[[1]]

## on récupère la liste de vecteurs logiques contenant les lignes qui doivent être analysées.
applicable <- notApplicable[[2]]

## vérification que le jeu de donnée ne contient que des chiffres
onlyDigit <- statsBordeaux::checkNotDigitInDataframe(mtcars)

## labellisation des modalitées des variables qualitatives
# on rajoute les labels des modalités de la nouvelle variable qualitative
labels <- rbind(labels, data.frame(Variable = c("nSpeed", "nSpeed"),
                                   Modality = c(5, 6),
                                   Label = c("5 vitesses", "6 vitesses")))
# labellisation des modalitées des variables qualitatives
labelledData <- statsBordeaux::setLabelToFactorLevels(mtcars, labels)

## labélisation des variables du data.frame
labelVariable <- rbind(labelVariable,
                       data.frame(Variable = "nSpeed",
                                  Label = "Nombre de vitesses (boite manuelle)"))
labelledData <- statsBordeaux::setLabelToVariable(labelledData, labelVariable)

##----------------

## création du tableau de sortie vierge
descriptionNonAvailable <-  statsBordeaux::createOutput()

## boucle qui va traiter chaque colonne du data.frame les unes après les autres
for(i in 1:ncol(labelledData)){
  # cas où la colonne en cours est de type quantitative
  if(is.numeric(labelledData[, i])){
    descriptionNonAvailable <- statsBordeaux::statsQT(output = descriptionNonAvailable,
                                                      data = statsBordeaux::subset_withAttributes(
                                                        labelledData,
                                                        applicable[[colnames(labelledData)[i]]]),
                                                      variable = colnames(labelledData)[i])
  }
  # cas où la colonne en cours est de type qualitative
  else if(is.factor(labelledData[, i])){
    descriptionNonAvailable <- statsBordeaux::statsQL(output = descriptionNonAvailable,
                                                      data = statsBordeaux::subset_withAttributes(
                                                        labelledData,
                                                        applicable[[colnames(labelledData)[i]]]),
                                                      variable = colnames(labelledData)[i])
  }
  # autre cas non pris en charge
  else {
    print(paste0("Variable '", colnames(labelledData[i]), "' non décrite (", class(labelledData[, i]), ")"))
  }
}

descriptionNonAvailable[c(40:44, 55:57), ]
```

<script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["Variable"],"name":[1],"type":["chr"],"align":["left"]},{"label":["Modality"],"name":[2],"type":["chr"],"align":["left"]},{"label":["Description"],"name":[3],"type":["chr"],"align":["left"]},{"label":["All"],"name":[4],"type":["chr"],"align":["left"]}],"data":[{"1":"Transmission","2":"NA","3":"NA","4":"NA","_rn_":"40"},{"1":"NA","2":"All modalities","3":"N (m.d. ; %)","4":"32 (0 ; 0)","_rn_":"41"},{"1":"NA","2":"Automatic","3":"N (%)","4":"19 (59,375)","_rn_":"42"},{"1":"NA","2":"Manual","3":"N (%)","4":"13 (40,625)","_rn_":"43"},{"1":"Number of forward gears","2":"NA","3":"NA","4":"NA","_rn_":"44"},{"1":"NA","2":"All modalities","3":"N (m.d. ; %)","4":"12 (1 ; 7,692)","_rn_":"55"},{"1":"NA","2":"5 vitesses","3":"N (%)","4":"5 (41,667)","_rn_":"56"},{"1":"NA","2":"6 vitesses","3":"N (%)","4":"7 (58,333)","_rn_":"57"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

################# Analyse univari�e #########################

### R�sum�e des donn�es
summary( data )


########## Calcul des effectifs #########################

### Effectif de sexe
table( data$sexe )

### Effectif de niveau
table( data$niveau )

### Effectif de frereEtSoeur
table( data$frereEtSoeur )


########## Centralit� #########################

### Mode de sexe
sort ( table ( data$sexe ), decreasing = TRUE)

### Mode de niveau
sort ( table ( data$niveau ), decreasing = TRUE)

### Mode de frereEtSoeur
sort ( table ( data$frereEtSoeur ), decreasing  )

### M�diane de taille en prenant en compte les valeurs manquantes
median ( data $taille ,na.rm = TRUE)

### M�diane d'une variable ordonn�e 
sort ( data$niveau )[ round ( (length(na.omit ( data$niveau ))+1) /2 )]

### Calcul de la moyenne 
mean ( data$age ,na.rm = TRUE)


########## Dispersion #########################

### Calcul des quartiles pour une variable num�rique
quantile ( data$age ,na.rm = TRUE)

### Premier quartile (Q1) pour une variable ordonn�e
rangQ1 <- round ( (length(na. omit ( data$niveau ))+3) /4 )
sort ( data$niveau )[ rangQ1 ]

### Troisi�me quartile (Q3)
rangQ3 <- round ( (3*length(na. omit ( data$niveau ))+1) /4 )
sort ( data$niveau )[ rangQ3 ]


### Ecart type
sd( data$age ,na.rm = TRUE)


### Variance
var( data$age ,na.rm = TRUE)



########## Repr�sentation graphique #########################

### Histogramme en baton de l'effectif pour la variable UFR
barplot ( table ( data$UFR ))


### Camemberts de l'effectif pour la variable UFR
pie( table ( data$UFR ))

### Histogramme en baton de l'effectif pour la variable continue taille en fixant le nombre de colonne avec breaks
hist ( data$taille ,col=" grey ",breaks =10)


### Bo�te � moustaches
boxplot ( data $ taille )

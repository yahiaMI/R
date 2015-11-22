### Lecture des données en considérant les données manquantes
data <- read.csv2("FormationR.csv",na.string ="")

### Vérification que les données sont en mèmoire
data

### Contrôle des types des colonnes
str( data )

### Modification du type de departement
data$departement <- as.factor(data$departement)

### Modification du type de id
data$id <- as.factor(data$id)


### Ordonnancement de niveau
data$niveau <- ordered(data$niveau , levels =c("L1","L2","L3","M1","M2"))
data$niveau


### Contrôle des types des colonnes
str( data )
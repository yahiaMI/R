### Lecture des donn�es en consid�rant les donn�es manquantes
data <- read.csv2("FormationR.csv",na.string ="")

### V�rification que les donn�es sont en m�moire
data

### Contr�le des types des colonnes
str( data )

### Modification du type de departement
data$departement <- as.factor(data$departement)

### Modification du type de id
data$id <- as.factor(data$id)


### Ordonnancement de niveau
data$niveau <- ordered(data$niveau , levels =c("L1","L2","L3","M1","M2"))
data$niveau


### Contr�le des types des colonnes
str( data )
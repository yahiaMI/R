################# Analyse bivariée #########################


########## Calcul des effectifs croisés #########################

### Effectifs croisées de sexe & UFR
table ( data$sexe , data$UFR )


########## Calcul de moyenne et de l'écrat type  #########################

### Moyenne d'age du premier rapport homme
mean ( data$rapportAge [ data$sexe =="H"],na.rm = TRUE)


### Moyenne d'age du premier rapport femme
sd( data$rapportAge [ data$sexe =="F"],na.rm = TRUE)


### Ecart type des hommes
mean ( data$rapportAge [ data$sexe =="H"],na.rm = TRUE)


### Ecart type des femmes
sd( data$rapportAge [ data$sexe =="F"],na.rm = TRUE)



########## Représentation graphique  #########################

###Qualitative & numérique

# Premiere relation selon les UFR
# Boite à moustache côte à côte pour chaque modalité de la variable qualitative
boxplot ( data$rapportAge~data$UFR )


###numérique & numérique

# Age et taille : on trace un nuage de point
plot ( data$age , data$taille )


########## Tests #########################

# Tester, c'est répondre à la question : y a-t-il un lien entre mes deux variables ?.
# Il y a deux types de test : 
# 	- tests paramétriques : Tests puissants mais ils nécessitent que les variables aient certaines propriétés. 
# 	- tests non paramétriques : Tests moins puissants, mais n'imposent pas de condition d'application.

# Le choix du test se fait en fonction du type et du propriétés des variables.


### Qualitative & Qualitative

## Test paramétrique : Test de khi2
## Test non paramétrique : Test exact de Fisher

# Les valeurs de toutes les cases du tableau des effectifs attendus doivent être suppérieures à 5.
# Lien entre sexe et transAvecPres -> pas de warning Test khi2 OK
chisq.test( data$sexe , data$transAvecPres )

# Avec un petit p de 0.03, on peut conclure qu'il y a un lien entre les variables.
# Il y a un lien entre le sexe et le fait de penser qu'on peut contracter le SIDA même en utilisant un préservatif ; les femmes ont une plus grande confiance dans les préservatifs. Un bref examen du tableau des effectifs nous montre le sens du lien :
table ( data$sexe , data$transAvecPres )


#Si test un lien entre les variables sexe et rapportSexuel, on obtient un warning. On ne peut pas appliquer le test de khi2
chisq.test ( data$sexe , data$rapportRisque )

#On vérifie cela à l'aide du tableau croisée
table ( data$sexe , data$rapportRisque )

# On utilise donc le test de fisher 
fisher.test ( data$sexe , data$rapportRisque )

# Avec un p de 0.62, on peut conclure qu'il n'y a pas de lien entre le sexe et la prise de risque : les hommes et les femmes se comportent de la même manière vis-à-vis du risque.


### Qualitative (2 classes) & Numérique
# Les tests possibles sont le T de Student (paramétrique) et le test des rangs de Wilcoxon (non paramétrique).
# Les conditions d'application du Test de Student sont : 
# - Les écart types des deux groupes sont égaux
# - Pour chaque groupe, la variable numérique suit une loi normale OU les effectifs sont supérieurs à 30.


# On utilise F de Fisher pour comparaison des variances : des ages selon le sexe

summary (aov( data$age~data$sexe ))

#Avec un p de 0.683, les variances peuvent être considérées comme égales.


# Reste à vérifier la distribution. Notre population étant de petite taille, il est plus pertinent de tracer des barplot que des histogrammes

# Permet de placer 2 graphiques cote à cote
par( mfrow =c(1 ,2))
# Trace les histogramme de age selon les sexes
barplot ( table ( data$taille [ data$sexe =="F"]))
barplot ( table ( data$taille [ data$sexe =="H"]))

#Les deux variables suivent une loi normale, on peut donc utiliser le T de Student.


# T de Student
t.test ( data$taille~data$sexe ,var.equal = TRUE)

#Avec un p value = 4.195e-08, on conclut que la taille des garçons est significativement plus grande que celle des filles.



#Si on souhaite tester le lien entre l'âge et la prise de risque, on constate que le groupe ayant pris des risques est tellement petit (4 personnes) qu'il ne peut pas suivre une loi normale. On doit donc appliquer un test des rangs de Wilcoxon

wilcox.test ( data$age~data$rapportRisque )

#Avec un petit p à 0.416, on peut conclure qu'il n'y a pas de lien entre la prise de risque et l'âge.


### Qualitative (3 classes et plus) & Numérique

## Les tests possibles sont l'analyse de variance ou ANOVA qui se conclut par un F de Fisher (paramétrique) et le test de Kruskal Wallis (non paramétrique).
# Les conditions d'application du Test de ANOVA sont (pareil que le test de T de Student: 
# - Les écart types des deux groupes sont égaux
# - Pour chaque groupe, la variable numérique suit une loi normale OU les effectifs sont supérieurs à 30.

# But comparaison de l'âge du premier rapport selon les UFR

# Age du premier rapport selon les UFR
summary (aov( data$rapportAge~data$UFR ))

# p étant de 0.0901 , les variances peuvent être considérées comme égales. Mais, vérifions la normalité de la variable rapportAge


### Normalité de rapportAge selon les UFR
par( mfrow =c(1 ,3))
barplot( table (c( data $ rapportAge [ data$UFR =="SEGMI"] ,14:21)) -1 , xlab ="SEGMI")
barplot( table (c( data $ rapportAge [ data$UFR =="SJAP"] ,14:21)) -1 , xlab ="SJAP")
barplot( table (c( data $ rapportAge [ data$UFR =="STAPS"] ,14:21)) -1 , xlab ="STAPS")

# La normalité n'est pas respectée. Il est donc nécessaire d'utiliser un test non paramétrique

# Age du premier rapport selon les UFR
kruskal.test( data$rapportAge~data$UFR )

# Avec p égal à 0.06, on peut conclure qu'il n'y a pas de lien entre l'âge de la première relation sexuelle et l'appartenance à un UFR.



### Numérique & Numérique

## Les tests possibles sont : 
## - la corrélation de Pearson (paramétrique) 
## - la corrélation de Spearman (non paramétrique)

## La condition d'application du test de corrélation de Pearson est qu'au moins une des deux variables doit suivre une loi normale.

#On veut étudier le score de connaissance du SIDA et l'âge du premier rapport

# Vérification de la normalité
par( mfrow = c(1 ,2) )
barplot ( table ( data$scoreConnaissance ))
barplot ( table (c( data$rapportAge ,14:21)) - 1)

#La variable scoreConnaissance suit une loi normale, on peut donc utiliser le R de Pearson

### Correlation de Pearson
cor.test ( data$scoreConnaissance , data$rapportAge )

#Le petit p étant de 0.22, donc, il n'y a pas de lien entre l'âge de la première relation et la connaissance du SIDA


# On veut maintenant étudier la taille et l'âge du premier rapport
# Vérification de la normalité
par( mfrow =c(1 ,2))
barplot ( table (c( data $taille ,(160:185) / 100)) -1)
barplot ( table (c( data $ rapportAge ,14:21)) -1)

# -> Aucune varaible ne suit une loi normale
# -> On utilise le test de corrélation de spearman

cor.test ( data$scoreConnaissance , data$rapportAge , method ="spearman")

#Le petit p étant de 0.18, donc, il n'y a pas de lien entre l'âge de la première relation et la taille.














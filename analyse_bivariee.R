################# Analyse bivari�e #########################


########## Calcul des effectifs crois�s #########################

### Effectifs crois�es de sexe & UFR
table ( data$sexe , data$UFR )


########## Calcul de moyenne et de l'�crat type  #########################

### Moyenne d'age du premier rapport homme
mean ( data$rapportAge [ data$sexe =="H"],na.rm = TRUE)


### Moyenne d'age du premier rapport femme
sd( data$rapportAge [ data$sexe =="F"],na.rm = TRUE)


### Ecart type des hommes
mean ( data$rapportAge [ data$sexe =="H"],na.rm = TRUE)


### Ecart type des femmes
sd( data$rapportAge [ data$sexe =="F"],na.rm = TRUE)



########## Repr�sentation graphique  #########################

###Qualitative & num�rique

# Premiere relation selon les UFR
# Boite � moustache c�te � c�te pour chaque modalit� de la variable qualitative
boxplot ( data$rapportAge~data$UFR )


###num�rique & num�rique

# Age et taille : on trace un nuage de point
plot ( data$age , data$taille )


########## Tests #########################

# Tester, c'est r�pondre � la question : y a-t-il un lien entre mes deux variables ?.
# Il y a deux types de test : 
# 	- tests param�triques : Tests puissants mais ils n�cessitent que les variables aient certaines propri�t�s. 
# 	- tests non param�triques : Tests moins puissants, mais n'imposent pas de condition d'application.

# Le choix du test se fait en fonction du type et du propri�t�s des variables.


### Qualitative & Qualitative

## Test param�trique : Test de khi2
## Test non param�trique : Test exact de Fisher

# Les valeurs de toutes les cases du tableau des effectifs attendus doivent �tre supp�rieures � 5.
# Lien entre sexe et transAvecPres -> pas de warning Test khi2 OK
chisq.test( data$sexe , data$transAvecPres )

# Avec un petit p de 0.03, on peut conclure qu'il y a un lien entre les variables.
# Il y a un lien entre le sexe et le fait de penser qu'on peut contracter le SIDA m�me en utilisant un pr�servatif ; les femmes ont une plus grande confiance dans les pr�servatifs. Un bref examen du tableau des effectifs nous montre le sens du lien :
table ( data$sexe , data$transAvecPres )


#Si test un lien entre les variables sexe et rapportSexuel, on obtient un warning. On ne peut pas appliquer le test de khi2
chisq.test ( data$sexe , data$rapportRisque )

#On v�rifie cela � l'aide du tableau crois�e
table ( data$sexe , data$rapportRisque )

# On utilise donc le test de fisher 
fisher.test ( data$sexe , data$rapportRisque )

# Avec un p de 0.62, on peut conclure qu'il n'y a pas de lien entre le sexe et la prise de risque : les hommes et les femmes se comportent de la m�me mani�re vis-�-vis du risque.


### Qualitative (2 classes) & Num�rique
# Les tests possibles sont le T de Student (param�trique) et le test des rangs de Wilcoxon (non param�trique).
# Les conditions d'application du Test de Student sont : 
# - Les �cart types des deux groupes sont �gaux
# - Pour chaque groupe, la variable num�rique suit une loi normale OU les effectifs sont sup�rieurs � 30.


# On utilise F de Fisher pour comparaison des variances : des ages selon le sexe

summary (aov( data$age~data$sexe ))

#Avec un p de 0.683, les variances peuvent �tre consid�r�es comme �gales.


# Reste � v�rifier la distribution. Notre population �tant de petite taille, il est plus pertinent de tracer des barplot que des histogrammes

# Permet de placer 2 graphiques cote � cote
par( mfrow =c(1 ,2))
# Trace les histogramme de age selon les sexes
barplot ( table ( data$taille [ data$sexe =="F"]))
barplot ( table ( data$taille [ data$sexe =="H"]))

#Les deux variables suivent une loi normale, on peut donc utiliser le T de Student.


# T de Student
t.test ( data$taille~data$sexe ,var.equal = TRUE)

#Avec un p value = 4.195e-08, on conclut que la taille des gar�ons est significativement plus grande que celle des filles.



#Si on souhaite tester le lien entre l'�ge et la prise de risque, on constate que le groupe ayant pris des risques est tellement petit (4 personnes) qu'il ne peut pas suivre une loi normale. On doit donc appliquer un test des rangs de Wilcoxon

wilcox.test ( data$age~data$rapportRisque )

#Avec un petit p � 0.416, on peut conclure qu'il n'y a pas de lien entre la prise de risque et l'�ge.


### Qualitative (3 classes et plus) & Num�rique

## Les tests possibles sont l'analyse de variance ou ANOVA qui se conclut par un F de Fisher (param�trique) et le test de Kruskal Wallis (non param�trique).
# Les conditions d'application du Test de ANOVA sont (pareil que le test de T de Student: 
# - Les �cart types des deux groupes sont �gaux
# - Pour chaque groupe, la variable num�rique suit une loi normale OU les effectifs sont sup�rieurs � 30.

# But comparaison de l'�ge du premier rapport selon les UFR

# Age du premier rapport selon les UFR
summary (aov( data$rapportAge~data$UFR ))

# p �tant de 0.0901 , les variances peuvent �tre consid�r�es comme �gales. Mais, v�rifions la normalit� de la variable rapportAge


### Normalit� de rapportAge selon les UFR
par( mfrow =c(1 ,3))
barplot( table (c( data $ rapportAge [ data$UFR =="SEGMI"] ,14:21)) -1 , xlab ="SEGMI")
barplot( table (c( data $ rapportAge [ data$UFR =="SJAP"] ,14:21)) -1 , xlab ="SJAP")
barplot( table (c( data $ rapportAge [ data$UFR =="STAPS"] ,14:21)) -1 , xlab ="STAPS")

# La normalit� n'est pas respect�e. Il est donc n�cessaire d'utiliser un test non param�trique

# Age du premier rapport selon les UFR
kruskal.test( data$rapportAge~data$UFR )

# Avec p �gal � 0.06, on peut conclure qu'il n'y a pas de lien entre l'�ge de la premi�re relation sexuelle et l'appartenance � un UFR.



### Num�rique & Num�rique

## Les tests possibles sont : 
## - la corr�lation de Pearson (param�trique) 
## - la corr�lation de Spearman (non param�trique)

## La condition d'application du test de corr�lation de Pearson est qu'au moins une des deux variables doit suivre une loi normale.

#On veut �tudier le score de connaissance du SIDA et l'�ge du premier rapport

# V�rification de la normalit�
par( mfrow = c(1 ,2) )
barplot ( table ( data$scoreConnaissance ))
barplot ( table (c( data$rapportAge ,14:21)) - 1)

#La variable scoreConnaissance suit une loi normale, on peut donc utiliser le R de Pearson

### Correlation de Pearson
cor.test ( data$scoreConnaissance , data$rapportAge )

#Le petit p �tant de 0.22, donc, il n'y a pas de lien entre l'�ge de la premi�re relation et la connaissance du SIDA


# On veut maintenant �tudier la taille et l'�ge du premier rapport
# V�rification de la normalit�
par( mfrow =c(1 ,2))
barplot ( table (c( data $taille ,(160:185) / 100)) -1)
barplot ( table (c( data $ rapportAge ,14:21)) -1)

# -> Aucune varaible ne suit une loi normale
# -> On utilise le test de corr�lation de spearman

cor.test ( data$scoreConnaissance , data$rapportAge , method ="spearman")

#Le petit p �tant de 0.18, donc, il n'y a pas de lien entre l'�ge de la premi�re relation et la taille.














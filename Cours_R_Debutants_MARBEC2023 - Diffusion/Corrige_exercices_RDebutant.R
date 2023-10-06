
# Cours MARBEC - DEN Initiation a R
# 20-21 mars 2023
# Corrige exercices 

# Exercice 1 --------------------------------------------------------------

# Creer les 3 vecteurs suivants et verifier qu'ils apparaissent dans la fenêtre Global environment en haut a droite :

#   a, un vecteur numerique de longueur 10 contenant les dizaines de 10 a 100 : 10, 20, 30...100; 
a <- c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
# ou bien :
a <- seq(10,100,10)

#   b un vecteur alphanumerique (caractere) contenant les noms de 4 planetes du systeme solaire;
b <- c("Mars", "Saturne", "Venus", "Jupiter")

#   c un vecteur logique de longueur 5, compose de vrai, faux, vrai, faux, faux 
c <- c(TRUE,FALSE,TRUE,FALSE,FALSE)
# ou bien :
c <- c(T,F,T,F,F)
# Attention: obligatoirement en majuscules (et en anglais !)


# Utiliser les differentes fonctions que nous avons vues pour verifier 
# le contenu, la longueur, le type de ces trois vecteurs.

# Contenu
a
b
c

# Longueur
length(a)
length(b)
length(c)

# Type
mode(a)
mode(b)
mode(c)


# On peut aussi ajouter une description avec cat() 
# Le caractere \n (new line) impose un retour a la ligne apres affichage du resultat
cat("Le vecteur a contient : ", a, "\n")
cat("La longueur du vecteur a est de ", length(a), "elements\n")


# Selectionner de 3 manieres differentes les elements n°1,3,6,8 du vecteur a. 
# Placer le resultat dans un vecteur a1. 

# 1 : Entiers positifs
a1 <- a[c(1,3,6,8)]

# 2 : Entiers negatifs
a11 <- a[c(-2,-4,-5,-7,-9,-10)]
# ou bien
a11 <- a[-c(2,4,5,7,9,10)]

# 3 : Vecteur logique
a111 <- a[c(T,F,T,F,F,T,F,T,F,F)]
# ou bien utiliser le vecteurc deja cree plus haut (qui est recycle car il est de longueur 5)
a111 <- a[c]
# Attention : dans la premiere syntaxe, c correspond a la FONCTION de concatenation
# Dans la seconde, c'est l'OBJET c cree ci-dessus


# Generer un facteur bf a partir du vecteur b et verifier le type de ces deux vecteurs.
bf <- as.factor(b)
mode(b)
mode(bf)
# Le facteur est de type numerique bien que le vecteur soit de type character

# Supprimer de bf le premier element et mettre a jour l'attribut levels. 
bf <- bf[-1]
bf 
# Le niveau supprime des donnees apparait toujours

bf <-droplevels(bf)
bf 
# Maintenant il n'apparait plus


# Exercice 2 --------------------------------------------------------------

# Constituez une matrice m de dimension [10,2] a partir du vecteur a et du vecteur b. 
# Qu'observez-vous ? Quel est le mode de m ? Affichez la premiere colonne de m.
m <- cbind(a,b)
m
mode(m) # m est de type character car le vecteur a (numerique) a ete converti en caracteres
m[,1]

# De même creez  une matrice mf a partir de a et de bf. Quel est le mode de mf ?
mf <- cbind(a,bf)
mf
mode(mf) # mf est de type numerique car le facteur bf est stocke sous forme numerique (voir mode(bf))
mf[,1]

# Creez une liste contenant 3 elements : le vecteur a, le vecteur b et le vecteur c. 
# Donnez un nom personnalise a chaque element de la liste. 
# Affichez le 2eme element de deux manieres differentes

mesneveux <- list(riri=a,fifi=b, loulou=c)
mesneveux[[2]]
meneveux$fifi


# Exercice 3 --------------------------------------------------------------

# Sous R, charger le jeu de donnees predefini iris, 
# l'explorer en utilisant les fonctions vues ci-dessus. 
# Vous pouvez utiliser l'aide R associee a iris pour en savoir plus.
data(iris)

dim(iris)
str(iris)
head(iris)
tail(iris)
summary(iris)

?iris

# Exporter le data.frame iris en format texte dans le repertoire de travail 
# sous le nom de iris.txt. 
write.table(iris, "iris.txt", row.names=F, sep="\t", dec=",")
# Les options permettent :
# - de ne pas exporter les identifiants des lignes (row.names=F)
# - de definir les separateurs de colonnes comme des tabulations (sep="\t")
# - de definir la virgule comme separateur decimal

# Ensuite, verifier la presence du fichier iris.txt 
# dans le repertoire de travail et l'ouvrir avec un tableur. 
# Dans ce tableur, ajouter une sixieme colonne intitulee codesp 
# contenant les trois premieres lettres de la colonne Species. 
# Enregistrer ce fichier au format texte delimite par des tabulations sous le nom iris2.txt. 

# Puis retourner sous R et importer ce fichier de donnees 
# dans un nouveau data.frame appele irisnew. 
irisnew <- read.delim2("iris2.txt")
# La fonction read.delim2 comprend par defaut des noms de colonnes (header=T),
# des separateurs tabulation (sep="\t") et la virgule comme separateur decimal (dec=",")
# Cela fonctionne pour un fichier texte delimite par des tabulations au format francais

# Verifiez son contenu sous R avec dim, head, etc. 
dim(irisnew)
str(irisnew)
head(irisnew)
tail(irisnew)
summary(irisnew)

# Vous pourrez egalement enregistrer iris2 au format csv 
# et le lire sous R avec la fonction correspondante.
irisnew <- read.csv2("iris2.csv")
# read.csv2 est adapte pour un fichier csv format francais (separateur ";" et decimale ",")

str(irisnew)
head(irisnew)

# Sauver le jeu de donnees iris au format .rds. 
# Puis le relire sous R en l'appelant iris3
saveRDS(iris, "iris.rds")
iris3 <- readRDS("iris.rds")


# Exercice 4 --------------------------------------------------------------

# Reprenez le jeu de donnees Plantgrowth vu plus haut. 
# Utilisez les fonctions que l'on vient de voir pour faire 
# une description statistique de la colonne weight : 
# etendue de la distribution, moyenne, variance, ecart-type, mediane et quartiles.
data(PlantGrowth)

min(PlantGrowth$weight)
max(PlantGrowth$weight)
range(PlantGrowth$weight)
mean(PlantGrowth$weight)
var(PlantGrowth$weight)
sd(PlantGrowth$weight)
median(PlantGrowth$weight)
quantile(PlantGrowth$weight)

summary(PlantGrowth)


# Sur ce même jeu de donnees faire une regression de la variable weight 
# en fonction de la variable group. Quelles conclusions en tirez-vous ? 
m <- lm(weight~group, data=PlantGrowth) 
m
summary(m)
# La seule difference (faiblement) significative observee est entre trt2 et ctrl 


# Exercice 5 --------------------------------------------------------------

# Chargez le jeu de donnees de R mtcars et affichez son descriptif
data(mtcars)
?mtcars

# Faites un draftsman display. Modifiez la taille et la police des labels. 
# cex.labels gere la taille des noms de variables (c'est 2 par defaut)
pairs(mtcars, cex.labels=1.5) 
# Noms de variables en italique avec font.labels=3
pairs(mtcars, font.labels=3) 
# On n'affiche pas les graphes en-dessous de la diagonale
pairs(mtcars, lower.panel = NULL) 
# On n'affiche pas les graphes au-dessus de la diagonale
pairs(mtcars, upper.panel = NULL) 
# Seulement les 7 premieres colonnes
pairs(mtcars[,1:7], upper.panel=NULL, cex.labels=2.5, font.labels=3) 

# Puis faites un scatterplot de la variable mpg en fonction de la variable hp, 
# avec un titre et des labels personnalises sur les axes. 
plot(mpg~hp, 
     data=mtcars, 
     main="Miles per gallon vs. horsepower", 
     xlab="Gross horsepower",
     ylab="Miles per gallon")


# Exercice 6 --------------------------------------------------------------

# Reprenez le jeu de donnees PlantGrowth et faites l'histogramme de la colonne weight 
hist(PlantGrowth$weight, 
     freq=F, 
     xlab="Weight", 
     main="Distribution de la variable weight")

# en superposant la courbe de densite.
lines(density(PlantGrowth$weight), col="red", lwd=1.5)

# Toujours sur PlantGrowth, representez un boxplot de weight pour chacun des 3 groupes, 
# avec des encoches autour de la mediane 
boxplot(weight~group, data=PlantGrowth, notch=T)

# Warning ! Utiliser notch=F car ici ce n'est pas adapte a la configuation des donnees
boxplot(weight~group, data=PlantGrowth, notch=F)

# Discutez de ce resultat en le comparant avec le resultat du lm() de l'exercice 4.

# On retrouve que weight pour le traitement 1 (trt1) est plutot inferieur au controle
# Mais dans le modele ce n'est pas significatif car forte dispersion des points
# Et le traitement 2 (trt2) donne bien des rendements superieurs a ctrl 


# Exercice 7 --------------------------------------------------------------

# Reprenez le jeu de donnees mtcars et le plot de la variable mpg 
# en fonction de la variable hp. 
plot(mpg~hp, data=mtcars, main="Miles per gallon vs. horsepower", xlab="gross horsepower",
     ylab="Miles per gallon")

# Superposez la courbe lissee par la fonction loess (en rouge - trait plein) 
lines(lowess(mtcars$hp, mtcars$mpg), col="red", lwd=2) # lwd pour l'epaisseur du trait

# et la droite de regression (en bleu - trait pointille).
# Ici au lieu de faire le modele et de recuperer les coefficients a et b pour les passer a abline()
# on passe directement la formule du modele
abline(lm(mpg~hp, data=mtcars), col="blue", lwd=2, lty=2) # lty = type de ligne (1=trait plein, 2 = grands pointilles...)


# Exercice 8 -------------------------------------------------------------

# Sur le jeu de donnees mtcars, calculer la valeur moyenne des colonnes 1 a 7
# par modalite de la colonne 'transmission'
motors <- aggregate(mtcars[,1:7], 
                    by=list(transmission=mtcars$am), 
                    FUN=mean)
motors$transmission <- as.factor(motors$transmission)

# Puis afficher le resultat sous forme de barplot en utilisant une boucle for
par(mfrow=c(1,7))
for (i in (2:8)){
barplot(motors[,i]~motors$transmission, 
        xlab="", 
        ylab="",
        cex.main=2,
        col=c("red","blue"),
        main=colnames(motors)[i])
}

# Exercice 9 -------------------------------------------------------------

# Mettre dans un objet demain de type Date la date de demain. 
# L'afficher en faisant apparaitre, separes par des tirets :
# le jour de la semaine, le jour, le mois en toutes lettres et l'annee
# Extraire de cet objet les differents elements (jour, mois, annee).
demain <- Sys.Date() + 1
format(demain, "%A-%d-%B-%Y")
demain

# Quel jour de la semaine êtes-vous ne(e) ?
birthdate <- as.Date("2023-03-20")
format(birthdate, "%A")

# Calculer le nombre de jours ecoules entre le jour de votre naissance et aujourd'hui
nbjours <- Sys.Date() - birthdate
nbjours

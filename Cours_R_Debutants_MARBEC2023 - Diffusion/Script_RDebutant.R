

# Script cours R debutants MARBEC DEN 10-11 mars 2022 

# Quelques commandes R ----------------------------------------------------

getwd()

# setwd("C:/Users/msimier/Documents/Stage R")
# setwd("C:\\Users\\msimier\\Documents\\Stage R")
# 
# setwd("..") 

2+3

n <- 2+3
n
N <- 4+6
N
n

ls()

rm(N)
ls()

rm(list=ls())

help(rm)
?rm


# Le langage R : le vecteur -----------------------------------------------

v1 <- c(0,14,23)
v1
length(v1)
mode(v1)

v2 <- c(0.1,14,23.75)
v2
mode(v2)

v3 <- c("rouge", "vert", "bleu fonce", "vert")
v3
mode(v3)

v4 <- c(TRUE, FALSE, F, T)
v4
mode(v4)

f3 <- as.factor(v3)
f3

v5 <- as.character(v1)
v5

v6 <- as.logical(v1)
v6

v7 <- as.numeric(v3)
v7

x <- c(10, 20, 30, 40, 50)
x>30

x <- c(10, 20, 30, 40, 50)
y <- c(0.5, -0.5, 0.5, -0.5, 0.5)
z <- x*y ; z

x <- c(10, 20, 30, 40, 50)
y <- c(0.5, -0.5)
z <- x*y ; z

x[2] # Renvoie le second element de x
x[c(2,5)] # Renvoie le second et le 5eme elements de x
x[2:5] # Renvoie les elements de 2 a 5

x[-2] # Supprime le second element de x
x[c(-1,-2,-3)] # Supprime les elements 1, 2 et 3
x[-c(1,2,3)] # Supprime les elements 1, 2 et 3

x[x>30] # Selectionne les elements strictement superieurs a 30

f3
f3 <- f3[-1]
f3
f3 <- droplevels(f3)
f3


# La matrice : un objet a 2 dimensions ------------------------------------

mdat <- matrix(c(1,2,3, 11,12,13), 
               nrow = 2, 
               ncol = 3, 
               byrow = TRUE,
               dimnames = list(c("row1", "row2"),
                               c("col1", "col2", "col3")))
mdat
mode(mdat)
mdat[2,3] 
mdat[,3] # 3eme colonne
mdat[1,] # Premiere ligne



# La liste ----------------------------------------------------------------

liste1 <- list(coeff=y, 
               prod=z, 
               nb=c("un", "deux"))
liste1

# extraction du premier element de la liste par index
liste1[[1]]

# extraction du premier element de la liste par nom
liste1$coeff

# extraction d'un sous-element de coeff par index
liste1[[1]][1]


# Le tableau de donnees data.frame ----------------------------------------

mydf <- data.frame(taille=c(167, 170, 185, 190), 
                   poids=c(56, 68, 70, 85))
mydf

mydf$taille
mydf[,1]



# Concatener des objets R -------------------------------------------------
v3 <- v3[-4] # On enleve le dernier element de v3 pour en avoir seulement 3

v8 <- rbind(v1,v3)
v8
is.matrix(v8) # Pose la question : L'objet v8 est-il une matrice ?

v9 <- cbind(v1,v3)
v9

cbind(mydf, mydf)

rbind(mydf, mydf)


# Les fonctions R ---------------------------------------------------------

vecteur1 <- c(1,2,3,4,5,6,7,8,9,10)

# on met dans vecteur2 le resultat de la fonction sin() 
# qui calcule le sinus d'un vecteur
vecteur2 <- sin(vecteur1)
vecteur2


# Generer des donnees sous R ----------------------------------------------

x <- 1:30 ; x

x <- seq(1, 5, 0.5) ; x

x <- rep(1, 5) ; x

x <- gl(3, 5) ; x
x <- gl(2, 3, label=c("Male", "Female")) ; x

x <- rnorm(100, mean=0, sd=1) ; x

xf <- cut(x, 
          breaks= c(min(x), 0, max(x)), 
          include.lowest=T, 
          labels=c("negatif","positif")) ; xf


# Importer un jeu de donnees sous R ---------------------------------------

data(PlantGrowth)
help(PlantGrowth)
dim(PlantGrowth)
names(PlantGrowth)
head(PlantGrowth, 3)
tail(PlantGrowth, 3)
str(PlantGrowth)
summary(PlantGrowth)

write.table(PlantGrowth, 
            file="PlantGrowth.txt", 
            sep="\t", 
            col.names=T, 
            row.names=F, 
            dec=",")

PG <- read.table("PlantGrowth.txt", 
                 sep="\t", 
                 header=T, 
                 dec=",")
PG


# Sauvegarder/recharger un/des objets R

# Format .RData
save(PG,file="PlantGrowth.RData")
rm(PG)
load("PlantGrowth.RData") # On recharge l'objet sauvegardé avec le même nom

# Format .rds
saveRDS(PG,file="PlantGrowth.rds")
rm(PG)
PG <- readRDS("PlantGrowth.rds") # On recharge l'objet sauvegardé en lui donnant le nom qu'on veut
toto <- readRDS("PlantGrowth.rds")

rm(toto)


# Statistiques descriptives -----------------------------------------------

data(iris)

# affichage du contenu de la variable
iris$Sepal.Width

# Nombre d'observations = longueur du vecteur
length(iris$Sepal.Width)

# affichage des valeurs minimum et maximum et de l'etendue (range)
min(iris$Sepal.Width)
max(iris$Sepal.Width)
range(iris$Sepal.Width)

# Mediane et quantiles
median(iris$Sepal.Width)
quantile(iris$Sepal.Width)
quantile(iris$Sepal.Width, probs=seq(0,1,0.1))

# Moyenne et somme
mean(iris$Sepal.Width)
sum(iris$Sepal.Width)
sum(iris$Sepal.Width) / length(iris$Sepal.Width)

w <- rep(1,150)
length(w)
weighted.mean(iris$Sepal.Width, w)

colMeans(iris[,1:4])

# Variance et ecart-type
var(iris$Sepal.Width)
sqrt(var(iris$Sepal.Width))
sd(iris$Sepal.Width)

# Fonction summary
summary(iris$Sepal.Width)
summary(iris)

# Coefficient de correlation
cor(iris$Petal.Length, iris$Sepal.Length)
cor(iris$Petal.Length, iris$Sepal.Length, method="spearman")
cor(iris[,1:4])

# Regression lineaire
m <- lm(Petal.Length~Sepal.Length, data=iris)
summary(m)
m$coef

m1 <- lm(Petal.Length~Sepal.Length*Species, data=iris)
summary(m1)
m1$coef


# Graphiques --------------------------------------------------------------

x <- seq(1,30,0.1)
y <- sin(x)
plot(x,y, type="p")

# Les instructions ci-dessous sont desacivees pour eviter 
# de perturber le fonctionnement de R

# windows()          # ouvre une fenetre graphique
# x11()              # idem
# pdf("mygraph.pdf") # ouvre un fichier pdf dans repertoire courant

# dev.list()
# dev.cur()
# dev.off()

pdf("mygraph.pdf")
plot(x,y, type="l") # cree le graphe dans le peripherique actif
dev.off()           # ferme le peripherique actif

plot(x,y, type="l") # cree le graphe dans le peripherique actif


# Fonction plot -----------------------------------------------------------

# Chargement des donnees au depart de la librairie interne 'base'
data(iris)

# Affichage de l'aide concernant le jeu de donnees 'iris'
help(iris)

# Resume du tableau de donnees iris (statistiques descriptives)
summary(iris)

# Nuage de points avec en abscisse la variable Sepal_Length,
# en ordonnee Petal.Length, des symboles differents 
# selon l'espece avec l'argument pch=
# un titre general avec main= 
# et des titres d'axes personnalises avec xlab= et ylab=
plot( x=iris$Sepal.Length, 
      y=iris$Petal.Length, 
      pch=(1:3)[iris$Species],
      main="Graphique de base - Iris de Fisher", 
      xlab="Longueur des sepales(mm)", ylab="Longueur des petales (mm)")

# Ajout d'une legende au graphique
help(legend)
legend(7, 3, c("I.setosa", "I.versicolor", "I.virginica"),pch=1:3)

# Autres syntaxes
plot(iris$Petal.Length~iris$Sepal.Length) 
plot(Petal.Length~Sepal.Length, data=iris) 


# Fonction pairs ----------------------------------------------------------

pairs(iris[1:4], main = "Iris de Fisher", pch = 21, 
      bg = c("red", "green3", "blue")[iris$Species], 
      col="black")

pairs(iris[1:4], main = "Iris de Fisher", pch = 21, 
      bg = c("red", "green3", "blue")[iris$Species], 
      lower.panel=NULL,      
      upper.panel=panel.smooth, 
      labels=c("SL","SW","PL","PW"), 
      font.labels=2, 
      cex.labels=2.5)


# Fonction pie ------------------------------------------------------------

# Generation du vecteur de donnees et attribution de noms avec names()
pie.sales <- c(0.12, 0.3, 0.26, 0.16, 0.04, 0.12)
names(pie.sales) <- c("Blueberry", "Cherry",
                      "Apple", "Boston Cream", "Other", "Vanilla Cream")
pie.sales

pie(pie.sales, main="Ice cream",
    col = c("purple", "violetred1", "green3", "cornsilk", "cyan", "white"))


# Fonction hist -----------------------------------------------------------

hist(iris$Sepal.Width, main="Sepal Width", xlab="")

hist(iris$Sepal.Width, main="Sepal Width", xlab="", freq=F)
lines(density(iris$Sepal.Width), col="green")

hist1 <- hist(iris$Sepal.Width)
hist1


# Fonction barplot --------------------------------------------------------

data(VADeaths)
dim(VADeaths)
VADeaths
is.matrix(VADeaths)
is.data.frame(VADeaths)

VADeaths.7074 <- VADeaths[5,]
barplot(VADeaths.7074, main="70-74 ans")

barplot(VADeaths)

barplot(VADeaths, horiz=T, col=rainbow(5),legend=rownames(VADeaths))

barplot(VADeaths, beside = TRUE,
        col = c("lightblue", "mistyrose", "lightcyan",
                "lavender", "cornsilk"),
        legend = rownames(VADeaths), ylim = c(0, 100))
title(main = "Death Rates in Virginia", font.main = 4)


# Fonction boxplot --------------------------------------------------------

boxplot(iris$Sepal.Width)

boxplot(iris$Sepal.Width, plot=F)

# Definir 4 fenetre dans la fenetre graphique
op <- par(mfrow=c(2,2))
boxplot(iris$Sepal.Width, main="Distribution of Sepal Width - Global", col="bisque")
boxplot(iris$Sepal.Width~iris$Species, main="Distribution of Sepal Width by Species", col=heat.colors(3), xlab="")
boxplot(iris$Sepal.Width~iris$Species, main="Adding a notch", col=topo.colors(3), notch=T, xlab="")
boxplot(iris$Sepal.Width~iris$Species, main="Horizontal boxplot", col=rainbow(3), notch=T, horizontal=T, xlab="", ylab="", las=2)
# Retablir la configuration a une seule fenetre
par(op)


# Fonction stars ----------------------------------------------------------

# Premier exemple sur les donnees des iris
# Les 4 variables quantitatives sont representees pour chaque individu
# ncol=10 pour afficher les individus sur 10 colonnes 
# full=FALSE pour representer les variables sur un demi-cercle 
# au lieu d'un cercle complet (full=TRUE qui est l'option par defaut)
# key.loc donne la position de la legende en (x,y)

stars(iris[,1:4], key.loc = c(17,0), full=FALSE, ncol=10)

# Second exemple sur les donnees mtcars
# L'instruction palette() choisit 7 couleurs sur la palette rainbow.
# Les 7 variables sont representees pour chaque modele de voiture
# key.loc donne la position de la legende en (x,y)
# draw.segments=TRUE permet une representation en parts de camembert  
# de longueur proportionnelle a la variable. Essayer en mettant a FALSE

palette(rainbow(7))
stars(mtcars[, 1:7], len = 0.8, key.loc = c(12, 1.5), main = "Motor Trend Cars", draw.segments = TRUE)


# Fonctions de trace de courbes de niveau ---------------------------------

data(volcano)
dim(volcano)

op <- par(mfrow=c(1,3))
contour(volcano, main="contour : courbes de niveau")
image(volcano, main="image : representation coloree")
persp(volcano, main="persp : representation 3D")
par(op)

filled.contour(volcano, main="filled.contour : aires colorees")


# Fonctions de cartographie -----------------------------------------------

library(maps)
m <- map(wrap = c(-180,180), fill = TRUE, col = 1:10)
str(m)

map(wrap = c(-180,180), fill = TRUE, col = 1:10)
map.axes()

library(mapdata)
map('worldHires', 'Mauritania')
map.axes()
title('Mauritania')

# Afficher des points sur un fond de carte
site <- seq(1,6,1)
lat <- c(16.1, 16.9, 18.4, 19.55, 20.56, 20.51)
lon <- c(-16.2, -16.5, -17.1, -17.13, -17.05, -17.97)
catch <- c(2000,4500,5400,6000,2500,1750)
dm <- cbind.data.frame(site,lat,lon,catch)

rlat <- c(min(dm$lat)-0.1, max(dm$lat)+0.1)
rlon <- c(min(dm$lon)-0.1, max(dm$lon)+0.2)

windows()
par(mfrow=c(1,2))
map('worldHires',xlim=rlon, ylim=rlat, col="black")
map.axes()
points(dm$lon,dm$lat,pch=16,cex=1.2)
text(dm$lon,dm$lat,dm$site, pos=4)

map('worldHires',xlim=rlon, ylim=rlat, col="black")
map.axes()
points(dm$lon,dm$lat,pch=16,cex=3*dm$catch/max(dm$catch), col="red")
title("Catches - Banc d'Arguin")


library(tmap)
data("World")
tmap_mode("plot")
tm_shape(World) +
  tm_polygons("HPI")


# Fonctions graphiques secondaires ----------------------------------------

str(cars)

plot(dist~speed, data=cars, main="Stopping Distance versus Speed")
lines(c(10,20), c(20,80), col="blue")
lines(lowess(cars$speed, cars$dist), col="red", lwd=2)
text(x=10,y=100,labels="Stopping distance increases with speed ! ", cex=1.3, col="red")

m <- lm(dist~speed, data=cars)
coef(m)
ordon <- coef(m)[1]
pente <- coef(m)[2]
plot(dist~speed, data=cars)
abline(a=ordon, b=pente, lwd=1.5, col="green")
equa<-paste("dist=", round(ordon,2),"+", round(pente,2),"speed")
text(10,100, labels=equa)

boxplot(iris$Sepal.Width)
abline(h=median(iris$Sepal.Width), col="red", lwd=1.5)
abline(h=max(iris$Sepal.Width), lty=3, col="blue", lwd=1.5)
abline(h=min(iris$Sepal.Width), lty=3, col="blue", lwd=1.5)

plot(-4:4, -4:4, type = "n", xlab="", ylab="")
points(rnorm(200), rnorm(200), col = "red")   
points(rnorm(100)/2, rnorm(100)/2, col = "blue", cex = 1.5, pch=19) 


# Boucle for --------------------------------------------------------------

par(mfrow=c(1,4))
for (i in 1:4) { boxplot(iris[,i], main=colnames(iris)[i])}


# Boucles implicites ------------------------------------------------------

iris_mat <- as.matrix(iris[,1:4])
apply(iris_mat,MARGIN=2,FUN=mean)

L <- list(a = 1:10, beta = exp(-3:3), logic = c(TRUE,FALSE,FALSE,TRUE))
lapply(L,mean)

tapply(iris$Sepal.Width, iris$Species, mean)

aggregate(iris[,1:4],by=list(Variete=iris[,5]), FUN=mean)



# Definir sa propre fonction ----------------------------------------------

Salut <- function( x )
{
  cat("Bonjour", x, "\n")
}

# Application
Salut("Le Monde")


CoefVar <- function( datum )
{
  # Fonction permettant de calculer le coefficient de variation
  # Calcul de l'ecart type
  EcartType <- sd(datum, na.rm=T) 
  # Calcul de la moyenne
  Moyenne <- mean(datum, na.rm=T)
  # Calcul du coefficient de variation
  CV <- 100 * EcartType / Moyenne
  # Retourne le resultat
  return(CV)
}

# Application a un jeu de donnees contenant un NA
TestData <- c( 1, 2, 3, 4, 5, NA )
A <- CoefVar( datum = TestData)
# Affichage du resultat en controlant le nombre de chiffres significatifs
print(A, digits=3)

ai <- aggregate(iris[,1:4],list(Variete=iris[,5]), CoefVar)
print(ai, digits=3)

panel.pearson <- function(x, y, ...) {
  horizontal <- (par("usr")[1] + par("usr")[2]) / 2; 
  vertical <- (par("usr")[3] + par("usr")[4]) / 2; 
  text(horizontal, vertical, format(abs(cor(x,y)), digits=2) , cex=2) 
}
# Application de la function :
pairs(iris[1:4], main = "Les iris de Fisher", pch = 21, 
      bg = c("red","green3","blue")[unclass(iris$Species)], 
      upper.panel=panel.pearson)

# Le format date ----------------------------------------------------------
datestring <- c("2007-06-22", "2004-02-13")
datestring
madate <- as.Date(datestring)
madate

jours <- madate[1] - madate[2]
jours

Sys.Date()
date()

today <- Sys.Date()
format(today, format="%A %d %B %y")
format(today, format="%d/%m/%y")

datestring2 <- c("05/01/1965", "16/08/1975")
madate2 <- as.Date(datestring2, "%d/%m/%Y")
format(madate2, "%A %d %B %y") 

annee <- as.numeric(format(madate2, "%Y")) ; annee
mois <- as.numeric(format(madate2, "%m")) ; mois
jour <- as.numeric(format(madate2, "%d")) ; jour


months(madate2)

date <- "15 janvier 2017 12h57:10"
date_format <- "%d %B %Y %Hh%M:%S"
date_complete <- as.POSIXct(date,
                            format = date_format, 
                            tz = "GMT")
date_complete


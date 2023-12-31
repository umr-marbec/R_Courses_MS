#                                   #
# Script analyse multivari�e sous R #
# Deuxi�me partie :                 #
# Analyse factorielle 2 tableaux    #


# Chargement de la librairie ade4
library(ade4)
library(MASS)


# Analyse Inter et Intra-classes 
# ex sur ACP norm�e mil (meaudret) 

# Analyses Inter-classes ----
# En anglais between-class

data(meaudret)
mil <- meaudret$env
pca1 <- dudi.pca(mil, scannf=F, nf=5)

# Repr�sentation graphique des �toiles par saison ou par site
plan <- meaudret$design
plan

windows()
par(mfrow = c(3, 3))
s.class(pca1$li, plan$season, xax = 1, yax = 2, cellipse = 0, clabel =2, sub="Samples by season, axes 1-2", csub=1.5)
s.class(pca1$li, plan$season, xax = 1, yax = 3, cellipse = 0, clabel =2, sub="Samples by season, axes 1-3", csub=1.5)
s.class(pca1$li, plan$season, xax = 2, yax = 3, cellipse = 0, clabel =2, sub="Samples by season, axes 2-3", csub=1.5)
s.class(pca1$li, plan$site, xax = 1, yax = 2, cellipse = 0, clabel =2, sub="Samples by site, axes 1-2", csub=1.5)
s.class(pca1$li, plan$site, xax = 1, yax = 3, cellipse = 0, clabel =2, sub="Samples by site, axes 1-3", csub=1.5)
s.class(pca1$li, plan$site, xax = 2, yax = 3, cellipse = 0, clabel =2, sub="Samples by site, axes 2-3", csub=1.5)
s.corcircle(pca1$co, xax=1, yax = 2, clabel =2, sub="Variables, axes 1-2", csub=1.5)
s.corcircle(pca1$co, xax=1, yax = 3, clabel =2, sub="Variables, axes 1-3", csub=1.5)
s.corcircle(pca1$co, xax=2, yax = 3, clabel =2, sub="Variables, axes 2-3", csub=1.5)


# On veut d�composer la variabilit� d�crite par l'ACP du tableau mil entre :
# variabilit� inter-sites + intra-sites
# variabilit� inter-saisons + intra-saisons
# (M�me id�e qu'en analyse de variance)

# 1. Analyse inter-sites ----
# Se focaliser sur les diff�rences entre les sites
bca1 <- bca(pca1,plan$site, scannf=F, nf=2)
bca1
bca1$ratio
sum(bca1$eig)/sum(pca1$eig)

# Tests de permutations
rt1 <- rtest(bca1, nrepet=1000)
rt1

windows()
par(mfrow=c(2,2))
s.label(bca1$li, clab=2)
s.class(bca1$ls,plan$site, clab=2)
s.corcircle(bca1$co)
plot(rt1, main="Effet site, 1000 simulations")

# 2. Analyse inter-saisons ----
# Se focaliser sur les diff�rences entre les saisons
bca2 <- bca(pca1,plan$season, scannf=F, nf=2)
bca2
bca2$ratio
sum(bca2$eig)/sum(pca1$eig)
rt2 <- rtest(bca2, nrepet=1000)
rt2

windows()
par(mfrow=c(2,2))
s.label(bca2$li, clab=2)
s.class(bca2$ls,plan$season, clab=2)
s.corcircle(bca2$co)
plot(rt2, main="Effet saison, 1000 simulations")


# Analyses Intra-classes ----
# En anglais within-class

# 1. Analyse intra-sites ----
# Eliminer l'effet site
wca1 <- wca(pca1,plan$site, scannf=F, nf=2)
wca1
wca1$ratio
sum(wca1$eig)/sum(pca1$eig)

windows()
par(mfrow=c(2,2))
s.corcircle(wca1$co)
s.label(wca1$li)
s.class(wca1$li,plan$site, clab=2) # Points moyens des sites � l'origine des axes
s.class(wca1$li,plan$season)

# 2. Analyse intra-saisons ----
wca2 <- wca(pca1,plan$season, scannf=F, nf=2)
wca2
wca2$ratio
sum(wca2$eig)/sum(pca1$eig)

windows()
par(mfrow=c(2,2))
s.corcircle(wca2$co)
s.label(wca2$li)
s.class(wca2$li,plan$season) # Points moyens des saisons � l'origine des axes
s.class(wca2$li,plan$site)

# R�capitulatif des ratio inter et intra
bca1$ratio
bca2$ratio
wca1$ratio
wca2$ratio

# On v�rifie que la somme des variances inter et intra pour un facteur donn� vaut 1
bca1$ratio + wca1$ratio # 1 (car plan �quilibr�)
bca2$ratio + wca2$ratio # 1


# Analyse Discriminante lin�aire ----
# cf Partie3.pdf

data(iris)

# Approche univari�e
# Histogrammes par Species pour les 4 variables num�riques
windows()
par(mfcol = c(3, 4))
for (k in 1:4) {
  j0 <- names(iris)[k]
  br0 <- seq(min(iris[, k]), max(iris[, k]), le = 11)
  x0 <- seq(min(iris[, k]), max(iris[, k]), le = 50)
  for (i in 1:3) {
    i0 <- levels(iris$Species)[i]
    x <- iris[iris$Species == i0, j0]
    hist(x, br = br0, proba = T, col = grey(0.8), main = i0,
         xlab = j0)
    lines(x0, dnorm(x0, mean(x), sd(x)), col = "red", lwd = 2)
  }
}

# Approche bivari�e

# Analyses de variance � un facteur (Species) pour chaque variable num�rique
# Exemple : effet esp�ce sur Sepal.Length

# Moyennes et �carts-types par groupe
tapply(iris$Sepal.Length, iris$Species, mean)
tapply(iris$Sepal.Length, iris$Species, sd)

# ANOVA � un facteur
(aoviris <- lm(iris$Sepal.Length ~ iris$Species))
anova(aoviris)

# Nuages de points de toutes les variables num�riques 2 � 2
windows()
par(mar = c(0, 0, 0, 0))
# Nuages de points simples
pairs(iris[, 1:4])
# Avec des �toiles par Species 
pan1 <- function(x, y, ...) {
  xy <- cbind.data.frame(x, y)
  s.class(xy, iris$Species, include.origin = F, add.plot = T, clab = 1.5,
          col = c("blue", "black", "red"), cpoint = 2, cstar = 0.5)
}
pairs(iris[, 1:4], panel = pan1)

# Chercher � mesurer ce qui s�pare des groupes connus est ce qu'on appelle 
# discriminer. Pourquoi discriminer ? C'est essentiellement pour affecter un
# nouvel individu dont on ne connait pas le groupe mais uniquement les mesures
# qui ont g�n�r� la discrimination.
# On dit alors qu'on a un probl�me de discrimination descriptive quand la
# question est : qu'est-ce qui s�pare les groupes ?
# et un probl�me de discrimination pr�dictive quand la question est : � quel
# groupe est-ce que je peux affecter un nouvel individu et avec quel type d'erreur ?

# Valeur discriminante de chaque variable ?
apply(iris[, 1:4], 2, function(x) summary(lm(x ~ iris[, 5])))

# Analyse discriminante lin�aire avec la fonction lda de MASS ----
library(MASS)
?lda
lda1 <- lda(iris[, 1:4], iris$Species)
lda1
# lda fournit une combinaison lin�aire des variables de d�part,
# avec les coefficients qui sont dans la colonne LD1
lda1$scaling
w1 <- as.vector(as.matrix(iris[, 1:4]) %*% lda1$scaling[, 1]) # %*% = produit matriciel
head(w1)
tail(w1)

# # Calcul de la pr�diction pour le premier individu
# iris[1, 1:4]
# lda1$scaling[, 1]
# 5.1 * 0.8293776 + 3.5 * 1.5344731 + 1.4 * -2.2012117 + 0.2 * -2.8104603 
# w1[1]


# Analyse discriminante lin�aire avec la fonction discrimin d'ade4 ----

dis1 <- discrimin(dudi.pca(iris[, 1:4], scannf = F), iris$Species, scannf = F)
dis1
# discrimin fournit une combinaison lin�aire des variables normalis�es (variance 1)
# avec les coefficients qui sont dans la composante fa 
# (fa pour facteur, dans le vocabulaire du sch�ma de dualit�).
dis1$fa
w2 <- as.vector(scalewt(iris[, 1:4]) %*% dis1$fa[, 1]) 
head(w2)
tail(w2)

# a <- scalewt(iris[, 1:4]) ; a[1,]
# dis1$fa[, 1]
# -0.90068117*0.1200150 + 1.01900435*0.1168775 + -1.34022653*-0.6790443 + -1.3154442950* -0.3743571

# Les coefficients w1 (de lda) et w2 (de discrimin) sont coh�rents
plot(w1, w2, pch = 20)
abline(lm(w2 ~ w1))

# # discrimin donne une combinaison lin�aire de variance totale=1
# var(w2) * 149/150  
# # qui maximise la variance inter-classe (premi�re valeur propre)
# dis1$eig
# summary(lm(w2 ~ iris[, 5]))$r.squared
# 
# # lda donne une combinaison lin�aire de variance intra-classe unit�
# tapply(w1, iris[, 5], var)
# mean(tapply(w1, iris[, 5], var))
# # qui maximise la variance inter-classe

# # Lien avec la MANOVA (ANOVA multivari�e)
# size <- as.matrix(iris[, 1:4])
# spec <- iris[, 5]
# m1 <- manova(size ~ spec)
# summary(m1, test = "Pillai")
# 
# # Le crit�re de Pillai de la MANOVA correspond � la somme des valeurs propres de l'analyse discriminante:
# sum(dis1$eig)

# A la fonction discrimin est associ� un test non param�trique de significativit�
plot(randtest(dis1))
# o� la statistique observ�e est le crit�re de Pillai divis� par le rang de l'analyse de d�part :
# sum(dis1$eig)/4

# Repr�sentation graphique associ�e � discrimin
windows()
plot(dis1)

# Pour faire chaque graphe ind�pendamment
windows()
par(mfrow=c(3,2))
# Eigenvalues diagram
barplot(dis1$eig)
# Canonical weights = loadings = dis1$fa
s.arrow(dis1$fa)
# Cos(variates,canonical variates) = dis1$va
s.corcircle(dis1$va)
# Cos(components,canonical variates) = dis1$cp
s.corcircle(dis1$cp)
# Scores and classes = dis1$li et dis1$gc
s.class(dis1$li, iris$Species)
# Class scores = dis1$gc
s.label(dis1$gc)


# La fonction lda est centr�e sur la question de l'affectation d'un individu � une classe. 
# Peut-on pr�dire � quelle classe appartient un individu dont on conna�t les mesures ?

# On divise au hasard le tableau de donn�es en deux parties
# la premi�re pour chercher une fonction discriminante,
# la seconde pour d�terminer l'esp�ce � l'aide de cette fonction. 
# On compare ensuite avec un table() le r�sultat obtenu et les vraies valeurs.
echa <- sample(1:150, 50)
tabref <- iris[echa, 1:4]
espref <- iris[echa, 5]
tabsup <- iris[-echa, 1:4]
espsup <- iris[-echa, 5]
lda2 <- lda(tabref, espref)
lda2
p.iris <- predict(lda2, tabsup)
espestim <- p.iris$class
table(espestim, espsup)



# Autre exemple : Charolais vs. z�bus ----

library(ade4)
data(chazeb)
chazeb$tab
?chazeb

cz <- chazeb$tab
esp <- chazeb$cla
cz
esp

apply(cz, 2, function(x) anova(lm(x ~ esp)))

# Analyse avec la fonction discrimin()
# ACP exploratoire
pcacz <- dudi.pca(cz, scannf = F, nf=3)
scatter(pcacz)
score(pcacz)

discz <- discrimin(pcacz, esp, scannf = FALSE, nf = 1) # on n'a qu'1 seul axe car seulement 2 groupes 
discz
discz$fa

windows()
plot(discz)

rcz <- randtest(discz)
rcz
plot(rcz)


# Analyse avec la fonction lda()
ldacz <- lda(cz,esp)
ldacz

# Pr�diction 
echa <- sample(1:23, 15)
tabref <- cz[echa, 1:6]
dim(tabref)
espref <- esp[echa]
tabsup <- cz[-echa, 1:6]
dim(tabsup)
espsup <- esp[-echa]

lda2 <- lda(tabref, espref)
lda2

pcz <- predict(lda2, tabsup)
espestim <- pcz$class
table(espestim, espsup)


# Couplages de tableaux ----
# cf Partie4.pdf

# Analyses sur variables instrumentales ----
# Ce sont des m�thodes dissym�triques : 
#  - un tableau X de variables explicatives (instrumentales)
#  - un tableau Y de variables � �tudier
# Application dans ade4 avec la fonction pcaiv()

# 1. ACPVI ou Analyse des redondances ----
# Exemple Doubs : couplage entre l'ACP centr�e du tableau faunistique poi
# et les variables de milieu (mil)

data(doubs)
poi <- doubs$fish
mil <- doubs$env
pcafau <- dudi.pca(poi, scale = F, scannf = F, nf = 2)
pcaivdoubs <- pcaiv(pcafau, mil, scannf = F, nf = 2)
pcaivdoubs
windows()
plot(pcaivdoubs)
s.label(pcafau$li)
s.arrow(pcafau$co)

# D�composition du graphe :
# Loadings
windows()
s.arrow(pcaivdoubs$fa)
# Correlations
s.arrow(pcaivdoubs$cor)
# Scores and predictions
s.match(pcaivdoubs$li, pcaivdoubs$ls)
# Inertia axes
s.corcircle(pcaivdoubs$as)
# Variables
s.arrow(pcaivdoubs$c1)
# Eigenvalues
screeplot(pcaivdoubs)
# ou
barplot(pcaivdoubs$eig)


# Il existe deux possibilit�s pour interpr�ter une ACPVI. L'analyse recherche des
# coeffcients (fa) des variables de X. La combinaison lin�aire obtenue est une
# composante principale ou composante explicative (l1). La composante
# explicative maximise la somme des carr�s de corr�lations (si Y est analys�
# par une ACP norm�e) ou covariances (ACP centr�e) avec les variables de Y.
# Les colonnes de Y sont alors repr�sent�es par leurs corr�lations ou covariances
# (co) avec la composante explicative. Les corr�lations entre X et la composante
# explicative sont dans cor.

var(pcaivdoubs$l1)/30 * 29

head(cov(poi, pcaivdoubs$l1)/30 * 29)
head(pcaivdoubs$co)

sum(pcaivdoubs$co[, 1]^2)
pcaivdoubs$eig[1]

# La deuxi�me interpr�tation de l'ACPVI consiste � calculer un pseudo axe principal (PAP)
# (c1). Les lignes de Y sont projet�es sur les pseudo axes principaux. Ces
# projections (ls) sont des combinaisons des variables de Y maximisant la variance
# expliqu�e par X. Les pr�dictions de ces projections par X sont contenues dans li

t(as.matrix(pcaivdoubs$c1)) %*% as.matrix(pcaivdoubs$c1)
head(as.matrix(pcafau$tab) %*% as.matrix(pcaivdoubs$c1))
head(pcaivdoubs$ls)
lmprovi <- lm(pcaivdoubs$ls[, 1] ~ as.matrix(mil))
predict(lmprovi)[1:5]
pcaivdoubs$li[1:5, 1]
sum(predict(lmprovi)^2)/30

# L'ACPVI fournit donc un compromis entre l'analyse canonique (maximisation
# du carr� de la corr�lation multiple) et l'analyse en composantes principales
# (maximisation de la variance) en maximisant la variance expliqu�e (maximisation
# du produit).

summary(pcaivdoubs)
# Decomposition per axis:
#   iner inercum inerC inercumC ratio    R2 lambda
# 1 42.75    42.7 42.59     42.6 0.996 0.902  38.42
# 2  8.16    50.9  7.76     50.4 0.989 0.767   5.95

# iner = valeurs propres de l'ACP simple
# inercum = valeurs propres cumul�es de l'ACP simple
# lambda = valeurs propres de l'ACPVI
# inerC = somme (pond�r�e par les lw) des carr�s des coordonn�es des lignes (ls)
# inercumC = inerC cumul�s
# R2 = carr� de corr�lation multiple
# ratio 

# L'analyse simple trouve des combinaisons des variables de Y de variance
# maximale (iner et inercum pour le cumul). Les valeurs propres de l'ACPVI
# (lambda) sont des variances expliqu�es (lambda) et correspondent au produit de
# la variance (inerC) par le carr� de la corr�lation multiple (R2). En maximisant
# un compromis (la variance expliqu�e), on rajoute une contrainte (pr�diction par
# les variables de X) et la maximisation de la variance n'est donc plus optimale
# (elle l'est pour l'analyse simple). On mesure l'importance de cette contrainte par
# le ratio des variances des combinaisons des variables de Y des deux analyses :

pcafau$eig[1] # iner=42.75
pcaivdoubs$eig[1] # lambda=38.42
sum(pcaivdoubs$lw * pcaivdoubs$ls[, 1]^2) # inerC=42.6
sum(pcaivdoubs$lw * pcaivdoubs$ls[, 1]^2)/pcafau$eig[1] # ratio=0.996




# 2. AFCVI ou Analyse Canonique des Correspondances ----
# Exemple Doubs : couplage entre l'AFC du tableau fau
# et les variables de milieu (mil)

data(doubs)
poi <- doubs$fish
mil <- doubs$env

coafau <- dudi.coa(poi, scannf = F, nf = 2)
ccadoubs <- pcaiv(coafau, mil, scannf = F, nf = 2)
ccadoubs
windows()
plot(ccadoubs)
summary(ccadoubs)

# L'analyse recherche des coefficients ou loadings (fa) des variables de X. La combinaison
# lin�aire obtenue est une composante principale sous contrainte (l1). C'est un
# score des relev�s de variance unit�, combinaison lin�aire des variables de milieu.
# Les esp�ces (co) sont positionn�es � la moyenne des relev�s. L'analyse maximise
# la variance des moyennes conditionnelles. Cette vision est parfaitement adapt�e
# � la vision de la niche �cologique et des gradients environnementaux sur lesquels
# se s�parent les niches des esp�ces. Il existe un deuxi�me point de vue.
# La deuxi�me interpr�tation de l'AFCVI consiste � calculer un pseudo axe
# principal (c1). Les lignes de Y (sites) sont projet�es sur les pseudo axes principaux
# et positionn�s � la moyenne des esp�ces qu'ils contiennent (ls). Les
# pr�dictions de ces projections par X sont contenues dans li.


# # Essai de s�lection de variables d'environnement
# names(mil)
# 
# mil2 <- mil[,c(1,5,9)]
# names(mil2)
# acpmil <- dudi.pca(mil2, scannf=F, nf=2)
# s.corcircle(acpmil$co)
# s.label(acpmil$li)
# 
# coafau <- dudi.coa(poi, scannf = F, nf = 2)
# ccadoubs2 <- pcaiv(coafau, mil2, scannf = F, nf = 2)
# summary(ccadoubs2)
# windows()
# plot(ccadoubs2)
# 
# pcafau <- dudi.pca(poi, scannf = F, nf = 2)
# pcaivdoubs2 <- pcaiv(pcafau, mil2, scannf = F, nf = 2)
# summary(pcaivdoubs2)
# windows()
# plot(pcaivdoubs2)
# 


# Analyse de coinertie ----

# 1. Coinertie ACP-ACP ----

data(fruits)
?fruits
pcajug <- dudi.pca(fruits$jug, scann = FALSE)
pcavar <- dudi.pca(fruits$var, scann = FALSE)

windows()
par(mfrow = c(2,2)) 
s.corcircle(pcajug$co, sub="ACP du tableau juges")
s.class(pcajug$li, fac = fruits$type, sub="ACP du tableau juges")
s.corcircle(pcavar$co, sub="ACP du tableau variables")
s.class(pcavar$li, fac = fruits$type, sub="ACP du tableau variables")
par(mfrow = c(1,1))
 
coi.fruits <- coinertia(pcajug, pcavar, scan = FALSE) 
coi.fruits
summary(coi.fruits)

# Pour retrouver le RV:
sum(cor(pcajug$tab, pcavar$tab)^2)/sqrt(sum(cor(pcajug$tab, pcajug$tab)^2) * sum(cor(pcavar$tab, pcavar$tab)^2))

plot(coi.fruits)

# Retrouver chaque �l�ment du graphique:
# X axes
# Projections des axes de l'ACP de X sur les axes de coinertie
s.corcircle(coi.fruits$aX)

# Y axes
# Projections des axes de l'ACP de Y sur les axes de coinertie
s.corcircle(coi.fruits$aY)

# Projection des lignes des tableaux X et Y
s.match(coi.fruits$mX, coi.fruits$mY)

# Y canonical weights
s.arrow(coi.fruits$l1)

# X canonical weights
s.arrow(coi.fruits$c1)




# 2. Coinertie AFC-ACP ----
data(doubs)
poi <- doubs$fish
mil <- doubs$env
coafau <- dudi.coa(poi, scannf = F, nf = 2)
pcamil <- dudi.pca(mil, row.w = coafau$lw, scannf = F, nf = 2)
coi.doubs <- coinertia(pcamil, coafau, scannf = F, nf = 2)
coi.doubs
windows()
plot(coi.doubs)
summary(coi.doubs)



# Avifaune Prodon et Lebreton : CCA vs. coinertie ----

# example for the caiv
data(rpjdl)
?rpjdl
millog <- log(rpjdl$mil + 1)
coa1 <- dudi.coa(rpjdl$fau, scannf = FALSE)
caiv1 <- pcaiv(coa1, millog, scannf = FALSE)

windows()
plot(caiv1)

# analysis with c1 - as - li -ls
# projections of inertia axes on PCAIV axes
s.corcircle(caiv1$as)

# Species positions
s.label(caiv1$c1, 2, 1, clab = 0.5, xlim = c(-4, 4))
# Sites positions at the weighted mean of present species
s.label(caiv1$ls, 2, 1, clab = 0, cpoi = 1, add.p = TRUE)

# Prediction of the positions by regression on environmental variables
s.match(caiv1$ls, caiv1$li, 2, 1, clab = 0.5)

# analysis with fa - l1 - co -cor
# canonical weights giving unit variance combinations
s.arrow(caiv1$fa)

# sites position by environmental variables combinations
# position of species by averaging
s.label(caiv1$l1, 2, 1, clab = 0, cpoi = 1.5)
s.label(caiv1$co, 2, 1, add.plot = TRUE)

s.distri(caiv1$l1, rpjdl$fau, 2, 1, cellipse = 0, cstar = 0.33)
s.label(caiv1$co, 2, 1, clab = 0.75, add.plot = TRUE)

# coherence between weights and correlations
par(mfrow = c(1, 2))
s.corcircle(caiv1$cor, 2, 1)
s.arrow(caiv1$fa, 2, 1)
par(mfrow = c(1, 1))


# Coinertia
pca1 <- dudi.pca(millog, row.w = coa1$lw, scannf=F, nf=2)
coi1 <- coinertia(coa1,pca1, scannf=F, nf=2)
summary(coi1)
windows()
plot(coi1)

windows()
plot(caiv1)

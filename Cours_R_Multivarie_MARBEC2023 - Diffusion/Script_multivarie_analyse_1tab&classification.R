#                                   #
# Script analyse multivariée sous R #
# Première partie :                 #
# - Analyse factorielle 1 tableau   #
# - Classification automatique      #


# Chargement de la librairie ade4
library(ade4)

# Chargement de la librairie FactoMineR
library(FactoMineR)


# ACP normée  ----

# ACP normée  avec ade4 sur méaudret milieu----
# dudi.pca() : exécuter l'ACP
# Diagramme des valeurs propres - screeplot() et add.scatter.eig()
# s.corcircle() : cercle de corrélations des variables
# s.label() : nuage de points des individus
# s.class() : projection d'une variable facteur sur le nuage de points des individus

data(meaudret)
?meaudret
is.list(meaudret)
meaudret
mil <- meaudret$env
mil
is.data.frame(mil)
str(mil)

# ACP normée
# Par défaut : choix du nombre d'axes par l'utilisateur
# pca1 <- dudi.pca(mil)

# Ici on demande a priori de conserver 5 axes
pca1 <- dudi.pca(mil, scannf=F, nf=5)
pca1

# Affichage du diagramme des valeurs propres
barplot(pca1$eig, ylab = "Valeurs propres")
# ou
screeplot(pca1)

# Pourcentages d'inertie par axe
pve <- 100 *pca1$eig / sum(pca1$eig)
print(pve, digits=2)

# Pourcentages cumulés d'inertie par axe
cpve <- cumsum(pve) 
print(cpve, digits=3)

# Statistiques d'inertie
iner <-inertia.dudi(pca1)
iner
iner <-inertia.dudi(pca1,row.inertia=TRUE,col.inertia=TRUE)
iner
?inertia.dudi

# Repartition de l'inertie totale entre les axes
iner$tot.inertia

# Contributions des lignes à l'inertie totale (somme=100)
iner$row.contrib
sum(iner$row.contrib)

# Contributions absolues des lignes à l'inertie de chaque axe
iner$row.abs
sum(iner$row.abs[,1])
sum(iner$row.abs[,2])

# Contributions relatives des lignes = cosinus carres
iner$row.rel

# Contributions relatives cumulées des lignes (décomposition par ligne)
iner$row.cum

# Contributions des colonnes à l'inertie totale (somme=100)
iner$col.contrib
sum(iner$col.contrib)

# Contributions absolues des colonnes à l'inertie de chaque axe
iner$col.abs
sum(iner$col.abs[,1])
sum(iner$col.abs[,2])

# Contributions relatives des colonnes (decomposition par axe)
iner$col.rel

# Contributions relatives des colonnes (decomposition par axe)
iner$col.cum

# Représentations graphiques

# Les colonnes
pca1$co

# Cercle de corrélations
# Valeurs par défaut
s.corcircle(
  pca1$co, 
  xax = 1, 
  yax = 2, 
  label = row.names(pca1$co),
  clabel = 1, 
  grid = FALSE, 
  sub = "", 
  csub = 1, 
  possub = "bottomleft", 
  cgrid = 0, 
  fullcircle = TRUE, 
  box = FALSE, 
  add.plot = FALSE)

# Exemple de paramétrage du cercle de corrélations
s.corcircle(pca1$co,
            clabel=0.8,
            grid=F, 
            sub="Cercle de corrélations", 
            csub=1.2,
            possub="topright")

# Pour rappeler le diagramme des valeurs propres sur le cercle de corrélations
add.scatter.eig(pca1$eig,xax=1,yax=2, posi="bottomright")


# Les lignes
pca1$li

# Nuage des individus
# Valeurs par défaut
s.label(pca1$li, 
        xax = 1, yax = 2, 
        label = row.names(pca1$li),
        clabel = 1, 
        pch = 20, 
        cpoint = 0, 
        boxes = TRUE,
        neig = NULL, cneig = 2, 
        xlim = NULL, ylim = NULL, 
        grid = TRUE, 
        addaxes = TRUE,
        cgrid = 1, 
        include.origin = TRUE, 
        origin = c(0,0), 
        sub = "", csub = 1.25, 
        possub = "bottomleft", 
        pixmap = NULL, 
        contour = NULL, area = NULL, add.plot = FALSE)

# Exemple de paramétrage du nuage des individus
s.label(pca1$li, 
        xax = 1, yax = 2, 
        xlim = NULL, ylim = NULL, 
        boxes = F,
        grid = F, 
        sub = "Nuage des individus", csub = 1.25, 
        possub = "bottomleft") 


# Illustratif : Projection des points moyens par saison et par site
plan <- meaudret$design 
plan
s.class(pca1$li,plan$season, sub="Saison")
s.class(pca1$li,plan$site, sub="Site")

# Les 4 graphiques sur la même fenetre
windows()
par(mfrow=c(2,2))
s.corcircle(pca1$co)
s.label(pca1$li)
s.class(pca1$li,plan$season)
s.class(pca1$li,plan$site)
dev.off()

# ACP normée avec FactoMineR sur méaudret milieu -----

data(meaudret)

# Concaténer env et design (nécessaire pour les variables illustratives)
meo <- cbind(meaudret$env, meaudret$design)

# ACP du tableau env (colonnes 1 à 9 actives) 
# avec season (10) et site (11) en illustratif qualitatif
res.meo <- PCA(meo, quali.sup=c(10,11),graph=FALSE)
res.meo

# Récapitulatif de l'ACP
summary(res.meo)

# Diagramme des valeurs propres
barplot(res.meo$eig[,2],main="Valeurs propres",names.arg=1:nrow(res.meo$eig))

# Description des axes 1 et 2
dimdesc(res.meo, axes = 1:2)
?dimdesc

# Nuage des variables (cercle des corrélations)
plot.PCA(res.meo,
         choix='var',
         title="Graphe des variables de l'ACP")

# On ne garde que les variables dont le cos2 (qualité de la représentation) est supérieur à 0.5 
plot.PCA(res.meo,
         choix='var',
         select='cos2  0.5',
         title="Graphe des variables de l'ACP")

# Nuage des individus
plot.PCA(res.meo,
         choix='ind',
         invisible=c('quali','ind.sup'),
         title="Graphe des individus de l'ACP",
         label = c('ind'))

# Nuage des individus avec des couleurs différentes selon la saison (colonne 10)
plot.PCA(res.meo,
         invisible=c('quali','ind.sup'),
         title="Graphe des individus de l'ACP",
         label =c('ind'),
         habillage=10)

#  Tracer des ellipses correspondant aux saisons
plotellipses(res.meo, 10, level=0.95)

# Nuage des individus avec des couleurs différentes selon le site (colonne 11)
plot.PCA(res.meo,
         invisible=c('quali','ind.sup'),
         title="Graphe des individus de l'ACP",
         label =c('ind'),
         habillage=11)

#  Tracer des ellipses correspondant aux sites
plotellipses(res.meo, 11, level=0.95)


# Utilitaire FactoInvestigate
require(FactoInvestigate)
Investigate(res.meo)


## Interface graphique Factoshiny
require(Factoshiny)
res <- Factoshiny(meo)


# Example avec des données manquantes avec la librairie missMDA
require(missMDA)
data(orange)
summary(orange) # valeurs manquantes
res.pca <- PCA(orange) # Les valeurs manquantes sont remplacées par la moyenne de la variable
plot(res.pca, choix = "ind")

# Estimation des valeurs manquantes avec imputePCA
nb <- estim_ncpPCA(orange,ncp.min=0,ncp.max=5,method.cv="Kfold",nbsim=50)
imputed <- imputePCA(orange,ncp=nb$ncp)
imputed

# Refaire l'ACP sur les données complètes
res.pca2 <- PCA(imputed$completeObs)
plot(res.pca2, choix = "ind")

windows()
plot(res.pca, choix = "var", title="valeurs manquantes remplacées par moyenne")
windows()
plot(res.pca, choix = "ind", title="valeurs manquantes remplacées par moyenne")
windows()
plot(res.pca2, choix = "var", title="valeurs manquantes remplacées par imputePCA")
windows()
plot(res.pca2, choix = "ind", title="valeurs manquantes remplacées par imputePCA")


# # ACP normée sur monde84   ----            
# # cf Champely, 2005 - Intro analyse factorielle p.7 
# # Détail des éléments de l'ACP, le schéma de dualité (dudi = duality diagram)
# # la fonction score() : interprétation d'un seul axe
# # la fonction table.value() : représentation graphique du tableau
# 
# data(monde84)
# ?monde84
# monde84
# 
# # Description des données
# dim(monde84)
# dimnames(monde84)[[1]]
# dimnames(monde84)[[2]]
# str(monde84)
# summary(monde84)
# attach(monde84)
# 
# windows()
# par(mfrow=c(2,5))
# hist(pib)
# hist(croipop)
# hist(morta)
# hist(anal)
# hist(scol)
# 
# hist(log(pib))
# hist(croipop)
# hist(log(morta))
# hist(log(anal+1))
# hist(scol)
# 
# # Création d'un dataframe avec transformation log de pib, morta et anal 
# monde2<-data.frame(log(pib),croipop,log(morta),log(anal+1),scol)
# dimnames(monde2)<-list(dimnames(monde84)[[1]], c("lpib","croipop","lmorta","lanal","scol"))
# cor(monde2)
# print(cor(monde2), digits = 2)
# plot(monde2)
# 
# # ACP normée : on garde 2 axes
# pca.monde2<-dudi.pca(monde2, center=T, scale=T, scannf=F, nf=2)
# pca.monde2
# 
# # Détail des éléments de l'ACP, le schéma de dualité (dudi = duality diagram)
# 
# # Le tableau centré-réduit
# pca.monde2$tab
# 
# # On vérifie que dans ce tableau les 5 variables sont 
# # de moyenne 0 
# lapply(pca.monde2$tab, FUN=mean)
# # et d'écart-type 1
# lapply(pca.monde2$tab, FUN=sd)
# 
# # Les poids des lignes = uniformes par défaut (1/n)
# pca.monde2$lw
# sum(pca.monde2$lw)
# 
# # Les poids des colonnes = unitaires par défaut (1)
# pca.monde2$cw
# sum(pca.monde2$cw)
# 
# # Pour retrouver le tableau transformé $tab
# # row.w <- pca.monde2$lw
# # f1 <- function(v) sum(v * row.w)/sum(row.w)
# # f2 <- function(v) sqrt(sum(v * v * row.w)/sum(row.w))
# # center <- apply(monde2, 2, f1)
# # df <- sweep(monde2, 2, center)
# # norm <- apply(df, 2, f2)
# # norm[norm < 1e-08] <- 1
# # df <- sweep(df, 2, norm, "/")
# # head(df)
# # head(pca.monde2$tab)
# 
# # Les valeurs propres
# pca.monde2$eig
# 
# # Coordonnées des individus (pays) sur les axes
# 
# # Sur l'axe 1
# 
# # li = Coordonnées de variance lambda=valeur propre
# head(pca.monde2$li)
# sum(pca.monde2$lw * pca.monde2$li[,1]^2) # on retrouve la première valeur propre
# 
# # l1 = Coordonnées de variance 1
# head(pca.monde2$l1)
# sum(pca.monde2$lw * pca.monde2$l1[,1]^2) # 1
# 
# windows()
# plot(pca.monde2$li[,1]~pca.monde2$l1[,1])
# 
# # Idem pour l'axe 2
# sum(pca.monde2$lw * pca.monde2$li[,2]^2) # on retrouve la deuxième valeur propre
# sum(pca.monde2$lw * pca.monde2$l1[,2]^2) # 1
# plot(pca.monde2$li[,2]~pca.monde2$l1[,2])
# 
# 
# # Corrélations entre variables et axes
# 
# # co => de variance lambda
# pca.monde2$co
# sum(pca.monde2$cw * pca.monde2$co[,1]^2)
# 
# # c1 => de variance 1
# pca.monde2$c1
# sum(pca.monde2$cw * pca.monde2$c1[,1]^2)
# 
# 
# # les valeurs propres sont la somme des carrés des corrélations
# pca.monde2$eig
# sum(pca.monde2$cw * pca.monde2$co[,1]^2)
# sum(pca.monde2$cw * pca.monde2$co[,2]^2)
# 
# 
# # Diagramme des valeurs propres
# barplot(pca.monde2$eig, ylab="Valeurs propres")
# 
# 
# # Le cercle des corrélations
# s.corcircle(pca.monde2$co)
# 
# # Le nuage des individus
# s.label(pca.monde2$li, boxes=F)
# add.scatter.eig(pca.monde2$eig, xax=1,yax=2, posi="bottomright")
# 
# 
# # Dans le cas présent, le premier axe résume presque toute l'information
# # Utiliser la fonction score pour interpréter les axes :
# # La fonction score permet de représenter pour chaque variable
# # le plot de sa valeur (en vertical) en fonction de sa coordonnée sur un axe (horizontal)
# ?score.pca
# score(pca.monde2)
# 
# 
# 
# # Représenter le tableau de données monde2 en ordonnant les lignes et les colonnes 
# # selon leurs coordonnées sur l'axe1
# windows()
# table.value(monde2, x=pca.monde2$co[,1], y=pca.monde2$li[,1])
# 
# # Pour éviter les superpositions de variables, on peut prendre le rang de leur coordonnée
# table.value(monde2, x=rank(pca.monde2$co[,1]), y=pca.monde2$li[,1])
# 
# # Et on peut aussi représenter le tableau centré réduit plutôt que les données brutes
# table.value(pca.monde2$tab, x=rank(pca.monde2$co[,1]), y=pca.monde2$li[,1])
# 
# # Pour enlever la légende, diminuer la taille des noms de pays, augmenter celle des noms de variables
# # et diminuer la taille des carrés
# table.value(pca.monde2$tab, x=rank(pca.monde2$co[,1]), y=pca.monde2$li[,1], 
#             clegend=0, clabel.row=0.8, clabel.col=1.2, csize=0.8)
# 
# 
# 


# # ACP normée sur olympic  -----                        
# # cf Champely,2005 - Intro analyse factorielle p.11 
# # et Partie2-2_ACP p.11 Decathlon 
# # Représentation biplot avec s.label()+s.arrow() ou avec scatter() 
# # Statistiques d'inertie avec inertia.dudi()
# 
# data(olympic)
# ?olympic
# # olympic est une liste de deux composants tab et score
# # olympic$tab est le tableau qui nous intéresse ici
# names(olympic$tab)
# print(cor(olympic$tab),digit=1)
# windows()
# plot(olympic$tab)
# 
# # ACP normée
# pca1<-dudi.pca(olympic$tab,center=T,scale=T, scannf=F, nf=2)
# 
# # Les valeurs propres
# pca1$eig
# barplot(pca1$eig,ylab="Valeurs propres")
# 
# # Le cercle de corrélation des variables sur les axes 1 et 2
# s.corcircle(pca1$co)
# 
# # Petite difficulté pour l'interprétation des résultats :
# # La performance des athlètes augmente avec les distances des lancers, 
# # la hauteur et la longueur des sauts (variables 2,3,4,7,8,9)
# # Mais elle décroit avec le temps pour les épreuves de course (variables 1, 5, 6 et 10)
# # On fait une transformation des données pour que tout aille dans le même sens:
# 
# names(olympic$tab)
# olympic2 <- olympic$tab
# olympic2[, c(1,5,6,10)] = -olympic2[, c(1,5,6,10)]
# pca2 <- dudi.pca(olympic2, scan = F)
# 
# # Le cercle de corrélation des variables
# s.corcircle(pca2$co)
# 
# # Les individus sur les axes 1 et 2
# s.label(pca2$li)
# 
# # Ou bien les deux ensemble (en prenant le l1 pour les individus)
# s.label(pca2$l1, clab = 0.7)
# s.arrow(2 * pca2$co, add.plot = T)
# 
# # Idem avec la fonction scatter (par défaut, diagramme des VP en haut à gauche)
# # positions possible : "none", "topright", "topleft", "bottomright", "bottomleft"
# scatter(pca2)
# scatter(pca2, posi="none")
# scatter(pca2, posi="topright")
# 
# # Statistiques d'inertie
# iner <-inertia.dudi(pca2)
# iner
# iner <-inertia.dudi(pca2,row.inertia=TRUE,col.inertia=TRUE)
# iner
# ?inertia.dudi
# 
# # Repartition de l'inertie totale entre les axes
# iner$tot.inertia
# 
# # Contributions des lignes à l'inertie totale (somme=100)
# iner$row.contrib
# sum(iner$row.contrib)
# 
# # Contributions absolues des lignes à l'inertie de chaque axe
# iner$row.abs
# sum(iner$row.abs[,1])
# sum(iner$row.abs[,2])
# 
# # Contributions relatives des lignes = cosinus carres
# iner$row.rel
# 
# # Contributions relatives cumulées des lignes (décomposition par ligne)
# iner$row.cum
# 
# # Contributions des colonnes à l'inertie totale (somme=100)
# iner$col.contrib
# sum(iner$col.contrib)
# 
# # Contributions absolues des colonnes à l'inertie de chaque axe
# iner$col.abs
# sum(iner$col.abs[,1])
# sum(iner$col.abs[,2])
# 
# # Contributions relatives des colonnes (decomposition par axe)
# iner$col.rel
# 
# # Contributions relatives des colonnes (decomposition par axe)
# iner$col.cum
# 

# ACP centrée sur méaudret faune ----
data(meaudret)
fau <- meaudret$spe

pca2 <- dudi.pca(fau, center=T, scale=F, scannf=F, nf=5)

pve2 <- 100 *pca2$eig / sum(pca2$eig)
pve2
cumsum(pve2)

apply(pca2$tab, 2, FUN=mean) # moyenne égale à 0
apply(pca2$tab, 2, FUN=sd)   # Ecart-type non égal à 1

# Vecteur des poids des lignes et des poids des colonnes
pca2$lw
pca2$cw


# En ACP centrée, les variables ne s'inscrivent plus dans un cercle de rayon 1
# Il faut donc utiliser la fonction slabel comme pour les individus
s.label(pca2$co)

# Ou s.arrow pour les relier à l'origine par une flèche (et rappeler ainsi le cercle de corrélations)
s.arrow(pca2$co)

# Les individus
s.label(pca2$li)

# Illustratif : Points moyens par site et par saison
plan <- meaudret$design
s.class(pca2$li, plan$site)
s.class(pca2$li, plan$season)

# Les 4 graphiques sur la même fenetre
windows()
par(mfrow=c(2,2))
s.arrow(pca2$co)
s.label(pca2$li)
s.class(pca2$li, plan$site)
s.class(pca2$li, plan$season)


# AFC sur méaudret faune ----

coa1 <- dudi.coa(fau, scannf=F, nf=5)

pve3 <- 100 *coa1$eig / sum(coa1$eig)
pve3
cumsum(pve3)

apply(coa1$tab, 2, FUN=mean)
apply(coa1$tab, 2, FUN=sd)

coa1$lw
coa1$cw


windows()
par(mfrow=c(2,2))

s.label(coa1$co)
s.label(coa1$li)

s.class(coa1$li, plan$site)
s.class(coa1$li, plan$season)

# Représenttaion biplot (lignes et colonnes simultanément) avec la fonction scatter()
windows()
scatter(coa1)


# Comparaison entre ACP centrée et AFC
par(mfrow=c(2,2))
s.label(pca2$li, sub="Centred PCA, rows") # upper left
s.label(pca2$co, sub="Centred PCA, columns") # upper right
s.label(coa1$li, sub="COA, rows") # lower left
s.label(coa1$co, sub="COA, columns") # lower right
par(mfrow=c(1,1))




# # AFC, détails sur la transformations des données ----
# # D'apres Partie2-3_Initiation_AFC.pdf 2. table de contingence (p.5)
# 
# # Données assurance
# # en colonnes la situation maritale
# # en lignes le mode de paiement
# sitpay <- matrix(c(209, 1483, 41, 320, 60, 34, 151, 1, 70, 10, 535,
#                    2448, 33, 897, 135, 77, 245, 4, 139, 9), byrow = T, ncol = 5)
# colnames(sitpay) <- c("celibataire", "concubin", "divorce", "marie","veuf")
# rownames(sitpay) <- c("annuel", "mensuel", "semestriel", "trimestriel")
# sitpay
# 
# n <- sum(sitpay) ; n # effectif total du tableau
# 
# # Frequences relatives
# freqsitpay <- sitpay/n
# round(freqsitpay, digits = 4)
# 
# # Ex pour celibataire X annuel
# 209/6901 # 0.0303
# windows()
# table.value(freqsitpay, row.labels = rownames(sitpay), col.labels = colnames(sitpay),
#             csize = 1.5)
# 
# # Profils lignes
# sumLignes <- rowSums(sitpay) ; sumLignes
# profLignes <- prop.table(sitpay, 1) ; profLignes
# # Ex pour celibataire X annuel
# 209/2113 # 0.0989115
# 
# # Profils colonnes
# sumColonnes <- colSums(sitpay) ; sumColonnes
# profColonnes <- prop.table(sitpay, 2) ; profColonnes
# # Ex pour celibataire
# 209/855 # 0.244444
# 
# 
# rowSums(profLignes)
# colSums(profColonnes)
# 
# # Calcul du tableau transformé d'AFC 
# 
# # Frequences marginales des lignes et des colonnes
# Frow <- rowSums(freqsitpay) ; Frow
# Fcol <- colSums(freqsitpay) ; Fcol
# # On divise les fréquences relatives par les fréquences marginales des lignes
# a <- freqsitpay/Frow
# # On divise le résultat par les fréquences marginales des colonnes
# a <- sweep(a, 2, Fcol, "/") - 1
# a
# 
# # On vérifie que le résultat est bien identique au tableau transformé de l'AFC
# dfsitpay <- as.data.frame(sitpay)
# afc <- dudi.coa(dfsitpay, scannf = F, nf = 3)
# afc$tab
# 
# # Lien avec le Chi-2
# reschi <- chisq.test(sitpay)
# reschi
# reschi$expected # effectifs theoriques sous Hyp. independance
# # Effectif théorique pour celibataire X annuel
# 2113*855/6901
# 
# # Lien entre Chi-2 et AFC
# (dfsitpay - reschi$expected)/reschi$expected
# afc$tab
# 
# reschi$statistic
# reschi$statistic/sum(sitpay)
# sum(afc$eig)
# # La somme des valeurs propres de l'AFC est égale à la statistique du Chi-2
# 
# # Pondérations = frequences marginales 
# afc$cw
# apply(dfsitpay, 2, function(x) sum(x)/n)
# # Ex pour celibataire
# 855/6901 # 0.1238951
# 
# afc$lw
# apply(dfsitpay, 1, function(x) sum(x)/n)
# # Ex pour annuel
# 2113/6901 # 0.3061875
# 
# windows()
# scatter(afc, posieig="none")
# iner <- inertia.dudi(afc, row.inertia=T, col.inertia=T)
# iner






# ACM sur tableau ours ----

data(ours)
?ours
str(ours)
summary(ours)
# Toutes la variables sont des facteurs
# Les 8 premières sont des indicateurs de favorabilité à la réinstallation de l'ours
# de 1 (peu favorable) à 3 (très favorable)
# La variable 9 indique la date de disparition de l'ours dans la zone
# La variable 8 est le nom du département

# On réalise une ACM sur les 8 premières variables
acm <- dudi.acm(ours[, 1:8], scannf=F)
acm

# Rapports de corrélation entre variables et axes
# Identifier les variables qui forment les axes
acm$cr
# Représentation graphique
par(mfrow = c(1, 2), mar = c(5, 4, 2, 0))
barplot(acm$cr[, 1], horiz = TRUE, xlim = c(0, 1), names.arg = colnames(ours[1:8]),las = 1, main = "Axis 1", col = "lightblue", xlab = "Correlation ratio")
barplot(acm$cr[, 2], horiz = TRUE, xlim = c(0, 1), names.arg = colnames(ours[1:8]),las = 1, main = "Axis 2", col = "lightblue", xlab = " Correlation ratio ")
par(mfrow=c(1,1), mar=c(2,2,2,2))

# Représentation 2-D
windows()
par(mfrow=c(2,2))
s.arrow(acm$co, grid=F, sub="Modalités 8 variables", possub="topleft")
s.label(acm$li, sub="Zones", possub ="topleft")
s.class(acm$l1, ours$depart, col=rainbow(7), sub="Departements", possub ="topleft")
s.class(acm$l1, ours$citat, col=rainbow(7), sub="Disparition ours", possub="topleft")

# Représentation de l'ACM par scatter()
# Un graphe pour chaque variable, une couleur par modalité
windows()
scatter(acm, col = rep(c("black", "red3", "darkblue", "green"), 2))

# Représentation 1-D (à utiliser si axe 1 très prédominant)
score(acm, xax = 1)




# Jeu de données Doubs ----
# cf PDF Partie2-5_Ordination_Tableaux_Ecologiques, p.3
# Aides à l'interprétation pour les données spatialisées avec s.value()
# Comparaison ACP normée vs. ACM sur le tableau environnement
# Comparaison ACP centrée vs. AFC sur le tableau faunistique

data(doubs)
?doubs
doubs

# Carte de l'échantillonnage
xy <- doubs$xy
windows()
plot(xy, col = "blue", type = "l", lwd = 2)
s.label(xy, add.plot = T)
text(45, 10, "Amont", cex = 1.5, col = "red")
text(25, 110, "Aval", cex = 1.5, col = "red")


# Exercice : Doubs environnement, ACP normée ----

mil <- doubs$env
pca1 <- dudi.pca(mil, scannf=F, nf=2)
inertia.dudi(pca1, row.inertia=T, col.inertia=T)

par(mfrow=c(2,2))

s.corcircle(pca1$co)
s.label(pca1$li)

# Cartographie sommaire des coordonnées sur les axes
s.value(xy, pca1$li[, 1], sub = "Coordonnées sur axe 1")
s.value(xy, pca1$li[, 2], sub = "Coordonnées sur axe 2")

par(mfrow=c(1,1))

# Lien entre variables et premier axe de l'ACP
score(pca1)

# Représentation du tableau de données
# Tableau dans l'ordre original
table.value(pca1$tab) 
# Lignes et colonnes ordonnées selon leur coord sur axe1
table.value(pca1$tab, x=pca1$co[,1], y=pca1$li[,1])
# Lignes et colonnes ordonnées selon le rang de leur coord sur axe1
table.value(pca1$tab, x=rank(pca1$co[,1]), y=rank(pca1$li[,1]))  


#  Doubs environnement, ACM ----

# Découpage en classes des variables de milieu : transformation d'une var quantitative en var qualitative
milqual =as.data.frame(lapply(mil, 
                               function(x) 
                                 factor(cut(x, 
                                            breaks = unique(quantile(x, seq(0, 1, length.out = 5))), 
                                            include.lowest = T, labels=F))))
summary(milqual)

acmdoubs = dudi.acm(milqual, scannf = F, nf = 2)
scatter(acmdoubs, col = rep(c("cyan","blue","red","black"), 2))

score(acmdoubs)

s.label(acmdoubs$co)
s.label(acmdoubs$li)

# Comparaison ACP-ACM
par(mfrow=c(2,2))
s.label(pca1$li, sub="ACP")
s.label(acmdoubs$li, sub="ACM")
plot(pca1$li[,1],acmdoubs$li[,1], xlab="Axe1, ACP", ylab="Axe1, ACM")
plot(pca1$li[,2],acmdoubs$li[,2], xlab="Axe2, ACP", ylab="Axe2, ACM")
par(mfrow=c(1,1))


# Doubs poissons

poi <- doubs$fish
table.value(poi, csize = 0.4)

# Répartition de la Truite
par(mfrow=c(1,2))
s.value(xy, poi[, names(poi) == "Satr"], sub = "truite fario - squaresize", csub = 1.5, method = "squaresize")
s.value(xy, poi[, names(poi) == "Satr"], sub = "truite fario - grey levels", csub = 1.5, method = "greylevel" )
par(mfrow=c(1,1))

# Répartition de toutes les espèces - utilisation de n2mfrow()
par(mfrow = n2mfrow(ncol(poi)))
for (i in 1:ncol(poi)) s.value(xy, poi[, i], method = "greylevel",
                                     sub = names(poi)[i], csub = 2, cleg = 0)
par(mfrow=c(1,1))

# Répartition de l'abondance totale
abontot <- apply(poi, 1, sum)
abontot
s.value(xy, abontot , sub = "Abondance Totale", csub = 1.5)

# Répartition de la richesse (nombre d'espèces)
rich = apply(ifelse(poi > 0, 1, 0), 1, sum)
s.value(xy, rich, sub = "Richesse", csub = 2)


# Doubs poissons, ACP centrée ----

pca2 <- dudi.pca(poi,scale=F, scannf=F, nf=2)
scatter(pca2, posi="topright")
inertia.dudi(pca2, row.inertia=T, col.inertia=T)

s.label(pca2$co)
par(mfrow=c(2,2))
s.value(xy, pca2$li[, 1], sub = "Axe 1", csub = 2)
s.value(xy, pca2$li[, 2], sub = "Axe 2", csub = 2)
s.value(xy, apply(poi[, 7:27], 1, sum), sub = "faune cyprinicole",
        csub = 2, method = "greylevel")
s.value(xy, apply(poi[, 1:6], 1, sum), sub = "faune salmonicole",
        csub = 2, method = "greylevel")
par(mfrow=c(1,1))


# Doubs poissons, AFC ----

coa2 = dudi.coa(poi, scannf = F, nf = 2)
scatter(coa2, posi="bottomright")

# Comparaison entre l'ACP centrée et l'AFC de poi
windows()
par(mfrow=c(2,1))
scatter(pca2, posi="topright", sub="PCA Poissons Doubs", csub=1)
scatter(coa2, posi="bottomright", sub="COA Poissons Doubs", csub=1.2)
par(mfrow=c(1,1))
dev.off()

# ACM sur les données ours avec FactoMineR

res.mca <- MCA(ours,quali.sup=9:10)
res.mca
summary(res.mca)

# Graphiques
# Les modalités des variables actives
plot(res.mca,invisible=c("ind","quali.sup","quanti.sup"),cex=0.7)
# Les individus
plot.MCA(res.mca,invisible=c("var","quali.sup","quanti.sup"),cex=0.7)
# Modalité et individus superposés
plot(res.mca,invisible=c("quali.sup","quanti.sup"),cex=0.8)

dimdesc(res.mca)
plotellipses(res.mca, keepvar=1:8)
plotellipses(res.mca,keepvar="citat")
plotellipses(res.mca,keepvar="depart")

## Graphical interface
require(Factoshiny)
res <- Factoshiny(ours)

Investigate(res.mca)


# Classification automatique ----

# CAH : Classification ascendante hiérarchique ----
# Exemple des iris de Fisher
data(iris)
dim(iris)
names(iris)
str(iris)

a <- iris[,1:4] # sélection des 4 variables numériques
apply(a, MARGIN=2, FUN=mean)
apply(a, MARGIN=2, FUN=sd)
boxplot(a)


# Calcul de la matrice de distances euclidiennes entre individus
iris.dist <- dist(a, method="euclidean")
iris.dist


# classification hiérarchique, méthode par défaut : complete linkage
iris.hclust.lc <- hclust(iris.dist)  
iris.hclust.lc
# Tracé du dendrogramme (hang=-1 pour que toutes les branches soient alignées)
plot(iris.hclust.lc, hang=-1)
# On coupe l'arbre pour faire 3 groupes 
iris.k.lc <- cutree(iris.hclust.lc, k=3)
iris.k.lc

# Projection de ces groupes sur l'ACP du tableau iris 
iris.pca <- dudi.pca(a, scannf=F, nf=2)
s.class(iris.pca$li, as.factor(iris.k.lc), col=c(1,2,3))
s.chull(iris.pca$li, as.factor(iris.k.lc), optchull =1, cpoint=1, col=c(1,2,3))

# Pour comparer, on projette les variétés : setosa est bien identifiée par la CAH,
# En revanche versicolor et virginica sont moins bien distinguées
s.chull(iris.pca$li, as.factor(iris$Species), optchull =1, cpoint=1,col=c(1,2,3))

# On représente les 3 groupes par des couleurs différentes
# Et les variétés par des figurés différents
plot(iris.pca$li[,1],iris.pca$li[,2], 
     col=iris.k.lc, 
     pch=(1:3)[iris$Species]) 
legend(2, 2.8, c("I.setosa", "I.versicolor", "I.virginica"), pch=1:3)

# Tableau croisé Species X groupe
table(iris$Species, iris.k.lc)



# Comparaison de différentes méthodes d'agrégation ------

# lien simple (ou single linkage)
iris.hclust.ls <- hclust(iris.dist, method = "single")  
iris.hclust.ls
iris.k.ls <- cutree(iris.hclust.ls, k=3)
table(iris$Species, iris.k.ls)


# lien moyen (ou average linkage)
iris.hclust.lm <- hclust(iris.dist, method = "average")  
iris.hclust.lm
iris.k.lm <- cutree(iris.hclust.lm, k=3)
table(iris$Species, iris.k.lm)

# Méthode de Ward
iris.hclust.wa <- hclust(iris.dist, method = "ward.D2")  
iris.hclust.wa
iris.k.wa <- cutree(iris.hclust.wa, k=3)
table(iris$Species, iris.k.wa)


# Comparaison des dendrogrammes
windows()
par(mfrow=c(2,2))
plot(iris.hclust.lc, hang=-1, main="Complete linkage", xlab="", ylab="", sub="")
plot(iris.hclust.ls, hang=-1, main="Single linkage", xlab="", ylab="", sub="")
plot(iris.hclust.lm, hang=-1, main="Average linkage", xlab="", ylab="", sub="")
plot(iris.hclust.wa, hang=-1, main="Ward method", xlab="", ylab="", sub="")

# Comparaison des groupes sur plan 1-2 de l'ACP
windows()
par(mfrow=c(2,2))
s.chull(iris.pca$li, as.factor(iris.k.lc), optchull =1, cpoint=1,col=c(1,2,3), sub="Complete linkage")
s.chull(iris.pca$li, as.factor(iris.k.ls), optchull =1, cpoint=1,col=c(1,2,3), sub="Single linkage")
s.chull(iris.pca$li, as.factor(iris.k.lm), optchull =1, cpoint=1,col=c(1,2,3), sub="Average linkage")
s.chull(iris.pca$li, as.factor(iris.k.wa), optchull =1, cpoint=1,col=c(1,2,3), sub="Ward method")



# kmeans ----
# Sur les données brutes
# NB : il faut fixer a priori le nombre de classes souhaitées, ici 3

cl.iris <- kmeans(iris[,1:4], 3, nstart=5)
cl.iris
s.chull(iris.pca$li, as.factor(cl.iris$cluster), optchull =1, cpoint=1,col=c(1,2,3))
# Comparer les groupes obtenus avec ceux de votre voisin(e)...

# Si on répète plusieurs fois le kmeans() on obtient des résultats différents
windows()
par(mfrow=c(3,3))
for (i in 1:9) {
     cl.iris <- kmeans(iris[,1:4],3, nstart=1)
     print(100*cl.iris$betweenss/cl.iris$totss)
     s.chull(iris.pca$li, as.factor(cl.iris$cluster), optchull =1, cpoint=1,col=c(1,2,3))
}


# Pour résoudre ce problème, utiliser l'option nstart avec
# une valeur >1 :
windows()
par(mfrow=c(3,3))
for (i in 1:9) {
  cl.iris <- kmeans(iris[,1:4], 3, nstart=10)
  print(100*cl.iris$betweenss/cl.iris$totss)
  s.chull(iris.pca$li, as.factor(cl.iris$cluster), optchull =1, cpoint=1,col=c(1,2,3))
}


# Utiliser les coordonnées des individus dans l'ACP ---------
# (ici 2 axes conservés)
# CAH
# Calcul de la matrice de distance entre individus
iris.dist <- dist(iris.pca$li, method="euclidean")
iris.hclust.acp <- hclust(iris.dist, method="ward.D2")  
iris.hclust.acp
plot(iris.hclust.acp, hang=-1)
# On coupe l'arbre pour faire 3 groupes 
iris.k.acp <- cutree(iris.hclust.acp, k=3)
iris.k.acp
# Projection de ces groupes sur l'ACP du tableau iris 
s.chull(iris.pca$li, as.factor(iris.k.acp), optchull =1, cpoint=1, col=c(1,2,3))

# Avec Kmeans
cl.iris.acp <- kmeans(iris.pca$li, 3, nstart=10)
s.chull(iris.pca$li, as.factor(cl.iris.acp$cluster), optchull =1, cpoint=1,col=c(1,2,3))

# Avec FactoMineR et la fonction HCPC
res.pca <- PCA(iris[1:4], graph=F)
res.hcpc <- HCPC(res.pca)

?HCPC
# nb.clust pour choix du nombre de cluster, 
# nb.clust=0: cliquer sur le dendrogramme pour choisir le niveau de coupure
# nb.clust=-1 pour choix automatique 

# Retour sur jeu de données ours avec classification des zones sur les coordonnées de l'ACM -----
data(ours)
acmours <- dudi.acm(ours[,1:8], scannf=F, nf=6)

# CAH
dist.ours <- dist(acmours$li)
h.ours <- hclust(dist.ours, method="ward.D2")
plot(h.ours, hang=-1)
cl.ours <- as.factor(cutree(h.ours, k=3))
s.chull(acmours$li, cl.ours, optchull =1, cpoint=1,col=c(1,2,3))

# Kmeans
cl.ours.acm <- kmeans(acmours$li, 3, nstart=10)
cl.ours.acm
s.chull(acmours$li, as.factor(cl.ours.acm$cluster), optchull =1, cpoint=1,col=c(1,2,3))

table(cl.ours,as.factor(cl.ours.acm$cluster))

# FactoMineR
res.ours <- MCA(ours, quali.sup=9:10, graph=F)
res.hcpc.ours <- HCPC(res.ours, nb.clust=-1)


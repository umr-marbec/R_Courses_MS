#Analyse comparative Ebrié, Gambie, Saloum, Fatala, en saison humide et saison sèche

# Chargement des librairies ----
library(ade4)
library(FactoMineR)

# Définition de palettes de couleurs  ----
pal2=c("goldenrod","deepskyblue")
pal4=c("deeppink","forestgreen", "salmon","turquoise")
pal8=c("deeppink4","deeppink1", "forestgreen","chartreuse2","salmon4","salmon1","turquoise4","turquoise1")
palcatecol <- c("darkolivegreen4","darkolivegreen3","darkseagreen2"," aquamarine2 ","cadetblue1","cadetblue2","cadetblue3","cadetblue4")


# Exercice 1 : ACP de l'environnement ----

### Lecture des données environnement ----
Envir <- read.csv2("Envir_PPEAO.csv", row.names=1, stringsAsFactors = T)

### Exploration des variables environnementales
windows()
par(mfrow=c(3,2))
boxplot(Envir$DistEmb~Envir$SystSais,notch=F,varwidth=T,  
        xlab="", ylab="Distance Mouth", cex.lab=1.2,las=2, col=pal8, cex.ax=1.5)
boxplot(Envir$Prof~Envir$SystSais,notch=F,varwidth=T,  
        xlab="", ylab="Depth", cex.lab=1.2,las=2, col=pal8, cex.ax=1.5)
boxplot(Envir$Transp~Envir$SystSais,notch=F,varwidth=T,  
        xlab="", ylab="Transparency", cex.lab=1.2,las=2, col=pal8, cex.ax=1.5)
boxplot(Envir$SalSfce~Envir$SystSais,notch=F,varwidth=T,  
        xlab="", ylab="Surface salinity", cex.lab=1.2,las=2, col=pal8, cex.ax=1.5)
boxplot(Envir$TempSfce~Envir$SystSais,notch=F,varwidth=T,  
        xlab="", ylab="Surface temperature", cex.lab=1.2,las=2, col=pal8, cex.ax=1.5)


### ACP normée de l'environnement -----
#acpEnvir <- dudi.pca(Envir[,c(10:14)])

acpEnvir <- dudi.pca(Envir[,c(10:14)], scannf = F, nf=3)

# Exploration des valeurs propres
round(head(acpEnvir$eig/sum(acpEnvir$eig)*100),2)
round(head(cumsum(acpEnvir$eig/sum(acpEnvir$eig)*100)),2)
# Axe 1 = 40.1%   Axe 2 = 24.1%  Axe 3 = 17.5% => 81.7% sur les 3 premiers axes

par(mfrow=c(1,1))
barplot(acpEnvir$eig)

inertia(acpEnvir, col.inertia=T)

# Exploration des axes 1 et 2

windows()
par(mfrow=c(1,1))
scatter(acpEnvir, posi="bottomleft")

par(mfrow=c(2,2))
s.corcircle(acpEnvir$co)
add.scatter.eig(acpEnvir$eig,nf=3,xax=1,yax=2,posi=c("topleft"),ratio=1/4)
s.class(acpEnvir$li,Envir$Syst, col=pal4, sub="Coups de pêche par système")
s.class(acpEnvir$li,Envir$Saison, col=pal2, sub="Coups de pêche par Saison")
s.class(acpEnvir$li,Envir$SystSais, col=pal8, sub="Coups de pêche par système et Saison")

# # Exploration des axes 2 et 3
# par(mfrow=c(2,2))
# s.corcircle(acpEnvir$co, xax=1, yax=3)
# add.scatter.eig(acpEnvir$eig,nf=3,xax=1,yax=3,posi=c("bottomright"),ratio=1/4)
# s.class(acpEnvir$li,Envir$Syst, xax=1, yax=3, col=pal4, sub="Coups de pêche par système")
# s.class(acpEnvir$li,Envir$Saison, xax=1, yax=3, col=pal2, sub="Coups de pêche par Saison")
# s.class(acpEnvir$li,Envir$SystSais, xax=1, yax=3, col=pal8, sub="Coups de pêche par système et Saison")


# Exercice 2 : analyse tableau Faune  ------

### Lecture des données faune -----
Faune <- read.csv2("Faune_PPEAO.csv", row.names=1)

### Lecture des catégories écologiques et trophiques -----
Categ <- read.csv2("Categ_PPEAO.csv", stringsAsFactors = T)
table(Categ$catecol)
table(Categ$cat_troph,Categ$cat_troph_gibao)

### AFC de la faune -----
par(mfrow=c(1,1))
#afcFau1 <- dudi.coa(Faune)

afcFau1 <- dudi.coa(Faune, scannf=F, nf=7)

round(head(afcFau1$eig/sum(afcFau1$eig)*100),2)
round(head(cumsum(afcFau1$eig/sum(afcFau1$eig)*100)),2)

par(mfrow=c(1,1))
barplot(afcFau1$eig)

# Exploration des axes 1 et 2

# Représentation des espèces
windows()
par(mfrow=c(2,2))
s.label(afcFau1$co,xax=1,yax=2)
add.scatter.eig(afcFau1$eig,nf=4,xax=1,yax=2,posi=c("bottomleft"),ratio=1/4)
# Ellipse par catecol et cat_troph en couleur 
s.class(afcFau1$co, Categ$catecol, grid=F,col=palcatecol)
s.class(afcFau1$co, Categ$cat_troph_gibao, grid=F,col=rainbow(7))
s.class(afcFau1$co, Categ$cat_troph, grid=F,col=terrain.colors(8))

# Representation des coups de pêche
windows()
par(mfrow=c(2,2))
s.label(afcFau1$li, clabel=0.7)
s.class(afcFau1$li, Envir$Syst, col=pal4, sub="Coups de pêche par système")
s.class(afcFau1$li, Envir$Saison, col=pal2, sub="Coups de pêche par saison")
s.class(afcFau1$li, Envir$SystSais, col=pal8, sub="Coups de pêche par système et saison")
par(mfrow=c(1,1))



### AFC de la faune après transformation log(x+1) -----

# transformation log
Faulog <- log(Faune+1)

#afcFau <- dudi.coa(Faulog)

afcFau <- dudi.coa(Faulog, scannf=F, nf=4)

round(head(afcFau$eig/sum(afcFau$eig)*100),2)
round(head(cumsum(afcFau$eig/sum(afcFau$eig)*100)),2)

par(mfrow=c(1,1))
barplot(afcFau$eig)

# Exploration des axes 1 et 2

# Représentation des espèces
windows()
par(mfrow=c(2,2))
s.label(afcFau$co,xax=1,yax=2)
add.scatter.eig(afcFau$eig,nf=7,xax=1,yax=2,posi=c("topleft"),ratio=1/4)
# Ellipse par catecol et cat_troph en couleur 
s.class(afcFau$co, Categ$catecol, grid=F,col=palcatecol)
s.class(afcFau$co, Categ$cat_troph_gibao, grid=F,col=rainbow(7))
s.class(afcFau$co, Categ$cat_troph, grid=F,col=terrain.colors(8))

# Representation des coups de pêche
windows()
par(mfrow=c(2,2))
s.label(afcFau$li, clabel=0.7)
s.class(afcFau$li, Envir$Syst, col=pal4, sub="Coups de pêche par système")
s.class(afcFau$li, Envir$Saison, col=pal2, sub="Coups de pêche par saison")
s.class(afcFau$li, Envir$SystSais, col=pal8, sub="Coups de pêche par système et saison")



### ACP centrée des espèces -----
#acpFau <- dudi.pca(Faulog, scale=F)
acpFau <- dudi.pca(Faulog, scale=F, scannf=F, nf=4)

round(head(acpFau$eig/sum(acpFau$eig)*100),2)
round(head(cumsum(acpFau$eig/sum(acpFau$eig)*100)),2)
par(mfrow=c(1,1))
barplot(acpFau$eig)

# Exploration des axes 1 et 2

# Représentation des espèces
windows()
par(mfrow=c(2,2))
s.label(acpFau$co,xax=1,yax=2)
# Ellipse par catecol et cat_troph en couleur
s.class(acpFau$co, Categ$catecol, grid=F,col=palcatecol)
s.class(acpFau$co, Categ$cat_troph_gibao, grid=F,col=rainbow(7))
s.class(acpFau$co, Categ$cat_troph, grid=F,col=terrain.colors(8))

# L'ACP montre les espèces les plus abondantes
tot <- colSums(Faune)
tot[order(tot, decreasing = T)]

# Representation des coups de pêche
windows()
par(mfrow=c(2,2))
s.label(acpFau$li, clabel=0.7)
s.class(acpFau$li, Envir$Syst, col=pal4, sub="Coups de pêche par système")
s.class(acpFau$li, Envir$Saison, col=pal2, sub="Coups de pêche par saison")
s.class(acpFau$li, Envir$SystSais, col=pal8, sub="Coups de pêche par système et saison")



# Exercice 3 : Classification automatique des coups de pêche à partir de l'ACP de Envir ----

### Avec la fonction hclust -----

acpEnvir <- dudi.pca(Envir[,c(10:14)], scannf=F, nf=2)
d1 <- dist(acpEnvir$li)
h1 <- hclust(d1, method = "ward.D")
par(mfrow=c(1,1))
plot(h1, hang=-1)
k1.env <- cutree(h1,3)

par(mfrow=c(1,2))
s.label(acpEnvir$li)
s.chull(acpEnvir$li, as.factor(k1.env), optchull=1)

# Comparer avec système et saison
table(k1.env,Envir$SystSais)


### Avec la fonction kmeans -----

k2.env <- kmeans(acpEnvir$li,centers=3, nstart=10)

# Comparer avec système et saison
table(k2.env$cluster,Envir$SystSais)

# Comparaison avec la partition de hclust
par(mfrow=c(1,2))
s.chull(acpEnvir$li, as.factor(k2.env$cluster), optchull=1, sub="kmeans")
s.chull(acpEnvir$li, as.factor(k1.env), optchull=1, sub="CAH")
table(k1.env,k2.env$cluster)
# lignes du tableau=k1 / colonnes=k2


### Avec la fonction HCPC de FactoMineR -----
res.PCA <- PCA(Envir[,10:14], ncp=2, graph=F)
hcpc1 <- HCPC(res.PCA)
head(hcpc1$data.clust)
hcpc1$desc.var

plot(catdes(hcpc1$data.clust, num.var=6), cex.names=3)

table(k1.env,hcpc1$data.clust$clust)



# Exercice 4a : Analyse inter classes -----

### Sur l'ACP des données environnement ----
windows()

# Systeme
bcaEnvir1 <- bca(acpEnvir, Envir$Syst , scannf = FALSE)
summary(bcaEnvir1)
b1 <- randtest(bcaEnvir1)
b1
plot(b1)
plot(bcaEnvir1)

# Saison
bcaEnvir2 <- bca(acpEnvir, Envir$Saison , scannf = FALSE)
summary(bcaEnvir2)
b2 <- randtest(bcaEnvir2)
b2
plot(b2)
plot(bcaEnvir2)

# SystSais
bcaEnvir3 <- bca(acpEnvir, Envir$SystSais, scannf = FALSE)
summary(bcaEnvir3)
b3 <- randtest(bcaEnvir3)
b3
plot(b3)
plot(bcaEnvir3)


### Sur l'AFC des données faune (transformation log) -----
windows()

# Systeme
bcaFau1 <- bca(afcFau, Envir$Syst, scannf = FALSE)
summary(bcaFau1)
c1 <- randtest(bcaFau1)
c1
plot(c1)
plot(bcaFau1)

# Saison
bcaFau2 <- bca(afcFau, Envir$Saison, scannf = FALSE)
summary(bcaFau2)
c2 <- randtest(bcaFau2)
c2
plot(c2)
plot(bcaFau2)


# SystSais
bcaFau3 <- bca(afcFau, Envir$SystSais, scannf = FALSE)
summary(bcaFau3)
c3 <- randtest(bcaFau3)
c3
plot(c3)
plot(bcaFau3)



# Exercice 4b : Analyse intra classes -----

### Sur l'ACP des données environnement ----
windows()

# Systeme
wcaEnvir1 <- wca(acpEnvir, Envir$Syst , scannf = FALSE)
summary(wcaEnvir1)
plot(wcaEnvir1)

# Saison
wcaEnvir2 <- wca(acpEnvir, Envir$Saison , scannf = FALSE)
summary(wcaEnvir2)
plot(wcaEnvir2)

# SystSais
wcaEnvir3 <- wca(acpEnvir, Envir$SystSais, scannf = FALSE)
summary(wcaEnvir3)
plot(wcaEnvir3)


### Sur l'AFC des données faune (transformation log)-----
windows()

# Systeme
wcaFau1 <- wca(afcFau, Envir$Syst, scannf = FALSE)
summary(wcaFau1)
plot(wcaFau1)

# Saison
wcaFau2 <- wca(afcFau, Envir$Saison, scannf = FALSE)
summary(wcaFau2)
plot(wcaFau2)


# SystSais
wcaFau3 <- wca(afcFau, Envir$SystSais, scannf = FALSE)
summary(wcaFau3)
plot(wcaFau3)


# Exercice 5 : AFCVI Faune environnement -----

afcvi1 <- pcaiv(afcFau,Envir[,10:14], scannf = FALSE, nf = 2)
summary(afcvi1)

windows()
plot(afcvi1)

windows()
par(mfrow=c(2,2))
s.arrow(afcvi1$fa, sub="Loadings")
s.arrow(afcvi1$cor, sub="Correlation")
s.match(afcvi1$li, afcvi1$ls, sub="Scores and predictions")
s.label(afcvi1$c1, sub="Species")


# Approche point de vue 1 = CCA Ter Braak, 1986
windows()
par(mfrow=c(2,2))
s.arrow(afcvi1$fa)
s.label(afcvi1$co)
s.class(afcvi1$co, Categ$catecol)
s.label(afcvi1$l1)

# Approche point de vue 2 = AFCVI Lebreton, 1991
windows()
par(mfrow=c(2,2))
s.arrow(afcvi1$fa)
s.label(afcvi1$c1)
s.class(afcvi1$c1, Categ$catecol)
s.match.class(afcvi1$li, afcvi1$ls, Envir$SystSais)



# Avec la librairie vegan
library(vegan)
cca1 <- cca(Faune, scale(Envir[10:14]))
windows()
pl <- plot(cca1, type="n", scaling="sites", correlation=T)
text(cca1, dis="cn", scaling="sites", col="red")
with(Envir, points(pl, "site", pch=21, col="black", bg=SystSais))
text(pl, "sp", arrow=F, length=0.05, col="black", cex=0.6, xpd=TRUE)
with(Envir, legend("bottomleft", levels(SystSais), pch=21, pt.bg=1:8, bty="n"))



# Exercice 6 : Analyse de coinertie entre AFC faune et ACP envir -----

Faulog <- log(Faune+1)
afcFau <- dudi.coa(Faulog, scannf=F, nf=4)

# Refaire l'ACP de Envir avec les pondération des lignes de l'AFC de Faune
acpEnvir2 <- dudi.pca(Envir[,c(10:14)], row.w=afcFau$lw, nf=3, scannf=F)

coin1 <- coinertia(afcFau,acpEnvir2, nf=2, scannf=F)
plot(coin1)
summary(coin1)

# Les variables environnementales et les espèces
windows()
par(mfrow = c(2,2))
s.arrow(coin1$l1, clab = 1, xlim=c(-1,1), ylim=c(-1,1), sub="Environnement")
s.arrow(coin1$c1, clab = 0.7, sub="Espèces")
s.class(coin1$c1, Categ$catecol,xax=1,yax=2, col=palcatecol, sub="Especes par cat. ecol. ")
s.class(coin1$c1, Categ$cat_troph_gibao,xax=1,yax=2, col=rainbow(7), sub="Especes par cat. troph.")

# Les coups de peche vus par les deux tableaux
windows()
par(mfrow = c(2,2))
s.match(coin1$mX, coin1$mY, sub="Coups Faune --> Envir")
s.match.class(coin1$mX, coin1$mY, Envir$Syst, sub="Coups groupés par système")
s.match.class(coin1$mX, coin1$mY, Envir$Saison, sub="Coups groupés par saison")
s.match.class(coin1$mX, coin1$mY, Envir$SystSais, sub="Coups groupés par système et saison")


# Exercice 7 : Analyse STATIS sur Envir ----

wit1 <- withinpca(Envir[, 10:14], Envir$SystSais, scannf = FALSE)
kta1 <- ktab.within(wit1)
statis1 <- statis(kta1, scann = FALSE)
windows()
plot(statis1)
kplot(statis1)


# Interstructure
barplot(statis1$RV.eig)
s.corcircle(statis1$RV.coo)

# Compromis
barplot(statis1$C.eig)
s.arrow(statis1$C.li)

kplot(statis1)


# Statis sur poi
dudi1 <- dudi.pca(Faulog, scann = FALSE)
wit1 <- wca(dudi1, Envir$SystSais, scann = FALSE)
kta3 <- ktab.within(wit1)
kplot(sepan(kta3))
statis3 <- statis(kta3, scann = FALSE)
windows()
plot(statis3)
kplot(statis3, traj=T, arrow=F)


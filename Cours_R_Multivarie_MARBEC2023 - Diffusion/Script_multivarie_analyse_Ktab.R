#                                   #
# Script analyse multivari�e sous R #
# Troisi�me partie :                #
# Analyses K tableaux avec ade4     #
# cf document tdr68.pdf             #

library(ade4)
#library(adegraphics)

# Analyse triadique partielle : quand on a un cube de donnees ----
# fonction pta()

data(meaudret)

# Analyse intra-saisons
wit1 <- withinpca(df = meaudret$env, fac = meaudret$design$season, scaling = "partial", scannf = FALSE)

# df	: a data frame with quantitative variables
# fac	: a factor partitioning the rows of df in classes
# scaling	: a string of characters as a scaling option :
#   if "partial", the sub-table corresponding to each class is centred and normed.
#   if "total", the sub-table corresponding to each class is centred and the total table is then normed.
# scannf	: a logical value indicating whether the eigenvalues bar plot should be displayed
# nf	: if scannf FALSE, an integer indicating the number of kept axes

wit1

# Transformation de l'intra-saisons en objet "k-tableaux"
# c�d un tableau par saison avec les variables en lignes et les sites en colonnes
# Utilisation de la fonction ktab.within
kta1 <- ktab.within(dudiwit = wit1, colnames = rep(c("S1", "S2", "S3", "S4", "S5"), 4))

# dudiwit	: an objet of class within
# rownames : the row names of the K-tables (otherwise the row names of dudiwit$tab)
# colnames : the column names of the K-tables (otherwise the column names of dudiwit$tab)
# tabnames : the names of the arrays of the K-tables (otherwise the levels of the factor which defines the within-classes)

kta1

# Transposition du k-tableaux pour avoir les variables en colonnes et les sites en lignes
kta2 <- t(kta1)
kta2

# Optionnel (juste pour voir) : Analyses s�par�es de chaque tableau
sep1 <- sepan(kta2)
sep1
summary(sep1)
plot(sep1)
kplot(sep1)

# R�alisation de l'Analyse triadique partielle
pta1 <- pta(kta2, scann = FALSE)
pta1


# D�tail des �tapes de l'analyse

# 1) Interstructure ($RV)
# La matrice RV donne les corr�lations (vectorielles) entre tableaux
pta1$RV

# Valeurs propres de l'interstructure
barplot(pta1$RV.eig)

# Repr�sentation de la corr�lation entre les k tableaux
s.corcircle(pta1$RV.coo)


# 2) Compromis = tableau "moyen" sites X especes 

# Valeurs propres du compromis
barplot(pta1$eig)

# Repr�sentation des colonnes (ici variables) et des lignes (ici sites) du compromis
s.arrow(pta1$co)

s.label(pta1$li)

# Graphe g�n�rique qui reprend ces �l�ments
windows()
plot(pta1)

# 3) Trajectoires : projections des �l�ments des tableaux de d�part sur les axes du compromis 
# qui sert de r�f�rentiel commun pour comparer la structure des tableaux (ici saisons)

windows()
kplot(pta1)
kplot(pta1, which.graph=2)
kplot(pta1, which.graph=3)





# Analyse STATIS : quand les tableaux n'ont qu'une dimension en commun ----
# fonction statis()

# Exemple 1 : Friday87. Analyse STATIS des tableaux faune et du tableau milieu ----

data(friday87)
?friday87
names(friday87)
dim(friday87$fau) # 16 �chantillons et 91 taxons
dim(friday87$mil) # 16 �chantillons et 11 variables environnementales
friday87$fau.blo  # les 91 taxons se r�partissent en 10 blocs = groupes d'esp�ces 
                  # Par ex., les 11 premiers taxons appartiennent au groupe des Hemiptera

# On cr�e un dataframe (ici w1) qui concat�ne le tableau faune � 91 colonnes et le tableau milieu � 11 colonnes
# En faisant un centrage sur la faune et un centrage-r�duction sur le milieu
w1 <- cbind.data.frame(scale(friday87$fau,scale=F), scale(friday87$mil))
dim(w1)

# # Juste pour v�rifier la transformation des tableaux
# apply(w1, 2, FUN="mean") # moyennes � z�ro
# apply(w1, 2, FUN="sd")   # �cart type non �gal � 1 pour  la faune et �gal � 1 pour le milieu

# Cr�ation du Ktableau, cette fois-ci � partir du data.frame w1 avec ktab.data.frame
kta1 <- ktab.data.frame(w1,c(friday87$fau.blo,11), tabnames=c(friday87$tab.names,"Milieu"))
# df	: a data frame
# blocks	: an integer vector for which the sum must be the number of variables of df. Its length is the number of arrays of the K-tables
# rownames	: the row names of the K-tables (otherwise the row names of df)
# colnames	: the column names of the K-tables (otherwise the column names of df)
# tabnames	: the names of the arrays of the K-tables (otherwise "Ana1", "Ana2", ...)
# w.row	: a vector of the row weightings
# w.col	: a vector of the column weightings

kta1
# NB : On a pour chaque tableau les 16 �chantillons en ligne
# C'est la dimension commune � tous les tableaux, car les colonnes sont diff�rentes pour chaque tableau

# Analyses s�par�es simultan�es
sep1 <- sepan(kta1)
kplot(sep1)

# Analyse STATIS
statis1 <- statis(kta1, scannf=F)
statis1

# Graphique g�n�rique de STATIS
windows()
plot(statis1)

# 1. Interstructure
# Matrice RV des corr�lations entre les tableaux
print(statis1$RV, digits=2)

# Valeurs propres de l'interstructure
barplot(statis1$RV.eig)

# Repr�sentation de la corr�lation entre les k tableaux
s.corcircle(statis1$RV.coo)

# 2. Compromis
# Valeurs propres de l'analyse du compromis
barplot(statis1$C.eig)

# Repr�sentation des �chantillons sur les 2 premiers axes du compromis (statis1$C.li)
# Les �chantillons sont communs � tous les tableaux, c'est donc leur position moyenne
windows()
s.label(statis1$C.li)

# Repr�sentation des colonnes sur les 2 premiers axes du compromis (statis1$C.Co)
# Il y a 102 colonnes=91 taxons et 11 var. envir.
windows()
s.label(statis1$C.Co)

# Plus lisible : multifen�trage par tableau
windows()
kplot(statis1)


# # Repr�sentations graphiques des diff�rentes �tapes du Statis avec adegraphics
# 
# library(adegraphics)
# 
# # Interstructure
# 
# bc2 <- plotEig(statis1$RV.eig, yax=1, nf=1, pbackground.box=T, psub=list(text="Eigenvalue", cex=2), plot=F)
# cs2 <- s.corcircle(statis1$RV.coo,pbackground.box=F, plot=F )
# 
# ADEgS(list(cs2,bc2), rbind(c(0,0,1,1), c(0, 0.65, 0.35, 1)))
# 
# # Le tableau milieu n'est pas bien corr�l� avec tous les groupes
# # C'est OK avec Oligochaeta, moins avec Hydracarina ou Mollusca
# 
# 
# # Repr�sentation de l'analyse du compromis avec focus sur le tableau Milieu
# 
# bcC <- plotEig(statis1$C.eig, yax=2, nf=2, pbackground.box=T,
#                psub=list(text="Eigenvalues", cex=2), plot=F)
# s1C <- s.label(statis1$C.li, plabels.cex=1.5, plabels.optim=T, plot=F)
# ccC <- s.corcircle(statis1$C.Co[statis1$TC[,1]=="Milieu",],
#                    pbackground.box=F, plot=F)
# ADEgS(list(s1C, ccC,bcC),rbind(c(0,0,0.5,1), c(0.5, 0, 1, 1), c(0.35, 0, 0.65,0.2)) )
# 
# 
# # Repr�sentation de l'analyse du compromis avec focus sur un groupe faunistique, le tableau Hemiptera par exemple
# 
# bcC <- plotEig(statis1$C.eig, yax=2, nf=2, pbackground.box=T,
#                psub=list(text="Eigenvalues", cex=2), plot=F)
# s1C <- s.label(statis1$C.li, plabels.cex=1.5, plabels.optim=T, plot=F)
# ccC <- s.label(statis1$C.Co[statis1$TC[,1]=="Hemiptera",],
#                    pbackground.box=T, plot=F)
# ADEgS(list(s1C, ccC,bcC),rbind(c(0,0,0.5,1), c(0.5, 0, 1, 1), c(0.35, 0, 0.65,0.2)) )
# 


# # Exemple 2 : Friday87. Analyse STATIS des tableaux faune uniquement ----
# # NB : ACP sur les tableaux
# 
# # Cr�ation du Ktableau
# kta2 <- ktab.data.frame(friday87$fau,friday87$fau.blo)
# kta2
# 
# # Analyse STATIS
# statis2 <- statis(kta2, scannf=F)
# statis2
# 
# windows()
# plot(statis2)
# 
# windows()
# kplot(statis2)
# 

# # Exemple 3 : Variante STATIS-COA : AFC sur les tableaux faune ----
# 
# data(friday87)
# 
# ## Cr�ation du K-tableau 
# 
# # Transposition du tableau fau pour avoir les �chantillons en colonnes
# tfridayfau <- as.data.frame(t(friday87$fau))
# 
# # AFC de ce tableau transpos�. Identique � l'AFC du tableau non transpos� (cf coa0 ci-dessous)
# coa1 <- dudi.coa(tfridayfau,scannf=F,nf=4)
# scatter(coa1)
# 
# # coa0 <- dudi.coa(friday87$fau,scannf=F,nf=4)
# # scatter(coa0)
# 
# # Cr�ation d'un vecteur de longueur 91 donnant pour chaque taxon le groupe auquel il appartient
# # Ce n'est pas la m�me chose que fau.blo qui donne juste le nombre de taxo par groupe
# tfridayfac <- as.factor(c(rep("Hemiptera",11), rep("Odonata",7),rep("Trichoptera",13),
#                 rep("Ephemeroptera",4), rep("Coleoptera",13),rep("Diptera",22),
#                 rep("Hydracarina",4), rep("Malacostraca",3),rep("Mollusca",8),
#                 rep("Oligochaeta",6)))
# tfridayfac
# 
# # AFC intra-groupes
# wit1 <- wca(coa1, tfridayfac,scannf=F,nf=4)
# scatter(wit1)
# 
# # Transformation de l'AFC intra-groupes en multitableau d'AFCs
# ktabcoa1 <- ktab.within(wit1, tabnames=friday87$tab.names)
# ktabcoa1
# 
# # Analyses s�par�es simultan�es
# sepcoa1 <- sepan(ktabcoa1)
# plot(sepcoa1)
# 
# # Analyse STATIS-COA
# statiscoa1 <- statis(ktabcoa1, scannf=F)
# statiscoa1
# 
# windows()
# plot(statiscoa1)
# 
# windows()
# kplot(statiscoa1)
# 


# AFM : Analyse Factorielle Multiple (AFMULT - Escofier et Pag�s, 1994) avec ade4----
# mfa() 

library(FactoMineR)
library(ade4) 

data(friday87)

# La pr�paration du k-tableau est la m�me que pour le STATIS (Exemple 1)
# On cr�e un dataframe (ici w1) qui concat�ne le tableau faune � 91 colonnes et le tableau milieu � 11 colonnes
# En faisant un centrage sur la faune et un centrage-r�duction sur le milieu
w1 <- cbind.data.frame(scale(friday87$fau,scale=F), scale(friday87$mil))
dim(w1)

# Cr�ation du Ktableau, cette fois-ci � partir du data.frame w1 avec ktab.data.frame
kta1 <- ktab.data.frame(w1,c(friday87$fau.blo,11), tabnames=c(friday87$tab.names,"Milieu"))

kta1

# R�alisation de l'AFM

mfa1 <- mfa(kta1, scannf = FALSE)
mfa1

# Repr�sentation moyenne des 16 mares (lignes des k tableaux)
s.label(mfa1$li)

# Graphique r�sum�
plot(mfa1)

# Chaque graphique de ce plot fait s�par�ment

# repr�sentation moyenne des 16 mares avec en suppl�mentaire les mares pour les 10 tableaux
s.class(mfa1$lisup, mfa1$TL[,2], label=row.names(mfa1$tab), cellipse = 0)

# repr�sentation des 91 taxons
s.arrow(mfa1$co)

# repr�sentation des 10 tableaux
s.corcircle(mfa1$T4comp[mfa1$T4[,2]==1,])


# Repr�sentation des lignes (mares) et des colonnes (taxons) des tableaux
# Les mares sont repr�sent�es par des points et les taxons par leur label
kplot(mfa1)




# AFM : Analyse Factorielle Multiple (AFMULT - Escofier et Pag�s, 1994) avec FactoMineR ----

# MFA() 
# Premier argument = tableau de donn�es (ici friday87$fau 16 x 91 + friday87$mil)
# group = vecteur avec le nombre de variables dans chaque sous-tableau
# type = type de variable dans chaque sous-tableau
#      "c" = variable quantitative � centrer ;
#      "s" = variable quantitative � normer ; 
#      "n" = variable qualitative ; 
#      "f" = frequences d'une table de contingence)
# name.group= nom des sous-tableaux

fridaydf <- cbind(friday87$fau,friday87$mil)
fridaygroup <-c(friday87$fau.blo,11)
fridaytype <- c(rep("c", length(friday87$fau.blo)), "s")
fridaynames <- c(friday87$tab.names, "Milieu")

res.MFA = MFA(fridaydf,
              group=fridaygroup, 
              type=fridaytype, 
              name.group=fridaynames)
print(res.MFA)
summary(res.MFA)

# Graphique des individus
plot(res.MFA,choix="ind")
plot(res.MFA,choix="ind", partial ="all")

# Graphique des variables
plot(res.MFA,choix="var", partial ="all")



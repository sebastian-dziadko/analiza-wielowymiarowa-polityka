
install.packages("ggparliament")
library(ggparliament)
install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
library(readxl)
install.packages("factoextra")
library(factoextra)


dane1<-read_excel("C:/Users/48690/Desktop/AWCJ/Cechy nomin.xlsx")
dane1 <- as.data.frame(dane1)
dane1
df<-dane1[,-1]
rownames(df)<-c("Bosak","Czarzasty", "Duda", "Hołownia","Kaczyński", "Kosiniak - Kamysz","Trzaskowski","Tusk","Idealny" )
View(df)
tablica_danych<-as.matrix(df)
tablica_danych
suma<-addmargins(tablica_danych)
suma
rozkład_brzegowy<-prop.table(tablica_danych)
rozkład_brzegowy
sumawzg<-addmargins(rozkład_brzegowy)
sumawzg
profil_wierszowy_wzg<-prop.table(tablica_danych,margin=1)
profil_wierszowy_wzg
profil_wierszowy_wzg_suma<-addmargins(profil_wierszowy_wzg,margin=2,FUN=sum)
profil_wierszowy_wzg_suma
View(profil_wierszowy_wzg_suma)
profil_kolumnowy_wzg<-prop.table(tablica_danych,margin=2)
profil_kolumnowy_wzg
profil_kolumnowy_wzg_suma<-addmargins(profil_kolumnowy_wzg,margin=1,FUN=sum)
profil_kolumnowy_wzg_suma
View(profil_kolumnowy_wzg_suma)



matplot(t(profil_kolumnowy_wzg_suma), type = "l", xlab = "Cechy", ylab = "Skala profilu kolumnowego", col = 1:9, lty = 1, lwd = 2)



chi<-chisq.test(df)
chi #

View(chi)
chi$expected 
View(chi)


install.packages("DescTools")
library(DescTools)

TschuprowT(df) 
ContCoef(df) 
CramerV(df) 

C<-ContCoef(df)

w=9
k=15

cmax=((sqrt(k-1)/k))+(sqrt((w-1)/w))/2
cmax

ckor=C/cmax
ckor



install.packages("ca")
library(ca)
library(ggplot2)

danek<-ca(df,graph=FALSE)
danek
danek$sv 
plot(danek)
ev<-get_eigenvalue(danek)
ev
View(ev)
inercja<-sum(ev[,1])
inercja

fviz_screeplot(danek,addlabels=TRUE,ylim=c(0,50)) 


row<-get_ca_row(danek)
row
View(row)
View(danek)
danek$rowmass 
row$coord
row$cos2
rowSums(row$cos2[,1:2])  


row$contrib 
row$inertia 
sum(row$inertia)
(row$inertia/sum(row$inertia))*100 
col<-get_ca_col(danek)
col
View(col)
View(danek)
danek$colmass 
col$coord#wspolrzedne wariabtiw 2 cechy
col$cos2 #cos2 dla wzzystkich wymairow
colSums(col$cos2[,1:2]) #jakosc dla dwoch wymairow
col$contrib#bezwwladnosc po wymiarach, suma =
col$inertia #inercja dla waruiabntow cechy 12
(col$inertia/sum(col$inertia))*100

fviz_ca_biplot(danek,repel=TRUE)
#maja byc tablice, nazwanie osi














housetasks
dim(housetasks)

tablica_danych<-as.matrix(housetasks)
tablica_danych


suma<-addmargins(tablica_danych)
suma

rozkład_brzegowy<-prop.table(tablica_danych)
rozkład_brzegowy

sumawzg<-addmargins(rozkład_brzegowy)
sumawzg
#maksymalna mapa to 3
#Wyliczymy rozkl brzegowe
#sum to przecietny profil wierszowy/masa lub kolumnowy, ta ostatnia kolumna/wiersz
#czestosci brzegowe

profil_wierszowy_wzg<-prop.table(tablica_danych,margin=1)
profil_wierszowy_wzg
profil_wierszowy_wzg_suma<-addmargins(profil_wierszowy_wzg,margin=2,FUN=sum)
profil_wierszowy_wzg_suma

profil_kolumnowy_wzg<-prop.table(tablica_danych,margin=2)
profil_kolumnowy_wzg

profil_kolumnowy_wzg_suma<-addmargins(profil_kolumnowy_wzg,margin=1,FUN=sum)
profil_kolumnowy_wzg_suma
#profile nie na plakacie
#wykresy liniowe proponuje, na osi x cecyh(oga byc x1,x2, z boku w raporcie lub skroty).
#TABELARYCZNIE na pewno, graficznie tez mozna, lepiej zrobic. Wykresy liniowe, kazdy polityk ma swoj

chi<-chisq.test(housetasks)
chi #chi k wadrat, moze byc z poprawka na ciaglosc Yates
#MA BYC CHI - KWADRAT
View(chi)
chi$expected #to chyba teoretyczne, wszystkie powyzej 5, cpo jest wymagane, inne warunki tez sa spelnione (5,10)
View(chi)

install.packages("DescTools")
library(DescTools)

TschuprowT(housetasks) #Czuprow słaby, lepiej nie
ContCoef(housetasks) #kontyngencji
CramerV(housetasks) #Tez zanizony, najlepiej wspolczaynnik C skorygowany, mamy zrobic to

C<-ContCoef(housetasks)

w=13
k=4

cmax=((sqrt(k-1)/k))+(sqrt((w-1)/w))/2
cmax

ckor=C/cmax
ckor


install.packages("ca")
library(ca)
library(ggplot2)

danek<-ca(housetasks,graph=FALSE)
danek
danek$sv #Wartości osobliwe
plot(danek)
#U nas będzie 8 wartosci wlasnych i 8 posobliwych
ev<-get_eigenvalue(danek)
ev
inercja<-sum(ev[,1])
inercja
#inercja to 1.11
#wziela sume chyba inercje i choi - kwadrat. Chikwadrat przez n to dostaje 1.11
#WVariance jaki %A inercji wyjasniaja dane wymiary


fviz_screeplot(danek,addlabels=TRUE,ylim=c(0,50)) #osypisko, najwikesy wklad 1. mozna zrobic w pracy, 50 to granica

row<-get_ca_row(danek)
row
View(row)
View(danek)
danek$rowmass #masy wierszowe, czestosci brzegowe wierszy, sredni profil kolimnowy
row$coord #wspolrzedne wariantow cechy 1 (wiersze)
row$cos2 #cos2 dla wszystkich wymiarów #cos^2 to korelacja wariantu z wymairem, im blizej 1 tym lepeij reprezentowane
rowSums(row$cos2[,1:2])  #jakośc dla dwoch wymiarów. cos w trojwymiarze interpretowasc tylko jak niskie. 


row$contrib #bezwladnosc po wymiarach, suma=1. Inercja jest zmiennosc jej pokazany, jakdany wymiar opisuje zmiennosc, im wyzsza tym lepsza info
row$inertia #inercja dla wariantow cechy 1 9wiersze, wariancja
sum(row$inertia)
(row$inertia/sum(row$inertia))*100 #wzgleda bezwladnosc, inercja, suma=100% #9x15 wiec maks wymairow to 8. To jak PCA

col<-get_ca_col(danek)
col
View(col)
View(danek)
danek$colmass #masty kolumnowe, czestosci brzegowe
col$coord#wspolrzedne wariabtiw 2 cechy
col$cos2 #cos2 dla wzzystkich wymairow
colSums(col$cos2[,1:2]) #jakosc dla dwoch wymairow
col$contrib#bezwwladnosc po wymiarach, suma =
col$inertia #inercja dla waruiabntow cechy 12
(col$inertia/sum(col$inertia))*100

fviz_ca_biplot(danek,repel=TRUE)
#maja byc tablice, nazwanie osi

library(ggparliament)
pol_semicircle <- parliament_data(election_data = df,
                                  type = "semicircle",
                                  parl_rows = 10,
                                  party_seats = df$osoby)

ggplot(pol_semicircle, aes(x = x, y = y, colour = partia)) +
  geom_parliament_seats() + 
  draw_partylabels(type = "semicircle",
                   party_names = nazwa_partii,
                   party_seats = osoby,
                   party_colours = kolor) + 
  draw_totalseats(n = 460, type = "semicircle") +
  theme_ggparliament() +
  labs(title = "Polska, 2023") +
  scale_colour_manual(values = pol_semicircle$kolor, 
                      limits = pol_semicircle$partia) 




pol_semicircle <- parliament_data(election_data = df,
                                 type = "semicircle", # Parliament type
                                 parl_rows = 10,      # Number of rows of the parliament
                                 party_seats = df$osoby) # Seats per party

ggplot(pol_semicircle, aes(x = x, y = y, colour = partia)) +
  geom_parliament_seats() + 
  theme_ggparliament() +
  labs(title = "Polska, 2023") +
  scale_colour_manual(values = pol_semicircle$kolor, 
                      limits = pol_semicircle$partia) 


##########

pol_circle <- parliament_data(election_data = df,
                             type = "circle",
                             parl_rows = 10,
                             party_seats = df$osoby)

ggplot(pol_circle, aes(x = x, y = y, colour = partia)) +
  geom_parliament_seats() + 
  theme_ggparliament() +
  labs(title = "Polska, 2023") +
  scale_colour_manual(values = pol_circle$kolor, 
                      limits = pol_circle$partia) 

#########

pol_ob <- parliament_data(election_data = df,
                         type = "opposing_benches",
                         group = df$rząd,
                         parl_rows = 10,
                         party_seats = df$osoby)

ggplot(pol_ob, aes(x = x, y = y, colour = partia)) +
  geom_parliament_seats() + 
  theme_ggparliament() +
  labs(title = "Polska, 2023") +
  scale_colour_manual(values = pol_ob$kolor, 
                      limits = pol_ob$partia) + coord_flip()


#########

pol_classroom <- parliament_data(election_data = df,
                                type = "classroom",
                                parl_rows = 11,
                                party_seats = df$osoby)

ggplot(pol_classroom, aes(x = x, y = y, colour = partia)) +
  geom_parliament_seats() + 
  theme_ggparliament() +
  labs(title = "Polska, 2023") +
  scale_colour_manual(values = pol_classroom$kolor, 
                      limits = pol_classroom$partia) 

##############


pol_semicircle <- parliament_data(election_data = df,
                                 type = "semicircle",
                                 parl_rows = 10,
                                 party_seats = df$osoby)

ggplot(pol_semicircle, aes(x = x, y = y, colour = partia)) +
  geom_parliament_seats() + 
  geom_highlight_government(rząd == 1) +
  draw_majoritythreshold(n = 225, label = TRUE, type = "semicircle") +
  theme_ggparliament() +
  labs(title = "Polska, 2023") +
  scale_colour_manual(values = pol_semicircle$kolor, 
                      limits = pol_semicircle$partia) 


##########
install.packages("ggrepel")
library(ggrepel)

pol_semicircle <- parliament_data(election_data = df,
                                 type = "semicircle",
                                 parl_rows = 10,
                                 party_seats = df$osoby)

ggplot(pol_semicircle, aes(x = x, y = y, colour = partia)) +
  geom_parliament_seats() + 
  geom_highlight_government(rząd == 1) +
  geom_parliament_bar(colour = kolor, party = nazwa_partii, label = TRUE) +
  draw_majoritythreshold(n = 225, label = TRUE, type = "semicircle") +
  theme_ggparliament() +
  labs(title = "Udział partii") +
  scale_colour_manual(values = pol_semicircle$kolor, 
                      limits = pol_semicircle$partia) 


###############

pol_semicircle <- parliament_data(election_data = df,
                                 type = "semicircle",
                                 parl_rows = 10,
                                 party_seats = df$osoby)

ggplot(pol_semicircle, aes(x = x, y = y, colour = partia)) +
  geom_parliament_seats() + 
  draw_partylabels(type = "semicircle",
                   party_names = nazwa_partii,
                   party_seats = osoby,
                   party_colours = kolor) + 
  draw_totalseats(n = 450, type = "semicircle") +
  theme_ggparliament() +
  labs(title = "Polska, 2023") +
  scale_colour_manual(values = pol_semicircle$kolor, 
                      limits = pol_semicircle$partia) 




################################################################################
################################################################################

election_data

ru <- election_data %>%
  filter(country == "Russia" & year == 2016)
ru
View(ru)


ru_semicircle <- parliament_data(election_data = ru,
                                 type = "semicircle", # Parliament type
                                 parl_rows = 10,      # Number of rows of the parliament
                                 party_seats = ru$seats) # Seats per party

ggplot(ru_semicircle, aes(x = x, y = y, colour = party_short)) +
  geom_parliament_seats() + 
  theme_ggparliament() +
  labs(title = "Russia, 2016") +
  scale_colour_manual(values = ru_semicircle$colour, 
                      limits = ru_semicircle$party_short) 

##########

ru_circle <- parliament_data(election_data = ru,
                             type = "circle",
                             parl_rows = 10,
                             party_seats = ru$seats)

ggplot(ru_circle, aes(x = x, y = y, colour = party_short)) +
  geom_parliament_seats() + 
  theme_ggparliament() +
  labs(title = "Russia, 2016") +
  scale_colour_manual(values = ru_circle$colour, 
                      limits = ru_circle$party_short) 

#######

ru_ob <- parliament_data(election_data = ru,
                         type = "opposing_benches",
                         group = ru$government,
                         parl_rows = 10,
                         party_seats = ru$seats)

ggplot(ru_ob, aes(x = x, y = y, colour = party_short)) +
  geom_parliament_seats() + 
  theme_ggparliament() +
  labs(title = "Russia, 2016") +
  scale_colour_manual(values = ru$colour, 
                      limits = ru$party_short) +
  coord_flip()

######

ru_classroom <- parliament_data(election_data = ru,
                                type = "classroom",
                                parl_rows = 11,
                                party_seats = ru$seats)

ggplot(ru_classroom, aes(x = x, y = y, colour = party_short)) +
  geom_parliament_seats() + 
  theme_ggparliament() +
  labs(title = "Russia, 2016") +
  scale_colour_manual(values = ru$colour, 
                      limits = ru$party_short) 

######

ru_semicircle <- parliament_data(election_data = ru,
                                 type = "semicircle",
                                 parl_rows = 10,
                                 party_seats = ru$seats)

ggplot(ru_semicircle, aes(x = x, y = y, colour = party_short)) +
  geom_parliament_seats() + 
  geom_highlight_government(government == 1) +
  draw_majoritythreshold(n = 225, label = TRUE, type = "semicircle") +
  theme_ggparliament() +
  labs(title = "Russia, 2016") +
  scale_colour_manual(values = ru_semicircle$colour, 
                      limits = ru_semicircle$party_short) 

#######

ru_semicircle <- parliament_data(election_data = ru,
                                 type = "semicircle",
                                 parl_rows = 10,
                                 party_seats = ru$seats)

ggplot(ru_semicircle, aes(x = x, y = y, colour = party_short)) +
  geom_parliament_seats() + 
  geom_highlight_government(government == 1) +
  geom_parliament_bar(colour = colour, party = party_long, label = TRUE) +
  draw_majoritythreshold(n = 225, label = TRUE, type = "semicircle") +
  theme_ggparliament() +
  labs(title = "R") +
  scale_colour_manual(values = ru_semicircle$colour, 
                      limits = ru_semicircle$party_short) 

#########

ru_semicircle <- parliament_data(election_data = ru,
                                 type = "semicircle",
                                 parl_rows = 10,
                                 party_seats = ru$seats)

ggplot(ru_semicircle, aes(x = x, y = y, colour = party_short)) +
  geom_parliament_seats() + 
  draw_partylabels(type = "semicircle",
                   party_names = party_long,
                   party_seats = seats,
                   party_colours = colour) + 
  draw_totalseats(n = 450, type = "semicircle") +
  theme_ggparliament() +
  labs(title = "Russia, 2016") +
  scale_colour_manual(values = ru_semicircle$colour, 
                      limits = ru_semicircle$party_short) 

install.packages("readxl")
library(readxl)

dane <- read_excel("C:/Users/48690/Desktop/AWCJ/pytania_metr.xlsx") #pytania

install.packages("corrplot")
library(corrplot)
Spearman=cor(dane,method=c("spearman")) #method=c("pearson","kendall","spearman)
Spearman
kor<-round(Spearman,2)
kor



data<-read_excel("C:/Users/48690/Desktop/AWCJ/politycy_spea.xlsx")
Spearman1=cor(data,method=c("spearman")) #method=c("pearson","kendall","spearman)
Spearman1<-round(Spearman1,2)
corrplot(Spearman1)




corrplot(kor,method='square')
corrplot(kor,method='square',diag=FALSE,order='hclust',addrect = 3,rect.col = 'darkblue',rect.lwd = 3,tl.pos = 'd')
corrplot(kor,method='ellipse',order='AOE',type='upper')

#polittcy

dane1 <- read_excel("C:/Users/48690/Desktop/AWCJ/politycy_spea.xlsx") #pytania
dane2<-dane1[c(1:15,100:115)]
install.packages("corrplot")
Spearman=cor(dane2,method=c("spearman")) #method=c("pearson","kendall","spearman)
Spearman
kor<-round(Spearman,2)
kor

corrplot(kor,method='ellipse',order='AOE',type='upper')
corrplot(kor,method='square')
corrplot(kor,method='square',diag=FALSE,order='hclust',addrect = 3,rect.col = 'darkblue',rect.lwd = 3,tl.pos = 'd')
corrplot(kor,method='ellipse',order='AOE',type='upper')

install.packages("readxl")
library(readxl)

dane1<-read_excel("C:/Users/48690/Desktop/AWCJ/Pytania.xlsx")
dane1<-as.data.frame(dane1)
str(dane1)

dane<-dane1
str(dane)
dim(dane)



#colnames(dane)<-c("x1","x2","x3","x4","x5","x6","x7","x8","x9","x10","x11","x12",
#            "x13","x14","x15","x16","x17","x18","x19","x20","x21","X22")
#colnames(dane)<-c("x1","x2","x3","x4","x5","x6","x7","x8","x9","x10","x11","x12","x13","x14","x15")
#rownames(dane)<-c("Bosak","Czarzasty","Duda","Hołownia","Kaczyński","Kosiniak - Kamysz","Trzaskowski","Tusk","Idealny")
colnames(dane)<-c("x1","x2","x3","x4","x5","x6","x7","x8","x9","x10","x11","x12","x13","x14","x15","x16","x17","x18","x19","x20","x21","x22")

#,"x3","x4","x5","x6","x7","x8","x9","x10","x11","x12",
#"x13","x14","x15","x16","x17","x18","x19","x20","x21"
##rownames(dane)<-c("pensje","immunitet","obietnice","klimat","korupcja","nepotyzm","łaska","unia","Broke_Bod","Ahmad_Tea",
#     "Loyd_Tea","Teekane","Ramsey","Bastek","Sir_Roger")

dane

install.packages("corrplot")
library(corrplot)
#W pracy OD TEGO MOIEJDCA ZACZYNAM
library(RColorBrewer)
library(ggplot2)
Spearman=cor(dane,method=c("spearman")) #method=c("pearson, "kendall","Spearman")
#macierz speramanowska  wpracy
kor<-round(Spearman, 2)
kor #Chyba pytania, 22x22


corrplot(kor, method='square',diag=FALSE,order='hclust',
         addrect=3,rect.col='darkblue',rect.lwd=3,tl.pos='d')

corrplot(kor, method='ellipse',order='AOE',type='upper')
pairs(dane)
corrplot(kor,method='square')

#################
install.packages("psych")
library(psych)

#test Bartletta
cortest.bartlett(dane)
KMO(dane)
dane



danebg<-dane
danebg #bez zmiennej x3, bo <0,6

cortest.bartlett(danebg)
KMO(danebg) #KMO dla X10 zwiększyło się po usunięciu X2
danebg



df<-danebg
df
#df<-data.frame(df)
#df

Spearman=cor(df,method=c("spearman")) #method=c("pearson","kendall","spearman)
Spearman
kor<-round(Spearman,2)
kor
corrplot(kor,method='square')
corrplot(kor,method='square',diag=FALSE,order='hclust',addrect = 3,rect.col = 'darkblue',rect.lwd = 3,tl.pos = 'd')
corrplot(kor,method='ellipse',order='AOE',type='upper')

#ustalenie liczby czynnikow stosujac very simple structrue Analysis
VSS(df)
#ustalenie liczby czynnikow metoda Parallel Analysis
fa.parallel(df) 

fa.parallel(df,fa="fa",fm="pa",main="Scree Plot") #metoda principal axis
abline(h=1,col="green",lwd=2,lty=2) #macierz danych, Pearson
#W DOMU NARYSOWAC WYKRES VERY SIMPLE STRUCTURE VSS nwm o co
#tutaj 2 czynniki, bo jest monotonicznosc, moze byc tez wykres osypiska

fa.parallel(df,fa="fa",fm="ml",main="Scree Plot") #metoda najwiekszej wiatygodnsci
abline(h=1,col="green",lwd=2,lty=2) #macierz danych, Pearson

fa.parallel(kor,fa="fa",fm="pa",main="Scree Plot") #metoda Prinicpal Axis
abline(h=1,col="green",lwd=2,lty=2) #Spearman

fa.parallel(kor,fa="fa",fm="minres",main="Scree Plot") #metoda minres
abline(h=1,col="green",lwd=2,lty=2)  #Spearman



#######PCA#########
pc0<-principal(r=df,10,rotate="none",cor=TRUE)
pc0

pc1<-principal(r=df,3,rotate = "none",cor=TRUE)
pc1
fa.diagram(pc1)

pc2<-principal(r=df,3,rotate='varimax',cor=TRUE) 
pc2#
fa.diagram(pc2)

pc3<-principal(kor,3,rotate="none")
pc3
fa.diagram(pc3)

#PCA z macierza Spearmana i rotacją Varimax

pc4<-principal(kor,3,rotate="varimax")
pc4
fa.diagram(pc4)


ml0<-fa(kor,nfactor=2,rotate="none",fm="ml",residuals=TRUE) #bez rptacji
ml0 #

ml1<-fa(kor,nfactors=2,rotate="varimax",fm="ml",residuals=TRUE)
ml1 
fa.diagram(ml1) #3 cechy buduja 2 czynnik, niestety 7 nie, mozna by poprawic samemu, że mogłaby byc do dwoch czynnikow, ale na R wykres nie pokazuje tak
#przerywana linia to korelacja ujemna

#####Metoda minres#####
mm0<-fa(kor,nfactors=2,fm="minres",rotate="none")
mm0
fa.diagram(mm0)

mm1<-fa(kor,nfactors=2,fm="minres",rotate="varimax")
mm1
fa.diagram(mm1)

mm2<-fa(kor,nfactors=2,fm="minres",rotate="quartimax")
mm2
#sa varimin, equamax, 
mm3<-fa(kor, nfactors=2,fm="minres",rotate="varimin")
mm3

print(fa(kor,2,fm="minres",rotate="varimax")$loadings,cut=0.5)
fa.diagram(mm1)
fa.diagram(mm3)

mm4<-fa(df,nfactors=2,fm="minres",rotate="equamax")
mm4


####Metoda osi glownych
#metoda prinical axis
library(psych)

pa0<-fa(kor,nfactors=2,fm="pa",rotate="none")
pa0
fa.diagram(pa0)

pa1<-fa(kor,nfactors=2,fm="pa",rotate="varimax")
pa1
fa.diagram(pa1)

pa2<-fa(kor,nfactors=2,fm="pa",rotate="varimax")$loadings
pa2
print(fa(kor,nfactors=2,fm="pa",rotate="varimax")$loadings,cut=0.5)
#Test Alfa Cronbacha
dane2<-read_excel("C:/Users/48690/Downloads/Średniezamienione.xlsx")
dane2<-as.data.frame(dane2)
str(dane2)
View(dane2)

dz<-dane2
str(dz)
dim(dz)

colnames(dz)<-c("x1","x2","x3","x4","x5","x6","x7","x8","x9","x10","x11","x12","x13","x14","x15",
                "x16","x17","x18","x19","x20","x21","x22") 
View(dz)

#######
#dzielenie zbioru n 2 czesci, byc moze nie bedzie potrzby
#TRZEBA ZMIENIC DANE GDY KORELACJE UJEMNE
#Chyba stad wgrywanie tegfo dane2

czynnik1<-dz[,c(1,2,5,6,8,13,19)]
czynnik1

czynnik2<-dz[,c(7,9,12,20,22)]
czynnik2

czynnik3<-dz[,c(4,11,14,16,18,21)]
czynnik3


library(DescTools)
CronbachAlpha(czynnik1)
CronbachAlpha(czynnik2)
CronbachAlpha(czynnik3)

czyn1<-cor(czynnik1)
czyn1

czyn2<-cor(czynnik2)
czyn2

czyn3<-cor(czynnik3)
czyn3

par(mfrow=c(1,2))

corrplot(czyn1,method='square',diag=FALSE,order='hclust',addrect=3,rect.col='darkblue',rect.lwd=3,tl.pos='d',
         title='Czynniki 1',line=-1)
#WYKJRES MA B TC
corrplot(czyn2,method='square',diag=FALSE,order='hclust',addrect=3,rect.col='darkblue',rect.lwd=3,tl.pos='d',
         title='Czynniki 2',line=-1)
corrplot(czyn3,method='square',diag=FALSE,order='hclust',addrect=3,rect.col='darkblue',rect.lwd=3,tl.pos='d',
         title='Czynniki 3',line=-1)

par(mfrow=c(1,1))


install.packages("readxl")
install.packages("vegan")
install.packages("jaccard")
install.packages("proxy")
install.packages("tidyverse")
library(readxl)
library("vegan")
library("jaccard")      #włączyć go w packages
library("proxy")
library("tidyverse")

dane3 <- read_excel("C:/Users/48690/Desktop/AWCJ/0-1.xlsx")
dane3 <- as.data.frame(dane3)
str(dane3)
View(dane3)
jednostki = dane3[,1]
jednostki

dane4 <- dane3[-1]
dane4
row.names(dane4) <- jednostki
dane4
View(dane4)


df<- data.frame(dane4)

vegdist(df, method = "jaccard")
jaccard_prob <- 1-vegdist(df, method = "jaccard")
jaccard_prob

jaccard_odl <-vegdist(x=df, method = "jaccard")
jaccard_odl <- as.matrix(jaccard_odl) [1:9, 1:9]
jaccard_odl
jaccard_odl <- round((jaccard_odl), digits = 3)
jaccard_odl  

jaccard_podob <- 1 - jaccard_odl
jaccard_podob <- round((1-jaccard_odl), digits = 3)
jaccard_podob



#wsp podobieństwa Sokala Michenera
library(proxy)
df <- data.frame(dane4)
df
sm_podob <- simil(df, y = NULL, method = NULL, diag = FALSE, upper = FALSE,
                  pairwise = FALSE, by_rows = TRUE, convert_distances = TRUE,
                  auto_convert_data_frames = TRUE)
sm_podob
sm_podob <- as.matrix(sm_podob) [1:9, 1:9]
diag(sm_podob) = 1
sm_podob


#wizualizacja macierzy odległości i podobieństwa jaccarda

install.packages("factoextra")
library(factoextra)
jaccard_odl <- get_dist(df, method = "binary", stand = FALSE)
jaccard_podob = 1 - jaccard_odl
jaccard_podob
fviz_dist(jaccard_odl,order=FALSE,gradient = list(low = "yellow", mid="pink", high = "red"))
fviz_dist(jaccard_podob, order=TRUE)
fviz_dist(jaccard_podob, order=FALSE,
          gradient = list(low = "yellow", mid="pink", high = "red"))
fviz_dist(jaccard_podob, order=FALSE,
          gradient = list(low = "ivory", mid="lightblue", high = "midnightblue"))

library(corrplot)
jaccard_odl <- as.matrix(jaccard_odl)
jaccard_podob <- as.matrix(jaccard_podob)
par(mfrow = c(1,2))
corrplot(jaccard_odl, order = "AOE")
corrplot(jaccard_podob, order = "AOE")
par(mfrow = c(1,1))



#wizualizacja macierzy odległości i podobieństwa SM
df <- data.frame(dane4)
df
sm_podob <- simil(df, y = NULL, method = NULL, diag = FALSE, upper = FALSE,
                  pairwise = FALSE, by_rows = TRUE, convert_distances = TRUE,
                  auto_convert_data_frames = TRUE)
sm_podob

fviz_dist(sm_podob, order=FALSE,
          gradient = list(low = "seashell", mid="pink", high = "red"))
fviz_dist(sm_podob, order=FALSE,
          gradient = list(low = "ivory", mid="lightblue", high = "midnightblue"))

sm_odl<-simil(df, y = NULL, method = NULL, diag = FALSE, upper = FALSE,
              pairwise = FALSE, by_rows = TRUE, convert_distances = TRUE,
              auto_convert_data_frames = TRUE)
fviz_dist(sm_odl, order=FALSE,
          gradient = list(low = "seashell", mid="pink", high = "red"))


install.packages("cluster")
library("cluster")

install.packages("dendextend")
library("dendextend")

odl <- dist(dane4, method = "Jaccard")
dend1 <- hclust(odl, method = "complete")
View(dend1)



dend1$height
plot(dend1)
plot(dend1, hang=-1)
grupy <- cutree(dend1, k=3)
grupy
rect.hclust(dend1, k=3, border ="turquoise")



### Gower


dane1 <- read_excel("C:/Users/48690/Desktop/AWCJ/0-1.xlsx")
dane <- as.data.frame(dane1)

jednostki = dane[,1]
dane <- dane[,-1]
row.names(dane) <- jednostki
dane <- dane

dane$płeć <- factor(x = dane$płeć, levels = c("K", "M"))
dane$wyksz <- factor(x = dane$wyksz, levels = c("S", "W"))
dane$ocena <- factor(x = dane$ocena, levels = c("BW", "W", "N"))
dane$dodatki <- factor(x = dane$dodatki, levels = c("T", "N"))
dane$palenie <- factor(x = dane$palenie, levels = c("T", "N"))


daisy(dane, metric = c("gower"))
gower_odl <- daisy(dane, metric = c("gower"))
gower_odl <- as.matrix(gower_odl) [1:9, 1:9]
gower_odl <- round((gower_odl), digits = 3)
gower_odl

odl <- dist(dane, method = "Gower")
dend1 <- hclust(odl, method = "complete")
View(dend1)
plot(dend1)
plot(dend1, hang=-1)
grupy <- cutree(dend1, k=4)
grupy
rect.hclust(dend1, k=3, border ="turquoise")
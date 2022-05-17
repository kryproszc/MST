library(PerformanceAnalytics)
library(quantmod)
library(car)
library(MASS)
library(data.table)
library(mnormt)
library(tseries)
library(PerformanceAnalytics)
library(igraph)
library(emstreeR)
############ ANALIZA MARKOWITZA  ##################


dane<-NULL
name<-list.files(pattern="*.csv")
name # wypisuje nazwy społek

# Tworze wktor, który zawiera trzy pierwsze litery nazw  spółek z wielkich liter
names<-toupper(substr(name,1,3))

#Tworzymy tablicę w której pierwsza kolumna to daty a kolejne to ceny zamknięcia akcji 
for(i in 1:length(name)){
  dane[[i]] <-read.table(name[i],header=TRUE,sep = ",", dec = ".")[,c(1,5)];
  colnames(dane[[i]])<-c("Date",toupper(substr(name[i],1,3)))} 
# nadaje pierwszej  kolumnie nazwę "Date", 
# a w pozostałych ustawiam trzy pierwsze litery sktótów nazw spółek  

# Sklejamy dane w całość
dane_all<-merge(dane[[1]],dane[[2]],by="Date")
for(i in 3:length(name)){ dane_all<-merge(dane_all,dane[[i]],by="Date") }

# Usuwam z tablic kolumnę zawierającą daty
dane=dane_all[,-1]
# Wyliczam proste stopy zwrotu 
return_all = apply(dane_all[-1], 2, function(x) diff(x)/x[-length(x)])

# Robie proszczenie tablicy
return_all=simplify2array(return_all)

# Wyliczam logarytmiczne stopy zwrotu
logreturn_all=(log(dane[-1,])-log(dane[-nrow(dane) ,]))
logreturn_all=simplify2array(logreturn_all)

# Wyliczam wektor średnich dla prostych stóp zwrotu
mu_all=colMeans(return_all)

# Wyliczam macierz kowarincji dla prostych stóp zwrotu 
cov_all=cov(return_all)


# Wczytuje dane wig_20 do zmiennej wig_20
wig_banki<-read.csv("wig20_d.csv",header=TRUE,sep=",",dec=".")
# Wczytuje do zmiennej wig_banki ceny zamkniecia 
wig_banki<-cbind(wig_banki=wig_banki[,5])

#  wektor prostych stóp zwrotu dla wig_banki 
return_wig<-cbind(wig_banki=(apply(wig_banki, 2, function(x) diff(x)/x[-length(x)])))
odchylenie_standardowe<-sqrt(diag(cov_all))
Średnia_ze_stóp_zwrotu<-mu_all
# Wyznaczamy punkty brzegowe
point_edge<-t(rbind(odchylenie_standardowe,Średnia_ze_stóp_zwrotu))# transponujemy 
# Tworze wykres punktów brzegowych               
plot(point_edge,ylab="Średnia ze stóp zwrotu",xlab="Odchylenie standardowe",
     main="Portfele brzegowe",col="blue",pch=20,cex=2,xlim=c(0,0.08),ylim=c(-0.020,0.020))

# Nanosze na wykres 100 portfeli z losowymi nieujemnymi wagami, które losuje z rozkładu jednostajnego
n=length(dane)
for(i in 1:100){wagi=runif(n)^4
wagi=wagi/sum(wagi)
waga_mu=mean(return_all%*%wagi)
waga_var=var(return_all%*%wagi)
points(sqrt(waga_var),waga_mu,col="black")                
}  

# Obliczam granica efektywności Markowitza
# different_means to wektor srednich w kolejności od najmniejszej do największej
different_means=seq(from=min(mu_all),to=max(mu_all),length=120) 
# Tworze pusty wektor, do którego będziemy wpisywać minimalne wariancje
opt_var=NULL
# Obliczam portfel o minimalnym odchyleniu standardowym
for(i in 1:length(different_means)) {
  opt_var=c(opt_var, portfolio.optim(return_all,pm=different_means[i],shorts=FALSE)$ps)
  # portfolio.optim()$ps zwraca odchylenie standardowe stopy zwrotu portfela
}

# Tworze tablice eff_frontier, gdzie pierwsza kolumna odpowiada średnim, a druga  wariancjom
eff_frontier=cbind(different_means,opt_var)

# Nanosze krzywą na wykres 
lines(x=eff_frontier[,2],y=eff_frontier[,1],col="green",lw=5)
# Wybieram portfel o minimalnej wariancji
wallet_min=which.min(eff_frontier[,2])

# Nanosze portfel o minimalnej wariancji na wykres
points(eff_frontier[wallet_min,2],eff_frontier[wallet_min,1],col="red",pch=20,cex=2)

# Tworze zmienną opt_weight, która zawiera wagi portfela o minimalnej wariancji
A=mu_all%*%cov_all%*%rep(1,n)
C=rep(1,n)%*%cov_all%*%rep(1,n)
mu_opt=A/C

opt_weight=portfolio.optim(return_all,pm=mu_opt,shorts=F)$pw 
# portfolio.optim()$pw zwraca wektor z wagami portfela o minimalnej wariancji
opt_weight
spółki<-c("AAL","AAP","BA","IRB","NFL","TSL")

wagi<-t(rbind(spółki,opt_weight))

# Licze wskaźnik Sharpe'a dla każdego z portfeli na krzywej efektywności ze stopa wolna od ryzyka  3,5%
r_free=0.035/252

#  funkcja (SR), która liczy wskaźnik Sharpe'a
SR=function(ret,r=r_free){
  (mean(ret-r))/sd(ret-r)}
# Tworze pustą listę wallet do której wpisze wartości portfeli z krzywej efektywności
wallet<-list()

# Tworze pusty wektor do którego wpisze wartości  wskaźników Sharpe'a
eff_frontier_sharpe<-c()

# Wpisuje wartości do naszych pustych zmiennych
for(i in 1:length(different_means)){
  weights=portfolio.optim(return_all,pm=different_means[i],shorts=FALSE)$pw 
  # wagi portfel o minimalnym odchyleniu standardowym
  wallet[[i]]=return_all%*%weights
  eff_frontier_sharpe<-c(eff_frontier_sharpe,(SR(wallet[[i]],r_free)))
}

# Nanosze na wykres  styczny do krzywej Markowitza korzystając 
max_sharpe=eff_frontier_sharpe[which.max(eff_frontier_sharpe)]
abline(r_free,max_sharpe,col="black")

#nanosze punkt stycznosci
points(eff_frontier[which.max(eff_frontier_sharpe),2],
       eff_frontier[which.max(eff_frontier_sharpe),1],col="blue",pch=20,cex=2)

# Licze wskaźnik Sharpe'a dla indeksu wig20 
wig_banki_sharpe=SR(return_wig,r_free)
wig_banki_sharpe 

# Licze portfel o minimalnej wariancji i jego wskaźnik Sharpe'a
wallet_min_var=return_all%*%portfolio.optim(return_all,pm=eff_frontier[wallet_min,1], shorts=FALSE)$pw
wallet_min_var_sharpe=SR(wallet_min_var,r_free)  
wallet_min_var_sharpe

Sharp_dla_portfela_z_wagami_z_Marcowitza<-wallet_min_var_sharpe
# Licze portfel w którym inwestujemy w każdą spółkę 1/13 kapitału i jego 
# wskaźnik Sharpe'a

wallet_part=return_all%*%rep(1/n,n)
wallet_part_sharpe=SR(wallet_part,r_free)


wig_banki_sharpe
wallet_min_var_sharpe
wallet_part_sharpe

# konstruuje minilne drzewo rozpinajace
out2 <- ComputeMST(cov_all, verbose = FALSE)
out2
plot(out2, col.pts = "red", col.segts = "blue")

links=data.frame(
  source=c("PEO","KTY", "ENA", "MBK", "EUR","DNP", "EUR", "CPG", "DNP","BIO", "ART", "MBK"),
  target=c("PZU","PZU", "PZU", "PEO", "KTY","PZU","RON", "RON", "TOR ","DNP", "ENA", "RBW")
)
network <- graph_from_data_frame(d=links, directed=F) 

deg <- degree(network, mode="all")

plot(network, vertex.size=deg*6, vertex.color=rgb(0.1,0.7,0.8,0.5) )

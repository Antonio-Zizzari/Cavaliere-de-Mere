cavaliere.merè<-function(prove){
  dado<-c(1:6)
  esperimento.A<-0
  esperimento.B<-0
  for(i in 1:prove){ #ripeto l'esperimento 100.000 volte
    l<-sample(dado,size=4,replace=TRUE) #simulo il lancio di un dado 4 volte
    ris<-sum(l==6) #conto il numero di 6 usciti
    if(ris>0) #controllo se nei 4 lanci c'è stato un 6
      esperimento.A<-esperimento.A+1 #aumento di 1 la variabile dei casi favorevoli al prob. A
    #simulo il lancio delle 2 coppie di dadi 24 volte
    tmp.x<-sample(dado,size=24,replace=TRUE)
    tmp.y<-sample(dado,size=24,replace=TRUE)
    tmp.xy<-tmp.x+tmp.y  #calcolo la somma per controllare se sono 2 dadi da 6
    ris<-sum(tmp.xy==12) #conto il numero di doppi 6 usciti
    if(ris>0) #controllo se nei 24 lanci c'è stato un doppio 6
      esperimento.B<-esperimento.B+1 #aumento di 1 la variabile dei casi favorevoli al prob. B
  }
  x<-cat(esperimento.A,esperimento.B,esperimento.A/prove,esperimento.B/prove,"\n")
}

cavaliere.merè(100)
cavaliere.merè(1000)
cavaliere.merè(10000)
cavaliere.merè(20000)


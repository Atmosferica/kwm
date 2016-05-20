#Metodo di Kwee-van Woerden
#segnale di prova
x<-c(-1000:1000)
noise<-runif(2001)
y<-x*x+500000*noise

#inizio dell'algoritmo
minimi<-NULL
differenze<-NULL

estr1<- -300
estr2<- 600

xn<-x[which(x>=estr1&x<=estr2)]
yn<-y[which(x>=estr1&x<=estr2)]

min<-(max(xn)+min(xn))*0.5
for(i in 1:5000){
  ya<-y[which(x>=estr1&x<=min)]
  ydiff<-ya-y[which(x>=min&x<=estr2)]
  if(mean(ydiff)<0){
    min<-min-0.1
    estr1<-estr1-0.1
    estr2<-estr2-0.1
  }else
  {
    min<-min+0.1
    estr1<-estr1+0.1
    estr2<-estr2+0.1
  }
  minimi<-c(minimi,min)
  differenze<-c(differenze,mean(ydiff))
  
}

plot(x,y,t='l')
lines(x[which(x>=estr1&x<=estr2)],y[which(x>=estr1&x<=estr2)],col='red')
abline(v=min, lwd=4, col='green')

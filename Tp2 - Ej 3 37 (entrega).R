#3.35 Sea Sn = sum(Xi) de i=1 a n, donde las Xi son un conjunto de variables i.i.d.
#Sea Sn* su correspondiente variable estandarizada. En cada uno de los siguientes casos,
#simular 100000 valores de Sn*, computar y graficar la funcion de distribucion empirica.
#Comparar la grafica obtenida con la de la funcion de distribucion de una variable N(0,1)

sim<-100000 #Cantidad de simulaciones

#b) Xi es Bernoulli de parametro p = 1/2. Analizar los casos de n = 10, 30
p=1/2

bernoulli <- function(){
  cantexito<-p*10
  cantfallo<-10-cantexito
  valores<-c(rep(1, cantexito),rep(0,cantfallo))
  
  return(sample(valores,1))
} #Agarra un valor de bernoulli (0 / 1) teniendo en cuenta que p = 0.5


##Con n=10
#Simulo 100.000 valores de Sn*
Sn_est_Values<-c() #Para guardar valores de Sn*

for (i in (1:sim)){
  ##Simulo Sn y la estandarizo
  n<-10
  Sn<- c()
  for (i in (1:n)){
    Sn<-c(Sn, bernoulli())
  } #Obtengo vector con valores de X
  
  Sn_est<-(sum(Sn)+mean(Sn))/ var(Sn) #Estandariza Sn
  
  Sn_est_Values<-c(Sn_est_Values, Sn_est) #Lo almacena con los Sn*
}

#Grafico
plot(sort(Sn_est_Values), ylab="Sn*",xlab = "Simulaciones")
x<- sort(Sn_est_Values)/1000 #Divido por 1000 ya que no puedo analizar valores mayores a 1 como variables ~N(0,1)
lines(qnorm(x,0, 1)*(100)+230, col="lightgreen", lwd=3) #Le sumo 230 a los valores de qnorm para poder superponerlo en el grafico


##Con n=30
#Simulo 100.000 valores de Sn*
Sn_est_Values<-c() #Para guardar valores de Sn*

for (i in (1:sim)){
  ##Simulo Sn y la estandarizo
  n<-30
  Sn<- c()
  for (i in (1:n)){
    Sn<-c(Sn, bernoulli())
  }
  
  Sn_est<-(sum(Sn)+mean(Sn))/ var(Sn) #Estandariza Sn
  
  Sn_est_Values<-c(Sn_est_Values, Sn_est) #Lo almacena con los Sn*
}

plot(sort(Sn_est_Values), ylab="Sn*",xlab = "Simulaciones")
x<- sort(Sn_est_Values)/1000 #Divido por 1000 ya que no puedo analizar valores mayores a 1 como variables ~N(0,1)
lines(qnorm(x,0, 1)*(100)+215, col="lightblue", lwd=3)

#Conclusiones: cuanto mas valores considere en la sumatoria, más se parece el gráfico de los valores estandarizados con la funcion de probabilidad acumuladada de una variable aleatoria x ~ N(0,1)





#c) Xi es Bernoulli de parametro p = 0.001. Analizar los casos de n = 30, 100, 500
p=0.001 #1/1000

bernoulli <- function(){
  cantexito<-p*1000
  cantfallo<-1000-cantexito
  valores<-c(rep(1, cantexito),rep(0,cantfallo))
  
  return(sample(valores,1))
} #Agarra un valor de bernoulli (0 / 1) teniendo en cuenta que p = 0.001


##Con n=30
#Simulo 100.000 valores de Sn*
Sn_est_Values<-c() #Para guardar valores de Sn*

for (i in (1:sim)){
  ##Simulo Sn y la estandarizo
  n<-30
  Sn<- c()
  for (i in (1:n)){
    Sn<-c(Sn, bernoulli())
  } #Obtengo vector con valores de X
  
  Sn_est<-(sum(Sn)+mean(Sn))/ var(Sn) #Estandariza Sn
  
  Sn_est_Values<-c(Sn_est_Values, Sn_est) #Lo almacena con los Sn*
}

#Grafico
x<-0
plot(sort(Sn_est_Values), ylab="Sn*",xlab = "Simulaciones")
x<- sort(Sn_est_Values)/1000 #Divido por 1000 ya que no puedo analizar valores mayores a 1 como variables ~N(0,1)
lines(qnorm(x,0, 1)*(100)+217.63, col="darkorchid", lwd=3) #Le sumo 353.15 para poder superponer el grafico



##Con n=100
#Simulo 100.000 valores de Sn*
Sn_est_Values<-c() #Para guardar valores de Sn*

for (i in (1:sim)){
  ##Simulo Sn y la estandarizo
  n<-100
  Sn<- c()
  for (i in (1:n)){
    Sn<-c(Sn, bernoulli())
  } #Obtengo vector con valores de X
  
  Sn_est<-(sum(Sn)+mean(Sn))/ var(Sn) #Estandariza Sn
  
  Sn_est_Values<-c(Sn_est_Values, Sn_est) #Lo almacena con los Sn*
}

#Grafico
plot(sort(Sn_est_Values), ylab="Sn*",xlab = "Simulaciones")
x<- sort(Sn_est_Values)/1000 #Divido por 1000 ya que no puedo analizar valores mayores a 1 como variables ~N(0,1)
lines(qnorm(x,0, 1)*(100)+228.57, col="orange", lwd=3)



##Con n=500
#Simulo 100.000 valores de Sn*
Sn_est_Values<-c() #Para guardar valores de Sn*

for (i in (1:sim)){
  ##Simulo Sn y la estandarizo
  n<-500
  Sn<- c()
  for (i in (1:n)){
    Sn<-c(Sn, bernoulli())
  } #Obtengo vector con valores de X
  
  Sn_est<-(sum(Sn)+mean(Sn))/ var(Sn) #Estandariza Sn
  
  Sn_est_Values<-c(Sn_est_Values, Sn_est) #Lo almacena con los Sn*
}

#Grafico
x<-0
plot(sort(Sn_est_Values), ylab="Sn*",xlab = "Simulaciones")
x<- sort(Sn_est_Values)/1000 #Divido por 1000 ya que no puedo analizar valores mayores a 1 como variables ~N(0,1)
lines(qnorm(x,0, 1)*(100)+500.75, col="red", lwd=3)

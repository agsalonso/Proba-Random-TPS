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
  
  Sn_est<-(Sn - mean(Sn))/sd(Sn) #Estandariza Sn
  
  Sn_est_Values<-c(Sn_est_Values, Sn_est) #Lo almacena con los Sn*
}

#Grafico
plot(sort(Sn_est_Values), ylab="Sn*",xlab = "Simulaciones")
x<- sort(Sn_est_Values) 
lines(pnorm(x,0, 1), col="lightgreen", lwd=3)


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
  
  Sn_est<-(Sn - mean(Sn))/sd(Sn) #Estandariza Sn
  
  Sn_est_Values<-c(Sn_est_Values, Sn_est) #Lo almacena con los Sn*
}


plot(sort(Sn_est_Values), ylab="Sn*",xlab = "Simulaciones")
x<- sort(Sn_est_Values) #Divido por 1000 ya que no puedo analizar valores mayores a 1 como variables ~N(0,1)
lines(pnorm(x,0, 1), col="lightblue", lwd=3)


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
  
  Sn_est<-(Sn - mean(Sn))/sd(Sn) #Estandariza Sn
  
  Sn_est_Values<-c(Sn_est_Values, Sn_est) #Lo almacena con los Sn*
}

#Grafico
x<-0
plot(sort(Sn_est_Values), ylab="Sn*",xlab = "Simulaciones")
x<- sort(Sn_est_Values)
lines(pnorm(x,0, 1), col="darkorchid", lwd=3)



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
  
  Sn_est<-(Sn - mean(Sn))/sd(Sn) #Estandariza Sn
  
  Sn_est_Values<-c(Sn_est_Values, Sn_est) #Lo almacena con los Sn*
}

#Grafico
plot(sort(Sn_est_Values), ylab="Sn*",xlab = "Simulaciones")
x<- sort(Sn_est_Values)
lines(pnorm(x,0, 1), col="orange", lwd=3)



##Con n=500
#Simulo 100.000 valores de Sn*
sim<-10000
Sn_est_Values<-c() #Para guardar valores de Sn*

for (i in (1:sim)){
  ##Simulo Sn y la estandarizo
  n<-500
  Sn<- c()
  for (i in (1:n)){
    Sn<-c(Sn, bernoulli())
  } #Obtengo vector con valores de X
  
  Sn_est<-(Sn - mean(Sn))/sd(Sn) #Estandariza Sn
  
  Sn_est_Values<-c(Sn_est_Values, Sn_est) #Lo almacena con los Sn*
}

#Grafico
x<-0
plot(sort(Sn_est_Values), ylab="Sn*",xlab = "Simulaciones")
x<- sort(Sn_est_Values)
lines(pnorm(x,0, 1), col="red", lwd=3)


#Conclusiones: cuanto mas valores considere en la sumatoria, más se acerca el gráfico de los valores estandarizados a la funcion de una variable aleatoria x ~ N(0,1)






#Por ejemplo si quiero ver S2^* para una uniforme en (0,1) puedo hacer
N<- 1000 # Cantidad de simulaciones

#Defino funcion que retorna valores de Bernoulli
rbernoulli <- function(n,p) {
  x <- c()
  for (i in 1:n) {
    u <- runif(1,0,1)
    ifelse(u <= p,x <- c(x, 1),x <- c(x, 0))
    }
  return (x)
  }

#x <- rbernoulli(500, p) #n=500
#sum(x)


S500<- rep(0,N) # Vector donde almaceno las muestras de las distintas realizaciones de S500.
for (i in (1:500)){
  S500<- S500 + rbernoulli(N,0.5)
} # Sumo 500 realizaciones independientes de las uniformes.
S500_estandarizada <- (S500 - mean(S500))/sd(S500) # Estandarizo.

Normal <- rnorm(N) # N realizaciones de una normal
func_empirica <- ecdf(S500_estandarizada) # Calculo la funcion de distribucion empirica de S500_estandarizada
plot(func_empirica, main = "Gráficos de las funciones de dist. empírica - 1000 simulaciones", xlim=c(-2, 6)) # Grafico (Función de distribución empírica para Sn* Vs. función de distribución de variable normal estándar)
lines(ecdf(Normal), col = "plum", lwd=3) # Grafico para la normal



































plot.ecdf(S500_estandarizada)
lines(ecdf(rnorm(N)))

#Por ejemplo si quiero ver S2^* para una uniforme en (0,1) puedo hacer
N<- 100 # Cantidad de simulaciones
S2<- rep(0,N) # Vector donde almaceno las muestras de las distintas realizaciones de S2.
S2 = runif(N) + runif(N) # Sumo dos realizaciones independientes de las uniformes.
S2_estandarizada <- (S2 - mean(S2))/sd(S2) # Estandarizo.
Normal <- rnorm(N) # N realizaciones de una normal
func_empirica <- ecdf(S2_estandarizada) # Calculo la funcion de distribucion empirica de S2_estandarizada
plot(func_empirica) # Grafico
lines(ecdf(Normal), col = "red") # Grafico para la normal
sort(S2_estandarizada)

### Exercício 1

dado <- 1:6
moeda <- 0:1 # Cara = 1
R= 100000
contA = contB = 0
for(i in 1:R){
  lado_moeda = sample(moeda,1)
  lado_dado = sample(dado,1)
  #Letra A 
  if(lado_moeda == 1){
    if(lado_dado == 3){
      contA = contA + 1
    }
  }
  # Letra B
  if(lado_moeda == 1 | lado_dado %% 2 == 0){
      contB = contB + 1
  }
}
contA/R
contB/R

### Exercício 2 

soma = 0
theta = 2

for(i in 1:R){
  x = rexp(1, 1/theta)
  soma = soma + x
}

sum(rexp(R, 1/theta)/R)
soma/R


### Exercício 3

g <- function(x){
  exp(x + x^2) 
}

a = -2
b = 2
somag = 0

for(i in 1:R){
  somag = somag + g(runif(1,a,b))
}

(b - a)/R*somag
integrate(g,-2,2)

### Aula

############ Passo 1 #####################
n<-100 # tamanho da amostra
theta<-1 # valor do parametro
phi<-exp(-theta)
##########################################
# inicializacao dos vetores que guardarao as replicas dos estimadores
vphi1<-NULL
vphi2<-NULL
for(i in 1:R){ # laco de MC ###Passo 4
  ##### Passo 2 ###########################
  x <- rpois(n,theta) # n ocorrencias de um Poisson(theta) (amostra)
  #########################################
  ##### Passo 3 ###########################
  phi1<-exp(-mean(x)) # EMV de phi=exp(theta)
  phi2<-((n-1)/n)^sum(x) # Estimador nao viesado de phi
  # para guardar todas as replicas de MC dos estimadores
  vphi1[i]<-phi1
  vphi2[i]<-phi2
  ########################################
} ######### Fim do passo 4
### Passo 5 e 6
# media
m1<-mean(vphi1)
m2<-mean(vphi2)
# vies
b1<-m1-phi
b2<-m2-phi
# vies relativo
vr1<-(b1/phi)*100
vr2<-(b2/phi)*100
# erro-padrao
ep1 <- sd(vphi1)
ep2 <- sd(vphi2)
#EQM
eqm1 <- var(vphi1)+b1^2
eqm2 <- var(vphi2)+b2^2
mresults<-matrix(rep(NA,10),nrow=2)
colnames(mresults)<-c("media","vies","VR", "Erro-padrao",
                      "EQM")
rownames(mresults)<-c("phi_1","phi_2")
mresults[,1] <- c(m1,m2)
mresults[,2] <- c(b1,b2)
mresults[,3] <- c(vr1,vr2)
mresults[,4] <- c(ep1,ep2)
mresults[,5] <- c(eqm1,eqm2)
mresults
phi


##################################PROVA 1##################################
#Questão 1
theta = 3
soma1 <- as.vector(NULL)
i = 0
r = 5000
e = 0.00001
e_est = 1
while(e_est > e){
  soma1[i+1] = (exp(-theta)*(theta^i))/factorial(i)
  i = i + 1
  e_est = soma1[i] 
}

sum(soma1)


#Questão 3

g1 <- function(x){
  (1/sqrt(2*pi))*exp(-((x-15)^2/2))
}
##A

a1 = 15
b1 = 500
somag1 = 0

for(i in 1:R){
  somag1 = somag1 + g1(runif(1,a1,b1))
}

(b1 - a1)/R*somag1
integrate(g1,a1,b1)

##B

a2 = -500
b2 = 14.64
somag2 = 0

for(i in 1:R){
  somag2 = somag2 + g1(runif(1,a2,b2))
}

(b2 - a2)/R*somag2
integrate(g1,a2,b2)

##C
a3 = 14.64
b3 = 15
somag3 = 0

for(i in 1:R){
  somag3 = somag3 + g1(runif(1,a3,b3))
}

(b3 - a3)/R*somag3
integrate(g1,a3,b3)

### Questão 4
r1<-50000
n1<-c(20,50,100,300)
theta<-5
  
vtheta<-list(NULL, NULL, NULL, NULL)

for(i in 1:r1){ 
  for(j in n1){
    mtheta <- min(runif(j,theta,15))
    vtheta[[match(j,n1)]] <- c(vtheta[[match(j,n1)]], mtheta) 
  }
} 

### Passo 5 e 6
# media
(m1<-mean(vtheta[[1]]))
(m2<-mean(vtheta[[2]]))
(m3<-mean(vtheta[[3]]))
(m4<-mean(vtheta[[4]]))
# vies
(b1<-m1-theta)
(b2<-m2-theta)
(b3<-m3-theta)
(b4<-m4-theta)
# vies relativo
vr1<-(b1/theta)*100
vr2<-(b2/theta)*100
vr3<-(b3/theta)*100
vr4<-(b4/theta)*100
# erro-padrao
ep1 <- sd(vtheta[[1]])
ep2 <- sd(vtheta[[2]])
ep3 <- sd(vtheta[[3]])
ep4 <- sd(vtheta[[4]])
#EQM
eqm1 <- var(vtheta[[1]])+b1^2
eqm2 <- var(vtheta[[2]])+b2^2
eqm3 <- var(vtheta[[3]])+b3^2
eqm4 <- var(vtheta[[4]])+b4^2


mresults<- as.data.frame(NULL)
mresults[1:4,1] <- c(m1,m2,m3,m4)
mresults[1:4,2] <- c(b1,b2,b3,b4)
mresults[1:4,3] <- c(vr1,vr2,vr3,vr4)
mresults[1:4,4] <- c(ep1,ep2,ep3,ep4)
mresults[1:4,5] <- c(eqm1,eqm2,eqm3,eqm4)
names(mresults)<-c("media","vies","VR", "Erro-padrao",
                      "EQM")

mresults
theta

##################################PROVA 2##################################

g2<-function(x){
  (4^3*x^(3-1)*exp(-4*x))/gamma(3)
}

a11 = 10
b11 = 100
somag2 = 0

for(i in 1:R){
  somag2 = somag2 + g2(runif(1,a11,b11))
}

(b11 - a11)/R*somag2
integrate(g2,a11,b11)


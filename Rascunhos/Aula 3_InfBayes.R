x <- rbinom(n, 1, 0.5)
soma.x <- cumsum(x)

for(i in 1:n){
  plot(theta, dbeta(theta, 50, + soma.x[i], i - soma.x[i] + 1), col=2, main = i, type='l')
  readline()
}
#E se a moeda fosse honesta com uma priori Unifomre(0,1)

for(i in 1:n){
  plot(theta, dbeta(theta, 1+soma.x[i], i-soma.x[i]+1), col=2, main=i, type='l')
  readline()
}

#theta é a probabilidade de uma moeda dar cara
#Priori muito informativa de que a moeda é viciada e possui duas caras
#theta ~ Beta(0,1)
#gráfico da priori

theta <- seq(0,1,0.001)
plot(theta, dbeta(theta, 50,1), type = 'l', main='Priori')
#Verossimilhança (modelo Bernoulli(theta))
# Imagine  que na verdade, a moeda possui duas coroas
#Ou seja, x será sempre zero

# Amostra 1
#x1 = 0 #Primeiro resultado deu que é cara
# Posteriori

#Theta [x1] ~ Beta(50,2)
plot(theta, dbeta(theta, 50,2), col=2, lwd=2, type = 'l', main='Priori')

n <- 5000
for(i in 1:n){
  plot(theta, dbeta(theta, 50, i+1), col =2, main = i, type='l')
}

# E se a moeda fosse honesta com a mesma priori?
x <- rbinom(n, 1, 0.5)
soma.x <- cumsum(x)
for(i in 1:n){
  plot(theta, dbeta(theta, 50, + soma.x[i], i - soma.x[i] + 1), col=2, main = i, type='l')
  readline()
}

#simulacion de muestra aleatoria del modelo normal
#Generacion de intervalo de confianza para la media
#con varianza conocida

n <- 10
alpha <- 0.05
nsim <- 100000

#Verificacion de que el intervalo cubre el numero de 
#veces esperado segun la confianza 
count <- 0
for(i in 1:nsim){
  sample <- rnorm(n,5,2)
  li <- mean(sample) - 2/sqrt(n)*qnorm(1-alpha/2,0,1)
  ls <- mean(sample) + 2/sqrt(n)*qnorm(1-alpha/2,0,1)
  if(li <= 5 & ls >= 5){
    count <- count + 1 
  }
}

count/nsim

#Generacion del intervalo bajo 
#bootstrap parametrico
sample.original <- rnorm(n,5,2)

mu.mv <- mean(sample.original)
mu.boot <- rep(0,nsim)

for(i in 1:nsim){
  sample <- rnorm(n,mu.mv,sqrt(sigma.mv2))
  mu.boot[i] <- mean(sample)
}
hist(mu.boot, freq = FALSE, breaks = 100)

quantile(mu.boot, c(alpha/2,1-alpha/2))
quantile(var.boot, c(alpha/2,1-alpha/2))

#Comparcion con el metodo de cantidad pivotal
li <- mean(sample.original) - 2/sqrt(n)*qnorm(1-alpha/2,0,1)
ls <- mean(sample.original) + 2/sqrt(n)*qnorm(1-alpha/2,0,1)


#bootstrap no parametrico
#muestrear de la distribucion empirica
sample.original
mu.boot <- rep(0,nsim)
for(i in 1:nsim){
  #la funcion sample remuestrea la muestra original
  sample <- sample(sample.original, n, replace = TRUE )
  mu.boot[i] <- mean(sample)
}
hist(mu.boot, freq = FALSE, breaks = 100)
quantile(mu.boot, c(alpha/2,1-alpha/2))


#Suponiendo ambos parametros desconocidos usando bootrap parametrico
sample.original <- rnorm(n,5,2)
mu.mv <- mean(sample.original)
sigma.mv2 <- var(sample.original) 
mu.boot <- rep(0,nsim)
var.boot <- rep(0,nsim)
for(i in 1:nsim){
  sample <- rnorm(n,mu.mv,sqrt(sigma.mv2))
  mu.boot[i] <- mean(sample)
  var.boot[i] <- var(sample)
}
hist(mu.boot, freq = FALSE, breaks = 100)
hist(var.boot, freq = FALSE, breaks = 100)

alpha <- 0.05
quantile(mu.boot, c(alpha/2,1-alpha/2))
quantile(var.boot, c(alpha/2,1-alpha/2))


plot(mu.boot,var.boot)



#Ejemplo

mortalidad=read.csv("Mortalidad.csv")
dias=mortalidad[,1]*365+mortalidad[,2]
par(mfrow =c(1,1))
hist(dias,freq=FALSE)

n=length(dias)
#Supondremos alpha conocida e igual a 76.90719
alpha=76.90719
beta.mv=alpha/mean(dias)
hist(dias,freq=FALSE)
curve(dgamma(x,alpha,beta.mv), add = TRUE, col = 2, lwd = 2)

plot(ecdf(dias))
curve(pgamma(x,alpha,beta.mv), add = TRUE, col = 2, lwd = 2)

#Estimacion para la probabilidad de vivir mas de 70 años
p.est=1-pgamma(70*365,alpha,beta.mv)

#Intervalo de Confianza
alpha.1=0.05
Z=qnorm(1-alpha.1/2,0,1)

partial.f=-gamma(alpha+1)/(gamma(alpha)*beta.mv)*(1-pgamma(70*365,alpha+1,beta.mv))+alpha/beta.mv*(1-pgamma(70*365,alpha,beta.mv))
li.1=p.est-abs(partial.f)*beta.mv/sqrt(n*alpha)*Z
ls.1=p.est+abs(partial.f)*beta.mv/sqrt(n*alpha)*Z

round(c(li.1,p.est,ls.1),3)


#Estimacion Bootstrap paramétrico

beta.mv=alpha/mean(dias)

p.est.boot<-NA
for(i in 1:100000){
  sample=rgamma(n,alpha,beta.mv)
  p.est.boot[i]=1-pgamma(70*365,alpha,alpha/mean(sample))
}
hist(p.est.boot)
c(round(quantile(p.est.boot,alpha.1/2),3),round(quantile(p.est.boot,1-alpha.1/2),3))



#Estimacion Bootstrap ambos parametros desconocidos
mortalidad=read.csv("Mortalidad.csv")
dias=mortalidad[,1]*365+mortalidad[,2]
par(mfrow =c(1,1))
hist(dias,freq=FALSE)
n=length(dias)

ajuste = MASS::fitdistr(dias, "gamma")
ajuste$estimate

alpha.mv <- ajuste$estimate[1]
beta.mv  <- ajuste$estimate[2]


plot(ecdf(dias))
curve(pgamma(x,alpha.mv,beta.mv), add = TRUE, col = 2, lwd = 2)

hist(dias, freq = FALSE, breaks = 20)
curve(dgamma(x,alpha.mv,beta.mv), add = TRUE, col = 2, lwd = 2)


t <- proc.time()
p.est.boot<-NA
for(i in 1:500000){
  sample=rgamma(n,alpha.mv,beta.mv)
  ajuste = MASS::fitdistr(sample, "gamma")
  alpha.mv.Boot <- ajuste$estimate[1]
  beta.mv.Boot <- ajuste$estimate[2]
  
  p.est.boot[i]=1-pgamma(70*365,alpha.mv.Boot,beta.mv.Boot)
}
hist(p.est.boot)
c(round(quantile(p.est.boot,alpha.1/2),3),round(quantile(p.est.boot,1-alpha.1/2),3))
proc.time() - t



#paraleizando el proceso
library(doParallel)
parallel::detectCores()
cl <- makeCluster(4)
registerDoParallel(cl)
t <- proc.time()

result <- foreach(i=1:500000,  .combine=rbind) %dopar% {
  
  sample=rgamma(n,alpha.mv,beta.mv)
  ajuste = MASS::fitdistr(sample, "gamma")
  alpha.mv.Boot <- ajuste$estimate[1]
  beta.mv.Boot <- ajuste$estimate[2]
  1-pgamma(70*365,alpha.mv.Boot,beta.mv.Boot)
  
}

stopCluster(cl)
hist(result)
c(round(quantile(result,alpha.1/2),3),round(quantile(result,1-alpha.1/2),3))
proc.time() -t 

round(quantile(result,1-alpha.1/2),3)-round(quantile(result,alpha.1/2),3)
#y su no es gamma??????

mortalidad=read.csv("Mortalidad.csv")
dias=mortalidad[,1]*365+mortalidad[,2]
par(mfrow =c(1,1))
hist(dias,freq=FALSE)
n=length(dias)


plot(ecdf(dias))

p.est.boot <- NA
for(i in 1:100000){
  sample_boot= base::sample(dias,size = n, replace = TRUE)
  p.est.boot[i] = mean(sample_boot > 70*365)
  
}
hist(p.est.boot)
c(round(quantile(p.est.boot,alpha.1/2),3),round(quantile(p.est.boot,1-alpha.1/2),3))


#Ejemplo

mortalidad=read.csv("C:/Users/Jose Damian/Desktop/Tareas/BigData20202/bootstrap/Mortalidad.csv")
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


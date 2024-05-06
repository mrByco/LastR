x="cboyzf99999";#neptun kód
z=charToRaw(iconv(x, "latin1", "UTF-8"))
for (i in 1:6) v=paste("0x",z,sep="")
e=strtoi(v)
ax=e[1];ay=e[2];az=e[3];av=e[4];ss=sum(strtoi(v))+24
cat("ax=",ax,"\n")
cat("ay=",ay,"\n")
cat("az=",az,"\n")
cat("av=",av,"\n")
cat("ss=",ss,"\n")
ar=c( "FB","AAPL","AMZN","GOOG","NFLX","TSLA")
ai=ss-6*floor(ss/6)
ev=2022-(ss-10*floor(ss/10))
cat("ev=",ev,"\n")
cat("reszveny=",ar[ai+1],"\n")

set.seed(ss)

nx=700
v=matrix(c(ax, abs(ax-ay), abs(ax-ay), ay),2)
w=chol(v)
z1=sqrt(-2*log(runif(nx)))*sin(runif(nx)*2*pi)
z2=sqrt(-2*log(runif(nx)))*cos(runif(nx)*2*pi)
zm=matrix(c(z1,z2),ncol=2)
zn=5*zm%*%w


# import the csv
library(readr)

data = read.csv("META.csv")

library(ggpubr)

# plot the close date
ggdensity(data$Close)

print(log(10))
print(log10(10))

details <- read.csv("META.csv")
logreturn = c() # logreturn létrehozása
zaro <- details$Close # A záró értékek kinyerése
for (i in 1:length(zaro)-1) {
    logreturn[i] = log(zaro[i+1]/zaro[i]) # logreturn értékek begyűjtése
}
logreturn = abs(logreturn)
chisq.test(logreturn) # Khi-négyzet teszt

ggdensity(logreturn) 

hist(logreturn, main="Histogram :)")

plot(logreturn)

plot(data$Close)


print("7. Feladat")

# Egy évre becsülés
mu<-mean(logreturn) # logreturn átlaga
sig<-sd(logreturn) # logreturn szórása
price<-rep(NA,365) # egy év
price[1]<- zaro[length(zaro)] # utolsó ismert érték
# Árak szimulálása
for(i in 2:365){
    price[i]<-price[i-1]*exp(rnorm(1,mu,sig))
}
random_data<-cbind(price,1:365)
colnames(random_data)<-c("Price","Day")
random_data<-as.data.frame(random_data)
random_data%>%ggplot(aes(Day,Price))+geom_line()+labs(title="Google (GOOG) árfolyam szimuláció 1 évre")+theme_bw()

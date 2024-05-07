# Értékek generálása
x="l27ncj";
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


# Mintarealizáció generálása
set.seed(ss)

nx=700
v=matrix(c(ax,abs(ax-ay),abs(ax-ay),ay),2)
w=chol(v)
z1=sqrt(-2*log(runif(nx)))*sin(runif(nx)*2*pi)
z2=sqrt(-2*log(runif(nx)))*cos(runif(nx)*2*pi)
zm=matrix(c(z1,z2),ncol=2)
zn=5*zm%*%w


# Statisztikai elemzés, paraméterek becslése 
summary(zn)
# Min: A mintarealizáció legkisebb értéke
# 1st Qu. : Első kvartilis - A mintarealizáció 25%-a ez alá az érték alá esik
# Median: A mintarealizáció mediánja
# Mean: A mintarealizáció empirikus közepe
# 3rd Qu. : Harmadik kvartilis - A mintarealizáció 75%-a ez alá az érték alá esik
# Max. : A mintarealizáció legnagyobb értéke

library(moments)
# Ferdeség
skewness(zn)

# Lapultság
kurtosis(zn)

# Peremek függetlenségének vizsgálata
# Korrelációs mátrix:
cor(zn)
# Ha a korrelációs mátrix diagonális, vagyis a nem-diagonális elemek közelítőleg 0-k, akkor a peremek függetlenek lehetnek.
# "Az adatok erősen korreláltak." (Ha 0-tól távoli) 

#cor.test(zn[,1],zn[,2])

# Eloszlás vizsgálat

# mean_zn = colMeans(zn)
# cov_zn = cov(zn)

# marginal_1 = list(mean = mean_zn[1], sd = sqrt(cov_zn[1, 1]))
# marginal_2 = list(mean = mean_zn[2], sd = sqrt(cov_zn[2, 2]))


#Vizuális igazolás beépített függvényekkel.
#Sűrűségdiagrammal: A görbe bizonyítja a Poisson/Normális eloszlást.
library(ggpubr)
ggdensity(zn[,1], main = "Sűrűségfüggvény")
ggdensity(zn[,2], main = "Sűrűségfüggvény")
# Vizsgálat Kvantilis diagram alapján:
# Megrajzolja az összefüggést egy adott minta és a normális eloszlás között, 45 fokos referenciavonalon.
library(car)
qqPlot(zn)


# Ábrázolások

# Adatok betöltése
x <- zn[,1]
y <- zn[,2]

# Rács készítése
grid_size <- 20
x_range <- seq(min(x), max(x), length.out = grid_size)
y_range <- seq(min(y), max(y), length.out = grid_size)

# Kétváltozós sűrűség becslése
library(MASS)  # csomag a kde2d() függvényhez
fhat <- kde2d(x, y, n = grid_size)

# Plot-ok 2x2-es elhelyezése
par(mfrow = c(2, 2))

# Többdimenziós ábrázolás
plot(zn, main = "A zn mátrix")

# Szintvonalas ábrázolás
contour(
  x_range,
  y_range,
  fhat$z,
  xlab = "X",
  ylab = "Y",
  main = "Szintvonalak"
)

# Perspektívikus ábrázolás
persp(
  x_range,
  y_range,
  fhat$z,
  theta = 45,
  phi = 30,
  xlab = "X",
  ylab = "Y",
  zlab = "Z",
  main = "Perspektívikus ábrázolás"
)


# Exponenciális

set.seed(ss)

# Minta méret
nx <- 700
# Korrelációs együttható
rho <- -0.7

# Kovariancia mátrix
cov_matrix <- matrix(c(1, rho, rho, 1), ncol = 2)

# Eloszlás generálás
library(MASS)
data <- mvrnorm(n = nx, mu = c(0, 0), Sigma = cov_matrix)

# Transzformálás exponenciálissá
x <- rexp(nx, rate = exp(0.5 * data[, 1]))
y <- rexp(nx, rate = exp(0.5 * data[, 2]))

# Ábrázolás
plot(x, y, xlab = "X", ylab = "Y", main = "Exponenciális eloszlás korrelációval")

# Brown folyamat

library(LSMRealOptions)

set.seed(ss + 37)
n <- 1
t <- 100/365
mu <- ax
sigma <- (ax+ay)/(ax+ay+az)
S0 <- 100
dt <- 1/365

gbm <- GBM_simulate(n, t, mu, sigma, S0, dt)
plot(gbm, type='l')

summary(gbm)
skewness(gbm)
kurtosis(gbm)

# plots <- GBM_simulate(4, 4, ax, (ax+az)/(ax+ay+az), 100, 1/12)
# plot(plots[,1], type='l') # 1. generáció
# plot(plots[,2], type='l') # 2. generáció
# plot(plots[,3], type='l') # 3. generáció
# plot(plots[,4], type='l') # 3. generáció


# Kézi megoldás

library(stats)

gbm <- function(mu, sigma, T, S0, dt, ss) {
  set.seed(ss + 37)

  # Lépések megadása
  t <- seq(0, T, dt)

  # Inicializáció
  S <- numeric(length(t))
  S[1] <- S0  # Kezdőár

  # GBM szimuláció
  for(i in 2:length(t)) {
    dW <-  rnorm(1, mean = 0, sd = sqrt(dt))
    S[i] <- S[i-1]*exp((mu-0.5*sigma^2)*dt+sigma*dW)
  }

  # Ábrázolás
  plot(t, S, type="l", main="Brown folyamat")

  return(S)
}

S <- gbm(mu=ax, sigma=(ax+ay)/(ax+ay+az), T=100/365, S0=100, dt=1/365, ss)


# Poisson folyamat

set.seed(ss+17)
# Várható érték (lambda)
lambda <- 2 

# Generálás
# Időintervallum
T <- 1000
n <- rpois(T, lambda)

# Kumulatív összeggé alakítás
n_cumsum <- cumsum(n)

# Ábrázolás
plot(0:T, c(0, n_cumsum), type="s", xlab="Time", ylab="Number of Events", main="Poisson Process")


# Importálás (csv)
library(readr)
data <- read.csv("C:/Users/HP/Desktop/5/META.csv")

# logreturn létrehozása
logreturn = c() 

# A záró értékek kinyerése
close <- data$Close 

# logreturn értékek számítása
for (i in 1:length(close)-1) {
    logreturn[i] = log(close[i+1]/close[i]) 
}

logreturn = abs(logreturn)

# Eloszlás
library(ggpubr)
ggdensity(logreturn) 
qqPlot(logreturn)

# Khi-négyzet teszt
chisq.test(logreturn)
# Shapiro teszt
shapiro.test(logreturn)

# Grafikus ábrázolás
hist(logreturn, main="Histogram :)")

plot(logreturn)


# Egy évre becslés

# Átlag
mu<-mean(logreturn) 
# Szórás
sig<-sd(logreturn)
# 365 elemű üres tömb
price<-rep(NA,365)
# Az első becsült érték beállítása az utolsó ismert értékre
price[1]<- close[length(close)] 
# Becsült értékek szimulálása
for(i in 2:365){
    price[i]<-price[i-1]*exp(rnorm(1,mu,sig))
}
# Ábrázolás
random_data <- data.frame(Day = 1:365, Price = price)
random_data %>%
  ggplot(aes(Day, Price)) +
  geom_line() +
  labs(title = "Facebook (META) árfolyam szimuláció 1 évre") +
  theme_bw()

# Új adatok beolvasása
new_data <- read.csv("C:/Users/Desktop/5/new.csv")

library(caret)
# RMSE kiszámítása (Ellenőrzés)
rmse <- RMSE(price, new_data$Close)
cat("Root Mean Squared Error (RMSE):", rmse, "\n")

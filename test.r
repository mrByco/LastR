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
v=matrix(c(ax,abs(ax-ay),abs(ax-ay),ay),2)
w=chol(v)
z1=sqrt(-2*log(runif(nx)))*sin(runif(nx)*2*pi)
z2=sqrt(-2*log(runif(nx)))*cos(runif(nx)*2*pi)
zm=matrix(c(z1,z2),ncol=2)
zn=5*zm%*%w

summary(zn)

cor(zn)
library(moments)

cor.test(zn[,1],zn[,2])

a = c(49, 90, 71, 92, 4, 24, 46, 50, 48, 18, 33, 9, 73, 63, 90, 87, 82, 19, 7, 43, 75, 26, 4, 71, 47, 4, 63, 22, 29, 47, 90, 33, 82, 76, 94, 61, 24, 7, 9, 13, 26, 89, 58, 12, 89, 66, 32, 90, 22, 75, 8, 95, 99, 7, 83, 63, 67, 2, 81, 1, 28, 44, 23, 69, 16, 46, 89, 54, 21, 64, 83, 12, 88, 22, 6, 85, 38, 45, 90, 13, 99, 4, 69, 83, 26, 70, 60, 61, 15, 51, 48, 77, 84, 13, 49, 88, 45, 93, 60, 31)
b = c(49, 90, 71, 92, 4, 24, 46, 50, 48, 18, 33, 9, 73, 63, 90, 87, 82, 19, 7, 43, 75, 26, 4, 71, 47, 4, 63, 22, 29, 47, 90, 33, 82, 76, 94, 61, 24, 7, 9, 13, 26, 89, 58, 12, 89, 66, 32, 90, 22, 75, 8, 95, 99, 7, 83, 63, 67, 2, 81, 1, 28, 44, 23, 69, 16, 46, 89, 54, 21, 64, 83, 12, 88, 22, 6, 85, 38, 45, 90, 13, 99, 4, 69, 83, 26, 70, 60, 61, 15, 51, 48, 77, 84, 13, 49, 88, 45, 93, 60, 31)
c = c(17, 98, 77, 5, 11, 21, 55, 93, 26, 14, 42, 6, 52, 83, 47, 91, 5, 68, 92, 79, 12, 61, 41, 7, 25, 21, 36, 29, 40, 24, 9, 3, 64, 20, 45, 82, 90, 49, 15, 76, 53, 28, 92, 35, 24, 64, 38, 89, 71, 47, 6, 30, 10, 16, 42, 97, 18, 67, 58, 73, 93, 51, 31, 65, 68, 4, 3, 39, 91, 77, 12, 34, 57, 78, 16, 94, 41, 62, 65, 8, 73, 26, 11, 1, 60, 56, 31, 94, 2, 80, 59, 96, 70, 85, 45, 64, 59, 13, 74, 90)
length(c)
cor.test(a, b)

xD = c(1, 34, 81, 65, 24, 11, -5, 0, -1, -3)
Xd = c(0, 50, 55,55,55,55,55,55,55, 50, 0)

print("Skewness of a:")
skewness(xD)

print("Kurtosis of a:")
kurtosis(Xd)

print("neXT feladat:")

#TODO solve
#contour(zn, col="blue", xlab="X", ylab="Y", main="Contour plot of X and Y")


# Minta realizáció paraméterek alapján
set.seed(ss)
nx <- 700
rho <- -0.7
mean <- c(ax, ay)
print(mean)
cov_matrix <- matrix(c(av^2, rho * av * az, rho * av * az, az^2), ncol = 2)
library(MASS)
data <- mvrnorm(n = nx, mu = mean, Sigma = cov_matrix)
x <- data[, 1]
y <- data[, 2]

plot(x, y, xlab = "X", ylab = "Y", main = "Exponential Distribution", pch = 16, col = "blue")

print("3 Feladat")

# Adatok betöltése
x <- zn[,1]
y <- zn[,2]

# Rács készítése
grid_size <- 5
x_range <- seq(min(x), max(x), length.out = grid_size)
y_range <- seq(min(y), max(y), length.out = grid_size)

# Kétváltozós sűrűség becslése
library(MASS)  # csomag a kde2d() függvényhez
fhat <- kde2d(x, y, n = grid_size)
print(fhat)
# Plot-ok 2x2-es elhejezése
par(mfrow = c(2, 2))

# 5x5 matrix with random values
matrix_5x5 <- matrix(sample(1:100, 25), nrow = 5, ncol = 5)

# Perspektívikus ábrázolás
persp(
  x_range,
  y_range,
  fhat$z,
  theta = 45,
  phi = 30,
  xlab = "X",
  ylab = "Y",
  zlab = "2",
  main = "Perspektívikus ábrázolás"
)

# Szintvonalas ábrázolás
contour(
  x_range,
  y_range,
  fhat$z,
  xlab = "X",
  ylab = "Y",
  main = "Szintvonalak"
)

plot(zn, main = "A zn mátrix")

print("3.5 Feladat")

library(LSMRealOptions)

set.seed(ss + 37)

GBM_simulate(1, 2, ax, (ax+az)/(ax+ay+az), 100, 1/12)
plot(GBM_simulate(1, 2, ax, (ax+az)/(ax+ay+az), 100, 1/12))

plots <- GBM_simulate(4, 4, ax, (ax+az)/(ax+ay+az), 100, 1/12)
plot(plots[,1], type='l') # 1. generáció
plot(plots[,2], type='l') # 2. generáció
plot(plots[,3], type='l') # 3. generáció
plot(plots[,4], type='l') # 3. generáció


print("Kézi megoldás")

# A stats csomag betöltése
library(stats)

# Lépések megadása
dt <- 1/1000  # Időintervallum (évente 12 hónap)

# Inicializálás
SO <- 100  # Kezdeti árfolyam

# GBM szimuláció
gbm <- function (mu, sigma, T) {
  # Lépések megadása
  t <- seq(0, T, dt)

  # Inicializálás
  S <- numeric(length(t))
  S[1] <- SO

  # Generáljuk le a GBM szimuláció eredményeit
  for (i in 2:length(t)) {
    dW <- rnorm(1, mean = 0, sd = sqrt(dt))
    S[i] <- S[i-1] * exp((mu - 0.5 * sigma^2) * dt + sigma * dW)
  }

  # Visszatérünk az eredménnyel
  return(S)
}

# GBM szimuláció futtatása
S <- gbm(mu = 0.05, sigma = 0.2, T = 1)

# Rajzoljuk ki a gráfot
set.seed(ss + 37)
plot(GBM_simulate(1, 100, 0.05, 0.2, 100, 1/1000), type='l')
set.seed(ss + 37)

plot(gbm(0.05, 0.2, 100), type='l')
set.seed(ss + 37)
plot(gbm(0.05, 0.2, 100), type='l')

# Amelia
#sp <- c(0,1.5,2.5,3.5,4,4.5,5,5.5,6,6.5,7,8)
#teff <- c(4000,3750,3600,3500,3300,3200,3260,3100,3050,3000,2700)

#Rajpurohit

#sp <- c(0,0.5,1,1.5,2,2.5,3,3.5,4.5,5,5.5,6,6.5,7,7.5,8,8.5,9,9.5)
#teff <- c(3900,3800,3700,3600,3500,3400,3300,3200,3100,2900,2900,2800,2700,2700,2600,2600,2500,2500,2400)


f2 <- function(t){
teff = 4400.9-467.26*t+54.67*t^2-4.4727*t^3+0.17667*t^4-0.0025492*t^5
teff
}

a <- read.csv2("sptp.txt", header=F)
ms <- grepl("V",a[,1])
g <- grepl("III",a[,1])

# Remove luminosity type
a[,1]<-gsub("V","", a[,1],fixed=T)
a[,1]<-gsub("III","", a[,1],fixed=T)
sg <- grepl("II",a[,1])
a[,1]<-gsub("II","", a[,1],fixed=T)
ssg <- grepl("I",a[,1])
a[,1]<-gsub("I","", a[,1],fixed=T)

colon <- grepl(":",a[,1])
DwCarbon <-  grepl("DwCarbon",a[,1])
gt <- grepl(">=",a[,1])
plus <- grepl("+",a[,1],fixed=T)
spf <- grepl("7-8",a[,1])
epf <- grepl("8-9",a[,1])
pec <- grepl("pec",a[,1])
sd <-  grepl("sd",a[,1])
r <- grepl("(r)",a[,1])

#Remove colons
a[,1]<-gsub(":","", a[,1],fixed=T)
# Remove DwCarbon
a[DwCarbon,1] <- NA
# Remove L star
a[gt,1] <- NA
# Remove pluses
a[,1]<-gsub("+","", a[,1],fixed=T)
# Remove peculiarities
a[,1]<-gsub("pec","", a[,1],fixed=T)
a[,1]<-gsub("sd","", a[,1],fixed=T)
a[,1]<-gsub("(r)","", a[,1],fixed=T)
a[plus,1]<-NA

a[,1]<-gsub("M","1", a[,1],fixed=T)
a[,1]<-gsub("K","", a[,1],fixed=T)
a[spf,1] <- "17.5"
a[epf,1] <- "18.5"
b<-as.numeric(a[,1])
b[b==1]<-NA

sptn <- b 
raj <- read.table("rajpurohit.dat")
load("boyajian.RData")

data <- rbind(raj,boyajian.spt)
y0 <- data[,2]
x0 <- data[,1]

xp <- seq(5,10,0.1)
plot(xp,f2(xp),ty="l",ylim=c(2000,6000),xlim=c(-12,12),xlab="Spectral Type (M0 = 0)", ylab=expression(T[eff]),lwd=3)
points(boyajian.spt[,1]-10,boyajian.spt[,2],col="orange",pch=16)
points(raj[,1]-10,raj[,2],pch=16,col="blue")

xtmp <- seq(0,20,0.1)
# Parabolic model to Rajpurohit data only
y <- y0[x0>7]
x <- x0[x0>7]
m1 <- lm(y~x)
new <- data.frame(x = sptn)
teff <- predict.lm(m1,new,se.fit=TRUE)
lines(sptn-10,teff$fit,pch=16,col="green",lwd=3)
# Smoothing spline
m2 <- smooth.spline(x0,y0,df=8)
spline <- predict(m2,xtmp)
lines(xtmp-10,spline$y,pch=16,col="red",lwd = 3)
teff2 <- rep(NA,length(sptn))
teff2[!is.na(sptn)] <- predict(m,sptn[!is.na(sptn)])$y

# CArroll & Ostein used by Cesetti in many of his temperatures

CarOstV <- cbind(
  c(0,1,2,3,4,5,6,7,8,9),
#  c(3840,3660,3520,3400,3290,3170,3030,2860)
  c(3850,3720,3580,3470,3370,3240,3050,2940,2640,2600)
)

m3 <- smooth.spline(CarOstV[,1],CarOstV[,2],df=8)
xt <- seq(0,10,0.1)
yt <- predict(m3,xt)
lines(xt,yt$y,col="violet",lwd=3)
points(CarOstV,col="violet",pch=15)

legend(-5,6000,c("Stephens, Leggett & Cushing (2009)","Boyajian et al. (2012)","Rajpurohit et al. (2013)","Spline fit", "Linear Fit"),col=c("black","orange","blue","red", "green"),pch=16,lty=1)


save.image("Teffs.RData")

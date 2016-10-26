
rm(list=ls())
load("LSB_Tdata_plot_IPAC.RData")

lc2 <- 15

#############################################
# Teffs Validation

c1 <- apply(df_T_10[,c(2:9,18,19)],2,"-",df_T_10$T_teo)
m1 <- apply(c1,2,median,na.rm=T)
c1 <- apply(c1,1,"-",m1)
c1 <- t(c1)
c1 <- apply(c1,2,mad,na.rm=T)
c2 <- apply(df_T_50[,c(2:9,18,19)],2,"-",df_T_50$T_teo)
m2 <- apply(c2,2,median,na.rm=T)
c2 <- apply(c2,1,"-",m2)
c2 <- t(c2)
c2 <- apply(c2,2,mad,na.rm=T)
c3 <- apply(df_T_inf[,c(2:9,18,19)],2,"-",df_T_inf$T_teo)
m3 <- apply(c3,2,median,na.rm=T)
c3 <- apply(c3,1,"-",m3)
c3 <- t(c3)
c3 <- apply(c3,2,mad,na.rm=T)

pdf("../ipac-teff.pdf",width=8,height=5)
par(cex.axis=1.0)
nrows = 2
ncols = 2
nplots = nrows*ncols
windim = c(60,30) 
mardim=7
layoutmat = matrix(c(1,2,3,4),nrows,ncols, byrow=T)
nf = layout(layoutmat,respect=T,
            widths=c(windim[1],windim[1]),
            heights=c(windim[2],windim[2]+mardim)
)

xl <- c(2000,4500)
par(mar = c(0,5,1,0))
plot(df_T_50$T_teo,df_T_50$Chi2_50,pch=lc2,cex=0.5,col="red",
     axes=F,xlab="",ylab="",cex.lab=1.5,xlim=xl,ylim=xl)
box()
text(2000,4000,expression(GA-chi^2-50),pos=4,cex=1.5)
axis(2)
abline(0,1)

par(mar = c(0,0,1,4))
plot(df_T_50$T_teo,df_T_inf$RF,pch=lc2,cex=0.5,col="red",axes=F,xlab="",
     cex.lab=1.5,xlim=xl,ylim=xl)
box()
axis(4)
text(2000,4000,"GA-RF-inf",pos=4,cex=1.5)
abline(0,1)

par(mar = c(4,5,0,0))
plot(df_T_50$T_teo,df_T_10$MARS,pch=lc2,cex=0.5,col="red",axes=F,
     xlab=expression(T[eff]),
     cex.lab=1.5,xlim=xl,ylim=xl)
box()
axis(1)
axis(2)
text(2000,4000,"GA-MARS-10",pos=4,cex=1.5)
box()
abline(0,1)
#
par(mar = c(4,0,0,4))
plot(df_T_50$T_teo,df_T_10$NNR,pch=lc2,cex=0.5,col="red",axes=F,
          xlab=expression(log(T[eff-spt])),
     ylab=expression(T[eff]),cex.lab=1.5,xlim=xl,ylim=xl)
box()
axis(1)
axis(4)
text(2000,4000,"GA-NNR-10",pos=4,cex=1.5)
box()
axis(1)
abline(0,1)

dev.off()

#############################################
# Teffs vs logg

load("../irtf-figs/irtf.Rdata")

teffirtf <- df_T_inf$KNN
loggirtf <- df_G$NNR_50
load("LSB_Tdata_plot_IPAC.RData")
load("Gdata_plot.RData")

load("Trv_GA_models_IPAC-interp2_newT.RData")
pchmask <- rep(NA,length(LC))
pchmask[LC=="V"] = 15
pchmask[LC=="III"] = 17
pchmask[is.na(LC)] = 1
colmask <- rep(NA,length(LC))
colmask[LC=="V"] = "blue"
colmask[LC=="III"] = "red"
colmask[is.na(LC)] = "gray"
df_G$LG_teo[LC=="V"]=5
df_G$LG_teo[LC=="III"]=1
df_G$LG_teo[is.na(LC)]=NA


c1 <- apply(df_G[,c(2:31)],2,"-",df_G$LG_teo)
m1 <- apply(c1,2,median,na.rm=T)
c1 <- apply(c1,1,"-",m1)
c1 <- t(c1)
c1 <- apply(c1,2,mad,na.rm=T)

lc <- 15

pdf("../ipac-teff-logg.pdf",width=8,height=5)
par(cex.axis=1.0)
nrows = 2
ncols = 2
nplots = nrows*ncols
windim = c(60,30) 
mardim=7
layoutmat = matrix(c(1,2,3,4),nrows,ncols, byrow=T)
nf = layout(layoutmat,respect=T,
            widths=c(windim[1],windim[1]),
            heights=c(windim[2],windim[2]+mardim)
)

par(mar = c(0,5,1,0))
plot(log10(df_T_inf$RF),df_G$NNR_10,pch=pchmask,cex=0.5,col=colmask,axes=F,xlab="",
     ylab="log(g)",cex.lab=1.5, xlim=c(3.9,3.25), ylim=c(6,-1))
points(log10(teffirtf),loggirtf,cex=.5,pch=lc)
box()
text(3.4,0,"GA-NNR-10",pos=4,cex=0.7)
axis(2)

par(mar = c(0,0,1,4))
plot(log10(df_T_inf$RF),df_G$SVR_50,pch=pchmask,cex=0.5,col=colmask,axes=F,xlab="",
     ylab="log(g)",cex.lab=1.5, xlim=c(3.9,3.25), ylim=c(6,-1))
points(log10(teffirtf),loggirtf,cex=.5,pch=lc)
box()
axis(4)
text(3.4,0,"GA-SVR-50",pos=4,cex=0.7)

par(mar = c(4,5,0,0))
plot(log10(df_T_inf$RF),df_G$Chi2_50,pch=pchmask,cex=0.5,col=colmask,axes=F,xlab=expression(log(T[eff])),
     ylab="log(g)",cex.lab=1.5, xlim=c(3.9,3.25), ylim=c(6,-1))
points(log10(teffirtf),loggirtf,cex=.5,pch=lc)
box()
axis(1)
axis(2)
text(3.4,0,expression(GA-chi^2-50),pos=4,cex=0.7)
box()
#
par(mar = c(4,0,0,4))
plot(log10(df_T_inf$RF),df_G$ICA_10,pch=pchmask,cex=0.5,col=colmask,axes=F,xlab=expression(log(T[eff])),
     ylab="log(g)",cex.lab=1.5, xlim=c(3.9,3.25), ylim=c(6,-1))
points(log10(teffirtf),loggirtf,cex=.5,pch=lc)
box()
axis(1)
axis(4)
text(3.4,0,"ICA-10",pos=4,cex=0.7)
box()
axis(1)

dev.off()

#####################  4 CESSETI  ###############################

load("Mdata_plot.RData")

df_G_ours <- df_G
df_M_ours <- df_M

load("Gdata_CES_plot.RData")
load("Mdata_plot_CES.RData")

df_G_CES <- df_G
df_M_CES <- df_M
df_G <- df_G_ours
df_M <- df_M_ours
rm(df_G_ours,df_M_ours)

# Gives error because of different sizes in X and Y
# Email sent to jbmere on Oct 14 2016

pdf("../ipac-Cesseti.pdf",width=8,height=5)
par(cex.axis=1.0)
nrows = 2
ncols = 2
nplots = nrows*ncols
windim = c(60,30) 
mardim=7
layoutmat = matrix(c(1,2,3,4),nrows,ncols, byrow=T)
nf = layout(layoutmat,respect=T,
            widths=c(windim[1],windim[1]),
            heights=c(windim[2],windim[2]+mardim)
)

par(mar = c(0,5,1,0))
plot(log10(df_T_inf$RF_Ces),df_G_CES$NNR_10,pch=pchmask,cex=0.5,col=colmask,axes=F,xlab="",
     ylab="log(g)",cex.lab=1.5, xlim=c(3.9,3.25), ylim=c(6,-1))
points(log10(teffirtf),loggirtf,cex=.5,pch=lc)
box()
text(3.4,0,"CES-NNR-10",pos=4,cex=0.7)
axis(2)

par(mar = c(0,0,1,4))
plot(log10(df_T_inf$RF_Ces),df_G_CES$SVR_50,pch=pchmask,cex=0.5,col=colmask,axes=F,xlab="",
     ylab="log(g)",cex.lab=1.5, xlim=c(3.9,3.25), ylim=c(6,-1))
points(log10(teffirtf),loggirtf,cex=.5,pch=lc)
box()
axis(4)
text(3.4,0,"CES-SVR-50",pos=4,cex=0.7)

par(mar = c(4,5,0,0))
plot(log10(df_T_inf$RF_Ces),df_G_CES$Chi2_50,pch=pchmask,cex=0.5,col=colmask,axes=F,xlab=expression(log(T[eff])),
     ylab="log(g)",cex.lab=1.5, xlim=c(3.9,3.25), ylim=c(6,-1))
points(log10(teffirtf),loggirtf,cex=.5,pch=lc)
box()
axis(1)
axis(2)
text(3.4,0,expression(CES-chi^2-50),pos=4,cex=0.7)
box()
#
par(mar = c(4,0,0,4))
plot(log10(df_T_inf$RF_Ces),df_G_CES$ICA_10,pch=pchmask,cex=0.5,col=colmask,axes=F,xlab=expression(log(T[eff])),
     ylab="log(g)",cex.lab=1.5, xlim=c(3.9,3.25), ylim=c(6,-1))
points(log10(teffirtf),loggirtf,cex=.5,pch=lc)
box()
axis(1)
axis(4)
text(3.4,0,"ICA-10",pos=4,cex=0.7)
box()
axis(1)

dev.off()

################# Metallicities

source("plot-M.r")



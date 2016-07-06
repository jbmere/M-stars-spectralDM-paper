
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
plot(df_T_50$T_teo,df_T_10$MARS,pch=lc2,cex=0.5,col="blue",axes=F,
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
plot(df_T_50$T_teo,df_T_10$NNR,pch=lc2,cex=0.5,col="blue",axes=F,
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
load("~/Escritorio/M-stars-spectralDM-paper/paper/figs/ipac-figs/Gdata_plot.RData")

lc2 <- 16
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
plot(log10(df_T_10$PLS),df_G$RF_10,pch=lc2,cex=0.5,col="red",axes=F,xlab="",
     ylab="log(g)",cex.lab=1.5, xlim=c(3.9,3.25), ylim=c(6,-1))
points(log10(teffirtf),loggirtf,cex=.5,pch=lc)
box()
text(3.4,0,"GA-RF-10",pos=4,cex=0.7)
axis(2)

par(mar = c(0,0,1,4))
plot(log10(df_T_10$PLS),df_G$GB_10,pch=lc2,cex=0.5,col="red",axes=F,xlab="",
     ylab="log(g)",cex.lab=1.5, xlim=c(3.9,3.25), ylim=c(6,-1))
points(log10(teffirtf),loggirtf,cex=.5,pch=lc)
box()
axis(4)
text(3.4,0,"GA-GB-10",pos=4,cex=0.7)

par(mar = c(4,5,0,0))
plot(log10(df_T_10$PLS),df_G$Chi2_50,pch=lc2,cex=0.5,col="blue",axes=F,xlab=expression(log(T[eff])),
     ylab="log(g)",cex.lab=1.5, xlim=c(3.9,3.25), ylim=c(6,-1))
points(log10(teffirtf),loggirtf,cex=.5,pch=lc)
box()
axis(1)
axis(2)
text(3.4,0,"GA-NNR-50",pos=4,cex=0.7)
box()
#
par(mar = c(4,0,0,4))
plot(log10(df_T_10$PLS),df_G$Chi2_inf,pch=lc2,cex=0.5,col="blue",axes=F,xlab=expression(log(T[eff])),
     ylab="log(g)",cex.lab=1.5, xlim=c(3.9,3.25), ylim=c(6,-1))
points(log10(teffirtf),loggirtf,cex=.5,pch=lc)
box()
axis(1)
axis(4)
text(3.4,0,"ICA-10",pos=4,cex=0.7)
box()
axis(1)

dev.off()


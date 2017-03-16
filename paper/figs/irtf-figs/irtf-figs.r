rm(list=ls())

load("Tdata_plot.RData")
load("Gdata_plot.RData")
load("Mdata_plot.RData")
tab3 <- read.table("Table3.txt",sep="|",colClasses = c("character",rep("numeric",4)))
lc <- rep(NA,dim(tab3)[1])
tmp <- tab3[,1]
mask <- grep("IV",tmp)
lc[mask] <- 16
tmp <- gsub("IV","",tmp)
mask <- grep("V",tmp)
lc[mask] <- 15
tmp <- gsub("V","",tmp)
mask <- grep("III",tmp)
lc[mask] <- 17
tmp <- gsub("III","",tmp)
mask <- grep("II",tmp)
lc[mask] <- 18
tmp <- gsub("II","",tmp)
mask <- grep("I",tmp)
lc[mask] <- 18
tmp <- gsub("I","",tmp)
mask <- is.na(lc)
lc[mask] <- 4

irtf.lcs <- read.table("../../../names/IRTF-origweb.txt",sep=",")
irtf.lcs[,1] <- gsub(" ","",irtf.lcs[,1])
irtf.lcs[,1] <- gsub("[\t]","",irtf.lcs[,1])
match <- match(df_M[,1],irtf.lcs[,1])
pec <- is.na(match)
tck <- which(pec)
k <- sum(pec)
for (i in 1:k)
{
  idx <- grep(substr(df_M[tck[i],1],1,10),irtf.lcs[,1],fixed=T)
  print(paste(i," ",df_M[tck[i],1]," ", idx, " ",irtf.lcs[idx,1]))
  print(match)
  match[df_M[,1]==df_M[tck[i],1]] <- idx  
  print(match)
}
irtf.lcs.sorted <- irtf.lcs[match,2]
irtf.lcs.sorted <- gsub("[\t]","",irtf.lcs.sorted)
irtf.lcs.sorted <- gsub(" ","",irtf.lcs.sorted)
lc2 <- rep(NA,length(irtf.lcs.sorted))
tmp <- irtf.lcs.sorted

mask <- grep("IV",tmp)
lc2[mask] <- 16
tmp <- gsub("IV","",tmp)

mask <- grep("V",tmp)
lc2[mask] <- 15
tmp <- gsub("V","",tmp)

mask <- grep("III",tmp)
lc2[mask] <- 17
tmp <- gsub("III","",tmp)

mask <- grep("II",tmp)
lc2[mask] <- 18
tmp <- gsub("II","",tmp)

mask <- grep("I",tmp)
lc2[mask] <- 18
tmp <- gsub("I","",tmp)

mask <- is.na(lc2)
lc2[mask] <- 4

tmp2 <- gsub("M","",tmp)
tmp2 <- gsub(":","",tmp2)
tmp2 <- gsub("a","",tmp2)
tmp2 <- gsub("b","",tmp2)
tmp2 <- gsub("e","",tmp2)
tmp2 <- gsub("\\+","",tmp2)
tmp2 <- gsub("7-7.5","7.25",tmp2)
tmp2 <- gsub("5-9","7",tmp2)
tmp2 <- gsub("7-8","7.5",tmp2)
tmp2 <- gsub("6-9","7.5",tmp2)
tmp2 <- gsub("1-2","1.5",tmp2)
tmp2 <- gsub("3to4","3.5",tmp2)
tmp2 <- gsub("8-9","8.5",tmp2)
tmp2 <- gsub("3-4","3.5",tmp2)
#tmp2 <- gsub("3to4","3.5",tmp2)
tmp2 <- gsub("-","",tmp2)
tmp2 <- as.numeric(tmp2)+10

load("../../../sptype-teff/Teffs.RData")
teff2 <- predict(m2,tmp2)$y
new <- data.frame(x = tmp2)
teff <- predict.lm(m1,new,se.fit=TRUE)$fit

referenceTeff <- df_T_inf$T_Ces
referenceTeff[lc2==15] <- teff2[lc2==15]

################################################


cex.size <- 1
pdf("../irtf-teffs-literature.pdf",width=8,height=8)
par(mar=c(7,7,2,2))
plot(referenceTeff,df_T_inf$KNN,pch=lc2,col="black",xlim=c(1500,4500),ylim=c(1500,4500),
     xlab=expression(T[eff-lit]),ylab=expression(T[eff-est]),cex=cex.size,cex.lab=2.0,
     cex.axis=2.0)
#points(df_T_inf$T_Ces,df_T_10$MARS,col="red",pch=lc2,cex=cex.size/2)
points(referenceTeff,df_T_10$Chi2_10,col="orange",pch=lc2,cex=cex.size/2)
points(referenceTeff,df_T_10$ICA_10,col="blue",pch=lc2,cex=cex.size/2)
abline(0,1)
dev.off()

# Not needed: only 4 without Teff
#cex.size <- 1
#pdf("../irtf-teffs-spts.pdf",width=8,height=8)
#par(mar=c(7,7,2,2))
#plot(teff2,df_T_inf$KNN,pch=lc2,col="black",xlim=c(1500,4500),ylim=c(1500,4500),
#     xlab=expression(T[eff-spt]),ylab=expression(T[eff-est]),cex=cex.size,cex.lab=1.5,cex.axis=1.5)
#points(teff2,df_T_10$Chi2_10,col="orange",pch=lc2,cex=cex.size/2)
#abline(0,1)
#dev.off()
#cex.size <- 1
#pdf("../irtf-teffs-spts-literature.pdf",width=8,height=8)
#par(mar=c(7,7,2,2))
#plot(teff2,df_T_inf$T_Ces,pch=lc2,col="black",xlim=c(1500,4500),ylim=c(1500,4500),
#     xlab=expression(T[eff-spt]),ylab=expression(T[eff-est]),cex=cex.size,cex.lab=1.5,cex.axis=1.5)
#abline(0,1)
#dev.off()




################################################

pdf("../ordieres-fig4.pdf",width=8,height=5)
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
plot(log10(df_T_inf$KNN),df_G$`Rule-Regression_50`,pch=lc2,cex=0.5,col="red",axes=F,xlab="",
     ylab="log(g)",cex.lab=1.5, xlim=c(3.9,3.25), ylim=c(6,-1))
points(log10(tab3[,2]),tab3[,3],cex=.5,pch=lc)
box()
text(3.4,0,"GA-RR-50",pos=4,cex=0.7)
axis(2)

par(mar = c(0,0,1,4))
plot(log10(df_T_inf$KNN),df_G$PLS_50,pch=lc2,cex=0.5,col="red",axes=F,xlab="",
     ylab="log(g)",cex.lab=1.5, xlim=c(3.9,3.25), ylim=c(6,-1))
points(log10(tab3[,2]),tab3[,3],cex=.5,pch=lc)
box()
axis(4)
text(3.4,0,"GA-PLS-50",pos=4,cex=0.7)

par(mar = c(4,5,0,0))
plot(log10(df_T_inf$KNN),df_G$NNR_50,pch=lc2,cex=0.5,col="blue",axes=F,xlab=expression(log(T[eff])),
     ylab="log(g)",cex.lab=1.5, xlim=c(3.9,3.25), ylim=c(6,-1))
points(log10(tab3[,2]),tab3[,3],cex=.5,pch=lc)
box()
axis(1)
axis(2)
text(3.4,0,"GA-NNR-50",pos=4,cex=0.7)
box()
#
par(mar = c(4,0,0,4))
plot(log10(df_T_inf$KNN),df_G$ICA_10,pch=lc2,cex=0.5,col="blue",axes=F,xlab=expression(log(T[eff])),
     ylab="log(g)",cex.lab=1.5, xlim=c(3.9,3.25), ylim=c(6,-1))
points(log10(tab3[,2]),tab3[,3],cex=.5,pch=lc)
box()
axis(1)
axis(4)
text(3.4,0,"ICA-10",pos=4,cex=0.7)
box()
axis(1)

dev.off()

#################    4 CESSETI  ###############################

df_G_ours <- df_G
df_M_ours <- df_M

load("Gdata_CES_plot.RData")
load("Mdata_CES_plot.RData")

df_G_CES <- df_G
df_M_CES <- df_M
df_G <- df_G_ours
df_M <- df_M_ours
rm(df_G_ours,df_M_ours)


cex.size <- 1
pdf("../irtf-CESteffs-literature.pdf",width=8,height=8)
par(mar=c(7,7,2,2))
plot(referenceTeff,df_T_inf$NNR_Ces,pch=lc2,col="black",xlim=c(1500,4500),ylim=c(1500,4500),
     xlab=expression(T[eff-lit]),ylab=expression(T[eff-est]),cex=cex.size,cex.lab=2.0,
     cex.axis=2.0)
#points(df_T_inf$T_Ces,df_T_10$MARS,col="red",pch=lc2,cex=cex.size/2)
points(referenceTeff,df_T_10$Chi2_10,col="orange",pch=lc2,cex=cex.size/2)
points(referenceTeff,df_T_10$ICA_10,col="blue",pch=lc2,cex=cex.size/2)
abline(0,1)
dev.off()


pdf("../irtf-Cesseti.pdf",width=8,height=5)
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
plot(log10(df_T_inf$KNN_Ces),df_G_CES$GB_50,pch=lc2,cex=0.5,col="red",axes=F,xlab="",
     ylab="log(g)",cex.lab=1.5, xlim=c(3.9,3.25), ylim=c(6,-1))
points(log10(tab3[,2]),tab3[,3],cex=.5,pch=lc)
box()
text(3.4,0,"Ces-GB-50",pos=4,cex=0.7)
axis(2)

par(mar = c(0,0,1,4))
plot(log10(df_T_inf$KNN_Ces),df_G_CES$Chi2_50,pch=lc2,cex=0.5,col="red",axes=F,xlab="",
     ylab="log(g)",cex.lab=1.5, xlim=c(3.9,3.25), ylim=c(6,-1))
points(log10(tab3[,2]),tab3[,3],cex=.5,pch=lc)
box()
axis(4)
text(3.4,0,expression("Ces-"~chi^2~"-50"),pos=4,cex=0.7)

par(mar = c(4,5,0,0))
plot(log10(df_T_inf$KNN_Ces),df_G_CES$NNR_50,pch=lc2,cex=0.5,col="blue",axes=F,xlab=expression(log(T[eff])),
     ylab="log(g)",cex.lab=1.5, xlim=c(3.9,3.25), ylim=c(6,-1))
points(log10(tab3[,2]),tab3[,3],cex=.5,pch=lc)
box()
axis(1)
axis(2)
text(3.4,0,"Ces-NNR-50",pos=4,cex=0.7)
box()
#
par(mar = c(4,0,0,4))
plot(log10(df_T_inf$KNN_Ces),df_G_CES$ICA_10,pch=lc2,cex=0.5,col="blue",axes=F,xlab=expression(log(T[eff])),
     ylab="log(g)",cex.lab=1.5, xlim=c(3.9,3.25), ylim=c(6,-1))
points(log10(tab3[,2]),tab3[,3],cex=.5,pch=lc)
box()
axis(1)
axis(4)
text(3.4,0,"ICA-10",pos=4,cex=0.7)
box()
axis(1)

dev.off()

############# Metallicity #############

pcc <- apply(df_M[,c(2:31)],2,function(tmp){
  cor.test(x = tmp,y=df_M$M_teo,method="p")$p.value})

n3 <- read.table("../../../metallicities/NevesIII-IRTF-final.tsv", sep="|")
nt8 <- read.table("../../../metallicities/Neves-t8-IRTF-final.tsv", sep="|")
ra <- read.table("../../../metallicities/RA-IRTF-final.tsv", sep="|")
mann <- read.table("../../../Cross-Match-Catalogues/Mann2015/final-Mann2015-IRTF.tsv", sep="|")
new <- read.table("../../../Cross-Match-Catalogues/Newton2014/final-Newton2014-IRTF.tsv", sep="|")
gaidos <- read.table("../../../Cross-Match-Catalogues/Gaidos2014/final-Gaidos2014-IRTF.tsv", sep="|")

n3.m <- rep(NA,length(df_M[,31]))
nt8.m <- cbind(n3.m,n3.m)
ra.m <- cbind(nt8.m,nt8.m)
mann.m <- n3.m
new.m <- n3.m
gaidos.m <- n3.m

n3[,1] <- gsub(" ","",n3[,1])
idcs <- match(n3[,1],df_M[,1])
n3.m[idcs] <- n3[,18]
nt8[,1] <- gsub(" ","",nt8[,1])
idcs <- match(nt8[,1],df_M[,1])
nt8.m[idcs,1] <- nt8[,17]
nt8.m[idcs,2] <- nt8[,18]
ra[,1] <- gsub(" ","",ra[,1])
idcs <- match(ra[,1],df_M[,1])
ra.m[idcs,1] <- ra[,20]
ra.m[idcs,2] <- ra[,21]
ra.m[idcs,3] <- ra[,22]
ra.m[idcs,4] <- ra[,23]
mann[,1] <- gsub(" ","",mann[,1])
idcs <- match(mann[,1],df_M[,1])
mann.m[idcs] <- mann[,11]
new[,1] <- gsub(" ","",new[,1])
idcs <- match(new[,1],df_M[,1])
new.m[idcs] <- new[,15]
gaidos[,1] <- gsub(" ","",gaidos[,1])
idcs <- match(gaidos[,1],df_M[,1])
gaidos.m[idcs] <- gaidos[,34]

avail <- !(is.na(df_M$M_teo))

pdf("irtf-all-met.pdf")
par(mar = c(6,6,1,1))
for(i in 2:31)
{
  plot(df_M$M_teo, df_M[,i],xlim=c(-1,1),ylim=c(-1,1))
  points(n3.m, df_M[,i],pch=16,col="orange") # Fe/H
  points(nt8.m[,1], df_M[,i],pch=15,col="green") # Fe/H 1
  points(nt8.m[,2], df_M[,i],pch=15,col="darkgreen") # Fe/H 2
  points(ra.m[,1], df_M[,i],pch=17,col="cyan") # M/H
  points(ra.m[,3], df_M[,i],pch=17,col="blue") # Fe/H
  points(mann.m, df_M[,i],pch=18,col="red") # Fe/H
  points(new.m, df_M[,i],pch=19,col="yellow") # Fe/H
  points(gaidos.m, df_M[,i],pch=20,col="black") # Fe/H
  text(0,0.2,paste(colnames(df_M)[i],sum(abs(df_M[avail,i]) < 0.4,na.rm=T)))
  abline(0,1)

  x <- cbind(df_M$M_teo,n3.m,nt8.m[,1],nt8.m[,2],ra.m[,1],ra.m[,3],mann.m,new.m,gaidos.m)
  x2 <- x-df_M[,i]
  x3<- apply(x,1,mean,na.rm=T)
  x4 <- apply(x,1,sd,na.rm=T)
  y <- df_M[,i]
  y1 <- median(x2,na.rm=T)  
  y2 <- y - y1
  y3 <- sd(x-y2,na.rm=T)
  print(paste(colnames(df_M)[i],"=",y3))
}
dev.off()


pdf("irtf-M-ICA10.pdf")
par(mar = c(6,6,1,1))
plot(df_M$M_teo, df_M[,30],pch=lc2,xlim=c(-.8,.8),ylim=c(-.8,.8),xlab="Literature Fe/H or M/H",ylab="M/H",cex.axis=1.5,cex.lab=1.5)
points(n3.m, df_M[,30],pch=lc2,col="orange") # Fe/H
points(nt8.m[,1], df_M[,30],pch=lc2,col="green") # Fe/H 1
points(nt8.m[,2], df_M[,30],pch=lc2,col="darkgreen") # Fe/H 2
points(ra.m[,1], df_M[,30],pch=lc2,col="cyan") # M/H
points(ra.m[,3], df_M[,30],pch=lc2,col="blue") # Fe/H
points(mann.m, df_M[,30],pch=lc2,col="red") # Fe/H
points(new.m, df_M[,30],pch=lc2,col="yellow") # Fe/H
points(gaidos.m, df_M[,30],pch=lc2,col="black") # Fe/H
#  text(0,0.2,paste(colnames(df_M)[30],sum(abs(df_M[avail,i]) < 0.4,na.rm=T)))
abline(0,1)
dev.off()


################ CEsseti ##########################3

pcc_cess <- apply(df_M_CES[,c(2:31)],2,function(tmp){
  cor.test(x = tmp,y=df_M$M_teo,method="p")$p.value})

pdf("IRTF-all-met-CES.pdf")
par(mar = c(6,6,1,1))
for(i in 2:31)
{
  plot(df_M$M_teo, df_M_CES[,i],xlim=c(-1,1),ylim=c(-1,1))
  points(n3.m, df_M[,i],pch=16,col="orange") # Fe/H
  points(nt8.m[,1], df_M[,i],pch=15,col="green") # Fe/H 1
  points(nt8.m[,2], df_M[,i],pch=15,col="darkgreen") # Fe/H 2
  points(ra.m[,1], df_M[,i],pch=17,col="cyan") # M/H
  points(ra.m[,3], df_M[,i],pch=17,col="blue") # Fe/H
  points(mann.m, df_M[,i],pch=18,col="red") # Fe/H
  points(new.m, df_M[,i],pch=19,col="yellow") # Fe/H
  points(gaidos.m, df_M[,i],pch=20,col="black") # Fe/H
  text(0,0.2,paste(colnames(df_M)[i],sum(abs(df_M[avail,i]) < 0.4,na.rm=T)))
  abline(0,1)
}
dev.off()


pdf("irtf-M-CES.pdf")
par(mar = c(6,6,1,1))
plot(df_M$M_teo, df_M_CES[,18],pch=lc2,xlim=c(-.8,.8),ylim=c(-.8,.8),xlab="Literature Fe/H or M/H",ylab="M/H",cex.axis=1.5,cex.lab=1.5)
points(n3.m, df_M[,18],pch=lc2,col="orange") # Fe/H
points(nt8.m[,1], df_M[,18],pch=lc2,col="green") # Fe/H 1
points(nt8.m[,2], df_M[,18],pch=lc2,col="darkgreen") # Fe/H 2
points(ra.m[,1], df_M[,18],pch=lc2,col="cyan") # M/H
points(ra.m[,3], df_M[,18],pch=lc2,col="blue") # Fe/H
points(mann.m, df_M[,18],pch=lc2,col="red") # Fe/H
points(new.m, df_M[,18],pch=lc2,col="yellow") # Fe/H
points(gaidos.m, df_M[,18],pch=lc2,col="black") # Fe/H
#  text(0,0.2,paste(colnames(df_M)[30],sum(abs(df_M[avail,i]) < 0.4,na.rm=T)))
abline(0,1)
dev.off()

#####################################################
pal <- colorRampPalette(c("yellow","red"),space = "rgb")
SetupPalette<-function(c)
{
  nl <- 50
  palette<-pal(nl)
  #palette <- tim.colors(nl)
  col <- c-min(c,na.rm=TRUE)
  col <- col/max(col,na.rm=TRUE)
  colour <- palette[as.integer(((nl-1)*col)+1)]
  return(colour)
}

pdf("../ordieres-fig8.pdf",width=8,height=5)
par(cex.axis=1.0,mar=c(5,5,1,7))
col <- SetupPalette(df_M$ICA_10)
plot(log10(df_T_inf$KNN),df_G$`Rule-Regression_50`,pch=lc2,cex=0.5,col=col,xlab=expression(log(T[eff])),
     ylab="log(g)",cex.lab=1.7, cex.axis=1.5, xlim=c(3.9,3.25), ylim=c(6,-1))
points(log10(tab3[,2]),tab3[,3],cex=.5,pch=lc)
text(3.4,-0.75,"GA-RR-50",pos=4,cex=1.2)
image.plot(legend.only=TRUE, zlim= range(df_M$ICA_10,na.rm=T), horizontal=FALSE,
           legend.width=2, reset.graphics=TRUE, axis.args=list(cex.axis=1.5,cex.lab=1),
           legend.mar=6,col=pal(50))
dev.off()

save.image("irtf.Rdata")





load("Tdata_plot.RData")
load("Gdata_plot.RData")
load("Mdata_plot.RData")
tab3 <- read.table("Table3.txt",sep="|",colClasses = c("character",rep("numeric",4)))
lc <- rep(NA,dim(tab3)[1])
tmp <- tab3[,1]
mask <- grep("IV",tmp)
lc[mask] <- 4
tmp <- gsub("IV","",tmp)
mask <- grep("V",tmp)
lc[mask] <- 5
tmp <- gsub("V","",tmp)
mask <- grep("III",tmp)
lc[mask] <- 3
tmp <- gsub("III","",tmp)
mask <- grep("II",tmp)
lc[mask] <- 2
tmp <- gsub("II","",tmp)
mask <- grep("I",tmp)
lc[mask] <- 1
tmp <- gsub("I","",tmp)
mask <- is.na(lc)
lc[mask] <- 6

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
plot(log10(df_T_inf$KNN),df_G$`Rule-Regression_50`,pch=16,cex=0.5,col="red",axes=F,xlab="",
     ylab="log(g)",cex.lab=1.5, xlim=c(3.9,3.25), ylim=c(6,-1))
points(log10(tab3[,2]),tab3[,3],cex=.5,pch=lc)
box()
text(3.4,0,"GA-RR-50",pos=4,cex=0.7)
axis(2)

par(mar = c(0,0,1,4))
plot(log10(df_T_inf$KNN),df_G$PLS_50,pch=16,cex=0.5,col="red",axes=F,xlab="",
     ylab="log(g)",cex.lab=1.5, xlim=c(3.9,3.25), ylim=c(6,-1))
points(log10(tab3[,2]),tab3[,3],cex=.5,pch=lc)
box()
axis(4)
text(3.4,0,"GA-PLS-50",pos=4,cex=0.7)

par(mar = c(4,5,0,0))
plot(log10(df_T_inf$KNN),df_G$NNR_50,pch=16,cex=0.5,col="blue",axes=F,xlab=expression(log(T[eff])),
     ylab="log(g)",cex.lab=1.5, xlim=c(3.9,3.25), ylim=c(6,-1))
points(log10(tab3[,2]),tab3[,3],cex=.5,pch=lc)
box()
axis(1)
axis(2)
text(3.4,0,"GA-NNR-50",pos=4,cex=0.7)
box()
#
par(mar = c(4,0,0,4))
plot(log10(df_T_inf$KNN),df_G$ICA_10,pch=16,cex=0.5,col="blue",axes=F,xlab=expression(log(T[eff])),
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

kcc <- apply(df_M[,c(2:30)],2,function(tmp){
  cor.test(x = tmp,y=df_M$M_teo,method="k")$p.value})
scc <- apply(df_M[,c(2:30)],2,function(tmp){
  cor.test(x = tmp,y=df_M$M_teo,method="s")$p.value})
pcc <- apply(df_M[,c(2:30)],2,function(tmp){
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
pdf("tmp.pdf")
par(mar = c(6,6,1,1))
for(i in 2:30)
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
}
dev.off()


pdf("M-ICA10.pdf")
par(mar = c(6,6,1,1))
plot(df_M$M_teo, df_M[,30],xlim=c(-.8,.8),ylim=c(-.8,.8),xlab="Literature Fe/H or M/H",ylab="M/H",cex.axis=1.5,cex.lab=1.5)
  points(n3.m, df_M[,30],pch=16,col="orange") # Fe/H
  points(nt8.m[,1], df_M[,30],pch=15,col="green") # Fe/H 1
  points(nt8.m[,2], df_M[,30],pch=15,col="darkgreen") # Fe/H 2
  points(ra.m[,1], df_M[,30],pch=17,col="cyan") # M/H
  points(ra.m[,3], df_M[,30],pch=17,col="blue") # Fe/H
  points(mann.m, df_M[,30],pch=18,col="red") # Fe/H
  points(new.m, df_M[,30],pch=19,col="yellow") # Fe/H
  points(gaidos.m, df_M[,30],pch=20,col="black") # Fe/H
#  text(0,0.2,paste(colnames(df_M)[30],sum(abs(df_M[avail,i]) < 0.4,na.rm=T)))
  abline(0,1)
dev.off()



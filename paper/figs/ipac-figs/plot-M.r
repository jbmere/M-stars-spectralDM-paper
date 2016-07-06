############# Metallicity #############

kcc <- apply(df_M[,c(2:30)],2,function(tmp){
  cor.test(x = tmp,y=df_M$M_teo,method="k")$p.value})
scc <- apply(df_M[,c(2:30)],2,function(tmp){
  cor.test(x = tmp,y=df_M$M_teo,method="s")$p.value})
pcc <- apply(df_M[,c(2:30)],2,function(tmp){
  cor.test(x = tmp,y=df_M$M_teo,method="p")$p.value})

n3 <- read.table("../../../metallicities/NevesIII-IPAC-final.tsv", sep="|")
nt8 <- read.table("../../../metallicities/Neves-t8-IPAC-final.tsv", sep="|")
ra <- read.table("../../../metallicities/RA-IPAC-final.tsv", sep="|")
mann <- read.table("../../../Cross-Match-Catalogues/Mann2015/final-Mann2015-IPAC.tsv", sep="|")
new <- read.table("../../../Cross-Match-Catalogues/Newton2014/final-Newton2014-IPAC.tsv", sep="|")
gaidos <- read.table("../../../Cross-Match-Catalogues/Gaidos2014/final-Gaidos2014-IPAC.tsv", sep="|")

n3.m <- rep(NA,length(df_M[,31]))
nt8.m <- cbind(n3.m,n3.m)
ra.m <- cbind(nt8.m,nt8.m)
mann.m <- n3.m
new.m <- n3.m
gaidos.m <- n3.m

n3[,1] <- gsub(" ","",n3[,1])
n3[,1] <- gsub("2MASS","",n3[,1])
idcs <- match(n3[,1],df_M[,33])
n3.m[c(idcs[!is.na(idcs)])] <- n3[which(!is.na(idcs)),18]
nt8[,1] <- gsub(" ","",nt8[,1])
nt8[,1] <- gsub("2MASS","",nt8[,1])
idcs <- match(nt8[,1],df_M[,33])
nt8.m[c(idcs[!is.na(idcs)]),1] <- nt8[which(!is.na(idcs)),17]
nt8.m[c(idcs[!is.na(idcs)]),2] <- nt8[which(!is.na(idcs)),18]
ra[,1] <- gsub(" ","",ra[,1])
ra[,1] <- gsub("2MASS","",ra[,1])
idcs <- match(ra[,1],df_M[,33])
ra.m[c(idcs[!is.na(idcs)]),1] <- ra[which(!is.na(idcs)),20]
ra.m[c(idcs[!is.na(idcs)]),2] <- ra[which(!is.na(idcs)),21]
ra.m[c(idcs[!is.na(idcs)]),3] <- ra[which(!is.na(idcs)),22]
ra.m[c(idcs[!is.na(idcs)]),4] <- ra[which(!is.na(idcs)),23]
mann[,1] <- gsub(" ","",mann[,1])
mann[,1] <- gsub("2MASS","",mann[,1])
idcs <- match(mann[,1],df_M[,33])
mann.m[c(idcs[!is.na(idcs)])] <- mann[which(!is.na(idcs)),11]
new[,1] <- gsub(" ","",new[,1])
new[,1] <- gsub("2MASS","",new[,1])
idcs <- match(new[,1],df_M[,33])
new.m[c(idcs[!is.na(idcs)])] <- new[which(!is.na(idcs)),15]
gaidos[,1] <- gsub(" ","",gaidos[,1])
gaidos[,1] <- gsub("2MASS","",gaidos[,1])
idcs <- match(gaidos[,1],df_M[,33])
gaidos.m[c(idcs[!is.na(idcs)])] <- gaidos[which(!is.na(idcs)),34]

avail <- !(is.na(df_M$M_teo))

pdf("tmp.pdf")
par(mar = c(6,6,1,1))
for(i in 2:31)
{
  plot(df_M$M_teo, df_M[,i],xlim=c(-3,1),ylim=c(-3,1))
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

lc2=16
pdf("M-RFInf.pdf")
par(mar = c(6,6,1,1))
plot(df_M$M_teo, df_M[,2],pch=lc2,xlim=c(-1.5,1),ylim=c(-1.5,1),xlab="Literature Fe/H or M/H",ylab="M/H",cex.axis=1.5,cex.lab=1.5)
points(n3.m, df_M[,2],pch=lc2,col="orange") # Fe/H
points(nt8.m[,1], df_M[,2],pch=lc2,col="green") # Fe/H 1
points(nt8.m[,2], df_M[,2],pch=lc2,col="darkgreen") # Fe/H 2
points(ra.m[,1], df_M[,2],pch=lc2,col="cyan") # M/H
points(ra.m[,3], df_M[,2],pch=lc2,col="blue") # Fe/H
points(mann.m, df_M[,2],pch=lc2,col="red") # Fe/H
points(new.m, df_M[,2],pch=lc2,col="yellow") # Fe/H
points(gaidos.m, df_M[,2],pch=lc2,col="black") # Fe/H
#  text(0,0.2,paste(colnames(df_M)[30],sum(abs(df_M[avail,i]) < 0.4,na.rm=T)))
abline(0,1)
dev.off()



load("datosT.RData")

names(datosT)
attach(datosT)

plot(T,datosT[,5])
abline(0,1)

a <- datosT[,5:28]
a <- a-T
a <- a^2
a <- sqrt(apply(a,2,median,na.rm=T))
a <- sqrt(apply(a,2,sum,na.rm=T)/length(a))

plot(a[1:8],ylim=c(min(a),max(a)))
points(a[9:16],pch=16,col="blue")
points(a[17:24],pch=15,col="red")

LC[LC=="V"]=15
LC[LC=="III"]=17


#Best RMSE
pdf("../../../ipac_LG_Trojas_Tknn_10.pdf")
par(mar=c(5,5,1,1))
plot(10^T,10^datosT[,5],pch=as.numeric(LC),xlim=c(2000,4500),ylim=c(2000,4500),
     xlab=expression(T[eff](Literature)),ylab=expression(T[eff](GA)),
     cex.lab=1.4,cex.axis=1.4)
abline(0,1)
#Best median
points(10^T,10^datosT[,26],pch=as.numeric(LC),col="blue")
dev.off()


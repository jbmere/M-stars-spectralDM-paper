

load("datosT.RData")

names(datosT)
attach(datosT)

#plot(T,datosT[,5])
#abline(0,1)

#a <- datosT[,5:28]
#a <- a-T
#a <- a^2
#a <- sqrt(apply(a,2,median,na.rm=T))
#a <- sqrt(apply(a,2,sum,na.rm=T)/length(a))

#plot(a[1:8],ylim=c(min(a),max(a)))
#points(a[9:16],pch=16,col="blue")
#points(a[17:24],pch=15,col="red")

LC[LC=="V"]=15
LC[LC=="III"]=17

# Last results indicated by ESM 2017
load("../../LSB_Tdata_plot_IPAC.RData")

idcs <- c(grep("Gl_228_A",df_T_inf[,1]),
          grep("Gl_514",df_T_inf[,1]),
          grep("Gl_825",df_T_inf[,1]),
          grep("Gl_880",df_T_inf[,1]),
          grep("Gl_908",df_T_inf[,1]),
          grep("Gl_250_B",df_T_inf[,1]),
          grep("Gl_176",df_T_inf[,1]),
          grep("Gl_317",df_T_inf[,1]),
          grep("Gl_436",df_T_inf[,1]),
          grep("Gl_581",df_T_inf[,1]),
          grep("Gl_628",df_T_inf[,1]),
          grep("Gl_674",df_T_inf[,1]),
          grep("Gl_849",df_T_inf[,1]),
          grep("Gl_876",df_T_inf[,1])
)

LindgrenHS<- rbind(
  c(3300,4.82,-0.29),
  c(3727,4.78,0.07),
  c(3727,4.78,0.07),
  c(3795,4.69,-0.01),
  c(3720,4.74,0.20),
  c(3646,4.86,-0.51),
  c(3646,4.86,-0.51),
  c(3550,4.80,-0.07),
  c(3550,4.76,-0.11),
  c(3375,4.97,0.16),
  c(3400,4.80,0.03),
  c(3400,4.80,0.03),
  c(3350,4.92,-0.02),
  c(3275,4.93,0.12),
  c(3350,4.88,-0.01),
  c(3350,4.76,0.28),
  c(3250,4.89,0.19)
)


#Best RMSE
pdf("../../../ipac_LG_Trojas_Tknn_10.pdf")
par(mar=c(5,5,1,1))
plot(10^T,10^datosT[,5],pch=as.numeric(LC),xlim=c(2000,4500),ylim=c(2000,4500),
     xlab=expression(T[eff](Literature)),ylab=expression(T[eff](GA)),
     cex.lab=1.4,cex.axis=1.4)
abline(0,1)
#Best median
points(10^T,10^datosT[,26],pch=as.numeric(LC),col="blue")
points(LindgrenHS[,1],df_T_10$MARS[idcs],pch=15, col="orange")
points(LindgrenHS[,1],df_T_inf$RF[idcs],pch=15, col="darkgreen")
abline(-100,1,col="grey")
abline(100,1,col="grey")
dev.off()


load("LSB_Tdata_plot_IPAC.RData")
load("Gdata_plot.RData")
load("Mdata_plot.RData")

idcs <- c(grep("Gl_228_A",df_G[,1]),
          grep("Gl_514",df_G[,1]),
          grep("Gl_825",df_G[,1]),
          grep("Gl_880",df_G[,1]),
          grep("Gl_908",df_G[,1]),
          grep("Gl_250_B",df_G[,1]),
          grep("Gl_176",df_G[,1]),
          grep("Gl_317",df_G[,1]),
          grep("Gl_436",df_G[,1]),
          grep("Gl_581",df_G[,1]),
          grep("Gl_628",df_G[,1]),
          grep("Gl_674",df_G[,1]),
          grep("Gl_849",df_G[,1]),
          grep("Gl_876",df_G[,1])
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

plot(LindgrenHS[,1],df_T_50$Chi2_50[idcs]-LindgrenHS[,1],xlim=c(3000,4000),ylim=c(-300,300),pch=16)
abline(0,0)
abline(-100,0,col="grey")
abline(100,0,col="grey")

pdf("../ESM2017-logg.pdf")
par(mar=c(5,5,1,1))
plot(LindgrenHS[,2],df_G$ICA_10[idcs]-LindgrenHS[,2],xlim=c(4.6,5),ylim=c(-1,1),pch=15,
     xlab=expression(log(g)[LHS]),ylab=expression(log(g)[ICA10]-log(g)[LHS]) )
abline(0,0)
abline(-0.1,0,col="grey")
abline(0.1,0,col="grey")
dev.off()

plot(LindgrenHS[,3],df_M$RF_Inf[idcs]-LindgrenHS[,3],xlim=c(-1,1),ylim=c(-1,1),pch=16)
abline(0,0)
abline(-0.1,0,col="grey")
abline(0.1,0,col="grey")





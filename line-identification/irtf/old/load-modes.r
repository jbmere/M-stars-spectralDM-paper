require(fields)
SetupPalette<-function(c)
{
pal <- colorRampPalette(c("blue","green","yellow","orange","red"),space = "rgb")
nl <- 50
#palette<-pal(nl)
palette <- tim.colors(nl)
col <- c-min(c,na.rm=TRUE)
col <- col/max(col,na.rm=TRUE)
colour <- palette[as.integer(((nl-1)*col)+1)]
return(colour)
}

load("BT_SETTL_pb_clean.RData")

sp<-matrix(NA,length(bp_clean),ncol=3)
for (i in 1:length(bp_clean)){
  sp[i,] <- c(bp_clean[[i]]$stellarp[[1]],
  bp_clean[[i]]$stellarp[[2]],
  bp_clean[[i]]$stellarp[[3]])
}
  
# Region around Ca triplet
plot(bp_clean[[1]]$data[[1]],xlim=c(8600, 8700),ty="l",ylim=c(0,0.002),col="white")

for (i in 2:length(bp_clean)){

  if(bp_clean[[i]]$stellarp[[3]] == 0 & bp_clean[[i]]$stellarp[[2]] == -5)
     {
       lines(bp_clean[[i]]$data[[1]])
     }

   }


# Region around Pa 1
plot(bp_clean[[1]]$data[[1]],xlim=c(8600, 8700),ty="l",ylim=c(0,0.005),col="white")

col <- SetupPalette(sp[,1])

for (i in 2:length(bp_clean)){

  if(bp_clean[[i]]$stellarp[[3]] == 0 & bp_clean[[i]]$stellarp[[2]] == -5)
     {
       lines(bp_clean[[i]]$data[[1]],col=col[i])
     }

   }

xa <- 8623.0 
xb <- 8648.2
lines(c(xa,xa),c(0,0.005))
lines(c(xb,xb),c(0,0.005))
xa <- 8655.4
xb <- 8680.6
lines(c(xa,xa),c(0,0.005),col="red")
lines(c(xb,xb),c(0,0.005),col="red")

xa <- 8461.0 
xb <- 8471.8
lines(c(xa,xa),c(0,0.005))
lines(c(xb,xb),c(0,0.005))
xa <- 8482.6
xb <- 8522.2
lines(c(xa,xa),c(0,0.005),col="red")
lines(c(xb,xb),c(0,0.005),col="red")

xa <- 8756.2 
xb <- 8767.0
lines(c(xa,xa),c(0,0.005))
lines(c(xb,xb),c(0,0.005))
xa <- 8720.2
xb <- 8752.6
lines(c(xa,xa),c(0,0.005),col="red")
lines(c(xb,xb),c(0,0.005),col="red")

xa <- 8986.6 
xb <- 8997.4
lines(c(xa,xa),c(0,0.005))
lines(c(xb,xb),c(0,0.005))
xa <- 8925.4
xb <- 8950.6
lines(c(xa,xa),c(0,0.005),col="red")
lines(c(xb,xb),c(0,0.005),col="red")

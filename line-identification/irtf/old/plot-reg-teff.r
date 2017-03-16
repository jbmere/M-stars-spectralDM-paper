
library(fields)
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

data <- NULL
teff <- NULL
logg <- NULL
met <- NULL

target <- "BT_SETTL_pb_clean.RData"
load(target)
for (i in 1:length(bp_clean))
  {
    if (bp_clean[[i]]$stellarp[2] == -5 & bp_clean[[i]]$stellarp[3] == 0 )
      {
        data <- rbind(data,bp_clean[[i]]$data[[1]][,2])
        teff <- rbind(teff,as.numeric(bp_clean[[i]]$stellarp[1]))
        logg <- rbind(logg,as.numeric(bp_clean[[i]]$stellarp[2]))
        met <- rbind(met,as.numeric(bp_clean[[i]]$stellarp[3]))
      }
  }
wav <- bp_clean[[1]]$data[[1]][,1]

convolved <- T

if (!convolved)
  {
    target <- "bt_orig_cortado.RData"
    load(target)
    data <- NULL
    for (i in 1:length(bp_clean))
      {
        if (bp_clean[[i]]$stellarp[2] == -5 & bp_clean[[i]]$stellarp[3] == 0 )
          {
            data <- rbind(data,bts_clean[[i]]$data[,2])
          }
      }
    wav <- bts_clean[[1]]$data[,1]
  }

c <- SetupPalette(teff)
lims <-  rbind(
               c(8461.0,0,8471.8,0.035),
               c(8630.2,0,8641.0,0.035),
               c(8662.6,0,8680.6,0.035),
               c(8756.1,0,8767.0,0.035),
               c(8986.6,0,8997.4,0.035))

lims2 <-  rbind(
               c(8482.6,0,8522.2,0.035),
               c(8655.4,0,8680.6,0.035),
               c(8623.0,0,8648.2,0.035),
               c(8720.2,0,8752.6,0.035),
               c(8925.4,0,8950.6,0.035))

               
for (j in 1:5){
  jpeg(paste("Feature",j,".jpg",sep=""),quality=100,width=1000, height=1000)
  par(mar=c(7,7,5,10))
  plot(wav,data[1,], ty="l",col=c[1],ylim=c(0,0.035),main=paste("Feature ",j,sep=""),
       cex.axis=2, cex.lab=2, xlab="Lambda(Angstroms)", ylab="Flux(relative units)")
  for ( i in 2:(dim(data)[1]))
    {
      lines(wav,data[i,]+(i-1)*0.0015,col=c[i])
    }
  rect(lims[j,1],lims[j,2],lims[j,3],lims[j,4],)
  rect(lims2[j,1],lims2[j,2],lims2[j,3],lims2[j,4],col="grey",density=5)

  paschen <- c(955,923,902,887,874.8,866.3,859.6,854.3,850.0,846.5)
  for (w in 1:length(paschen)){abline(v=10*paschen[w]+3,col="blue",lwd=1)}
  caii <- c(849.8,854.2,866.2)
  for (w in 1:length(caii)){abline(v=10*caii[w]+3,col="red",lwd=1)}
  magi <- 880.7
  abline(v=10*magi+3,col="green",lwd=1)
  
  image.plot(legend.only=TRUE, zlim= range(teff), horizontal=FALSE,
  legend.width=4, reset.graphics=TRUE, axis.args=list(cex.axis=2,cex.lab=2),
  legend.mar=8)

  dev.off()
}

  jpeg(paste("All-Teff.jpg",sep=""),quality=100,width=1000, height=1000)
  par(mar=c(7,7,5,10))
  plot(wav,data[1,], ty="l",col=c[1],ylim=c(0,0.035),main=paste("Feature ",j,sep=""),
       cex.axis=2, cex.lab=2, xlab="Lambda(Angstroms)", ylab="Flux(relative units)")

for (j in 1:5){
  for ( i in 2:(dim(data)[1]))
    {
      lines(wav,data[i,]+(i-1)*0.0015,col=c[i])
    }
  rect(lims[j,1],lims[j,2],lims[j,3],lims[j,4],)
  rect(lims2[j,1],lims2[j,2],lims2[j,3],lims2[j,4],col="grey",density=5)

  paschen <- c(955,923,902,887,874.8,866.3,859.6,854.3,850.0,846.5)
  for (w in 1:length(paschen)){abline(v=10*paschen[w]+3,col="blue",lwd=1)}
  caii <- c(849.8,854.2,866.2)
  for (w in 1:length(caii)){abline(v=10*caii[w]+3,col="red",lwd=1)}
  magi <- 880.7
  abline(v=10*magi+3,col="green",lwd=1)
  
  image.plot(legend.only=TRUE, zlim= range(teff), horizontal=FALSE,
  legend.width=4, reset.graphics=TRUE, axis.args=list(cex.axis=2,cex.lab=2),
  legend.mar=8)

}
  
  dev.off()

library("graphics")
library(fields)
SetupPalette<-function(c)
{
pal <- colorRampPalette(c("blue","red"),space = "rgb")
nl <- 50
palette<-pal(nl)
#palette <- tim.colors(nl)
col <- c-min(c,na.rm=TRUE)
col <- col/max(col,na.rm=TRUE)
colour <- palette[as.integer(((nl-1)*col)+1)]
return(colour)
}

data <- NULL
teff <- NULL
logg <- NULL
met <- NULL

target <- "BT-Settl-2011-IRTF.RData"
load(target)

for (i in 1:length(bp_clean))
  {
    if (bp_clean[[i]]$stellarp[2] == 5 & bp_clean[[i]]$stellarp[3] == 0 )
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
        if (bp_clean[[i]]$stellarp[2] == 5 & bp_clean[[i]]$stellarp[3] == 0 )
          {
            data <- rbind(data,bts_clean[[i]]$data[,2])
          }
      }
    wav <- bts_clean[[1]]$data[,1]
  }

c <- SetupPalette(teff)


plot.fig <- function(snr,physpar)
{
  pdf(paste("../../paper/figs/BT-spectraAtIRTF-",snr,"-",physpar,".pdf",sep=""))
  par(mar=c(7,7,3,7))
  plot(wav,data[1,], ty="l",col=c[1],xlim=c(left,right), ylim=c(0,top),
       cex.axis=1.5, cex.lab=1.5, xlab="Lambda(Angstroms)", ylab="Flux(relative units)")
  for (j in 1:dim(lims)[1])
  {
    rect(lims[j,1],lims[j,2],lims[j,3],lims[j,4])
    rect(lims2[j,1],lims2[j,2],lims2[j,3],lims2[j,4],col="grey",density=-5)
  }
  for ( i in 2:(dim(data)[1]))
  {
    lines(wav,data[i,]+(i-1)*0.000015,col=c[i])
  }
  
  image.plot(legend.only=TRUE, zlim= range(teff), horizontal=FALSE,
             legend.width=2, reset.graphics=TRUE, axis.args=list(cex.axis=1.3,cex.lab=1.3),
             legend.mar=6,col=c)
  
  dev.off()
}


top <- 0.00065; right<- 14000; left <- min(wav)
#physpar <- "teff"
#physpar <- "logg"
physpar <- "mh"
#snr <- "Inf"
#snr <- 10
snr <- 50
#filename <- paste(physpar,"-features-snr",snr,".r",sep="")
filename <- paste(physpar,"-features-snr",snr,".r",sep="")
#filename <- paste(physpar,"-features-snr",snr,".r",sep="")
source(filename)
plot.fig(snr,physpar)




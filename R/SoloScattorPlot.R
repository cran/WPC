SoloScattorPlot <-
function(data,cutoff,xlab=c("Marker"),ylab=c("Time to Event"),main=c("Scattor Plot"),ylim=c(0,3600),xlim=c(0,100),
				col1="red",col2="red",lwd=2,pch1=20,pch2=21,legendloc="bottomright",legendtxt=c("death","censor"),ncol=1)
	{	   plot(data$marker[which(data$censor==1)],data$event[which(data$censor==1)],xlab=xlab,ylab=ylab,main=main,ylim=ylim,xlim=xlim,col=col1,pch=pch1)
		   points(data$marker[which(data$censor==0)],data$event[which(data$censor==0)],col=col1,pch=pch2,lwd=lwd)
		   legend(legendloc,legendtxt,col=c(col1,col1),ncol=ncol,bty="n",cex=0.8,pch=c(pch1,pch2))
		   abline(h=c(cutoff),lty=3,col=col2,lwd=lwd)	}

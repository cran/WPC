DuoScattorPlot <-
function(data1,data2,cutoff,xlab=c("Marker"),ylab=c("Time to Event"),main="Scattor Plot",ylim=c(0,3600),xlim=c(0,100),
				col1="red",col2="black",col3="tomato",lwd=2,pch1=20,pch2=21,legendloc="bottomright",legendtxt=c("death - group1","censor - group1","death - group2","censor - group2"),ncol=1)	
	{	   plot(data1$marker[which(data1$censor==1)],data1$event[which(data1$censor==1)],xlab=xlab,ylab=ylab,main=main,ylim=ylim,xlim=xlim,col=col1,pch=pch1)
		   points(data1$marker[which(data1$censor==0)],data1$event[which(data1$censor==0)],col=col1,pch=pch2,lwd=lwd)
  		   points(data2$marker[which(data2$censor==1)],data2$event[which(data2$censor==1)],xlab=xlab,ylab=ylab,main=main,ylim=ylim,xlim=xlim,col=col2,pch=pch1)
		   points(data2$marker[which(data2$censor==0)],data2$event[which(data2$censor==0)],col=col2,pch=pch2,lwd=lwd)
		   legend(legendloc,legendtxt,col=c(col1,col1,col2,col2),ncol=ncol,bty="n",cex=0.8,pch=c(pch1,pch2,pch1,pch2))
		   abline(h=c(cutoff),lty=3,col=col3,lwd=lwd)}

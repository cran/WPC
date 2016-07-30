SoloWPCCurve <-
function(wpc,xlab=c("Marker"),ylab=c("Survival Rate"),main=c("Weighted Predictiveness Curve"),ylim=c(0,1),xlim=c(0,100),type="l",col="red",lwd=2,
			legendloc="bottomright",legendtxt=c("Method1"),confi="N",ptsest="N")
	{	   plot(wpc$x,wpc$s,xlab=xlab,ylab=ylab,main=main,ylim=ylim,xlim=xlim,type=type,col=col,lwd=lwd)	
		   if(confi=="N"){legend(legendloc,legendtxt,col = c(col),bty="n",lty=c(1),lwd=c(lwd))}
	 	   if(confi=="Y"){
			points(wpc$x,wpc$lb,lwd=2,lty=2,col=col,type="l")
		   	points(wpc$x,wpc$ub,lwd=2,lty=2,col=col,type="l")
			color_transparent <- adjustcolor(col, alpha.f = 0.1) 	
			polygon(c(wpc$x, rev(wpc$x)), c(wpc$lb, rev(wpc$ub)),col = color_transparent, border = NA) 
			legend(legendloc,legendtxt,col = c(col,col),bty="n",lty=c(1,2),lwd=c(lwd,lwd))}	
		   if(ptsest=="Y"){points(wpc$x,wpc$y,lwd=3,col=col)}}

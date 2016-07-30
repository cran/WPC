DuoWPCCurve <-
function(wpc1,wpc2,xlab="Marker",ylab="Survival Rate",main="Weighted Predictiveness nqCurve",ylim=c(0,1),xlim=c(0,100),type="l",col1="red",
				col2="blue",lwd=2,legendloc="bottomright",legendtxt=c("Method1","Method2"),confi="N",ptsest="N")
	{	   plot(wpc1$x, wpc1$s, xlab=xlab, ylab=ylab, main=main, ylim=ylim, xlim=xlim, type=type,col=col1,lwd=lwd)
		   points(wpc2$x, wpc2$s, type=type, col=col2, lwd=lwd)
		   if(confi=="N"){legend(legendloc,legendtxt,col = c(col1,col2),bty="n",lty=c(1,1),lwd=c(lwd,lwd))}				  
		   if(confi=="Y"){
 		   	points(wpc1$x,wpc1$lb,lwd=2,lty=2,col=col1,type="l")
		  	points(wpc1$x,wpc1$ub,lwd=2,lty=2,col=col1,type="l")
			color_transparent <- adjustcolor(col1, alpha.f = 0.1) 	
			polygon(c(wpc1$x, rev(wpc1$x)), c(wpc1$lb, rev(wpc1$ub)),col = color_transparent, border = NA)  		  
			points(wpc2$x,wpc2$lb,lwd=2,lty=2,col=col2,type="l")
		   	points(wpc2$x,wpc2$ub,lwd=2,lty=2,col=col2,type="l")
			color_transparent <- adjustcolor(col2, alpha.f = 0.1) 	
			polygon(c(wpc2$x, rev(wpc2$x)), c(wpc2$lb, rev(wpc2$ub)),col = color_transparent, border = NA)  	
			legend(legendloc,legendtxt,col = c(col1,col1,col2,col2),bty="n",lty=c(1,2,1,2),lwd=c(lwd,2,lwd,2))	}	
		    if(ptsest=="Y"){
			points(wpc1$x,wpc1$y,lwd=3,col=col1)
			points(wpc2$x,wpc2$y,lwd=3,col=col2)}}

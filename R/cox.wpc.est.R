cox.wpc.est <-
function(event,censor,marker,cutoff,quantile=0.95)
	{	coxmd = coxph(Surv(event,censor)~marker)
		coxsurvf = summary(survfit(coxmd, newdata = data.frame(marker),se.fit=T,conf.int = quantile))
		coxfit   = coxsurvf$surv[sum(as.numeric(coxsurvf$time <= cutoff)), ]
		coxlower = coxsurvf$lower[sum(as.numeric(coxsurvf$time <= cutoff)), ]  
		coxupper = coxsurvf$upper[sum(as.numeric(coxsurvf$time <= cutoff)), ]
		coxre = cbind(marker,coxfit,coxlower,coxupper)
		coxre = coxre[order(coxre[,1]),]
	return(list(x=coxre[,1],y=coxre[,2],s=coxre[,2],lb=coxre[,3],ub=coxre[,4]))	}

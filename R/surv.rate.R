surv.rate <-
function(data,cutoff,wts,xwin)
	{	   event = data$event
		  censor = data$censor
		 tmp.fit = survfit(Surv(event,censor)~1,data=data, weight = wts)
		tmp.time = summary(tmp.fit)$time
		tmp.surv = summary(tmp.fit, times = cutoff)$surv
		
		if(length(tmp.surv)==0)
			{	if(max(event) <= cutoff && length(event[which(censor == 1)])>0 && length(tmp.time)>0)
				{	y = summary(tmp.fit,times = max(tmp.time))$surv
					x = xwin	}
                  	else{ y = NA ; x = NA}		}
		if(length(tmp.surv)>0)
			{	if(tmp.surv == 0)
				{	if(min(event[which(censor == 1)])>=cutoff){y = 1 ; x = xwin}
					else{y = tmp.surv ; x = xwin}		}
				if(tmp.surv > 0){y = tmp.surv ; x = xwin }	}
            
	return(list(x = x, y = y))	}

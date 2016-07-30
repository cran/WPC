npr.wpc.est <-
function(event,censor,marker,cutoff,method,weights,wdth=0,nsub=0,sspeed,df=2,confi="NO",nbtsp=1000,quantile=0.95)
	{       if(method == "window.width")  {summary = ww.windows(event,censor,marker,wdth,sspeed)}
              if(method == "number.subjt")  {summary = ns.windows(event,censor,marker,nsub,sspeed)}
             
		  fit = vector("list",summary$ntotal); x = y = X = Y = s = NA

              for(i in 1:summary$ntotal)
			{	if(summary$nsam[i]!=0)
					{	if(weights == "uniform"   || summary$winsize[i]==0)
							{	wts = rep(1,summary$nsam[i])		}
                   			if(weights == "normal"    && summary$winsize[i]!=0)
							{	wts = dnorm(x=summary$wdata[[i]]$marker , mean=summary$xwin[i] , sd=summary$winsize[i]/8)		}
                   			if(weights == "trunnormal"&& summary$winsize[i]!=0)
							{	wts = dtnorm(summary$wdata[[i]]$marker , summary$xwin[i] , sd=summary$winsize[i]/8 ,
                        						     summary$xwin[i]-summary$winsize[i]/8 , summary$xwin[i]+summary$winsize[i]/8)		}
                   			if(weights == "huber"     && summary$winsize[i]!=0)
							{	wts = ifelse(abs(summary$wdata[[i]]$marker-summary$xwin[i]) <= summary$winsize[i]/8, 1, 
											summary$winsize[i]/8/abs(summary$wdata[[i]]$marker-summary$xwin[i]))		}
                   		fit[[i]] = surv.rate(summary$wdata[[i]] , cutoff , wts , summary$xwin[i])
                  	    	    x[i] = fit[[i]]$x
                                  y[i] = fit[[i]]$y		} 
                 		else{	x[i] = NA ; y[i]=NA}	}

			Y = na.omit(y)
                	X = na.omit(x)

                 	s = predict(loess(Y ~ X,df = df))
                 	s[which(s<=0)]=0;	s[which(s>=1)]=1	
		 
		if(confi=="NO"){	return(list(x=X,y=Y,s=s)) }
		if(confi == "YES"){	n = length(marker)
						nxwin = length(x)
						data = data.frame(event,censor,marker)
						REboot =REcurve= vector("list",nbtsp)
						REmatrix = matrix(NA,nxwin,nbtsp)
						sw.lower = sw.upper=slb=sub  =lb=ub= NA

						for(j in 1:nbtsp)
							{	print(j)
								cur.ix = sample(1:n,n,replace=TRUE)
                 						D = data[cur.ix,]
                 						REboot[[j]] = npr.wpc.est(D$event,D$censor,D$marker,cutoff,method,weights,wdth,nsub,sspeed,df)
                        				REcurve[[j]] = cbind (REboot[[j]]$x,REboot[[j]]$s)
								for(i in 1:nxwin){if(length(which(REcurve[[j]][,1]==x[i])>0)){REmatrix[i,j]=REcurve[[j]][min(which(REcurve[[j]][,1]==x[i])),2]}}		}
             		
						for(i in 1:nxwin)
							{	quant = quantile(na.omit(REmatrix[i,]),c(1-quantile,quantile))
                 						sw.lower[i] = max(0,quant[1])
                 						sw.upper[i] = min(1,quant[2])		}
				
						slb=na.omit(sw.lower)
						sub=na.omit(sw.upper)
						lb = predict(loess(slb ~ X,df = df)); lb[which(lb<=0)]=0; lb[which(lb>=1)]=1	
						ub = predict(loess(sub ~ X,df = df)); ub[which(ub<=0)]=0; ub[which(ub>=1)]=1		
						return(list(x=X,y=Y,s=s,lb=lb,ub=ub))	}
}

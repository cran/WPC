ww.windows <-
function(event=event,censor=censor,marker=marker,wdth,sspeed)
	{	 nwin = ceiling((max(marker)-min(marker))/sspeed)
   		  xwin = min(marker)+(0:nwin)*sspeed   ###### marker values to be estimated #
  		ntotal = length(xwin)  			  ###### number of windows
   		
		wdata = vector("list",ntotal); wband = nsam = winsize = NA
   		 data = data.frame(event=event, censor=censor, marker=marker)
   		
		for(i in 1:ntotal)
		   {	  wband[i] = min(xwin[i]-min(marker),max(marker)-xwin[i],wdth/2) 
    			wdata[[i]] = data.frame(data[which(marker <= xwin[i] + wband[i] & marker >= xwin[i] - wband[i]),])
     			   nsam[i] = dim(wdata[[i]])[1]
     			winsize[i] = 2*wband[i]		}	
	
	return(list(xwin = xwin, ntotal = ntotal, wdata = wdata, nsam = nsam, winsize = winsize))		}

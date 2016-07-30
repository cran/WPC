ns.windows <-
function(event=event,censor=censor,marker=marker,nsub,sspeed)
	{	data = data.frame(event,censor,marker)
		dataorder = data[order(data$marker),]
    		
		ntotal = length(marker)
      	xtotal = ceiling(ntotal/sspeed)
      	wdata = vector("list",xtotal); wband = nsam = winsize = xwin = NA
     		
		i = j = 1
     		repeat{
			wband[i] = min(j-1,nsub,ntotal-j)
       		wdata[[i]] = dataorder[c((j-wband[i]):(j+wband[i])),]
        		nsam[i] = dim(wdata[[i]])[1]  
        		xwin[i] = dataorder$marker[j]
        		winsize[i] = max(wdata[[i]]$marker)-min(wdata[[i]]$marker)
        		i <- i+1
        		j <- j+sspeed
        		if(j>ntotal) 
			break()	}

   	return(list(xwin = xwin, ntotal = xtotal, wdata = wdata, nsam = nsam, winsize = winsize))	}

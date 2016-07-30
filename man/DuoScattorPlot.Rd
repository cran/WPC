\name{DuoScattorPlot}
\alias{DuoScattorPlot}

\title{Generate Scatter Plots for Time-to-Event and Biomarkers for Two Groups}

\description{This function will generate the scatter plot of time-to-event and biomarker for two dataset. 
It helps to visualize the relationship between survival endpoints and biomarkers. It can also help to compare the two datasets}

\usage{
DuoScattorPlot(data1, data2, cutoff, xlab, ylab, main, ylim, xlim, col1, col2, col3, lwd, 
pch1, pch2, legendloc, legendtxt, ncol)
}
\arguments{\item{data1}{Data object 1 with three variables included: \emph{event}: the survival time, a positive numerical vector with no missing values; 
		\emph{censor}: the censor information, a vector with 1 indicating an event and 0 indicating right censored;
		\emph{marker}: the biomarker information, or other interesting variables.}
	\item{data2}{Data object 2 with the same structure as data object 1.}

	\item{cutoff}{This is to define the interesting data cutoff time point to see the relationship between time-to-events and markers.} 
  
  	\item{xlab}{It is the title for x axis; default is "Marker".}
  
	\item{ylab}{It is the title for y axis; default is "Time to Event".}
  
	\item{main}{It is the title for the plot; default is "Scattor Plot".}
	
	\item{ylim}{It creates the continuous scale of y axis of the plot; default is "c(0,3600)".}

	\item{xlim}{It creates the continuous scale of y axis of the plot; default is "c(0,100)".}
	
	\item{col1}{It defines the color of the dot in the dataset 1; default is "red".}
	\item{col2}{It defines the color of the dot in the dataset 2; default is "black".}
	\item{col3}{It defines the color of the cutoff line; default is "tomato".}
	
	\item{lwd}{It defines the width of the cutoff line; default is "2".}

  	\item{pch1}{It defines the type of the dot for event; default is "20".}

	\item{pch2}{It defines the type of the dot for censor; default is "21".}

  	\item{legendloc}{It specifies the location of the legend; default is "bottomright".}

	\item{legendtxt}{It provides the text of the legend; default is "c("death-group1","censor-group1","death-group2","censor-group2")".}
	\item{ncol}{It specifies the number of columns displayed in legend; default=1}}

\references{Yang H., Tang R., Hale M. and Huang J. (2016) A visualization method measuring the performance of biomarkers for guiding treatment decisions \emph{Pharmaceutical Statistics}, 15(\bold{2}), 1539-1612
}

\author{
Hui Yang \email{huiy@amgen.com}, Rui Tang \email{rui_tang@vrtx.com} and Jing Huang \email{jinghuang0@gmail.com}
}

\seealso{
\code{\link{SoloScattorPlot}}, \code{\link{TrioScattorPlot}}
}

\examples{
	## Create two data objects for the function:
	
        \dontshow{data(wpcdata.data)}
	tmppb = wpcdata[wpcdata$ATRT=="Placebo",]
	tmptrt = wpcdata[wpcdata$ATRT=="Treatment",]
	o.data1 =data.frame(event=tmppb$OSday, censor=tmppb$OScensor, marker=tmppb$Biomarker1)
	o.data2 =data.frame(event=tmptrt$OSday, censor=tmptrt$OScensor, marker=tmptrt$Biomarker1)
	
        ## Draw the scattor plot for the three data objects:

	DuoScattorPlot(o.data1,o.data2,180,xlab=c("Marker"),ylab=c("Survival Rate"),
		main=c("Weighted Predictiveness Curve"),ylim=c(0,600),xlim=c(0,100),
		col1="red",col2="black",lwd=2,pch1=20,pch2=21,legendloc="bottomright",ncol=1)
}

\keyword{Data Scattor Plot}

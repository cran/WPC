\name{npr.wpc.est}
\alias{npr.wpc.est}


\title{Generate Weighted Predictiveness Curve Estimates Using Non-Parametric Approach. }

\description{
This function generates weighted predictiveness curve estimates and/or confident bands using non-parametric approach.
}

\usage{
npr.wpc.est(event, censor, marker, cutoff, method, weights, wdth, nsub, sspeed, df, 
confi, nbtsp, quantile)
}

\arguments{
  \item{event}{This is the survival time. It is a positive numerical vector with no missing values.}
  \item{censor}{This specifies censor information. It is a vector, with 1 indicating an event and 0 indicating right censored. No missing values are allowed.}
  \item{marker}{This is the biomarker information (or other interesting variables). It is numerical with no missing values.}
  \item{cutoff}{This is to define the interesting data cutoff time point. The weighted predictiveness curve will be plotted based on this time point.} 
  \item{method}{This is to specify the method used to define the series of overlapping windows. Two options are provided: method=window.width when the approach of fixing the biomarker scale window width is used.
				method=number.subjt when the approach of fixing the number of subjects within each window is used.}
  \item{weights}{This is to specify the weight function, which will be applied to the Kaplan Meier approach for the survival rate estimates within each window. There are four options provided for this weight function:
  "uniform", "normal", "trunnormal", and "huber".}
  \item{wdth}{"This is to specify window width,  which is defined based on the biomarker scale. The smaller the window width is, the more the overlapping windows are specified.
				This parameter needs to be specified when we are using the fixed window width approach.}
  \item{nsub}{This is to specify the fixed number of patients within each window. The smaller the number of patients within each window, the more the overlapping windows are specified. 
				This parameter need to be specified when we are using the fixed number of subject within each window arrpoach.}
  \item{sspeed}{This is to specify the window sliding step. The window is gradually moving from small values on the left to the large values on the right. 
  This variable specifies the window sliding step being removed from the left and added on the right, in order to keep the same window width for each window.}
  \item{df}{It defines the degree of polynomials used for loess function when the local regression method is implemented. Normally, we take the value of 1 or 2. Here df=2 as default.}
  \item{confi}{This provides the option of reporting the confident band. If confi="NO", the confident band will not be generated. If confi="YES", the confident band will be generated. 
				Since we are using the bootstrap resampling method, it can be time-consuming to generate the confident band. Default is "NO".}
  \item{nbtsp}{This specifies the number of resampling for generating confident band. This number needs to be specified if the confi=YES. Default is 1000.}
  \item{quantile}{This specifies the quantile of the confident band. Default is 0.95, 95\% Confident band will be generated.}}


\details{
Given the series of overlapping sliding windows, for a fixed survival time \emph{cutoff}, the survival probability within each window is estimated using Kaplan-Meier method and assigned to the median biomarker value
within that window. For a given biomarker value, the window works to borrow information from its neighborhood to enhance the estimation of survival rate. Three weight options are incorporated: normal, Huber, and uniform (i.e., no weight)
to give the user the maximum flexibility. 

Repeating the process and assigning the survival rate estimate for each biomarker value, we can obtain the pair of data, in term of biomarker value and survival rate estimates, for each window. From those series of paired data,
we can draw the survival rate estimation curve of the biomarker value. To avoid over-fitting, we implement a local regression (loess) method to smooth across all window-specific median estimates to generate a relatively smooth predictiveness curve.

To have a measure of the precision of the predictiveness curve, we also provide the option of drawing the confident intervals in addition to the point estimates. Since it is very challenging to derive a close-form formula in this non-parametric setting, 
we use a non-parametric bootstrap technique to construct the confident bands.}

\value{
A list with components:
\item{x}{a vector of biomarker values for each overlapping window.}
\item{s}{A vector of survival rate estimates for each overlapping window.}
\item{lb}{A vector of lower band of survival rate estimates for each overlapping window.}
\item{ub}{A vector of upper band of survival rate estimates for each overlapping window.}
}

\references{ Yang H., Tang R., Hale M. and Huang J. (2016) A visualization method measuring the performance of biomarkers for guiding treatment decisions \emph{Pharmaceutical Statistics}, 15(\bold{2}), 1539-1612
}

\author{
Hui Yang \email{huiy@amgen.com}, Rui Tang \email{rui_tang@vrtx.com} and Jing Huang \email{jinghuang0@gmail.com}
}

\seealso{
\code{\link{cox.wpc.est}}
}


\examples{

	## install packages "survival" and "msm"
	
	library("survival")
	library("msm")



	## Fixed 10 subjects within each window and window sliding step is 1, 
	## normal weight function is used:
	
	npr.object1 = npr.wpc.est(event=wpcdata$OSday, censor=wpcdata$OScensor, 
	marker=wpcdata$Biomarker1,cutoff=180,method="number.subjt",weights="normal",
	nsub=10,sspeed=1,df=2,confi="NO")
	print(npr.object1)
	
	## Fixed biomarker scale window width 10 and window sliding step is 1, 
	## huber weight function is used:	
	
	\dontrun{npr.object2 = npr.wpc.est(event=wpcdata$OSday, censor=wpcdata$OScensor, 
	marker=wpcdata$Biomarker1,cutoff=180,method="window.width",weights="huber",
	wdth=10,sspeed=1, df=2, confi="YES", nbtsp=100)
	print(npr.object2)}}

\keyword{WPC Estimates}


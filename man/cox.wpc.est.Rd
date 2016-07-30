\name{cox.wpc.est}
\alias{cox.wpc.est}

\title{Generate Weighted Predictiveness Curve Estimates Using Parametric Approach.}

\description{
This function generates weighted predictiveness curve estimates and/or confident bands using parametric approach.
}

\usage{
cox.wpc.est(event, censor, marker, cutoff, quantile)
}

\arguments{
  \item{event}{This is the survival time. It is a positive numerical vector with no missing values.}
  \item{censor}{This specifies censor information. It is a vector, with 1 indicating an event and 0 indicating right censored. No missing values are allowed.}
  \item{marker}{This is the biomarker information (or other interesting variables). It is numerical with no missing values.}
  \item{cutoff}{This is to define the time cutoff.} 
  \item{quantile}{This specifies the quantile of the confident band. Default is 0.95, 95\% Confident band will be generated.}}


\details{
The Cox proportional hazard model with a single biomarker will be used to derive and draw the predictiveness curve for parametric WPC. The relationship could be written in the form of the survival function as follows:
\eqn{S(t)=[S_0(t)]^{exp{x\beta}}}, where S(t) is survival function, \eqn{S_0(t)} is baseline survivor function, and x is the biomarker of interest. The effect of the biomarker is expressed by the \eqn{exp(x\beta)}
term and quantified as a shift from the baseline survival \eqn{S_0(t)}. Because \eqn{S_0(t)} is always between 0 and 1, a positive coefficient \eqn{\beta} will decrease the survival function with increasing biomarker values; 
a negative coefficient \eqn{\beta} will increase the survival function with decreasing biomarker values. For any given time t, the baseline survival function \eqn{S_0(t)} could be estimated. Therefore, with a fixed coefficient 
estimate and fixed time, we could do such prediction for a range of x values by fitting x values into the formula earlier and then connect the predictions derived from the smallest x value to the largest x value. 
That will form the predictiveness curve for that particular time point.}

\value{
A list with components:
\item{x}{a vector of biomarker values.}
\item{s}{A vector of survival rate estimates for each biomarker value.}
\item{lb}{A vector of confident lower bands}
\item{ub}{A vector of confident upper band}
}

\references{ Yang H., Tang R., Hale M. and Huang J. (2016) A visualization method measuring the performance of biomarkers for guiding treatment decisions \emph{Pharmaceutical Statistics}, 15(\bold{2}), 1539-1612
}

\author{
Hui Yang \email{huiy@amgen.com}, Rui Tang \email{rui_tang@vrtx.com} and Jing Huang \email{jinghuang0@gmail.com}
}

\seealso{
\code{\link{npr.wpc.est}}
}

\examples{

	## install packages "survival" and "msm"
	
	library("survival")
	library("msm")

	cox.object = cox.wpc.est(event=wpcdata$OSday, censor=wpcdata$OScensor, 
	marker=wpcdata$Biomarker1,cutoff=180,quantile=0.95)
	
	print(cox.object)}

\keyword{WPC Estimates}

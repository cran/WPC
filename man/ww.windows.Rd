\name{ww.windows}
\alias{ww.windows}

\title{Create a Series of Overlapping Windows by Fixing Biomarker Scale Window Width}

\description{This function creates a series of overlapping windows by fixing the biomarker scale window width}

\usage{ww.windows(event, censor, marker, wdth, sspeed)}

\arguments{
  \item{event}{This is the survival time. It is a positive numerical vector with no missing values.}
  \item{censor}{This specifies censor information. It is a vector, with 1 indicating an event and 0 indicating right censored. No missing values are allowed.}
  \item{marker}{This is the biomarker information (or other interesting variables). It is numerical with no missing values.}
  \item{wdth}{This is to specify window width of each overlapping window. The window width is defined based on the biomarker scale. The smaller the window width is, the more the overlapping windows are specified.}
  \item{sspeed}{This is to specify the window sliding step. Since the window is gradually moving from small values on the left to the large values on the right. 
  This variable specifies the window sliding step being removed from the left and added on the right, in order to keep the same window width for each window.}
}

\details{
	It begins by ordering all the subjects based on their biomarker values from low to high. Let \eqn{x_1, x_2, \dots, x_n} be the ordered unique values of X observed in the data. 
	Then a series of overlapping windows can be defined using two parameters: \eqn{\gamma} - the biomarker-scale window width and \eqn{\nu} - the window sliding step. 
	Because the window is gradually moving from small values on the left to large values on the right, in order to keep the same window width for each window. The first window starts 
	from the first subject with the smallest biomarker value \eqn{x_1}, including subjects whose biomarker values are in the biomarker-scale window of \eqn{[x_1, x_1+\gamma]}. The second
	window will move forward by \eqn{\nu} biomarker-scale width, and include subjects whose biomarker values dropped in the second window \eqn{[x_1+\nu, x_1+\nu+\gamma]}. This process
	continues until all subjects have been included in at least one window. Subjects can be included in several windows.	
}

\value{
A list with components:
\item{xwin}{A series of marker values which will be assigned to the estimated survival rates within each window.}
\item{ntotal}{The total number of overlapping windows defined.}
\item{wdata}{A list of overlapping windows and each list representing a window with data frame of event, censor and marker}
\item{nsam}{A vector, with each number representing the number of patients within each overlapping window.}
\item{winsize}{A vector, with each number representing the half width for each overlapping window. Each window width is two times of it.}
}

\references{ Yang H., Tang R., Hale M. and Huang J. (2016) A visualization method measuring the performance of biomarkers for guiding treatment decisions \emph{Pharmaceutical Statistics}, 15(\bold{2}), 1539-1612
}

\author{
Hui Yang \email{huiy@amgen.com}, Rui Tang \email{rui_tang@vrtx.com} and Jing Huang \email{jinghuang0@gmail.com}
}
\note{
R packages \bold{survival} and \bold{msm} need to be installed before running the function.
}
\seealso{
\code{\link{ns.windows}}
}
\examples{
	
	## Window width is specified as 10 and window sliding step is 1:

	
	object = ww.windows(event=wpcdata$OSday, censor=wpcdata$OScensor, 
	marker=wpcdata$Biomarker1, wdth=10, sspeed=1)

	print(object)}

\keyword{WPC Estimates}
\keyword{Local Assessment}
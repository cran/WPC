\name{surv.rate}
\alias{surv.rate}

\title{
Calculate Survival Rate at a Fix Time Point
}
\description{
This function is implemented in the npr.wpc.est function.
}
\usage{
surv.rate(data, cutoff, wts, xwin)
}

\arguments{
  \item{data}{Data with event - the survival time, a positive numerical vector with no missing values; censor- censor information, a vector with 1 indicating an event and 0 indicating right censored}
  \item{cutoff}{This is to define the interesting data cutoff time point to see the relationship between time-to-events and markers.}
  \item{wts}{This is to specify the weight function, which will be applied to the Kaplan Meier approach for the survival rate estimates within each window. There are four options provided for this weight function: "uniform", "normal", "trunnormal",
and "huber". }
  \item{xwin}{A series of marker values which will be assigned to the estimated survival rates within each window.}
}

\references{
Yang H., Tang R., Hale M. and Huang J. (2016) A visualization method measuring the performance of biomarkers for guiding treatment decisions \emph{Pharmaceutical Statistics}, 15(\bold{2}), 1539-1612
}
\author{
Hui Yang \email{huiy@amgen.com}, Rui Tang \email{rui_tang@vrtx.com} and Jing Huang \email{jinghuang0@gmail.com}
}

\seealso{
\code{\link{npr.wpc.est}}
}


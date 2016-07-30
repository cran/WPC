\name{wpcdata}
\alias{wpcdata}
\docType{data}
\title{A Data Example to Illustrate WPC Approach.}

\description{
This survival data example is to illustrate WPC approach. 90 patients are randomized into three different arms, 1 placebo and 2 treatments (high dose and low dose).
For each patient, four biomarkers are measured at baseline. Overall survival and progression free survival information are collected.}

\usage{data("wpcdata")}
\format{
  A data frame with 90 observations on the following 13 variables.
  \describe{
    \item{\code{SUBJID}}{a numeric vector, indicating subject id information.}
    \item{\code{TRTA}}{a factor, indicating three different arms, with levels \code{HighDose} \code{LowDose} \code{Placebo}}
    \item{\code{ATRT}}{a factor, indicating whether patients receive placebo or treatment, with levels \code{Placebo} \code{Treatment}}
    \item{\code{Biomarker1}}{a numeric vector, first biomarker with the value between 0 and 100}
    \item{\code{Biomarker2}}{a numeric vector, second biomarker with the value between 0 and 200}
    \item{\code{Biomarker3}}{a numeric vector, third biomarker with the value between 0 and 100}
    \item{\code{Biomarker4}}{a numeric vector, fourth biomarker with the value between 0 and 200}
    \item{\code{OSday}}{a numeric vector, overall survival in days}
    \item{\code{OSmonth}}{a numeric vector, overall survival in months}
    \item{\code{OScensor}}{a numeric vector, censor information for overall survival, 0 = alive and 1 = dead}
    \item{\code{PFSday}}{a numeric vector, progression free survival in days}
    \item{\code{PFSmonth}}{a numeric vector, progression free survival in months}
    \item{\code{PFScensor}}{a numeric vector, censor information for progression free survival, 0 = censor and 1 = event}
  } 
}


\seealso{
\code{\link{SoloScattorPlot}}, \code{\link{DuoScattorPlot}}, \code{\link{TrioScattorPlot}}
}

\keyword{datasets}

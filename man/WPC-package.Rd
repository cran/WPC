\name{WPC-package}
\alias{WPC-package}
\alias{WPC}
\docType{package}
\title{
Implement Weighted Predictiveness Curve to Visualize the Marker-by-Treatment Relationship and Measure the Performance of Biomarkers for Guiding 
Treatment Decision. }

\description{
The package includes ten functions and one example for illustration purpose. 

Based on the nature of the data, it generates predictiveness curve by utilizing either parametric or nonparametric approaches.
When the unique effect of biomarker value change is indeed multiplicative with respect to hazard rate, parametric WPC (based on COX proportional hazard model) can most efficiently estimate 
survival rate for each biomarker value. The function \code{cox.wpc.est} will generate the parametric WPC using Cox model, and returns point estimates, confidence intervals 
and their biomarker values.

For most real world cases, little is known about the data structure, and that's when nonparametric WPC should be considered. The estimates are based on a series of overlapping windows (subpopulation).
There are two ways to generate the series of overlapping windows: by fixing the number of patients within each window and by fixing the biomarker scale window width. The function \code{ns.windows} uses
 the first approach and report the detailed information of each window, while the function \code{ww.windows} utilizes the second approach. The function \code{npr.wpc.est} incorporate those two functions and their associated parameters to generate nonparametric WPC.
 Similar to \code{cox.wpc.est}, the function \code{npr.wpc.est} returns point estimates, confidence intervals 
and their each biomarker values.

The primary functions in the package are \code{SoloWPCCurve}, \code{DuoWPCCurve} and \code{TrioWPCCurve}. They generates the graphs of single, double and triple weighted predictiveness curves based on the point estimates
 and confidence intervals reported by \code{cox.wpc.est} and \code{npr.wpc.est}.

The packages can be used to compare biomarkers and identify the one with the highest impact. Equally important, by simultaneously depicting several treatment-specific WPC curves, it is easy to detect treatment heterogeneity 
as well as treatment-specific patterns, which in turn will help us with subgroup selection and biomarker cut-off Optimization.
}
\details{
\packageIndices{WPC}
}

\author{
\packageAuthor{WPC}
}

\references{

Yang H., Tang R., Hale M. and Huang J. (2016) A visualization method measuring the performance of biomarkers for guiding treatment decisions \emph{Pharmaceutical Statistics}, 15(\bold{2}), 1539-1612 

Therneau T (2013). A Package for Survival Analysis in S. R package version 2.37-4, \url{http://CRAN.R-project.org/package=survival}.

Christopher H. Jackson (2011). Multi-State Models for Panel Data: The msm Package for R. \emph{Journal of Statistical Software}, 38(\bold{8}), 1-29. \url{http://www.jstatsoft.org/v38/i08/}.
}

\keyword{ package }
\seealso{Package Survival
}

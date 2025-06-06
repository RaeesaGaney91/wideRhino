% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CVAbiplot.R
\name{CVAgsvd}
\alias{CVAgsvd}
\title{CVA Biplot using the GSVD}
\usage{
CVAgsvd(X, group)
}
\arguments{
\item{X}{n x p data matrix}

\item{group}{vector of size n showing the groups}
}
\value{
An object with components of a CVA biplot
}
\description{
Create a CVA biplot using the generalised singular value decomposition when
number of variables (p) is larger than the number of samples (n).
}
\details{
If p < n, then the solution defaults to the standard CVA biplot.
}
\examples{
CVAgsvd(X=iris[,1:4],group = iris[,5]) |>
CVAbiplot(group.col = c("orange","red","pink"))
}

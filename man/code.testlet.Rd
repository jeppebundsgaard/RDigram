% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/digram.object.R
\name{code.testlet}
\alias{code.testlet}
\title{Code items as a testlet/local dependant}
\usage{
code.testlet(do, testlet = NULL, names = NULL, labels = NULL, append = F)
}
\arguments{
\item{do}{A digram.object}

\item{testlet}{String. The items that are part of a testlet/are local dependant. Give as a list of comma separated variable numbers, variable labels or variable names. If there is spaces in the variable names, they can be delimited by ".}

\item{names}{A vector of strings naming the testlets. If names are not given, they are composed of the testlet item names.}

\item{append}{Logical. Append new testlet variables to the existing ones.}
}
\value{
Returns a digram.object with the revised testlet-data.frame.
}
\description{
Code items as a testlet/local dependant
}
\details{
Local dependence is often caused by items sharing a common stimulus. This is called testlets or item bundles (Wang & Wilson 2006. Coding for Local Dependence is the same as identifying a testlet or an item bundle.
}
\examples{
data(DHP)
do<-code.testlet(do=DHP,testlet=c("ab,dhp36 dhp37,5 6"))
}
\references{
Wang, W.-C., & Wilson, M. (2005). The Rasch Testlet Model. \emph{Applied Psychological Measurement}, 29(2), 126–149. https://doi.org/10.1177/0146621604271053
}

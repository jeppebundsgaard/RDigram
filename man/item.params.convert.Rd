% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/item.params.convert.R
\name{item.params.convert}
\alias{item.params.convert}
\title{Convert item parameters from one parametrization to another}
\usage{
item.params.convert(
  from.model = NULL,
  item.params = c(),
  from = c("pcm", "pcm.cent", "andersen", "psd", "conquest"),
  to = c("pcm", "pcm.cent", "andersen", "psd", "conquest"),
  return.vector = F
)
}
\arguments{
\item{from.model}{A model of class TAM or eRm.}

\item{item.params}{A matrix of item parameters (items in rows, thresholds in columns) (not needed if from.model is given)}

\item{from, to}{Type of item parameters. One of pcm, pcm.cent, andersen, psd, conquest (from not needed if from.model is given).}

\item{return.vector}{Get result as a vector of category values instead of the default data.frame}
}
\value{
Returns item parameters in the parametrization specified in \emph{to}.
}
\description{
Convert item parameters from one parametrization to another
}
\details{
The Rasch model can be parameterized in multiple ways. This function translates parameters from one to the other. The following models are supported:
\itemize{
\item{Power series distribution parametrization (psd)}{\cr
\eqn{\xi^x\gamma_x \over G(\xi,\gamma_1 ... \gamma_k)}, where \eqn{\xi} is the person parameter (ability), and \eqn{\gamma_x} are the item parameters. Related through \eqn{\xi=exp(\theta)} and \eqn{\gamma_x=exp(\delta_x)} to the Andersen parametrization
}
\item{Andersen parametrization (andersen)} {\cr
\eqn{exp(x\theta+\delta_x) \over G(\theta,\delta_1 ... \delta_k)}, where \eqn{\theta} is the person parameter (ability), and \eqn{\delta_x} are the item (easiness) parameters
}
\item{Partial Credit Model (PCM)/Masters' parametrization (pcm)} {\cr
\eqn{exp(x\theta-\sum\limits_{i=1}^x\tau_i) \over G(\theta,\tau_1 ... \tau_k)}, where \eqn{\theta} is the person parameter (ability), and \eqn{\tau_x} are the item step parameters
}
\item{Partial Credit Model (PCM)/Masters' parametrization with centralized item step parameters (pcm.cent)} {\cr
\eqn{exp(x(\theta-\beta)-\sum\limits_{i=1}^x\beta_i) \over G(\theta,\beta_1 ... \beta_k)}, where \eqn{\theta} is the person parameter (ability), \eqn{\beta={1 \over k}\sum\limits_{i=1}^k \tau_i} is the average of the \eqn{\tau} parameters from the PCM parametrization, and \eqn{\beta_x=\tau_x-{1 \over k}\sum\limits_{i=1}^k \tau_i} are the centralized item step parameters
}
\item{Conquest parametrization (conquest)} {\cr
\eqn{exp(x(\theta-\psi-\sum\limits_{i=1}^x \psi_x) \over G(\theta,\psi_1 ... \psi_k)}, where \eqn{\sum\limits_{i=1}^x \psi_i \equiv 0}. And where \eqn{\theta} is the person parameter (ability), \eqn{\psi={1 \over k}\sum\limits_{i=1}^k \tau_i} is the average of the \eqn{\tau} parameters from the PCM parametrization, and \eqn{\psi_x=\tau_x-{1 \over k}\sum\limits_{i=1}^k \tau_i} are the centralized item step parameters
}
}
\link{TAM} uses Conquest parametrization. \link{eRm} uses Andersen parametrization. RUMM 2030 uses Partial Credit parametrization with centralized item step parameters. DIGRAM uses Power Series Distribution parametrization.
}
\examples{
item.params<-matrix(c(0,1,2,3,1,2,3,4),nrow=4)
item.params.convert(item.params=item.params,from="conquest",to="psd")
}
\references{
Andersen, B. E. (1970). Asymptotic properties of conditional likelihood estimators. \emph{Journal of the Royal Statistical Society}, Series B, 32,283-301.

Brown, N. J. S. (2004). Interpreting Ordered Partition Model Parameters from ConQuest. \link[=https://bearcenter.berkeley.edu/sites/default/files/report - opm_parameters.pdf]{https://bearcenter.berkeley.edu/sites/default/files/report\%20-\%20opm_parameters.pdf}

Hatzinger, R., & Rusch, T. (2009). IRT models with relaxed assumptions in eRm: A manual-like instruction. \emph{Psychology Science Quarterly}, 51(1), 87–120.

Kreiner, S. (n.d.). Om beregning af item-parametre I TAM.

Kreiner, s. (n.d.). Parameterization of graphical loglinear Rasch models.

Bundsgaard, J. &  Kreiner, S. (2019). \emph{Undersøgelse af De Nationale Tests måleegenskaber}. 2nd Ed. Copenhagen: DPU, Aarhus University.

Masters, G. N. (1982). A Rasch model for partial credit scoring. \emph{Psychometrika}, 47(2), 149–174.
}
\author{
Jeppe Bundsgaard & Svend Kreiner
}

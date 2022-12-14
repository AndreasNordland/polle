% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/earl.R
\name{control_earl}
\alias{control_earl}
\title{Control arguments for Efficient Augmentation and Relaxation Learning}
\usage{
control_earl(
  moPropen,
  moMain,
  moCont,
  regime,
  iter = 0L,
  fSet = NULL,
  lambdas = 0.5,
  cvFolds = 0L,
  surrogate = "hinge",
  kernel = "linear",
  kparam = NULL,
  verbose = 0L
)
}
\arguments{
\item{moPropen}{Propensity model of class "ModelObj", see \link[modelObj:modelObj]{modelObj::modelObj}.}

\item{moMain}{Main effects outcome model of class "ModelObj".}

\item{moCont}{Contrast outcome model of class "ModelObj".}

\item{regime}{An object of class \link{formula} specifying the design of the policy/regime.}

\item{iter}{Maximum number of iterations for outcome regression.}

\item{fSet}{A function or NULL defining subset structure.}

\item{lambdas}{Numeric or numeric vector. Penalty parameter.}

\item{cvFolds}{Integer. Number of folds for cross-validation of the parameters.}

\item{surrogate}{The surrogate 0-1 loss function. The options are
\code{"logit"}, \code{"exp"}, \code{"hinge"}, \code{"sqhinge"}, \code{"huber"}.}

\item{kernel}{The options are \code{"linear"}, \code{"poly"}, \code{"radial"}.}

\item{kparam}{Numeric. Kernel parameter}

\item{verbose}{Integer.}
}
\value{
list of (default) control arguments.
}
\description{
\code{control_earl} sets the default control arguments
for efficient augmentation and relaxation learning , \code{type = "earl"}.
The arguments are passed directly to \code{\link[DynTxRegime:earl]{DynTxRegime::earl()}} if not
specified otherwise.
}

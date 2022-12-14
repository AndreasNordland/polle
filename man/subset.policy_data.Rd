% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/policy_data_functions.R
\name{subset.policy_data}
\alias{subset.policy_data}
\title{Subset Policy Data on ID}
\usage{
\method{subset}{policy_data}(x, id, ...)
}
\arguments{
\item{x}{Object of class \link{policy_data}.}

\item{id}{character vectors of IDs.}

\item{...}{Additional parameters passed to lower level functions.}
}
\value{
Object of class \link{policy_data}.
}
\description{
\code{subset} returns a policy data object containing the given IDs.
}
\examples{
library("polle")
### Single stage:
source(system.file("sim", "single_stage.R", package="polle"))
d <- sim_single_stage(5e2, seed=1)
# constructing policy_data object:
pd <- policy_data(d, action="A", covariates=list("Z", "B", "L"), utility="U")
pd

# getting the observation IDs:
get_id(pd)[1:10]

# subsetting on IDs:
pdsub <- subset(pd, id = 250:500)
pdsub
get_id(pdsub)[1:10]
}

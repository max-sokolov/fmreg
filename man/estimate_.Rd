% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimate_.R
\name{estimate_}
\alias{estimate_}
\title{Estimate a model on the first stage of the Fama-MacBeth regression.}
\usage{
estimate_(model, .data, .date)
}
\arguments{
\item{model}{A list specifying the model.
E.g., x and y fields should contain rhs and lhs of the model.}

\item{.data}{A data frame: the data on which the model is estimated.
These data is assumed to be a "slice" of the full data for
date \code{.date}.}

\item{.date}{Date corresponding to the "slice" of the data.}
}
\value{
A list with the fit of the model.
        The fit does not include the data itself
        because the function is intended to work with relatively big data.
        \code{aggregate_} should be able to work with this list on
        the second stage of the Fama-MacBeth regression.
}
\description{
\code{estimate_} estimates a specified model for a "slice" (cross-section)
of the data.
}
\seealso{
\code{\link[fmreg]{aggregate_}}
}
\keyword{internal}


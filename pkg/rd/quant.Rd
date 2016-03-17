\name{quant}
\alias{quant}
\alias{print.quant}
\title{Quantiles for a Continuous variable}
\encoding{latin1}

\description{
  The function provides quantiles for a continuous variable,
  possibly according to levels of one or several categorical variables.
  }

\usage{
  quant(formula, data, probs = NULL, digits = 3)
  \method{print}{quant}(x, \ldots)
  }

\arguments{
  \item{formula}{
  A formula with the numerical variable on the left-hand side and the categorical variable(s) on
  the right-hand side. 
  When several categorical variables are included, they must be separated
  by a \code{+}, i.e., interactions (denoted by symbols \code{:} or \code{*}) are not allowed.
  }
  \item{data}{A data frame containing the variables indicated in the formula.}
  \item{probs}{A numeric vector of probabilities with values in [0,1].} 
  \item{digits}{A scalar indicating the number of decimal digits left when rounding the result of the statictics.}
  \item{x}{An object of class \dQuote{quant}.}
  \item{...}{Further arguments to be passed to the functions.}
  }

\value{
  A list with components \code{CALL}, \code{tab}, \code{nbfact} and \code{digits}.
  Component \code{tab} is the data frame built according to the formula and function used in the call.
  }

\seealso{ \code{\link{summary}},  \code{\link{quantile}}, \code{\link{aggregate}} }

\examples{

tmp <- data.frame(
    f1 = c(NA, rep("a", 2), rep("b", 5), NA, "a", "a"),
    f2 = rep(c("c", "d"), times = c(5, 6)),
    f3 = rep(c("e", NA, "f"), times = c(4, 1, 6)),
    y1 = c(rnorm(n = 9, mean = 10, sd = 5), NA, 2.1)
    )
tmp

quant(formula = y1 ~ f1, data = tmp)
quant(formula = y1 ~ f1, data = tmp, digits = 1)
quant(formula = y1 ~ f1,  data = tmp, probs = c(0.02, 0.50, 0.98))
tab <- quant(formula = y1 ~ f1, data = tmp)
tab$tab

quant(formula = y1 ~ 1, data = tmp)

quant(formula = y1 ~ f1 + f2, data = tmp)

  }

\keyword{manip}

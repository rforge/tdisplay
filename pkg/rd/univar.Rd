\name{univar}
\alias{univar}
\alias{print.univar}
\title{Descriptive Statistics for a Continuous variable}
\encoding{latin1}

\description{
  The function provides descriptive statistics for a continuous variable,
  possibly according to levels of one or several categorical variables.
  }

\usage{
  univar(formula, data, digits = 3)
  \method{print}{univar}(x, \ldots)
  }

\arguments{
  \item{formula}{A formula. See examples for syntax.}
  \item{data}{A data frame containing the variables indicated in the formula.}
  \item{digits}{A scalar indicating the number of decimal digits left when rounding the result of the statictics.}
  \item{x}{An object of class \dQuote{univar}.}
  \item{...}{Further arguments to be passed to the functions.}
  }

\value{
  A list with components \code{CALL}, \code{tab}, \code{nbfact} and \code{digits}.
  Component \code{tab} is the data frame built according to the formula and function used in the call.
  }

\seealso{ \code{\link{summary}}, \code{\link{aggregate}} }

\examples{

tmp <- data.frame(
    f1 = c(NA, rep("a", 2), rep("b", 5), NA, "a", "a"),
    f2 = rep(c("c", "d"), times = c(5, 6)),
    f3 = rep(c("e", NA, "f"), times = c(4, 1, 6)),
    y1 = c(rnorm(n = 9, mean = 10, sd = 5), NA, 2.1)
    )
tmp

univar(formula = y1 ~ f1, data = tmp)
univar(formula = y1 ~ f1, data = tmp, digits = 1)
tab <- univar(formula = y1 ~ f1, data = tmp)
tab$tab

univar(formula = y1 ~ 1, data = tmp)

univar(formula = y1 ~ f1 + f2, data = tmp)

  }

\keyword{manip}

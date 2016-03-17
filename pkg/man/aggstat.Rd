\name{aggstat}
\alias{aggstat}
\alias{print.aggstat}
\title{Complement to Aggregate}
\encoding{latin1}

\description{
  The function computes a statistic (sum, mean, standard deviation, etc.) on one or several continuous variables,
  according to levels of one or several categorical variables.
  }

\usage{
  aggstat(formula, data, FUN, digits = NULL, ...)
  \method{print}{aggstat}(x, \ldots)
  }

\arguments{
  \item{formula}{
  A formula with the numerical variable(s) on the left-hand side and the categorical variable(s) on
  the right-hand side. 
  When several numerical variables are included, they must be handled with \code{cbind}
  (e.g., \code{cbind(y1, y2, y3)}).
  When several categorical variables are included, they must be separated
  by a \code{+}, i.e., interactions (denoted by symbols \code{:} or \code{*}) are not allowed.
  When there is no variable in the left-hand side of the formula, the function computes the number of rows
  in the data frame according to the categorical variable(s) (no argument \code{FUN} is needed and
  the resulting variable is \code{n.aggstat}).
  }
  \item{data}{A data frame containing the variables indicated in the formula.}
  \item{FUN}{A function returning a scalar (e.g., \code{mean}, \code{sd} or \code{median}}
  \item{digits}{A scalar indicating the number of decimal digits left when rounding the result of the statictics. 
    Default to NULL (no rounding).}
  \item{\dots}{Further arguments passed to the function defined in FUN.}
  \item{x}{An object of class \dQuote{aggstat}.}
  }

\value{
  A list with components \code{CALL}, \code{tab}, \code{nbfact} and \code{digits}.
  Component \code{tab} is the data frame built according to the formula and function used in the call.
  }

\seealso{ \code{\link{aggregate}}, \code{\link{xtabs}}, \code{\link{table}} }

\examples{

tmp <- data.frame(
    y1 = c(NA, rnorm(n = 8, mean = 10, sd = 5), NA),
    y2 = c(rep(NA, 2), rnorm(n = 6, mean = 10, sd = 5), rep(NA, 2)),
    y3 = rnorm(n = 10, mean = 10, sd = 5),
    y4 = rnorm(n = 10, mean = 10, sd = 5),
    f1 = rep(c("a", NA, "b"), times = c(3, 1, 6)),
    f2 = rep(c("c", "d", NA), times = c(5, 3, 2)),
    f3 = rep(c("e", "f", "g"), times = c(3, 3, 4))
    )
tmp

aggstat(formula = y1 ~ f1, data = tmp, FUN = mean)
aggstat(formula = y1 ~ f1, data = tmp, FUN = mean, digits = 1)

aggstat(formula = y1 ~ f1 + f2, data = tmp, FUN = mean)

aggstat(formula = y1 ~ 1, data = tmp, FUN = mean)

aggstat(formula = cbind(y1, y2, y3) ~ f1 + f2, data = tmp, FUN = median)
aggstat(formula = cbind(y1, y2, y3) ~ f1 + f2, data = tmp, FUN = quantile,
        prob = 0.5, digit = 1)
tab <- aggstat(formula = cbind(y1, y2, y3) ~ f1 + f2, data = tmp, FUN = quantile,
               prob = 0.5)
tab$tab

# Nb. rows in data frame
aggstat(formula = ~ 1, data = tmp) 
aggstat(formula = ~ f1, data = tmp) 
aggstat(formula = ~ f2, data = tmp)
aggstat(formula = ~ f1 + f2, data = tmp)

  }

\keyword{manip}

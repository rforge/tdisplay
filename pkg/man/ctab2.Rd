\name{ctab2}
\alias{ctab2}
\alias{print.ctab2}
\title{ctab2 Cross Tabulation (data frame outputs)}
\encoding{latin1}

\description{
  The function builds contingency tables: counts and row proportions.
  \code{ctab2} is an alternative to \code{ctab}. The difference
  is that outputs of \code{ctab2} are data frames, while outputs of ctab are tables.
  Both \code{ctab2} and \code{ctab} have the same syntax.
  
  Three types of data are managed by \code{ctab2}:
  \itemize{
        \item Split individual data;
        \item Grouped data with weights;
        \item Grouped data of form \dQuote{cbind(success, failure)}.
        }
  For grouped data, \code{ctab2} uses \code{splitbin}.
  }

\usage{
  ctab2(formula, data, weights = NULL, digits = 2)
  }

\arguments{
  \item{formula}{A 2-sided formula describing the contincency table.
    The left-hand side represents the column-variable. See examples for syntax.}
  \item{data}{A data frame where \code{formula} is evaluated.}
  \item{weights}{Variable representing the weight for grouped data with weights.}
  \item{digits}{A scalar indicating the number of decimal digits left when rounding the result of the statictics.}

  }

\value{
  A list with components \code{CALL}, \code{formula}, \code{f}, \code{tab}, \code{tab.p} and \code{digits}.
  Component \code{tab} is the basic contingency table.
  }

\seealso{ \code{\link{table}}, \code{\link{xtabs}}, \code{\link{ftable}} }

\examples{

#=== Example of the 3 types of data managed by ctab2

    # Split data

tmp <- data.frame(
    treatment = c("B", "A", "C", "C", "B", "B", "C", "A", "B", "B", NA, "C"),
    surv = c("YES", "NO", "YES", "YES", "NO", "NO", "NO", "YES", "YES", "NO", "NO", NA)
    )
#tmp$treatment <- factor(tmp$treatment, levels = c("C", "A", "B"))
tmp

ctab2(formula = surv ~ 1, data = tmp)
ctab2(formula = surv ~ treatment, data = tmp, digits = 1)

    # Grouped data with weights

tmp <- data.frame(
    treatment = c("A", "B", "C", "A", "B", "C", NA, "C"),
    surv = c("NO", "NO", "NO", "YES", "YES", "YES", "NO", NA),
    n = c(1, 3, 1, 1, 2, 2, 1, 1)
    )
tmp

ctab2(formula = surv ~ 1, data = tmp, weights = n)
ctab2(formula = surv ~ treatment, data = tmp, weights = n)

    # Grouped data of the form "cbind(success, failure)"

tmp <- data.frame(
    treatment = c("A", "B", "C", NA),
    surv = c(1, 2, 2, 0),
    death = c(1, 3, 1, 1)
    )
tmp

ctab2(formula = cbind(surv, death) ~ 1, data = tmp)
ctab2(formula = cbind(surv, death) ~ treatment, data = tmp)

#=== Examples of tables

n <- 80
tmp <- data.frame(
    y1 = sample(1:2, n, replace = TRUE),
    y2 = sample(1:2, n, replace = TRUE),
    y3 = sample(1:3, n, replace = TRUE),
    y4 = sample(1:4, n, replace = TRUE)
    )
head(tmp)
ctab2(formula = y3 ~ 1, data = tmp)

res <- ctab2(formula = y3 ~ y1, data = tmp)
res$tab
res$tab.p

ctab2(formula = y3 ~ y1 + y2, data = tmp)
ctab2(formula = y3 ~ y1 + y2 + y4, data = tmp)

    }

\keyword{multivariate}

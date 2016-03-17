\name{ctab}
\alias{ctab}
\alias{print.ctab}
\title{ctab Cross Tabulation (table outputs)}
\encoding{latin1}

\description{
  The function builds contingency tables: counts and row proportions.
  When right-hand side formula contains more than 1 variable,
  \code{ctab} builds successive two-way tables, vertically and with same column-variable.

  Three types of data are managed by \code{ctab}:
  \itemize{
        \item Split individual data;
        \item Grouped data with weights;
        \item Grouped data of form \dQuote{cbind(success, failure)}.
        }
  For grouped data, \code{ctab} uses \code{splitbin}.
  }

\usage{
  ctab(formula, data, weights = NULL, digits = 2)
  \method{print}{ctab}(x, \ldots)
  }

\arguments{
  \item{formula}{A 2-sided formula describing the contincency table.
    The left-hand side represents the column-variable. See examples for syntax.}
  \item{data}{A data frame where \code{formula} is evaluated.}
  \item{weights}{Variable representing the weight for grouped data with weights.}
  \item{digits}{A scalar indicating the number of decimal digits left when rounding the result of the statictics.}
  \item{x}{An object of class \dQuote{ctab}.}
  \item{...}{Further arguments to be passed to the functions.}
  }

\value{
  A list with components \code{CALL}, \code{formula}, \code{f}, \code{tab}, \code{tab.counts}, \code{tab.p}, \code{counts}
   and \code{digits}.
  Component \code{tab} is the basic contingency table.
  Component \code{counts} is the corresponding data frame (grouped data with weights).
  }

\seealso{ \code{\link{table}}, \code{\link{xtabs}}, \code{\link{ftable}} }

\examples{

#=== Example of the 3 types of data managed by ctab

    # Split data

tmp <- data.frame(
    treatment = c("B", "A", "C", "C", "B", "B", "C", "A", "B", "B", NA, "C"),
    surv = c("YES", "NO", "YES", "YES", "NO", "NO", "NO", "YES", "YES", "NO", "NO", NA)
    )
#tmp$treatment <- factor(tmp$treatment, levels = c("C", "A", "B"))
tmp

ctab(formula = surv ~ 1, data = tmp)
ctab(formula = surv ~ treatment, data = tmp, digits = 1)

    # Grouped data with weights

tmp <- data.frame(
    treatment = c("A", "B", "C", "A", "B", "C", NA, "C"),
    surv = c("NO", "NO", "NO", "YES", "YES", "YES", "NO", NA),
    n = c(1, 3, 1, 1, 2, 2, 1, 1)
    )
tmp

ctab(formula = surv ~ 1, data = tmp, weights = n)
ctab(formula = surv ~ treatment, data = tmp, weights = n)

    # Grouped data of the form "cbind(success, failure)"

tmp <- data.frame(
    treatment = c("A", "B", "C", NA),
    surv = c(1, 2, 2, 0),
    death = c(1, 3, 1, 1)
    )
tmp

ctab(formula = cbind(surv, death) ~ 1, data = tmp)
ctab(formula = cbind(surv, death) ~ treatment, data = tmp)

#=== Examples of tables

n <- 80
tmp <- data.frame(
    y1 = sample(1:2, n, replace = TRUE),
    y2 = sample(1:2, n, replace = TRUE),
    y3 = sample(1:3, n, replace = TRUE),
    y4 = sample(1:4, n, replace = TRUE)
    )
head(tmp)
ctab(formula = y3 ~ 1, data = tmp)

res <- ctab(formula = y3 ~ y1, data = tmp)
res$tab
res$tab.p

ctab(formula = y3 ~ y1 + y2, data = tmp)
ctab(formula = y3 ~ y1 + y2 + y4, data = tmp)

    }

\keyword{multivariate}

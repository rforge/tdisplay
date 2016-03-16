\name{display}
\alias{display}
\title{Contents of Variables of a Data frame}
\encoding{latin1}

\description{
  The function displays the contents of each of the variables of the data frame.
  }

\usage{
  display(data)
  }

\arguments{
  \item{data}{A data frame containing the variables.}
  }

\value{
  \code{display} does not return anything. The output is printed to the terminal.
  }

\seealso{\code{\link{head}}, \code{\link{summary}}, \code{\link{str}} }

\examples{

tmp <- data.frame(
    y1 = c(NA, NA, rnorm(3)),
    y2 = c(rnorm(4), NA),
    f1 = c(1, NA, 1, NA, 2)
    )
tmp$f1 <- as.factor(tmp$f1)
tmp

display(data = tmp)

  }

\keyword{manip}


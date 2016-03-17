\name{vtest} 
\alias{vtest}
\alias{plot.vtest}
\encoding{latin1}

\title{Description of a Classification with Test Values}
\description{
  Facilitate the description (\cite{Morineau, 1984}) of the classes of a partition (e.g., after an automatic 
  classification). Test values are calculated for each continuous variable or category of a qualitative variable. 
  They are measurements of the distance between the within-class value and the overall value.}

\usage{
  vtest(formula = NULL, data)
  \method{plot}{vtest}(x, group = TRUE, conf = NULL,
  main = if(group) "test-values by groups" else "test values by variables",
  xlab = "test value", bg = "black", \ldots)
  }

\arguments{
  \item{formula}{A 2-sided formula with all numerical or categorical variable(s) on the left-hand side and a single 
    variable (factor) indicating group membership on the right-hand side. When several variables are included, they 
    must be handled with \code{cbind} (e.g., \code{cbind(y1, y2, y3)}).}
  \item{data}{A data frame containing all the variables in the left-hand side of formula.}
  \item{x}{An object of class \dQuote{vtest}.}
  \item{group}{A logical indicating if test values should be ordered by groups (\code{TRUE}) or by variables 
    (\code{FALSE}). Default to \code{TRUE}.}
  \item{conf}{Confidence level for plotting confidence region, \bold{in percents}. Default to no plotting (\code{NULL}).}
  \item{main}{Title of the plot. Default set up according to \code{group}.}
  \item{xlab}{Label for x axis. Default set up according to \code{group}.}
  \item{bg}{Background color for the points. Default to \dQuote{black}.}
  \item{\ldots}{Other graphical parameters to be passed to the plotting function.}
  }

\details{
  For a continuous variable \eqn{X}, test values compare \eqn{\overline{X}_k}{mean(Xk)}, the mean of \eqn{X} in the 
  group \eqn{k} with the overall mean \eqn{\overline{X}}{mean(X)} accounting for the within-group variance 
  \eqn{s_k^2(X)}{Sk(X)^2}. 
  The test value for a variable \eqn{X} and a group \eqn{k} is:
  \deqn{t_k(X) = \frac{\overline{X}_k - \overline{X}}{s_k(X)}}{Tk(X) = (mean(Xk) - mean(X))/Sk(X)}
  with \eqn{s_k^2(X) = \frac{n - n_k}{n-1} \frac{s^2(X)}{n_k}}{Sk(X)^2 = [(n - Nk)/(n - 1)] * (S(X)^2/Nk)}.
  \eqn{n} is the total number of observations and \eqn{n_k}{Nk} is the number of observations in the group \eqn{k}.

  Under the null hypothesis that \eqn{\overline{X}_k}{mean(Xk)} has expectation equal to \eqn{\overline{X}}{mean(X)}
  the test value \eqn{t_k(X)}{Tk(X)} is asymptotically distributed like \eqn{N(0, 1)}. The table \code{pval} contains the 
  P-values of the tests.

  For a qualitative variable, test values compare the proportion of the population with the category \eqn{j} in a 
  group \eqn{k} with the proportion of the population with the category \eqn{j} in the whole population (\eqn{n}). 
  A normal approximation is used for the hypergeometric distribution of these counts.

  The number \eqn{N} of observations with the category \eqn{j} in the group \eqn{k} is estimated by \eqn{n_{kj}}{Nkj}.

  The test-value is:
  \deqn{t_k(N) = \frac{N~-~E(N)}{s_k(N)}}{Tk(N) = (N - E(N)) * Sk(N)}
  with the expectation value of N:
  \deqn{E(N) = n_k \frac{n_j}{n}}{E(N) = Nk * Nj / n}
  and the variance of N :
  \deqn{s_k^2(N) = n_k \frac{n - n_k}{n - 1} \frac{n_j}{n} \biggl ( 1 - \frac{n_j}{n}\biggr )}{%
    Sk(N)^2 = Nk * (n - Nk)/(n - 1) * Nj / n * (1 - Nj / n)}

  P-values for the test values only make sense if the set of variables in the left-hand side of the formula were not used 
  to build the partition. When this situation is met, test values may only be used as similarity indices between variables 
  and groups (\cite{Lebart and al., 1995}).
  
  When several test values are computed, P-values should be adjusted for multiple comparisons.
  }

\value{An object of class \dQuote{vtest}, with 3 components:
  \item{CALL}{The call which produced the result.}
  \item{vtest}{A data frame with the test values.}
  \item{pval}{A data frame with P-values of corresponding test values.}
  }

\references{
  Morineau, A., 1984. \emph{Note sur la caractérisation statistique d'une classe et les valeurs tests}.
  Bulletin Technique du Centre de Statistique et d'Informatique Appliqués \bold{1}, 9:12.\cr
  Lebart, L., Morineau, A., Piron M., 1995. \emph{Statistique exploratoire multidimensionnelle}. Dunod. 439 p.
  }

\seealso{\code{\link{aggstat}}} 

\examples{ 
  f <- vtest(cbind(Sepal.Width, Petal.Length, Petal.Width) ~ Species,
             data = iris)
  plot(f)
  # with 95 per cent confidence region
  plot(f, conf = 95)
  # display test values ordered by variables
  plot(f, group = FALSE)
  }
  
\keyword{classif}
\keyword{htest}

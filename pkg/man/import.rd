\name{import}
\alias{import}
\title{Import Data From External Databases}
\encoding{latin1}

\description{
  This function imports data from tables located in external databases. It is a wrapper for functions
  \code{odbcConnect} and \code{sqlQuery} from the package \pkg{RODBC}. Three database formats are managed
  only on Windows platform : MS Access, MS Excel and Dbase. Note that The 2007 versions *.xlsx and *.accdb
  work with the drivers which are installed with Microsoft Office 2007. Dbase file name can only have a maximum 
  length of 8 characters.
  }

\usage{
  import(tab = NULL, db = NULL, query = "all", as.Date = FALSE)
  }

\arguments{
  \item{tab}{A character chain giving the name of the table in the external database.}
  \item{db}{A character chain giving the name of the external data base. If left empty, file.choose is called resulting 
    in a pop-up window allowing to choose the desired file with the mouse.
    }
  \item{query}{A character chain giving any valid SQL statement. The default ("all") results in the selection of all the 
    records in the target table. It is equivalent to \code{"select * from target_table"}, where \code{target_table} is 
    the table given in \code{tab}.
    }
  \item{as.Date}{Logical (default = FALSE). If TRUE, all variables of classes \code{POSIXct} and \code{POSIXlt}
  are transformed into variables of class \code{Date} with the \code{as.Date} function using a UTC timezone.
    }
  }
\value{A data frame.}
\seealso{ \code{\link[RODBC]{odbcConnect}}, \code{\link[RODBC]{sqlQuery}}, \code{\link{file.choose}} }

\examples{\dontrun{

tmp <- import()
tmp

tmp <- import(tab = "Tab1", db = "D:/Users/Data/tmp.accdb")
tmp
tmp <- import(tab = "Tab1", db = "D:/Users/Data/tmp.mdb")
tmp

tmp <- import(tab = "Sheet1", db = "D:/Users/Data/tmp.xlsx")
tmp
tmp <- import(tab = "Sheet1", db = "D:/Users/Data/tmp.xls")
tmp

  }
}

\keyword{database}
\keyword{utilities}

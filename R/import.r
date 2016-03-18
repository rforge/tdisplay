import <-
function(tab = NULL, db = NULL, query = "all", as.Date = FALSE){

    # Note: The first row of the imported table must be the header

    TZ <- Sys.getenv("TZ")
    if(as.Date) Sys.setenv(TZ = "UTC")

    # function to reverse strings
    strReverse <- function(x)
        sapply(lapply(strsplit(x, NULL), rev), paste, collapse = "")

    # load the RODBC package and stops the program if not available
    if(!require(RODBC))
        stop("This function requires the RODBC package.\n")

    # close all databases in case of error
    on.exit(odbcCloseAll())

    # name of the database if not provided
    if(is.null(db)){

        # Replacement of choose.file (Windows dependent) by tkOpenFile (tcltk)
        # Old:
        #Databases <- matrix(c(
        #    "All files",                  "*.*",
        #    "MS Excel file (*.xls)",      "*.xls",
        #    "MS Access database (*.mdb)", "*.mdb",
        #    "dBase-like file (*.dbf)",    "*.dbf"),
        #    nrow = 4, byrow = TRUE)
        #File <- choose.files(filters = Databases, multi = FALSE, caption = "Select a database.")
        # End Old

        require(tcltk)
        File <- tclvalue(
            tkgetOpenFile(filetypes = "{{All files} *} {{MS Excel file} .xls} {{MS Access database} *.mdb} {{dBase-like file} *.dbf} {{MS Excel file 2007} *.xlsx} {{MS Access database 2007} *.accdb} ")
            )

        if(File != ""){
            sop <- match(".", rev(strsplit(File, NULL)[[1]]))[1]
            ext <- tolower(substring(File, nchar(File) - sop + 2, nchar(File)))
            channel <- switch(EXPR = ext,
                        xls = odbcConnectExcel(File),
                        xlsx = odbcConnectExcel2007(File),
                        mdb = odbcConnectAccess(File),
                        accdb = odbcConnectAccess2007(File),
                        dbf = odbcConnectDbase(File),
                        )
            }
            else
                stop("No database file was selected.\n")

            }

    # name of the database if provided
    else{
        sop <- match(".", rev(strsplit(db, NULL)[[1]]))[1]
        if(is.na(sop))
            stop("You must provide the full path and the extension for the database.\n")
                else{
                    ext <- tolower(substring(db, nchar(db) - sop + 2, nchar(db)))
                    channel <- switch(EXPR = ext,
                        xls   = odbcConnectExcel(db),
                        xlsx  = odbcConnectExcel2007(db),
                        mdb   = odbcConnectAccess(db),
                        accdb = odbcConnectAccess2007(db),
                        dbf   = odbcConnectDbase(db),
                        stop("import not implemented for databases of format .", ext, "\n"))
                    }
        }

    if(ext != "dbf"){
        # sheet or table name is not provided (Excel or Access)
        if(is.null(tab)){
            tabdat <- sqlTables(channel, errors = TRUE, as.is = TRUE) ## 04 Feb 2007: add errors = TRUE
            names(tabdat) <- tolower(names(tabdat))

            # with Access, we limit the access to data tables (system tables are not shown).
            if(ext == "mdb" | ext == "accdb")
                namefil <- tabdat[tabdat$table_type == "TABLE", 3]

            # Excel worksheets
            if(ext == "xls" | ext == "xlsx"){
                tabnam <- tabdat[ , "table_name"]
                namefil <- substring(tabnam, 1, nchar(tabnam) - 1)
                }

            # Now select the table to be imported
            fil <- select.list(namefil, title = "Select a table.")
            if(ext == "xls" | ext == "xlsx"){
                n <- match(fil, table = namefil)
                fil <- tabnam[namefil == fil]
                }
            }

        # sheet or table name is provided (Excel or Access)
        else{
            fil <- if(ext == "xls" | ext == "xlsx") paste(tab, "$", sep = "") else tab
            }

        if(length(fil) == 0)
            stop("No file was selected.")
                else
                    fil <- paste("[", fil, "]", sep = "")

        }

    else{
        # dBase file
        if(!is.null(db))
            File <- db
        root <- tolower(substring(File, 1, nchar(File) - sop))
        revstr <- rev(strsplit(root, NULL)[[1]])
        tab <- strReverse(substring(paste(revstr, collapse = ""), 1, match(c("/"), revstr) - 1))
        direct <- substring(root, 1, nchar(root) - nchar(tab))
        wdir <- getwd()
        setwd(direct)
        fil <- paste("[", tab, "]", sep = "")        
        }

    # retrieve the data
    if(query == "all")
        dat <- sqlQuery(channel = channel, query = paste("select * from", fil))
            else
                dat <- sqlQuery(channel = channel, query = query)
    odbcCloseAll()
    if(ext == "dbf")
        setwd(wdir)

    if(as.Date) dat <- as.data.frame(lapply(dat, function(x) if (class(x)[1] %in% c("POSIXct", "POSIXlt"))  as.Date(x) else x), stringsAsFactors = FALSE)
    Sys.setenv(TZ = TZ)

    dat

    }


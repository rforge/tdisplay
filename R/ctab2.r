ctab2 <-
function (formula, data, weights = NULL, digits = 2) {

    dfname <- as.character(substitute(data))
    if(!is.data.frame(data))
        stop(paste("\n\nThe object ", dfname, " is not data frame.\n\n", sep = ""))

    weights <- as.character(substitute(weights))
    if(length(weights) > 0)
        data[, match(weights, names(data))] <- as.numeric(as.character(data[, match(weights, names(data))]))

    CALL <- match.call()
    # in case the formula was provided as an object
    CALL[[2]] <- formula(deparse(formula))
    
    # convert any variable of mode character into a factor
    data <- as.data.frame(lapply(data, function(x) if(mode(x) == "character") factor(x) else x))
   
    recod <- function(z) {
    	z <- gsub(pattern = "+", replacement = ".", x = z, fixed  = TRUE)
    	z <-gsub(pattern = "-", replacement = ".", x = z)
    	z <-gsub(pattern = "*", replacement = ".", x = z, fixed  = TRUE)
    	z <-gsub(pattern = "/", replacement = ".", x = z)
    	z
    	}
    data <- as.data.frame(lapply(data, function(x) if(is.factor(x)) recod(x) else x))
    
    
    tmp <- data
    f <- formula
        
    #mydata <- formula(agegp ~ alcgp + tobgp)
    #f <- myformula
    #weights <- "wtd"
    #mydata <- tmpx
    
    ######## Transformation of data if needed
    # - tmp: split data used in body of ctab
    # - f: formula passed in body of ctab

        # Grouped data with weights
    if(length(weights) > 0) {
        listvar <- all.vars(formula)
        ftmp <- formula(paste(weights, "~", paste(listvar, collapse = " + ")))
        tmp <- splitbin(formula = ftmp, data = data)$tab
        }
    
        # Grouped data of form "cbind(success, failure)"
    if(substring(deparse(formula)[1], 1, 5) == "cbind") {
        listvar <- all.vars(formula)
        rhs.var <- attr(terms(formula), "term.labels")
        tmp <- splitbin(formula = formula, data = data)$tab
        if(length(rhs.var) == 0)
            f <- formula(paste(listvar[1], "~ 1"))
        else
            f <- formula(paste(listvar[1], "~", paste(rhs.var, collapse = " + ")))  
        }

    f
    head(tmp)
    ######## END

    left.var <- deparse(f[[2]])
    right.var <- attr(terms(f), "term.labels")
    nbfact <- length(right.var)

    left.var
    right.var
    nbfact
    
    nbfactor <- nbfact
    
    # If right-hand = "~ 1"
    if(nbfact == 0) { 
        tmp$xx.v.xx <- rep(1, nrow(tmp))
        f <- formula(paste(deparse(f[[2]]), "~", "xx.v.xx", collapse = "")) 
        right.var <- attr(terms(f), "term.labels")
        nbfactor <- 1
        }

    tabtmp <- data.frame(x = tmp[, left.var])
    names(tabtmp) <- left.var
    head(tabtmp)


    ######## Function acm.disjonctif from package ade4
    acm.dj <- function (df) {
    acm.util <- function(i) {
        cl <- df[, i]
        cha <- names(df)[i]
        n <- length(cl)
        cl <- as.factor(cl)
        x <- matrix(0, n, length(levels(cl)))
        x[(1:n) + n * (unclass(cl) - 1)] <- 1
        dimnames(x) <- list(row.names(df), paste(cha, levels(cl),
            sep = "."))
        return(x)
    }
    G <- lapply(1:ncol(df), acm.util)
    G <- data.frame(G, check.names = FALSE)
    return(G)
    }
    ######## END

    tabdj <- acm.dj(tabtmp)
    nam <- names(tabdj)
    tmp <- cbind(tmp, tabdj)
    head(tmp)

    y <- paste(nam, ",", sep = "", collapse = "")
    y <- substr(y, start = 1, stop = nchar(y) - 1)
    y <- paste("cbind(", y, ")", collapse = "")
    y
    if(nbfactor == 1)
        x <- right.var else {
        	x <- paste(right.var, "+", collapse = "")
        	x  <- substr(x, start = 1, stop = nchar(x) - 1)
        	}
    x
   
    newf <- formula(paste(y, "~", x, collapse = ""))
    newf
    
    tab <- aggstat(formula = newf, data = tmp, FUN = sum)$tab
    z <- (nbfactor + 1):ncol(tab)
    tab$ctab2.Total <- rowSums(tab[, z])
    tab

    tab.p <- tab
    tab.p[, z] <- round(100 * tab.p[, z] / tab.p$ctab2.Total, digits = digits)
    tab.p <- tab.p[, -ncol(tab.p)]
    tab.p
    
    if(nbfact == 0) {
        tab <- tab[, -1]
        tab.p <- tab.p[, -1]
        }
    
    names(tab)[ncol(tab)] <- paste(left.var, ".Total", sep = "")
    
    structure(list(CALL = CALL, formula = CALL[[2]], f = formula(deparse(f)), 
        tab = tab, tab.p = tab.p, digits = digits), class = "ctab2")
        
    }

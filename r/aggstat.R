`aggstat` <-
function(formula, data, FUN, digits = NULL, ...){
    
    dfname <- as.character(substitute(data))
    if(!is.data.frame(data))
        stop(paste("\n\nThe object ", dfname, " is not data frame.\n\n", sep = ""))

    CALL <- match.call()
    f <- formula(deparse(formula))
    CALL[[2]] <- f

    # Convert any variable of mode character into a factor
    data <- as.data.frame(lapply(data, function(x) if(mode(x) == "character") factor(x) else x))
    
    # If no left-hand, aggstat computes the number of rows per group in the dataframe
    if(length(f) == 2) {
        data$n.aggstat <- rep(1, nrow(data))
        f <- formula(paste("n.aggstat ~", f[2]))
        FUN <- "sum"
        }

    right.var <- attr(terms(f), "term.labels")
    nbfact <- length(right.var)

    # Remove NA's in right-hand factors    
    useNA <- FALSE
    if(!useNA & nbfact > 0) {
        X <- data[, right.var]
        if(is.null(dim(X))) X <- data.frame(x = X)
        X <- as.data.frame(lapply(X, function(x) if(is.factor(x)) factor(x, exclude = c(NA, NaN)) else x))
        X <- is.na(X)
        X <- data.frame(nam = 1:nrow(X), na = rowSums(X))
        data <- data[X$nam[X$na == 0], ]
        } 
    
    # If right-hand = "~ 1"
    if(nbfact == 0) { 
        data$xx.v.xx <- rep(1, nrow(data))
        f <- formula(paste(deparse(f[[2]]), "~", "xx.v.xx", collapse = "")) 
        right.var <- attr(terms(f), "term.labels")
        }
      
    # If left-hand = 1 variable
    if(as.character(f[[2]])[1] != "cbind"){
        tmp.data <- data[ , - match(as.character(f[2]), table = names(data)), drop = FALSE]
        mf <- model.frame(formula = f[-2], data = tmp.data)
        List <- vector(mode = "list", length = ncol(mf))
        for(i in seq(ncol(mf)))
            List[[i]] <- levels(factor(mf[, i]))
        tab <- aggregate(
             data[ , match(as.character(f[2]), table = names(data)), drop = FALSE],
             by = mf,
             FUN = FUN,
             na.rm = TRUE, ...)
        names(tab)[ncol(tab)] <- as.character(f[[2]])
        for(j in seq(ncol(tab) - 1))
            tab[, j] <- factor(as.character(tab[, j]), levels = List[[j]])
        }

    # If left-hand > 1 variable  
    else{
        left.var <- as.character(f[[2]])[-1]
        tabs <- list()
        for(i in seq(length(left.var))){ 
            ftmp <- formula(paste(left.var[i], "~", deparse(f[[3]])))
            tmp.data <- data[ , - match(as.character(ftmp[2]), table = names(data)), drop = FALSE]
            mf <- model.frame(formula = ftmp[-2], data = tmp.data)
            List <- vector(mode = "list", length = ncol(mf))
            for(j in seq(ncol(mf)))
                List[[j]] <- levels(factor(mf[, j]))
            tabtmp <- aggregate(
                data[ , match(as.character(ftmp[2]), table = names(data)), drop = FALSE],
                by = mf,
                FUN = FUN,
                na.rm = TRUE, ...)
            names(tabtmp)[ncol(tabtmp)] <- left.var[i]
            for(j in seq(ncol(tabtmp) - 1))
                tabtmp[, j] <- factor(as.character(tabtmp[, j]), levels = List[[j]])
            tabs[[i]] <- tabtmp
            }
        tab <- tabs[[1]]
        for(i in 2:length(tabs))
            tab <- merge(tab, tabs[[i]], by = names(tab)[seq(length(right.var))])  
        }

    ## Order the data set according to the right-hand side of the formula (from left to right)
    
    ordvar <- paste(paste("tab$", right.var, sep = ""), collapse = ", ")
    chaine <- paste("tab[order(", ordvar, "), ]", sep = "")
    tab <- eval(parse(text = chaine))
    
    if(nbfact == 0) {
        if(ncol(tab) == 2) {
            nam <- names(tab)[2]
            tab <- data.frame(tab[, -1])
            names(tab) <- nam
            } else
                tab <- tab[, -1]
        }
    
    structure(list(CALL = CALL, tab = tab, nbfact = nbfact, digits = digits), class = "aggstat")
        
    }




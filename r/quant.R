`quant` <-
function(formula, data, probs = NULL, digits = 3) {

    dfname <- as.character(substitute(data))
    if(!is.data.frame(data))
        stop(paste("\n\nThe object ", dfname, " is not data frame.\n\n", sep = ""))

    CALL <- match.call()
    # in case the formula was provided as an object
    CALL[[2]] <- formula(deparse(formula))

    # Convert any variable of mode character into a factor
    data <- as.data.frame(lapply(data, function(x) if(mode(x) == "character") factor(x) else x))
    
    if(is.null(probs))
        probs <- c(0, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 1)

    all.var <- all.vars(formula)
    right.var <- attr(terms(formula), "term.labels")
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

    y <- data[, all.var[1]]

    ## If right-hand = "~1"

    if(nbfact == 0) {

        z <- matrix(NA, nrow = 1, ncol = length(probs))
        z[1, ] <- quantile(y, probs = probs, na.rm = TRUE)
        tab <- data.frame(z, row.names = 1)
        tab <- cbind(length(y[is.na(y)]), length(y[!is.na(y)]), tab)
        
        names(tab) <- c("NA's", "n", paste(100 * probs, "%", sep = ""))

        }

    ## If right-hand != "~1"

    if(nbfact > 0) {

        data$xx.NA.xx <- ifelse(is.na(y), 1, 0)
        data$xx.n.xx <- 1 - data$xx.NA.xx
        
        f.right <- formula(paste("~", paste(right.var, collapse = "+")))
        f.NA <- formula(paste("xx.NA.xx", "~", paste(right.var, collapse = "+")))
        f.n <- formula(paste("xx.n.xx", "~", paste(right.var, collapse = "+")))

        tmp <- aggstat(formula = f.NA, data = data, FUN = sum)$tab
        names(tmp)[ncol(tmp)] <- "NA's"
        tmp$n <- aggstat(formula = f.n, data = data, FUN = sum)$tab$xx.n.xx
        
        z <- matrix(NA, nrow = nrow(tmp), ncol = length(probs))
        for(i in 1:length(probs))
            z[, i] <- aggstat(formula = formula, data = data, FUN = quantile, probs = probs[i])$tab[, nbfact + 1]
        tab <- data.frame(z, row.names = 1:nrow(z))
        names(tab) <- paste(100 * probs, "%", sep = "")
        tab <- cbind(tmp, tab)

        }
    
    structure(list(CALL = CALL, tab = tab, nbfact = nbfact, digits = digits), class = "quant")

    }


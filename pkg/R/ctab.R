`ctab` <-
function(formula, data, weights = NULL, digits = 2){

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

    ## Transformation of data (when needed)
    # - tmp: split data used in body of ctab
    # - f: formula passed in body of ctab

    tmp <- data
    f <- formula

        # Grouped data with weights
    if(length(weights) > 0) {
        listvar <- all.vars(formula)
        newf <- formula(paste(weights, "~", paste(listvar, collapse = " + ")))
        tmp <- splitbin(formula = newf, data = data)$tab
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

    listvar <- all.vars(f)
    nbvar <- length(listvar)

    right.var <- attr(terms(f), "term.labels")
    nbfact <- length(right.var)
    
    ## Body of ctab

        # One variable in the formula (1-by-J tables)
    if(nbvar == 1)
        tab <- with(tmp, table(eval(parse(text = listvar)), dnn = list(listvar)))

        # At least two variables in the formula (Two-way tables or larger)
    else{
        newlistvar <- listvar[c(nbvar, 1, (nbvar - 1):2)]
        newf <- formula(paste("~", paste(newlistvar, collapse = " + ")))
        tab <- xtabs(formula = newf, data = tmp, drop.unused.levels = TRUE)
        }

    ## Outputs
    
    counts <- data.frame(tab)
    names(counts)[ncol(counts)] <- "n.ctab"
    counts <- counts[counts$n.ctab > 0, ]
    
    if(length(dim(tab)) == 1)
        tab.counts <- addmargins(tab, FUN = list(Total = sum))
    else
        tab.counts <- ftable(formula = f, data = addmargins(tab, margin = 2, FUN = list(Total = sum)))

    if(nbvar == 1)
        tab.p <- prop.table(tab)
    else {
        tab.p <- ftable(formula = f, data = tab)
        tab.p <- prop.table(tab.p, margin = 1)
        }
    tab.p <- round(100 * tab.p, digits = digits)
    
    structure(list(CALL = CALL, formula = CALL[[2]], f = formula(deparse(f)),
        tab = tab, tab.counts = tab.counts, tab.p = tab.p, counts = counts,
        digits = digits), class = "ctab")

    }


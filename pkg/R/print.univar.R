print.univar <-
function(x, ...){

    tab <- x$tab
    nbfact <- x$nbfact
    digits <- x$digits

    #cat("\n")
    #print(x$CALL)
    cat("\n")

    # round the output when necessary
    if(!is.null(digits) && ncol(tab) > 0 && nrow(tab) > 0)
        tab[, (nbfact + 1):ncol(tab)] <- round(tab[, (nbfact + 1):ncol(tab)], digits = digits)

    print(tab)
    cat("\n")

    }

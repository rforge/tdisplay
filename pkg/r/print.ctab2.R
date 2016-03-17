print.ctab2 <-
function(x, ...){

    CALL <- x$CALL
    tab <- x$tab
    tab.p <- x$tab.p
    digits <- x$digits
    
    #cat("\n")
    #print(CALL)
    #cat("\n")
    #cat("\n------------------------------------ Table description \n\n")
    
    #cat("\n")
    #if(length(attr(terms(x$f), "term.labels")) > 0)
    #    z <- paste ("Contingency table ", paste(dim(tab), collapse = "-by-", sep = ""), sep = "")
    #else
    #    z <- paste ("Contingency table 1-by-", dim(tab), sep = "")
    #cat(z, "\n\n")

    cat("\n\n $ Counts: \n\n")
    print(tab)
    cat("\n\n$ Row percentages: \n\n")
    print(tab.p)
    cat("\n")
    
    }

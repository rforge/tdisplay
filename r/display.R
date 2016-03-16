display <-
function(data){

    #, p = 0.05
    
    dfname <- as.character(substitute(data))
    if(!is.data.frame(data))
        stop(paste("\n\nThe object ", dfname, " is not data frame.\n\n", sep = ""))

    CALL <- match.call()
        
    listvar <- names(data)
    
    ## Initial call
    #cat("\n")
    #print(CALL)
    cat("\n")

    ## Display
    
    bullet <- "$ "

    for(j in 1:length(listvar)) {

        x <- data[, listvar[j]]
        if(is.numeric(x)) typvar <- "Numeric"
        if(is.factor(x)) typvar <- "Factor"
        if(is.character(x)) {
            typvar <- "Character"
            x <- as.factor(x)
            }
        if(class(x)[1] == "POSIXct" | class(x)[1] == "POSIXt") typvar <- "Date"

        if(typvar == "Character") {
            cat(bullet, listvar[j], ": ", typvar, "\n\n", sep = "")
            print(table(data[, listvar[j]], exclude = FALSE))
            cat("\n\n")
            }

        if(typvar == "Factor") {
            cat(bullet, listvar[j], ": ", typvar, "\n\n", sep = "")
            print(summary(data[, listvar[j]]))
            cat("\n\n")
            }

        if(typvar == "Numeric") {
            #qlo <- quantile(x, p = p, na.rm = TRUE)
            #qup <- quantile(x, p = 1 - p, na.rm = TRUE)
            cat(bullet, listvar[j], ": ", typvar, "\n", sep = "")
            cat("\n")
            print(summary(x))
            #cat("\n")
            #cat("Values less than percentile ", 100 * p, "%: \n", sep = "")
            #y <- x[!is.na(x)]
            #print(y[y < qlo])
            #cat("\n")
            #cat("Values higher than percentile ", 100 * (1 - p), "%: \n", sep = "")
            #print(y[y > qup])
            cat("\n\n")
            }

        if(typvar == "Date") {
            cat(bullet, listvar[j], ": ", typvar, "\n", sep = "")
            print(table(data[, listvar[j]], exclude = FALSE))
            cat("\n\n")
            }

        }

    }

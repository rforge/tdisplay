vtest <-
function (formula = NULL, data)
{
    acm.disjonctif <- function(df) {
        acm.util <- function(i) {
            cl <- df[, i]
            cha <- names(df)[i]
            n <- length(cl)
            cl <- as.factor(cl)
            x <- matrix(0, n, length(levels(cl)))
            x[(1:n) + n * (unclass(cl) - 1)] <- 1
            dimnames(x) <- list(row.names(df), paste(cha, levels(cl),
                sep = "."))
            x
        }
        G <- lapply(seq(ncol(df)), acm.util)
        data.frame(G, check.names = FALSE)
    }
    CALL <- match.call()
    formula <- formula(deparse(formula))
    CALL[[2]] <- f <- formula
    data <- as.data.frame(lapply(data, function(x) if (mode(x) ==
        "character")
        factor(x)
    else x))
    leftvar <- if (as.character(f[[2]])[1] != "cbind")
        as.character(f[[2]])
    else as.character(f[[2]])[-1]
    if (is.null(f))
        stop("You must specify a formula.")
    ftmp <- formula(paste("~", deparse(f[[3]])))
    group <- if (!deparse(f[[3]]) %in% ls())
        data.frame(model.frame(formula = ftmp, data = data))[,1]
    else eval(f[[3]])
    if (!is.factor(group))
        group <- factor(group)
    n <- nrow(data)
    nk <- summary(group)
    tabs <- list()
    tabp <- list()
    vt <- vector()
    pval <- vector()
    if (is.numeric(as.matrix(data[, leftvar]))) {
        Xbar <- colMeans(as.data.frame(data[, c(leftvar)]))
        varX <- var(data[, c(leftvar)])
        if (length(varX) != 1)
            varX <- diag(varX)
       # if formula grouping variable is not in data
       if (!deparse(f[[3]]) %in% colnames(data))
       {
       datatmp <- data.frame(group = group, data)
       colnames(datatmp)[1] <- deparse(f[[3]])
       }
       else datatmp <- data
        N <- aggstat(formula = f, FUN = mean, data = datatmp)
        N <- N$tab[, -1]
        if (length(leftvar) == 1)
            N <- data.frame(N)
        for (i in seq(length(levels(group)))) {
            for (j in seq(length(leftvar))) {
                Sk <- sqrt(((n - nk[i])/(n - 1)) * ((varX[j])/nk[i]))
                vt[j] <- (N[i, j] - Xbar[j])/Sk
                pval[j] <- (1 - pnorm(abs(vt[j]), lower.tail = TRUE)) *
                  2
            }
            tabs[[i]] <- round(vt, digits = 4)
            tabp[[i]] <- round(pval, digits = 4)
        }
    }
    else {
        if (all(unlist(lapply(X = data[, c(leftvar)], FUN = is.factor)))) {
            if (length(leftvar) == 1) {
                df <- data.frame(data[, leftvar])
                colnames(df) <- deparse(f[[2]])
            }
            else df <- data[, leftvar]
            disj <- acm.disjonctif(df)
            nj <- apply(disj, 2, sum)
            N <- aggregate(disj, by = list(group), FUN = sum)
            N <- N[, -1]
            for (i in seq(length(nk))) {
                for (j in seq(length(nj))) {
                  Sk <- sqrt(nk[i] * ((n - nk[i])/(n - 1)) *
                    (nj[j]/n) * (1 - nj[j]/n))
                  EN <- nk[i] * (nj[j]/n)
                  vt[j] <- (N[i, j] - EN)/Sk
                  pval[j] <- (1 - pnorm(abs(vt[j]), lower.tail = TRUE)) *
                    2
                }
                tabs[[i]] <- round(vt, digits = 4)
                tabp[[i]] <- round(pval, digits = 4)
            }
        }
        else stop("All variables within 'cbind' should be numeric or factors.")
    }
    vtest <- tabs[[1]]
    for (i in 2:length(tabs)) vtest <- cbind(vtest, tabs[[i]])
    vtest <- as.data.frame(vtest)
    dimnames(vtest) <- list(colnames(N), levels(group))
    pval <- tabp[[1]]
    for (i in 2:length(tabp)) pval <- cbind(pval, tabp[[i]])
    pval <- as.data.frame(pval)
    dimnames(pval) <- list(colnames(N), levels(group))
    structure(list(CALL = CALL, vtest = vtest, pval = pval),
        class = "vtest")
}
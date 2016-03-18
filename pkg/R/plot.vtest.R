plot.vtest <-
function(x, group = TRUE, conf = NULL ,
  main = if(group) "test-values by groups" else "test values by variables",
  xlab = "test value", bg = "black", ...){
  if(length(x$vtest) == 0)
    cat("NULL data frame with", length(row.names(x)), "rows.\n")
  else
    if(group)
      dotchart(as.matrix(x$vtest), main = main, xlab = xlab, bg = bg, ...)
    else
      dotchart(t(as.matrix(x$vtest)), main = main, xlab = xlab, bg = bg, ...)
  abline(v = 0)
  # draw the confidence region
  if(!is.null(conf)){
    conf <- (conf + (100 - conf) / 2) / 100
    ci <- qnorm(conf, lower.tail = TRUE)
    abline(v = ci * c(-1, 1), lty = 2)
    }
  }


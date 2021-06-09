print.summary.results <-
  function(x, ...)
  {
    cat(paste("Total Number of Dives: ", as.character(x$tot_dive)))
    cat("\n\n")
    cat("Summary Statistics: \n")
    print(x$md)
    cat("\n")
    invisible(x)
  }
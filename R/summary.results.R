summary.results <-
  function(object, ...)
  {
    md <- matrix(object$avg_max_depth, ncol = 1)
    est.md <- unlist(md[,1])
    names(est.md) <- c("Avg Max Depth")
    std.md <- sqrt(unlist(matrix(object$var_max_depth, ncol = 1)[,1]))

    start <- matrix(object$avg_percentof_md_start_BP, ncol = 1)
    est.start <- unlist(start[,1])
    names(est.start) <- c("Avg % Max Depth at Start")
    std.start <- sqrt(unlist(matrix(object$var_percentof_md_start_BP, ncol = 1)[,1]))

    end <- matrix(object$avg_percentof_md_end_BP, ncol = 1)
    est.end <- unlist(end[,1])
    names(est.end) <- c("Avg % Max Depth at End")
    std.end <- sqrt(unlist(matrix(object$var_percentof_md_end_BP, ncol = 1)[,1]))

    est.btw <- matrix(object$avg_BTW_ratio, ncol = 1)
    names(est.btw) <- c("Bottom-to-Whole Ratio")
    std.btw <- sqrt(unlist(matrix(object$var_BTW_ratio, ncol = 1)[,1]))

    md <- cbind(Estimate = c(est.md, est.start, est.end, est.btw),
                StdDev = c(std.md, std.start, std.end, std.btw))

    tot_dive <- as.numeric(object$total_num_dives)

    out <- list(md=md, tot_dive=tot_dive)
    class(out) <- "summary.results"
    return(out)

  }

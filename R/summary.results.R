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

    start2 <- matrix(object$avg_s_start_BP, ncol = 1)
    est.start2 <- unlist(start2[,1])
    names(est.start2) <- c("Avg % Min Speed at Start")
    std.start2 <- sqrt(unlist(matrix(object$var_s_start_BP, ncol = 1)[,1]))

    end2 <- matrix(object$avg_s_end_BP, ncol = 1)
    est.end2 <- unlist(end2[,1])
    names(est.end2) <- c("Avg % Max Speed at End")
    std.end2 <- sqrt(unlist(matrix(object$var_s_end_BP, ncol = 1)[,1]))

    btw <- matrix(object$avg_BTW_ratio, ncol = 1)
    est.btw <- unlist(btw[,1])
    names(est.btw) <- c("Bottom-to-Whole Ratio")
    std.btw <- sqrt(unlist(matrix(object$var_BTW_ratio, ncol = 1)[,1]))

    # bdv <- matrix(object$mean_bottom_depth_variance, ncol = 1)
    # est.bdv <- unlist(bdv[,1])
    # names(est.bdv) <- c("Avg Bottom-Depth Variance")
    # std.bdv <- sqrt(unlist(matrix(object$var_bottom_depth_variance, ncol = 1)[,1]))
    #

    md <- cbind(Estimate = c(est.md, est.start, est.end, est.start2, est.end2, est.btw),
                StdDev = c(std.md, std.start, std.end, std.start2, std.end2, std.btw))

    tot_dive <- as.numeric(object$total_num_dives)

    out <- list(md=md, tot_dive=tot_dive)
    class(out) <- "summary.results"
    return(out)

  }

#' Computes summary statistics for the previously created phases dataset.
#'
#' @param phases A dataframe corresponding to the output of process_data$phases.
#' @return A dataframe of the dives_by_row. Calling summary on this object will
#' print basic summary statistics about the data.
#' @examples
#' results(full_data, "~/Documents/whale_data/")
#' @export
results <- function(f, outdir){

  loaded <- bustR::loadResults(outputDir = outdir, f)
  d1 <- bustR::process_data(f, loaded)

  phases <- data.frame(d1$phases)
  value <- list()

  value$bottom_phase_data <- bustR::clean_results(loaded, f)

  value$total_num_dives <- nrow(phases)

  value$avg_max_depth <- mean(phases$max_depth_dive)
  value$var_max_depth <- var(phases$max_depth_dive)

  value$avg_percentof_md_start_BP <- abs(mean(as.numeric(phases$depth_begining_bottom / phases$max_depth_dive)))
  value$var_percentof_md_start_BP <- abs(var(as.numeric(phases$depth_begining_bottom / phases$max_depth_dive)))

  value$avg_percentof_md_end_BP   <- abs(mean(as.numeric(phases$depth_ending_bottom   / phases$max_depth_dive)))
  value$var_percentof_md_end_BP   <- abs(var(as.numeric(phases$depth_ending_bottom   / phases$max_depth_dive)))

  value$avg_BTW_ratio <- mean(as.numeric(phases$bottom_to_whole_ratio))
  value$var_BTW_ratio <- var(as.numeric(phases$bottom_to_whole_ratio))

  # value$mean_bottom_depth_variance <- mean(as.numeric(phases$bottom_depth_variance))
  # value$var_bottom_depth_variance <- var(as.numeric(phases$bottom_depth_variance))

  class(value) <- "results"
  return(value)

}

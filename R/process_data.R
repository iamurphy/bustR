#'
#' @param complete_data A dataframe consisting of the complete time-depth record. This should be the output
#' of the find_dives function, specifically the full_data dataframe.
#' @param bottom_phase_results A dataframe consisting of each individual dive on a separate row with appropriate
#' start and end times included. This should be the output of the loadResults once the ShinyApp has been completed.
#' @return phases, which is very similar to the input dataframe bottom_phase_results but now with additional
#' columns added corresponding to potentially useful
#' statistics about the phases of the dives. A complete list of these columns can be found HERE.
#' @examples
#' process_data(dives$full_data, results)
#' @export
process_data <- function(complete_data, bottom_phase_results){

  phases <- bottom_phase_results
  dat <- complete_data

  for (i in 1:(length(unique(phases$dive_num)))) {

    begin_dive <- findInterval(phases$dive_start[i], dat$time)
    begin_bottom <- findInterval(phases$startBP[i], dat$time)
    end_bottom <- findInterval(phases$endBP[i], dat$time)
    end_dive <- findInterval(phases$dive_end[i], dat$time)

    if(begin_dive == 0){
      begin_dive <- begin_dive + 1
    }
    if(begin_bottom == 0){
      begin_bottom <- begin_bottom + 1
    }
    if(end_bottom == 0){
      end_bottom <- end_bottom + 1
    }
    if(end_dive == 0){
      end_dive <- end_dive + 1
    }

    if(begin_dive > nrow(dat)){
      begin_dive <- nrow(dat)
    }
    if(begin_bottom > nrow(dat)){
      begin_bottom <- nrow(dat)
    }
    if(end_bottom > nrow(dat)){
      end_bottom <- nrow(dat)
    }
    if(end_dive > nrow(dat)){
      end_dive <- nrow(dat)
    }

    # dat[begin_dive:begin_bottom, "phase"] <- "D"
    # dat[begin_bottom:end_bottom, "phase"] <- "B"
    # dat[end_bottom:end_dive, "phase"] <- "A"

    sliced_dat <- dat[begin_dive:end_dive, ]
    # We find min depth (because all depths are negative).
    phases$max_depth_dive[i] <- min(sliced_dat$depth)
    phases$depth_begining_bottom[i] <- dat[begin_bottom, ]$depth
    phases$depth_ending_bottom[i] <- dat[end_bottom, ]$depth

    phases$dive_duration_seconds[i] <- as.numeric(phases$dive_end[i] - phases$dive_start[i])

    phases$bottom_duration_seconds[i] <- as.numeric(phases$endBP[i] - phases$startBP[i])

    phases$bottom_to_whole_ratio[i] <- as.numeric(phases$bottom_duration_seconds[i] /
                                                    phases$dive_duration_seconds[i])

  }
  return(list(phases = phases))
}

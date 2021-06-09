#' Clean the resulting dataframe to prep it for modelling. Add max depth column.
#'
#' @param u User results from ShinyApp. Use r$bottom_phase_data
#' @param d Full data frame. Use r$full_data
#' @return u but modified with extra columns.
#' @export
clean_results <- function(u, d){
  
  # Add MAX DEPTH
  i1 <- findInterval(as.numeric(u$dive_start), d$time)
  i2 <- findInterval(as.numeric(u$dive_end), d$time)
  
  max_d <- c()
  
  for (j in 1:length(i1)) {
    subd <- d[i1[j]:i2[j],]
    max_d <- c(max_d, min(subd$depth))
  }
  
  u$max_depth <- round(max_d, 2)
  
  
  # Add START and END Proportions
  
  u$BP_prop_start <- (u$startBP - u$dive_start) / (u$dive_end - u$dive_start)
  u$BP_prop_end <- (u$endBP - u$dive_start) / (u$dive_end - u$dive_start)
  
  # Add DURATION
  
  u$duration <- u$dive_end - u$dive_start
  
  return(u)
}
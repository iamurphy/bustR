#' Loading the data produced from the Shiny App
#'
#' @param outputDir A string containing the path to where the ShinyApp data has been stored.
#' @return A dataframe containing the start and end times of the dive as well as the start
#' and end times of the user-defined bottom phase. The dive number and the time stamp of when
#' the bottom phase data had been saved are included as well.
#' @examples
#' loadResults("path/to/Shiny/data/ouputs/")
#' @export
loadResults <- function(outputDir, full_data) {
  # Read all the files into a list
  files <- list.files(outputDir, full.names = TRUE)
  if(length(files) == 0){
    return(NULL)
  }
  data <- lapply(files, read.csv, stringsAsFactors = FALSE)
  # Concatenate all data together into one data.frame
  data <- do.call(rbind, data)
  # Sort by dive_num
  data <- data %>%
    dplyr::arrange(by = dive_num)

  return(data)
}

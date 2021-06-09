#' Run the ShinyApp with the user's own dataset.
#'
#' @param entire_record A dataframe containing time-depth data as produced from the read_dive_data function.
#' @param individual_dives A dataframe wherein each row is an individual dive from the animal. This
#' must be equivalent to the output of bustR::find_dives().
#' @param init_num The initial number of randomly selected dives that the user wishes to label the bottom phase.
#' Defaults to 10 dives.
#' @param rand_select Defaults to TRUE. If FALSE, dives will not be randomly chosen from the dataset, but instead will
#' start at 1 and continue until init_num is reached.
#' @param file_path A string indicating a path to a folder where the resulting data will be stored and accessed.
#' @param continuing A boolean, defaulting to FALSE. If you are continuing bottom phase identification from a
#' previous session, and you have your previous data stored in file_path, then set this parameter to be TRUE.
#' The app will ignore all dives which have been previously labelled, allowing you to "pick up where you left off".
#' If this parameter is set to be TRUE, then you need to make sure that the inputted dataframes are identical to
#' the ones from the previous session. The data should come from the same tagged animal, and all other parameters should be
#' the same as they were before, including mindepth and surface from the find_dives function. If there are discrepancies or
#' differences between the previous sessions' data and the current sessions' data, errors can potentially arise since the
#' time stamps of the dives in the current dataframe may not line up with the ones that are currently saved on your machine.
#' @return Runs an interactive ShinyApp and saves your responses as .csv files in the file_path folder.
#' @examples
#' runBottomPhaseApp(dives$full_data, init_num = 50, file_path = "~/Desktop/saved_data_whale1/", continuing = TRUE)
#' runRBottomPhaseApp(dives$full_data, init_num = 10, file_path = "C://Users/name/Documents/whale_data/", continuing = FALSE)
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_smooth geom_vline scale_x_continuous
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @export
runBottomPhaseApp <- function(entire_record, file_path, init_num = 10, custom_dives = c(),
                              rand_select = TRUE, continuing = FALSE, launch_browser = FALSE) {

  .GlobalEnv$.entire.record <- entire_record
  .GlobalEnv$.init_num <- init_num
  .GlobalEnv$.saved_app_outputs <- file_path

  appDir <- system.file("shiny-example", "myapp", package = "bustR")

  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `bustR`.", call. = FALSE)
  }
  if(.init_num <= 0){
    stop("init_num must be > 0.", call. = FALSE)
  }
  if(length(custom_dives) != 0){
    .GlobalEnv$.rand_dives <- custom_dives
    shiny::runApp(appDir, display.mode = "normal", launch.browser = launch_browser)
    return()
  }

  pop <- sort(unique(.entire.record$dive_num)[unique(.entire.record$dive_num) != 0])
  print(pop)

  ###### CHECK IF CONTINUING IS TRUE THEN REMOVE THE ALREADY SEEN DIVES, THEN RANDOMLY SAMPLE AND CHECK CONDITIONS

  if(continuing == TRUE){

    if(length(custom_dives) != 0){
      stop("custom_dives has non-zero length, but continuing == TRUE. If you want to mark custom_dives, then set continuing == FALSE.")
    }

    .GlobalEnv$.previous_results <- bustR::loadResults(file_path)

    if(is.null(.previous_results)){
      message(paste("continuing has been set to TRUE, however,",
                    "no data was found at the given file_path."))
      user_input <- readline(prompt = "Do you want to continue with the ShinyApp? [y/n]: ")

      if(grepl("n", as.character(user_input), ignore.case = TRUE)){
        return()
      }
      else{
        new_pop <- pop
      }
    }

    if(!is.null(.previous_results)){
      dives_seen <- unique(.previous_results$dive_num)
      new_pop <- pop[!(pop %in% dives_seen)]
    }
  }

  if(continuing == FALSE){
    new_pop <- pop
  }

  if(.init_num > length(new_pop)){
    stop(paste("init_num is larger than total number of dives available (",
               length(new_pop),
               "). Decrease init_num.", sep = ""))
  }

  if(rand_select == TRUE){
    .GlobalEnv$.rand_dives <- c(sample(new_pop, .init_num), new_pop[1])
  }
  if(rand_select == FALSE){
    .GlobalEnv$.rand_dives <- c(new_pop[1:.init_num], new_pop[1])
  }

  on.exit(rm(.init_num, envir=.GlobalEnv))
  on.exit(rm(.entire.record, envir=.GlobalEnv))
  on.exit(rm(.rand_dives, envir=.GlobalEnv))

  shiny::runApp(appDir, display.mode = "normal", launch.browser = launch_browser)
}

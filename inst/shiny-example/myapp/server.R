read_in_prev_dives <- function(p, num){
  # Get list of file names
  list_of_files <- list.files(path = p,
                              recursive = TRUE,
                              pattern = "\\.csv$",
                              full.names = FALSE)

  list_of_files2 <- list.files(path = p,
                               recursive = TRUE,
                               pattern = "\\.csv$",
                               full.names = TRUE)

  correct_name <- c()
  correct_path <- c()

  for (i in 1:length(list_of_files)) {
    if(grepl(as.character(num), list_of_files[i], fixed = TRUE)){
      correct_name <- c(correct_name, list_of_files[i])
    }
  }

  for (i in 1:length(list_of_files2)) {
    if(grepl(correct_name[1], list_of_files2[i], fixed=TRUE)){
      correct_path <- c(correct_path, list_of_files2[i])
    }
  }

  dive_data <- read.csv(correct_path[1], header=TRUE)

  sBP <- dive_data$startBP
  eBP <- dive_data$endBP

  return(c(sBP, eBP))
}



############################################################ Define SERVER ############################################################
server <- function(input, output, session) {

  # observeEvent(input$browser,{
  #   browser()
  # })

  vals <- reactiveValues(count = 1)
  all_dives <- reactiveValues(ad = as.vector(.rand_dives))
  submitted_dives <- reactiveValues(sd = c())

  dive_df <- reactiveValues(df1 = .entire.record)

  v <- reactiveValues(data = NULL)
  v$start <- 0
  v$end <- 0

  observeEvent(input$nextdive, {
    vals$count <- vals$count + 1

    complete_info <- dive_df$df1
    subData <- dplyr::filter(complete_info, complete_info$dive_num == .rand_dives[vals$count])

    max_i <- which.max(subData$depth)
    max_t <- as.numeric(subData$time[max_i])

    v$subData <- subData
    v$start <- subData$time[1]
    v$end <- subData$time[nrow(subData)]
    v$dive_num <- subData$dive_num[1]
    v$dive_start_time <- subData$time[1]
    v$dive_end_time <- subData$time[nrow(subData)]
    v$max_t <- max_t

  })

  observeEvent(input$prevdive, {
    vals$count <- vals$count - 1

    complete_info <- dive_df$df1
    subData <- dplyr::filter(complete_info, complete_info$dive_num == .rand_dives[vals$count])

    max_i <- which.max(subData$depth)
    max_t <- as.numeric(subData$time[max_i])

    v$subData <- subData
    v$start <- subData$time[1]
    v$end <- subData$time[nrow(subData)]
    v$dive_num <- subData$dive_num[1]
    v$dive_start_time <- subData$time[1]
    v$dive_end_time <- subData$time[nrow(subData)]
    v$max_t <- max_t
  })

  observeEvent(input$GO, {
    complete_info <- dive_df$df1
    subData <- dplyr::filter(complete_info, complete_info$dive_num == .rand_dives[vals$count])

    max_i <- which.max(subData$depth)
    max_t <- as.numeric(subData$time[max_i])

    v$subData <- subData
    v$start <- subData$time[1]
    v$end <- subData$time[nrow(subData)]
    v$dive_num <- subData$dive_num[1]
    v$dive_start_time <- subData$time[1]
    v$dive_end_time <- subData$time[nrow(subData)]
    v$max_t <- max_t
  })


  # s <- eventReactive(c(input$nextdive, input$prevdive, input$GO), {
  #
  #   complete_info <- dive_df$df1
  #
  #   subData <- dplyr::filter(complete_info, complete_info$dive_num == .rand_dives[vals$count])
  #
  #   max_i <- which.max(subData$depth)
  #   max_t <- as.numeric(subData$time[max_i])
  #
  #   v$subData <- subData
  #   v$start <- subData$time[1]
  #   v$end <- subData$time[nrow(subData)]
  #   v$dive_num <- subData$dive_num[1]
  #   v$dive_start_time <- subData$time[1]
  #   v$dive_end_time <- subData$time[nrow(subData)]
  #   v$max_t <- max_t
  #
  #   subData
  #
  # })

  ######## PRODUCE GRAPH ########
  output$new_plot <- renderPlot({

    if(is.null(v$subData)) return()
    subData2 <- v$subData
    num.time <- as.numeric(subData2$time)

    # # Set up the data for the plot. Modify the x axis
    x <- ggplot(data=subData2,
                aes(x=as.numeric(time), y = depth)) +
      ggplot2::labs(x = "Time (in seconds)",
                    y = "Depth (m)",
                    title = paste("Dive #", as.character(subData2$dive_num), sep = "")) +
                    #subtitle = paste("vals$count = ", vals$count)) +
      ggplot2::theme_classic()
    # # Plot the dive as well as the vertical lines which will move.
    x <- x +
      geom_line() +
      geom_vline(xintercept = as.numeric(input$startBP), color = "blue") +
      geom_vline(xintercept = as.numeric(input$endBP), color = "red") +
      scale_x_continuous(breaks = c(min(num.time),
                                    (max(num.time) +
                                       min(num.time)) / 2,
                                    max(num.time)),
                         labels = c(0,
                                    round((max(num.time) +
                                             min(num.time)) / 2  -
                                            min(num.time), 1),
                                    round(max(num.time) -
                                            min(num.time), 1)))


    x
  }, height = 400, width = 600)



  ########  DEFINE THE SLIDERS  ########
  output$start_e <- renderUI({
    fluidRow(
      column(12,
             tagList(
               sliderInput(inputId = "startBP",
                           "Pick a start time:",
                           min = as.numeric(v$start),
                           max = as.numeric(v$end),
                           value = ifelse(.rand_dives[vals$count] %in% submitted_dives$sd,
                                          read_in_prev_dives(.saved_app_outputs,
                                                             as.numeric(.rand_dives[vals$count]))[1],
                                          as.numeric(v$start)),
                           ticks = FALSE,
                           step = 0.02,
                           width = "600px")))
    )
  })

  output$end_e <- renderUI({
    fluidRow(
      column(12,
             tagList(
               sliderInput(inputId = "endBP",
                           "Pick an end time:",
                           min = as.numeric(v$start),
                           max = as.numeric(v$end),
                           value = ifelse(.rand_dives[vals$count] %in% submitted_dives$sd,
                                          read_in_prev_dives(.saved_app_outputs,
                                                             as.numeric(.rand_dives[vals$count]))[2],
                                          as.numeric(v$end)),
                           ticks = FALSE,
                           step = 0.02,
                           width = "600px")))
    )
  })



  # # ########  SAVE THE DATA TO THE CSV DROPBOX  ########
  observeEvent(input$submit, {

    if (.rand_dives[vals$count] %in% submitted_dives$sd) {

      shinyalert::shinyalert(title="Warning",
                             text = "You have already submitted information for this dive during this session. By pressing continue, you will overwrite the previously saved data. Do you wish to continue?",
                             type = "warning",
                             confirmButtonText = "Continue",
                             showCancelButton = TRUE,
                             showConfirmButton = TRUE,
                             closeOnEsc = FALSE,
                             closeOnClickOutside = FALSE,
                             inputId = "confirmSubmit")
    }

    else{

      outputDir <- .saved_app_outputs

      if(file.exists(file.path(outputDir,
                               sprintf("dive%s.csv",
                                       as.integer(.rand_dives[vals$count]))))){

        shinyalert::shinyalert(title = "Warning",
                               text = "A file already exists for this dive. If you are continuing from a previous session, make sure to set continuing = TRUE. Otherwise check if the file_path is pointing to the correct location for this tag data. Do you wish to continue? Continuing will overwrite the previously saved file.",
                               type = "warning",
                               confirmButtonText = "Continue",
                               showCancelButton = TRUE,
                               showConfirmButton = TRUE,
                               closeOnEsc = FALSE,
                               closeOnClickOutside = FALSE,
                               inputId = "confirmReplaceDive")
      }

      else{

        fields <- c("startBP", "endBP")
        temp <- sapply(fields, function(x) input[[x]])
        temp <- c(temp,
                  dive_start = v$dive_start_time,
                  dive_end = v$dive_end_time,
                  dive_num = v$dive_num,
                  timestamp = format(Sys.time(), "%Y%m%d-%H%M%OS"))

        d <- temp
        data <- t(d)

        outputDir <- .saved_app_outputs

        # Create a unique file name
        fileName <- sprintf("dive%s.csv", as.integer(.rand_dives[vals$count]))
        # Write the file to the local system
        write.csv(
          x = data,
          file = file.path(outputDir, fileName),
          row.names = FALSE, quote = TRUE
        )

        submitted_dives$sd <- c(submitted_dives$sd, .rand_dives[vals$count])

      }
    }

    if (vals$count == .init_num) {
      shinyalert::shinyalert("This is the last dive in the dataset!",
                             type = "info",
                             animation = FALSE,
                             inputId = "finished")
    }

  })

  observeEvent(c(input$confirmSubmit, input$confirmReplaceDive), {

    fields <- c("startBP", "endBP")
    temp <- sapply(fields, function(x) input[[x]])
    temp <- c(temp,
              dive_start = v$dive_start_time,
              dive_end = v$dive_end_time,
              dive_num = v$dive_num,
              timestamp = format(Sys.time(), "%Y%m%d-%H%M%OS"))
    d <- temp
    data <- t(d)

    outputDir <- .saved_app_outputs
    # Create a unique file name
    fileName <- sprintf("dive%s.csv", as.integer(.rand_dives[vals$count]))
    # Write the file to the local system
    write.csv(
      x = data,
      file = file.path(outputDir, fileName),
      row.names = FALSE, quote = TRUE
    )

    submitted_dives$sd <- c(submitted_dives$sd, .rand_dives[vals$count])

  })

  observe({
    shinyjs::toggleState(id = "prevdive", condition = vals$count > 1)
    shinyjs::toggleState(id = "nextdive", condition = vals$count < (length(.rand_dives) - 1))
  })

  observe({
    if(.rand_dives[vals$count] %in% submitted_dives$sd){
      shinyjs::show('text_div')
      output$text <- renderText({"Dive information has been saved for this dive."})
    }
    else{
      shinyjs::hide('text_div')
    }
  })

  # observeEvent(input$submit, {
  #   if (vals$count == .init_num) {
  #     shinyalert::shinyalert("This is the last dive in the dataset!",
  #                            type = "info",
  #                            animation = FALSE,
  #                            inputId = "finished")
  #   }
  # })

  session$onSessionEnded(function() {
    stopApp()
  })

}

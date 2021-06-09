print.results <-
  function(x, ...)
  {
    cat("Data from ShinyApp: \n")
    print(x$bottom_phase_data)
  }
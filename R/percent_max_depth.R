#' Produce Optimized Theta value
#'
#' @param d dataframe of results from Shiny App.
#' @return Optimized Theta
#' @export
percent_max_depth <- function(full_data,
                              bottom_phase_data, 
                              start=TRUE){
  
  theta_start <- optimize(optim_theta_start, 
                          c(0.50, 0.99),
                          tol = 0.0000001,
                          d = full_data,
                          r = bottom_phase_data)
  theta_end <- optimize(optim_theta_end, 
                        c(0.50, 0.99),
                        tol = 0.0000001,
                        d = full_data,
                        r = bottom_phase_data)
  
  ans1 <- paste0("The percentage threshold (of max depth) for the start: ", 
                round(theta_start$minimum, 3)*100, "%")
  ans2 <- paste0("The percentage threshold (of max depth) for the end: ", 
                round(theta_end$minimum, 3)*100, "%")
  
  print(c(ans1, ans2))
}

optim_theta_start <- function(t, d, r){
  df <- bustR::produce_Ri_depth(t, d, r)
  sum_squares <- sum((df$UserStart - df$start_prop_theta)^2)
  if(is.na(sum_squares)){
    return(1000000000000)
  }
  return(sum_squares)
}

optim_theta_end <- function(t, d, r){
  df <- bustR::produce_Ri_depth(t, d, r)
  sum_squares <- sum((df$UserEnd - df$end_prop_theta)^2)
  if(is.na(sum_squares)){
    return(1000000000000)
  }
  return(sum_squares)
}










# 
# 
# 
# # Bootstrap to get distribution of Theta
# 
# B <- 500
# t_start <- c()
# t_end <- c()
# 
# for (b in 1:B) {
#   print(b)
#   
#   samp <- sample(1:length(data), size = length(data), replace = TRUE)
#   
#   boot_data <- data[samp]
#   boot_start <- y[samp]
#   boot_end <- y.end[samp]
#   
#   # Perform optimization
#   results_start <- optimize(optim_theta_start, 
#                             c(0.5, 0.95),
#                             tol = 0.0000001,
#                             data = boot_data,
#                             responses = cbind(boot_start, boot_end))
#   
#   # Optimize end of bottom phase
#   results_end <- optimize(optim_theta_end, 
#                           c(0.5, 0.95),
#                           tol = 0.0000001,
#                           data = boot_data,
#                           responses = cbind(boot_start, boot_end))
#   
#   t_start <- c(t_start, results_start$minimum)
#   t_end   <- c(t_end,   results_end$minimum)
#   
#   hist(t_start)
# }
# 
# hist(t_start, breaks = 20)
# hist(t_end, breaks = 20)
# 
# mean(t_start)
# mean(t_end)
# 
# median(t_start)
# median(t_end)
# 
# sd(t_start)
# sd(t_end)

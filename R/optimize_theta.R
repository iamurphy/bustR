# #' @export 
#' optim_theta_start <- function(t, d, r){
#'   df <- bustR::produce_Ri_depth(t, d, r)
#'   sum_squares <- sum((df$UserStart - df$start_prop_theta)^2)
#'   if(is.na(sum_squares)){
#'     return(1000000000000)
#'   }
#'   return(sum_squares)
#' }


# optim_theta_end <- function(t, d, r){
#   df <- bustR::produce_Ri_depth(t, d, r)
#   sum_squares <- sum((df$UserEnd - df$end_prop_theta)^2)
#   if(is.na(sum_squares)){
#     return(1000000000000)
#   }
#   return(sum_squares)
# }
# theta_end <- optimize(optim_theta_end, 
# c(0.50, 0.99),
# tol = 0.0000001,
# d = full_data,
# r = cbind(bottom_phase_data$BP_prop_start,
#           bottom_phase_data$BP_prop_end))

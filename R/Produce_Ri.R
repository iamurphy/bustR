#' @export 
produce_Ri_depth <- function(theta, data, responses){
  
  dive_nums <- responses$dive_num
  
  final_L <- list()
  final_L2 <- list()
  
  for (j in 1:length(theta)) {

    res.1 <- c()
    res.2 <- c()
    
    for (i in dive_nums) {

      curr_dive <- data %>% 
        dplyr::filter(dive_num == i)
      
      start_time <- as.numeric(responses$dive_start[i])
      end_time   <- as.numeric(responses$dive_end[i])
      
      curr_depth_target <- theta[j] * min(curr_dive$depth)
      
      sb_prop_index <- as.numeric(which(diff(sign(curr_depth_target - curr_dive$depth))!=0)[1])
      sb_prop <- sb_prop_index / nrow(curr_dive)
      
      eb_prop_index <- tail(which(diff(sign(curr_depth_target - curr_dive$depth))!=0), n = 1)
      eb_prop <- eb_prop_index / nrow(curr_dive)
      
      if(is.na(sb_prop_index)){
        if(length(eb_prop_index) == 0){
          res.1 <- c(res.1, 0)
          res.2 <- c(res.2, 1)
          next
        }
        res.1 <- c(res.1, 0)
        res.2 <- c(res.2, eb_prop)
        next
      }
      
      if(length(eb_prop_index) == 0){
        res.1 <- c(res.1, sb_prop)
        res.2 <- c(res.2, 1)
        next 
      }
      
      res.1 <- c(res.1, sb_prop)
      res.2 <- c(res.2, eb_prop)
    }
    final_L[[j]] <- res.1
    final_L2[[j]] <- res.2
  }
  
  Ris <- as.data.frame(do.call(cbind, final_L))
  Ris2 <- as.data.frame(do.call(cbind, final_L2))
  names(Ris) <- "start_prop_theta"
  names(Ris2) <- "end_prop_theta"
  df <- cbind("UserStart" = responses$BP_prop_start, 
              "UserEnd" = responses$BP_prop_end,
              Ris,
              Ris2)
  return(df)
}

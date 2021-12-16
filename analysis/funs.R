filter_data <- function(true_data) {
  
  require(tidyverse)
  participant_breakdown <- matrix(nrow = 0, ncol = 5)
  for (file in true_data) {
      this_data <- read.csv(file.path(data_path, file))
      
      this_pb <- cbind(this_data$condition[which(this_data$condition!="")], 
                       this_data$genre[which(this_data$genre!="")], 
                       this_data$order[which(this_data$order!="")])
      
      wm <- this_data$wm_recall_response.text[which(this_data$wm_recall_response.text!="")]
      
      if (is.null(this_pb))
        next
      
      else {
        if (is.null(wm)) {
          this_pb <- cbind(this_pb, rep(0, nrow(this_pb)))
        } else {
          this_pb <- cbind(this_pb, rep(1, nrow(this_pb)))
        }
        this_pb <- cbind(rep(strsplit(file,"_")[[1]][1], nrow(this_pb)), this_pb)
        participant_breakdown <- rbind(participant_breakdown, this_pb)
      }
    }
    
    participant_breakdown <- as.data.frame(participant_breakdown)
    colnames(participant_breakdown) <- c("ID", "condition", "genre", "order", "wm")
    
    tallied_conditions <- participant_breakdown %>% group_by(ID, genre) %>% tally()
    
    complete_datasets <- tallied_conditions[tallied_conditions$n==3,]
    return(complete_datasets)
  }
require(tidyverse)

filter_data <- function(true_data) {
  
  file_names <- matrix(nrow = 0, ncol = 1)
  for (file in true_data) {
    this_data <- read.csv(file.path(data_path, file))
    
    this_pb <- cbind(this_data$condition[which(this_data$condition!="")], 
                     this_data$genre[which(this_data$genre!="")], 
                     this_data$order[which(this_data$order!="")])
    
    ratings <- this_data$ratings_slider.response[which(this_data$ratings_slider.response!="")]
    openbox <- this_data$open_text_response.text[which(this_data$open_text_response.text!="")]
    
    if ( is.null(this_pb) ) { # no data
      next
      
    } else if ( nrow(this_pb) < 6 ) { # not all conditions seen
      next
      
    } else if ( length(ratings) < (3*2*4) ) { # not all ratings given, N = levels of prime * genres * questions
      next
      
    } else if ( length(openbox) < (3*2*2) ) { # not all open-text responses given, N = levels * genre * 2 boxes
      next
      
    } else { # data's fine!
      file_names <- rbind(file_names, file)
    }
  }
  return(file_names)
}

wrangle_data <- function(data_path, file_names) {
  
  data <- data.frame()
  demog_data <- data.frame()
  for (file in file_names) {
    this_data <- read.csv(file.path(data_path, file))
    
    # demographic and musical background data
    this_demogdata <- cbind(this_data$participant, this_data$EXPRA.code..only.for.EXPRA.students., this_data[1], this_data$age_resp.text)[which(this_data$age_resp.text!=""),] %>%
      as.data.frame()
    this_musicdata <- cbind(this_data$scale, this_data$GSI_question_loop.thisIndex, this_data$questions_english, this_data$questions_german, 
                            this_data$GSI_slider.response, this_data$labels_marks)[which(this_data$scale!=""),] %>% as.data.frame() %>%
      add_column(this_demogdata, .before = 1)
    
    # dependent variables
    this_maindata <- cbind(this_data$ratings_loop.thisIndex, this_data$questions, this_data$ratings_slider.response, this_data$open_text_response.text, this_data$eval_slider.response)[which(this_data$questions!=""),]
    this_knew <- this_data$knew_slider.response[which(this_data$knew_slider.response!="")] 
    # independent variables
    this_struct <- cbind(this_data$condition, this_data$genre, this_data$order, this_data$probe)[which(this_data$condition!=""),]
    this_struct.full <- matrix(rep(this_struct, each = 6), ncol=ncol(this_struct), byrow = FALSE) #
    
    # compile
    this_data2 <- cbind(
      rbind(this_struct.full[1:(nrow(this_struct.full)/2),], matrix(rep(c("NA", this_struct.full[1,2], "NA", "NA"),3), ncol = 4, byrow = TRUE),
            this_struct.full[(nrow(this_struct.full)/2+1):nrow(this_struct.full),], matrix(rep(c("NA", this_struct.full[nrow(this_struct.full),2], "NA", "NA"),3), ncol = 4, byrow = TRUE)),
      this_maindata) %>% as.data.frame() 
    
    this_data3 <- this_data2 %>%
      setNames(c("condition", "genre", "prime_order", "probe", "question_n" ,"question", "rating", "open_text", "final_eval")) %>%
      mutate(knew_piece = rep(this_knew, each = nrow(this_data2)/2)) %>%
      mutate(subj = this_demogdata$`this_data$participant`, .before = 1) %>%
      mutate(EXPRA = this_demogdata$`this_data$EXPRA.code..only.for.EXPRA.students.`,
             sex = this_demogdata$`ï..sex_resp.response`,
             age = this_demogdata$`this_data$age_resp.text`)
    
    
    # working memory
    wm_data <- cbind(this_data$wm_recall_response.text,this_data$confidence_resp.response)[which(this_data$wm_recall_response.text!=""),]
    
    if (is.null(wm_data)) {
      this_data3 <- this_data3 %>% mutate(wm = 0, .after = "probe")
      this_data4 <- this_data3 %>% mutate(wm_response = NA, .after = "wm") %>% mutate(confidence = NA, .after = "wm_response") 
      
    } else {
      this_data3 <- this_data3 %>% mutate(wm = 1, .after = "probe")
      wm_data <- wm_data[2:nrow(wm_data),]
      
      wm_data.full <- matrix(rep(wm_data, each = 6), ncol=ncol(wm_data), byrow = FALSE)
      wm_data.paste <- rbind(wm_data.full[1:(nrow(wm_data.full)/2),], matrix(rep(rep("NA",2),3), ncol = 2, byrow = TRUE),
                             wm_data.full[(nrow(wm_data.full)/2+1):nrow(wm_data.full),], matrix(rep(rep("NA", 2),3), ncol = 2, byrow = TRUE)) %>% as.data.frame() %>%
        setNames(c("wm_response", "confidence"))
      
      this_data4 <- this_data3 %>% add_column(wm_data.paste, .after = "wm") 
    }
    
    data <- rbind(data, this_data4)
    demog_data <- rbind(demog_data, this_musicdata)
  }  
  return(list(data = data, demgraphics = demog_data))
}
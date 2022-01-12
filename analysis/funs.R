require(tidyverse)

  filter_data <- function(all_data) {
    
    participant_IDs <- unlist(lapply(strsplit(all_data,"_"), '[[', 1)) # extract participant ID
    true_participants <- !stringr::str_starts(participant_IDs, "00") # remove those IDs that start with 00, such as 001 or 002
    true_data <- all_data[true_participants]
    
    file_names <- matrix(nrow = 0, ncol = 1)
    participant_breakdown <- matrix(nrow = 0, ncol = 5)
    for (file in true_data) {
      # print(file)
      skip_file <- FALSE
      tryCatch({this_data <- read.csv(file.path(data_path, file), header = TRUE)},
               error = function(err) {
                 skip_file <- TRUE
                 #print(paste("Error on file:", file," | Sys:", err))
               })
      
      if(skip_file) { next 
      } else { 
      this_pb <- cbind(this_data$condition[which(this_data$condition!="")], 
                       this_data$genre[which(this_data$genre!="")], 
                       this_data$order[which(this_data$order!="")])
      
      wm <- this_data$wm_recall_response.text[which(this_data$wm_recall_response.text!="")]
      ratings <- this_data$ratings_slider.response[which(this_data$ratings_slider.response!="")]
      openbox <- this_data$open_text_response.text[which(this_data$open_text_response.text!="")]
      
      if ( is.null(this_pb) ) { # no data
        #print(paste("No data for file:", file))
        next
        
      } else if ( nrow(this_pb) < 6 ) { # not all conditions seen
        #print(paste("Incomplete data for file:", file))
        next
        
      } else if ( length(ratings) < (3*2*4) ) { # not all ratings given, N = levels of prime * genres * questions
        #print(paste("Incomplete data for file:", file))
        next
        
      } else if ( length(openbox) < (3*2*2) ) { # not all open-text responses given, N = levels * genre * 2 boxes
        #print(paste("Incomplete data for file:", file))
        next
        
      } else { # data is fine!
        if (is.null(wm)) {
          this_pb <- cbind(this_pb, rep(0, nrow(this_pb)))
        } else {
          this_pb <- cbind(this_pb, rep(1, nrow(this_pb)))
        }
        this_pb <- cbind(rep(strsplit(file,"_")[[1]][1], nrow(this_pb)), this_pb)
        participant_breakdown <- rbind(participant_breakdown, this_pb)
        file_names <- rbind(file_names, file)
      }
      }
    }
    participant_breakdown <- participant_breakdown %>% as.data.frame() %>% setNames(c("ID", "condition", "genre", "order", "wm"))
    
    return(list(participant_breakdown = participant_breakdown, file_names = file_names))
    #return(file_names)
}

wrangle_data <- function(data_path, file_names) {
  
  data <- data.frame()
  demog_data <- data.frame()
  for (file in file_names) {
    print(file)
    this_data <- read.csv(file.path(data_path, file))
    
    # demographic and musical background data
    this_demogdata <- cbind(this_data$participant, this_data$EXPRA.code..only.for.EXPRA.students., this_data[1], this_data$age_resp.text)[which(this_data$age_resp.text!=""),] %>%
      as.data.frame() %>% setNames(c("ID", "EXPRA_code", "sex", "age"))
    this_musicdata <- cbind(this_data$scale, this_data$GSI_question_loop.thisIndex, this_data$questions_english, this_data$questions_german, 
                            this_data$GSI_slider.response, this_data$labels_marks)[which(this_data$scale!=""),] %>% 
      as.data.frame() %>% setNames(c("scale","q_n","question_en","question_de","response","resp_scale")) %>%
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
      mutate(ID = this_demogdata$ID, .before = 1) %>%
      mutate(EXPRA = this_demogdata$EXPRA_code,
             sex = this_demogdata$sex,
             age = this_demogdata$age)
    
    
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
                             wm_data.full[(nrow(wm_data.full)/2+1):nrow(wm_data.full),], matrix(rep(rep("NA", 2),3), ncol = 2, byrow = TRUE)) %>% 
                      as.data.frame() %>%
                      setNames(c("wm_response", "confidence"))
      
      this_data4 <- this_data3 %>% add_column(wm_data.paste, .after = "wm") 
    }
    
    #fctrs <- c("condition","genre","prime_order","question","question_n")
    #numcs <- c("age","sex","confidence","rating","final_eval","knew_piece","wm")
    data <- rbind(data, this_data4) #%>% mutate(across(all_of(fctrs), as.factor),
                                    #           across(all_of(numcs), as.numeric)) 

    #fctrs2 <- c("EXPRA_code","scale","question_en","question_de","resp_scale")
    #numcs2 <- c("response","q_n","age","sex")  
    demog_data <- rbind(demog_data, this_musicdata) #%>% mutate(across(all_of(fctrs2), as.factor),
                                    #                           across(all_of(numcs2), as.numeric))
  }  
  
  data <- data %>% mutate(ID = as.factor(ID))
  demog_data <- demog_data %>% mutate(ID = as.factor(ID))
  
  return(list(data = data, demgraphics = demog_data))
}
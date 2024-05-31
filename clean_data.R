## In this script we clean the data set from the English Lexicon Project

rm(list=ls())

library(tidyverse)



skip_list = c("9999.LDT", "793DATA.LDT", "Data999.LDT", "Data1000.LDT", 
              "Data1010.LDT", "Data1016.LDT")

# read in participant data
ELP_raw_dir <-  "ldt_raw_OSF/"
part_file_list <- tibble(fn = list.files(path = ELP_raw_dir))
part_file_list_filter <- part_file_list %>% filter(!fn %in% skip_list) %>% flatten()


# helper function to generate master list
correct_date <- function(date_in){
  delim <- str_match(date_in,"[:punct:]")
  
  if(!is.na(delim)){
    if(delim =="\\")  delim <- "\\\\"
    split_date <- str_split_1(date_in,delim) 
  } else {
    if(nchar(date_in) == 8) split_date <- str_sub_all(date_in, start = c(1,3,5), end = c(2, 4, 8)) %>% flatten() 
    else if(nchar(date_in) == 6) split_date <- str_sub_all(date_in, start = c(1,3,5), end = c(2, 4, 6)) %>% flatten() 
    else if(nchar(date_in) == 5) split_date <-  str_sub_all(date_in,start = c(1,2,4), end=c(1,3,5)) %>% flatten()
    else return(date_in)
  }
  return(paste(split_date %>% pad_date(), collapse='-' ))
}

pad_date <- function(split_date){
  for(i in 1:2){
    if(nchar(split_date[[i]]) == 1) split_date[[i]] <- paste0("0",split_date[[i]])
  }
  if(nchar(split_date[[3]]) == 2) split_date[[3]] <- paste0("19",split_date[[3]])
  return(split_date)
}

correct_date_vec <- Vectorize(correct_date, vectorize.args = "date_in")

part_data <- function(fn){
  print(paste0("Reading ... ", fn))
  
  trial_data <- read_lines(paste0(ELP_raw_dir, fn))
  
  # Get demographic information from bottom of file. We can work on this later.
  # Using length(trial_data) instead of max(dem_info_index) takes care of files in which the 
  # demographic information has only an opening row of equal signs and not a closing one, see e.g.
  # Data2049.LDT
  
  dem_info_index <- str_which(trial_data, "==")
  dem_info <- trial_data[min(dem_info_index):length(trial_data)]
  
  # Get session information
  session_header_begin <- str_which(trial_data, "Univ")
  N_session_headers <- length(session_header_begin)
  session_header_index <- vector(mode = "integer", length = 2*N_session_headers)
  
  for(i in 1:N_session_headers){
    session_header_index[(2*i-1):(2*i)] = c(session_header_begin[i], session_header_begin[i]+1)
  }
  
  session_info_names = vector(mode = "list", length = N_session_headers)
  session_info_values = vector(mode = "list", length = N_session_headers)
  
  for(i in 1:N_session_headers){
    session_info_names[i] <- trial_data[session_header_begin[i]]  
    session_info_values[i] <- trial_data[session_header_begin[i]+1]
  }
  
  session_info <- tibble(session_info_values) %>% 
    separate_wider_delim(session_info_values, delim = ",", names = session_info_names[[1]] %>% str_split(",") %>% as_vector()) %>% 
    mutate(fn = fn)
  
  session_info <- session_info %>% mutate(DOB = correct_date_vec(DOB))
  
  session_info_unique <- session_info %>% summarise(unique_Univ = unique(Univ) %>% length(),
                                                    unique_Subject = unique(Subject) %>% length(),
                                                    unique_DOB = unique(DOB) %>% length(),
                                                    unique_Education = unique(Education) %>% length())
  
  session_info <- session_info %>% mutate(unique = if_else(session_info_unique %>% filter(if_any(everything(), ~ .x != 1)) %>% nrow() == nrow(session_info_unique), FALSE, TRUE))
  
  RT_data <- trial_data[-c(session_header_index, min(dem_info_index):length(trial_data))] %>% 
    as_tibble() %>% 
    filter(!value == "") %>% 
    separate_wider_delim(value, delim = ",", names = c("trial", "item", "type", "acc", "RT", "word")) %>% 
    mutate(ID = session_info$Subject[1],
           Univ = session_info$Univ[1],
           DOB = session_info$DOB[1],
           Education = session_info$Education[1], .before = trial) %>% 
    mutate(across(c(ID,Univ,DOB, trial,item,type), as.factor), across(c(Education, acc, RT), as.numeric))
  
  return(lst(session_info, RT_data))
}


part_data_list <- map(part_file_list_filter, ~part_data(.x))

RT_data <- map(part_data_list, ~ .x %>% `[[`("RT_data")) %>% 
  bind_rows()

session_info <-  map(part_data_list, ~ .x %>% `[[`("session_info")) %>% 
  bind_rows()

non_unqiue_session_info <- session_info %>% filter(unique == "FALSE")


non_unqiue_summary <- non_unqiue_session_info %>% group_by(Subject) %>% 
  summarise(N_DOB = unique(DOB) %>% length(),
            diff_Education = abs(diff(as.numeric(Education))))

non_unique_count <- non_unqiue_summary %>% count(N_DOB, diff_Education)
print(non_unique_count)


mean_RT <- RT_data %>% 
  filter(type == 1) %>% 
  group_by(word) %>% 
  summarise(I_Mean_RT_trial = mean(RT)) %>% 
  right_join(ELP_original %>% select(Word, I_Mean_RT) %>% filter(!is.na(I_Mean_RT)) %>% rename(I_Mean_RT_mean = I_Mean_RT), by=c("word"="Word")) %>% 
  mutate(diff_RT = I_Mean_RT_mean - I_Mean_RT_trial,
         log_diff_RT = sign(diff_RT)*log10(abs(diff_RT)+1))

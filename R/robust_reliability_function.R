################################################################################
#   Smiles and Masks Lookit study
#   Written by: Michaela DeBolt
#   Last edit: 9/8/21
#   Contact: mdebolt@ucdavis.edu
################################################################################


#' The goal of this code is to stitch together the primary and secondary coders'
#' data and then attach the timing info from the timing_maps.R code in order to 
#' later calculate phase reliability.

library(tidyverse)
library(stringr)
#library(googlesheets4)

# # Testing
# RA1 = "FQ"
# RA2 = "BB"
# child_id = "dRGX3Z"
# i = 1

robust_reliability <- function(RA1 = "RA1", RA2 = "RA2", child_id = "PTB42L") {
  
  #' First, read in the time data that indicated when things happened in the trials.
  #' This file was created using the timing_maps.R file and produced a final dataframe entitled "time_data"
  #' the time_data file indicates (relative to the beginning of the video recording) when the stimuli appeared on the 
  #' screen. This will allow us to determine the "phases" of the trial
  processed_data_folder <- "~/Box/Research/Smiles_and_Masks_LookitSudy_2020/data/processed_data/"
  time_data <- read.csv(paste0(processed_data_folder, "processed_frame_data/", "processed_frame_data_2021-08-31.csv"))
  #dim(time_data) #[1] 4375   10
  
  # Set working directory to the coder's files
  #setwd("/Volumes/General/Backup/CodingProjects/Smiles_and_Masks_LookitStudy/VideosFinishedCoding/")
  setwd("/Users/michaeladebolt/Desktop/datavyu_backup_july/VideosFinishedCoding/")
  # Find the RA's folder
  RA1_files <- list.files(pattern = RA1)
  RA2_files <- list.files(pattern = RA2)
  
  # List all the files that correspond to the child_id parameter
  RA1_coded_files <- data.frame(files = list.files(paste0(RA1_files,"/", child_id), pattern = ".txt") )
  RA2_coded_files <- data.frame(files = list.files(paste0(RA2_files,"/", child_id), pattern = ".txt") )
  
  # Remove initials from the file names so they are identical between coders:
  RA1_coded_files$files <- gsub(pattern = paste0("_",RA1), x = RA1_coded_files$files, replacement = "")
  RA2_coded_files$files <- gsub(pattern = paste0("_",RA2), x = RA2_coded_files$files, replacement = "")
  
  # Remove the trials from the 1st coder that were not coded by the 2nd coder:
  RA1_coded_files <- RA1_coded_files %>% filter(files %in% RA2_coded_files$files)
  
  # Add the coder's initials back to the file names, or else this throws an error later when reading in data
  RA1_coded_files$files <- paste0(str_sub(RA1_coded_files$files,1, -5), "_", RA1, ".txt")
  RA2_coded_files$files <- paste0(str_sub(RA2_coded_files$files,1, -5), "_", RA2, ".txt")
  
  # Change these dataframes back into lists to be used in the loops below:
  RA1_coded_files <-unlist(as.list(RA1_coded_files$files))
  RA2_coded_files <-unlist(as.list(RA2_coded_files$files))
  
  # List all of the files for the child_id and for both RA's combined
  #all_files <- c(RA1_coded_files, RA2_coded_files)
  data_list_RA1 <- list()
  data_list_RA2 <- list()
  # Read in all of the files for the child_id and both RA's data
  if( length(RA1_coded_files) == length(RA2_coded_files) ) {
    for(i in 1:length(RA1_coded_files)) { # If the lengths are the same, you can just use coder 1 length to loop
      # RA1
      data_list_RA1[[i]] <- read.csv(paste0(RA1_files,"/", child_id,"/", RA1_coded_files[i]), na.strings = "", sep = "]")
      # RA2
      data_list_RA2[[i]] <- read.csv(paste0(RA2_files,"/", child_id,"/", RA2_coded_files[i]), na.strings = "", sep = "]")
      
      dim(data_list_RA1[[i]])
      dim(data_list_RA2[[i]])
      # Check to see if the RA's trials have the same number of frames. If not, then extend the length of the
      # shorter trial to be as long as the longer trial in both the frame number AND the time column
      if( dim(data_list_RA1[[i]])[1] != dim(data_list_RA2[[i]])[1] ){
        # Compare the dimensions between the two coded files. If they are different, do the following things 
        # below. If the length of the files are the same, this portion gets skipped. 
        
        max_nFrames <- max(data_list_RA1[[i]]$framenum, data_list_RA2[[i]]$framenum) # compare the max number of frames to get the larger value
        max_time <- max(data_list_RA1[[i]]$time, data_list_RA2[[i]]$time, na.rm = T) # compare the max time to get the larger value
        time_increment <- data_list_RA1[[i]][2,]$time # The time variable should increment similarly between the two files
        # so it doesn't matter which file you use to determine how time increments.
        
        # Make the frame number amd the time column equal between the two files
        data_list_RA1[[i]] <- merge(data.frame(framenum = seq(1:max_nFrames),
                                               time = seq(0, max_time, by = time_increment)), 
                                    data_list_RA1[[i]], by = c("framenum", "time"), all.x = TRUE)
        
        
        data_list_RA2[[i]] <- merge(data.frame(framenum = seq(1:max_nFrames),
                                               time = seq(0, max_time, by = time_increment)), 
                                    data_list_RA2[[i]], by = c("framenum", "time"), all.x = TRUE)
        
        # Make sure both files are sorted by time! 
        data_list_RA1[[i]] <- arrange(data_list_RA1[[i]], time)
        data_list_RA2[[i]] <- arrange(data_list_RA2[[i]], time)
        # Make the time column equal between the two files 
        data_list_RA2[[i]]$time == data_list_RA1[[i]]$time 
        
      } else {
        next() #move on
      }
    }# for
  } else{
    print("RA's don't have the same number of coded trials for this child!")
  }
  
  # Set up a lot of storage lists:
  coder_1_looks.ordinal <- list()
  coder_2_looks.ordinal <- list()
  coder_1_looks.code01 <- list()
  coder_2_looks.code01 <- list()
  trial <- list()
  n_frame <- list()
  current_child_id <- list()
  all_trials <- list()
  reliability <- list()
  phase_reliability <- list()
  time <- list()
  newColNames <- c("framenum", "time","child_id_child_id",
                   "test_date_date" ,"trial_trial_name",
                   "coder_1_initials_coder_1_initials",
                   "coder_1_looks_ordinal",
                   "coder_1_looks_coder_1_looks",
                   "coder_1_notes_coder_1_notes",
                   "tracks___fps",	"tracks___file")
  
  # Combine each trial for each RA:
  for(i in 1:length(data_list_RA1)) {
    
    # Check the column names of coder 1's data to see if it contains the string "code01"
    ifelse(sum(grepl("code01", colnames(data_list_RA1[[i]]))) > 0,
           colnames(data_list_RA1[[i]]) <- newColNames, colnames(data_list_RA1[[i]]) <- colnames(data_list_RA1[[i]]))
    # Check the column names of coder 2's data to see if it contains the string "code01"
    ifelse(sum(grepl("code01", colnames(data_list_RA2[[i]]))) > 0,
           colnames(data_list_RA2[[i]]) <- newColNames, colnames(data_list_RA2[[i]]) <- colnames(data_list_RA2[[i]]))
    
    ## Need to fix the problem of having two time columns!!!
    
    coder_1_looks.ordinal[[i]] <- data_list_RA1[[i]]$coder_1_looks_ordinal #Assign coder 1 looks
    coder_2_looks.ordinal[[i]] <- data_list_RA2[[i]]$coder_1_looks_ordinal #Assign coder 2 looks
    
    coder_1_looks.code01[[i]] <- data_list_RA1[[i]]$coder_1_looks_coder_1_looks #Assign coder 1 looks
    coder_2_looks.code01[[i]] <- data_list_RA2[[i]]$coder_1_looks_coder_1_looks #Assign coder 2 looks
    
    trial[[i]] <- data_list_RA1[[i]]$trial_trial_name #extract current trial name
    n_frame[[i]] <- data_list_RA1[[i]]$framenum # extract the frame number information
    current_child_id[[i]] <- data_list_RA1[[i]]$child_id_child_id # extract the current child_id
    time[[i]] <- data_list_RA1[[i]]$time # extract the current time column from RA1 -- the time column between the two 
    # RAs might not be equivalent at this point if they differ! Just FYI!!! This will be a point to fix. 
    
    temp <- data.frame(bind_cols( coder_1_looks.ordinal = coder_1_looks.ordinal[[i]],
                                  coder_1_looks.code01 = coder_1_looks.code01[[i]],
                                  coder_2_looks.ordinal = coder_2_looks.ordinal[[i]],
                                  coder_2_looks.code01 = coder_2_looks.code01[[i]],
                                  trial = trial[[i]], 
                                  n_frame = n_frame[[i]],
                                  child_id = current_child_id[[i]], 
                                  time = time[[i]]))
    
    all_trials[[i]] <- temp
    ## To do: need to add the notes to this, basically everything you would want/need for the final data processing step
  }
  
  for(i in 1:length(all_trials)) { # For each trial there was reliability data for:
    data <- all_trials[[i]]
    # Mark instances of looking off screen/not looking/cells that were simply not coded
    data$coder_1_looks.ordinal <- tidyr::replace_na(data = data$coder_1_looks.ordinal, replace = "offscreen")
    data$coder_1_looks.code01 <- tidyr::replace_na(data = as.character(data$coder_1_looks.code01), replace = "offscreen")
    
    data$coder_2_looks.ordinal <- tidyr::replace_na(data = data$coder_2_looks.ordinal, replace = "offscreen")
    data$coder_2_looks.code01 <- tidyr::replace_na(data = as.character(data$coder_2_looks.code01), replace = "offscreen")
    
    # Indicate agreement for each frame
    data$agree <- ifelse(data$coder_1_looks.code01 == data$coder_2_looks.code01, 1,0)
    
    ## Add the timing information at this stage:
    current_id <- data[1,]$child_id # assign the current child id to a variable
    current_trial_name <- data[1,]$trial # assign the current trial name to a variable
    # Filter the time_data for the current_id variable
    current_id_time_data <- time_data %>% filter(child_hashed_id == current_id & frame_id == current_trial_name) 
    
    ## Attaching the timing information differently depending on what type of trial and/or study this is:
    #* note that the only type of trial that needs to be offset by the starting time point is the memory test trial because this is the only
    #* instance in which a video plays; all other trials indicate when things happened in the frame data (including the memory and pref calibration trials)
    if(current_id_time_data[1,]$study == "pref" & !grepl(pattern = "cal", ignore.case = T, x = current_id_time_data[1,]$frame_id) ) {
      # If the current trial is from the pref study and it is NOT a calibration trial:
      data$phase <- ifelse(data$time >= current_id_time_data$startStreamTime*1000, "2.test", "1.AG")
      
    }else if(current_id_time_data[1,]$study == "pref" & grepl(pattern = "cal", ignore.case = T, x = current_id_time_data[1,]$frame_id) ) {
      # If the current trial is from the pref study and is IS a calibration trial:
      data$phase <- ifelse(data$time < current_id_time_data[1,]$startStreamTime*1000, "1.AG",
                           ifelse(data$time >= current_id_time_data[1,]$startStreamTime*1000 & data$time < current_id_time_data[2,]$startStreamTime*1000, "2.center_1",
                                  ifelse(data$time >= current_id_time_data[2,]$startStreamTime*1000 & data$time < current_id_time_data[3,]$startStreamTime*1000, "3.left", 
                                         ifelse(data$time >= current_id_time_data[3,]$startStreamTime*1000 & data$time < current_id_time_data[4,]$startStreamTime*1000, "4.right", "5.center_2"))))
      
    } else if(current_id_time_data[1,]$study == "mem" & !grepl(pattern = "cal", ignore.case = T, x = current_id_time_data[1,]$frame_id) ) {
      # If the current trial is the from the memory study and is NOT a calibration trial 
      data$phase <- ifelse(data$time < current_id_time_data[1,]$startStreamTime*1000, "0.video_not_started",
                           ifelse(data$time >= current_id_time_data[1,]$startStreamTime*1000 & data$time < current_id_time_data[1,]$startStreamTime*1000 + (6*1000), "1.AG",
                                  ifelse(data$time >= current_id_time_data[1,]$startStreamTime*1000 + (6*1000) & data$time < current_id_time_data[1,]$startStreamTime*1000 + (18*1000), "2.fam", 
                                         ifelse(data$time >= current_id_time_data[1,]$startStreamTime*1000 + (18*1000) & data$time < current_id_time_data[1,]$startStreamTime*1000 + (19.5*1000), "3.AG_1",
                                                ifelse(data$time >= current_id_time_data[1,]$startStreamTime*1000 + (19.5*1000)  & data$time < current_id_time_data[1,]$startStreamTime*1000 + (24.5*1000), "4.test_1",
                                                       ifelse(data$time >= current_id_time_data[1,]$startStreamTime*1000 + (24.5*1000)  & data$time < current_id_time_data[1,]$startStreamTime*1000 + (26*1000), "5.AG_2", "6.test_2"))))))
      
    } else {
      # If the current trial is from the memory study and IS a calibration trial
      data$phase <- ifelse(data$time < current_id_time_data[1,]$startStreamTime*1000, "0.video_not_started",
                           ifelse(data$time >= current_id_time_data[1,]$startStreamTime*1000 & data$time < current_id_time_data[2,]$startStreamTime*1000, "1.center_1",
                                  ifelse(data$time >= current_id_time_data[2,]$startStreamTime*1000 & data$time < current_id_time_data[3,]$startStreamTime*1000, "2.left", 
                                         ifelse(data$time >= current_id_time_data[3,]$startStreamTime*1000 & data$time < current_id_time_data[4,]$startStreamTime*1000, "3.right", "4.center_2"))))
    }
    
    # Calculate reliability for this trial for each phase and store in a dataframe:
    phase_r <- data.frame( 
      data %>% group_by(phase) %>% 
        add_tally(name = "n_phase_frames") %>% 
        summarise(raw_agreement = sum(agree)/max((n_phase_frames))*100 ),
      coder1 = RA1, coder2 = RA2,
      trial = data$trial[1])
    
    # Calculate reliability collapsed across the entire trial and store in a dataframe
    r <- data.frame(phase = "whole_trial",
                    raw_agreement = sum(data$agree)/max(data$n_frame)*100, 
                    coder1 = RA1, 
                    coder2 = RA2,
                    trial = data$trial[1])
    
    reliability[[i]] <- r
    phase_reliability[[i]] <- phase_r
    ## Print the stitched data file somewhere for the code verification process:
    write.csv(data, paste0(processed_data_folder, "stitched_exported_data/", current_id, "_", current_trial_name, "_stitched.csv"), row.names = T)
  }
  
  # Bind the reliability data in order to print
  final_reliability <- bind_rows(reliability)
  final_phase_reliability <- bind_rows(phase_reliability)
  return(list(phase_results = final_phase_reliability, overall_results = final_reliability) )
  
} # outer-most loop



#robust_reliability(RA1 = "JL", RA2 = "VP", child_id = "cAcREF")







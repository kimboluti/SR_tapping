
# Load functions 

#install.packages("dplyr", "stringr", "tidyr", "zipcode")



get_files = function(data, file_type, dir){
    
   current <- getwd()    
   setwd(dir)
    
   paths <- grep(file_type, data, value = TRUE) 
   
           if(file_type == ".csv"){
               read.file = function(x) read.csv(x, header = FALSE)
               
           }else{
               if(file_type == ".survey"){
               read.file = function(x){tryCatch(read.table(x, header = FALSE),
                                    error = function(cond){
                                        message(cond)
                                        return("NA")
                                    }
                                )
                                    }
                }
            }
          
   taps <- lapply(paths, read.file)
   names(taps) <- sub(file_type, "", paths)
 
   setwd(current)
   return(taps)
   
}



get_cond = function(data){
    
    cond <- as.data.frame(unique(sub(".csv|.txt|.survey","", data)))
    names(cond) <- "filename"
    
    cond <- cbind(cond, separate(data = cond, col = filename,
             into = c("seed", "id", "sourceId", "mode", "type", "chain"),
             sep = "-"))
    
    cond <- cond %>% 
        mutate(rejects = ifelse(str_detect(seed, "r"), "r", "n")) %>% 
        mutate(seed = str_replace(seed, "r", "")) %>% 
        mutate(chain_name = paste0(seed, mode, type, sep = "")) %>% 
        mutate(starter = ifelse(seed == sourceId, "1", "0")) %>% 
        mutate(filename = as.character(filename)) %>% 
        mutate(chain = as.numeric(chain))
    
    
    conditions <- unique(cond$chain_name)
    full_data <- data.frame()
 
    
    # improve this nested for loop by turning the last two loops into functions 
    
    for(i in 1: length(conditions)){ 
        
        starters <- cond %>% 
            filter(sourceId == seed, chain_name == conditions[i])
        
        parts <- cond %>% 
            filter(chain_name == conditions[i])
            
        # people whose source = starter are first person in chain
        start <- starters$id
        
        for(j in 1: length(start)){
            # pull out 
            chain <- starters %>% 
                filter(id == start[j]) %>% 
                select(chain_name, id) %>% 
                mutate(chain_id = paste(id, chain_name, sep = "-")) 
            
            chain_id <- unique(chain$chain_id)
            
            startID = start[j]
            
            # assign the first person in the chain w/ the unique chain ID
            
            parts$chain_name[parts$id == start[j] & parts$seed == parts$sourceId] <- chain_id
            
            # assign the rest of the people in the same chain with the same unique chain ID
            for(k in 1:20){
                startID <- parts[parts$sourceId %in% startID, "id"] 
                parts$chain_name[parts$id %in% startID] <- chain_id
            } 
        }
        
        full_data <- rbind(parts, full_data)
        
    }

    full_data <- full_data %>% 
        mutate(sourceId = ifelse(nchar(sourceId) == 1, paste0(sourceId, type), sourceId))
    
    return(full_data)
}




clean_survey = function(survey){
    
    require("zipcode")
    data(zipcode)
    
    temp <- do.call(rbind, lapply(lapply(survey, `[[`, 1), data.frame,
                                  stringsAsFactors = FALSE))
    names(temp) <- "survey"
    
    temp <- separate(data = temp, col = survey,
                                 into = c("age", "gender", "zip", "music", "comments"),
                                 sep = ",")

    # recode 
    musicalCodes <- c("Nonmusician",
                      "Music-loving nonmusician", 
                      "Amateur musician",
                      "Serious amateur musician", 
                      "Semiprofessional musician", 
                      "Professional musician")
    
    musicalCodes2 <- c(musicalCodes, "NR")
    
    music <- sort(as.numeric(unique(temp$music)))
    names(musicalCodes) <- music
    
    ageCodes <- c("17 or below", "18-24", "25-34", "35-44", "45-54", "55-64", "over 65")
    ageCodes2 <- c(ageCodes, "NR")
    
    genderCodes <- c("Male", "Female", "NR")
    
    age_r <- sort(as.numeric(unique(temp$age)))
    names(ageCodes) <- age_r
     
    gender <- sort(as.numeric(unique(temp$gender)))
    names(genderCodes) <- gender
 

    temp <- temp %>% 
        mutate(filename = rownames(temp)) %>% 
        mutate(age = str_replace_all(age, ageCodes)) %>% 
        mutate(gender = str_replace_all(gender, genderCodes)) %>% 
        mutate(zip = ifelse(zip %in% zipcode$zip, zip, "NR")) %>% 
        mutate(music = str_replace_all(music, musicalCodes)) %>% 
        select(filename, age, gender, zip, music) %>% 
        mutate_each(funs(ifelse(. == "", "NR", .))) %>% 
        mutate_each(funs(as.factor)) %>% 
        mutate(filename = as.character(filename)) %>% 
        mutate(music = factor(music, levels = musicalCodes2)) %>% 
        mutate(age = factor(age, levels = ageCodes2)) %>% 
        left_join(zipcode, "zip")
        
    return(temp)
    
}  


clean_taps = function(taps, row){
    
    require(plyr)
    require(dplyr)
    
    temp <- do.call(rbind.fill,
                    lapply(lapply(taps, '[', i = row, j = TRUE),
                    data.frame, stringsAsFactors = FALSE))
    
    temp <- cbind(as.data.frame(names(taps)), temp)
    
    long_temp <- gather(temp, tap_number, tap_int, V1:V28) 
    
    names(long_temp)[1] <- "filename"
   
    long_temp <- long_temp %>% 
        filter(!is.na(tap_int)) %>% 
        mutate(tap_number = str_replace(tap_number, "V", "")) %>% 
        mutate(tap_number = as.integer(tap_number)) %>% 
        mutate(filename = as.character(filename))
        
    detach("package:plyr", unload=TRUE)
    
  
    return(long_temp)
}










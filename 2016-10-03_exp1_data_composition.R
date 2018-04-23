# Data composition script for artprize serial reproduction (SR) data
# Exp 1 of Fromboluti dissertation
#
# Composes data from artprize raw data:
# (1) taps: sr tap intervals from second trial of SR task 
# (2) sr prac: sr tap intervals from first (practice) trial of SR task
# (3) smt: tap intervals from spontaneous motor tempo (~practice tapping) trial
#
# Also outputs summaries with mean, median, sd, median absolute deviation, cov, comv for each data type
# Selects only data from AUDITORY TEMPO condition
#

# output names for compiled data ---------------------------------------

  taps_output <- "compiled data/taps_sr_2016-10-03.csv"
  taps_sr_prac_output <- "compiled data/taps_sr_prac_2016-10-03.csv"
  taps_smt_output <- "compiled data/taps_smt_2016-10-03.csv"
  
  taps_summary_output <- "compiled data/summary_sr_2016-10-17.csv"
  taps_sr_prac_summary_output <- "compiled data/summary_sr_prac_2016-10-17.csv"
  taps_smt_summary_output <- "compiled data/summary_smt_2016-10-17.csv"

  demo_output <- "compiled data/demographics_2016-10-14.csv"

# Set directory, get original directory info -----------------------------
    #setwd("C:/Users/Ellie/Google Drive/experiments & projects/iterated tapping - tempo/ANALYSIS - 10-2016 (diss)/Experiment 1 (Artprize)")
    original_directory <- getwd()   
# Load libraries -----------------------------
    #library(zipcode)
    library(stringr)
    library(tidyr)
    library(ggplot2)
    #library(devtools)
    library(plyr)
    
    library(dplyr) # load dplyr after plyr 
    #install_github('ggmap','dkahle')
    #library(ggmap)
# source functions -----------------------------
    source("2016-04-11_functions.R")
        
# get seeds -----------------------------    
    stimuli <- read.csv("seeds.csv")
    
# get names of files -----------------------------  
    dat <- list.files("raw data")   
    
# load the taps and survey------------------------------     
    taps <- get_files(dat, ".csv", "raw data")
    taps_smt <- clean_taps(taps, 1) # load the 1st row 
    taps_sr_prac <- clean_taps(taps, 2) # load the 2nd row 
    taps <- clean_taps(taps, 3) # load the 3rd row 
    
    # need to modify T's functions to read in timeseries data
    # timeseries <- get_files(dat, ".txt", "raw data")
        
    survey <- get_files(dat, ".survey", "raw data")
    survey <- clean_survey(survey) 
    
# get unique participant info-------------------------- 
    ids <- get_cond(dat)
    

# Compiled raw tap interval data---------------------------------------------------------- 
# TAPS: compile data frames for auditory tempo ------------------------------     

    # get source taps for produced taps
    # create the source id file 
    id_taps <- taps %>% 
      left_join(ids, by = "filename") %>% 
      select(id, tap_number, tap_int) %>% 
      full_join(stimuli, by = c("id", "tap_number", "tap_int")) %>% 
      rename(source_tap_int = tap_int) 
    
    # join id file with source taps 
    taps_full <- taps %>% 
      left_join(ids, by = "filename") %>% 
      select(sourceId, tap_number, tap_int, filename) %>% 
      rename(id = sourceId) %>% 
      left_join(id_taps, by = c("id", "tap_number")) %>% 
      select(filename, tap_number, tap_int, source_tap_int) %>% 
      mutate(tap_diff = source_tap_int - tap_int) %>% 
      mutate(abs_tap_diff = abs(source_tap_int - tap_int))
    
    # add id info & filter by auditory tempo. INCLUDES REJECTS (for inspection)
    taps_full_at <- taps_full %>%
      inner_join(ids, by = "filename") %>%
      filter(mode == "a" & type == "t") %>%
      rename(generation = chain) %>%
      select(mode, type, seed, chain_name,
             filename, rejects, starter, id, sourceId, generation,
             tap_number, tap_int, source_tap_int, tap_diff, abs_tap_diff) %>%
      arrange(chain_name, generation, tap_number)
      
    # write to csv for further processing
    write.csv(taps_full_at, taps_output)
      
# SR PRACTICE TAPS: compile data frames for auditory tempo ------------------------------     
  
  # get source taps for produced taps 
  # create the source id file 
  id_taps <- taps_sr_prac %>% 
    left_join(ids, by = "filename") %>% 
    select(id, tap_number, tap_int) %>% 
    full_join(stimuli, by = c("id", "tap_number", "tap_int")) %>% 
    rename(source_tap_int = tap_int) 
  
  # join id file with source taps 
  taps_full_sr_prac <- taps_sr_prac %>% 
    left_join(ids, by = "filename") %>% 
    select(sourceId, tap_number, tap_int, filename) %>% 
    rename(id = sourceId) %>% 
    left_join(id_taps, by = c("id", "tap_number")) %>% 
    select(filename, tap_number, tap_int, source_tap_int) %>% 
    mutate(tap_diff = source_tap_int - tap_int) %>% 
    mutate(abs_tap_diff = abs(source_tap_int - tap_int))
  
  # add id info & filter by auditory tempo. INCLUDES REJECTS (for inspection)
  taps_full_sr_prac_at <- taps_full_sr_prac %>%
    inner_join(ids, by = "filename") %>%
    filter(mode == "a" & type == "t") %>%
    rename(generation = chain) %>%
    select(mode, type, seed, chain_name,
           filename, rejects, starter, id, sourceId, generation,
           tap_number, tap_int, source_tap_int, tap_diff, abs_tap_diff) %>%
    arrange(chain_name, generation, tap_number)
  
  # write to csv for further processing
  write.csv(taps_full_sr_prac_at, taps_sr_prac_output)
  
# SMT TAPS: compile data frames for auditory tempo ------------------------------     
  
  # new df for number of reps (from tap_number == 1 in raw data) 
  # to be added back to smt data as a  separate column
  reps <- taps_smt %>%
    filter(tap_number == 1) %>%
    rename(n_reps = tap_int) %>%
    select(filename, n_reps)
    
  # renumber tap intervals, add reps column, join with id info & filter by aud tempo
  taps_smt_at <- taps_smt %>%
    filter(tap_number !=1) %>%
    full_join(reps, by = "filename") %>%
    mutate(tap_number = tap_number - 1) %>%
    #arrange(filename, tap_number) %>%
    inner_join(ids, by = "filename") %>%
    filter(mode == "a" & type == "t") %>%
    rename(generation = chain) %>%
    select(mode, type, seed, chain_name,
           filename, rejects, starter, id, sourceId, generation, n_reps,
           tap_number, tap_int) %>%
    arrange(chain_name, generation, tap_number)
  
  # write to csv for further processing
  write.csv(taps_smt_at, taps_smt_output)    
  

# Summary stats---------------------------------------------------------------------------
# TAPS: calculate mean, median, std, cov by individual ----------------------------------------
    taps_summary_at <- taps_full_at %>% 
        group_by(filename) %>% 
        summarize(mean_tempo = mean(tap_int),
                  sd_taps = sd(tap_int),
                  median_tempo = median(tap_int), 
                  mad_taps = median(abs(tap_int - median_tempo))) %>%            
        mutate(cov = sd_taps/mean_tempo,
               comv = mad_taps/median_tempo) %>%
        inner_join(ids, by = "filename") %>%  
        filter(rejects == "n") %>% 
        rename(generation = chain) %>%
        select(mode, type, seed, chain_name,
               filename, starter, id, sourceId, generation,
               mean_tempo, sd_taps, cov,
               median_tempo, mad_taps, comv) %>%
        arrange(chain_name, generation)
    
    # write to csv for further processing
    write.csv(taps_summary_at, taps_summary_output)

          # some plot checks
          
          # spaghetti plots            
          ggplot(taps_summary_at, aes(x = generation, y = mean_tempo, group = chain_name, color = seed)) +
              geom_point() + 
              geom_line() +
              facet_grid(type~mode) + 
              theme_bw()
              
          ggplot(tap_summary, aes(x = generation, y = median_tempo, group = chain_name, color = seed)) +
            geom_point() + 
            geom_line() +
            facet_grid(type~mode) + 
            theme_bw()

          ggplot(taps_summary_at, aes(x = generation, y = sd_taps, group = chain_name, color = seed)) +
            geom_point() + 
            geom_line() +
            facet_grid(type~mode) + 
            theme_bw()
          
          # curious: quick calc cov as sd / median:
          ggplot(taps_summary_at, aes(x = generation, group = chain_name, color = seed)) +
            geom_point(aes(y = sd_taps/median_tempo), na.rm = TRUE) +
            geom_line(aes(y = sd_taps/median_tempo), na.rm = TRUE)
  

          ggplot(taps_summary_at, aes(x = generation, y = cov, group = chain_name, color = seed)) +
            geom_point() + 
            geom_line() +
            facet_grid(seed~.) + 
            theme_bw()
          
          ggplot(taps_summary_at, aes(x = generation, y = comv, group = chain_name, color = seed)) +
            geom_point() + 
            geom_line() +
            facet_grid(type~mode) + 
            theme_bw()
  
    # further summarizing by mode, type, and chain
    # currently this collapses across generations though:
    # need to look at just final
    tap_summary_table <- taps_summary_at %>%
      group_by(chain_name, mode, type, seed) %>%
      summarize_each(funs(mean), mean_tempo, sd_taps, cov, median_tempo, mad_taps, comv)
      
         
    # just first and final for each chain
    first_and_final_summary <- taps_summary_at %>%
      group_by(chain_name) %>%
      mutate(max_gen = max(generation)) %>%
      filter(starter == 1 | generation == max_gen) #%>%
      #separate(cov,starter,c(cov_first,cov_final)) # working here - or spread??
      # add cov_drift = final - first
    
    # oops no longer have cov_drift in first_and_final
    ggplot(first_and_final_summary, aes(x = seed, y = cov_drift, group = seed, color = seed)) +
      geom_point() + 
      #geom_line() +
      theme_bw()

    
# SR PRACTICE TAPS: calculate mean, median, std, cov by individual----------------------------------------
    taps_summary_sr_prac_at <- taps_full_sr_prac_at %>% 
      group_by(filename) %>% 
      summarize(mean_tempo = mean(tap_int),
                sd_taps = sd(tap_int),
                median_tempo = median(tap_int), 
                mad_taps = median(abs(tap_int - median_tempo))) %>%            
      mutate(cov = sd_taps/mean_tempo,
             comv = mad_taps/median_tempo) %>%
      inner_join(ids, by = "filename") %>%  
      filter(rejects == "n") %>% 
      rename(generation = chain) %>%
      select(mode, type, seed, chain_name,
             filename, starter, id, sourceId, generation,
             mean_tempo, sd_taps, cov,
             median_tempo, mad_taps, comv) %>%
      arrange(chain_name, generation)

    # write to csv for further processing
    write.csv(taps_summary_sr_prac_at, taps_sr_prac_summary_output)
      
      # some plot checks
      
      # spaghetti plots            
      ggplot(taps_summary_sr_prac_at, aes(x = generation, y = mean_tempo, group = chain_name, color = seed)) +
        geom_point() + 
        geom_line() +
        facet_grid(type~mode) + 
        theme_bw()
      
      ggplot(taps_summary_sr_prac_at, aes(x = generation, y = median_tempo, group = chain_name, color = seed)) +
        geom_point() + 
        geom_line() +
        facet_grid(type~mode) + 
        theme_bw()
      
      ggplot(taps_summary_sr_prac_at, aes(x = generation, y = cov, group = chain_name, color = seed)) +
        geom_point() + 
        geom_line() +
        facet_grid(type~mode) + 
        theme_bw()
      
      ggplot(taps_summary_sr_prac_at, aes(x = generation, y = comv, group = chain_name, color = seed)) +
        geom_point() + 
        geom_line() +
        facet_grid(type~mode) + 
        theme_bw()
# SMT TAPS: calculate mean, median, std, cov by individual----------------------------------------
    taps_summary_smt_at <- taps_smt_at %>% 
      group_by(filename) %>% 
      summarize(mean_tempo = mean(tap_int),
                sd_taps = sd(tap_int),
                median_tempo = median(tap_int), 
                mad_taps = median(abs(tap_int - median_tempo))) %>%
      mutate(cov = sd_taps/mean_tempo,
             comv = mad_taps/median_tempo) %>%
      inner_join(ids, by = "filename") %>%  
      filter(rejects == "n") %>% 
      rename(generation = chain) %>%
      select(mode, type, seed, chain_name,
             filename, starter, id, sourceId, generation,
             mean_tempo, sd_taps, cov,
             median_tempo, mad_taps, comv) %>%
      arrange(chain_name, generation)
    
    # write to csv for further processing
    write.csv(taps_summary_smt_at, taps_smt_summary_output)

      # some plot checks
      
      # spaghetti plots            
      ggplot(taps_summary_smt_at, aes(x = generation, y = mean_tempo, group = chain_name, color = seed)) +
        geom_point() + 
        geom_line() +
        facet_grid(type~mode) + 
        theme_bw()
      
      ggplot(taps_summary_smt_at, aes(x = generation, y = median_tempo, group = chain_name, color = seed)) +
        geom_point() + 
        geom_line() +
        facet_grid(type~mode) + 
        theme_bw()
      
      ggplot(taps_summary_smt_at, aes(x = generation, y = cov, group = chain_name, color = seed)) +
        geom_point() + 
        geom_line() +
        facet_grid(type~mode) + 
        theme_bw()

      # curiosity - quick calc cv as sd/median since smt means all over the place
      ggplot(taps_summary_smt_at, aes(x = generation, group = chain_name, color = seed)) +
        geom_point(aes(y = sd_taps/median_tempo), na.rm = TRUE) +
        geom_line(aes(y = sd_taps/median_tempo), na.rm = TRUE)
      # nah just need to filter outliers and recalculate sd to make it useful/meaningful

      ggplot(taps_summary_smt_at, aes(x = generation, y = comv, group = chain_name, color = seed)) +
        geom_point() + 
        geom_line() +
        facet_grid(type~mode) + 
        theme_bw()
    
    
        
    
 

# NOT UPDATED OCT 2016: other checks / analysis from proposal -------------------------------
# accuracy 
    # how well did people reproduce the mean target interval 
    # calculate avg tap rate, source rate, and diff by participant
    acc_summary <- taps_full %>% 
        group_by(filename) %>% 
        summarize_each(funs(mean), tap_int, source_tap_int, abs_tap_diff) %>% 
        left_join(ids, by = "filename") %>% 
        filter(rejects == "n") %>% 
        mutate(chain = as.numeric(chain))
    
    # plot avg tap difference by condition and mode   
    ggplot(acc_summary, aes(x = chain, y = abs_tap_diff, group = chain_name, color = seed)) +
        geom_point() + 
        geom_line() +
        facet_grid(type~mode) + 
        theme_bw()
    
    # how well did people reproduce the target interval? 
    # mean-square error between tap and target intervals
    taps_acc <- taps_full %>%
      group_by(filename) %>%
      mutate(tap_sq_error = tap_diff^2) %>%
      summarize(mse = mean(tap_sq_error)) %>%
      left_join(ids, by = "filename") %>%
      filter(rejects == "n") %>%
      mutate(chain = as.numeric(chain))     
    
    ggplot(taps_acc, aes(x = chain, y = mse, group = chain_name, color = seed)) +
      geom_point() + 
      facet_grid(type~mode) + 
      theme_bw()
    
    taps_acc_at <- taps_full %>%
      group_by(filename) %>%
      mutate(tap_sq_error = tap_diff^2) %>%
      summarize(mse = mean(tap_sq_error)) %>%
      left_join(ids, by = "filename") %>%
      filter(rejects == "n", mode == "a", type == "t") %>%
      mutate(chain = as.numeric(chain))     
    
    ggplot(taps_acc_at, aes(x = chain, y = mse, group = chain_name, color = seed)) +
      geom_point() + 
      geom_line() +
      facet_grid(.~seed)+
      theme_bw()
    
    
    
        
    
    
    
    
    
    
# drift # EKF added script - working
    # final chain - seed 
    # across the entire chain - how far away did they drift from seed 
    # calculate avg tap rate, source rate, and diff by participant
    drift_summary <- taps_full %>% 
      group_by(filename) %>% 
      summarize_each(funs(mean), tap_int, source_tap_int, abs_tap_diff) %>% 
      left_join(ids, by = "filename") %>% 
      filter(rejects == "n") %>% 
      mutate(chain = as.numeric(chain))
    
    # plot avg tap difference by condition and mode   
    ggplot(drift_summary, aes(x = chain, y = abs_tap_diff, group = chain_name, color = seed)) +
      geom_point() + 
      geom_line() +
      facet_grid(type~mode) + 
      theme_bw()
    

# meter taps - distribution plots (15 chains - 12a, 3v)
    taps_meter <- taps %>% 
      left_join(ids, by = "filename") %>% 
      filter(type == 'm', tap_int < 3000) %>%
      select(mode, seed, chain_name, chain, tap_number, tap_int) %>%
      
      
    #timeseries
    ggplot(taps_meter, aes(x = tap_number, y = tap_int, color = chain_name)) +
      geom_point() + 
      geom_line() +
      facet_grid(seed~mode) + 
      theme_bw()
    
    #density or histogram plots separate by chains
    ggplot(taps_meter, aes(x = tap_int, color = chain_name)) +
      #geom_bar(stat = "count")
      #geom_density() + 
      geom_histogram(binwidth = 50)
      #scale_x_sqrt() +
      #scale_y_sqrt() +
      #coord_polar(theta = 'x', direction = 1)
      facet_grid(seed~mode) +
      theme_bw()
    
    #density plots average over chains
    ggplot(taps_meter, aes(x = tap_int, color = tap_number)) +
      #geom_bar(stat = "count")
      geom_density() + 
      scale_x_sqrt() +
      scale_y_sqrt() +
      #coord_polar(theta = 'x', direction = 1)
      facet_grid(seed~mode) +
      theme_bw()
    
    #density - one chain - show each generation
    
    
    
    
    
    
# metric complexity measure
    # ratio of n to n+1
    
  taps_temp  <- taps_full %>% 
      left_join(ids, by = "filename") %>%
      arrange(filename, tap_number) %>%
      unite(condition,seed,mode,type, sep = "") %>%
      filter(condition == "0am") %>%
      mutate(id = as.numeric(id)) %>%
      mutate(group = NA) %>%
      mutate(group = ifelse(tap_number %in% c(1:4), 1, group)) %>%
      mutate(group = ifelse(tap_number %in% c(5:6), 2, group)) %>%
      mutate(group = ifelse(tap_number %in% c(7:8), 3, group)) %>%
      mutate(group = ifelse(tap_number %in% c(9:12), 4, group)) %>%
      mutate(group = ifelse(tap_number %in% c(13:14), 5, group)) %>%
      mutate(group = ifelse(tap_number %in% c(15), 6, group)) %>%
      group_by(id, group, condition) %>%
      summarize(sums = sum(tap_int)) %>%
      filter(id ==1133 ) #remove for all Ps
     
    #add with a do to do for all participants
    pairs <- combn(taps_temp$sums,2)
    ratios <- pairs[1,]/pairs[2,]
    
    #testing permutations
    a <- do.call(cbind, cominat::permn(unique()))
    apply(taps_temp$sums, 2, function(x) {x[]})
    
    
    
  
      
    
    

# AR(1) process: N+1 against N
    smoothScatter(taps_full$source_tap_int,taps_full$tap_int,xlim = c(0,2500), ylim = c(0,2500))
  
    #full data including a hand-full of extemists
    ggplot(taps_full, aes(x = source_tap_int, y = tap_int)) +
      geom_point() +
      geom_smooth(method=lm)
    
    #trimming data for several extreme tap values
    trim_taps <- taps_full %>%   
      group_by(filename) %>% 
      left_join(ids, by = "filename") %>% 
      filter(!tap_int %in% c(outlier(tap_int))) %>%
      filter(!source_tap_int %in% c(outlier(source_tap_int)))
      
    #this plot isn't qutie what i want...
    ggplot(trim_taps, aes(x = source_tap_int, y = tap_int, group = mode, color = type)) +
      geom_boxplot() + 
      geom_jitter(width = 0.2)
    
    #this is a little better. there are stil outliers
    ggplot(trim_taps, aes(x = source_tap_int, y = tap_int, color = type)) +
      geom_point() +
      facet_grid(type~mode) +
      coord_fixed()
      #geom_smooth(method=lm)
    
    
# AR(1) bias: [(N+1) - N] against N
    

# inspect participant info ----------------------------------     

# participant counts; about 7% of people were rejected 
ids %>% 
  group_by(rejects) %>% 
  summarize(n=n())

# chain counts 
chain_counts <- ids %>% 
  group_by(chain_name, rejects, seed, mode, type) %>% 
  summarize(n = n()) %>% 
  spread(rejects, n, fill = 0) %>% 
  rename(n_keepers = n, n_rejects = r) 

# plot of number of keepers (bars) vs. rejects (dots) by chain     
ggplot(chain_counts, aes(x = chain_name, y = n_keepers, group = seed, fill = seed)) +
  geom_bar(stat = "identity") + 
  geom_point(data = chain_counts, 
             aes(x = chain_name, y = n_rejects, stat = "identity")) + 
  facet_wrap(type~mode, nrow = 1) + 
  theme_bw() + 
  coord_flip()

# inspect position of rejects     
reject_counts <- ids %>% 
  group_by(chain, rejects) %>% 
  summarize(n = n()) %>% 
  filter(rejects == "r")

# plot position of rejects 
ggplot(reject_counts, aes(x = chain,  y = n)) + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous( breaks = (1:20)) + 
  xlab("chain position")

# demographic summary--------------------------------------------------------------------- 
    # age groups 
    # musical ability 
    # gender 
    # location 
    
    # select grouping variables to obtain various counts (full data set):
   demo_counts <- survey %>% 
        left_join(ids, by = "filename") %>%
        filter(rejects == "n") %>%
        group_by(mode, type, chain_name, chain, id, age, gender, music, zip) %>% 
        summarize(n = n())    
    

    # for diss: select & save auditory tempo accepted data
    demo_AT <- survey %>% 
      left_join(ids, by = "filename") %>%
      filter(rejects == "n",
             mode == 'a',
             type == 't')
      
    write.csv(demo_AT, demo_output)
    

  
    
    ggplot(demo_counts, aes(x = music, y = n, group = type, fill = type)) +
        geom_bar(stat="identity", position = position_dodge()) +
        facet_grid(type~mode)
    
    ggplot(demo_counts, aes(x = age, y = n, group = type, fill = type)) +
        geom_bar(stat="identity", position = position_dodge()) +
        facet_grid(type~mode)
    
    
    ggplot(demo_counts, aes(x = gender, y = n, group = type, fill = type)) +
        geom_bar(stat="identity", position = position_dodge())+
        facet_grid(type~mode)
    

    
# avergae tempo by location 
    temp_location  <- survey %>% 
        right_join(tap_summary, by = "filename")

    mich <- qmap(location = 'USA', zoom = 4, maptype = "toner-lite")
        
    mich + 
    geom_point(data = temp_location, 
               aes(x = longitude, y = latitude, color = avg_rate, stat = "identity"),
               alpha = .5) +
               scale_color_continuous(low = "green", high = "red")
    
 
 

    

  
  
  
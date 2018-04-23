# ack! kind of gave up. 

# shortcut -------
tempo <- read.csv("compiled data/tempo_apcam.csv") %>% select(-X)

# data composition... -------

# source functions --
  source("2016-04-11_functions.R")
  
  # get seeds --
  stimuli <- read.csv("seeds.csv")
  
  # get names of files --
  dat <- list.files("raw data")   
  
  # load the taps and survey--
  taps <- get_files(dat, ".csv", "raw data")
  taps <- clean_taps(taps, 3) # load the 3rd row 
  
  # get unique participant info--
  ids <- get_cond(dat)
  
  # Compiled raw tap interval data--
  # TAPS: compile data frames for auditory tempo --
  
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
  
  # add id info & filter rejects. 
  taps_full_apcam <- taps_full %>%
    inner_join(ids, by = "filename") %>%
    filter(rejects == "n") %>%
    rename(generation = chain) %>%
    select(mode, type, seed, chain_name,
           filename, rejects, starter, id, sourceId, generation,
           tap_number, tap_int, source_tap_int, tap_diff, abs_tap_diff) %>%
    arrange(chain_name, generation, tap_number)
  
  # write to csv for further processing
  write.csv(taps_full_apcam, "compiled data/taps_apcam.csv")

# summarize tempo --
  #(data already filtered)
  taps_summary_apcam <- taps_full_apcam %>% 
    group_by(filename) %>% 
    summarize(tempo = mean(tap_int),
              sd_taps = sd(tap_int)) %>%            
    mutate(cov = sd_taps/tempo) %>%
    inner_join(ids, by = "filename") %>%  
    rename(generation = chain) %>%
    select(mode, type, seed, chain_name,
           filename, starter, id, sourceId, generation,
           tempo, sd_taps, cov) %>%
    arrange(chain_name, generation)
  
  # write to csv for further processing
  write.csv(taps_summary_apcam, "compiled data/tempo_apcam.csv")

# spaghetti
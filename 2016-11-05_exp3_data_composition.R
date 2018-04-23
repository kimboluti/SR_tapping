# Data composition script for individual serial reproduction (SR) data
# Exp 3 of Fromboluti dissertation
#
# Composes data from E-prime (ep) merged data:
# (1) E-prime (ep) data:
#     - raw data = individual tap intervals for SR and self-paced motor tempo (mt) tapping tasks
# (2) No adjusted mean (am) tempi as in Exp 2. 
#     - during stim gen, used medians each generation
#     - 3 exceptions (for P500) - hand-coded to replace
#

# set up------------------------------
  
  # input data:
    ep_filename <- 'raw data/IT03_091616_allexport_final.txt'

  # output files:
    ep_output <- "compiled data/taps_ep_sr_2016-11-05.csv"
    ep_mt_output <- "compiled data/taps_ep_mt_2016-11-05.csv"
    #ep_sc_output <- "compiled data/taps_ep_sync_2016-XX-XX.csv"

    ep_summary_output <- "compiled data/summary_ep_2016-11-05.csv"  
    ep_mt_summary_output <- "compiled data/summary_ep_mt_2016-11-05.csv"    
    #ep_sc_summary_output <- "compiled data/summary_ep_sync_2016-XX-XX.csv"
  
  # Set directory 
    #setwd('C:\Users\Ellie\Google Drive\experiments & projects\iterated tapping - tempo\ANALYSIS - 10-2016 (diss)\Experiment 2 (IT02 -- lab iterations)')
    original_directory <- getwd()   
    
  # Load libraries 
    library(stringr)
    library(tidyr)
    library(ggplot2)
    library(plyr)
    
    library(dplyr) # load dplyr after plyr 

# e-prime (ep): load data & clean up------------------------------
# check...can i force it to read-in tap_int as numeric instead of interval? is it converting to inverval??  

  ep_readin <- read.delim(ep_filename) # takes awhile to process
  
  ep_raw <- ep_readin %>%
    filter(ChainID != 6600) %>% # filtering participant who did not complete
    # fix for mix-labeled P700 session info:
    mutate(Session = ifelse(Subject==702 | Subject==703, 1,
                            ifelse(Subject==704|Subject==705|Subject==706, 2, Session))) %>%
    # generic re-naming and clean-up:
    mutate(train_type = as.factor(ifelse(Subject < 1000, "synchronize", "observe")),
           id = as.factor(Subject), # unique identifer for the iteration (redundant with cohort+generation)
           #sourceId = as.factor(Source), # source id may be messed up...just remake source later
           participant = as.factor(ChainID), # i.e., participant...
           # want to make cohort consistent across conds...so 1100 & 100 -> 1, etc.
           cohort = ifelse(ChainID == 100 | ChainID == 1100, 1,
                           ifelse(ChainID == 200 | ChainID == 2200, 2,
                                  ifelse(ChainID == 300 | ChainID == 3300, 3,
                                         ifelse(ChainID == 400 | ChainID == 4400, 4,
                                                ifelse(ChainID == 500 | ChainID == 5500, 5,
                                                       ifelse(ChainID == 600 | ChainID == 6600, 6,
                                                              ifelse(ChainID == 700 | ChainID == 7700, 7, NA))))))),
           generation = as.numeric(Chain), 
           chain_name = paste(train_type, cohort, seed, sep = "_"),
           seed_int = seed,
           seed = as.factor(seed))


# fixes...TO DO:


# fixes...DONE:

  #700 had mis-labelled session
  tmp <- ep_raw[ep_raw$cohort==7,]
  table(tmp$Subject, tmp$Session)
  table(tmp$Subject, tmp$generation)
  table(tmp$Subject, tmp$generation, tmp$Session)

  # check all...should be three rows with numbers in each session for each subject
  table(ep_raw$Subject, ep_raw$Session)



# ep - SR: filter and select (i.e. compose) serial reproduction data to be saved -------------------------
# SR taps: select only serial reproduction taps and clean-up


    ep_sr_data <- ep_raw %>% 
      filter(Procedure.SubTrial. == "ContinProc") %>%
      mutate(tap_type = as.factor("sr"),
             session = Session,
             tap_number = LogLevel5,
             tap_int = as.numeric(Tap.dt.LogLevel5.)) %>%
      select(train_type, cohort, participant, session, seed, seed_int, chain_name,
             id, generation, 
             tap_type, tap_number, tap_int) %>%
      # remove 1 instance of tap_int = NA from data set - expect nrow = 38175
      na.omit() %>%  
      full_join(sr_filters_byblock, by = c("train_type","cohort","participant","session",
                                           "seed", "seed_int", "chain_name", 
                                           "id", "generation")) %>%
      mutate(tap_incl_rmfirst = ifelse(tap_number == 1, 0, 1),
             tap_incl_rm100ms = ifelse(tap_int < 100, 0, 1),
             tap_incl_rmupper = ifelse(tap_int > upper_cut_bymedian, 0, 1),
             tap_incl_rmnints = ifelse(nints <= 10, 0, 1)) %>%
      # include summary column
      mutate(tap_include = ifelse(tap_number == 1, 0, # exclude if first tap of trial
                                  ifelse(tap_int < 100, 0, # exclude if tap is < 100ms
                                         ifelse(tap_int > upper_cut_bymedian, 0, # exclude if tap is > 1.75*median by block for individual
                                                ifelse(nints <= 10, 0,1))))) # exclude if number of intervals that went into calculating cut-off is <= 5
          
    # write all ep_sr_data to csv file for easy loading later
    write.csv(ep_sr_data, ep_output)
    # note: '...old_filter...' file includes >4000ms upper cut-off


  # plot tests to make sure i've got the variables i need:
  ep_sr_data %>%
    ggplot(aes(x=tap_number, y=tap_int, group=chain_name, color=seed)) +
    geom_point() + coord_cartesian(ylim=c(0,3000)) +
    facet_grid(train_type~cohort)
  
  ep_sr_data %>%
    filter(tap_include == 1) %>% # wow pretty differnt Ps 300, 400, 500...
    group_by(train_type,cohort,seed,generation,chain_name) %>%
    summarize(meantaps = mean(tap_int, na.rm=T)) %>%
    ggplot(aes(x=generation, y=meantaps, group=chain_name, color=seed)) +
    geom_point() + geom_line() +
    coord_cartesian(ylim=c(0,3000)) +
    facet_grid(train_type~cohort)
  
  # test plot - return (kinda cool looking)
  ep_sr_data %>%
    filter(tap_number!=1) %>%
    ggplot(aes(x=tap_int, y=lead(tap_int), color=generation)) +
    geom_point() + #coord_cartesian(ylim=c(0,3000)) +
    coord_cartesian(xlim=c(0,3000),ylim=c(0,3000))+
    facet_grid(train_type~cohort)
  

# ep - SR: create median filter (to match MT method - #3 option from exp 2) --------------------
# generate df for filtering by median 
# create sr_filters_byblock to use above in ep_sr_data df creation section

# filter by n-ints & upper cut
# upper: 1.75*median of tap_ints by BLOCK (aka generation) by participant (after excluding first tap of block)
    
    # ***FILTER DF: same upper bound as option exp2_2a: 1.75*median    
    sr_filters_byblock <- ep_sr_data %>%
      filter(tap_number != 1) %>%
      group_by(train_type,cohort,participant,session,seed,seed_int,chain_name,
               id,generation) %>% 
      summarize(nints = n(),
                median_for_filter = median(tap_int)) %>%
      mutate(upper_cut_bymedian = 1.75*median_for_filter)
    
    # for myself to review:
    summary <- sr_filters_byblock %>%
      group_by(train_type, seed) %>%
      summarize(upper = mean(upper_cut_bymedian))
    
    
    
    
    # used this to get a quick idea of nints by block & what those data look like
    counts <- ep_sr_data %>%
      filter(tap_number != 1) %>%
      group_by(train_type,cohort,participant,session,seed,seed_int,chain_name,
               id,generation) %>%
      summarize(nints = n())
    
    low_counts <- counts[counts$nints <= 10,] # 10->2 trials (nints(506,759)=6, nints(505,1139)=2).
      # 12->2 also.
    
    low_count_data <- low_counts %>%
      inner_join(ep_sr_data, by=c("train_type","participant",
                                  "cohort","id","generation","session","seed","chain_name"))
    
    # just plot low count tap interval series
    low_count_data %>% 
      #group_by(exp_name,tap_type,block,id) %>% 
      #filter(tap_int < 1.75*median(tap_int)) %>%
      ggplot(aes(x=tap_number, y=tap_int, group=id, color=id)) +
      geom_point() + geom_line() + #coord_cartesian(ylim=c(0,3500)) + 
      facet_grid(~chain_name)
    #facet_grid(tap_type~block~exp_name, scales="free")
    


# maybe later... - ep - SR: exclusion summaries and plot checks ---------------------------------

  # summaries of what will be included if exclusion filters implemented

    # by exp_cond X seed summarizes number of EXCLUDED (saved to csv)
    # other groupings summarize included (not saved to csv)
    
    # MOST USEFUL PROBABLY: exp condtion X seed (28 rows)   
    tap_excl_counts <- ep_sr_data %>%
      mutate(count = 1) %>%
      group_by(exp_name, mode, train_type, seed, seed_int) %>%
      summarize(incl_all = sum(count),
                excluded_first = incl_all - sum(tap_incl_rmfirst),
                excluded_100ms = incl_all - sum(tap_incl_rm100ms),
                excluded_4000ms = incl_all - sum(tap_incl_rm4000ms),
                excluded_all = incl_all - sum(tap_include)) %>%
      mutate(prop_rmfirst = excluded_first/incl_all, 
             prop_rm100ms = excluded_100ms/incl_all,
             prop_rm4000ms = excluded_4000ms/incl_all,
             prop_rmall = excluded_all/incl_all)

    write.csv(tap_excl_counts,"compiled data/taps_ep_exclusion_summary_2016-10-10.csv")


    # other groupings:
    
    # exp condtion X seed X block (84 rows)
    tap_excl_counts_BLOCKS <- ep_sr_data %>%
      mutate(count = 1) %>%
      group_by(exp_name, mode, train_type, seed, seed_int, block) %>%
      summarize(incl_all = sum(count),
                excluded_first = incl_all - sum(tap_incl_rmfirst),
                excluded_100ms = incl_all - sum(tap_incl_rm100ms),
                excluded_3000ms = incl_all - sum(tap_incl_rm3000ms),
                excluded_all = incl_all - sum(tap_include)) %>%
      mutate(prop_rmfirst = excluded_first/incl_all, 
             prop_rm100ms = excluded_100ms/incl_all,
             prop_rm3000ms = excluded_3000ms/incl_all,
             prop_rmall = excluded_all/incl_all)

      byblock <- tap_excl_counts_BLOCKS %>%
        group_by(block) %>%
        summarize(all = sum(incl_all),
                  prop_rmfirst = sum(excluded_first)/all,
                  prop_rm100ms = sum(excluded_100ms)/all,
                  prop_rm4000ms = sum(excluded_4000ms)/all,
                  prop_rmall = sum(excluded_all)/all)

    # exp condtion X cohort X seed (140 rows)
    tap_counts_ecs <- ep_sr_data %>%
      mutate(count = 1) %>%
      group_by(exp_name, mode, train_type, cohort, seed, seed_int) %>%
      #filter(tap_include == 1) %>%
      summarize(incl_all = sum(count),
                incl_after_rmfirst = sum(tap_incl_rmfirst),
                incl_after_rm100ms = sum(tap_incl_rm100ms),
                incl_after_rm3000ms = sum(tap_incl_rm3000ms),
                incl_after_rmall = sum(tap_include)) %>%
      mutate(prop_rmfirst = (incl_all - incl_after_rmfirst)/incl_all, 
             prop_rmlimits = (incl_after_rmfirst-incl_after_rmall)/incl_all,
             prop_rmall = (incl_all - incl_after_rmall) / incl_all)
    
    # exp condtion X cohort (20 rows)
    tap_counts_ecs <- ep_sr_data %>%
      mutate(count = 1) %>%
      group_by(exp_name, mode, train_type, cohort) %>%
      #filter(tap_include == 1) %>%
      summarize(incl_all = sum(count),
                incl_after_rmfirst = sum(tap_incl_rmfirst),
                incl_after_rm100ms = sum(tap_incl_rm100ms),
                incl_after_rm4000ms = sum(tap_incl_rm4000ms),
                incl_after_rmall = sum(tap_include)) %>%
      mutate(prop_rmfirst = (incl_all - incl_after_rmfirst)/incl_all, 
             prop_rmlimits = (incl_after_rmfirst-incl_after_rmall)/incl_all,
             prop_rmall = (incl_all - incl_after_rmall) / incl_all)
    
    # by seed (7 rows)   
    tap_counts_s <- ep_sr_data %>%
      mutate(count = 1) %>%
      group_by(seed, seed_int) %>%
      #filter(tap_include == 1) %>%
      summarize(incl_all = sum(count),
                incl_after_rmfirst = sum(tap_incl_rmfirst),
                incl_after_rm100ms = sum(tap_incl_rm100ms),
                incl_after_rm4000ms = sum(tap_incl_rm4000ms),
                incl_after_rmall = sum(tap_include)) %>%
      mutate(prop_rmfirst = (incl_all - incl_after_rmfirst)/incl_all, 
             prop_rmlimits = (incl_after_rmfirst-incl_after_rmall)/incl_all,
             prop_rmall = (incl_all - incl_after_rmall) / incl_all)

    # by experimental condition (4 rows)  
    tap_counts <- ep_sr_data %>%
      mutate(count = 1) %>%
      group_by(exp_name, mode, train_type) %>%
      #filter(tap_include == 1) %>%
      summarize(incl_all = sum(count),
                incl_after_rmfirst = sum(tap_incl_rmfirst),
                incl_after_rm100ms = sum(tap_incl_rm100ms),
                incl_after_rm4000ms = sum(tap_incl_rm4000ms),
                incl_after_rmall = sum(tap_include)) %>%
      mutate(prop_rmfirst = (incl_all - incl_after_rmfirst)/incl_all, 
             prop_rmlimits = (incl_after_rmfirst-incl_after_rmall)/incl_all,
             prop_rmall = (incl_all - incl_after_rmall) / incl_all)
     
    # alt quick method for some tables:
    table(ep_sr_data$tap_include, ep_sr_data$seed, ep_sr_data$exp_name)


    # plot checks

    # some distribution checks...to be further pursued in the future?
    # using to determine 1st tap removal, min & max tap_int cut-offs 
    # also checking against adjusted means plots from previous analysis
    # most taps below 3000; above 100
    
      ep_sr_first <- ep_sr_data %>%
        filter(tap_number == 1)
      
        qqnorm(ep_sr_first$tap_int)
      
        ggplot(ep_sr_first, aes(tap_int, group = chain_name, color = seed)) +
          geom_histogram(binwidth = 1000, position = "dodge") + 
          theme_bw()
    
    # implementing exclusion filters before checking data:      
      ep_sr_data_check <- ep_sr_data %>%
        filter(tap_include == 1)
    
      ggplot(ep_sr_data_check, aes(tap_int, fill = seed)) +
        #geom_histogram(binwidth = 50, position = "dodge") +
        #coord_cartesian(xlim = c(0, 4000)) +
        geom_bar(stat="bin", bins = 1000) +
        facet_grid(seed~.) +
        theme_bw()
      
      ggplot(ep_sr_data_check, aes(tap_int, group = chain_name, color = seed)) +
        geom_freqpoly(binwidth = 75) +
        #coord_cartesian(xlim = c(0, 4000)) +
        theme_bw() +
        facet_grid(seed~.)


# ep - SR summary: tempo for each generation. median, siqr, cov ------------------------
# old include filter...rm > 4000 ms
# TWO NEW VERSIONS: 
#   (1) no filters but replacements for P500 to match stim gen; 
#   (2) filtered (maybe for cvs)


  # (1) FIRST VERSION: NO FILTERS (to reproduce stim gen tempi)
  #   !did in excel after saving: replaced 3 tempi for P500, to reflect stim gen tempi
  ep_sr_summary <- ep_sr_data %>% 
    group_by(train_type, cohort, participant, session, seed, seed_int, chain_name, id, generation) %>% 
    summarize(nints = n(),
              median_tempo = median(tap_int, na.rm=T),
              sd_taps = sd(tap_int, na.rm=T),
              IQR = quantile(tap_int, .75) - quantile(tap_int, .25),
              SIQR = .5*(quantile(tap_int, .75) - quantile(tap_int, .25))) %>%   
    mutate(cv = SIQR/median_tempo)

  write.csv(ep_sr_summary, ep_summary_output) # !!modified this file for P500 (see below)
    # P500 investigation: expecting missing or outlier data for gen5-seed1139 and gen8-seed759 and gen8-seed1709  
    tmp <- ep_sr_summary %>% 
      filter(participant==500, 
             generation == 5 | generation == 8)
    # tmp tells me that gen5-1139 and gen8-759 need to be replaced
    #   but gen8-1709 doesn't exist and needs to be added
    #   FOR ALL OF THESE: I used the tempo from the gen before. 
    #     So, Ill use the SD etc from gen before (for lack of a better idea)
    #   MODIFIED IN EXCEL AFTER SAVING MAIN DF. just easier than doing in R.
  # to create plots with p500 changes included...
  ep_sr_summary_w500 <- read.csv(ep_summary_output) %>% select(-X)


  # (2) SECOND VERSION: IMPLEMENT FILTERS (may be necessary for cvs)
  #   DON'T replace 3 tempi for P500, as was done during stim gen
  ep_sr_summary_filt <- ep_sr_data %>% 
    filter(tap_include == 1) %>% 
    group_by(train_type, cohort, participant, session, seed, seed_int, chain_name, id, generation) %>% 
    summarize(nints = n(),
              median_tempo = median(tap_int, na.rm=T),
              sd_taps = sd(tap_int, na.rm=T),
              IQR = quantile(tap_int, .75) - quantile(tap_int, .25),
              SIQR = .5*(quantile(tap_int, .75) - quantile(tap_int, .25))) %>%   
    mutate(cv = SIQR/median_tempo)
  
  
  write.csv(ep_sr_summary_filt, "compiled data/summary_ep_FILTERED_2016-11-06.csv")
  # note: '...old_filter...' file implements filters, including >4000ms upper cut-off

  
  
  

      # some table count checks:
      
      #each p should have ~21 rows: 
      table(ep_sr_summary$participant, ep_sr_summary$session, ep_sr_summary$train_type)
      
      # each p should have 1 in each entry:
      table(ep_sr_summary$seed, ep_sr_summary$generation, ep_sr_summary$participant)
  

      # some plot checks
      
      # spaghetti plots            
      ggplot(ep_sr_summary_w500, aes(x = generation, y = median_tempo, group = chain_name, color = as.factor(seed))) +
        geom_point(size=2) + geom_line() +
        facet_grid(train_type~cohort) +
        theme_bw()
      
      #cv
      ggplot(ep_sr_summary_w500, aes(x = generation, y = cv, group = chain_name, color = as.factor(seed))) +
        geom_point(size=2) + geom_line() + #coord_cartesian(ylim=c(0,.8)) +
        facet_grid(train_type~cohort) + 
        theme_bw()

      ggplot(ep_sr_summary, aes(x=train_type, y=cv)) + 
        geom_boxplot() + scale_y_log10()
      

# not updated ep - SC (sync-contin): filter and select (i.e. compose) synchronization data to be saved -------------------------
  # SYNC and sr taps: select only sync condition taps and clean-up
    
    ep_sync_data <- ep_raw %>% 
      filter(train_type == "sc") %>%
      filter(Procedure.Block. == "SyncContBlockProc") %>%      
      mutate(tap_type = ifelse(Procedure.SubTrial. == "SyncProc", "sync",
                               ifelse(Procedure.SubTrial. == "ContinProc", "cont", NA))) %>%
      select(exp_name, mode, train_type, cohort, seed, seed_int, chain_name,
             id, sourceId, generation, 
             block = Block,
             tap_type, tap_number = LogLevel5, tap_int = Tap.dt.LogLevel5., tap_ts = Tap.t.LogLevel5.) %>%
      # re-number blocks by order (1,2,3) instead of ep order (4,7,10)
      mutate(block = ifelse(block == 4, 1,
                            ifelse(block == 7, 2,
                                   ifelse(block == 10, 3, NA)))) %>%
      na.omit() %>%
      # re-number indices for visual sync condition
      mutate(tap_number = ifelse(tap_number > 62, (tap_number - 62), tap_number)) %>%    
      # includes by criterion (for summary of exclusion by crit)
      mutate(tap_incl_rmfirst = ifelse(tap_number == 1, 0, 1),
             tap_incl_rm100ms = ifelse(tap_int < 100, 0, 1),
             tap_incl_rm4000ms = ifelse(tap_int > 4000, 0, 1)) %>%
      # include summary column
      mutate(tap_include = ifelse(tap_number == 1, 0, # exclude if first tap of trial
                                  ifelse(tap_int < 100, 0, # exclude if tap is < 100ms
                                         ifelse(tap_int > 4000, 0, 1)))) # exclude if > 4000

    # write all ep_sc_data to csv file for easy loading later
    write.csv(ep_sync_data, ep_sc_output)



    # tap_number check: weird for sync - visual condition (confirmed by plot that it's only vis sync)
    ggplot(ep_sync_data, aes(x = tap_number, y = tap_int, group = chain_name, color = seed)) +
      geom_point() + 
      coord_cartesian(ylim = c(0, 4000)) +
      facet_grid(tap_type~mode) +
      theme_bw()
    
    # filtering data confirms that only 63:93 for vis sync:
    check_indices <- ep_sync_data %>%
      filter(exp_name == "v_sc", tap_type == "sync") 
    
    unique(check_indices$tap_number) # confirms that only 63 through 93


    # quick plots to check overall  
    ggplot(ep_sync_data, aes(x = tap_number, y = tap_int, group = chain_name, color = tap_type)) +
      geom_point() + 
      coord_cartesian(ylim = c(0, 4000)) +
      facet_grid(mode~seed) +
      theme_bw()
    
    ggplot(ep_sync_data, aes(x = tap_number, y = tap_int, group = chain_name, color = tap_type)) +
      geom_point() + 
      coord_cartesian(ylim = c(0, 5000)) +
      facet_grid(mode~block) +
      theme_bw()

    
# not updated ep - SC summary: BY BLOCK calculate mean, median, std, cov for individual--------------------

  ep_sync_summary <- ep_sync_data %>% 
    filter(tap_include == 1) %>% 
    group_by(exp_name, mode, train_type, cohort, seed, seed_int, chain_name, generation, 
             block, tap_type) %>% 
    summarize(nints = n(),
              mean_tempo = mean(tap_int),
              sd_taps = sd(tap_int),
              median_tempo = median(tap_int), 
              mad_taps = median(abs(tap_int - median_tempo))) %>%            
    mutate(se = sd_taps/sqrt(nints),
           cov = sd_taps/mean_tempo,
           comv = mad_taps/median_tempo)
  
  write.csv(ep_sync_summary, ep_sc_summary_output)

  # some spaghetti plot checks
    
    #mean
    ggplot(ep_sync_summary, aes(x = generation, y = mean_tempo, group = chain_name, color = seed)) +
      geom_point() +
      geom_line() +
      facet_grid(tap_type~block) +
      theme_bw()
    
    #median
    ggplot(ep_sync_summary, aes(x = generation, y = median_tempo, group = chain_name, color = seed)) +
      geom_point() + 
      geom_line() +
      facet_grid(tap_type~block) + 
      theme_bw()
    
    # another median check
    ggplot(ep_sync_summary, aes(x = generation, y = median_tempo, group = chain_name, color = seed)) +
      geom_point() +
      facet_grid(.~tap_type)
    
    #cov
    ggplot(ep_sync_summary, aes(x = generation, y = cov, group = chain_name, color = seed)) +
      geom_point() + 
      geom_line() +
      facet_grid(tap_type~block) + 
      theme_bw()
    
    #comv
    ggplot(ep_sync_summary, aes(x = generation, y = comv, group = chain_name, color = seed)) +
      geom_point() + 
      geom_line() +
      facet_grid(tap_type~block) + 
      theme_bw()
    
    # another comv check
    ggplot(ep_sync_summary, aes(x = generation, y = comv, group = chain_name, color = seed)) +
      geom_point() +
      facet_grid(tap_type~cohort)
      

# ep - MT (motor tempo): filter and select (i.e. compose) motor tempo data to be saved -------------------------
# see below the 'write.csv' line for steps taken to check data, decide on cut-offs, plot checks, etc.

  ep_mt_data <- ep_raw %>% 
    filter(Procedure.Block. == "SMTBlockProc" | 
             Procedure.Block. == "FastBlockProc" | 
             Procedure.Block. == "SlowBlockProc") %>%      
    mutate(session = Session,
           tap_type = as.factor(ifelse(Procedure.Block. == "FastBlockProc", "fast mt", 
                             ifelse(Procedure.Block. == "SMTBlockProc", "smt",
                                    ifelse(Procedure.Block. == "SlowBlockProc", "slow mt", NA)))),
           block = as.factor(ifelse(generation==1|generation==4|generation==7|generation==10|generation==13,"1",
                          ifelse(generation==2|generation==5|generation==8|generation==11|generation==14,"2",
                                 ifelse(generation==3|generation==6|generation==9|generation==12|generation==15,"3",NA))))) %>%  
    select(train_type, cohort, participant, session,
           id, generation, block, tap_type, tap_number = SubTrial, 
           tap_int = as.numeric(Tap.dt.SubTrial.), tap_ts = as.numeric(Tap.t.SubTrial.)) %>%
    na.omit() %>% # i don't think there are any NAs, but for consistency...
    # run this script up to na.omit to re-generation mt_filters_byblock in section below (ep - MT: (#3 option from exp 2)...
    # cut-offs for excluding data based on median filters in mt_filters_byblock (see sections below for details & df creation)
    full_join(mt_filters_byblock, by = c("train_type","cohort","participant","session","id", 
                                         "generation", "block","tap_type")) %>%
    mutate(tap_incl_rmfirst = ifelse(tap_number == 1, 0, 1),
             tap_incl_rm100ms = ifelse(tap_int < 100, 0, 1),
             tap_incl_rmupper = ifelse(tap_int > upper_cut_bymedian, 0, 1),
             tap_incl_rmnints = ifelse(nints <= 10, 0, 1)) %>%
      # include summary column
    mutate(tap_include = ifelse(tap_number == 1, 0, # exclude if first tap of trial
                                ifelse(tap_int < 100, 0, # exclude if tap is < 100ms
                                       ifelse(tap_int > upper_cut_bymedian, 0, # exclude if tap is > 1.75*median by block for individual
                                              ifelse(nints <= 10, 0,1))))) # exclude if number of intervals that went into calculating cut-off is <= 5

  # write all tap data to csv file for easy loading later
  write.csv(ep_mt_data, ep_mt_output)
  


  # just some counts to think about:
  # 1.75 uppercut PLUS nints > 10 removes 15675 - 15179 = 496 rows (byeond rmfist and rm100), 
  #   i.e. 496/15698 = 3.2% of all taps
  
  # total rows to start with: n = 16229
  count(ep_mt_data) 
  
  # count of included after rmfirst: n = 15698
  count(ep_mt_data[ep_mt_data$tap_incl_rmfirst==1,])
  
  # count of included after rmfirst and rm<100: n = 15675
  count(ep_mt_data[ep_mt_data$tap_incl_rmfirst==1 &
                     ep_mt_data$tap_incl_rm100ms==1,])
  
  
  # count of included after rmupper and rmnints: n = 15179
  count(ep_mt_data[ep_mt_data$tap_include==1,])


  
  # check and trouble-shooting, finding cut-offs, plot checks
  
    # no NAs
    tmp <- ep_mt_data[is.na(ep_mt_data$tap_int),]
    
    # percentile cut-offs by tap_type
    percentile_summary <- ep_mt_data %>%
      filter(tap_number != 1) %>%
      group_by(tap_type) %>% # train_type,
      summarize(min = min(tap_int),
                per1 = quantile(tap_int, .01),
                per5 = quantile(tap_int,.05),
                per50 = quantile(tap_int, .5),
                mean = mean(tap_int),
                per95 = quantile(tap_int, .95),
                per99 = quantile(tap_int, .99),
                max = max(tap_int))           

    # checking percentiles after implementing filters
    check_percentile <- ep_mt_data %>%
      filter(tap_include == 1) %>%
      group_by(tap_type) %>%
      summarize(min = min(tap_int),
                per1 = quantile(tap_int, .01),
                per5 = quantile(tap_int,.05),
                per50 = quantile(tap_int, .5),
                mean = mean(tap_int),
                per95 = quantile(tap_int, .95),
                per99 = quantile(tap_int, .99),
                max = max(tap_int))
    

##### ep - MT: plots to check data -------

    # quick plot to view tap_ints by tap_number
    ep_mt_data %>%
      filter(tap_include==1) %>%
      ggplot(aes(x=tap_number, y=tap_int, group=tap_type, color=tap_type)) +
      geom_point() + #geom_line() +
      coord_cartesian(ylim = c(0,2000)) +
      #scale_y_log10()+
      facet_grid(train_type~cohort, scales = "free")
    
    # check timeseries for EACH participant...
    ep_mt_data %>%
      filter(participant == 500) %>%
      filter(tap_include==1) %>%
      ggplot(aes(x=tap_number, y=tap_int, group=tap_type, color=tap_type)) +
      geom_point() + geom_line() + coord_cartesian(ylim=c(0,4000)) +
      facet_grid(block~session, scales="free")
    
    # 100 - good, a few outliers in slow session 2 block 3. genuinely slow slow of ~2000-3000. by eye, i'd cut ~10 taps from all'
    # 200 - seems pretty self-consistent...not a lot of outliers (maybe some fast). slow ~1500  
    # 300 - sessions 1&2 ok, but lotsa outliers (esp in slow) after that. actual slow tempo prob ~2000
    # 400 - some major outliers (esp slow) sessions 3&4, block 3...slow tempo prob ~2000 max
    # 500 - session 1 issues (esp smt), slowest slow ~3000 
    # 600 - moderate outliers (esp smt, also slow). slow ~2000
    # 700 - wow. perfect data...though slow only ~1000
    # 1100 - good number of bounce outliers (esp slow). slow ~1200
    # 2200 - v clean, only a few outliers. genuine slow ~2500 to ~6000!
    # 3300 - slow < 1000. pretty clean data tho
    # 4400 - slowest slow ~1500. pretty clean data.
    # 5500 - session 3 block 3 fast tempo error (started trial tapping slowly?). otherwise v. clean. slowest slow ~ 1200


    # hist by block by exp_name with lines = tap_type
    # turn filter on or off to check pre- and post-inclusion filtering
    ep_mt_data %>%
      filter(tap_include == 1) %>%
      ggplot(aes(tap_int, group = tap_type, color = tap_type)) +
      geom_freqpoly(binwidth = 75) +
      #coord_cartesian(xlim = c(0, 3000)) +
      theme_bw() +
      facet_grid(session~cohort)


# ep - MT: create median filter (#3 option from exp 2) --------------------
# generate df for filtering by median 
# create mt_filters_byblock to use above in ep_mt_data df creation section

# filter by n-ints & upper cut
# upper: 1.75*median of tap_ints by BLOCK by participant (after excluding first tap of block)

    # ***FILTER DF: same upper bound as option exp2_2a: 1.75*median    
    mt_filters_byblock <- ep_mt_data %>%
      filter(tap_number != 1) %>%
      group_by(train_type,cohort,participant,session,id,generation,block,tap_type) %>% 
      summarize(nints = n(),
                median_for_filter = median(tap_int)) %>%
      mutate(upper_cut_bymedian = 1.75*median_for_filter)
    
    # for myself to review:
    summary <- mt_filters_byblock %>%
      group_by(train_type, tap_type) %>%
      summarize(upper = mean(upper_cut_bymedian))
    



    # used this to get a quick idea of nints by block & what those data look like
    counts <- ep_mt_data %>%
      filter(tap_number != 1) %>%
      group_by(train_type,participant,cohort,id,generation,session,block,tap_type) %>%
      summarize(nints = n())
    
    low_counts <- counts[counts$nints <= 10,] # 10->2 trials (nints(2212)=2, nints(503)=6).
      # 12-> 4 trials (many from 500...but they look like they should be cut...then again, i used 10 in exp 2)
      
    low_count_data <- low_counts %>%
      inner_join(ep_mt_data, by=c("train_type","participant",
                                 "cohort","id","generation","session","block","tap_type"))
    
    # just plot low count tap interval series
    low_count_data %>% 
      #group_by(exp_name,tap_type,block,id) %>% 
      #filter(tap_int < 1.75*median(tap_int)) %>%
    ggplot(aes(x=tap_number, y=tap_int, group=id, color=id)) +
      geom_point() + geom_line() + #coord_cartesian(ylim=c(0,3500)) + 
      facet_grid(tap_type~block~train_type)
      #facet_grid(tap_type~block~exp_name, scales="free")


# ep - MT: summary BY BLOCK calculate mean, median, std, cov for individual--------------------
# i.e., summarize over tap intervals

  ep_mt_summary <- ep_mt_data %>% 
    filter(tap_include == 1) %>% 
    group_by(train_type, cohort, participant, session, id, generation, 
             block, tap_type) %>% 
    summarize(nints = n(),
              mean_tempo = mean(tap_int, na.rm = TRUE),
              sd_taps = sd(tap_int, na.rm = TRUE),
              median_tempo = median(tap_int, na.rm = TRUE), 
              IQR = quantile(tap_int, .75) - quantile(tap_int, .25),
              SIQR = .5*(quantile(tap_int, .75) - quantile(tap_int, .25))) %>%            
    mutate(se = sd_taps/sqrt(nints),
           cv_with_sd = sd_taps/mean_tempo,
           cv = SIQR/median_tempo)
  
  
  write.csv(ep_mt_summary, ep_mt_summary_output)
  



  # check of how many tempi are based on very few intervals
    table((ep_mt_summary$nints))


  # box plot for cv = siqr/median
  ep_mt_summary %>%
    ggplot(aes(x=tap_type, y=cv)) +
    geom_boxplot() + scale_y_log10()


  # some spaghetti plot checks
  # plotting note: session X generation meaningful in terms of development of things over EXPERIMENT
  #     block is for convenience of plotting, within a session, development of things in a SESSION
  
  #mean
  ggplot(ep_mt_summary, aes(x = generation, y = mean_tempo, group = tap_type, color = tap_type)) +
    geom_point() + geom_line() +
    facet_grid(train_type~cohort) + 
    theme_bw()
  
  #median
  ggplot(ep_mt_summary, aes(x = generation, y = median_tempo, group = tap_type, color = tap_type)) +
    geom_point() + geom_line() +
    facet_grid(train_type~cohort) + 
    theme_bw()
  
  #cov
  ggplot(ep_mt_summary, aes(x = as.factor(session), y = cv, group = tap_type, color = tap_type)) +
    geom_point(size = 3) + #geom_line() +
    coord_cartesian(ylim=c(0,.5)) +
    theme_bw() +
    facet_grid(train_type~cohort)
  
  #cov calculated with sds
  ggplot(ep_mt_summary, aes(x = as.factor(session), y = cv_with_sd, group = tap_type, color = tap_type)) +
    geom_point() + #geom_line() +
    facet_grid(train_type~cohort) + 
    theme_bw()
  

# ?todo? ep - MT summary: OVER BLOCKS calculate mean, median, std, cov by individual ------------------------
# i.e., summarize over blocks
# update again 2016-10-31 -- tempo & cv as individual measures to be summarized over (so not worrying about sd and se of these measures)
# plot checks not updated 10-06-16
# redoing 2016-10-19 to fix cv, add se, counts, etc.
# may want to use wav. for covs, need to calculate their own wav_cov to replace mean_cov or whatever.
  

# quick thing 11-05-16
# summarize by participant, mean smt, fast mt, and slow mt (and consistency?)
mt_by_participant <- ep_mt_summary %>%
  group_by(participant, tap_type) %>%
  summarize(nmeasures = n(),
            avg_tempo = mean(median_tempo, na.rm=T),
            sd_tempo = sd(median_tempo, na.rm=T),
            avg_cv = mean(cv, na.rm=T),
            sd_cv = sd(cv, na.rm=T)) %>%
  mutate(se_tempo = sd_tempo/sqrt(nmeasures),
         se_cv = sd_cv/sqrt(nmeasures))

write.csv(mt_by_participant,"tables/mt_summary_by_participant_2016-11-05.csv")

# and...overall....
mt_overall <- ep_mt_summary %>%
  group_by(tap_type) %>%
  summarize(nmeasures = n(),
            avg_tempo = mean(median_tempo, na.rm=T),
            sd_tempo = sd(median_tempo, na.rm=T),
            avg_cv = mean(cv, na.rm=T),
            sd_cv = sd(cv, na.rm=T)) %>%
  mutate(se_tempo = sd_tempo/sqrt(nmeasures),
         se_cv = sd_cv/sqrt(nmeasures))

write.csv(mt_overall,"tables/mt_summary_overall_2016-11-05.csv")




  
  
  
  
  
  
  # 2016-10-31 -- new filter, fewer columns:
  ep_mt_summary_acrossblocks <- ep_mt_summary %>% 
    rename(median_tempo_byblock = median_tempo) %>%
    group_by(exp_name, mode, train_type, cohort, id, generation, tap_type) %>%
    summarize(nints = sum(nints),
              nblocks = n(),
              tempo = mean(median_tempo_byblock, na.rm = TRUE), # by-gen mean of by-block MEDIAN tempi
              cv = mean(cov2, na.rm = TRUE))
  
  write.csv(ep_mt_summary_acrossblocks, "compiled data/summaryacrossblocks_ep_mt_2016-10-31.csv")



  # NO:
  
  # # do i need this for sd of the mean??
  # var <- ep_mt_revised %>%
  #   filter(tap_include == 1) %>%
  #   group_by(exp_name, mode, train_type, cohort, id, generation, tap_type) %>%
  #   summarize(nints=n(),
  #             sd_taps_by_type = sd(tap_int, na.rm = TRUE)) %>%
  #   mutate(se_taps_by_type = sd_taps_by_type/sqrt(nints))
                
  # OOH REPLACING THIS ABOVE (but keeping here in case) -- I don't need sd, se, etc. at this level!
  # new filter, fewer columns:
  ep_mt_summary_acrossblocks <- ep_mt_summary %>% 
    rename(median_tempo_byblock = median_tempo) %>%
    group_by(exp_name, mode, train_type, cohort, id, generation, tap_type) %>%
    summarize(nints = sum(nints),
              nblocks = n(),
              avg_tempo_mean = mean(median_tempo_byblock, na.rm = TRUE), # by-gen mean of by-block MEDIAN tempi
              avg_tempo_sd = sd(median_tempo_byblock, na.rm = TRUE), # by-gen mean of by-block MEDIAN tempi
              avg_cv_mean = mean(cov2, na.rm = TRUE),
              avg_cv_sd = sd(cov2, na.rm = TRUE)) %>%
    mutate(avg_tempo_se = avg_tempo_sd/sqrt(nblocks),
           avg_cv_se = avg_cv_sd/sqrt(nblocks)) %>%
    # re-order so tempo measures are together and variability are together:
    select(exp_name, mode, train_type, id, cohort, generation, tap_type,
           nints, nblocks, avg_tempo_mean, avg_tempo_sd, avg_tempo_se,
           avg_cv_mean, avg_cv_sd, avg_cv_se)   
            

  # old:
  ep_mt_summary_acrossblocks <- ep_mt_summary %>% 
    #filter(nints < 10) %>% # a somewhat arbitrary cut-off, but to be considered (see BY BLOCK section)
    # weight for each block used in summary calculations (from Taylor Intro to Error Analysis):
    #   wav = Weighted AVerage
    mutate(wav_wt = 1/sd_taps^2) %>% 
    rename(mean_tempo_byblock = mean_tempo, 
           median_tempo_byblock = median_tempo) %>%
    group_by(exp_name, mode, train_type, cohort, generation, tap_type) %>%
    summarize(nints = sum(nints),
              nblocks = n(),
              # summarize tempo by generation X tap_type:
              mean_tempo = mean(mean_tempo_byblock, na.rm = TRUE), # by-gen mean of by-block MEAN tempi
              meanmedian_tempo = mean(median_tempo_byblock, na.rm = TRUE), # by-gen mean of by-block MEDIAN tempi
              sd_tempo = sd(mean_tempo_byblock, na.rm = TRUE), # spread of by-block MEAN tempi        
              wav_nom = sum(wav_wt*mean_tempo_byblock),
              wav_denom = sum(wav_wt),
              # summarize variability by generation X tap_type:
              mean_sd = mean(sd_taps, na.rm = TRUE),
              mean_mad = mean(mad_taps, na.rm = TRUE),
              mean_cov = mean(cov, na.rm = TRUE), # could do wav for this, too
              sd_cov = sd(cov, na.rm = TRUE), # could do wav for this, too
              mean_cov2 = mean(cov2, na.rm = TRUE), # ) end here 10-31-2016
              sd_cov2 = sd(cov2, na.rm = TRUE),
              mean_comv = mean(comv, na.rm = TRUE)) %>%
    mutate(se_mean = sd_tempo/sqrt(nblocks),
           wav_tempo = wav_nom/wav_denom,
           wav_sd = 1/sqrt(wav_denom), # this wav refers to tempo
           wav_se = wav_sd/sqrt(nblocks), # this wav refers to tempo
           se_cov = sd_cov/sqrt(nblocks),
           se_cov2 = sd_cov2/sqrt(nblocks)) %>%
    # re-order so tempo measures are together and variability are together:
    select(exp_name, mode, train_type, cohort, generation, tap_type,
           nblocks, mean_tempo, median_tempo, sd_tempo, se_mean, wav_tempo, wav_sd, wav_se, # also cutting out wav calculations
           mean_sd, mean_mad, mean_cov, sd_cov, se_cov, mean_cov2, sd_cov2, se_cov2, mean_comv)   

  #write.csv(ep_mt_summary_acrossblocks, ep_mt_summary_output_acrossblocks)
  write.csv(ep_mt_summary_acrossblocks, "compiled data/summaryacrossblocks_ep_mt_2016-10-27.csv")
  
  # spaghetti plot checks
  # median tempo by generation across cohorts
  #NEEDS MORE REFINING
  ggplot(ep_mt_summary_acrossblocks, aes(x = generation, y = median_tempo, group = cohort, color = tap_type)) +
    geom_point() + 
    geom_line() + # need to figure out correct group for line
    facet_grid(mode~train_type~cohort) +
    theme_bw()
  
  # cov2
  #NEEDS MORE REFINING
  ggplot(ep_mt_summary_acrossblocks, aes(x = generation, y = mean_cov2, group = cohort, color = tap_type)) +
    geom_point() + 
    #geom_line() +
    facet_grid(mode~train_type~cohort) +
    theme_bw()

# Data composition script for mode-move serial reproduction (SR) data
# Exp 2 of Fromboluti dissertation
#
# Composes data from parallel data sets:
# (1) Adjusted mean (am) tempi used during experiment stim gen: 
#     - raw data = mean tempi by seed for SR task, compiled in an Excel from means-for-stim-gen spreadsheets
# (2) E-prime (ep) data:
#     - raw data = individual tap intervals for SR and self-paced motor tempo (mt) tapping tasks
#

# set up------------------------------
  
  # input data:
    am_filename <- 'raw data/IT02-all_taps_summary_exp_092116_final.csv'
    ep_filename <- 'raw data/IT02-all_092216_FINAL_ALLEXPORT.txt'

  # output files:
    ep_output <- "compiled data/taps_ep_2016-10-10.csv"
    ep_sc_output <- "compiled data/taps_ep_sync_2016-10-10.csv"
    ep_mt_output <- "compiled data/taps_ep_mt_2016-10-31.csv"
    
    ep_sr_return_output <- "compiled data/taps_averagedacrossblocks_for_return_2016-10-19.csv"

    am_summary_output <- "compiled data/summary_am_2016-10-04.csv" # no such thing as tap ints for am (already summarized)
    ep_summary_output <- "compiled data/summary_ep_2016-10-19.csv" # updated 10-19 with cv and number of intervals used to calculate summary measures
    ep_summary_output_acrossblocks <- "compiled data/summaryacrossblocks_ep_2016-10-19.csv" # updated 10-19 with cv, se, wav, etc etc
    
    ep_sc_summary_output <- "compiled data/summary_ep_sync_2016-10-19.csv"
    ep_sc_summary_output_acrossblocks <- "compiled data/summaryacrossblocks_ep_sync_2016-10-19.csv"
    ep_mt_summary_output <- "compiled data/summary_ep_mt_2016-10-19.csv"
    ep_mt_summary_output_acrossblocks <- "compiled data/summaryacrossblocks_ep_mt_2016-10-19.csv"
  
  # Set directory 
    #setwd('C:\Users\Ellie\Google Drive\experiments & projects\iterated tapping - tempo\ANALYSIS - 10-2016 (diss)\Experiment 2 (IT02 -- lab iterations)')
    original_directory <- getwd()   
    
  # Load libraries 
    library(stringr)
    library(tidyr)
    library(ggplot2)
    library(plyr)
    
    library(dplyr) # load dplyr after plyr 

# adjusted means (am): load data & clean up------------------------------
  # already in a summary format, so not much to do here...
  
  am_readin <- read.csv(am_filename) 
  
  am_summary <- am_readin %>%
    # some re-naming and data clean-up
    rename(mean_tempo_am = avg_rate) %>%
    # make exp_name variable same format as ep data
    mutate(exp_name = ifelse(exp_name == "a_sc", "a_sc", 
                           ifelse(exp_name == "b_or", "a_or", 
                                  ifelse(exp_name == "c_sc", "v_sc", 
                                         ifelse(exp_name == "d_or", "v_or", NA))))) %>%
    mutate(chain_name = paste(exp_name, cohort, seed, sep = "_"), # unique id for each chain
           cohort = as.factor(cohort),
           seed_int = seed,
           seed = as.factor(seed)) %>%
    select(exp_name,mode,train_type,cohort,seed,seed_int,chain_name,generation,mean_tempo_am)

  # filename for am_summary_output
  write.csv(am_summary, am_summary_output)

# e-prime (ep): load data & clean up------------------------------
  
  ep_readin <- read.delim(ep_filename) # takes awhile to process

  ep_raw <- ep_readin %>%
    # mis-labeled chain and chain id for generation 15 in v-or condition
    mutate(Chain = ifelse(ChainID == 15, 15, Chain),
           ChainID = ifelse(ChainID == 15, 1, ChainID)) %>%
    # generic re-naming and data clean-up
    # need to check renaming and clean up viable for self-paced mt tasks
    mutate(exp_name = ifelse(ExperimentName == "IT02_S&C_arduino_v0", "a_sc", 
                             ifelse(ExperimentName == "IT02-A_S&C_arduino_v0", "a_sc", 
                                    ifelse(ExperimentName == "IT02_S&C_arduino_aud_LR_v0", "a_or", 
                                           ifelse(ExperimentName == "IT02-C_SC_vis_v0", "v_sc", 
                                                  ifelse(ExperimentName == "IT02-D_LR_vis_v0", "v_or", NA)))))) %>%
    mutate(mode = ifelse(ExperimentName == "IT02_S&C_arduino_v0", "a", 
                         ifelse(ExperimentName == "IT02-A_S&C_arduino_v0", "a", 
                                ifelse(ExperimentName == "IT02_S&C_arduino_aud_LR_v0", "a", 
                                       ifelse(ExperimentName == "IT02-C_SC_vis_v0", "v", 
                                              ifelse(ExperimentName == "IT02-D_LR_vis_v0", "v", NA)))))) %>%
    mutate(train_type = ifelse(ExperimentName == "IT02_S&C_arduino_v0", "sc", 
                               ifelse(ExperimentName == "IT02-A_S&C_arduino_v0", "sc", 
                                      ifelse(ExperimentName == "IT02_S&C_arduino_aud_LR_v0", "or", 
                                             ifelse(ExperimentName == "IT02-C_SC_vis_v0", "sc", 
                                                    ifelse(ExperimentName == "IT02-D_LR_vis_v0", "or", NA)))))) %>%
    mutate(exp_name = as.factor(exp_name),
           mode = as.factor(mode),
           train_type = as.factor(train_type),
           id = as.factor(Subject), # redundant with cohort + generation?
           sourceId = as.factor(Source), 
           cohort = as.factor(ChainID),
           generation = as.numeric(Chain), 
           chain_name = paste(exp_name, cohort, seed, sep = "_"),
           seed_int = seed,
           seed = as.factor(seed))
  
    # trouble-shooting participant with flip-flopped generation and cohort information:
      # troubling data:
      tmp <- ep_readin %>% filter(Subject == 115 & ExperimentName == "IT02-D_LR_vis_v0" & Procedure.SubTrial. == "ContinProc")
      # as a comparison:
      tmp2 <- ep_readin %>% filter(Subject == 115 & ExperimentName == "IT02-C_SC_vis_v0" & Procedure.SubTrial. == "ContinProc")
      # check post fix:
      tmp3 <- ep_raw %>% filter(Subject == 115 & ExperimentName == "IT02-D_LR_vis_v0" & Procedure.SubTrial. == "ContinProc")
      tmp4 <- ep_raw %>% filter(Subject == 115 & ExperimentName == "IT02-C_SC_vis_v0" & Procedure.SubTrial. == "ContinProc")

# ep - SR: filter and select (i.e. compose) serial reproduction data to be saved -------------------------
  # SR taps: select only serial reproduction taps and clean-up

    ep_sr_data <- ep_raw %>% 
      filter(Procedure.SubTrial. == "ContinProc") %>%
      mutate(tap_type = as.factor("sr")) %>%
      select(exp_name, mode, train_type, cohort, seed, seed_int, chain_name,
             id, sourceId, generation,
             block = Block,
             tap_type, tap_number = LogLevel5, tap_int = Tap.dt.LogLevel5.) %>%
      # re-number blocks by order (1,2,3) instead of ep order (4,7,10)
      mutate(block = ifelse(block == 4, 1,
                            ifelse(block == 7, 2,
                                   ifelse(block == 10, 3, NA)))) %>%
      # remove 6 instances of tap_int = NA from data set - expect nrow = 194093
      na.omit() %>%
      # includes by criterion (for summary of exclusion by crit)
      mutate(tap_incl_rmfirst = ifelse(tap_number == 1, 0, 1),
             tap_incl_rm100ms = ifelse(tap_int < 100, 0, 1),
             tap_incl_rm4000ms = ifelse(tap_int > 4000, 0, 1)) %>%
      # include summary column
      mutate(tap_include = ifelse(tap_number == 1, 0, # exclude if first tap of trial
                                    ifelse(tap_int < 100, 0, # exclude if tap is < 100ms
                                           ifelse(tap_int > 4000, 0, 1)))) # exclude if > 4000
            
    # write all ep_sr_data to csv file for easy loading later
    write.csv(ep_sr_data, ep_output)

# ep - SR: percentile cut-off formulations (10-10-2016) ---------------------------------
# used to re-calibrate exclusion criterion in data (filter & select) composition section above.
# start from ep_sr_data (inclde cols of ep_sr_data df will later be refined by percentile analysis)
    
    # percentile cut-offs by tap_type
    
    # first filter first taps from data
    ep_sr_data_filt1 <- ep_sr_data %>%
      filter(tap_number != 1) 
     
    # percentiles by experiment and seed
    percentile_summary <- ep_sr_data_filt1 %>%
      group_by(exp_name, seed) %>%
      summarize(min = min(tap_int),
                per1 = quantile(tap_int, .01),
                per5 = quantile(tap_int,.05),
                per50 = quantile(tap_int, .5),
                mean = mean(tap_int),
                per95 = quantile(tap_int, .95),
                per99 = quantile(tap_int, .99),
                max = max(tap_int))
    
    # percentiles by seed
    byseed <- ep_sr_data_filt1 %>%
      group_by(seed) %>%
      summarize(min = min(tap_int),
                per1 = quantile(tap_int, .01),
                per5 = quantile(tap_int,.05),
                per50 = quantile(tap_int, .5),
                mean = mean(tap_int),
                per95 = quantile(tap_int, .95),
                per99 = quantile(tap_int, .99),
                max = max(tap_int))
    
    # low cut-off of 100 is uncontroversial: should remove < 1% of data
    # upper cut-off is trickier...
    
    # viewing closer, by cohort:
    bycohort_plus <- ep_sr_data_filt1 %>%
      group_by(exp_name, cohort, seed) %>%
      summarize(min = min(tap_int),
                per1 = quantile(tap_int, .01),
                per5 = quantile(tap_int,.05),
                per50 = quantile(tap_int, .5),
                mean = mean(tap_int),
                per95 = quantile(tap_int, .95),
                per99 = quantile(tap_int, .99),
                max = max(tap_int))
    
    summary(bycohort_plus)
    
    tellme <- bycohort_plus %>%
      filter(per99 >= 3000)
    tellme_cohort_4000 <- bycohort_plus %>%
      filter(per99 >= 4000)
    tellme_exp <- percentile_summary %>%
      filter(per99 >= 3000)
    tellme_seed <- byseed %>%
      filter(per99 >= 3000)
    
    
    table(tellme$seed, tellme$cohort, tellme$exp_name)
    
    ggplot(data = bycohort_plus, aes(x = cohort, y = per99, color = seed)) +
      geom_boxplot() +
      facet_grid(exp_name~.)
    
    ggplot(data = bycohort_plus, aes(x = cohort, y = per99, group = exp_name, color = seed)) +
      geom_point(size = 3) +
      facet_grid(seed~exp_name)
    
    # rawest taps (first tried by block, then by gen)
    # using per99 as cutoff from this would leave too much crap in 
    bygen_plus <- ep_sr_data_filt1 %>%
      group_by(exp_name, cohort, seed, generation) %>%
      summarize(min = min(tap_int),
                per1 = quantile(tap_int, .01),
                per5 = quantile(tap_int,.05),
                per50 = quantile(tap_int, .5),
                mean = mean(tap_int),
                per95 = quantile(tap_int, .95),
                per99 = quantile(tap_int, .99),
                max = max(tap_int))
    
    # percentile summary for adjusted mean data
    # logic: goal is to have ep data reflect same thing as am data
    # note: am data starts at summary level
    am_percentile <- am_summary %>%
      group_by(exp_name, cohort, seed) %>%
      summarize(min = min(mean_tempo_am),
                per1 = quantile(mean_tempo_am, .01),
                per5 = quantile(mean_tempo_am,.05),
                per50 = quantile(mean_tempo_am, .5),
                mean = mean(mean_tempo_am),
                per95 = quantile(mean_tempo_am, .95),
                per99 = quantile(mean_tempo_am, .99),
                max = max(mean_tempo_am))
    
    tellme_am <- am_percentile %>%
      filter(per99 >= 3000)
    
    
    # checking percentiles after implementing filters
    check_percentile <- ep_sr_data %>%
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
    
    # quick plot to view for each tap_type, tap_ints by tap_number to decide fast/slow cut-offs
    ep_sr_data %>%
      filter(tap_type == "sr") %>%
      mutate(block = as.factor(block)) %>%
      ggplot(aes(x = tap_number, y = tap_int, group = id, color = block)) +
      geom_point() +
      #geom_line() +
      coord_cartesian(ylim = c(0,6500)) +
      #scale_y_log10()+
      facet_grid(cohort~exp_name)
    
    # checking normality of data...ha not normal at all
    # turn filter on/off
    ep_sr_data %>%
      filter(tap_include == 1) %>%
      ggplot(aes(sample = tap_int, color = seed)) +
      stat_qq()
    
    # log-transforming tap ints before plotting...
    # heavy-tailed data
    ep_sr_data %>%
      mutate(log_ints = log10(tap_int)) %>%
      ggplot(aes(sample = log_ints, color = seed)) +
      stat_qq()
    
    # checking am summary data normality
    am_summary %>%
      ggplot(aes(sample = mean_tempo_am, color = seed)) +
      stat_qq()
    
    
    # box plot...turn filter on/off
    # ** saved this plot post-filter (turn on/off scale and facets for dif versions)
    ep_sr_data %>%
      #filter(tap_include == 1) %>%
      ggplot(aes(x = seed, y = tap_int, color = seed)) +
      geom_boxplot() +
      theme_bw() + 
      #scale_y_log10() +
      #facet_grid(exp_name~cohort)
      facet_grid(exp_name~block)

    # after log-transforming data
    ep_sr_data %>%
      mutate(log_ints = log10(tap_int)) %>%
      ggplot(aes(x = seed, y = log_ints, color = seed)) +
      geom_boxplot()
    
    # hist by block by exp_name with lines = tap_type
    # turn filter on or off to check pre- and post-inclusion filtering
    ep_sr_data %>%
      filter(tap_include == 1) %>%
      ggplot(aes(tap_int, group = tap_type, color = tap_type)) +
      geom_freqpoly(binwidth = 75) +
      #coord_cartesian(xlim = c(0, 3000)) +
      theme_bw() +
      facet_grid(block~exp_name)
    # and again with log-transformed data
    ep_sr_data %>%
      filter(tap_include == 1) %>%
      mutate(log_ints = log10(tap_int)) %>%
      ggplot(aes(log_ints, group = seed, color = seed)) +
      geom_freqpoly(binwidth = 0.025)



# ep - SR: exclusion summaries and plot checks ---------------------------------

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

# ep - SR: tap_ints averaged across blocks (for dynamics analysis)  -------------------------
#   start from ep_sr_data
#   calculating a single tap_int for each chain_name by averaging across blocks
#   esp useful for return plots?
  
  ep_sr_return <- ep_sr_data %>%
    filter(tap_include == 1) %>%
    group_by(exp_name,mode,train_type,cohort,seed,seed_int,chain_name,id,sourceId,generation,tap_type, 
             tap_number) %>%
    arrange(chain_name,generation,tap_number) %>%
    summarize(nints = n(),    
              ITI_plus = mean(tap_int, na.rm = TRUE),
              ITI_plus_sd = sd(tap_int, na.rm = TRUE)) %>%
    mutate(ITI_plus_se = ITI_plus_sd/sqrt(nints),
           ITI_plus_cv = ITI_plus_sd/ITI_plus,
           i = tap_number - 1,
           ITI_i = lag(ITI_plus), # awk - need ifelse for starter tap with stim tempo
           delta = ITI_plus - ITI_i)
  
    # in the future, if i get to adding source tempo (from summary?):
    # mutate(source_gen = generation - 1,
    #        source_tempo = ifelse(source_gen == 0, seed_int, lag(tempo)),
    #        delta = tempo - source_tempo)
    # 
  
  write.csv(ep_sr_return, ep_sr_return_output)

# ep - SR summary: BY BLOCK calculate mean, median, std, cov by individual ------------------------
# 2016-10-19: re-doing...fix cv and add nints and se

  ep_sr_summary <- ep_sr_data %>% 
    filter(tap_include == 1) %>% 
    group_by(exp_name, mode, train_type, cohort, seed, seed_int, chain_name, generation, block) %>% 
    summarize(nints = n(),
              mean_tempo = mean(tap_int),
              sd_taps = sd(tap_int),
              median_tempo = median(tap_int), 
              mad_taps = median(abs(tap_int - median_tempo))) %>%            
    mutate(se = sd_taps/sqrt(nints),
           cov = sd_taps/mean_tempo,
           comv = mad_taps/median_tempo)
    
  write.csv(ep_sr_summary, ep_summary_output)
      
      # some plot checks
      
      # spaghetti plots            
      ggplot(ep_sr_summary, aes(x = generation, y = mean_tempo, group = chain_name, color = seed)) +
        geom_point() + 
        geom_line() +
        facet_grid(cohort~block) +
        theme_bw()
      
      #mean
      ggplot(ep_sr_summary, aes(x = generation, y = mean_tempo, group = chain_name, color = train_type)) +
        geom_point() + 
        geom_line() +
        facet_grid(seed~block) +
        theme_bw()
      
      #median
      ggplot(ep_sr_summary, aes(x = generation, y = median_tempo, group = chain_name, color = seed)) +
        geom_point() + 
        #geom_line() +
        facet_grid(train_type~block) + 
        theme_bw()

      #cov
      ggplot(ep_sr_summary, aes(x = generation, y = cov, group = chain_name, color = seed)) +
        geom_point() + 
        #geom_line() +
        facet_grid(train_type~cohort) + 
        theme_bw()
      
      #comv
      ggplot(ep_sr_summary, aes(x = generation, y = comv, group = chain_name, color = seed)) +
        geom_point() + 
        #geom_line() +
        facet_grid(train_type~cohort) + 
        theme_bw()


# ep - SR summary: OVER BLOCKS calculate mean, median, std, cov by individual ------------------------
# start with summary by block to take mean of medians -- most similar to method used during experiment
# 2016-10-19: added weighted average tempo (wav, across blocks), se, corrected COV etc.

    ep_sr_summary_acrossblocks <- ep_sr_summary %>% 
      # weight for each block used in summary calculations (from Taylor Intro to Error Analysis):
      #   wav = Weighted AVerage
      mutate(wav_wt = 1/sd_taps^2) %>% 
      rename(mean_tempo_byblock = mean_tempo, 
             median_tempo_byblock = median_tempo) %>%
      group_by(exp_name, mode, train_type, cohort, seed, seed_int, chain_name, generation) %>% 
      # summarize tempo by generation:
      summarize(nblocks = n(),
                mean_tempo = mean(mean_tempo_byblock, na.rm = TRUE), # by-gen mean of by-block MEAN tempi
                median_tempo = mean(median_tempo_byblock, na.rm = TRUE), # by-gen mean of by-block MEDIAN tempi
                sd_tempo = sd(mean_tempo_byblock, na.rm = TRUE), # spread of by-block MEAN tempi        
                wav_nom = sum(wav_wt*mean_tempo_byblock),
                wav_denom = sum(wav_wt),
                # summarize variability by generation:
                mean_sd = mean(sd_taps),
                mean_mad = mean(mad_taps),
                mean_cov = mean(cov),
                sd_cov = sd(cov),
                mean_comv = mean(comv)) %>%
      mutate(se_mean = sd_tempo/sqrt(nblocks),
             wav_tempo = wav_nom/wav_denom,
             wav_sd = 1/sqrt(wav_denom),
             wav_se = wav_sd/sqrt(nblocks),
             se_cov = sd_cov/sqrt(nblocks)) %>%
      # re-order so tempo measures are together and variability are together:
      select(exp_name, mode, train_type, cohort, seed, seed_int, chain_name, generation,
             nblocks, mean_tempo, median_tempo, sd_tempo, se_mean, wav_tempo, wav_sd, wav_se, # also cutting out wav calculations
             mean_sd, mean_mad, mean_cov, sd_cov, se_cov, mean_comv)   
  
      write.csv(ep_sr_summary_acrossblocks, ep_summary_output_acrossblocks)
    
    # spaghetti plot check
    ggplot(ep_sr_summary_acrossblocks, aes(x = generation, y = wav_tempo, group = chain_name, color = seed)) +
      geom_point() + 
      geom_line() +
      facet_grid(train_type~mode~cohort) +
      theme_bw()


# ep - SC (sync-contin): filter and select (i.e. compose) synchronization data to be saved -------------------------
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

    
# ep - SC summary: BY BLOCK calculate mean, median, std, cov for individual--------------------

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
      
# ep - SC summary: OVER BLOCKS calculate mean, median, std, cov by individual ------------------------
# start with summary by block to take mean of medians
  
  ep_sync_summary_acrossblocks <- ep_sync_summary %>% 
    # weight for each block used in summary calculations (from Taylor Intro to Error Analysis):
    #   wav = Weighted AVerage
    mutate(wav_wt = 1/sd_taps^2) %>% 
    rename(mean_tempo_byblock = mean_tempo, 
           median_tempo_byblock = median_tempo) %>%  
    group_by(exp_name, mode, train_type, cohort, seed, seed_int, chain_name, generation,
             tap_type) %>% 
    summarize(nblocks = n(),
              # summarize tempo by generation:
              mean_tempo = mean(mean_tempo_byblock, na.rm = TRUE), # by-gen mean of by-block MEAN tempi
              median_tempo = mean(median_tempo_byblock, na.rm = TRUE), # by-gen mean of by-block MEDIAN tempi
              sd_tempo = sd(mean_tempo_byblock, na.rm = TRUE), # spread of by-block MEAN tempi        
              wav_nom = sum(wav_wt*mean_tempo_byblock),
              wav_denom = sum(wav_wt),
              # summarize variability by generation:
              mean_sd = mean(sd_taps),
              mean_mad = mean(mad_taps),
              mean_cov = mean(cov), # could (but didn't) calculate wav mean cov 
              sd_cov = sd(cov), # could  (but didn't) calculate wav sd cov 
              mean_comv = mean(comv)) %>%
    mutate(se_mean = sd_tempo/sqrt(nblocks),
           wav_tempo = wav_nom/wav_denom,
           wav_sd = 1/sqrt(wav_denom),
           wav_se = wav_sd/sqrt(nblocks),
           se_cov = sd_cov/sqrt(nblocks)) %>%
    # re-order so tempo measures are together and variability are together:
    select(exp_name, mode, train_type, cohort, seed, seed_int, chain_name, generation, tap_type,
           nblocks, mean_tempo, median_tempo, sd_tempo, se_mean, wav_tempo, wav_sd, wav_se, # also cutting out wav calculations
           mean_sd, mean_mad, mean_cov, sd_cov, se_cov, mean_comv)   
    
  write.csv(ep_sync_summary_acrossblocks, ep_sc_summary_output_acrossblocks)
    
  # spaghetti plot checks
    # median tempo by generation across cohorts
    ggplot(ep_sync_summary_acrossblocks, aes(x = generation, y = wav_tempo, group = chain_name, color = seed)) +
      geom_point() + 
      geom_line() +
      facet_grid(mode~tap_type~cohort) +
      theme_bw()
    
    # comv by generation across cohorts
    ggplot(ep_sync_summary_acrossblocks, aes(x = generation, y = mean_comv, group = chain_name, color = seed)) +
      geom_point() + 
      geom_line() +
      facet_grid(mode~tap_type~cohort) +
      theme_bw()


# ep - MT (motor tempo): filter and select (i.e. compose) motor tempo data to be saved -------------------------
# see below 'write.csv' line for steps taken to check data, decide on cut-offs, plot checks, etc.
# *****2016-10-31 update: added second filter option for upperbound cutoff based on individual blocks
#       see "MT #2 reconsiering filters" section for new df 
  
  ep_mt_data <- ep_raw %>% 
    filter(Procedure.Block. == "SMTBlockProc" | 
             Procedure.Block. == "FastBlockProc" | 
             Procedure.Block. == "SlowBlockProc") %>%      
    mutate(tap_type = ifelse(Procedure.Block. == "SMTBlockProc", "smt",
                             ifelse(Procedure.Block. == "FastBlockProc", "fast mt", 
                                    ifelse(Procedure.Block. == "SlowBlockProc", "slow mt", NA)))) %>%
    select(exp_name, mode, train_type, cohort,
           id, sourceId, generation, 
           block = Block,
           tap_type, tap_number = SubTrial, tap_int = Tap.dt.SubTrial., tap_ts = Tap.t.SubTrial.) %>%
    # re-number blocks by order (1,2,3) instead of ep order (1:12, separate by tap_type)
    mutate(block = ifelse(block == 1 | block == 2 | block == 3, 1,
                          ifelse(block == 6 | block == 11 | block == 12, 2,
                                 ifelse(block == 9, 3, NA)))) %>%
    na.omit() %>%
    # includes by criterion (for summary of exclusion by crit)
    # for smt & fast mt, upper bound = 3000 consistent with previous cut-offs
    # for slow mt, upper bound =  99th percentile after elim first taps
    mutate(upper_cut = ifelse(tap_type != "slow mt", 3000,
                              ifelse(tap_type == "slow mt", 6092, NA))) %>%
    mutate(tap_incl_rmfirst = ifelse(tap_number == 1, 0, 1),
           tap_incl_rm100ms = ifelse(tap_int < 100, 0, 1),
           tap_incl_rmupper = ifelse(tap_int > upper_cut, 0, 1)) %>%
    # include summary column
    mutate(tap_include = ifelse(tap_number == 1, 0, # exclude if first tap of trial
                                ifelse(tap_int < 100, 0, # exclude if tap is < 100ms
                                       ifelse(tap_int > upper_cut, 0, 1)))) # exclude if > upper bound
  
  # write all tap data to csv file for easy loading later
  write.csv(ep_mt_data, ep_mt_output)
  
  
  # check and trouble-shooting, finding cut-offs, plot checks
  
    # finding NAs
    tmp <- ep_mt_data[is.na(ep_mt_data$tap_int),]
    # 8 rows have NA
    # participants (id): 207, 208, 307, 306
    
    # checking visual program differences...esp tap indices -- seemed to match up
    table(ep_mt_data$exp_name, ep_mt_data$tap_type)
    
    # percentile cut-offs by tap_type
    ep_mt_data_filt1 <- ep_mt_data %>%
      filter(tap_number != 1) %>%
      group_by(exp_name, tap_type) %>%
      summarize(min = min(tap_int),
                per1 = quantile(tap_int, .01),
                per5 = quantile(tap_int,.05),
                per50 = quantile(tap_int, .5),
                mean = mean(tap_int),
                per95 = quantile(tap_int, .95),
                per99 = quantile(tap_int, .99),
                max = max(tap_int))         


    percentile_summary <- ep_mt_data_filt1 %>%
      group_by(tap_type) %>%
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
    
    # quick plot to view for each tap_type, tap_ints by tap_number to decide fast/slow cut-offs
    ep_mt_data %>%
      filter(tap_type == "smt") %>%
      mutate(block = as.factor(block)) %>%
      ggplot(aes(x = tap_number, y = tap_int, group = id, color = block)) +
      geom_point() +
      #geom_line() +
      coord_cartesian(ylim = c(0,6500)) +
      #scale_y_log10()+
      facet_grid(cohort~exp_name)
    
    # checking normality of data...ha not normal at all
    qqnorm(ep_mt_data$tap_int)
    
    # box plot... by block by exp_name with lines = tap_type
    ggplot(ep_mt_data, aes(x = tap_type, y = tap_int)) +
      geom_boxplot() +
      #coord_cartesian(xlim = c(0, 3000)) +
      theme_bw() + scale_y_log10() +
      facet_grid(block~exp_name)

    # quick look at distribution of taps by tap type...(from saved data set)
    taps_mt %>%
      ggplot(aes(x = tap_type, y = tap_int)) +
      geom_boxplot() + scale_y_log10()


    # hist by block by exp_name with lines = tap_type
    # turn filter on or off to check pre- and post-inclusion filtering
    ep_mt_data %>%
      filter(tap_include == 1) %>%
      ggplot(aes(tap_int, group = tap_type, color = tap_type)) +
      geom_freqpoly(binwidth = 75) +
      #coord_cartesian(xlim = c(0, 3000)) +
      theme_bw() +
      facet_grid(block~exp_name)

# ep - MT: #1: reconsidering filters (2016-10-31) --------------------
# because of P514 in a_or (7-8 second ITI in slow!)
# new upper as 1.5*median of tap_ints by participant (after excluding first tap of block)

  # try different filter boundaries...
  # exclude first tap first.
  #   taps that are > 1.5*median by tap type, avg across all blocks of the same type, by individual
    mt_newfilters <- ep_mt_data %>%
      filter(tap_number != 1) %>%
      group_by(exp_name,mode,train_type,cohort,id,generation,tap_type) %>%
      summarize(nints = n(),
                median_for_filter = median(tap_int),
                IQR3 = quantile(tap_int,.75),
                IQR1 = quantile(tap_int,.25)) %>%
      mutate(SemiIQR = 0.5*(IQR3 - IQR1),
             upper_cut_bymedian = 1.5*median_for_filter,
             lower_cut_bymedian = 0.5*median_for_filter,
             upper_bysiqr = median_for_filter + 2*SemiIQR,
             lower_bysiqr = median_for_filter - 2*SemiIQR)

    # check mean filters
    newfilters_summary <- mt_newfilters %>%
      group_by(exp_name,tap_type) %>%
      summarize(upper1 = mean(upper_cut_bymedian),
                lower1 = mean(lower_cut_bymedian),
                upper2 = mean(upper_bysiqr),
                lower2 = mean(lower_bysiqr))
    
    # check filter values for the P that started all this
    filter_for_514 <- mt_newfilters %>%
      filter(exp_name == "a_or", id == "514") %>%
      group_by(tap_type) %>%
      summarize(upper1 = mean(upper_cut_bymedian),
                lower1 = mean(lower_cut_bymedian),
                upper2 = mean(upper_bysiqr),
                lower2 = mean(lower_bysiqr))
      
  # check implementing filters - how do taps looks?
  # upper by 1.5*median is more liberal. i will try that.
  # stick with 100ms lower or also do this by participant?
    ep_mt_revised <- ep_mt_data %>%
      full_join(mt_newfilters, by = c("exp_name", "mode", "train_type", 
                                      "cohort", "id", "generation", "tap_type")) %>%
      mutate(tap_incl_rmupper_new = ifelse(tap_int > upper_cut_bymedian, 0, 1)) %>%
      # include summary column
      mutate(tap_include_new = ifelse(tap_number == 1, 0, # exclude if first tap of trial
                                  ifelse(tap_int < 100, 0, # exclude if tap is < 100ms
                                         ifelse(tap_int > upper_cut_bymedian, 0, 1))))


  # check implementing filters - how much data is excluded?
    mt_revisedfilter <- ep_mt_revised %>%
      filter(tap_include_new == 1) 
    # so far 64215-59106 = 5109 rows excluded, which is ~ 7%...starts to be alot...but maybe legit...
    
      # checking a participant. eg. a_sc id 101 seems to have a lto excluded by method 2
        #   excludes all of the first block of smt
        ep_mt_revised %>% filter(tap_include_new == 1, exp_name=="a_sc", id == 101) %>% 
          ggplot(aes(x=tap_number, y=tap_int, group=tap_type, color=tap_type)) +
          geom_point(size=3) + geom_line() + facet_grid(~block)
        # vs old -- there are some missed-looking taps
        ep_mt_revised %>% filter(tap_include == 1, exp_name=="a_sc", id == 101) %>% 
          ggplot(aes(x=tap_number, y=tap_int, group=tap_type, color=tap_type)) +
          geom_point(size=3) + geom_line() + facet_grid(~block)
        # vs no filter -- 
        ep_mt_revised %>% filter(exp_name=="a_sc", id == 101) %>% 
          ggplot(aes(x=tap_number, y=tap_int, group=tap_type, color=tap_type)) +
          geom_point(size=3) + geom_line() + facet_grid(~block)

  # some plots
    
    mt_revisedfilter %>%
      #filter(tap_type == "slow mt") %>%
      ggplot(aes(x=tap_number,y=tap_int, color=tap_type)) +
      geom_point() +
      facet_wrap(~exp_name)
    
    #compare to old filter
    ep_mt_revised %>%
      filter(tap_include==1) %>%
      ggplot(aes(x=tap_number,y=tap_int,color=tap_type)) +
      geom_point() +
      facet_grid(exp_name~cohort)
    
    ep_mt_revised %>%
      filter(tap_include==1) %>%
      ggplot(aes(x=tap_int,color=tap_type),pad=T) +
      geom_freqpoly(pad=T, binwidth=100) +
      geom_freqpoly(data=mt_revisedfilter, aes(x=tap_int,color=tap_type), linetype=2, pad=T, binwidth=100) +
      coord_cartesian(xlim=c(-50,5000)) +
      facet_grid(exp_name~cohort)
    
    # compare rought calculations old and new filter:
    mt_old_filter <- ep_mt_revised %>%
      filter(tap_include==1) %>%
      group_by(exp_name,cohort,generation,tap_type) %>%
      summarize(nints=n(),
                tempo_old = mean(tap_int),
                sd_old = sd(tap_int))
    
    mt_compare_filters <- ep_mt_revised %>%
      filter(tap_include_new==1) %>%
      group_by(exp_name,cohort,generation,tap_type) %>%
      summarize(nints=n(),
                tempo_new = mean(tap_int),
                sd_new = sd(tap_int)) %>%
      left_join(mt_old_filter, by=c("exp_name","cohort","generation","tap_type"))
      
    # plot tempo
    mt_compare_filters %>%
      ggplot(aes(x=generation,color=tap_type)) +
      geom_line(aes(y=tempo_old)) +
      geom_line(aes(y=tempo_new),linetype = 2) +
      facet_grid(exp_name~cohort)
    
    mt_compare_filters %>%
      ggplot(aes(x=tap_type,color=tap_type)) +
      geom_boxplot(aes(y=tempo_old)) +
      geom_boxplot(aes(y=tempo_new),linetype = 2) +
      facet_grid(exp_name~cohort)
    
    
    # plot cv: old - new
    mt_compare_filters %>%
      ggplot(aes(x=generation, y=(sd_old/tempo_old) - (sd_new/tempo_new),color=tap_type)) +
      geom_point(size=2) +
      #geom_line(aes(y=sd_new/tempo_new),linetype = 2) +
      facet_grid(exp_name~cohort)
      # conclusion: old variability consistently larger than new variability
    
    # plot nints: old - new
    mt_compare_filters %>%
      ggplot() +
      geom_line(aes(x=generation, y=nints.y - nints.x, color = tap_type)) +
      facet_grid(exp_name~cohort)
      # conclusion: old method kept more taps than new method




# ep - MT: #2: reconsidering filters (2016-10-31) --------------------
# method # 1 sometimes cutting too much...
# new upper as 1.5*median of tap_ints by BLOCK by participant (after excluding first tap of block)
      
    # try different filter boundaries...
      # exclude first tap first.
      #   taps that are > 1.5*median by tap type, BY block, by individual
      mt_newfilters_byblock <- ep_mt_data %>%
        filter(tap_number != 1) %>%
        group_by(exp_name,mode,train_type,cohort,id,generation,block,tap_type) %>%
        summarize(nints = n(),
                  median_for_filter = median(tap_int),
                  IQR3 = quantile(tap_int,.75),
                  IQR1 = quantile(tap_int,.25)) %>%
        mutate(SemiIQR = 0.5*(IQR3 - IQR1),
               upper_cut_bymedian = 1.75*median_for_filter,
               lower_cut_bymedian = 0.5*median_for_filter,
               upper_bysiqr = median_for_filter + 2*SemiIQR,
               lower_bysiqr = median_for_filter - 2*SemiIQR)
      
        # check mean filters
        newfiltersbyblock_summary <- mt_newfilters_byblock %>%
          group_by(exp_name,tap_type) %>%
          summarize(upper1 = mean(upper_cut_bymedian),
                    lower1 = mean(lower_cut_bymedian),
                    upper2 = mean(upper_bysiqr),
                    lower2 = mean(lower_bysiqr))
        

  # one last thing...option 2b...1.75*median instead of 1.5*median
    mt_newfilters_byblock <- ep_mt_data %>%
      filter(tap_number != 1) %>%
      group_by(exp_name,mode,train_type,cohort,id,generation,block,tap_type) %>%
      summarize(nints = n(),
                median_for_filter = median(tap_int)) %>%
      mutate(upper_cut_bymedian = 1.75*median_for_filter)
    
    summary <- mt_newfilters_byblock %>%
      group_by(exp_name, tap_type) %>%
      summarize(upper = mean(upper_cut_bymedian))


  # ***create df - revised with new filter added
    # 1.5 uppercut removes 64215 - 60568 = 3647 rows, i.e. 5.7% of all taps
    # 1.75 uppercut removes 64215 - 60783 = 3432 rows, i.e. 5.3% of all taps (is this worth it??)
    ep_mt_revised <- ep_mt_data %>%
      full_join(mt_newfilters_byblock, by = c("exp_name", "mode", "train_type", 
                                      "cohort", "id", "generation", "block","tap_type")) %>%
      mutate(tap_incl_rmupper_new = ifelse(tap_int > upper_cut_bymedian, 0, 1)) %>%
      # include summary column
      mutate(tap_include_new = ifelse(tap_number == 1, 0, # exclude if first tap of trial
                                      ifelse(tap_int < 100, 0, # exclude if tap is < 100ms
                                             ifelse(tap_int > upper_cut_bymedian, 0, 1)))) 
                                                    
    

  # try some plots...
    
    # new filter looks nice for the a_sc problem participant:
    ep_mt_revised %>%
      filter(tap_include_new == 1, exp_name == "a_sc", id=="101") %>%
      ggplot(aes(x=tap_number, y=tap_int, color=tap_type, group=tap_type)) +
      geom_point() + geom_line() + facet_grid(~block)
    
    # new filter seems ok for the original a_or problem participant:
    ep_mt_revised %>%
      filter(tap_include_new == 1, exp_name == "a_or", id=="514") %>%
      ggplot(aes(x=tap_number, y=tap_int, color=tap_type, group=tap_type)) +
      geom_point() + geom_line() + facet_grid(~block)

    # but new filter seems too permissive for this v_or problem participant:
    ep_mt_revised %>%
      filter(tap_include_new == 1, exp_name == "v_or", id=="305") %>%
      ggplot(aes(x=tap_number, y=tap_int, color=tap_type, group=tap_type)) +
      geom_point() + geom_line() + facet_grid(~block)
      # indeed...too few data points. really should be cut.

  # compare rough calculations old and new filter:
    mt_old_filter <- ep_mt_revised %>%
      filter(tap_include==1) %>%
      group_by(exp_name,cohort,generation,tap_type) %>%
      summarize(nints_old=n(),
                median_old = median(tap_int),
                IQR_old = IQR(tap_int),
                mad_old = mad(tap_int)) %>%
      mutate(cv2_old = IQR_old/median_old,
             cmv_old = mad_old/median_old)

    mt_compare_filters <- ep_mt_revised %>%
      filter(tap_include_new==1) %>%
      group_by(exp_name,cohort,generation,tap_type) %>%
      summarize(nints_new=n(),
                median_new = median(tap_int),
                IQR_new = IQR(tap_int),
                mad_new = mad(tap_int)) %>%
      mutate(cv2_new = IQR_new/median_new,
             cmv_new = mad_new/median_new) %>%
      left_join(mt_old_filter, by=c("exp_name","cohort","generation","tap_type"))

    # plot tempo
    mt_compare_filters %>%
      ggplot(aes(x=generation,color=tap_type)) +
      geom_line(aes(y=median_old)) +
      geom_line(aes(y=median_new),linetype = 2) +
      facet_grid(exp_name~cohort)
    
    mt_compare_filters %>%
      ggplot(aes(x=tap_type,color=tap_type)) +
      #geom_boxplot(aes(y=tempo_old)) +
      geom_boxplot(aes(y=median_new),linetype = 2) +
      facet_grid(exp_name~cohort)
    
        
    # plot - new cv didn't always look good. changed calculations (to check out cv2). cv2 not great?
    mt_compare_filters %>%
      ggplot(aes(x=generation, y=cmv_new, color=tap_type)) +
      geom_point(size=2) + facet_grid(exp_name~cohort)

    # plot cv2: old - new
    mt_compare_filters %>%
      ggplot(aes(x=generation, y=cv2_old - cv2_new, color = tap_type)) +
      geom_point(size=2) +
      #geom_line(aes(y=sd_new/tempo_new),linetype = 2) +
      facet_grid(exp_name~cohort)
    # conclusion: similar, but somtimes old is lower than new?
    
    # plot nints: old - new
    mt_compare_filters %>%
      ggplot() +
      geom_line(aes(x=generation, y=nints_old - nints_new, color = tap_type)) +
      facet_grid(~exp_name)
    # conclusion: old method someimtes kept more taps than new method

# ***ep - MT: #3: reconsidering filters (2016-10-31) --------------------
# methods #1 sometimes cutting too much...
# methods #2 and 2b sometimes not cutting enough...
# realized there is a nints issue. should filter by n-ints...
# new upper as 1.75*median of tap_ints by BLOCK by participant (after excluding first tap of block)
    
    # first, get a quick idea of nints by block & what those data look like
    counts <- ep_mt_data %>%
      filter(tap_number != 1) %>%
      group_by(exp_name,mode,train_type,cohort,id,generation,block,tap_type) %>%
      summarize(nints = n())
    
    low_counts <- counts[counts$nints <= 10,] # try also: 8:10 --> 11 trials omitted (vs 7 --> 7 trials)
      
    low_count_data <- low_counts %>%
      inner_join(ep_mt_data, by=c("exp_name","mode","train_type",
                                 "cohort","id","generation","block","tap_type"))
    
    # just plot low count tap interval series
    low_count_data %>% 
      #group_by(exp_name,tap_type,block,id) %>% 
      #filter(tap_int < 1.75*median(tap_int)) %>%
    ggplot(aes(x=tap_number, y=tap_int, group=id, color=id)) +
      geom_point() + geom_line() + coord_cartesian(ylim=c(0,3500)) +facet_grid(tap_type~block~exp_name)
      #facet_grid(tap_type~block~exp_name, scales="free")
      

      # ok. same upper bound as option 2a: 1.75*median
      mt_newfilters_byblock <- ep_mt_data %>%
      filter(tap_number != 1) %>%
      group_by(exp_name,mode,train_type,cohort,id,generation,block,tap_type) %>%
      summarize(nints = n(),
                median_for_filter = median(tap_int)) %>%
      mutate(upper_cut_bymedian = 1.75*median_for_filter)
    
    summary <- mt_newfilters_byblock %>%
      group_by(exp_name, tap_type) %>%
      summarize(upper = mean(upper_cut_bymedian))
  
  
  # ***create df - revised with new filter added
  # 1.5 uppercut removes 64215 - 60568 = 3647 rows, i.e. 5.7% of all taps
  # 1.75 uppercut removes 64215 - 60783 = 3432 rows, i.e. 5.3% of all taps (is this worth it??)
  # 1.75 uppercut PLUS nints > 5 removes 64215 - 60774 = 3441 rows, i.e. 5.4% of all taps
    ep_mt_revised <- ep_mt_data %>%
      # rename old cut-offs that have new replacements
      rename(upper_cut_old = upper_cut,
             tap_incl_rmupper_old = tap_incl_rmupper,
             tap_include_old = tap_include) %>%
      full_join(mt_newfilters_byblock, by = c("exp_name", "mode", "train_type", 
                                              "cohort", "id", "generation", "block","tap_type")) %>%
      mutate(tap_incl_rmupper = ifelse(tap_int > upper_cut_bymedian, 0, 1),
             tap_incl_rmnints = ifelse(nints < 5, 0, 1)) %>%
      # include summary column
      mutate(tap_include = ifelse(tap_number == 1, 0, # exclude if first tap of trial
                                      ifelse(tap_int < 100, 0, # exclude if tap is < 100ms
                                             ifelse(tap_int > upper_cut_bymedian, 0, # exclude if tap is > 1.75*median by block for individual
                                                    ifelse(nints <= 10, 0,1))))) # exclude if number of intervals that went into calculating cut-off is <= 5
                                                    
    write.csv(ep_mt_revised, "compiled data/taps_ep_mt_newfilter_2016-10-31.csv")
    
  # try some plots...
    
    # new filter looks nice for the a_sc problem participant:
    ep_mt_revised %>%
      filter(tap_include == 1, exp_name == "a_sc", id=="101") %>%
      ggplot(aes(x=tap_number, y=tap_int, color=tap_type, group=tap_type)) +
      geom_point() + geom_line() + facet_grid(~block)
    
    # new filter seems ok for the original a_or problem participant:
    ep_mt_revised %>%
      filter(tap_include == 1, exp_name == "a_or", id=="514") %>%
      ggplot(aes(x=tap_number, y=tap_int, color=tap_type, group=tap_type)) +
      geom_point() + geom_line() + facet_grid(~block)
    
    # new filter (w/nints <= 7) adequate for this v_or problem participant:
    ep_mt_revised %>%
      filter(tap_include == 1, exp_name == "v_or", id=="305") %>%
      ggplot(aes(x=tap_number, y=tap_int, color=tap_type, group=tap_type)) +
      geom_point() + geom_line() + facet_grid(~block)
    # indeed...too few data points. really should be cut.
    # without filter - faceted with free scales 
    ep_mt_revised %>%
      filter(exp_name == "v_or", id=="305") %>%
      ggplot(aes(x=tap_number, y=tap_int, color=tap_type, group=tap_type)) +
      geom_point() + geom_line() + facet_grid(tap_type~block, scale="free")
    
    
    # compare rough calculations old and new filter:
    mt_old_filter <- ep_mt_revised %>%
      filter(tap_include_old==1) %>%
      group_by(exp_name,cohort,generation,tap_type) %>%
      summarize(nints_old=n(),
                median_old = median(tap_int),
                IQR_old = IQR(tap_int),
                mad_old = mad(tap_int)) %>%
      mutate(cv2_old = IQR_old/median_old,
             cmv_old = mad_old/median_old)
    
    mt_compare_filters <- ep_mt_revised %>%
      filter(tap_include==1) %>%
      group_by(exp_name,cohort,generation,tap_type) %>%
      summarize(nints_new=n(),
                median_new = median(tap_int),
                IQR_new = IQR(tap_int),
                mad_new = mad(tap_int)) %>%
      mutate(cv2_new = IQR_new/median_new,
             cmv_new = mad_new/median_new) %>%
      left_join(mt_old_filter, by=c("exp_name","cohort","generation","tap_type"))
    
    # plot tempo
    mt_compare_filters %>%
      ggplot(aes(x=generation,color=tap_type)) +
      geom_line(aes(y=median_old)) +
      geom_line(aes(y=median_new),linetype = 2) +
      facet_grid(exp_name~cohort)
    
    # plot - new cv2 STILL doesn't always look good?
    mt_compare_filters %>%
      ggplot(aes(x=generation, y=cv2_new, color=tap_type)) +
      geom_point(size=2) + facet_grid(exp_name~cohort)
    
    # plot cv2: old - new
    mt_compare_filters %>%
      ggplot(aes(x=generation, y=cv2_old - cv2_new, color = tap_type)) +
      geom_point(size=2) +
      #geom_line(aes(y=sd_new/tempo_new),linetype = 2) +
      facet_grid(exp_name~cohort)
    # conclusion: similar, but somtimes old is lower than new?
    
    # plot nints: old - new
    mt_compare_filters %>%
      ggplot() +
      geom_line(aes(x=generation, y=nints_old - nints_new, color = tap_type)) +
      facet_grid(~exp_name)



# ep - MT: summary BY BLOCK calculate mean, median, std, cov for individual--------------------
# i.e., summarize over tap intervals
# Plot checks not updated 10-06-16
# redoing 2016-10-19 to fix cv, add se, counts, etc.
# redoing 2016-10-31 with new filter (and fewer measures)
  
  # new:
  ep_mt_summary <- ep_mt_revised %>% #using new filter 10-31-2016
    filter(tap_include == 1) %>% 
    group_by(exp_name, mode, train_type, cohort, id, generation, 
             block, tap_type) %>% 
    summarize(nints = n(),
              mean_tempo = mean(tap_int, na.rm = TRUE),
              sd_taps = sd(tap_int, na.rm = TRUE),
              median_tempo = median(tap_int, na.rm = TRUE), 
              mad_taps = median(abs(tap_int - median_tempo)),
              mad_taps2 = mad(tap_int),
              IQR = quantile(tap_int, .75) - quantile(tap_int, .25),
              SIQR = .5*(quantile(tap_int, .75) - quantile(tap_int, .25))) %>%            
    mutate(se = sd_taps/sqrt(nints),
           cov = sd_taps/mean_tempo,
           cov2 = SIQR/median_tempo,
           comv = mad_taps2/median_tempo)
  
  write.csv(ep_mt_summary, "compiled data/summary_ep_mt_2016-10-31.csv")

  #old:
  #ep_mt_summary <- taps_mt %>%
  ep_mt_summary <- ep_mt_data %>% 
    filter(tap_include == 1) %>% 
    group_by(exp_name, mode, train_type, cohort, generation, 
             block, tap_type) %>% 
    summarize(nints = n(),
              mean_tempo = mean(tap_int, na.rm = TRUE),
              sd_taps = sd(tap_int, na.rm = TRUE),
              median_tempo = median(tap_int, na.rm = TRUE), 
              mad_taps = median(abs(tap_int - median_tempo)),
              IQR = quantile(tap_int, .75) - quantile(tap_int, .25),
              SIQR = .5*(quantile(tap_int, .75) - quantile(tap_int, .25))) %>%            
    mutate(se = sd_taps/sqrt(nints),
           cov = sd_taps/mean_tempo,
           cov2 = SIQR/median_tempo,
           comv = mad_taps/median_tempo)
  
  #write.csv(ep_mt_summary, ep_mt_summary_output)
  write.csv(ep_mt_summary, "compiled data/summary_ep_mt_2016-10-27.csv")



  # check of how many tempi are based on very few intervals (motivated by CVs still looking large...)
    table((ep_mt_summary$nints))
    # unsure whether this merits filtering more...

  # box plot for cv2
  ep_mt_summary %>%
    ggplot(aes(x=tap_type, y=cov2)) +
    geom_boxplot() + scale_y_log10()


  # some spaghetti plot checks
  
  #mean
  ggplot(ep_mt_summary, aes(x = generation, y = mean_tempo, group = cohort, color = tap_type)) +
    geom_point() + 
    #geom_line() +
    facet_grid(exp_name~block) + 
    theme_bw()
  
  #median
  ggplot(ep_mt_summary, aes(x = generation, y = median_tempo, group = cohort, color = tap_type)) +
    geom_point() + 
    geom_line() +
    facet_grid(exp_name~block) + 
    theme_bw()
  
  # another median check
  ggplot(ep_sync_summary, aes(x = generation, y = median_tempo, group = chain_name, color = seed)) +
    geom_point() +
    facet_grid(.~tap_type)
  
  #cov
  ggplot(ep_mt_summary, aes(x = as.factor(block), y = cov, group = tap_type, color = tap_type)) +
    geom_point(size = 3) + #geom_line() +
    theme_bw() +
    facet_grid(exp_name~cohort)
  
  #cov2
  ggplot(ep_mt_summary, aes(x = as.factor(block), y = cov2, group = tap_type, color = tap_type)) +
    geom_point() + #geom_line() +
    facet_grid(exp_name~cohort) + 
    theme_bw()
  
  # another comv check
  ggplot(ep_mt_summary, aes(x = generation, y = comv, group = tap_type, color = tap_type)) +
    geom_point() +
    facet_grid(tap_type~cohort)

# ep - MT summary: OVER BLOCKS calculate mean, median, std, cov by individual ------------------------
# i.e., summarize over blocks
# update again 2016-10-31 -- tempo & cv as individual measures to be summarized over (so not worrying about sd and se of these measures)
# plot checks not updated 10-06-16
# redoing 2016-10-19 to fix cv, add se, counts, etc.
# may want to use wav. for covs, need to calculate their own wav_cov to replace mean_cov or whatever.
  

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

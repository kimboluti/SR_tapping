
# set up------------------------------

  # Set directory 
    
    #setwd("C:/Users/Ellie/Google Drive/experiments & projects/iterated tapping - tempo/ANALYSIS - 10-2016 (diss)/Experiment 1 (Artprize)")
    original_directory <- getwd()
  
  # Load libraries 
      library(stringr)
      library(tidyr)
      library(ggplot2)
      library(plyr)
      
      library(dplyr) # load dplyr after plyr 

  # input file names
      # TAPS
      taps_input <- "compiled data/taps_sr_2016-10-03.csv"
      taps_summary_input <- "compiled data/summary_sr_2016-10-17.csv"
        # to load summary with count by gen, read "compiled data/summary_sr_withcounts_2016-10-17.csv"
        # but count is the same == 11 -- for each gen.

      # SMT
      taps_smt_input <- "compiled data/taps_smt_2016-10-03.csv"
      taps_smt_summary_input <- "compiled data/summary_smt_2016-10-17.csv"

      # TAPS - PRACTICE (if used in the future)
      taps_sr_prac_input <- "compiled data/taps_sr_prac_2016-10-03.csv"  
      taps_sr_prac_summary_input <- "compiled data/summary_sr_prac_2016-10-17.csv"
      
      # DEMOGRAPHICS
      demo_input <- "compiled data/demographics_2016-10-14.csv"

  # tables: output file names
      smt_bychain_output <- "tables/smt_bychain_table_2016-10-17.csv"
      smt_byseed_output <- "tables/smt_byseed_table_2016-10-17.csv"

      taps_bychain_output <- "tables/taps_bychain_table_2016-10-17.csv"
      taps_byseed_output <- "tables/taps_byseed_table_2016-10-17.csv"
  
      tempo_bychain_output <- "tables/tempo_bychain_table_2016-10-17.csv" # for reporting
      tempo_byseed_output <- "tables/tempo_byseed_table_2016-10-17.csv"  # for reporting

      drift_lm_output <- "tables/drift_lm_table_2016-10-17.csv"
      # also output (names in script): tempo_return, lm_ar1, lm_ar1_bias

      # to be updated - info with descriptives, tables of data by condition of interest
      # info with linear fits - by spaghetti and by drift

      taps_output <- "tables/taps_sr_2016-10-16.csv"
      taps_sr_prac_output <- "compiled data/taps_sr_prac_2016-10-16.csv"
      taps_smt_output <- "compiled data/taps_smt_2016-10-16.csv"
      
      taps_summary_output <- "compiled data/summary_sr_2016-10-17.csv"
      taps_sr_prac_summary_output <- "compiled data/summary_sr_prac_2016-10-17.csv"
      taps_smt_summary_output <- "compiled data/summary_smt_2016-10-17.csv"

      demo_bychain_output <- "tables/demo_bychain.csv"
      demo_byseed_output <- "tables/demo_byseed.csv"
      
# TAPS: load auditory tempo data & clean up------------------------------
    
    taps_at_raw <- read.csv(taps_input)
    taps_at <- taps_at_raw %>%
      mutate(seed = as.factor(seed)) %>%
      mutate(seed_int = ifelse(seed == 0, 300, 
                               ifelse(seed == 1, 600, 
                                      ifelse(seed == 2, 900, 
                                             ifelse(seed == 3, 1200, NA))))) %>%
      #select(filename, chain_name, seed , seed_int, generation = chain, tap_number, tap_int) 
      select(-X)
  
    taps_summary_raw <- read.csv(taps_summary_input)
    taps_summary <- taps_summary_raw %>%
      mutate(seed = as.factor(seed)) %>%
      mutate(seed_int = ifelse(seed == 0, 300, 
                               ifelse(seed == 1, 600, 
                                      ifelse(seed == 2, 900, 
                                             ifelse(seed == 3, 1200, NA))))) %>%
      #select(filename, chain_name, seed , seed_int, generation = chain, tap_number, tap_int) 
      select(-X)

# SMT: load data from auditory tempo Ps & clean up------------------------------
    
    taps_smt_raw <- read.csv(taps_smt_input)
    taps_smt <- taps_smt_raw %>%
      mutate(seed = as.factor(seed)) %>%
      mutate(seed_int = ifelse(seed == 0, 300, 
                               ifelse(seed == 1, 600, 
                                      ifelse(seed == 2, 900, 
                                             ifelse(seed == 3, 1200, NA))))) %>%
      # see plots section for percentile summaries. cut-off based ~ percentiles + plots:
      mutate(include = ifelse(tap_int >= 2000, 0, 1)) %>%
      #select(filename, chain_name, seed , seed_int, generation = chain, tap_number, tap_int) 
      select(-X)
    
    # read from original composed summary:
    # taps_smt_summary_raw <- read.csv(taps_smt_summary_input)
#       taps_smt_summary <- taps_smt_summary_raw %>%
#         mutate(seed = as.factor(seed)) %>%
#         mutate(seed_int = ifelse(seed == 0, 300, 
#                                  ifelse(seed == 1, 600, 
#                                         ifelse(seed == 2, 900, 
#                                                ifelse(seed == 3, 1200, NA))))) %>%
#         #select(filename, chain_name, seed , seed_int, generation = chain, tap_number, tap_int) 
#         select(-X)


    # or revised summary from filtered taps:
    taps_smt_summary <- taps_smt %>%
      filter(rejects == "n", include == 1) %>%
      mutate(seed = as.factor(seed)) %>%
      mutate(seed_int = ifelse(seed == 0, 300, 
                               ifelse(seed == 1, 600, 
                                      ifelse(seed == 2, 900, 
                                             ifelse(seed == 3, 1200, NA))))) %>%
      group_by(mode,type,seed,seed_int,chain_name,
                   filename, starter, id, sourceId, generation) %>% 
      summarize(mean_tempo = mean(tap_int),
                sd_taps = sd(tap_int),
                median_tempo = median(tap_int), 
                mad_taps = median(abs(tap_int - median_tempo))) %>%
      mutate(cov = sd_taps/mean_tempo,
             comv = mad_taps/median_tempo) %>%
      select(mode, type, seed, seed_int, chain_name,
             filename, starter, id, sourceId, generation,
             mean_tempo, sd_taps, cov,
             median_tempo, mad_taps, comv) %>%
      arrange(chain_name, generation)
   
               
    
           

# TAPS: plot individual intervals by gen by seed plots for inspection------------------------------
        # inspection: for each seed individual tap intervals by index
            ggplot(taps_at, aes(x = tap_number, y = tap_int, group = seed, color = seed)) +
              geom_point() +
              #geom_line() +
              facet_grid(.~seed_int)
        
        # inspection: tap intervals by index. separate by seed for rows, generation in columns  
            ggplot(taps_at, aes(x = tap_number, y = tap_int, group = seed, color = chain_name)) +
              geom_point() +
              #geom_line() +
              facet_grid(seed_int~generation)

        # individual tap intervals: by tap_number, separate by seed for rows, generation in columns, color chain  
        ggplot(taps_at, aes(x = tap_number, y = tap_int, group = chain_name, color = chain_name)) +
          geom_point(size = 2) +
          geom_line() + # turn on for inspection but not presentation
          labs(title = 'Auditory tempo: individual taps across generations by seed', 
               x = 'tap index', y = 'tap interval (ms)') + 
          theme(plot.title = element_text(lineheight=.8, face="bold"),
                panel.grid.minor=element_blank(),
                panel.grid.major=element_blank(),
                axis.title.x = element_text(size=20),
                axis.text.x = element_text(size = 16),
                axis.title.y  = element_text(size =20),
                axis.text.x = element_text(size = 16)) +
          coord_cartesian(ylim=c(0, 2500), xlim=c(0,11)) +        
          facet_grid(seed_int~generation)
                # to move facet labels to axis and remove boxes, ad this after 'generation':        
                           #, switch = "both") +
                #theme(strip.background = element_blank())

        # inspection: tap intervals not separated by chain (kind of recreating spaghetti drift plot...)           
        ggplot(taps_at, aes(x = tap_number, y = tap_int, group = chain_name, color = seed)) +
          geom_point(size = 3) +
          #geom_line() + # turn on for inspection but not presentation
          labs(title = 'Auditory tempo: individual taps across generations by seed', 
               x = 'tap index', y = 'tap interval (ms)') + 
          theme(panel.grid.minor=element_blank(),
                panel.grid.major=element_blank()) +
          facet_grid(.~generation)

# SMT: plot individual tap intervals -------------------------------
# plots for checking (and revising filters)
# summary tables and plots for reporting at bottom of script
  
  # individual tap intervals: by tap_number, separate by seed for rows, generation in columns, color chain  
        ggplot(taps_smt, aes(x = tap_number, y = tap_int, group = chain_name, color = chain_name)) +
          geom_point(size = 2) +
          geom_line() + # turn on for inspection but not presentation
          labs(title = 'SMT for auditory tempo: individual taps across generations by seed', 
               x = 'tap index', y = 'tap interval (ms)') + 
          theme(plot.title = element_text(lineheight=.8, face="bold"),
                panel.grid.minor=element_blank(),
                panel.grid.major=element_blank(),
                axis.title.x = element_text(size=20),
                axis.text.x = element_text(size = 16),
                axis.title.y  = element_text(size =20),
                axis.text.x = element_text(size = 16)) +
          coord_cartesian(ylim=c(0, 2500), xlim=c(0,11)) +        
          facet_grid(seed_int~generation)
        # to move facet labels to axis and remove boxes, ad this after 'generation':        
        #, switch = "both") +
        #theme(strip.background = element_blank())


    # consider removing taps that are > 2SD from mean?
    bychain_percentiles <- taps_smt %>%
      filter(rejects == "n") %>%
      group_by(mode,type,seed,chain_name) %>%
      summarize(min = min(tap_int),
                per1 = quantile(tap_int, .01),
                per5 = quantile(tap_int,.05),
                per50 = quantile(tap_int, .5),
                mean = mean(tap_int),
                per95 = quantile(tap_int, .95),
                per98 = quantile(tap_int, .98),
                per99 = quantile(tap_int, .99),
                max = max(tap_int),
                IQR = quantile(tap_int, .75) - quantile(tap_int, .25),
                SD = sd(tap_int),
                sd2cut = 2*sd(tap_int))
                
    taps_smt_filt %>%
      filter(rejects == "n") %>%
      ggplot(aes(x = tap_number, y = tap_int, group = chain_name, color = chain_name)) +
      geom_point(size = 2) +
      geom_line() + # turn on for inspection but not presentation
      labs(title = 'SMT for auditory tempo: individual taps across generations by seed', 
           x = 'tap index', y = 'tap interval (ms)') + 
      theme(plot.title = element_text(lineheight=.8, face="bold"),
            panel.grid.minor=element_blank(),
            panel.grid.major=element_blank(),
            axis.title.x = element_text(size=20),
            axis.text.x = element_text(size = 16),
            axis.title.y  = element_text(size =20),
            axis.text.x = element_text(size = 16)) +
      #coord_cartesian(ylim=c(0, 2500), xlim=c(0,11)) +        
      facet_grid(seed_int~generation)
    
    ggplot(taps_smt_summary_filt, aes(x = generation, y = cov, group = chain_name, color = seed)) +
      geom_point() +
      geom_line() +
      geom_smooth(method = lm, se = FALSE)
      facet_grid(seed~.)
    
    # plotting after 2000 filter
    ggplot(taps_smt_summary, aes(x = generation, y = median_tempo, group = chain_name, color = seed)) +
      geom_point() +
      geom_line()
    
    ggplot(taps_smt_summary, aes(x = generation, y = sd_taps, group = chain_name, color = seed)) +
      geom_point() +
      geom_line() +
      facet_grid(seed~.)

  # individual tap int by generation
    taps_smt %>% filter(rejects == "n", include == 1) %>%
      ggplot(aes(x = tap_number, y = tap_int, group = chain_name, color = seed)) +
      geom_point() + geom_line () +
      facet_grid(seed~generation)
  

  # mean and cv by generation
  ggplot(taps_smt_summary, aes(x = generation, y = mean_tempo, group = chain_name, color = seed)) +
    geom_point() + geom_line () +
    geom_smooth(method = lm, se = FALSE)
  
  ggplot(taps_smt_summary, aes(x = generation, y = median_tempo, group = chain_name, color = seed)) +
    geom_point() + geom_line () +
    geom_smooth(aes(group = seed), method = lm, se = FALSE)
  
  ggplot(taps_smt_summary, aes(x = generation, y = cov, group = chain_name, color = seed)) +
    geom_point() + geom_line () +
    geom_smooth(aes(group = seed), method = lm, se = FALSE)
  
    
# SMT: quick review -------------------------------
# summary tables and plots for reporting at bottom of script

  # WORKING: goal here -- counts to know how much data was excluded by filter  
    smt_total <- taps_smt %>%
      filter(rejects == "n") %>%
      group_by(filename) %>%
      summarize(total = n()) 
    
    # why do these filenames have more than 11 taps associated with smt?
    test <- smt_total %>%
      filter(total > 11)
      
    smt_count <- taps_smt %>%  
      filter(rejects == "n", include == 1) %>%
      group_by(filename) %>%
      summarize(filtered = n()) %>%
      left_join(smt_total, by = "filename")

  # some distribution checks...to be further pursued in the future?
    ggplot(taps_smt_summary, aes(median_tempo, fill = seed)) +
      #geom_histogram(binwidth = 50, position = "dodge") +
      geom_bar(stat="bin",bins = 25) +
      facet_grid(seed~.) +
      theme_bw()
    
    ggplot(taps_smt_summary, aes(median_tempo, group = chain_name, color = chain_name)) +
      geom_freqpoly(binwidth = 75) +
      facet_grid(seed~.) +
      theme_bw()
    
    # with new cutoff, mean and median look pretty similar:
    ggplot(taps_smt_summary) +
      geom_freqpoly(aes(mean_tempo), linetype = 19, binwidth = 50) +
      geom_freqpoly(aes(median_tempo), linetype = 2, binwidth = 50) +
      facet_grid(seed~.)



# TAPS: summary measure tables (quick summary...by chains/seeds) -------------------------------
# NOT CHECKED FOR POOLED VARIANCE CALCUATION
# confusing -- is this for smt or sr?
# for reference, not reporting - summarizes means overall all generations
# later TAP SUMMARY tables summarize means for specific generations (e.g., first, final)

      taps_bychain_table <- taps_smt_summary %>%
        group_by(mode,type,seed,seed_int,chain_name) %>%
        summarize(mean_smt = mean(median_tempo),
                  mean_meansmt = mean(mean_tempo),
                  mean_sd = mean(sd_taps),
                  mean_mad = mean(mad_taps),
                  mean_cov = mean(cov),
                  mean_comv = mean(comv))

      write.csv(taps_bychain_table, taps_bychain_output)
      

      taps_byseed_table <- taps_bychain_table %>%
        group_by(mode,type,seed,seed_int) %>%
        summarize(mean_smt_seed = mean(mean_smt),
                  mean_meansmt_seed = mean(mean_meansmt),
                  mean_sd_seed = mean(mean_sd),
                  mean_mad_seed = mean(mean_mad),
                  mean_cov_seed = mean(mean_cov),
                  mean_comv_seed = mean(mean_comv))

      write.csv(taps_byseed_table, taps_byseed_output)

# TAPS: plots - accuracy ---------

  acc <- taps_at %>%
    filter(rejects == "n") %>%
    group_by(mode,type,seed,chain_name,generation) %>%
    summarize(mean_diff = mean(tap_diff),
              sd_diff = sd(tap_diff)) %>%
    mutate(cv_diff = sd_diff/mean_diff)
    
  ggplot(acc, aes(x = generation, y = mean_diff, group = chain_name, color = seed)) +
    geom_point()
  
  ggplot(acc, aes(x = generation, y = sd_diff, group = chain_name, color = seed)) +
    geom_point() + geom_line()
  
  ggplot(acc, aes(x = generation, y = cv_diff, group = chain_name, color = seed)) +
    geom_point() + geom_line() +
    coord_cartesian(ylim = c(-100,100))


# TAPS: plots - compare distribution of seed intervals to distribution of final tap intervals---------
# not sure this is useful at all

    # seed intervals data frame from spaghetti section            
    # for each chain, create rows for a seed participant
    # expect 11 intervals * 13 chains = 143 rows
    seeds <- taps_at %>%
      group_by(chain_name, seed, seed_int, tap_number) %>%
      summarize(tap_int = mean(seed_int)) %>%
      mutate(generation = 'a_seed') %>%
      select(chain_name, seed, seed_int, generation, tap_number, tap_int)
    
    ggplot(seeds, aes(x = tap_number, y = tap_int, group = seed, color = seed)) +
      geom_point()
    
    # final tap intervals data frame (also expecting 143 rows)
    finals <- taps_at %>%
      group_by(chain_name, seed, seed_int, tap_number) %>%
      mutate(max_gen = max(generation)) %>%
      filter(generation == max_gen) %>%
      mutate(generation = 'b_final') %>%
      select(chain_name, seed, seed_int, generation, tap_number, tap_int)
    
    ggplot(finals, aes(x = tap_number, y = tap_int, group = seed, color = seed)) +
      geom_point()
    
    # combine dfs (expect 286 rows)
    compare_taps <- seeds %>%
      bind_rows(finals)
    
    # plot ... left col seed right col final. colors for seed for both
    ggplot(compare_taps, aes(x = tap_number, y = tap_int, group = chain_name, color = seed)) +
      geom_point(alpha = .75, size = 3) +
      geom_line() + 
      theme(panel.grid.minor=element_blank(),
            panel.grid.major=element_blank()) +            
      facet_grid(seed~generation)
    
    # find max count in each distribution to find tempo?
    # maybe that's basically the same as finding like a mean.
    
    # KEEP WORKING ON THIS: histogram of taps 
    ggplot(compare_taps, aes(tap_int, group = generation, color = generation)) +
      geom_freqpoly(binwidth = 75) +
      facet_grid(seed~.)  
    
    # counts - difficult to compare across seeds and data...
    ggplot(compare_taps, aes(x = tap_int, fill = seed)) +
      geom_histogram(bins = 75, alpha = .5, position = 'identity') +
      facet_grid(seed~generation)
    
    # these density plots look really cool but seem like they distort info?
    ggplot(compare_taps, aes(x = tap_int, group = generation, fill = generation)) +
      geom_density(alpha=.3) +
      facet_grid(seed~.)


# TAP SUMMARY: plots to compare mean to median -------------------------------
# (for review only: sd to mad, and cov to comv)
  
  # compare frequency distributions of mean and median
  # (change group= and color= to view by chain_name or by seed)
  ggplot(taps_summary, aes(group = seed, color = seed)) +
    geom_freqpoly(aes(mean_tempo), binwidth = 75) +
    geom_freqpoly(aes(median_tempo), binwidth = 75, linetype = 2) +
    facet_grid(seed~.)

  # compare frequency distributions of means and medians
  # overall
  ggplot(taps_summary, aes()) +
    geom_freqpoly(aes(mean_tempo), binwidth = 75) +
    geom_freqpoly(aes(median_tempo), binwidth = 75, linetype = 2) +
    labs(title = 'comparison of mean (solid) and median (dashed) distributions', 
       x = 'tempo (ms)', y = 'count') + 
    theme_bw() +
    theme(panel.grid.minor=element_blank(),
          panel.grid.major=element_blank()) 
  
  # alt comparison - plotting one against the other
  ggplot(taps_summary, aes(x = mean_tempo, y = median_tempo, group = seed, color= seed)) +
    geom_point() +
    geom_line(aes(x = mean_tempo, y = mean_tempo)) + # is this a legit way to plot a slope = 1 line?
    labs(title = 'comparison of mean and median distributions', 
         x = 'MEAN tempo (ms)', y = 'MEDIAN tempo (ms)') + 
    theme_bw() +
    theme(panel.grid.minor=element_blank(),
          panel.grid.major=element_blank()) 

    # VARIABILITY distribution comparisons
    # sd and mad
    ggplot(taps_summary, aes(group = seed, color = seed)) +
      geom_freqpoly(aes(sd_taps), binwidth = 50) +
      geom_freqpoly(aes(mad_taps), binwidth = 50, linetype = 2) +
      labs(title = 'comparison of sd (solid) and mad (dashed) distributions', 
           x = 'variability (ms)', y = 'count') + 
      theme_bw() +
      theme(panel.grid.minor=element_blank(),
            panel.grid.major=element_blank()) +
      facet_grid(seed~.)

    # cov and comv
    ggplot(taps_summary, aes(group = seed, color = seed)) +
      geom_freqpoly(aes(cov), binwidth = 10) +
      geom_freqpoly(aes(comv), binwidth = 10, linetype = 2) +
      labs(title = 'comparison of cov (solid) and comv (dashed) distributions', 
           x = 'standardized variability', y = 'count') + 
      theme_bw() +
      theme(panel.grid.minor=element_blank(),
            panel.grid.major=element_blank()) +
      facet_grid(seed~.)    


  # alt comparison - plotting difference between measures
  
    # add variables that have absolute difference between mean and median
    dev <- taps_summary %>%
      mutate(central = abs(mean_tempo - median_tempo),
             var = abs(sd_taps - mad_taps),
             coeffvar = abs(cov - comv))  

    # central: mean - median (supposing 25ms is negligible difference)
    ggplot(dev, aes(x = central, group = seed, color = seed, fill = seed)) +
      #geom_histogram(binwidth = 25) + # as bare count
      geom_histogram(aes(y=..count../sum(..count..)), binwidth = 25) + # as proportion of total
      #geom_freqpoly(binwidth = 50, pad = TRUE) +
      theme_bw() +
      facet_grid(seed~.)
    
    # var: sd - mad (not sure of meaningful bin size here...)
    ggplot(dev, aes(var, group = seed, color = seed, fill = seed)) +
      geom_histogram(binwidth = 10) +
      theme_bw() +
      facet_grid(seed~.)
    
    ggplot(dev, aes(coeffvar, group = seed, color = seed, fill = seed)) +
      #geom_histogram(binwidth = 10) +
      stat_bin(binwidth = 10)+
      theme_bw() +
      facet_grid(seed~.)
      

  # other checks of normality...
    qqnorm(taps_summary$cov)
    qqnorm(taps_summary$comv)

    # mad against sd
    ggplot(taps_summary, aes(x = sd_taps, y = mad_taps, group = seed, color = seed)) +
      geom_point() +
      geom_line(aes(x = sd_taps, y = sd_taps)) +
      theme_bw()
    
    # comv against cov
    ggplot(taps_summary, aes(x = cov, y = comv, group = seed, color = seed)) +
      geom_point() +
      geom_line(aes(x = cov, y = cov)) +      
      theme_bw()

# TAP SUMMARY: prep data for - spaghetti (drift)------------------------------------------
# 

    # for spaghetti plot, need to add seeds to summary df
    seeds_summary <- taps_summary %>%
      group_by(chain_name,
               mode,type,seed,seed_int) %>%
      summarize(mean_tempo = mean(seed_int),
                sd_taps = sd(seed_int),
                cov = sd_taps/mean_tempo,
                median_tempo = median(seed_int),
                mad_taps = median(abs(seed_int - median(seed_int))),
                comv = mad_taps/median_tempo) %>%
      mutate(generation = 0,
             starter = 0,
             id = ifelse(seed == 0, "0t", 
                         ifelse(seed == 1, "1t",
                                ifelse(seed == 2, "2t",
                                       ifelse(seed == 3, "3t", NA)))),
             sourceId = "seed",
             filename = "seed") %>%
      select(mode,type,seed,chain_name,filename,starter,id,sourceId,generation,
             mean_tempo,sd_taps,cov,median_tempo,mad_taps,comv,seed_int)
    
    taps_and_seeds_summary <- taps_summary %>%
      mutate(id = as.character(id)) %>%
      bind_rows(seeds_summary)

# TAP SUMMARY: plots - spaghetti (drift)------------------------------------------

    # 2016-11-20 revision *** MEDIAN SPAGHETTI DRIFT PLOT ***
    # *(1) spaghetti with tempo by condition 
    taps_and_seeds_summary %>%
      ggplot(aes(x=generation, y=median_tempo, group=chain_name)) +
      geom_line(alpha = .25, lwd = 1, color = "#999999") +
      geom_smooth(aes(group=seed, color=as.factor(seed_int)), lwd = 1.5, alpha = .75, se = F) +
      #geom_hline(yintercept=414, linetype=2, alpha=.95) +
      theme_bw() +
      labs(title = '(A) Serial Reproduction Chains',
           x = 'Iteration', 
           y = 'Tempo (ms)') +
      theme(panel.grid.minor=element_blank(),
            panel.grid.major=element_blank(),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 11),
            #axis.text.x = element_text(angle = 90),
            #legend.position='none', #c(.9,.9) # 'none'
            legend.title = element_text(size=12),
            legend.text = element_text(size = 11)) +
      scale_color_discrete(name="Seed\nTempo (ms)") +
      scale_x_continuous(limits=c(-1,20), breaks=c(0,5,10,15,20)) +  
      scale_y_continuous(limits=c(0,1250), breaks=c(0,300,600,900,1200)) #+  
      #facet_grid(.~train_type) 
    
    # save the plot!
    ggsave("tempo_spag_2016-11-20.png", width=4.25, height=3, dpi=100)


      # *** MEDIAN SPAGHETTI DRIFT PLOT ***
      # change in produced rate each generation (similar to old excel plot)
      ggplot(taps_and_seeds_summary, aes(x = generation, y = median_tempo, group = chain_name, color = seed)) +
          geom_point(size = 3, alpha = .75) + 
          geom_line(alpha = .5, lwd = 2) +
          theme_bw() +
          labs(x = 'generation (position in chain)', 
               y = 'median tempo (ms)') + 
          theme(panel.grid.minor=element_blank(),
                panel.grid.major=element_blank(),
                axis.title = element_text(size = 26),
                axis.text = element_text(size = 22),
                legend.position='right', #c(.9,.9)
                legend.title = element_text(size=22),
                legend.text = element_text(size = 16)) +
                scale_x_continuous(breaks=seq(-1,18,1)) +
                scale_y_continuous(breaks=c(300,600,900,1200)) +
                scale_color_discrete(name="seed tempo (ms)",
                breaks=c('0','1','2','3'),
                labels=c("300", "600", "900", "1200"))

    # *** MEAN SPAGHETTI DRIFT PLOT ***
    # change in MEAN tempo
    ggplot(taps_and_seeds_summary, aes(x = generation, y = mean_tempo, group = chain_name, color = seed)) +
      geom_point(size = 3, alpha = .75) + 
      geom_line(alpha = .5, lwd = 2) +
      theme_bw() +
      labs(x = 'generation', 
           y = 'mean tempo (ms)') + 
      theme(panel.grid.minor=element_blank(),
            panel.grid.major=element_blank(),
            axis.title = element_text(size = 26),
            axis.text = element_text(size = 22),
            legend.position='right', #c(.9,.9)
            legend.title = element_text(size=22),
            legend.text = element_text(size = 16)) +
      scale_x_continuous(breaks=seq(-1,18,1)) +
      scale_y_continuous(breaks=c(300,600,900,1200)) +
      scale_color_discrete(name="seed tempo (ms)",
                           breaks=c('0','1','2','3'),
                           labels=c("300", "600", "900", "1200"))
                
    # *** MAD - VARIABILITY SPAGHETTI DRIFT PLOT ***
    # change in coefficient of median absolute deviation each generation
    ggplot(taps_summary, aes(x = generation, y = comv, group = chain_name, color = seed)) +
      geom_point(size = 3, alpha = .75) + 
      geom_smooth(aes(x = generation, y = comv, group = seed, color = seed),
                  method = lm, se=FALSE, size = 1) +
      #geom_line(alpha = .5, lwd = 2) +
      theme_bw() +
      labs(x = 'generation (position in chain)', 
           y = 'coefficient of median abs dev') + 
      theme(panel.grid.minor=element_blank(),
            panel.grid.major=element_blank(),
            axis.title = element_text(size = 26),
            axis.text = element_text(size = 22),
            legend.position='right', #c(.9,.9)
            legend.title = element_text(size=22),
            legend.text = element_text(size = 16)) +
      scale_x_continuous(breaks=seq(-1,18,1)) +
      #scale_y_continuous(breaks=c(300,600,900,1200)) +
      scale_color_discrete(name="seed tempo (ms)",
                           breaks=c('0','1','2','3'),
                           labels=c("300", "600", "900", "1200"))                 

    # *** CV - VARIABILITY SPAGHETTI DRIFT PLOT ***
    # change in cov each generation
    ggplot(taps_summary, aes(x = generation, y = cov, group = chain_name, color = seed)) +
      geom_point(size = 3, alpha = .75) + 
      # trying some other functional relationships between y and x...alt: y ~ poly(x,2)
      # maybe need to facet by seed to see what's going on in each chain better...
      #geom_smooth(aes(x = generation, y = comv, group = seed, color = seed),
       #           method = lm, formula = y ~ log(x), se=FALSE, size = 1) +
      geom_line(alpha = .5, lwd = 2) +
      theme_bw() +
      labs(x = 'generation', 
           y = 'CV') + 
      theme(panel.grid.minor=element_blank(),
            panel.grid.major=element_blank(),
            axis.title = element_text(size = 26),
            axis.text = element_text(size = 22),
            legend.position='right', #c(.9,.9)
            legend.title = element_text(size=22),
            legend.text = element_text(size = 16)) +
      scale_x_continuous(breaks=seq(-1,18,1)) +
      #scale_y_continuous(breaks=c(300,600,900,1200)) +
      scale_color_discrete(name="seed tempo (ms)",
                           breaks=c('0','1','2','3'),
                           labels=c("300", "600", "900", "1200"))                 
    

      # other variability plots...
              # cov / comv from taps summary
                  ggplot(taps_summary, aes(x = generation, y = mad_taps, group = seed, color = seed)) +
                    geom_point() + 
                    geom_smooth(alpha = .2) +
                    #facet_grid(type~mode) + 
                    theme_bw()

              # quick view - scalar property observed? (x = mean, y = sd or with medians)
                  ggplot(taps_summary, aes(x = median_tempo, y = mad_taps, group = seed, color = seed)) +
                    geom_point() + 
                    geom_smooth() +
                    #facet_grid(type~mode) + 
                    theme_bw()
                  
# TAP SUMMARY: spaghetti - linear fits------------------------------
# linear coefficients for spaghetti (tempo by generation) by chain_name 
# creates dataframe to save as table (output name only in this section, not above)
  
  # use taps_and_seeds_summary df from spaghetti plots
  
  data <- taps_and_seeds_summary
  conds <- unique(data$chain_name)
  
  nrowOutput <- length(conds)
  ncolOutput <- 4
  output <- data.frame(matrix(ncol = ncolOutput, nrow = nrowOutput))
  cnames <- c("chain_name", "slope", "rsq", "y_int")
  colnames(output) <- cnames
  
  #output$chain_name <- conds
  
  n = 1
  
  for (j in 1:length(conds)) {
        
      lm_out <- lm(mean_tempo ~ generation, 
                   data = subset(data,chain_name == conds[j]))
      
      output$chain_name[n] <- as.character(conds[j])
      output$slope[n] <- coefficients(lm_out)[2]
      output$y_int[n] <- coefficients(lm_out)[1]
      output$rsq[n] <- summary(lm_out)$r.squared 
      
      n = n + 1
      
  }
  
  # re-name for later joining and calculate x-intercept
  lm_spag_bychain <- output %>%
    mutate(x_int = -y_int / slope)
  
  # clear holding variables
  rm(data,conds,nrowOutput,ncolOutput,output,cnames,j,n,lm_out)

  # save lm params
  write.csv(lm_spag_bychain, "tables/lm_spag_bychain_usingmeans_2016-10-16.csv")

  # simulation in separate script (2016-10-16_exp1_simulation.R)


# TAP SUMMARY: tables central values and drift (seed, first, and final generation, for reporting) ---------------------
# first by chain
# then by seed (rolling up across chains)
# includes drift calculations, appends lm fits for spaghetti by chain 
  
  # BY CHAIN SUMMARIES:
    # start with taps_and_seeds_summary from spaghetti plots section
    
    # separate dfs for seeds, firsts, and finals
      seedsbychain <- taps_and_seeds_summary %>%
        group_by(mode,type,seed,seed_int,chain_name) %>%
        mutate(max_gen = max(generation)) %>%  
        filter(generation == 0) %>%
        rename(seed_meantempo = mean_tempo,
               seed_sd = sd_taps,
               seed_cov = cov,
               seed_tempo = median_tempo,
               seed_mad = mad_taps,
               seed_comv = comv) %>%
        select(mode,type,seed,seed_int,chain_name,max_gen,
               seed_meantempo,seed_sd,seed_cov,seed_tempo,seed_mad,seed_comv)
      
      firstsbychain <- taps_and_seeds_summary %>%
        group_by(mode,type,seed,seed_int,chain_name) %>%
        mutate(max_gen = max(generation)) %>%
        filter(starter == 1) %>%
        rename(first_meantempo = mean_tempo,
               first_sd = sd_taps,
               first_cov = cov,
               first_tempo = median_tempo,
               first_mad = mad_taps,
               first_comv = comv) %>%
        select(mode,type,seed,seed_int,chain_name,max_gen,
               first_meantempo,first_sd,first_cov,first_tempo,first_mad,first_comv)
    
      finalsbychain <- taps_and_seeds_summary %>%
        group_by(mode,type,seed,seed_int,chain_name) %>%
        mutate(max_gen = max(generation)) %>%
        filter(generation == max_gen) %>%
        rename(final_meantempo = mean_tempo,
               final_sd = sd_taps,
               final_cov = cov,
               final_tempo = median_tempo,
               final_mad = mad_taps,
               final_comv = comv) %>%
        select(mode,type,seed,seed_int,chain_name,max_gen,
               final_meantempo,final_sd,final_cov,final_tempo,final_mad,final_comv)
    
    
    # summarize measures for seed, first, final by chain (expect 13 rows X 24 columns)
    # ALSO COMPUTE DRIFT (final - seed tempo) and VARIABILITY CHANGE (final - first comv)
    tempo_bychain_table <- seedsbychain %>%
      inner_join(firstsbychain, by = c("mode","type","seed","seed_int","chain_name","max_gen")) %>%
      inner_join(finalsbychain, by = c("mode","type","seed","seed_int","chain_name","max_gen")) %>%
      mutate(drift_means = final_meantempo - seed_meantempo,             
             sd_drift = final_sd, # true?
             change_sd = final_sd - first_sd,
             change_cov = final_cov - first_cov,
             drift_meds = final_tempo - seed_tempo,
             change_mad = final_mad - first_mad,
             change_comv = final_comv - first_comv) %>%
      #left_join(lm_spag_bychain, by = "chain_name") # not adding lm fits - potentially misleading to include since can't predict the summarized data from this fit?
      arrange(seed_int)

  # easy to add SE? e.g., mean_se = sd/11 (n = 11 for all generations)
    
    write.csv(tempo_bychain_table, tempo_bychain_output)
    
    
  # BY SEED SUMMARIES (rolling up by-chain summary):
    nchains <- tempo_bychain_table %>%
      group_by(seed_int) %>%
      summarize(nchains = n())
    
    # pooled sd for each seed across chains
    pooled <- tempo_bychain_table %>%
      group_by(seed_int) %>%
      summarize(mean_finaltempo = mean(final_meantempo),
                sum_var = sum((final_sd*final_sd))) %>%
      left_join(nchains, by = "seed_int") %>%
      mutate(n_taps = 11,
             df = n_taps - 1,
             spool = sqrt((df*sum_var)/(df*nchains)))
    
    tempo_byseed_table <- tempo_bychain_table %>%
      group_by(mode,type,seed,seed_int) %>%
      select(-chain_name) %>%
      summarize_each(funs(mean)) %>%
      #left_join(nchains, by = "seed_int") %>%
      left_join(pooled, by = "seed_int")
    
    write.csv(tempo_byseed_table, tempo_byseed_output)
            

tempo_byseed_medians <- tempo_bychain_table %>%
  group_by(mode,type,seed,seed_int) %>%
  summarize(nchains = n(),
            final_avg = mean(final_tempo, na.rm=T),
            final_sd = sd(final_tempo, na.rm=T),
            drift_avg = mean(drift_meds, na.rm=T),
            drift_sd = sd(drift_meds, na.rm=T))


  # some standard error calculations...

  # just a quick overall calc...
  chains_overall <- finalsbychain %>%
    group_by(mode) %>%
    summarize(n = n(),
              min_final = min(final_meantempo),
              max_final = max(final_meantempo),
              mean_mean = mean(final_meantempo),
              sd_mean = sd(final_meantempo),
              se_mean = sd_mean/sqrt(n),
              mean_cov = mean(final_cov),
              sd_cov = sd(final_cov),
              se_cov = sd_cov/sqrt(n))
    
    # by seed -- finals
    error_byseed <- finalsbychain %>%
      group_by(mode, type, seed, seed_int) %>%
      summarize(min_final = min(final_meantempo),
                max_final = max(final_meantempo),
                mean_mean = mean(final_meantempo),
                sd_mean = sd(final_meantempo),
                mean_cov = mean(final_cov),
                sd_cov = sd(final_cov)) %>%
      left_join(nchains, by = "seed_int") %>%
      mutate(se_mean = sd_mean/sqrt(nchains),
             se_cov = sd_cov/sqrt(nchains))
  
  write.csv(error_byseed, "tables/finals_sem_byseed_101716.csv")

    
    # by seed -- firsts
    error_byseed <- firstsbychain %>%
      group_by(mode, type, seed, seed_int) %>%
      summarize(min_first = min(first_meantempo),
                max_first = max(first_meantempo),
                mean_mean = mean(first_meantempo),
                sd_mean = sd(first_meantempo),
                mean_cov = mean(first_cov),
                sd_cov = sd(first_cov)) %>%
      left_join(nchains, by = "seed_int") %>%
      mutate(se_mean = sd_mean/sqrt(nchains),
             se_cov = sd_cov/sqrt(nchains))
    
    write.csv(error_byseed, "tables/firsts_sem_byseed_101716.csv")


    # by chain -- just finals?
    # no sem for cv by chain because it's one individual
    error_bychain <- finalsbychain %>%
      mutate(n = 11,
             se_mean = final_sd/sqrt(n))

  write.csv(error_bychain, "tables/finals_sem_bychain_101716.csv")

# TAPS SUMMARY: drift - plots for central values & variability ------------------------------
# use data frame from table created in section "TAP SUMMARY tables comparing seeds..."
    
  # *2016-11-20 drift by condition
  
  tempo_bychain_table %>%
    ggplot(aes(x=seed_int, y=drift_meds)) +
    geom_point(aes(color=as.factor(seed_int)), size = 3) + 
    #geom_line(aes(group=cohort), alpha = .5, lwd = 1, color = "#999999") +
    geom_smooth(aes(group=mode), method = lm, se = FALSE, lwd=1.5, alpha=.75, color='black') +
    geom_hline(aes(yintercept=0),color="#999999") + # no drift line
    #geom_vline(aes(xintercept=414),linetype=2, alpha=.95) + # smt line
    theme_bw() +
    labs(title = '(B) Chain Drift',
         x = 'Seed Tempo (ms)', 
         y = 'Drift (ms)') +
    theme(panel.grid.minor=element_blank(),
          panel.grid.major=element_blank(),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 11),
          #axis.text.x = element_text(angle = 90),
          #legend.position='right', #c(.9,.9) # 'none'
          legend.title = element_text(size=12),
          legend.text = element_text(size = 11)) +
    scale_color_discrete(name="Seed\nTempo (ms)") +
    scale_x_continuous(breaks=unique(tempo_bychain_table$seed_int)) +  
    scale_y_continuous(limits=c(-1000,500), breaks=c(-1000,-500,0,500)) 
  
  
  # save the plot!
  ggsave("tempo_drift_2016-11-20.png", width=4.25, height=3, dpi=100)



  # *DRIFT PLOT - medians (similar to other Excel plot)
  # with MEAN by chain of MEDIANS by participant
    ggplot(tempo_bychain_table, aes(x = seed_int, y = drift)) +
      geom_point(data=tempo_bychain_table, 
                 aes(x = seed_int, y = drift, color = seed), size = 3, alpha = .75) + 
      geom_smooth(method = lm, se=FALSE, color = '#999999', size = 1) +  
      theme_bw() +  
      labs(x = 'seed tempo (ms)', 
           y = 'drift = final - seed (ms)') + 
      theme(panel.grid.minor=element_blank(),
            panel.grid.major=element_blank(),
            axis.title = element_text(size = 26),
            axis.text = element_text(size = 22),
            legend.position='right', #c(.9,.9)
            legend.title = element_text(size=22),
            legend.text = element_text(size = 16)) +
      scale_x_continuous(breaks=c(300,600,900,1200)) +
      scale_color_discrete(name="seed tempo (ms)",
                           breaks=c('0','1','2','3'),
                           labels=c("300", "600", "900", "1200"))              
  
  # DRIFT 2 PLOT - means (similar to other Excel plot)
  # with MEAN by chain of MEANS by participant
    ggplot(tempo_bychain_table, aes(x = seed_int, y = drift_means)) +
      geom_point(data=tempo_bychain_table, 
                 aes(x = seed_int, y = drift_means, color = seed), size = 3, alpha = .75) + 
      geom_smooth(method = lm, se=FALSE, color = '#999999', size = 1) +  
      theme_bw() +  
      labs(x = 'seed tempo (ms)', 
           y = 'drift = final - seed (ms)') + 
      theme(panel.grid.minor=element_blank(),
            panel.grid.major=element_blank(),
            axis.title = element_text(size = 26),
            axis.text = element_text(size = 22),
            legend.position='right', #c(.9,.9)
            legend.title = element_text(size=22),
            legend.text = element_text(size = 16)) +
      scale_x_continuous(breaks=c(300,600,900,1200)) +
      scale_color_discrete(name="seed tempo (ms)",
                           breaks=c('0','1','2','3'),
                           labels=c("300", "600", "900", "1200"))              
  
  # VARIABILITY DRIFT PLOT - coefficient of MAD (median abs dev)
  # with mean by chain of COMV by participant
    ggplot(tempo_bychain_table, aes(x = seed_int, y = change_comv)) +
      geom_point(data=tempo_bychain_table, 
                 aes(x = seed_int, y = change_comv, color = seed), size = 3, alpha = .75) + 
      geom_smooth(method = lm, se=FALSE, color = '#999999', size = 1) +  
      theme_bw() +  
      labs(x = 'seed tempo (ms)', 
           y = 'change comv = final - first') + 
      theme(panel.grid.minor=element_blank(),
            panel.grid.major=element_blank(),
            axis.title = element_text(size = 26),
            axis.text = element_text(size = 22),
            legend.position='right', #c(.9,.9)
            legend.title = element_text(size=22),
            legend.text = element_text(size = 16)) +
      scale_x_continuous(breaks=c(300,600,900,1200)) +
      scale_color_discrete(name="seed tempo (ms)",
                           breaks=c('0','1','2','3'),
                           labels=c("300", "600", "900", "1200"))   
  
  # VARIABILITY DRIFT PLOT 2 - coefficient of variation (cov)
  # with mean by chain of cov by participant
    ggplot(tempo_bychain_table, aes(x = seed_int, y = change_cov)) +
      geom_point(data=tempo_bychain_table, 
                 aes(x = seed_int, y = change_cov, color = seed), size = 3, alpha = .75) + 
      geom_smooth(method = lm, se=FALSE, color = '#999999', size = 1) +  
      theme_bw() +  
      labs(x = 'seed tempo (ms)', 
           y = 'CV-drift = final - first') + 
      theme(panel.grid.minor=element_blank(),
            panel.grid.major=element_blank(),
            axis.title = element_text(size = 26),
            axis.text = element_text(size = 22),
            legend.position='right', #c(.9,.9)
            legend.title = element_text(size=22),
            legend.text = element_text(size = 16)) +
      scale_x_continuous(breaks=c(300,600,900,1200)) +
      scale_color_discrete(name="seed tempo (ms)",
                           breaks=c('0','1','2','3'),
                           labels=c("300", "600", "900", "1200")) 

        
# TAP SUMMARY: drift - linear fit------------------------------
# started with IT02 script
# for data, use tempo_bychain_table from earlier section

  # prep data frame for holding fits
  data <- tempo_bychain_table
  conds <- unique(data$mode)
  
  nrowOutput <- length(conds)
  ncolOutput <- 11
  output <- data.frame(matrix(ncol = ncolOutput, nrow = nrowOutput))
  cnames <- c("mode", "slope", "slope_p", "y_int", "y_int_p",
              "F_val", "F_numdf", "F_dendf", "F_sig", "rsq", "x_int")
  
  colnames(output) <- cnames
    
  
  n = 1
  
  for (j in 1:length(conds)) {
    
    lm_out <- lm(drift_meds ~ seed_int, 
                 data = subset(data,mode == conds[j]))
    
    output$mode[n] <- as.character(conds[j])
    output$slope[n] <- coefficients(lm_out)[2]
    output$slope_p[n] <- summary(lm_out)$coefficients[2,4]
    output$y_int[n] <- coefficients(lm_out)[1]
    output$y_int_p[n] <- summary(lm_out)$coefficients[1,4]
    output$F_val[n] <- summary(lm_out)$fstatistic[1]
    output$F_numdf[n] <- summary(lm_out)$fstatistic[2]
    output$F_dendf[n] <- summary(lm_out)$fstatistic[3]
    output$F_sig[n] <- pf(summary(lm_out)$fstatistic[1], summary(lm_out)$fstatistic[2], summary(lm_out)$fstatistic[3], lower.tail = FALSE)
    output$rsq[n] <- summary(lm_out)$r.squared 
    output$x_int[n] <- -1*coefficients(lm_out)[1]/coefficients(lm_out)[2]
    
    n = n + 1
    
  }
  
  
  # save lm params
  write.csv(output, "tables/lm_drift_medians_overall_2016-11-20.csv")

  
  # clear holding variables
  rm(data,conds,nrowOutput,ncolOutput,output,cnames,lm_out)

         
# TAPS & TAP SUMMARY phase return plot & deltas ------------------------
# what's the difference between AR(1) and phase return??
# how to interpret return plots?

  # TAPS: individual intervals (preserves serial order across generations - more important for meter than tempo)
  ggplot(taps_at, aes(x = source_tap_int, y = tap_int, group = chain_name, color = seed)) +
    geom_point(size = 2) +
    #geom_line() +
    geom_smooth(data = taps_at, aes(x = source_tap_int, y = tap_int, group = seed), 
                alpha = .2, size = 2) +
    labs(title = 'return plot - tap intervals', 
         x = 'interval i-1 (ms)', y = 'interval i (ms)') + 
    theme(plot.title = element_text(lineheight=.8, face="bold"),
          panel.grid.minor=element_blank(),
          panel.grid.major=element_blank(),
          axis.title.x = element_text(size=20),
          axis.text.x = element_text(size = 16),
          axis.title.y  = element_text(size =20),
          axis.text.y = element_text(size = 16)) +
    coord_cartesian(xlim = c(0,2500), ylim=c(0, 2500))   

  
  # TAP SUMMARY: return plot for tempo - data preparation 
  
  # df with source columns + change from generation n to n+1
  # to fix if time: one data point gets removed for NA by this method (source_gen != 0 though chain starter)
  tempo_return <- taps_summary %>%
    group_by(chain_name) %>%
    arrange(generation) %>%
    mutate(source_gen = generation - 1,
           source_mean_tempo = ifelse(source_gen == 0, seed_int, lag(mean_tempo)),
           source_sd_taps = ifelse(source_gen == 0, 0, lag(sd_taps)),
           source_cov = ifelse(source_gen == 0, 0, lag(cov)),
           source_median_tempo = ifelse(source_gen == 0, seed_int, lag(median_tempo)),
           source_mad_taps = ifelse(source_gen == 0, 0, lag(mad_taps)),
           source_comv = ifelse(source_gen == 0, 0, lag(comv)),
           delta_mean = mean_tempo - source_mean_tempo,
           delta_sd = sd_taps - source_sd_taps,
           delta_cov = cov - source_cov,        
           delta_median = median_tempo - source_median_tempo,
           delta_mad = mad_taps - source_mad_taps,
           delta_comv = comv - source_comv)
  
  # write tempo_return df to table
  write.csv(tempo_return, "tables/tempo_return.csv")

  # alternative method:
    # df of source tempi
      taps_summary_source <- taps_summary %>%
        select(id,
               mean_tempo,sd_taps,cov,median_tempo,mad_taps,comv) %>%
        rename(sourceId = id,
               source_mean_tempo = mean_tempo,
               source_sd_taps = sd_taps,
               source_cov = cov,
               source_median_tempo = median_tempo,
               source_mad_taps = mad_taps,
               source_comv = comv) %>%
        mutate(sourceId = as.factor(sourceId)) 
    
    # join to original df  
      taps_summary_return <- taps_summary %>%
        left_join(taps_summary_source, by = "sourceId") # %>%
        # add mean & median tempo info for starters
        # not sure why this isn't working...to be continued...
        #mutate(source_mean_tempo = ifelse(starter == 1, seed_int, source_mean_tempo)),
        #       source_median_tempo = ifelse(starter == 1, seed_int, source_median_tempo)) %>%
        # add 'discrete differential' 
        # also not working WHY???!??!?
        #mutate(tempo_diff = source_median_tempo - median_tempo) %>% 
        #mutate(abs_tempo_diff = abs(source_median_tempo - median_tempo))
  
    
    # return plot for median tempo
      ggplot(tempo_return, aes(x = source_median_tempo, y = median_tempo, group = chain_name, color = seed)) +
        geom_point(size = 2) +
        geom_smooth(aes(group = seed), method = lm, se = FALSE) +
        #geom_smooth(data = taps_summary_return, aes(x = source_median_tempo, y = median_tempo, group = seed), 
         #           alpha = .2, size = 2) +
        theme_bw() +
        labs(title = 'return plot - SR median tempo', 
             x = 'tempo n-1 (ms)', y = 'tempo n (ms)') + 
        theme(plot.title = element_text(lineheight=.8, face="bold"),
              panel.grid.minor=element_blank(),
              panel.grid.major=element_blank(),
              axis.title.x = element_text(size=20),
              axis.text.x = element_text(size = 16),
              axis.title.y  = element_text(size =20),
              axis.text.y = element_text(size = 16)) +
        coord_cartesian(xlim = c(0,1200), ylim=c(0, 1200)) 

# return plot for mean tempo
ggplot(tempo_return, aes(x = source_mean_tempo, y = mean_tempo, group = chain_name, color = seed)) +
  geom_point(size = 3, alpha = .5) +
  geom_smooth(aes(group = seed), method = lm, se = FALSE, size = 1.5, alpha = .5) +
  theme_bw() +
  labs(title = 'return plot - SR mean tempo', 
       x = 'tempo n-1 (ms)', y = 'tempo n (ms)') + 
  theme(plot.title = element_text(lineheight=.8, face="bold"),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        axis.title.x = element_text(size=20),
        axis.text.x = element_text(size = 16),
        axis.title.y  = element_text(size =20),
        axis.text.y = element_text(size = 16)) +
  coord_cartesian(xlim = c(0,1200), ylim=c(0, 1200)) 


  # 'bias' (x&g 2010) -- [tap_n - tap_n-1] X tap_n-1
  ggplot(tempo_return, aes(x = source_median_tempo, y = delta_median, group = chain_name, color = seed)) +
    geom_point(size = 2) +
    geom_smooth(aes(group = seed), method = lm, se = FALSE) +
    theme_bw() +
    labs(title = 'bias (x&g 2010) plot - SR - delta tempo X median tempo', 
         x = 'tempo n-1 (ms)', y = 'tempo n - tempo n-1 (ms)') + 
    theme(plot.title = element_text(lineheight=.8, face="bold"),
          panel.grid.minor=element_blank(),
          panel.grid.major=element_blank(),
          axis.title.x = element_text(size=20),
          axis.text.x = element_text(size = 16),
          axis.title.y  = element_text(size =20),
          axis.text.y = element_text(size = 16)) +
    coord_cartesian(xlim = c(0,1200), ylim=c(0, 1200)) 
      
    # return plot for cov for tempo
      ggplot(tempo_return, aes(x = source_cov, y = cov, group = chain_name, color = seed)) +
        geom_point(size = 2) +
        #geom_line() +
        geom_smooth(aes(group = seed), method = lm, se = FALSE) +
        #geom_smooth(data = tempo_return, aes(x = source_cov, y = cov, group = seed), 
         #           alpha = .2, size = 2) +
        labs(title = 'return plot - coefficient of variation', 
             x = 'cov n-1', y = 'cov n') + 
        theme_bw() +
        theme(plot.title = element_text(lineheight=.8, face="bold"),
              panel.grid.minor=element_blank(),
              panel.grid.major=element_blank(),
              axis.title.x = element_text(size=20),
              axis.text.x = element_text(size = 16),
              axis.title.y  = element_text(size =20),
              axis.text.y = element_text(size = 16)) #+
        #coord_cartesian(xlim = c(0,400), ylim=c(0,400)) 
  
  # 'bias' (x&g 2010) FOR COV -- [tap_n - tap_n-1] X tap_n-1
  tempo_return %>%
    #filter(generation != 1) %>%
    ggplot(aes(x = source_cov, y = delta_cov, group = chain_name, color = seed)) +
    geom_point(size = 2) +
    geom_smooth(aes(group = seed), method = lm, se = FALSE) +
    theme_bw()


  # other plot for convergence? (when change from gen to gen is zero as function of generation)
    # delta = tempo(i+1) - tempo(i), plotted as function of generation
    ggplot(tempo_return, aes(x = generation, y = delta_mean, group = chain_name, color = seed)) +
      geom_point(size = 2) +
      geom_line() +
      geom_smooth(aes(x = generation, y = delta_mean, group = seed),
                  alpha = .2, size = 2)
    
    # maybe convergence point = when change from gen to gen is zero as function of source tempo?
    ggplot(tempo_return, aes(x = source_median_tempo, y = delta_median, group = chain_name, color = seed)) +
      geom_point(size = 2) +
      geom_smooth(aes(x = source_median_tempo, y = delta_median, group = seed),
                  method = lm, se=FALSE, size = 1)

#  WORKING: AR(1) process ------------------
# linear fits to AR(1) and bias (following Xu & Griffiths, 2010)
# alternative estimate of zero-bias tempo
# start with tempo_return data frame from return plots section
 
  # lm_ar1. use tempo_return df
  data <- tempo_return
  conds <- unique(data$seed_int)
  
  nrowOutput <- length(conds)
  ncolOutput <- 4
  output <- data.frame(matrix(ncol = ncolOutput, nrow = nrowOutput))
  cnames <- c("seed_int", "slope", "rsq", "y_int")
  colnames(output) <- cnames
  
  #output$chain_name <- conds
  
  n = 1
  
  for (j in 1:length(conds)) {
    
    lm_out <- lm(mean_tempo ~ source_mean_tempo, 
                 data = subset(data,seed_int == conds[j]))
    
    output$seed_int[n] <- conds[j]
    output$slope[n] <- coefficients(lm_out)[2]
    output$y_int[n] <- coefficients(lm_out)[1]
    output$rsq[n] <- summary(lm_out)$r.squared 
    
    n = n + 1
    
  }
  
  # re-name for later joining and calculate x-intercept
  lm_ar1 <- output %>%
    mutate(x_int = -y_int / slope)
  
  # clear holding variables
  rm(data,conds,nrowOutput,ncolOutput,output,cnames,j,n,lm_out)

  # save table for ar1
  write.csv(lm_ar1,"tables/lm_ar1.csv")
    
    # ALT: lm_ar1_overall. use tempo_return df
    data <- tempo_return
    conds <- unique(data$mode)
    
    nrowOutput <- length(conds)
    ncolOutput <- 4
    output <- data.frame(matrix(ncol = ncolOutput, nrow = nrowOutput))
    cnames <- c("mode", "slope", "rsq", "y_int")
    colnames(output) <- cnames
    
    #output$chain_name <- conds
    
    n = 1
    
    for (j in 1:length(conds)) {
      
      lm_out <- lm(mean_tempo ~ source_mean_tempo, 
                   data = subset(data,mode == conds[j]))
      
      output$mode[n] <- as.character(conds[j])
      output$slope[n] <- coefficients(lm_out)[2]
      output$y_int[n] <- coefficients(lm_out)[1]
      output$rsq[n] <- summary(lm_out)$r.squared 
      
      n = n + 1
      
    }
    
    # re-name for later joining and calculate x-intercept
    lm_ar1_overall <- output %>%
      mutate(x_int = -y_int / slope)
    
    # clear holding variables
    rm(data,conds,nrowOutput,ncolOutput,output,cnames,j,n,lm_out)
    
    # save table for ar1
    write.csv(lm_ar1_overall,"tables/lm_ar1_overall.csv")

  # AR(1) bias: [(N+1) - N] against N
  
  # lm_ar1_bias. use tempo_return df
  data <- tempo_return
  conds <- unique(data$seed_int)
  
  nrowOutput <- length(conds)
  ncolOutput <- 4
  output <- data.frame(matrix(ncol = ncolOutput, nrow = nrowOutput))
  cnames <- c("seed_int", "slope", "rsq", "y_int")
  colnames(output) <- cnames
  
  #output$chain_name <- conds
  
  n = 1
  
  for (j in 1:length(conds)) {
    
    lm_out <- lm(delta_mean ~ source_mean_tempo, 
                 data = subset(data,seed_int == conds[j]))
    
    output$seed_int[n] <- conds[j]
    output$slope[n] <- coefficients(lm_out)[2]
    output$y_int[n] <- coefficients(lm_out)[1]
    output$rsq[n] <- summary(lm_out)$r.squared 
    
    n = n + 1
    
  }
  
  # re-name for later joining and calculate x-intercept
  lm_ar1_bias <- output %>%
    mutate(x_int = -y_int / slope)
  
  # summarize lm fit
  summary(lm_out) # only gives the last fit
  qqnorm(lm_out$residuals)
  
  # clear holding variables
  rm(data,conds,nrowOutput,ncolOutput,output,cnames,j,n,lm_out)
  
  # save table for ar1 -- bias
  write.csv(lm_ar1_bias,"tables/lm_ar1_bias.csv")
  
  
    # ALT: lm_ar1_bias_overall. use tempo_return df
    data <- tempo_return
    conds <- unique(data$mode)
    
    nrowOutput <- length(conds)
    ncolOutput <- 4
    output <- data.frame(matrix(ncol = ncolOutput, nrow = nrowOutput))
    cnames <- c("mode", "slope", "rsq", "y_int")
    colnames(output) <- cnames
    
    #output$chain_name <- conds
    
    n = 1
    
    for (j in 1:length(conds)) {
      
      lm_out <- lm(delta_mean ~ source_mean_tempo, 
                   data = subset(data,mode == conds[j]))
      
      output$mode[n] <- as.character(conds[j])
      output$slope[n] <- coefficients(lm_out)[2]
      output$y_int[n] <- coefficients(lm_out)[1]
      output$rsq[n] <- summary(lm_out)$r.squared 
      
      n = n + 1
      
    }
    
    # re-name for later joining and calculate x-intercept
    lm_ar1_bias_overall <- output %>%
      mutate(x_int = -y_int / slope)
    
    # clear holding variables
    rm(data,conds,nrowOutput,ncolOutput,output,cnames,j,n,lm_out)
    
    # save table for ar1
    write.csv(lm_ar1_bias_overall,"tables/lm_ar1_bias_overall.csv")


# smt: plots ------------------------

      # median smt by generation (with linear)
      ggplot(taps_smt_summary, aes(x = generation, y = median_tempo, group = chain_name, color = seed)) +
        geom_point(size = 2) +
        geom_line() +
        geom_smooth(data = taps_smt_summary, aes(x = generation, y = median_tempo, group = seed), 
                    method = lm, se = FALSE, alpha = .5, size = 2) +
        labs(title = 'median SMT by generation', 
             x = 'generation', y = 'median tempo (ms)') + 
        theme_bw() +
        labs(x = 'generation', 
             y = 'median SMT (ms)') + 
        theme(panel.grid.minor=element_blank(),
              panel.grid.major=element_blank(),
              axis.title = element_text(size = 26),
              axis.text = element_text(size = 22),
              legend.position='right', #c(.9,.9)
              legend.title = element_text(size=22),
              legend.text = element_text(size = 16)) +
        scale_x_continuous(breaks=seq(-1,18,1)) +
        coord_cartesian(ylim=c(0, 1200)) +
        scale_color_discrete(name="seed tempo (ms)",
                             breaks=c('0','1','2','3'),
                             labels=c("300", "600", "900", "1200"))
      
      
      # CV for smt by generation 
      # update y in parent plot for different measure
      ggplot(taps_smt_summary, aes(x = generation, y = cov, group = chain_name, color = seed)) +
        geom_point(size = 2) +
        geom_line() + 
        geom_smooth(aes(group = seed), method = lm, se = FALSE, alpha = .2, size = 2) +
        labs(title = 'SMT coefficient of deviation (CV) by generation', 
             x = 'generation', y = 'CV') + 
        theme_bw() +
        theme(panel.grid.minor=element_blank(),
              panel.grid.major=element_blank(),
              axis.title = element_text(size = 26),
              axis.text = element_text(size = 22),
              legend.position='right', #c(.9,.9)
              legend.title = element_text(size=22),
              legend.text = element_text(size = 16)) +
        scale_x_continuous(breaks=seq(-1,18,1)) +
        #coord_cartesian(ylim=c(0, 50)) +
        scale_color_discrete(name="seed tempo (ms)",
                             breaks=c('0','1','2','3'),
                             labels=c("300", "600", "900", "1200"))      

# smt: tables -------------------------------
# * see smt_overall for smt info to report

  # summarize excluded data
  smt_excluded_bychain <- taps_smt %>%
    group_by(mode,type,seed,seed_int,chain_name) %>%
    summarize(n_total = n(),
              n_included = sum(include),
              n_excluded = n_total - n_included)
  
  write.csv(smt_excluded_bychain,"tables/smt_excluded_bychain.csv")
  
  smt_excluded_byseed <- smt_excluded_bychain %>%
    group_by(mode,type,seed,seed_int) %>%
    summarize(n_total = sum(n_total),
              n_included = sum(n_included),
              n_excluded = n_total - n_included) %>%
    mutate(prop_excl = )
  
  smt_excluded_all <- smt_excluded_byseed %>%
    group_by(mode,type) %>%
    summarize(n_total = sum(n_total),
              n_included = sum(n_included),
              n_excluded = n_total - n_included) %>%
    mutate(prop_excl = n_excluded/n_total)
            

    
      smt_bychain_table <- taps_smt_summary %>%
        group_by(mode,type,seed,seed_int,chain_name) %>%
        summarize(mean_median = mean(median_tempo),
                  mean_mean = mean(mean_tempo),
                  mean_sd = mean(sd_taps),
                  mean_mad = mean(mad_taps),
                  mean_cov = mean(cov),
                  mean_comv = mean(comv),
                  weight = 1/mean_sd^2)
      write.csv(smt_bychain_table, smt_bychain_output)


    # add standard error
    smt_bychain_table_2 <- taps_smt_summary %>%
      #mutate(max_gen = max(generation)) %>%
      #filter(generation == max_gen) %>% # wait, why am i including this? 
      group_by(mode,type,seed,seed_int,chain_name) %>%
      summarize(mean_median = mean(median_tempo),
                sd_median = sd(median_tempo),
                mean_cov = mean(cov),
                sd_cov = sd(cov)) %>%
      left_join(nchains, by = "seed_int") %>%
      mutate(se_median = sd_median/sqrt(nchains),
             se_cov = sd_cov/sqrt(nchains))
    
    write.csv(smt_bychain_table_2, "tables/smt_bychain_table_with_error_2016-10-18.csv")            

      smt_byseed_table <- smt_bychain_table %>%
        group_by(mode,type,seed,seed_int) %>%
        summarize(mean_median = mean(mean_median),
                  mean_mean = mean(mean_mean),
                  mean_sd = mean(mean_sd),
                  mean_mad = mean(mean_mad),
                  mean_cov = mean(mean_cov),
                  mean_comv = mean(mean_comv))
      write.csv(smt_byseed_table, smt_byseed_output)


  smt_overall <- taps_smt_summary %>%
    group_by(mode) %>%
    summarize(n = n(),
              min_smt = min(median_tempo),
              max_smt = max(median_tempo),
              mean_med_smt = mean(median_tempo),
              sd_med_smt = sd(median_tempo),
              se_med_smt = sd_med_smt/n,
              mean_cov_smt = mean(cov),
              sd_cov_smt = sd(cov),
              se_cov_smt = sd_cov_smt/n)


    smt_byseed <- taps_smt_summary %>%
      group_by(mode,type,seed) %>%
      summarize(n = n(),
                min_smt = min(median_tempo),
                max_smt = max(median_tempo),
                mean_med_smt = mean(median_tempo),
                sd_med_smt = sd(median_tempo),
                se_med_smt = sd_med_smt/n,
                mean_cov_smt = mean(cov),
                sd_cov_smt = sd(cov),
                se_cov_smt = sd_cov_smt/n)

    write.csv(smt_byseed, "tables/smt_summary_byseed_2016-11-20.csv")

# smt taps: return -----------------------------------
# issue: why do some filenames have more than 11 taps?
  
  smt_taps_return <- taps_smt %>%
    filter(rejects == "n", include == 1) %>%
    group_by(mode,type,seed,chain_name,filename) %>%
    arrange(tap_number) %>%
    mutate(previous = tap_number - 1,
           prev_tap = lag(tap_int),
           delta = tap_int - prev_tap)
  
  
  ggplot(smt_taps_return, aes(x = prev_tap, y = tap_int)) +
    geom_point(aes(color = tap_number)) #+
    #geom_line() # to do: try adding line ending = arrow (check proper call)
    # also somehow redo color coding so end-point of iteration is more obvious?
  
  ggplot(smt_taps_return, aes(x = tap_number, y = delta)) +
    geom_point() 
  
  ggplot(smt_taps_return, aes(x = prev_tap, y = delta)) +
    geom_point()
  
# smt summary: return -----------------------------------
  
  smt_return <- taps_smt_summary %>%
    group_by(chain_name) %>%
    arrange(generation) %>%
    mutate(source_gen = generation - 1,
           source_mean_tempo = ifelse(source_gen == 0, seed_int, lag(mean_tempo)),
           source_sd_taps = ifelse(source_gen == 0, 0, lag(sd_taps)),
           source_cov = ifelse(source_gen == 0, 0, lag(cov)),
           source_median_tempo = ifelse(source_gen == 0, seed_int, lag(median_tempo)),
           source_mad_taps = ifelse(source_gen == 0, 0, lag(mad_taps)),
           source_comv = ifelse(source_gen == 0, 0, lag(comv)),
           delta_mean = mean_tempo - source_mean_tempo,
           delta_sd = sd_taps - source_sd_taps,
           delta_cov = cov - source_cov,        
           delta_median = median_tempo - source_median_tempo,
           delta_mad = mad_taps - source_mad_taps,
           delta_comv = comv - source_comv)
  
  # return plot for smt (median)
  ggplot(smt_return, aes(x = source_median_tempo, y = median_tempo, group = chain_name, color = seed)) +
    geom_point(size = 2) +
    #geom_line() +
    geom_smooth(aes(group = seed), method = lm, se = FALSE) +
    #geom_smooth(data = tempo_return, aes(x = source_cov, y = cov, group = seed), 
    #           alpha = .2, size = 2) +
    labs(title = 'return plot - smt median tempo', 
         x = 'tempo n-1', y = 'tempo n') + 
    theme_bw() +
    theme(plot.title = element_text(lineheight=.8, face="bold"),
          panel.grid.minor=element_blank(),
          panel.grid.major=element_blank(),
          axis.title.x = element_text(size=20),
          axis.text.x = element_text(size = 16),
          axis.title.y  = element_text(size =20),
          axis.text.y = element_text(size = 16)) #+
  #coord_cartesian(xlim = c(0,400), ylim=c(0,400)) 
  
  
  # return plot for smt variability
  ggplot(smt_return, aes(x = source_cov, y = cov, group = chain_name, color = seed)) +
    geom_point(size = 2) +
    #geom_line() +
    geom_smooth(aes(group = seed), method = lm, se = FALSE) +
    #geom_smooth(data = tempo_return, aes(x = source_cov, y = cov, group = seed), 
    #           alpha = .2, size = 2) +
    labs(title = 'return plot - smt coefficient of variation', 
         x = 'cov n-1', y = 'cov n') + 
    theme_bw() +
    theme(plot.title = element_text(lineheight=.8, face="bold"),
          panel.grid.minor=element_blank(),
          panel.grid.major=element_blank(),
          axis.title.x = element_text(size=20),
          axis.text.x = element_text(size = 16),
          axis.title.y  = element_text(size =20),
          axis.text.y = element_text(size = 16)) #+
  #coord_cartesian(xlim = c(0,400), ylim=c(0,400)) 
  

# smt: spaghetti - linear fits------------------------------
# linear coefficients for spaghetti (tempo by generation) by seed

data <- taps_smt_summary
conds <- unique(data$seed)

nrowOutput <- length(conds)
ncolOutput <- 11
output <- data.frame(matrix(ncol = ncolOutput, nrow = nrowOutput))
cnames <- c("seed", "slope", "slope_p", "y_int", "y_int_p",
            "F_val", "F_numdf", "F_dendf", "F_sig", "rsq", "x_int")

colnames(output) <- cnames

#output$chain_name <- conds

n = 1

for (j in 1:length(conds)) {
  
  lm_out <- lm(median_tempo ~ generation, 
               data = subset(data,seed == conds[j]))
  
  output$seed[n] <- as.character(conds[j])
  output$slope[n] <- coefficients(lm_out)[2]
  output$slope_p[n] <- summary(lm_out)$coefficients[2,4]
  output$y_int[n] <- coefficients(lm_out)[1]
  output$y_int_p[n] <- summary(lm_out)$coefficients[1,4]
  output$F_val[n] <- summary(lm_out)$fstatistic[1]
  output$F_numdf[n] <- summary(lm_out)$fstatistic[2]
  output$F_dendf[n] <- summary(lm_out)$fstatistic[3]
  output$F_sig[n] <- pf(summary(lm_out)$fstatistic[1], summary(lm_out)$fstatistic[2], summary(lm_out)$fstatistic[3], lower.tail = FALSE)
  output$rsq[n] <- summary(lm_out)$r.squared 
  output$x_int[n] <- -1*coefficients(lm_out)[1]/coefficients(lm_out)[2]
  
  n = n + 1
  
}


# save lm params
write.csv(output, "tables/lm_smt_byseed_2016-11-20.csv")

# clear holding variables
rm(data,conds,nrowOutput,ncolOutput,output,cnames,j,n,lm_out)

# smt: spaghetti - linear fits - OVERALL ------------------------------
# linear coefficients for spaghetti (tempo by generation) overall experiment

data <- taps_smt_summary
conds <- unique(data$mode)

nrowOutput <- length(conds)
ncolOutput <- 11
output <- data.frame(matrix(ncol = ncolOutput, nrow = nrowOutput))
cnames <- c("mode", "slope", "slope_p", "y_int", "y_int_p",
            "F_val", "F_numdf", "F_dendf", "F_sig", "rsq", "x_int")

colnames(output) <- cnames

#output$chain_name <- conds

n = 1

for (j in 1:length(conds)) {
  
  lm_out <- lm(median_tempo ~ generation, 
               data = subset(data,mode == conds[j]))
  
  output$mode[n] <- as.character(conds[j])
  output$slope[n] <- coefficients(lm_out)[2]
  output$slope_p[n] <- summary(lm_out)$coefficients[2,4]
  output$y_int[n] <- coefficients(lm_out)[1]
  output$y_int_p[n] <- summary(lm_out)$coefficients[1,4]
  output$F_val[n] <- summary(lm_out)$fstatistic[1]
  output$F_numdf[n] <- summary(lm_out)$fstatistic[2]
  output$F_dendf[n] <- summary(lm_out)$fstatistic[3]
  output$F_sig[n] <- pf(summary(lm_out)$fstatistic[1], summary(lm_out)$fstatistic[2], summary(lm_out)$fstatistic[3], lower.tail = FALSE)
  output$rsq[n] <- summary(lm_out)$r.squared 
  output$x_int[n] <- -1*coefficients(lm_out)[1]/coefficients(lm_out)[2]
  
  n = n + 1
  
}


# save lm params
write.csv(output, "tables/lm_smt_overall_2016-11-20.csv")

# clear holding variables
rm(data,conds,nrowOutput,ncolOutput,output,cnames,j,n,lm_out)



# smt and sr final relationship: plots ------------------------
# look at smt vs final and smt vs drift
# start with bychain tables

smt_sr <- smt_bychain_table %>%
  rename(smt_mean_tempo = mean_mean,
         smt_sd = mean_sd,
         smt_cov = mean_cov,
         smt_tempo = mean_median,
         smt_mad = mean_mad,
         smt_comv = mean_comv) %>%
  left_join(by = c("mode","type","seed","seed_int","chain_name"),
            tempo_bychain_table)

      # ? to do - compare gen by gen
      smt_sr <- taps_smt_summary %>%
        mutate(smt_mean = mean_tempo,
               smt_sd = sd_taps,
               smt_cv = cov,
               smt_median = median_tempo,
               smt_mad = mad_taps,
               smt_comv = comv) %>%
        left_join(by = 1:) # etc - in progress
        left_join(by = c("mode","type","seed","seed_int","chain_name","filename"))
          
    
  # smt vs. sr final tempo
    ggplot(smt_sr, aes(x = smt_tempo, y = final_meantempo, group = chain_name, color = seed)) +
      geom_point(size = 2) +
      labs(x = 'SMT median tempo (ms)', y = 'SR mean tempo (ms)') + 
      theme_bw() +
      theme(panel.grid.minor=element_blank(),
            panel.grid.major=element_blank(),
            axis.title = element_text(size = 26),
            axis.text = element_text(size = 22),
            legend.position='right', #c(.9,.9)
            legend.title = element_text(size=22),
            legend.text = element_text(size = 16)) +
      #scale_x_continuous(breaks=seq(-1,18,1)) +
      coord_cartesian(xlim = c(0,1000), ylim=c(0, 1000)) +
      scale_color_discrete(name="seed tempo (ms)",
                           breaks=c('0','1','2','3'),
                           labels=c("300", "600", "900", "1200"))
    
  # smt vs. drift
    ggplot(smt_sr, aes(x = smt_tempo, y = drift_means, group = chain_name, color = seed)) +
      geom_point(size = 4) +
      labs(x = 'SMT median tempo (ms)', y = 'SR drift (ms)') + 
      theme_bw() +
      theme(panel.grid.minor=element_blank(),
            panel.grid.major=element_blank(),
            axis.title = element_text(size = 26),
            axis.text = element_text(size = 22),
            legend.position='right', #c(.9,.9)
            legend.title = element_text(size=22),
            legend.text = element_text(size = 16)) +
      #scale_x_continuous(breaks=seq(-1,18,1)) +
      #coord_cartesian(xlim = c(0,1000), ylim=c(0, 1000)) +
      scale_color_discrete(name="seed tempo (ms)",
                           breaks=c('0','1','2','3'),
                           labels=c("300", "600", "900", "1200"))
    

# WORKING: demographics ------------------------
# read in demo data for auditory tempo condition stored in compiled data
# get counts for tables (bychain and byseed) & save
# plotting scripts if needed

  # demo data categories:
    # age groups 
    # musical ability 
    # gender 
    # location 

  ## read data & clean/sort
  demo_raw <- read.csv(demo_input)
  demo <- demo_raw %>%
    mutate(seed = as.factor(seed)) %>%
    mutate(seed_int = ifelse(seed == 0, '300', 
                             ifelse(seed == 1, '600', 
                                    ifelse(seed == 2, '900', 
                                           ifelse(seed == 3, '1200', NA))))) %>%
    select(mode,type,seed,seed_int,chain_name,filename,
           starter,id,sourceId,generation = chain,
           age,gender,zip,music,city,state,latitude,longitude)

    
  ## summaries by chain
    
    # counts by chain by category, to be combined to one table for saving
    
    agebychain <- demo %>%
      select(chain_name, age) %>%
      group_by(chain_name, age) %>%
      summarize(n = n()) %>%
      spread(age, n)
      
    musicbychain <- demo %>%
      select(chain_name, music) %>%
      group_by(chain_name, music) %>%
      summarize(n = n()) %>%
      spread(music, n) %>%
      rename(no_music = NR)
    
    genderbychain <- demo %>%
      select(chain_name, gender) %>%
      group_by(chain_name, gender) %>%
      summarize(n = n()) %>%
      spread(gender, n) %>%
      rename(no_gender = NR)
    
    bychain_counts <- demo %>%
      group_by(mode,type,seed,seed_int,chain_name) %>%
      summarize(n = n()) %>%
      left_join(genderbychain, by = "chain_name") %>%
      left_join(agebychain, by = "chain_name") %>%
      left_join(musicbychain, by = "chain_name")
    
    write.csv(bychain_counts, demo_bychain_output)


  ## summaries by seed
    
    # counts by seed by category; joined at end for saving

    chainsbyseed <- bychain_counts %>%
      group_by(seed_int) %>%
      summarize(n_chains = n())
    
    agebyseed <- demo %>%
      select(seed_int, age) %>%
      group_by(seed_int, age) %>%
      summarize(n = n()) %>%
      spread(age, n)
    
    musicbyseed <- demo %>%
      select(seed_int, music) %>%
      group_by(seed_int, music) %>%
      summarize(n = n()) %>%
      spread(music, n) %>%
      rename(no_music = NR)
    
    genderbyseed <- demo %>%
      select(seed_int, gender) %>%
      group_by(seed_int, gender) %>%
      summarize(n = n()) %>%
      spread(gender, n) %>%
      rename(no_gender = NR)
    
    byseed_counts <- demo %>%
      group_by(mode,type,seed,seed_int) %>%
      summarize(n = n()) %>%
      left_join(chainsbyseed, by = "seed_int") %>%
      left_join(genderbyseed, by = "seed_int") %>%
      left_join(agebyseed, by = "seed_int") %>%
      left_join(musicbyseed, by = "seed_int")
    
    write.csv(byseed_counts, demo_byseed_output)


  ## totals
      # aside: to replace NAs with zeroes:
      tmp <- byseed_counts
      tmp[is.na(tmp)] <- 0
    
    total_count <- byseed_counts %>%
      ungroup() %>%
      group_by(mode,type) %>%
      summarize_each(funs(sum(., na.rm = TRUE)), 5:23)


## plots

      agebyseed %>%
        gather("age", "n", 2:8) %>% # convert to long for plotting
        mutate(prop = n/)
      ggplot(aes(x = seed_int, y = n, group = age, fill = age)) +
      geom_bar(stat = "identity", position = position_dodge())
  
      ggplot(agebyseed, aes(x = music, y = n, group = seed_ioi, fill = seed_ioi)) +
        geom_bar(stat="identity", position = position_dodge()) +
        facet_grid(seed_ioi~.) +
        theme_bw() +
        labs(x = 'music experience rating', 
             y = 'count') + 
        theme(panel.grid.minor=element_blank(),
              panel.grid.major=element_blank(),
              axis.title = element_text(size = 26),
              #axis.text = element_text(size = 22),
              legend.position='right', #c(.9,.9)
              legend.title = element_text(size=22),
              legend.text = element_text(size = 16))
  

      ggplot(age_count, aes(x = age, y = n, group = seed_ioi, fill = seed_ioi)) +
        geom_bar(stat="identity", position = position_dodge()) +
        facet_grid(seed_ioi~.) +
        theme_bw() +
        labs(x = 'age range (years)', 
             y = 'count') + 
        theme(panel.grid.minor=element_blank(),
              panel.grid.major=element_blank(),
              axis.title = element_text(size = 26),
              #axis.text = element_text(size = 22),
              legend.position='right', #c(.9,.9)
              legend.title = element_text(size=22),
              legend.text = element_text(size = 16))

            
      ggplot(gender_count, aes(x = gender, y = n, group = seed_ioi, fill = seed_ioi)) +
        geom_bar(stat="identity", position = position_dodge()) +
        facet_grid(seed_ioi~.) +
        theme_bw() +
        labs(x = 'gender (years)', 
             y = 'count') + 
        theme(panel.grid.minor=element_blank(),
              panel.grid.major=element_blank(),
              axis.title = element_text(size = 26),
              #axis.text = element_text(size = 22),
              legend.position='right', #c(.9,.9)
              legend.title = element_text(size=22),
              legend.text = element_text(size = 16))
      

      ggplot(demo_counts, aes(x = gender, y = n, group = type, fill = type)) +
        geom_bar(stat="identity", position = position_dodge())+
        facet_grid(type~mode)
      
      

### workshop ----------

# cov as function of seed int
tempo_return %>%
  ggplot(aes(x = seed_int, y = cov)) +
  geom_point(aes(color = seed), size = 2, alpha = .5) +
  geom_smooth(method = lm, se = FALSE, color = "#999999", size = 2) +
  theme_bw()

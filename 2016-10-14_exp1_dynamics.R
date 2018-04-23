
# Just dynamical systems analysis of taps?
# keep using older separate script for drift analysis?

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
      smt_bychain_output <- "tables/smt_bychain_table.csv"
      smt_byseed_output <- "tables/smt_byseed_table.csv"

      taps_bychain_output <- "tables/taps_bychain_table.csv"
      taps_byseed_output <- "tables/taps_byseed_table.csv"
  
      tempo_bychain_output <- "tables/tempo_bychain_table.csv" # for reporting
      tempo_byseed_output <- "tables/tempo_byseed_table.csv"  # for reporting

      drift_lm_output <- "tables/drift_lm_table.csv"
      # also output (names below in script): tempo_return, lm_ar1, lm_ar1_bias

      # to be updated - info with descriptives, tables of data by condition of interest
      # info with linear fits - by spaghetti and by drift

      taps_output <- "tables/taps_sr_2016-10-03.csv"
      taps_sr_prac_output <- "compiled data/taps_sr_prac_2016-10-03.csv"
      taps_smt_output <- "compiled data/taps_smt_2016-10-03.csv"
      
      taps_summary_output <- "compiled data/summary_sr_2016-10-03.csv"
      taps_sr_prac_summary_output <- "compiled data/summary_sr_prac_2016-10-03.csv"
      taps_smt_summary_output <- "compiled data/summary_smt_2016-10-03.csv"

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
      #select(filename, chain_name, seed , seed_int, generation = chain, tap_number, tap_int) 
      select(-X)
    
    taps_smt_summary_raw <- read.csv(taps_smt_summary_input)
    taps_smt_summary <- taps_smt_summary_raw %>%
      mutate(seed = as.factor(seed)) %>%
      mutate(seed_int = ifelse(seed == 0, 300, 
                               ifelse(seed == 1, 600, 
                                      ifelse(seed == 2, 900, 
                                             ifelse(seed == 3, 1200, NA))))) %>%
      #select(filename, chain_name, seed , seed_int, generation = chain, tap_number, tap_int) 
      select(-X)

           

# SR TEMPO: prepare data: spaghetti plot = graph of solution to finite differential equation ------------------------------------------

    # for spaghetti plot, need to add seeds to summary df
    # seeds = initial conditions 
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

# SR TEMPO: spaghetti plot = grapch of solution to finite differential equation ------------------------------------------
# figure out how to do sub-scripts on plots

    # *** PLOT state = MEAN TEMPO ***
    ggplot(taps_and_seeds_summary, aes(x = generation, y = mean_tempo, group = chain_name, color = seed)) +
      geom_point(size = 3, alpha = .75) + 
      geom_line(alpha = .5, lwd = 2) +
      #geom_smooth(se = FALSE) +
      theme_bw() +
      labs(x = 'iteration i', 
           y = 'x(i) ms') + 
      theme(panel.grid.minor=element_blank(),
            panel.grid.major=element_blank(),
            axis.title = element_text(size = 26),
            axis.text = element_text(size = 22),
            legend.position='right', #c(.9,.9)
            legend.title = element_text(size=22),
            legend.text = element_text(size = 16)) +
      scale_x_continuous(breaks=seq(-1,18,1)) +
      scale_y_continuous(breaks=c(300,600,900,1200)) +
      scale_color_discrete(name="x(0) ms",
                           breaks=c('0','1','2','3'),
                           labels=c("300", "600", "900", "1200"))
                
    # *** VARIABILITY SPAGHETTI DRIFT PLOT ***
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

    # *** 2nd VARIABILITY SPAGHETTI DRIFT PLOT ***
    # change in cov each generation
    ggplot(taps_summary, aes(x = generation, y = cov, group = chain_name, color = seed)) +
      geom_point(size = 3, alpha = .75) + 
      # trying some other functional relationships between y and x...alt: y ~ poly(x,2)
      # maybe need to facet by seed to see what's going on in each chain better...
      geom_smooth(aes(x = generation, y = comv, group = seed, color = seed),
                  method = lm, formula = y ~ log(x), se=FALSE, size = 1) +
      #geom_line(alpha = .5, lwd = 1) +
      theme_bw() +
      labs(x = 'generation (position in chain)', 
           y = 'cov') + 
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
# creates dataframe to join later with tempo_bychain_table summary
# started withIT02 script (for drift linear fits) but spag by chain is simpler
  
  # use taps_and_seeds_summary df from spaghetti plots
  
  data <- taps_and_seeds_summary
  conds <- unique(data$chain_name)
  
  nrowOutput <- length(conds)
  ncolOutput <- 4
  output <- data.frame(matrix(ncol = ncolOutput, nrow = nrowOutput))
  cnames <- c("chain_name", "slope", "y_int","rsq")
  colnames(output) <- cnames
  
  output$chain_name <- conds
  
  n = 1
  
  for (j in 1:length(conds)) {
        
      lm_out <- lm(median_tempo ~ generation, 
                   data = subset(data,chain_name == conds[j]))
      
      #output$chain_name[n] <- conds[j] # ?not putting correct string value, treating like int, don't know why
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
    # ALSO add linear fit params from earlier section
    tempo_bychain_table <- seedsbychain %>%
      inner_join(firstsbychain, by = c("mode","type","seed","seed_int","chain_name","max_gen")) %>%
      inner_join(finalsbychain, by = c("mode","type","seed","seed_int","chain_name","max_gen")) %>%
      mutate(drift_means = final_meantempo - seed_meantempo,
             change_sd = final_sd - first_sd,
             change_cov = final_sd - first_cov,
             drift = final_tempo- seed_tempo,
             change_mad = final_mad - first_mad,
             change_comv = final_comv - first_comv) %>%
      left_join(lm_spag_bychain, by = "chain_name")
    
    write.csv(tempo_bychain_table, tempo_bychain_output)
    
    
  # BY SEED SUMMARIES (rolling up by-chain summary):
    nchains <- tempo_bychain_table %>%
      group_by(seed_int) %>%
      summarize(nchains = n())
    
    tempo_byseed_table <- tempo_bychain_table %>%
      group_by(mode,type,seed,seed_int) %>%
      select(-chain_name) %>%
      summarize_each(funs(mean)) %>%
      left_join(nchains, by = "seed_int")
    
    write.csv(tempo_byseed_table, tempo_byseed_output)


  # just a quick overall calc...
  chains_overall <- finalsbychain %>%
    group_by(mode) %>%
    summarize(n = n(),
              min_final = min(final_meantempo),
              max_final = max(final_meantempo),
              mean_mean = mean(final_meantempo),
              sd_mean = sd(final_meantempo),
              se_mean = sd_mean/n,
              mean_cov = mean(final_cov),
              sd_cov = sd(final_cov),
              se_cov = sd_cov/n)

              
# TAPS SUMMARY: drift - plots for central values & variability ------------------------------
# use data frame from table created in section "TAP SUMMARY tables comparing seeds..."
  
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
           y = 'change cov = final - first') + 
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
  
  nrowOutput <- 1 # fitting over all conditions (would be 4 if fit each seed)
  ncolOutput <- 4
  output <- data.frame(matrix(ncol = ncolOutput, nrow = nrowOutput))
  cnames <- c("cond", "slope", "y_int","rsq")
  colnames(output) <- cnames
  
  # linear fit of drift as a function of seed interval
  lm_out <- lm(drift ~ seed_int, data = data)
    output$cond <- 'exp_1'             
    output$slope <- coefficients(lm_out)[2]
    output$y_int <- coefficients(lm_out)[1]
    output$rsq <- summary(lm_out)$r.squared
  
  # clean up opportunity & compute x-intercept
  lm_drift <- output %>%
    mutate(x_int = -y_int / slope)
  
  write.csv(lm_drift,drift_lm_output)
  
  # testing something...
  plot(x = c(1,50,150,300)*lm_drift$slope, y = lm_drift$intercept)
  
  # clear holding variables
  rm(data,conds,nrowOutput,ncolOutput,output,cnames,lm_out)

         
# TAPS phase return plot & deltas ------------------------


  # TAPS: individual intervals (preserves serial order across generations - more important for meter than tempo)
  
  ggplot(taps_at, aes(x = source_tap_int, y = tap_int, group = chain_name, color = as.factor(tap_number))) +
    geom_point(size = 2) +
    geom_line(aes(x = source_tap_int, y = source_tap_int, color = seed)) +
    #geom_smooth(data = taps_at, aes(x = source_tap_int, y = tap_int, group = seed), 
     #           alpha = .2, size = 2) +
    labs(title = 'return plot - tap intervals', 
         x = 'interval i-1 (ms)', y = 'interval i (ms)') + 
    theme(plot.title = element_text(lineheight=.8, face="bold"),
          panel.grid.minor=element_blank(),
          panel.grid.major=element_blank(),
          axis.title.x = element_text(size=20),
          axis.text.x = element_text(size = 16),
          axis.title.y  = element_text(size =20),
          axis.text.y = element_text(size = 16)) +
    coord_cartesian(xlim = c(0,2500), ylim=c(0, 2500)) +   
    facet_wrap(~seed)

taps <- taps_at %>%
  filter(rejects == "n") %>%
  group_by(mode,type,seed,chain_name,filename,id,sourceId,generation, tap_number)

taps_at %>%
  filter(rejects == "n") %>%
  ggplot(aes(x = source_tap_int, y = tap_diff, group = chain_name, color = seed)) +
  geom_point() + #geom_line() +
  facet_wrap(~generation)

taps_at %>%
  filter(rejects == "n") %>%
  ggplot(aes(x = tap_number, y = tap_diff, group = chain_name, color = seed)) +
  geom_point() + geom_line() +
  facet_wrap(~generation)

  
##### TAP SUMMARY: return plot for tempo - data preparation -----
# what's the difference between AR(1) and phase return??
# kinda updated plots for dynamics...

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

  
# return plot for mean tempo
ggplot(tempo_return, aes(x = source_mean_tempo, y = mean_tempo, group = chain_name, color = seed)) +
  geom_point(size = 3, alpha = .5) +
  geom_smooth(aes(group = seed), method = lm, se = FALSE, size = 1.5, alpha = .5) +
  theme_bw() +
  labs(title = 'state space representation', 
       x = 'x(i)', y = 'x(i+1)') + 
  theme(plot.title = element_text(lineheight=.8, face="bold"),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        axis.title.x = element_text(size=20),
        axis.text.x = element_text(size = 16),
        axis.title.y  = element_text(size =20),
        axis.text.y = element_text(size = 16)) +
  coord_cartesian(xlim = c(0,1200), ylim=c(0, 1200)) 


  # 'bias' (x&g 2010) -- 
  # [tap_n - tap_n-1] X tap_n-1
  ggplot(tempo_return, aes(x = source_mean_tempo, y = delta_mean, group = chain_name, color = seed)) +
    geom_point(size = 2) +
    geom_smooth(aes(group = seed), method = lm, se = FALSE) +
    theme_bw() +
    labs(title = 'partial derivative wrt x(i)', 
         x = 'x(i)', y = 'x(i+1) - x(i)') + 
    theme(plot.title = element_text(lineheight=.8, face="bold"),
          panel.grid.minor=element_blank(),
          panel.grid.major=element_blank(),
          axis.title.x = element_text(size=20),
          axis.text.x = element_text(size = 16),
          axis.title.y  = element_text(size =20),
          axis.text.y = element_text(size = 16)) +
    coord_cartesian(xlim = c(0,1200), ylim=c(0, 1200)) 

# [tap_n - tap_n-1] X iteration
ggplot(tempo_return, aes(x = generation, y = delta_mean, group = chain_name, color = seed)) +
  geom_point(size = 2) +
  geom_smooth(aes(group = seed), method = lm, se = FALSE) +
  theme_bw() +
  labs(title = 'first discrete derivative wrt time', 
       x = 'iteration i', y = 'x(i+1) - x(i)') + 
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

      # median smt by generation (with loess line)
      ggplot(taps_smt_summary, aes(x = generation, y = median_tempo, group = chain_name, color = seed)) +
        geom_point(size = 2) +
        geom_line() +
        geom_smooth(data = taps_smt_summary, aes(x = generation, y = median_tempo, group = seed), 
                    method = lm, se = FALSE, alpha = .5, size = 2) +
        labs(title = 'median SMT by generation', 
             x = 'generation', y = 'median tempo (ms)') + 
        theme_bw() +
        theme(plot.title = element_text(lineheight=.8, face="bold"),
              panel.grid.minor=element_blank(),
              panel.grid.major=element_blank(),
              axis.title.x = element_text(size=20),
              axis.text.x = element_text(size = 16),
              axis.title.y  = element_text(size =20),
              axis.text.x = element_text(size = 16)) +
        coord_cartesian(ylim=c(0, 1200))        
        
      # median variability smt by generation 
      # update y in parent plot and geom_smooth for different measure
      ggplot(taps_smt_summary, aes(x = generation, y = comv, group = chain_name, color = seed)) +
        geom_point(size = 2) +
        geom_line() + 
        geom_smooth(data = taps_smt_summary, aes(x = generation, y = comv, group = seed), 
                    alpha = .2, size = 2) +
        labs(title = 'SMT coefficient of median abs deviation (MAD) by generation', 
             x = 'generation', y = 'coMv') + 
        theme(plot.title = element_text(lineheight=.8, face="bold"),
              panel.grid.minor=element_blank(),
              panel.grid.major=element_blank(),
              axis.title.x = element_text(size=20),
              axis.text.x = element_text(size = 16),
              axis.title.y  = element_text(size =20),
              axis.text.x = element_text(size = 16)) #+
        #coord_cartesian(ylim=c(0, 2500))        

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

# smt: tables -------------------------------
      smt_bychain_table <- taps_smt_summary %>%
        group_by(mode,type,seed,seed_int,chain_name) %>%
        summarize(mean_smt = mean(median_tempo),
                  mean_meansmt = mean(mean_tempo),
                  mean_sd = mean(sd_taps),
                  mean_mad = mean(mad_taps),
                  mean_cov = mean(cov),
                  mean_comv = mean(comv))
      write.csv(smt_bychain_table, smt_bychain_output)

      smt_byseed_table <- smt_bychain_table %>%
        group_by(mode,type,seed,seed_int) %>%
        summarize(mean_smt_seed = mean(mean_smt),
                  mean_meansmt_seed = mean(mean_meansmt),
                  mean_sd_seed = mean(mean_sd),
                  mean_mad_seed = mean(mean_mad),
                  mean_cov_seed = mean(mean_cov),
                  mean_comv_seed = mean(mean_comv))
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

# smt: return -----------------------------------
  
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
  
### workshop ----------

# cov as function of seed int
tempo_return %>%
  ggplot(aes(x = seed_int, y = cov)) +
  geom_point(aes(color = seed), size = 2, alpha = .5) +
  geom_smooth(method = lm, se = FALSE, color = "#999999", size = 2) +
  theme_bw()

# individual tap intervals - looking for expected bifurcations

taps_at %>%
  ggplot(aes(x = tap_number, y = tap_int, group = chain_name, color = seed)) +
  geom_point() + geom_line() +
  coord_cartesian(ylim=c(0,2500)) +
  facet_grid(seed~generation)

taps_at %>%
  filter(seed == 3) %>%
  ggplot(aes(x = tap_number, y = tap_int, group = chain_name, color = seed)) +
  geom_point() + geom_line() +
  facet_wrap(~generation)

taps_at %>%
  filter(seed == 3) %>%
  ggplot(aes(x = source_tap_int, y = tap_int, color = as.factor(tap_number))) +
  geom_point(size = 4, alpha = .75) +
  theme_bw() +
  # maybe confusing - plot arrows from tap to tap:
  #geom_segment(aes(xend=c(tail(source_tap_int, n=-1), NA), yend=c(tail(tap_int, n=-1), NA)), arrow = arrow()) +
  facet_wrap(~generation)

taps_at %>%
  filter(seed == 3) %>%
  ggplot(aes(x = source_tap_int, y = (tap_int - source_tap_int), color = as.factor(tap_number))) +
  geom_point(size = 4, alpha = .75) +
  theme_bw() +
  # maybe confusing - plot arrows from tap to tap:
  #geom_segment(aes(xend=c(tail(source_tap_int, n=-1), NA), yend=c(tail(tap_int, n=-1), NA)), arrow = arrow()) +
  facet_wrap(~generation)



taps_at %>%
  filter(seed == 2) %>%
  ggplot(aes(x = source_tap_int, y = tap_int, color = chain_name)) +
  geom_point(size = 4, alpha = .75) +
  facet_wrap(~generation)



taps_at %>%
  ggplot(aes(x = source_tap_int, y = tap_int, group = chain_name, color = as.factor(tap_number))) +
  geom_point(size = 2) + #geom_line() +
  coord_cartesian(ylim=c(0,2000)) +
  facet_grid(seed~generation)


taps_at %>%
  ggplot(aes(x = source_tap_int, y = tap_diff, group = chain_name, color = as.factor(tap_number))) +
  geom_point(size = 2) + #geom_line() +
  coord_cartesian(ylim=c(-500,500)) +
  facet_grid(seed~generation)


##### estimate a's for linear finite difference equation:

a^max_gen = x(max_gen) / x(seed)

a = (x(max_gen) / x(seed)) ^ 1/max_gen

a_param <- tempo_bychain_table %>%
  mutate(a = (final_meantempo / seed_meantempo) ^ (1/max_gen))

write.csv(a_param, "tables/dyn_tempo_bychain_plusAs.csv")

  

##### seed 1200 plots ------

theme_bw() +
  labs(x = 'iteration i', 
       y = 'x(i) ms') + 
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        axis.title = element_text(size = 26),
        axis.text = element_text(size = 22),
        legend.position='right', #c(.9,.9)
        legend.title = element_text(size=22),
        legend.text = element_text(size = 16)) +
  scale_x_continuous(breaks=seq(-1,18,1)) +
  scale_y_continuous(breaks=c(300,600,900,1200)) +
  scale_color_discrete(name="x(0) ms",
                       breaks=c('0','1','2','3'),
                       labels=c("300", "600", "900", "1200"))

# save as 600 for width

taps_at %>%
  filter(seed == 3) %>%
  ggplot(aes(x = tap_number, y = tap_int, group = chain_name, color = seed)) +
  geom_point() + geom_line() +
  theme_bw() +
    labs(x = 'tap number', 
         y = 'ITI (ms)') + 
    theme(panel.grid.minor=element_blank(),
          panel.grid.major=element_blank(),
          axis.title = element_text(size = 26),
          #axis.text = element_text(size = 16),
          legend.position='none') + #right', #c(.9,.9)
    scale_x_continuous(breaks=seq(-1,11,1)) +
    facet_wrap(~generation)


taps_at %>%
  filter(seed == 3) %>%
  ggplot(aes(x = source_tap_int, y = tap_int, color = as.factor(tap_number))) +
  geom_point(size = 4, alpha = .75) +
  theme_bw() +
  labs(x = 'ITI of generation i (ms)', 
       y = 'ITI of generation i+1 (ms)') + 
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        axis.title = element_text(size = 26),
        #axis.text = element_text(size = 16),
        legend.position='none') + #right', #c(.9,.9)
  facet_wrap(~generation)

taps_at %>%
  filter(seed == 3) %>%
  ggplot(aes(x = source_tap_int, y = (tap_int - source_tap_int), color = as.factor(tap_number))) +
  geom_point(size = 4, alpha = .75) +
  theme_bw() +
  labs(x = 'ITI of generation i (ms)', 
       y = 'ITI gen-i+1 - ITI gen-i (ms)') + 
    theme(panel.grid.minor=element_blank(),
          panel.grid.major=element_blank(),
          axis.title = element_text(size = 26),
          #axis.text = element_text(size = 16),
          legend.position='none') + #right', #c(.9,.9))) +
    facet_wrap(~generation)
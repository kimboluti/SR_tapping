
# set up------------------------------

  # Set directory 
    
    #setwd("C:/Users/Ellie/Google Drive/experiments & projects/iterated tapping - tempo/ANALYSIS - 10-2016 (diss)/Experiment 2 (IT02 -- lab iterations)")
    original_directory <- getwd()
  
  # Load libraries 
      library(stringr)
      library(tidyr)
      library(ggplot2)
      library(plyr)
      
      library(dplyr) # load dplyr after plyr 
    
  # input file names
    # serial reproduction (SR) TAPS - e-prime (EP)
    taps_input <- "compiled data/taps_ep_2016-10-10.csv"
    taps_summary_input <- "compiled data/summaryacrossblocks_ep_2016-10-19.csv"
    taps_summary_byblock_input <- "compiled data/summary_ep_2016-10-19.csv"
    
    # self-paced motor tempo (MT) TAPS
    taps_mt_input <- "compiled data/taps_ep_mt_newfilter_2016-10-31.csv"
    taps_mt_summary_input <- "compiled data/summaryacrossblocks_ep_mt_2016-10-31.csv"
    taps_mt_summary_byblock_input <- "compiled data/summary_ep_mt_2016-10-31.csv"
    
    # SYNC TAPS (if used in the future)
    taps_sync_input <- "compiled data/taps_ep_sync_2016-10-10.csv"
    taps_sync_summary_input <- "compiled data/summaryacrossblocks_ep_sync_2016-10-19.csv"
    taps_sync_summary_byblock_input <- "compiled data/summary_ep_sync_2016-10-19.csv"
    
    # *** SR TAPS - adjusted means (AM) from stimulus generation
# *** used this for dissertation analysis:
    taps_summary_am_input <- "compiled data/summary_am_2016-10-04.csv"
    # SR TAPS - AM summary of adjusted P's (which blocks were used) for subsetting ep sr for variability
    stim_gen_blocksused <- "compiled data/am_included_blocks_summary_2016-10-28.csv" 
    
    # B&S data - to add (or separate script) in future; start with Exp 1 demographics
    survey_input <- "b&s data/survey_ALL.csv"

# tables: output file names

  mt_wide_output <- "tables/mt_wide_table.csv"
  mt_long_output <- "tables/mt_long_table.csv"
  mt_bycond_output <- "tables/mt_bycond_table.csv"

  #taps_bychain_output <- "tables/taps_bychain_table.csv"
  #taps_byseed_output <- "tables/taps_byseed_table.csv"

  tempo_bychain_output <- "tables/tempo_bychain_table.csv" # for reporting
  tempo_bycond_output <- "tables/tempo_bycond_table.csv"  # for reporting


  drift_lm_bycohort_output <- "tables/drift_lm_bycohort_table.csv"  
  drift_lm_bycond_output <- "tables/drift_lm__bycond_table.csv" 



# ***AM - SR: load adjusted means serial reproduction summary data -------

  tempo_am <- read.csv(taps_summary_am_input) %>%
    rename(mean_tempo = mean_tempo_am) %>%
    mutate(mode = as.factor(ifelse(mode=="a","auditory",
                         ifelse(mode=="v","visual",NA))),
           train_type = as.factor(ifelse(train_type=="or","observe",
                               ifelse(train_type=="sc","synchronize",NA)))) %>%
    select(-X)

# ***AM - SR: spaghetti df composition and plots -------
  
  # for spaghetti plot, need to add seeds to summary df
  # expect nrow to be nchains = 140
  seeds <- tempo_am %>%
    group_by(exp_name, mode, train_type, cohort, seed, seed_int, chain_name) %>% 
    summarize(mean_tempo = mean(seed_int)) %>%
    mutate(generation = 0)
           
  # join taps and seeds summaries
  tempo_with_seeds <- tempo_am %>%
    bind_rows(seeds) %>%
    mutate(seed = as.factor(seed))

  
  # *** SPAGHETTI DRIFT PLOT ***
  
  # re-do 2016-11-20
  tempo_with_seeds %>%
    ggplot(aes(x=generation, y=mean_tempo, group=chain_name)) +
    geom_line(alpha = .25, lwd = 1, color = "#999999") +
    geom_smooth(aes(group=seed, color=seed), lwd = 1.5, alpha = .75, se = F) +
    theme_bw() +
    labs(title = '(A) Serial Reproduction Chains',
         x = 'Iteration', 
         y = 'Tempo (ms)') +
    theme(panel.grid.minor=element_blank(),
          panel.grid.major=element_blank(),
          plot.title = element_text(size=14),
          axis.title = element_text(size = 11),
          axis.text = element_text(size = 11),
          #axis.text.x = element_text(angle = 90),
          strip.text.x = element_text(size=11, lineheight=.5),
          strip.text.y = element_text(size=11, lineheight=.5),
          #legend.position='right', #c(.9,.9) # 'none'
          legend.title = element_text(size=11),
          legend.text = element_text(size = 11)) +
    scale_color_discrete(name="Seed\nTempo (ms)") +
    #scale_x_continuous(limits=c(-1,16), breaks=c(0,5,10,15)) +  
    facet_grid(mode~train_type)
  
  ggsave("tempo_spag_big_2016-11-20.png", width=6.5, height=5.45, dpi=100) # 14/12/11 font size
  ggsave("tempo_spag_small_2016-11-20.png", width=4, height=3.25, dpi=100) # 12/10/8 font size


# older:
  tempo_with_seeds %>%
    #filter(exp_name == "a_or") %>%
    ggplot(aes(x = generation, y = mean_tempo, group = chain_name)) +
    #  geom_point(size = 1, alpha = .5) + 
    geom_line(alpha = .25, lwd = 1, color = "#999999") +
    geom_smooth(aes(group = seed, color = seed), lwd = 1.5, alpha = .75, se = F) +
    theme_bw() +
    labs(title = '(A) Serial Reproduction Chains',
       x = 'Generation', 
       y = 'Reproduced tempo (ms)') +
    coord_cartesian(ylim = c(0,3500)) +
    #theme(legend.position = 'bottom') +
    facet_grid(mode~train_type)


# save dim: 600 x 515 
# save dim (smaller): 350
# save dim (sqaure): 500x400 -- 10/31/16


# AM - SR: distribution histograms -------
# start with df from spaghetti plots

  # (1) points or boxplot seems to illustrate the difference in means best:
      
      # both initial and final plotted together:
      tempo_with_seeds %>%
        filter(generation == 0 | generation == 15,
               exp_name == "a_or") %>%
        mutate(generation = as.factor(as.character(generation))) %>%
        ggplot(aes(x = seed, y = mean_tempo, color = generation)) +
        geom_point(size = 3, alpha = .5) + geom_line(alpha = .25) +
        #geom_boxplot() +
        labs(title = '(A) Comparison of Initial and Final Tempi',
             x = 'Chain initiation tempo', 
             y = 'Mean tempo (ms)') +
        # why isn't key relabelling working?
        #scale_fill_discrete(name = "Generation"),
         #                   labels = c("Seed", "Final")) +
        theme_bw() + 
        facet_grid(mode~train_type)
      
      
    # initial alone. save dim: try 300 X 500 for facte by exp
    tempo_with_seeds %>%
      filter(generation == 0) %>%
      mutate(generation = as.factor(as.character(generation))) %>%
      ggplot(aes(x = seed, y = mean_tempo, color = seed)) +
      #geom_point(size = 3, alpha = .5, position = "jitter") + geom_line(alpha = .25) +
      geom_boxplot() +
      labs(title = '(A) Initial Tempi',
           x = 'Chain initiation tempo', 
           y = 'Mean tempo (ms)') +
      coord_cartesian(ylim=c(0,3500)) +        
      theme_bw() + 
      facet_grid(exp_name~.)
    
    # final alone. save dim: try 300 X 500 for facte by exp
    tempo_with_seeds %>%
      filter(generation == 15) %>%
      mutate(generation = as.factor(as.character(generation))) %>%
      ggplot(aes(x = seed, y = mean_tempo, color = seed)) +
      #geom_point(size = 3, alpha = .75, position = "jitter") + 
      #geom_line(alpha = .25) +
      geom_boxplot() +
      labs(title = '(C) Final Tempi',
           x = 'Chain initiation tempo', 
           y = 'Mean tempo (ms)') +
      # why isn't key relabelling working?
      #scale_fill_discrete(name = "Generation"),
      #                   labels = c("Seed", "Final")) +
      coord_cartesian(ylim=c(0,3500)) +        
      theme_bw() + 
      facet_grid(exp_name~.)


    # ***(2) horizontal lines by condition
    # save dim: 275 X 500 for no legend
    # save dim (smaller): 177 X 325
    
    # init hozo lines
    tempo_with_seeds %>%
      #filter(exp_name == "a_or") %>%
      ggplot(aes(x = seed_int, y = mean_tempo, color = seed)) +
      geom_hline(aes(yintercept = seed_int, color = seed), size = 1.5, alpha = .75) +
      coord_cartesian(ylim=c(0,3500)) +  
      theme_bw() +
      labs(title = '(A) Seed Tempi',
           x = '', 
           y = 'Mean tempo (ms)') +
      theme(legend.position='none') +
      facet_grid(exp_name~.)
    

    # final hozo lines
    tempo_with_seeds %>%
      filter(generation == 15) %>%
      mutate(seed = as.factor(seed)) %>%
      group_by(exp_name, seed) %>%
      summarize(yint = mean(mean_tempo, na.rm=T)) %>%
      ggplot(aes(x = seed_int, y = mean_tempo, color = seed)) +
      geom_hline(aes(yintercept=yint, color = seed), size = 1.5, alpha = .75) +
      coord_cartesian(ylim=c(0,3500)) +  
      theme_bw() +
      labs(title = '(C) Final Tempi',
           x = '', 
           y = '') +
      theme(legend.position='none') +
      facet_grid(exp_name~.)
    

  
    # (3) true histograms are not very informative for n chains = 5 per condition
    # just in case extra script for hist plots below
    
  # just in case, though: initial stimuli histogram
  seeds %>%
    #filter(exp_name == "a_or") %>%
    ungroup() %>% mutate(seed = as.factor(seed)) %>%
    ggplot(aes(x = seed_int, color = seed)) + # fill = seed to fill with that color
    #geom_histogram(binwidth = 50, position = "dodge", fill = "white") +
    #geom_vline(aes(xintercept=seed_int, color = seed), size = 2) +
    geom_hline(aes(yintercept=seed_int, color = seed), size = 1.5, alpha = .75) +
    #geom_freqpoly(binwidth = 20, pad = TRUE) +
    #geom_point(aes(y = mean_tempo), size = 3)
    #geom_boxplot(aes(y = mean_tempo))
    facet_grid(exp_name~.)
  
  #geom_histogram(aes(y=..ncount..),binwidth=25, position="dodge") + # count normalized to 1 (but how?)
  #geom_freqpoly(pad=TRUE, binwidth=25) +
  #nifty for adding vertical line:
  #geom_vline(aes(xintercept=mean(mean_tempo, na.rm=T)),   # Ignore NA values for mean
   #          color="red", linetype="dashed", size=1)
  
  # just in case, though: final stimuli histogram
  tempo_am %>%
    filter(generation == 15) %>%
    mutate(seed = as.factor(seed)) %>%
    group_by(exp_name, seed) %>%
    summarize(xint = mean(mean_tempo, na.rm=T)) %>%
    ggplot(aes(x = mean_tempo, color = seed)) +
    #geom_histogram(aes(y = ..count..), binwidth = 50, position = "dodge", fill = "white") +
    #geom_vline(aes(xintercept=xint, color = seed), size = 2) +
    geom_hline(aes(yintercept=xint, color = seed), size = 1.5, alpha = .75) +
    facet_grid(exp_name~.)
  
    
  # (4) rotated histograms - still not as visually useful as the hozo lines
    
    # initial stimuli histogram - flipped
    tempo_with_seeds %>%
      filter(generation == 0) %>%
      ggplot(aes(x = seed_int, color = seed, fill = seed)) + # fill = seed to fill with that color
      geom_histogram(binwidth = 75, position = "dodge") +
      coord_flip() +
      facet_grid(exp_name~.)
    
    # final stimuli histogram - flipped
    tempo_with_seeds %>%
      filter(generation == 15) %>%
      ggplot(aes(x = mean_tempo, color = seed, fill = seed)) +
      geom_histogram(aes(y = ..count..), binwidth = 70, position = "dodge") +
      coord_flip() +
      facet_grid(exp_name~.)


# ***AM - SR: drift df composition -------
  
  # add first generation 
  first <- tempo_am %>%
    filter(generation == 1) %>%
    rename(first_tempo = mean_tempo,
           first_generation = generation)
  
  compare <- tempo_am %>%
    filter(generation == 15) %>%
    rename(final_tempo = mean_tempo,
           final_generation = generation) %>%
    left_join(seeds, by = c("exp_name", "mode", "train_type", "cohort", "seed", "seed_int", "chain_name")) %>%
    rename(seed_tempo = mean_tempo,
           seed_generation = generation) %>%
    left_join(first, by = c("exp_name", "mode", "train_type", "cohort", "seed", "seed_int", "chain_name")) %>%
    mutate(seed = as.factor(seed),
           drift = final_tempo - seed_tempo,
           final_minus_first = final_tempo - first_tempo,
           drift_to_first = first_tempo - seed_tempo) %>%
    select(exp_name, mode, train_type, cohort, seed, seed_int, chain_name,
           seed_generation, seed_tempo, 
           first_generation, first_tempo, drift_to_first,
           final_generation, final_tempo, final_minus_first, drift)
  
  # drift 
  compare %>%
    ggplot(aes(x = seed_int, y = drift, color = seed)) +
    geom_point(size = 3, alpha = .5) +
    geom_smooth(aes(x = seed_int, y = drift, group = exp_name), method = lm, se = TRUE) + # SE uses 95% CI by default
    facet_grid(mode~train_type)
  
  # final tempo of each chain by seed tempo
  compare %>%
    ggplot(aes(x = seed_int, y = final_tempo, color = seed)) +
    geom_point(size = 3, alpha = .5) +
    #geom_smooth(aes(x = seed_int, y = drift, group = exp_name), method = lm, se = TRUE) + # SE uses 95% CI by default
    facet_grid(mode~train_type)

  


# AM - SR: test - linear fits - final x seed -------
# start with compare df from earlier
  
  data <- compare
  conds <- unique(data$exp_name)
  
  nrowOutput <- length(conds)
  ncolOutput <- 11
  output <- data.frame(matrix(ncol = ncolOutput, nrow = nrowOutput))
  cnames <- c("exp_name", "slope", "slope_p", "y_int", "y_int_p",
              "F_val", "F_numdf", "F_dendf", "F_sig", "rsq", "x_int")
  
  colnames(output) <- cnames
  
  
  n = 1
  
  for (j in 1:length(conds)) {
    
    lm_out <- lm(final_tempo ~ seed_tempo, 
                 data = subset(data,exp_name == conds[j]))
    
    output$exp_name[n] <- as.character(conds[j]) 
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
  
  
  # save as table:
  write.csv(output,"tables/lm_am_sr_bycond_finalxseed_2016-10-26.csv")
  
  # check that i'm fitting what i think i am:
  dataplot <- data %>%
    ggplot(aes(x = seed_tempo, y = final_tempo)) + 
    geom_point(alpha = .5) + geom_smooth(method = lm, se = FALSE) +
    facet_wrap(~exp_name)
  
  dataplot 
  
  dataplot + geom_smooth(aes(x = seed_tempo, y = final_tempo, group = cohort), method = lm, se = F)
  
  # compare to lm fits:
  dataplot +
    geom_abline(data = output, aes(intercept = y_int, slope = slope), linetype = 2, size = 1.5) +
    facet_wrap(~exp_name)
  
  # plot residuals or res normed by seed (for last condition fit, at least)
  res <- data.frame(as.numeric(resid(lm_out))) %>%
    rename(residuals = as.numeric.resid.lm_out..)
  
  data %>%
    filter(exp_name == "v_or") %>%
    bind_cols(res) %>%
    ggplot(aes(x = seed_tempo, y = res)) + geom_point()
  
  
  # clear holding variables
  #rm(data,conds,nrowOutput,ncolOutput,output,cnames,lm_out)
  rm(data,conds,nrowOutput,ncolOutput,cnames,lm_out)
  

# AM - SR: test - linear fits - ratio (final:seed) x seed -------
# start with compare df from earlier
  
  data <- compare
  conds <- unique(data$exp_name)
  
  nrowOutput <- length(conds)
  ncolOutput <- 11
  output <- data.frame(matrix(ncol = ncolOutput, nrow = nrowOutput))
  cnames <- c("exp_name", "slope", "slope_p", "y_int", "y_int_p",
              "F_val", "F_numdf", "F_dendf", "F_sig", "rsq", "y_xequal1")
  
  colnames(output) <- cnames
  
  
  n = 1
  
  for (j in 1:length(conds)) {
    
    lm_out <- lm((final_tempo/seed_tempo) ~ seed_tempo, 
                 data = subset(data,exp_name == conds[j]))
    
    output$exp_name[n] <- as.character(conds[j]) 
    output$slope[n] <- coefficients(lm_out)[2]
    output$slope_p[n] <- summary(lm_out)$coefficients[2,4]
    output$y_int[n] <- coefficients(lm_out)[1]
    output$y_int_p[n] <- summary(lm_out)$coefficients[1,4]
    output$F_val[n] <- summary(lm_out)$fstatistic[1]
    output$F_numdf[n] <- summary(lm_out)$fstatistic[2]
    output$F_dendf[n] <- summary(lm_out)$fstatistic[3]
    output$F_sig[n] <- pf(summary(lm_out)$fstatistic[1], summary(lm_out)$fstatistic[2], summary(lm_out)$fstatistic[3], lower.tail = FALSE)
    output$rsq[n] <- summary(lm_out)$r.squared 
    output$y_xequal1[n] <- (1 - coefficients(lm_out)[1])/coefficients(lm_out)[2] # x when y = 1
    
    
    n = n + 1
    
  }
  
  
  # save as table:
  write.csv(output,"tables/lm_am_sr_bycond_normedfinalxseed_2016-10-26.csv")
  
  # check that i'm fitting what i think i am:
  dataplot <- data %>%
    ggplot(aes(x = seed_tempo, y = (final_tempo/seed_tempo))) + 
    geom_point(alpha = .5) + geom_smooth(method = lm, se = FALSE) +
    facet_wrap(~exp_name)
  
  dataplot 
  
  # compare to lm fits:
  dataplot +
    geom_abline(data = output, aes(intercept = y_int, slope = slope), linetype = 2, size = 1.5) +
    facet_wrap(~exp_name)
    
  # plot residuals or res normed by seed (for last condition fit, at least)
  res <- data.frame(as.numeric(resid(lm_out))) %>%
    rename(residuals = as.numeric.resid.lm_out..)
  
  data %>%
    filter(exp_name == "v_or") %>%
    bind_cols(res) %>%
    ggplot(aes(x = seed_tempo, y = res/seed_tempo)) + geom_point()


  # clear holding variables
  #rm(data,conds,nrowOutput,ncolOutput,output,cnames,lm_out)
  rm(data,conds,nrowOutput,ncolOutput,cnames,lm_out)
  

# AM - SR: test - linear fits - relative change (drift/seed) by seed -------
# start with compare df from earlier

data <- compare
conds <- unique(data$exp_name)

nrowOutput <- length(conds)
ncolOutput <- 11
output <- data.frame(matrix(ncol = ncolOutput, nrow = nrowOutput))
cnames <- c("exp_name", "slope", "slope_p", "y_int", "y_int_p",
            "F_val", "F_numdf", "F_dendf", "F_sig", "rsq", "x_int")

colnames(output) <- cnames


n = 1

for (j in 1:length(conds)) {
  
  lm_out <- lm((drift/seed_tempo) ~ seed_tempo, 
               data = subset(data,exp_name == conds[j]))
  
  output$exp_name[n] <- as.character(conds[j]) 
  output$slope[n] <- coefficients(lm_out)[2]
  output$slope_p[n] <- summary(lm_out)$coefficients[2,4]
  output$y_int[n] <- coefficients(lm_out)[1]
  output$y_int_p[n] <- summary(lm_out)$coefficients[1,4]
  output$F_val[n] <- summary(lm_out)$fstatistic[1]
  output$F_numdf[n] <- summary(lm_out)$fstatistic[2]
  output$F_dendf[n] <- summary(lm_out)$fstatistic[3]
  output$F_sig[n] <- pf(summary(lm_out)$fstatistic[1], summary(lm_out)$fstatistic[2], summary(lm_out)$fstatistic[3], lower.tail = FALSE)
  output$rsq[n] <- summary(lm_out)$r.squared 
  output$x_int[n] <- -coefficients(lm_out)[1]/coefficients(lm_out)[2]
  
  
  n = n + 1
  
}


# save as table:
write.csv(output,"tables/lm_am_sr_bycond_normeddriftxseed_2016-10-26.csv")

# check that i'm fitting what i think i am:
dataplot <- data %>%
  ggplot(aes(x = seed_tempo, y = (drift/seed_tempo))) + 
  geom_point(alpha = .5) + geom_smooth(method = lm, se = FALSE) +
  facet_wrap(~exp_name)

dataplot 

# compare to lm fits:
dataplot +
  geom_abline(data = output, aes(intercept = y_int, slope = slope), linetype = 2, size = 1.5) +
  facet_wrap(~exp_name)

# plot residuals or res normed by seed (for last condition fit, at least)
res <- data.frame(as.numeric(resid(lm_out))) %>%
  rename(residuals = as.numeric.resid.lm_out..)

data %>%
  filter(exp_name == "v_or") %>%
  bind_cols(res) %>%
  ggplot(aes(x = seed_tempo, y = res)) + geom_point()


# clear holding variables
#rm(data,conds,nrowOutput,ncolOutput,output,cnames,lm_out)
rm(data,conds,nrowOutput,ncolOutput,cnames,lm_out)


# ***AM - SR: linear fits - drift x seed -------
# start with compare df from earlier
  
  data <- compare
  conds <- unique(data$exp_name)
  
  nrowOutput <- length(conds)
  ncolOutput <- 11
  output <- data.frame(matrix(ncol = ncolOutput, nrow = nrowOutput))
  cnames <- c("exp_name", "slope", "slope_p", "y_int", "y_int_p",
              "F_val", "F_numdf", "F_dendf", "F_sig", "rsq", "x_int")
  
  colnames(output) <- cnames
  
  
  n = 1
  
  for (j in 1:length(conds)) {
    
    lm_out <- lm(drift ~ seed_int, 
                 data = subset(data,exp_name == conds[j]))
    
    output$exp_name[n] <- as.character(conds[j]) 
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
  
  
  # save as table:
  write.csv(output,"tables/lm_am_sr_bycond_driftxseed_2016-10-25.csv")
  
  # check that i'm fitting what i think i am:
  dataplot <- data %>%
    ggplot(aes(x = seed_int, y = drift)) + 
    geom_point(alpha = .5) + geom_smooth(method = lm, se = FALSE) +
    facet_wrap(~exp_name)
  
  dataplot 
  
  # compare to lm fits:
  dataplot +
    geom_abline(data = output, aes(intercept = y_int, slope = slope), linetype = 2, size = 1.5) +
    facet_wrap(~exp_name)
  
  # plot residuals or res normed by seed (for last condition fit, at least)
  res <- data.frame(as.numeric(resid(lm_out))) %>%
    rename(residuals = as.numeric.resid.lm_out..)
  
  data %>%
    filter(exp_name == "v_or") %>%
    bind_cols(res) %>%
    ggplot(aes(x = seed_tempo, y = res)) + geom_point()
    #ggplot(aes(x = seed_tempo, y = res/seed_tempo)) + geom_point()

  
  # clear holding variables
  #rm(data,conds,nrowOutput,ncolOutput,output,cnames,lm_out)
  rm(data,conds,nrowOutput,ncolOutput,cnames,lm_out)
  


# ***AM - SR: drift plots -------
# save dim without legends: 200 X 325

# not using: relationship between final and seed tempi
compare %>%
  ggplot(aes(x = seed_int, y = final_tempo)) +
  geom_point(aes(color = seed), size = 3, alpha = .75) +
  geom_smooth(aes(group = exp_name), method = lm, se = FALSE ) +
  theme_bw() +
  labs(title = '(A) Relationship', # useful for line breaks: '(A) Relationship between \n seed and final tempo'
      x = 'Seed tempo (ms)', 
      y = 'Final tempo (ms)') +
  theme(legend.position = 'none')+
  facet_grid(exp_name~.)


  # 2016-11-20 updating drift plot -- uses df from 11-10 MT-SR script
  # 2016-12-06 note: to add CI to regression line, change SE=FALSE to SE=TRUE in geom_smooth. default is 95% CI
  ind_drift %>%
    ggplot(aes(x=seed_int, y=drift)) +
    geom_point(aes(color=seed), size = 2) + 
    geom_line(aes(group=cohort), alpha = .5, lwd = 1, color = "#999999") +
    geom_smooth(aes(group=train_type), method = lm, se = FALSE, lwd=1.25, alpha=.75, color='black') +
    geom_hline(aes(yintercept=0),color="#999999") + # no drift line
    theme_bw() +
    labs(title = '(B) Chain Drift',
         x = 'Seed Tempo (ms)', 
         y = 'Drift (ms)') +
    theme(panel.grid.minor=element_blank(),
          panel.grid.major=element_blank(),
          plot.title = element_text(size=12),
          axis.title = element_text(size = 10),
          axis.text = element_text(size = 8),
          axis.text.x = element_text(size = 6.5, angle = 90, vjust=0.5),
          #axis.text.x = element_text(angle = 90),
          strip.text.x = element_text(size=8, lineheight=.5),
          strip.text.y = element_text(size=8, lineheight=.5),
          legend.position='right', #c(.9,.9) # 'none' 
          legend.title = element_text(size=8),
          legend.text = element_text(size = 8)) +
    scale_color_discrete(name="Seed\nTempo (ms)") +
    scale_x_continuous(breaks=unique(ind_drift$seed_int)) +  
    scale_y_continuous(limits=c(-2000,2000), breaks=c(-1000,0,1000)) +
    facet_grid(mode~train_type) 
  
  # save the plot!
  ggsave("tempo_drift_big_2016-11-20.png", width=6.5, height=5.45, dpi=100) # 14/12/11 font size
  ggsave("tempo_drift_small_2016-11-20.png", width=4, height=3.25, dpi=100) # 12/10/8 font size


# older:
# * drift plot 
compare %>%
  ggplot(aes(x = seed_int, y = drift)) +
  geom_point(aes(color = seed), size = 3, alpha = .75) +
  geom_line(aes(group=cohort), alpha = .25, lwd = 1, color = "#999999") +
  geom_hline(aes(yintercept = 0), color = "#999999") +
  geom_smooth(method = lm, se=F, lwd = 1.5, alpha = .75, color = 'black') + 
  # vertical line at x-intercept saved in output from lm, above
  # works with facet_wrap(~exp_name) but not as well with facet_grid(mode~train_type)
  #geom_vline(data=output, aes(xintercept=x_int, group=exp_name), color = "#999999") +
  theme_bw() +
  labs(title = '(B) SR Chain Drift',
       x = 'Seed tempo (ms)', 
       y = 'Final - Seed tempo (ms)') +
  #theme(legend.position = 'right') + # save dim with legend: 275ish X 325
  facet_grid(mode~train_type)
  

# AM - SR: TEST drift using avg of final 4 generations (12 through 15) -------
 
avg_finals <- tempo_with_seeds %>%
  filter(generation > 11) %>%
  group_by(exp_name,mode,train_type,cohort,seed,seed_int,chain_name) %>%
  summarize(n = n(),
            final_tempo = mean(mean_tempo),
            final_tempo_sd = sd(mean_tempo)) %>%
  mutate(final_tempo_se = final_tempo_sd/sqrt(n))

avg_firsts <- tempo_with_seeds %>%
  filter(generation < 4) %>%
  group_by(exp_name,mode,train_type,cohort,seed,seed_int,chain_name) %>%
  summarize(n = n(),
            first_tempo = mean(mean_tempo),
            first_tempo_sd = sd(mean_tempo)) %>%
  mutate(first_tempo_se = first_tempo_sd/sqrt(n))

avg_compare <- avg_firsts %>%
  left_join(avg_finals, by = c("exp_name", "mode", "train_type", "cohort", "seed", "seed_int", "chain_name")) %>%  
  ungroup() %>%
  mutate(seed = as.factor(seed),
         drift = final_tempo - first_tempo) %>%
  select(exp_name, mode, train_type, cohort, seed, seed_int, chain_name,
         first_tempo, first_tempo_sd, first_tempo_se,
         final_tempo, final_tempo_sd, final_tempo_se, drift)



avg_compare %>%
  ggplot(aes(x = seed_int, y = drift, color = seed)) +
  geom_point(size = 3, alpha = .5) + 
  #geom_boxplot() +
  geom_smooth(aes(group = exp_name), method = lm) +
  facet_grid(mode~train_type)


# ***AM - SR: auto-regression & bias - df and plots -------
# alt: read in ar1 from file: ar1 <- read.csv("compiled data/ar1_2016-11-1.csv") %>% select(-X)

ar1 <- tempo_with_seeds %>%
  group_by(chain_name) %>%
  mutate(generation = as.numeric(as.character(generation))) %>% # convert gen to char before num to preserve '0' (i.e. factor values, not just indices wrt factor levels)
  arrange(generation) %>%
  mutate(next_gen = generation + 1,
         next_mean_tempo = lead(mean_tempo, 1),
         gen_plus2 = generation + 2,
         plus2_mean_tempo = lead(mean_tempo, 2),
         delta_mean = next_mean_tempo - mean_tempo)

write.csv(ar1, "compiled data/ar1_2016-11-1.csv")
 
geom_point(aes(color = seed), size = 3, alpha = .75) +
  geom_line(aes(group=cohort), alpha = .25, lwd = 1, color = "#999999") +
  geom_hline(aes(yintercept = 0), color = "#999999") +
  geom_smooth(method = lm, se=F, lwd = 1.5, alpha = .75, color = 'black') + 

# *autoregression
ar1 %>%
  ggplot(aes(x = mean_tempo, y = next_mean_tempo)) +
  geom_point(aes(color = generation), size = 3, shape = 1) + 
  geom_smooth(aes(group = exp_name), method = lm, se = FALSE, lwd=1.5, alpha=.75, color='black') +
  scale_colour_gradientn(colours = rainbow(n = 4, start = .5, end = 1)) +
  geom_abline(aes(slope=1,intercept=0),color="#999999") +
  theme_bw() +
  labs(title = '(A) Autoregression',
       x = 'Stimulus tempo (ms)', #expression(x[i])
       y =  'Reproduced tempo (ms)') + #expression(x[i+1]))
  facet_grid(mode~train_type)
  

# *bias plot
ar1 %>%
  ggplot(aes(x = mean_tempo, y = delta_mean, color = generation)) +
  geom_point(aes(color = generation), size = 3, shape = 1) + 
  geom_smooth(aes(group = exp_name), method = lm, se = FALSE, lwd=1.5, alpha=.75, color='black') +
  scale_colour_gradientn(colours = rainbow(n = 4, start = .5, end = 1)) +
  geom_hline(aes(yintercept=0),color="#999999") +
  theme_bw() +
  labs(title = '(B) Bias',
       x = 'Stimulus tempo (ms)', #expression(x[i])
       y =  'Reproduced - Stimulus tempo (ms)') + #expression(x[i+1] - x[i]))
  facet_grid(mode~train_type)


# not used: markov assumption
ar1 %>%
  ggplot(aes(x = mean_tempo, y = plus2_mean_tempo, color = generation)) +
  geom_point() + geom_smooth(method = lm, se = FALSE) +
  facet_grid(mode~train_type)


# ***AM - SR: auto-regression - lm -------
# start with ar1 df from earlier

data <- ar1
conds <- unique(data$exp_name)

nrowOutput <- length(conds)
ncolOutput <- 11
output <- data.frame(matrix(ncol = ncolOutput, nrow = nrowOutput))
cnames <- c("exp_name", "slope", "slope_p", "y_int", "y_int_p",
            "F_val", "F_numdf", "F_dendf", "F_sig", "rsq", "x_int")

colnames(output) <- cnames


n = 1

for (j in 1:length(conds)) {
  
  lm_out <- lm(mean_tempo ~ next_mean_tempo, 
               data = subset(data,exp_name == conds[j]))
  
  output$exp_name[n] <- as.character(conds[j]) 
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


# save as table:
write.csv(output,"tables/lm_am_sr_ar1_10-26-2016.csv")

# check that i'm fitting what i think i am:
dataplot <- data %>%
  ggplot(aes(x = mean_tempo, y = next_mean_tempo)) + 
  geom_point(alpha = .5) + geom_smooth(method = lm, se = FALSE) +
  facet_wrap(~exp_name)

dataplot 

# compare to lm fits:
dataplot +
  geom_abline(data = output, aes(intercept = y_int, slope = slope), linetype = 2, size = 1.5) +
  facet_wrap(~exp_name)

# plot residuals or res normed by seed (for last condition fit, at least)
# not working?
res <- data.frame(as.numeric(resid(lm_out))) %>%
  rename(residuals = as.numeric.resid.lm_out..)

data %>%
  filter(exp_name == "v_sc") %>%
  bind_cols(res) %>%
  ggplot(aes(x = mean_tempo, y = res)) + geom_point()
#ggplot(aes(x = seed_tempo, y = res/seed_tempo)) + geom_point()


# clear holding variables
#rm(data,conds,nrowOutput,ncolOutput,output,cnames,lm_out)
rm(data,conds,nrowOutput,ncolOutput,cnames,lm_out)





# AM - SR: bias - lm -------
# start with ar1 df from earlier

data <- ar1
conds <- unique(data$exp_name)

nrowOutput <- length(conds)
ncolOutput <- 11
output <- data.frame(matrix(ncol = ncolOutput, nrow = nrowOutput))
cnames <- c("exp_name", "slope", "slope_p", "y_int", "y_int_p",
            "F_val", "F_numdf", "F_dendf", "F_sig", "rsq", "x_int")

colnames(output) <- cnames


n = 1

for (j in 1:length(conds)) {
  
  lm_out <- lm(delta_mean ~ mean_tempo, 
               data = subset(data,exp_name == conds[j]))
  
  output$exp_name[n] <- as.character(conds[j]) 
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


# save as table:
write.csv(output,"tables/lm_am_sr_bias_10-26-2016.csv")

# check that i'm fitting what i think i am:
dataplot <- data %>%
  ggplot(aes(x = mean_tempo, y = delta_mean)) + 
  geom_point(alpha = .5) + geom_smooth(method = lm, se = FALSE) +
  facet_wrap(~exp_name)

dataplot 

# compare to lm fits:
dataplot +
  geom_abline(data = output, aes(intercept = y_int, slope = slope), linetype = 2, size = 1.5) +
  facet_wrap(~exp_name)

# plot residuals or res normed by seed (for last condition fit, at least)
# not working?
res <- data.frame(as.numeric(resid(lm_out))) %>%
  rename(residuals = as.numeric.resid.lm_out..)

data %>%
  filter(exp_name == "v_sc") %>%
  bind_cols(res) %>%
  ggplot(aes(x = mean_tempo, y = res)) + geom_point()
#ggplot(aes(x = seed_tempo, y = res/seed_tempo)) + geom_point()

# alt:
ggplot(data = res, aes(x = residuals)) + geom_histogram()

# clear holding variables
#rm(data,conds,nrowOutput,ncolOutput,output,cnames,lm_out)
rm(data,conds,nrowOutput,ncolOutput,cnames,lm_out)




# AM - SR: dynamical systems plots -------
# use ar1 df from autoregression plots

# save dim with legend: 

# return plot -- deciding: arrows or line segments?
ar1 %>%
  filter(exp_name == "a_sc") %>%
  ggplot(aes(x = mean_tempo, y = next_mean_tempo, color = generation)) +
  geom_point(size = 3, shape = 1) + 
  # plot arrow from gen to gen:
#   geom_segment(aes(xend=c(tail(mean_tempo, n=-1), NA), 
#                    yend=c(tail(next_mean_tempo, n=-1), NA)), 
#                arrow = arrow(angle = 45, length = unit(0.075, "inches"), type = "closed")) +
#   
  # alt plot just line segments:
  geom_segment(aes(xend=c(tail(mean_tempo, n=-1), NA), 
                   yend=c(tail(next_mean_tempo, n=-1), NA))) + 
  
  scale_colour_gradientn(colours = rainbow(n = 4, start = .5, end = 1)) +
  theme_bw() +
  labs(title = '(A) Auditory-Observe Return Plot',
       x = expression(x[i]), 
       y = expression(x[i+1])) +
  #theme(legend.position = 'none')+
  facet_grid(cohort~seed, scales = "free")

# option 2: return plot with mean across cohorts
# still deciding: arrows or line segments?
ar1 %>%
  #filter(exp_name == "a_or") %>%
  group_by(exp_name, seed, seed_int, generation) %>%
  summarize(mean_tempo = mean(mean_tempo),
            next_mean_tempo = mean(next_mean_tempo)) %>%
  
  
  ggplot(aes(x = mean_tempo, y = next_mean_tempo, color = generation)) +
  geom_point(size = 2) + 
  # alt plot just line segments:
  geom_segment(aes(xend=c(tail(mean_tempo, n=-1), NA), 
                   yend=c(tail(next_mean_tempo, n=-1), NA))) + 
  
  scale_colour_gradientn(colours = rainbow(n = 4, start = .5, end = 1)) +
  theme_bw() +
  labs(title = '(A) Return Plot',
       x = expression(x[i]), 
       y = expression(x[i+1])) +
  #theme(legend.position = 'none')+
  facet_grid(exp_name~seed)
  
  
# delta plot with summary by cohort
ar1 %>%
  group_by(exp_name, seed, seed_int, generation) %>%
  summarize(mean_tempo = mean(mean_tempo),
            delta_mean = mean(delta_mean)) %>%
  
  
  ggplot(aes(x = mean_tempo, y = delta_mean, color = generation)) +
  geom_point(size = 2) + 
  # alt plot just line segments:
  geom_segment(aes(xend=c(tail(mean_tempo, n=-1), NA), 
                   yend=c(tail(delta_mean, n=-1), NA))) + 
  
  scale_colour_gradientn(colours = rainbow(n = 4, start = .5, end = 1)) +
  theme_bw() +
  labs(title = '(B) Delta Plot',
       x = expression(x[i]), 
       y = expression(x[i+1] - x[i])) +
  #theme(legend.position = 'none')+
  facet_grid(exp_name~seed)



# EP - SR: load e-prime (ep) serial reproduction (SR) taps & summary data & clean up------------------------------
      
    taps_sr <- read.csv(taps_input) %>%
        mutate(cohort = as.factor(cohort),
               seed = as.factor(seed),
               id = as.factor(id),
               sourceId = as.factor(sourceId),
               generation = as.factor(generation),
               block = as.factor(block)) %>%
        select(-X)
  
    taps_sr_summary <- read.csv(taps_summary_input) %>%
      mutate(cohort = as.factor(cohort),
             seed = as.factor(seed),
             generation = as.factor(generation)) %>%  
      select(-X)
    
# WORKING TAP SUMMARY: simulations from linear fits ------------------------------
# re-create (simulate) productions
# 1. by cohort: lm_drift_bycohort (40 rows) compare to tempo_bychain_table (40*7seeds = 140 rows)
# 2. by condition: lm_drift_bycond (4 rows) compare to tempo_bycond_table (4*7seeds = 28 rows)
# 3. spaghetti: lm_spaghetti_bychain (140 rows) compare to taps_sr_summary (140*15gens = 2100 rows)


# WORKING: simulation by chain -- plot looks wrong MISMATCHED Ys and LIN PARAMS!
# lm fit has 40 rows (for each expname*cohort)
# hm this isn't working right now...need to do more work - match values by expname and cohort to create
# new variables to input slope and y_int from bychain fit , then mutate to add y_pred column
simulation <- tempo_bychain_table %>%
  select(exp_name:chain_name) %>%
  mutate(cohort = as.integer(cohort)) %>%
  full_join(lm_drift_bycohort, by = c("exp_name", "cohort"))

# i want something like this, but for more seed_ints (the seed_ints in tempo_bychain_table)
simulation <- lm_drift_bycohort %>%
  mutate(seed_int = 600,
         y_pred = slope*seed_int + y_int)

ggplot(simulation, aes(x = seed_int)) +
  geom_point(aes(y = drift, color = seed), size = 3, shape = 19, alpha = .75) +
  geom_line(aes(y = drift), linetype = 'solid') +
  geom_point(aes(y = y_pred, color = seed), size = 3, shape = 5, alpha = .75) +
  geom_line(aes(y = y_pred), linetype = 'dashed') +
  facet_grid(mode~train_type~cohort) +
  theme_bw()

# more plot spec for later:
theme_bw() +  
  labs(x = 'seed tempo (ms)', 
       y = 'drift = final - seed (ms)') + 
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        axis.title = element_text(size = 26),
        axis.text = element_text(size = 16),
        axis.text.x = element_text(angle = 90),
        legend.position='right', #c(.9,.9)
        legend.title = element_text(size=22),
        legend.text = element_text(size = 16)) +
  scale_color_discrete(name="seed tempo (ms)") +
  scale_x_continuous(breaks=c(150,225,337,506,759,1139,1709)) +
  facet_grid(mode~train_type~cohort)

# WORKING: simulation by condition (28 rows)
# maybe easier to see
# plots still seems funky... MISMATCHED Ys and LIN PARAMS!
sim2 <- tempo_bycond_table %>%
  mutate(y_pred = slope*seed_int + y_int)

ggplot(sim2, aes(x = seed_int)) +
  geom_point(aes(y = drift, color = seed), size = 3, shape = 19, alpha = .75) +
  geom_line(aes(y = drift), linetype = 'solid') +
  geom_point(aes(y = y_pred, color = seed), size = 3, shape = 5, alpha = .75) +
  geom_line(aes(y = y_pred), linetype = 'dashed') +
  facet_grid(mode~train_type) +
  theme_bw()

# try spaghetti fits
# ...




# (TAPS &) TAP SUMMARY phase return plot & deltas ------------------------
# what's the difference between AR(1) and phase return??
# how to interpret return plots?

# TO DO (maybe) - TAPS: individual intervals (preserves serial order across generations - more important for meter than tempo)
# 2016-10-11: oy a bit complicated for exp 2 since source_int is tempo instead of individual taps
# possible to just plot all taps from generation n-1 against generation n taps? still color by seed?
# still need to make a separate column of source taps.
# source would really be the MEAN tempo from previous generation
# ideally, i could use the summary_am data to construct the source_int dataframe...
# leaving this for a later date for now...
ggplot(taps_sr, aes(x = source_tap_int, y = tap_int, group = chain_name, color = seed)) +
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
  #coord_cartesian(xlim = c(0,2500), ylim=c(0, 2500))   
  
  
  # TAP SUMMARY: return plot for tempo - data preparation 
  # need to start from taps_and_seeds_summary from spaghetti data prep
  tempo_return <- taps_and_seeds_summary %>%
  # just re-naming here instead of changing in mutate below...
  rename(mean_tempo = mean_tempo_2,
         sd_taps = mean_sd,
         cov = mean_cov,
         median_tempo = sr_tempo,
         mad_taps = mean_mad,
         comv = mean_comv) %>%
  group_by(chain_name) %>%
  mutate(generation = as.numeric(as.character(generation))) %>% # convert gen to char before num to preserve '0' (i.e. factor values, not just indices wrt factor levels)
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
         delta_comv = comv - source_comv) %>%
  filter(generation != 0) # removes seeds

# return plot for mean tempo by condition
ggplot(tempo_return, aes(x = source_mean_tempo, y = mean_tempo, group = chain_name, color = seed)) +
  geom_point(size = 2, alpha = .5) +
  geom_smooth(data = tempo_return, aes(x = source_mean_tempo, y = mean_tempo, group = seed), 
              alpha = .2, size = 1.25) +
  labs(title = 'return plot - mean tempo', 
       x = 'tempo n-1 (ms)', y = 'tempo n (ms)') + 
  theme_bw() +
  theme(plot.title = element_text(lineheight=.8, face="bold"),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        axis.title.x = element_text(size=20),
        axis.text.x = element_text(size = 16),
        axis.title.y  = element_text(size =20),
        axis.text.y = element_text(size = 16)) +
  coord_cartesian(xlim = c(0,3000), ylim=c(0,3000)) +
  facet_grid(train_type~mode) #+
#scale_x_log10() +
#scale_y_log10()

# just auditory-observe plot to compare to exp 1
tempo_return %>%
  filter(exp_name == "a_or") %>%
  ggplot(aes(x = source_mean_tempo, y = mean_tempo, group = chain_name, color = seed)) +
  geom_point(size = 2, alpha = .5) +
  geom_smooth(aes(x = source_mean_tempo, y = mean_tempo, group = seed), 
              alpha = .1, size = 1.25) +
  labs(title = 'auditory-observe: return plot - mean tempo', 
       x = 'tempo n-1 (ms)', y = 'tempo n (ms)') + 
  theme_bw() +
  theme(plot.title = element_text(lineheight=.8, face="bold"),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        axis.title.x = element_text(size=20),
        axis.text.x = element_text(size = 16),
        axis.title.y  = element_text(size =20),
        axis.text.y = element_text(size = 16))

# other plot for convergence? (when change from gen to gen is zero as function of generation)
# delta = tempo(i+1) - tempo(i), plotted as function of generation
ggplot(tempo_return, aes(x = generation, y = delta_mean, group = chain_name, color = seed)) +
  geom_point(size = 2, alpha = .5) +
  geom_line() +
  geom_smooth(aes(x = generation, y = delta_mean, group = seed),
              alpha = .2, size = 1.25) +
  facet_grid(train_type~mode)

# maybe convergence point = when change from gen to gen is zero as function of source tempo?
ggplot(tempo_return, aes(x = source_mean_tempo, y = delta_mean, group = chain_name, color = seed)) +
  geom_point(size = 2, alpha = .5) +
  geom_smooth(aes(x = source_mean_tempo, y = delta_mean, group = seed),
              method = lm, se=FALSE, size = 1.25) +
  facet_grid(train_type~mode)



# ***MT: load data from motor tempo (MT) productions & clean up------------------------------
    
    taps_mt <- read.csv(taps_mt_input) %>%
      mutate(cohort = as.factor(cohort),
             id = as.factor(id),
             sourceId = as.factor(sourceId),
             generation = as.factor(generation),
             block = as.factor(block)) %>%
      select(-X)
    

    taps_mt_summary <- read.csv(taps_mt_summary_input) %>%
      mutate(cohort = as.factor(cohort),
             generation = as.factor(generation)) %>%  
      mutate(mode = ifelse(mode=="a","auditory",
                           ifelse(mode=="v","visual",NA)),
             train_type = ifelse(train_type=="or","observe",
                                 ifelse(train_type=="sc","synchronize",NA))) %>%
      select(-X)
           

# *** MT: plots ------------------------

# save dim: 600 x 515 
# save dim (smaller): 350 x 300

  taps_mt_summary %>%
    filter(tap_type == "fast mt") %>%
    ggplot(aes(x = generation, y = tempo, group = cohort)) +
    geom_point(size = 1, alpha = .5, color = "#999999") + #color = "#999999"
    geom_line(alpha = .5, lwd = 1, color = "#999999") +
    geom_smooth(aes(group = exp_name), color = '#FF8080', lwd = 1.5, alpha = .75, se = F) +
    theme_bw() +
    labs(title = '(A) Fastest', x = 'Generation', y = 'Mean tempo (ms)') +
    facet_grid(exp_name~.)
  
  
  taps_mt_summary %>%
    filter(tap_type == "smt") %>%
    ggplot(aes(x = generation, y = tempo, group = cohort)) +
    geom_point(size = 1, alpha = .5, color = "#999999") + 
    geom_line(alpha = .5, lwd = 1, color = "#999999") +
    geom_smooth(aes(group = exp_name), color = '#409FFF', lwd = 1.5, alpha = .75, se = F) +
    theme_bw() +
    labs(title = '(B) Spontaneous', x = 'Generation', y = 'Mean tempo (ms)') +
    facet_grid(exp_name~.)
  
  
  taps_mt_summary %>%
    filter(tap_type == "slow mt") %>%
    ggplot(aes(x = generation, y = tempo, group = cohort)) +
    geom_point(size = 1, alpha = .5, color = "#999999") + 
    geom_line(alpha = .5, lwd = 1, color = "#999999") +
    geom_smooth(aes(group = exp_name), color = '#00BF00', lwd = 1.5, alpha = .75, se = F) +
    theme_bw() +
    labs(title = '(C) Slowest', x = 'Generation', y = 'Mean tempo (ms)') +
    facet_grid(exp_name~.)
  
  # **trying plotting all on one plot...
  # totally swamps the fast tempo, unless log scale?
  taps_mt_summary %>%
    mutate(tap_type = factor(taps_mt_summary$tap_type, levels = c("fast mt", "smt", "slow mt"))) %>% # handy trick for changing factor order for plots!
    ggplot(aes(x = generation, y = tempo, group = cohort)) +
    geom_point(size = 1, alpha = .5, color = "#999999") + 
    geom_line(alpha = .5, lwd = 1, color = "#999999") +
    geom_smooth(aes(group = exp_name, color = tap_type), lwd = 1.5, alpha = .75, se = F) +
    theme_bw() + #scale_y_log10() + 
    labs(title = '(A) Self-paced Motor Tempo', x = 'Generation', y = 'Mean tempo (ms)') +
    facet_grid(exp_name~tap_type)
  
  # mean(median_tempo) experimenting with free scales:
  taps_mt_summary %>%
    ggplot(aes(x = generation, y = tempo, group = cohort, color = tap_type)) +
    geom_point() + geom_line() +
    facet_grid(tap_type~exp_name, scales = "free")
  
  # mean(median_tempo) experimenting with boxplots
  taps_mt_summary %>%
    ggplot(aes(x = tap_type, y = tempo, color = tap_type)) +
    geom_boxplot() +
    theme_bw() + facet_grid(exp_name~.)
  
  
  # ** variability
  taps_mt_summary %>%
    #filter(tap_type == "slow mt") %>%
    mutate(tap_type = factor(taps_mt_summary$tap_type, levels = c("fast mt", "smt", "slow mt"))) %>%
    ggplot(aes(x = generation, y = cv, group = cohort)) +
    geom_point(size = 1, alpha = .5, color = "#999999") + 
    geom_line(alpha = .5, lwd = 1, color = "#999999") +
    geom_smooth(aes(group = exp_name, color = tap_type), lwd = 1.5, alpha = .75, se = F) +
    theme_bw() +
    labs(title = '(B) Self-paced Motor Variability', x = 'Generation', y = 'CV') +
    facet_grid(exp_name~tap_type)
  
  taps_mt_summary %>%
    ggplot(aes(x = tap_type, y = cv, color = tap_type)) +
    geom_boxplot() +
    theme_bw() + facet_grid(exp_name~.)
  


# MT: long to wide & normalized produced range (NPR) -------------------------------

# first create wide format by creating individual dfs by tap_type, renaming variabils, then recombining
  
  smt <- taps_mt_summary %>%
    filter(tap_type == "smt") %>%
    rename(smt_nints = nints,
           smt_nblocks = nblocks,
           smt_mean = tempo,
           smt_cv = cv) %>%
    select(-tap_type)

  fast_mt <- taps_mt_summary %>%
    filter(tap_type == "fast mt") %>%
    rename(fast_nints = nints,
           fast_nblocks = nblocks,
           fast_mean = tempo,
           fast_cv = cv) %>%
    select(-tap_type)

  slow_mt<- taps_mt_summary %>%
    filter(tap_type == "slow mt") %>%
    rename(slow_nints = nints,
           slow_nblocks = nblocks,
           slow_mean = tempo,
           slow_cv = cv) %>%
    select(-tap_type)

  # join mt's and calculate produced range (pr) and normalized pr (npr)
   mt_wide <- smt %>%
    full_join(fast_mt, by = c("exp_name","mode","train_type","cohort","id","generation")) %>%
    full_join(slow_mt, by = c("exp_name","mode","train_type","cohort","id","generation")) %>%
    mutate(range = slow_mean - fast_mean,
           norm_range = range/smt_mean)
           
  write.csv(mt_wide, "tables/mt_wide_2016-10-31.csv")
           

  # plot check
  ggplot(mt_wide, aes(x = generation, y = range, group = cohort, color = exp_name)) +
    geom_point(size = 3, alpha = .75) + 
    geom_line(alpha = .5, lwd = 2) +
    theme_bw() +
    facet_grid(exp_name~cohort)
  
  # useful plot - are cohorts the same within a condition?
  ggplot(mt_wide, aes(x = generation, y = range, group = cohort, color = cohort)) +
    geom_point(size = 3, alpha = .75) + 
    geom_line(alpha = .5, lwd = 2) +
    theme_bw() +
    facet_grid(train_type~mode)
   
  # checking out normed produced range
  ggplot(mt_wide, aes(x = generation, y = norm_range, group = cohort, color = cohort)) +
    geom_point() +
    geom_line() +
    theme_bw() +
    facet_grid(train_type~mode)

  

# MT: tables -------------------------------
      
    # (1) summarize excluded (start from taps_mt) 
    excluded <- taps_mt %>%
      mutate(count = 1) %>%
      group_by(exp_name, mode, train_type, tap_type) %>%
      #group_by(tap_type) %>% # to summarize exclude by tap type, comment out above and uncomment this
      summarize(incl_all = sum(count),
                excluded_first = incl_all - sum(tap_incl_rmfirst, na.rm=T),
                excluded_100ms = incl_all - sum(tap_incl_rm100ms, na.rm=T),
                excluded_upper = incl_all - sum(tap_incl_rmupper, na.rm=T),
                excluded_nints = incl_all - sum(tap_incl_rmnints, na.rm=T),
                excluded_all = incl_all - sum(tap_include)) %>%
      mutate(prop_rmfirst = excluded_first/incl_all, 
             prop_rm100ms = excluded_100ms/incl_all,
             prop_rmupper = excluded_upper/incl_all,
             prop_rmnints = excluded_nints/incl_all,
             prop_rmall = excluded_all/incl_all)
    
    write.csv(excluded, "tables/mt_excluded_bycond&taptype_2016-10-31.csv")
    
    
    # (2) summarize by COHORT, add consistency
    smt_summary <- smt %>%
      group_by(exp_name, mode, train_type, cohort) %>%
      summarize(smt_n = n(),
                smt_avg = mean(smt_mean, na.rm=T),
                smt_sd = sd(smt_mean, na.rm=T),
                smt_avg_cv = mean(smt_cv, na.rm=T),
                smt_avg_cv_sd = sd(smt_cv, na.rm=T)) %>%
      mutate(smt_se = smt_sd/sqrt(smt_n),
             smt_avg_cv_se = smt_avg_cv_sd/sqrt(smt_n),
             smt_consistency = smt_sd/smt_avg)
    
    fast_summary <- fast_mt %>%
      group_by(exp_name, mode, train_type, cohort) %>%
      summarize(fast_n = n(),
                fast_avg = mean(fast_mean, na.rm=T),
                fast_sd = sd(fast_mean, na.rm=T),
                fast_avg_cv = mean(fast_cv, na.rm=T),
                fast_avg_cv_sd = sd(fast_cv, na.rm=T)) %>%
      mutate(fast_se = fast_sd/sqrt(fast_n),
             fast_avg_cv_se = fast_avg_cv_sd/sqrt(fast_n),
             fast_consistency = fast_sd/fast_avg)
    
    slow_summary <- slow_mt %>%
      group_by(exp_name, mode, train_type, cohort) %>%
      summarize(slow_n = n(),
                slow_avg = mean(slow_mean, na.rm=T),
                slow_sd = sd(slow_mean, na.rm=T),
                slow_avg_cv = mean(slow_cv, na.rm=T),
                slow_avg_cv_sd = sd(slow_cv, na.rm=T)) %>%
      mutate(slow_se = slow_sd/sqrt(slow_n),
             slow_avg_cv_se = slow_avg_cv_sd/sqrt(slow_n),
             slow_consistency = slow_sd/slow_avg)
    
    npr_summary <- mt_wide %>%
      group_by(exp_name, mode, train_type, cohort) %>%
      summarize(npr_n = n(),
                range_avg = mean(range, na.rm=T),
                range_sd = sd(range, na.rm=T),
                npr_avg = mean(norm_range, na.rm=T),
                npr_sd = sd(norm_range, na.rm=T)) %>%
      mutate(range_se = range_sd/sqrt(npr_n),
             npr_se = npr_sd/sqrt(npr_n),
             npr_consistency = npr_sd/npr_avg)
    
    mt_summary_bycohort <- fast_summary %>%
      full_join(smt_summary, by = c("exp_name", "mode", "train_type", "cohort")) %>%
      full_join(slow_summary, by = c("exp_name", "mode", "train_type", "cohort")) %>%
      full_join(npr_summary, by = c("exp_name", "mode", "train_type", "cohort")) %>%
      ungroup() %>%
      mutate(mode = as.factor(mode),
             train_type = as.factor(train_type),
             cohort = as.integer(cohort))
    
    write.csv(mt_summary_bycohort, "tables/mt_summary_bycohort_wide_2016-11-10.csv")
    

  # (3) summarize by condition
  # feels like it makes sense to do separate tap_type dfs and then combine...(to get n's)
    smt_summary <- smt %>%
      group_by(exp_name, mode, train_type) %>%
      summarize(smt_n = n(),
                smt_avg = mean(smt_mean, na.rm=T),
                smt_sd = sd(smt_mean, na.rm=T),
                smt_avg_cv = mean(smt_cv, na.rm=T),
                smt_avg_cv_sd = sd(smt_cv, na.rm=T)) %>%
      mutate(smt_se = smt_sd/sqrt(smt_n),
             smt_avg_cv_se = smt_avg_cv_sd/sqrt(smt_n))
                
    fast_summary <- fast_mt %>%
      group_by(exp_name, mode, train_type) %>%
      summarize(fast_n = n(),
                fast_avg = mean(fast_mean, na.rm=T),
                fast_sd = sd(fast_mean, na.rm=T),
                fast_avg_cv = mean(fast_cv, na.rm=T),
                fast_avg_cv_sd = sd(fast_cv, na.rm=T)) %>%
      mutate(fast_se = fast_sd/sqrt(fast_n),
             fast_avg_cv_se = fast_avg_cv_sd/sqrt(fast_n))
    
    slow_summary <- slow_mt %>%
      group_by(exp_name, mode, train_type) %>%
      summarize(slow_n = n(),
                slow_avg = mean(slow_mean, na.rm=T),
                slow_sd = sd(slow_mean, na.rm=T),
                slow_avg_cv = mean(slow_cv, na.rm=T),
                slow_avg_cv_sd = sd(slow_cv, na.rm=T)) %>%
      mutate(slow_se = slow_sd/sqrt(slow_n),
             slow_avg_cv_se = slow_avg_cv_sd/sqrt(slow_n))
    
    npr_summary <- mt_wide %>%
      group_by(exp_name, mode, train_type) %>%
      summarize(npr_n = n(),
                range_avg = mean(range, na.rm=T),
                range_sd = sd(range, na.rm=T),
                npr_avg = mean(norm_range, na.rm=T),
                npr_sd = sd(norm_range, na.rm=T)) %>%
      mutate(range_se = range_sd/sqrt(npr_n),
             npr_se = npr_sd/sqrt(npr_n))
    
    mt_summary_bycondition <- fast_summary %>%
      full_join(smt_summary, by = c("exp_name","mode","train_type")) %>%
      full_join(slow_summary, by = c("exp_name","mode","train_type")) %>%
      full_join(npr_summary, by = c("exp_name","mode","train_type"))
    
    write.csv(mt_summary_bycondition, "tables/mt_summary_bycondition_2016-10-31.csv")

  # (4) summarize for whole experiment
    # to do...if needed...could also just type as needed...(should i write  function (mysummary) that reports mean, sd, se?)
    
    # for se's: n = 75 (except v_or slow mt, n = 74)
    report <- c(mean(mt_wide$fast_mean), sd(mt_wide$fast_mean), sd(mt_wide$fast_mean)/sqrt(75))

  # (5) summarize CONSISTENCY (=sd/mean) by COHORT (to parallel by participant in Exp 3)
    consistency <- taps_mt_summary %>%
      group_by(exp_name, mode, train_type, cohort, tap_type) %>%
      summarize(mean_tempo = mean(tempo, na.rm=T),
                sd_tempo = sd(tempo, na.rm=T)) %>%
      mutate(consistency = sd_tempo/mean_tempo)
    
    write.csv(consistency,"tables/consistency_bycohort_2016-11-09.csv")


# MT: summary plot by condition -------------------------------
# (not updated 10-31-16)

  # easier to plot from long than wide
  
  # to figure out -- how to add error bars
  # ack i'd need to write the SE function...
  # error <- summarySE(mt_long, measurevar = "mean_tempo_2", groupvars = c("exp_name", "cohort", "tap_type"))
  
  # bar chart of mean tempo by tap type, condition, cohort...
  ggplot(mt_long_table, aes(x = tap_type, y = mean_tempo_2)) +
    geom_bar(aes(fill = cohort), position = "dodge", stat="identity") +
    #geom_errorbar(aes(ymin = , ymax = )) +
    labs(title = 'mean MT by condition',
         x = 'tap type', y = 'mean tempo (ms)') +
    theme_bw() +
    theme(plot.title = element_text(lineheight=.8, face="bold"),
          panel.grid.minor=element_blank(),
          panel.grid.major=element_blank(),
          axis.title.x = element_text(size=20),
          axis.text.x = element_text(size = 16),
          axis.title.y  = element_text(size =20),
          axis.text.x = element_text(size = 16)) +
    facet_grid(train_type~mode)


##### 2016-11-01 MT-SR ratio : df composition -----------------
# use mt_wide as MT df (mt_wide <- read.csv("tables/mt_wide_2016-10-31.csv") %>% select(-X))
# use ar1 as SR df (ar1 <- read.csv("compiled data/ar1_2016-11-1.csv") %>% select(-X))
# creates indivs df (2100 rows for each participant*chain) - ar1 & mt data combined, pluse relative tempo
# or 2100 rows plus 140 to add a seed for each chain -- DEPENDS ON FILTER in indivs_ar1 

# kind of just renaming things, plus making a participant id
indivs_ar1 <- ar1 %>%
  mutate(stimulus_generation = generation,
         generation = next_gen,
         id = paste(exp_name, cohort, generation, sep = "_"), # unique id for each participant
         stimulus_tempo = mean_tempo,
         reproduced_tempo = next_mean_tempo,
         bias = reproduced_tempo - stimulus_tempo) %>%
  #filter(generation !=16) %>%
  select(exp_name,mode,train_type,cohort,id,generation, # shared with mt
         seed,seed_int,chain_name,stimulus_generation,
         stimulus_tempo, reproduced_tempo, bias)

# making a matching participant id for mt df
indivs_mt <- mt_wide %>%
  mutate(id = paste(exp_name, cohort, generation, sep = "_")) # unique id for each participant

# combining
indivs <- indivs_ar1 %>%
  full_join(indivs_mt, by = c("exp_name", "mode", "train_type", "cohort", "id", "generation")) %>%
  mutate(relative_tempo = stimulus_tempo/smt_mean)

# summarizing over seeds makes clearer pattern?
ind_summary <- indivs %>%
  group_by(exp_name, mode, train_type, cohort, id, generation) %>%
  summarize(mean_bias = mean(bias, na.rm = T),
            mean_smt = mean(smt_mean, na.rm = T)) 



##### to replace...WORKING HERE: MT - AM-SR tempo - data preparation for correlation -------
# start with ep_mt_summary_acrossblocks or mt_wide --- or just smt
# and tempo_am or ar1

# start above or read in:: mt_wide <- read.csv("tables/mt_wide_2016-10-31.csv") %>% select(-X)
  
  # quick exploration of ar1 how to consolidate across seeds?
  ar1 %>%
    #filter(exp_name == "v_or") %>%
    ggplot(aes(x = mean_tempo, y = delta_mean, group = cohort, color = seed)) +
    geom_point() + geom_line() + geom_smooth(method = lm, se = FALSE) +
    theme_bw() + 
    facet_grid(exp_name~cohort)
    
  smt %>%
    ggplot(aes(x = cohort, y = smt_mean)) +
    geom_boxplot() + facet_grid(exp_name~.)
  
  # kind of just renaming things, plus making a participant id
  indivs_ar1 <- ar1 %>%
    mutate(stimulus_id = generation,
           generation = next_gen,
           id = paste(exp_name, cohort, generation, sep = "_"), # unique id for each participant
           stimulus_tempo = mean_tempo,
           reproduced_tempo = next_mean_tempo,
           bias = reproduced_tempo - stimulus_tempo) %>%
    filter(generation !=16) %>%
    select(exp_name,mode,train_type,cohort,seed,seed_int,chain_name,
           id,stimulus_id,generation,
           stimulus_tempo, reproduced_tempo, bias)
           
  # add id to smt for fool-proofier combinging
  indivs_smt <- smt %>%
    mutate(id = paste(exp_name, cohort, generation, sep = "_")) # unique id for each participant

  # combining
  indivs <- indivs_ar1 %>%
    full_join(indivs_smt, by = c("exp_name", "mode", "train_type", "id")) %>%
    mutate(cohort = cohort.x,
           generation = generation.x) %>%
    select(exp_name,mode,train_type,cohort,seed,seed_int,chain_name,
           id, stimulus_id, generation,
           stimulus_tempo, reproduced_tempo, bias,
           smt_mean, smt_cov)
    
  # plotting -- for all seeds (so multiple points per smt)
  indivs %>%
    ggplot(aes(x = smt_mean, y = bias)) +
    geom_point(aes(color = seed)) + geom_smooth(method = lm, se=F) +
    geom_vline(xintercept = mean(indivs$smt_mean)) + # how to do a different vline for each panel?
    theme_bw() +
    facet_wrap(~exp_name)
  
  
  # plotting - summarizing over seeds makes clearer pattern?
  ind_summary <- indivs %>%
    group_by(exp_name, id) %>%
    summarize(mean_bias = mean(bias, na.rm = T),
              mean_smt = mean(smt_mean, na.rm = T)) 
  
  ind_summary %>%
    ggplot(aes(x = mean_smt, y = mean_bias)) +
    geom_point(aes(color = exp_name)) + geom_smooth(method = lm, se=F) +
    theme_bw()
   


##### PAUSING WORK HERE: bias probability function? -------
# start with indivs df from mt-sr correlation

indivs %>%
  ggplot(aes(x = stimulus_tempo, y = bias, color = seed)) +
  geom_point() + geom_smooth(aes(group = exp_name), method = lm, se = F) +
  facet_wrap(~exp_name)

indivs %>%
  ggplot(aes(x=bias, group = seed, color = seed)) +
  geom_histogram(binwidth = 50) +
  geom_freqpoly(binwidth = 50, size = 2) + # aes(y = ..ncount..), 
  facet_grid(exp_name~seed)

y <- density(indivs$bias, bw = 100) # but i don't know what this is doing. if i do not specify bandwidth, R chooses ~7

bias_total <- indivs %>%
  group_by(exp_name, seed) %>%
  summarize(total = n())

# bin bias -- use geom histogram
p <- ggplot(indivs, aes(bias)) + geom_histogram(binwidth=50) + facet_grid(exp_name~seed)
pg <- ggplot_build(p)
head(pg$data[[1]]) # number corresponds to number of geoms
tmp <- pg$data[[1]]




##### individual bias: lm fit -------
# for each participant, bias ~ stimulus_tempo
  
  data <- indivs
  conds <- unique(data$id)
  
  nrowOutput <- length(conds)
  ncolOutput <- 11
  output <- data.frame(matrix(ncol = ncolOutput, nrow = nrowOutput))
  cnames <- c("id", "slope", "slope_p", "y_int", "y_int_p",
              "F_val", "F_numdf", "F_dendf", "F_sig", "rsq", "x_int")
  
  colnames(output) <- cnames
  
  
  n = 1
  
  for (j in 1:length(conds)) {
    
    lm_out <- lm(bias ~ stimulus_tempo, 
                 data = subset(data,id == conds[j]))
    
    output$id[n] <- as.character(conds[j]) 
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
  
  
  # save as table:
  write.csv(output,"tables/lm_indiv_bias_10-29-2016.csv")
  
  # check that i'm fitting what i think i am (for one participant)
  dataplot <- data %>%
    filter(id == "v_sc_5_15") %>%
    ggplot(aes(x = stimulus_tempo, y = bias)) + 
    geom_point(alpha = .5) + geom_smooth(method = lm, se = FALSE) +
    # and, optionally, that bias is what i think it is:
    geom_point(aes(x = stimulus_tempo, y = reproduced_tempo - stimulus_tempo), color = "#FF0066") +
    coord_cartesian(xlim = c(0,1500))
    #facet_wrap(~exp_name)
  
  dataplot
  
  # compare to lm fits:dataplot 
  dataplot +
    geom_abline(data = output[output$id == "v_sc_5_15",], aes(intercept = y_int, slope = slope), linetype = 2, size = 1.5) +
    facet_wrap(~exp_name)
  
  # checking...
  output[output$id == "v_sc_5_15",]$x_int
  
  # plot residuals or res normed by seed (for last condition fit, at least)
  res <- data.frame(as.numeric(resid(lm_out))) %>%
    rename(residuals = as.numeric.resid.lm_out..)
  
  data %>%
    filter(id == "v_sc_5_15") %>%
    bind_cols(res) %>%
    ggplot(aes(x = stimulus_tempo, y = res)) + geom_point()
  #ggplot(aes(x = seed_tempo, y = res/seed_tempo)) + geom_point()
  

# clear holding variables
#rm(data,conds,nrowOutput,ncolOutput,output,cnames,lm_out)
rm(data,conds,nrowOutput,ncolOutput,cnames,lm_out)

##### playing around with smt vs bias, recreating somet dynamics plots -----

# join with smt
inds <- indivs_smt %>%
  left_join(output, by = "id")


# plot x-intercept of indiv bias (i.e., estimated PT) and smt
inds %>%
  filter(train_type == "or",
         x_int>0) %>%
  ggplot(aes(x = smt_mean, y = x_int)) +
  geom_point(size = 2) +
  coord_cartesian(xlim=c(0,1500), ylim=c(0,1500)) +
  geom_smooth(aes(group=exp_name), method = lm, se=F) +
  facet_grid(mode~.)

# if i zoom in, doesn't seem like x-int is centered on 0 anymore
inds %>%
  ggplot(aes(x=x_int)) +
  geom_histogram(binwidth=50) + coord_cartesian(xlim=c(0,1000))

indivs %>%
  ggplot(aes(x = smt_mean, y = bias)) + geom_point(size = 2) +
  geom_smooth(aes(group=exp_name), method = lm, se=F) +
  facet_grid(exp_name~.)

scatterplot3d(indivs$smt_mean,indivs$bias,indivs$seed_int)

plot3d(indivs$smt_mean,indivs$bias,indivs$stimulus_tempo)

with(indivs, plot3d(stimulus_tempo, bias, smt_mean, 
                  type="p", col=as.numeric(seed)))
subid <- currentSubscene3d()

ggplot(indivs, aes(x=smt_mean, y = stimulus_tempo)) + geom_point()

# timeseries plot
indivs %>%
  ggplot(aes(x=generation, y=reproduced_tempo, group=chain_name, color=seed)) +
  geom_point() + geom_line() +
  facet_wrap(~exp_name)
  
# return plot
indivs %>%
  ggplot(aes(x=stimulus_tempo, y=reproduced_tempo, group=chain_name, color=generation)) +
  geom_point() + geom_line() +
  geom_point(aes(x=stimulus_tempo, y=smt_mean), color='red') +
  facet_wrap(~exp_name)

# dx/dt
indivs %>%
  ggplot(aes(x=generation, y=bias, group=chain_name, color=seed)) +
  geom_point() + geom_line() +
  facet_wrap(~exp_name)

# dx/dx
indivs %>%
  ggplot(aes(x=stimulus_tempo, y=bias, group=chain_name, color=generation)) +
  geom_point() + geom_line() +
  facet_wrap(~exp_name)

##### *tests: ratio stimulus:smt "relative_tempo"-----
# this makes some sense

# trying ratio of stim tempo to smt
indivs %>%
  filter(id == 'a_or_1_15') %>%
  mutate(ratio = stimulus_tempo/smt_mean) %>%
  ggplot(aes(x=ratio, y=bias)) + 
  geom_point(aes(color=seed)) + geom_smooth(method=lm, se=F) + 
  geom_vline(xintercept=1) +
  facet_wrap(~exp_name)

# timeseries plot - change in relative tempo over generations
indivs %>%
  mutate(relative_tempo = stimulus_tempo/smt_mean) %>%
  ggplot(aes(x=generation, y=ratio)) + 
  geom_point(color = "grey", alpha=.5) + 
  geom_line(aes(group=chain_name),color = "grey", alpha=.5)+
  geom_smooth(aes(color=seed), method=lm, se=F) + 
  geom_hline(yintercept=1) +
  labs(title = "relative tempo shift over generations",
       xlab = 'Generation',
       ylab = 'Relative tempo') +
  theme_bw() +
  facet_wrap(~exp_name)

# return plot - 
indivs %>%
  mutate(relative_tempo = stimulus_tempo/smt_mean,
         relative_reproduction = reproduced_tempo/smt_mean) %>%
  ggplot(aes(x=relative_tempo, y=reproduced_tempo)) + 
  geom_point(color = "grey", alpha=.5) + 
  geom_line(aes(group=chain_name),color = "grey", alpha=.5)+
  geom_smooth(aes(color=seed), method=lm, se=F) + 
  geom_vline(xintercept=1) +
  theme_bw() +
  facet_wrap(~exp_name)

# bias against relative tempo
indivs %>%
  mutate(ratio = stimulus_tempo/smt_mean) %>%
  ggplot(aes(x=ratio, y=bias)) + 
  geom_point(color = "grey", alpha=.5) + 
  geom_line(aes(group=chain_name),color = "grey", alpha=.5)+
  geom_smooth(method=lm, se=F) + 
  geom_hline(yintercept=0) + geom_vline(xintercept=1) +
  coord_cartesian(xlim=c(0,2), ylim=c(-250,250))+
  labs(title = "bias as a function of relative tempo",
       x = 'Relative tempo',
       y = 'Bias (ms)') +
  theme_bw() +
  facet_wrap(~exp_name)



##### individual bias against ratio: lm fit -------
# for each participant, bias ~ stimulus_tempo:smt
# stupid. doesn't make any sense. x_int should never be negative?

data <- indivs
conds <- unique(data$id)

nrowOutput <- length(conds)
ncolOutput <- 11
output <- data.frame(matrix(ncol = ncolOutput, nrow = nrowOutput))
cnames <- c("id", "slope", "slope_p", "y_int", "y_int_p",
            "F_val", "F_numdf", "F_dendf", "F_sig", "rsq", "x_int")

colnames(output) <- cnames


n = 1

for (j in 1:length(conds)) {
  
  lm_out <- lm(bias ~ (stimulus_tempo/smt_mean), 
               data = subset(data,id == conds[j]))
  
  output$id[n] <- as.character(conds[j]) 
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


# save as table:
write.csv(output,"tables/lm_indiv_bias_by_relativetempo_10-29-2016.csv")



# MT, SR, and SURVEY info -----
survey <- read.csv(survey_input) # currently: basic demo + music, dance, sleep, a few others.



# REVISIONS: stat tests to compare pearson r correlation -----
#   the table currently read in is the output from the lm drift~seed. 
#     so, can obtain data by reading in (as script is currently, 
#     or by running through the first steps of this script and adding r and n)
# !for future: could improve by including an interaction term in regression model (but i need to do more reading to make sure I do this properly)

# ! can i use a similar method to compare SLOPES???



  # load data for drift by seed correlation:
    corr <- read.csv("tables/lm_am_sr_bycond_driftxseed_2016-10-25.csv") %>%
      mutate(r = sqrt(rsq),
             n = 35) %>% # number of points in the correlation. 35 for all conds in exp 2.
      select(exp_name, rsq, r, n, slope, y_int, x_int)
  
  # source functions for computing CIs (see function script for methods details)
    # CI that does not include 0 indicates a reliable difference
    source("compare_correlations_functions.R")
  
    # decide confidence level. default of functions is 95%
    # didn't get this to work...maybe need to change at function level
    # level = .99
    # function arguments for r.ind.ci: r1, r2, n1, n2=n1, conf.level = 0.95


  # MODALITY EFFECTS:
    # compare OBSERVE -- aud vs vis
      obs_av <- r.ind.ci(corr$r[corr$exp_name=="a_or"],corr$r[corr$exp_name=="v_or"],
                         corr$n[corr$exp_name=="a_or"],corr$n[corr$exp_name=="v_or"])
    
    # compare SYNC -- aud vs vis
      sync_av <- r.ind.ci(corr$r[corr$exp_name=="a_sc"],corr$r[corr$exp_name=="v_sc"],
                          corr$n[corr$exp_name=="a_sc"],corr$n[corr$exp_name=="v_sc"])
    
  
  # INTERACTION EFFECTS:
    # compare AUD -- obs vs. sync
      aud_os <- r.ind.ci(corr$r[corr$exp_name=="a_or"],corr$r[corr$exp_name=="a_sc"],
                         corr$n[corr$exp_name=="a_or"],corr$n[corr$exp_name=="a_sc"])
      
    # compare VIS -- obs vs. sync
      vis_os <- r.ind.ci(corr$r[corr$exp_name=="v_or"],corr$r[corr$exp_name=="v_sc"],
                         corr$n[corr$exp_name=="v_or"],corr$n[corr$exp_name=="v_sc"])
    

# REVISIONS 2015-12-06 AM - SR: linear fits - drift x seed -------
  # add SE for slopes
  # start with compare df from earlier
  
  data <- compare
  conds <- unique(data$exp_name)
  
  nrowOutput <- length(conds)
  ncolOutput <- 12
  output <- data.frame(matrix(ncol = ncolOutput, nrow = nrowOutput))
  cnames <- c("exp_name", "slope", "slope_se", "slope_p", "y_int", "y_int_p",
              "F_val", "F_numdf", "F_dendf", "F_sig", "rsq", "x_int")
  
  colnames(output) <- cnames
  
  
  n = 1
  
  for (j in 1:length(conds)) {
    
    lm_out <- lm(drift ~ seed_int, 
                 data = subset(data,exp_name == conds[j]))
    
    output$exp_name[n] <- as.character(conds[j]) 
    output$slope[n] <- coefficients(lm_out)[2]
    output$slope_se[n] <- summary(lm_out)$coefficients[2,2]
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
  
  
  # save as table:
  write.csv(output,"tables/lm_am_sr_bycond_driftxseed_plusSE_2016-12-06.csv")


# REVISIONS: stat tests to compare SLOPES -----
# option 1 -- pairwise t-tests
# kind of stupid way of contructing df but whatever

# http://core.ecu.edu/psyc/wuenschk/docs30/CompareCorrCoeff.pdf
# Kleinbaum, D. G., Kupper, L. L., Muller, K. E., & Nizam, A. (1978). Applied regression analysis and other multivariate techniques. North Scituate, MA: Dux-bury Press. KleinbaumApplied Regression Analysis and Other Multivariate Techniques1978.
# also stackexchange: http://stats.stackexchange.com/questions/55501/test-a-significant-difference-between-two-slope-values
# citing either Cohen Cohen Aiken & West or Paternoster, R., Brame, R., Mazerolle, P., & Piquero, A. R. (1998). Using the Correct Statistical Test for the Equality of Regression Coefficients. Criminology, 36(4), 859-866.
# verified result against http://www.danielsoper.com/statcalc/calculator.aspx?id=103

  # for data, need new lm output with SEs for slope
    output <- read.csv("tables/lm_am_sr_bycond_driftxseed_plusSE_2016-12-06.csv") %>% select(-X)
  
  # holding df:
  nrow_pw <- 4 
  ncol_pw <- 8 
  
  pairwise_tests <- data.frame(matrix(ncol=ncol_pw, nrow=nrow_pw))
  cnames <- c("conds_compared", "t_val", "p_val", "df", "slope_1", "slope_2", "se_1", "se_2")
  colnames(pairwise_tests) <- cnames
  
  pairwise_tests[,1] <- c("aud_obs-vs-sync", "vis_obs-vs-sync", "obs_a-vs-v", "syn_a-vs-v")

  
  # t_val = (slope 1 - slope 2 / SE of difference)
  # SE of difference = sqrt(SE1^2 + SE2^2)
  

## the tests (and add to holding df)

  # within auditory, is sync vs obs effect different? 
    slope_1 <- output$slope[output$exp_name == "a_or"]
    se_1 <- output$slope_se[output$exp_name == "a_or"]
    
    slope_2 <- output$slope[output$exp_name == "a_sc"]
    se_2 <- output$slope_se[output$exp_name == "a_sc"]
        
    df = 66 # N1-2 + N2-2 -- each N=35
    t_val <- (slope_1 - slope_2) / sqrt(se_1^2 + se_2^2)
    p_val = 2*pt(abs(t_val), df, lower=FALSE) # 2-tailed t-test. t(33) = 4.811, p = .000032
    # verified result against http://www.danielsoper.com/statcalc/calculator.aspx?id=103 -- pvalues slightly different...but significance pattern pretty much the same

  pairwise_tests$t_val[pairwise_tests$conds_compared == "aud_obs-vs-sync"] = t_val
  pairwise_tests$p_val[pairwise_tests$conds_compared == "aud_obs-vs-sync"] = p_val
  pairwise_tests$df[pairwise_tests$conds_compared == "aud_obs-vs-sync"] = df
  pairwise_tests$slope_1[pairwise_tests$conds_compared == "aud_obs-vs-sync"] = slope_1
  pairwise_tests$se_1[pairwise_tests$conds_compared == "aud_obs-vs-sync"] = se_1
  pairwise_tests$slope_2[pairwise_tests$conds_compared == "aud_obs-vs-sync"] = slope_2
  pairwise_tests$se_2[pairwise_tests$conds_compared == "aud_obs-vs-sync"] = se_2
  


  # within visual, is sync vs obs effect different? 
    slope_1 <- output$slope[output$exp_name == "v_or"]
    se_1 <- output$slope_se[output$exp_name == "v_or"]
    
    slope_2 <- output$slope[output$exp_name == "v_sc"]
    se_2 <- output$slope_se[output$exp_name == "v_sc"]
        
    df = 66 # N1-2 + N2-2 -- each N=35
    t_val <- (slope_1 - slope_2) / sqrt(se_1^2 + se_2^2)
    p_val = 2*pt(abs(t_val), df, lower=FALSE) # 2-tailed t-test. t(33) = 4.04, p = .000297
    # verified result against http://www.danielsoper.com/statcalc/calculator.aspx?id=103
  
  pairwise_tests$t_val[pairwise_tests$conds_compared == "vis_obs-vs-sync"] = t_val
  pairwise_tests$p_val[pairwise_tests$conds_compared == "vis_obs-vs-sync"] = p_val
  pairwise_tests$df[pairwise_tests$conds_compared == "vis_obs-vs-sync"] = df
  pairwise_tests$slope_1[pairwise_tests$conds_compared == "vis_obs-vs-sync"] = slope_1
  pairwise_tests$se_1[pairwise_tests$conds_compared == "vis_obs-vs-sync"] = se_1
  pairwise_tests$slope_2[pairwise_tests$conds_compared == "vis_obs-vs-sync"] = slope_2
  pairwise_tests$se_2[pairwise_tests$conds_compared == "vis_obs-vs-sync"] = se_2


  
  # within obs, is aud vs vis effect different? 
    slope_1 <- output$slope[output$exp_name == "a_or"]
    se_1 <- output$slope_se[output$exp_name == "a_or"]
    
    slope_2 <- output$slope[output$exp_name == "v_or"]
    se_2 <- output$slope_se[output$exp_name == "v_or"]
    
    #     z_test <- (slope_1 - slope_2) / sqrt(se_1^2 + se_2^2)
    #     p <- pnorm(-abs(z_test))
    
    df = 66 # N1-2 + N2-2 -- each N=35
    t_val <- (slope_1 - slope_2) / sqrt(se_1^2 + se_2^2)
    p_val = 2*pt(abs(t_val), df, lower=FALSE) # 2-tailed t-test. t(33) = 3.07, p = .0043
      # verified result against http://www.danielsoper.com/statcalc/calculator.aspx?id=103
    
    pairwise_tests$t_val[pairwise_tests$conds_compared == "obs_a-vs-v"] = t_val
    pairwise_tests$p_val[pairwise_tests$conds_compared == "obs_a-vs-v"] = p_val
    pairwise_tests$df[pairwise_tests$conds_compared == "obs_a-vs-v"] = df
    pairwise_tests$slope_1[pairwise_tests$conds_compared == "obs_a-vs-v"] = slope_1
    pairwise_tests$se_1[pairwise_tests$conds_compared == "obs_a-vs-v"] = se_1
    pairwise_tests$slope_2[pairwise_tests$conds_compared == "obs_a-vs-v"] = slope_2
    pairwise_tests$se_2[pairwise_tests$conds_compared == "obs_a-vs-v"] = se_2


  # within sync, is aud vs vis effect different?  
    slope_1 <- output$slope[output$exp_name == "a_sc"]
    se_1 <- output$slope_se[output$exp_name == "a_sc"]
    
    slope_2 <- output$slope[output$exp_name == "v_sc"]
    se_2 <- output$slope_se[output$exp_name == "v_sc"]

    df = 66 # N1-2 + N2-2 -- each N=35
    t_val <- (slope_1 - slope_2) / sqrt(se_1^2 + se_2^2)
    p_val = 2*pt(abs(t_val), df, lower=FALSE) # 2-tailed t-test. t(33) = 2.05, p = .0486 (p = .044 online)
      # verified result against http://www.danielsoper.com/statcalc/calculator.aspx?id=103
    
    pairwise_tests$t_val[pairwise_tests$conds_compared == "syn_a-vs-v"] = t_val
    pairwise_tests$p_val[pairwise_tests$conds_compared == "syn_a-vs-v"] = p_val
    pairwise_tests$df[pairwise_tests$conds_compared == "syn_a-vs-v"] = df
    pairwise_tests$slope_1[pairwise_tests$conds_compared == "syn_a-vs-v"] = slope_1
    pairwise_tests$se_1[pairwise_tests$conds_compared == "syn_a-vs-v"] = se_1
    pairwise_tests$slope_2[pairwise_tests$conds_compared == "syn_a-vs-v"] = slope_2
    pairwise_tests$se_2[pairwise_tests$conds_compared == "syn_a-vs-v"] = se_2


  write.csv(pairwise_tests, "tables/pairwise_tests_driftbyseed_2016-12-06.csv")
   

# ***REVISIONS 2015-12-06 -- SLOPE confidence intervals -------
# bookmarked useful web page: http://stattrek.com/regression/slope-confidence-interval.aspx?Tutorial=AP
# also consider (future) Ng & Wilcox, 2010 comparing the regression slopes of independent groups -- using bootstrap estimates of SE to construct CIs
  
  level = .99 # or .95
  
  slope_CIs <- output %>%
    select(exp_name, slope, slope_se) %>%
    mutate(n = 35,
           level = level,
           alpha = 1 - level,
           crit_p = 1 - alpha/2,
           df = n - 2,
           crit_t = abs(qt(crit_p,df)),  # tvalues in R: http://stackoverflow.com/questions/11526041/critical-t-values-in-r
           me = crit_t*slope_se, # margin of error
           lowerCI = slope - me,
           upperCI = slope + me)
    # checked these against an online calculator to verify that i did it correctly: http://www.danielsoper.com/statcalc/calculator.aspx?id=26
    
    write.csv(slope_CIs,"tables/lm_driftseed_slope99CIs_2016-12-06.csv") # 99% CI
    #write.csv(slope_CIs,"tables/lm_driftseed_slope95CIs_2016-12-06.csv") # 95% CI



#inprog Follow-up 2016-12-13 -- all conditions in one regression model --------
#   start with original data analysis (use drift linear fits)



#inprog Follow-up 2016-12-13 -- Final Tempo vs. SMT --------
#   suggested by Jan Brascamp during defense (and seconded by committee)
#   'bias in endpoint of sr chains relative to mean smt of cohort'
  
  # shortcuts to data (ind_drift and indivs) from 2016-11-10_exp2_MT-SR_relative-tempo.R script
    
    # basic check of data -- final tempo by seed int
    ind_drift %>%
      ggplot(aes(x = seed_int, y = final_tempo, color=seed, group = cohort)) +
      geom_point(size = 3) +
      facet_grid(mode~train_type)

    # seed tempo by avg smt of cohort -- no relationship between smt and seed
    ind_drift %>%
      ggplot(aes(x=smt_avg, y=seed_int, color=seed, group=exp_name)) +
      geom_point(size=3, position="jitter") + 
      geom_smooth(method=lm) + facet_grid(mode~train_type)

    # final tempo by avg smt of cohort -- positive linear relationship between smt and final
    #   for observe, but negative relationship for synchronize
    ind_drift %>%
      ggplot(aes(x=smt_avg, y=final_tempo, color=seed, group=seed)) +
      geom_point(size=3, position="jitter") + 
      geom_smooth(aes(group=exp_name), method=lm) + facet_grid(mode~train_type)
      



#inprog Follow-up 2016-12-13 -- By-Chain Estimates vs. SMT --------
#   suggested by Jan Brascamp during defense (and seconded by committee)
#   compare to using final tempo in each chain

  
  # start with df from initial read-in
  # run lm fit for each chain
  # plot x-intercept from fits against smt (expect these to fall on a line)
  
  
  
    # (1) linear fit to each chain (should be 140 rows)
      
      data <- tempo_with_seeds
      conds <- unique(data$chain_name)
      
      nrowOutput <- length(conds)
      ncolOutput <- 12
      output <- data.frame(matrix(ncol = ncolOutput, nrow = nrowOutput))
      cnames <- c("chain_name", "slope", "slope_se", "slope_p", "y_int", "y_int_p",
                  "F_val", "F_numdf", "F_dendf", "F_sig", "rsq", "x_int")
      
      colnames(output) <- cnames
      
      
      n = 1
      
      for (j in 1:length(conds)) {
        
        lm_out <- lm(mean_tempo ~ generation, 
                     data = subset(data,chain_name == conds[j]))
        
        output$chain_name[n] <- as.character(conds[j]) 
        output$slope[n] <- coefficients(lm_out)[2]
        output$slope_se[n] <- summary(lm_out)$coefficients[2,2]
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
      
      
      # save as table:
      write.csv(output,"tables/lm_am-sr_eachChain_2016-12-13.csv")
  
  
  # (2) plotting slopes and x-intercepts to get a feel for the data
  #     x-intercept = estimated convergence GENERATION?
  #     slope = convergence rate (negative = converge, positive = diverge)
  
  fit_summary <- output %>%
    left_join(tempo_with_seeds, by = "chain_name") %>%
    group_by(exp_name, mode, train_type) %>%
    summarize(mean_slope = mean(slope),
              mean_xint = mean(x_int))
  
  # linear fit parameters for each chain
  output %>%
    left_join(tempo_with_seeds, by = "chain_name") %>%
    ggplot(aes(x=slope, y=x_int, color = seed)) + 
    geom_point(size = 3) + 
    facet_grid(mode~train_type, scales = "free")
  
  # convergence rate -- if i estimated the x-intercept of THESE lines, that'd give me estimated convergence tempo
  output %>%
    left_join(tempo_with_seeds, by = "chain_name") %>%
    ggplot(aes(x=seed_int, y=slope, color = seed)) + 
    geom_point(size = 3) + 
    facet_grid(mode~train_type, scales = "free")
  
  
  
  # (3) x-intercepts (estimated convergence tempo for each chain) by mean SMT (by cohort)


#inprog Follow-up 2016-12-13 -- trying something...multiple regression --------

  # predict tempo from iteration and smt_avg and seed
  fit <- lm(reproduced_tempo ~ generation + smt_avg + seed_int, data=indivs)
  summary(fit) # show results
  coefficients(fit)
    # but what does it really mean?


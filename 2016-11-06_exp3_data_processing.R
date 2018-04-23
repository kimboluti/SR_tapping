
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
    taps_input <- "compiled data/taps_ep_sr_2016-11-05.csv"
    taps_summary_input <- "compiled data/summary_ep_2016-11-05.csv"
    # if decide to use FILTERED summary (for cvs)
    #taps_summary_input <- "compiled data/summary_ep_FILTERED_2016-11-05.csv"
    
    # self-paced motor tempo (MT) TAPS
    taps_mt_input <- "compiled data/taps_ep_mt_2016-11-05.csv"
    taps_mt_summary_input <- "compiled data/summary_ep_mt_2016-11-05.csv"
    
    # SYNC TAPS (if used in the future)
    #taps_sync_input <- "compiled data/taps_ep_sync_2016-10-10.csv"
    #taps_sync_summary_input <- "compiled data/summaryacrossblocks_ep_sync_2016-10-19.csv"
    
    # B&S data
    survey_input <- "b&s data/survey_neat.csv"

# !not updated! tables: output file names

# EP - SR: load e-prime (ep) serial reproduction (SR) taps & summary data & clean up------------------------------
  
  taps_sr <- read.csv(taps_input) %>%
    mutate(cohort = as.factor(cohort),
           participant = as.factor(participant),
           session = as.factor(session),
           seed = as.factor(seed),
           id = as.factor(id),
           tap_int = as.numeric(tap_int)) %>%
    select(-X)
  
  tempo_sr <- read.csv(taps_summary_input) %>%
    mutate(cohort = as.factor(cohort),
           participant = as.factor(participant),
           session = as.factor(session),
           seed = as.factor(seed),
           id = as.factor(id)) %>%
    rename(tempo = median_tempo) %>%
    select(-X)

# ***EP - SR: spaghetti df composition and plots -------
  
  # for spaghetti plot, need to add seeds to summary df
  # expect nrow to be nchains = 84
  seeds <- tempo_sr %>%
    group_by(train_type, cohort, participant, seed, seed_int, chain_name) %>% 
    summarize(tempo = mean(seed_int)) %>%
    mutate(session = "0",
           id = participant,
           generation = 0,
           nints = 31, sd_taps = 0, IQR = 0, SIQR = 0, cv = 0)
           
  # join taps and seeds summaries
  tempo_with_seeds <- tempo_sr %>%
    bind_rows(seeds) %>%
    mutate(seed = as.factor(seed))
  # don't worry about factor level warning...it's because seeds df adds new levels to session & id factors
  # to check seeds addition: tmp <- tempo_with_seeds[tempo_with_seeds$generation==0,]  

  write.csv(tempo_with_seeds, "compiled data/sr_tempo_with_seeds.csv")  


  # *** SPAGHETTI DRIFT PLOT ***
  #     by exposure condition
    s <- tempo_with_seeds %>%
      ggplot(aes(x = generation, y = tempo, group = chain_name)) +
      geom_line(alpha = .25, lwd = 1, color = "#999999") +
      geom_smooth(aes(group = seed, color = seed), lwd = 1.5, alpha = .75, se = F) +
      labs(title = '(A) Serial Reproduction Chains', x = 'Iteration', y = 'Tempo (ms)') +
      theme_bw() +
      theme(panel.grid.minor=element_blank(),
            panel.grid.major=element_blank(),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 11),
            #legend.position='right', #c(.9,.9) # 'none'
            legend.title = element_text(size=12),
            legend.text = element_text(size = 11)) +
      scale_color_discrete(name="Seed\nTempo (ms)") +
      scale_x_continuous(limits=c(-1,16), breaks=c(0,5,10,15)) +  
      facet_grid(.~train_type) 
  
    # save the plot!
    ggsave("sr_spag_2016-11-22.png", width=6.5, height=3.25, dpi=100)


# save dim (sqaure): 600 x 300
# dim from exp 2 for 2x2 array: 500x400 -- 10/31/16

# EP - SR: distribution histograms -------
# start with df from spaghetti plots
# not really useful

  # (1) points or boxplot seems to illustrate the difference in means best:
      
      # both initial and final plotted together:
      tempo_with_seeds %>%
        filter(generation == 0 | generation == 15) %>%
        mutate(generation = as.factor(as.character(generation))) %>%
        ggplot(aes(x = seed, y = tempo, color = generation)) +
        geom_point(size = 3, alpha = .5) + geom_line(alpha = .25) +
        #geom_boxplot() +
        labs(title = '(A) Comparison of Initial and Final Tempi',
             x = 'Chain initiation tempo', 
             y = 'Mean tempo (ms)') +
        # why isn't key relabelling working?
        #scale_fill_discrete(name = "Generation"),
         #                   labels = c("Seed", "Final")) +
        theme_bw() + 
        facet_grid(.~train_type)
      
      
    # initial alone.
    tempo_with_seeds %>%
      filter(generation == 0) %>%
      mutate(generation = as.factor(as.character(generation))) %>%
      ggplot(aes(x = seed, y = tempo, color = seed)) +
      #geom_point(size = 3, alpha = .5, position = "jitter") + geom_line(alpha = .25) +
      geom_boxplot() +
      labs(title = '(A) Initial Tempi',
           x = 'Chain initiation tempo', 
           y = 'Mean tempo (ms)') +
      coord_cartesian(ylim=c(0,3500)) +        
      theme_bw() + 
      facet_grid(train_type~.)
    
    # final alone. -- actually kind of telling?
    tempo_with_seeds %>%
      filter(generation == 15) %>%
      mutate(generation = as.factor(as.character(generation))) %>%
      ggplot(aes(x = seed, y = tempo, color = seed)) +
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
      facet_grid(train_type~.)


    # ***(2) horizontal lines by condition
    # save dim: 275 X 500 for no legend
    # save dim (smaller): 177 X 325
    
    # init hozo lines
    tempo_with_seeds %>%
      #filter(exp_name == "a_or") %>%
      ggplot(aes(x = seed_int, y = tempo, color = seed)) +
      geom_hline(aes(yintercept = seed_int, color = seed), size = 1.5, alpha = .75) +
      coord_cartesian(ylim=c(0,3500)) +  
      theme_bw() +
      labs(title = '(A) Seed Tempi',
           x = '', 
           y = 'Mean tempo (ms)') +
      theme(legend.position='none') +
      facet_grid(train_type~.)
    

    # final hozo lines
    tempo_with_seeds %>%
      filter(generation == 15) %>%
      mutate(seed = as.factor(seed)) %>%
      group_by(train_type, seed) %>%
      summarize(yint = mean(tempo, na.rm=T)) %>%
      ggplot(aes(x = seed_int, y = tempo, color = seed)) +
      geom_hline(aes(yintercept=yint, color = seed), size = 1.5, alpha = .75) +
      coord_cartesian(ylim=c(0,3500)) +  
      theme_bw() +
      labs(title = '(C) Final Tempi',
           x = '', 
           y = '') +
      theme(legend.position='none') +
      facet_grid(train_type~.)
    

  
    # (3) histograms from exp 2...not meaningfully adjusted for exp 3, but keeping just in case
    
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
    facet_grid(train_type~.)
  
  # just in case, though: final stimuli histogram
  tempo_sr %>%
    filter(generation == 15) %>%
    mutate(seed = as.factor(seed)) %>%
    group_by(train_type, seed) %>%
    summarize(xint = mean(tempo, na.rm=T)) %>%
    ggplot(aes(x = tempo, color = seed)) +
    #geom_histogram(aes(y = ..count..), binwidth = 50, position = "dodge", fill = "white") +
    #geom_vline(aes(xintercept=xint, color = seed), size = 2) +
    geom_hline(aes(yintercept=xint, color = seed), size = 1.5, alpha = .75) +
    facet_grid(train_type~.)
  
    
  # (4) rotated histograms - still not as visually useful as the hozo lines
    
    # initial stimuli histogram - flipped
    tempo_with_seeds %>%
      filter(generation == 0) %>%
      ggplot(aes(x = seed_int, color = seed, fill = seed)) + # fill = seed to fill with that color
      geom_histogram(binwidth = 75, position = "dodge") +
      coord_flip() +
      facet_grid(train_type~.)
    
    # final stimuli histogram - flipped
    tempo_with_seeds %>%
      filter(generation == 15) %>%
      ggplot(aes(x = tempo, color = seed, fill = seed)) +
      geom_histogram(aes(y = ..count..), binwidth = 70, position = "dodge") +
      coord_flip() +
      facet_grid(train_type~.)

# ***EP - SR: drift df composition -------

# add first generation 
first <- tempo_sr %>%
  filter(generation == 1) %>%
  rename(first_session = session,
         first_id = id,
         first_generation = generation,
         first_nints = nints,
         first_tempo = tempo,
         first_sd_taps = sd_taps,
         first_IQR = IQR,
         first_SIQR = SIQR,
         first_cv = cv)

# ack just re-doing this seeds df for compatibility
seeds_2 <- seeds %>%
  rename(seed_session = session,
         seed_id = id,
         seed_generation = generation,
         seed_nints = nints,
         seed_tempo = tempo,
         seed_sd_taps = sd_taps,
         seed_IQR = IQR,
         seed_SIQR = SIQR,
         seed_cv = cv)

# combining seeds, firsts, and finals, computing drift, only keeping select columns for simplicity
compare <- tempo_sr %>%
  group_by(participant) %>%
  filter(generation == max(generation)) %>%
  rename(final_session = session,
         final_id = id,
         final_generation = generation,
         final_nints = nints,
         final_tempo = tempo,
         final_sd_taps = sd_taps,
         final_IQR = IQR,
         final_SIQR = SIQR,
         final_cv = cv) %>%
  left_join(seeds_2, by = c("train_type", "cohort", "participant", "seed", "seed_int", "chain_name")) %>%
  left_join(first, by = c("train_type", "cohort", "participant", "seed", "seed_int", "chain_name")) %>%
  mutate(drift = final_tempo - seed_tempo,
         final_minus_first = final_tempo - first_tempo,
         drift_to_first = first_tempo - seed_tempo) %>%
  # update selected variables if i look at cvs later:
  select(train_type, cohort, participant, seed, seed_int, chain_name,
         seed_generation, seed_tempo, 
         first_generation, first_tempo, drift_to_first,
         final_generation, final_tempo, final_minus_first, drift)


compare %>%
  ggplot(aes(x = seed_int, y = drift_to_first, color = seed)) +
  geom_point(size = 3, alpha = .5) +
  #geom_smooth(aes(x = seed_int, y = drift, group = exp_name), method = lm, se = TRUE)+
  facet_grid(.~train_type)

write.csv(compare, "compiled data/chains_2016-11-08.csv")

# !notupdating? AM - SR: test - linear fits - final x seed -------
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

# !notupdating? AM - SR: test - linear fits - ratio (final:seed) x seed -------
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
  

# !notupdating? AM - SR: test - linear fits - relative change (drift/seed) by seed -------
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

# ***EP - SR: linear fits - drift x seed - by condition -------
# start with compare df from earlier
  
  data <- compare
  conds <- unique(data$train_type)
  
  nrowOutput <- length(conds)
  ncolOutput <- 11
  output <- data.frame(matrix(ncol = ncolOutput, nrow = nrowOutput))
  cnames <- c("train_type", "slope", "slope_p", "y_int", "y_int_p",
              "F_val", "F_numdf", "F_dendf", "F_sig", "rsq", "x_int")
  
  colnames(output) <- cnames
  
  
  n = 1
  
  for (j in 1:length(conds)) {
    
    lm_out <- lm(drift ~ seed_int, 
                 data = subset(data,train_type == conds[j]))
    
    output$train_type[n] <- as.character(conds[j]) 
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
  write.csv(output,"tables/lm_sr_bycond_driftxseed_2016-11-06.csv")
  
  # check that i'm fitting what i think i am:
  dataplot <- data %>%
    ggplot(aes(x = seed_int, y = drift)) + 
    geom_point(alpha = .5) + geom_smooth(method = lm, se = FALSE) +
    facet_wrap(~train_type)
  
  dataplot 
  
  # compare to lm fits:
  dataplot +
    geom_abline(data = output, aes(intercept = y_int, slope = slope), linetype = 2, size = 1.5) +
    facet_wrap(~train_type)
  
  # plot residuals or res normed by seed (for last condition fit, at least)
  res <- data.frame(as.numeric(resid(lm_out))) %>%
    rename(residuals = as.numeric.resid.lm_out..)
  
  data %>%
    filter(train_type == "synchronize") %>%
    bind_cols(res) %>%
    ggplot(aes(x = seed_tempo, y = res)) + geom_point()
    #ggplot(aes(x = seed_tempo, y = res/seed_tempo)) + geom_point()

  
  # clear holding variables
  #rm(data,conds,nrowOutput,ncolOutput,output,cnames,lm_out)
  rm(data,conds,nrowOutput,ncolOutput,cnames,lm_out)
  

# ***EP - SR: drift plots -------

  # not using: keeping code for line breaks in titles!
  # relationship between final and seed tempi
  compare %>%
    ggplot(aes(x = seed_int, y = final_tempo)) +
    geom_point(aes(color = seed), size = 3, alpha = .75) +
    geom_smooth(aes(group = train_type), method = lm, se = FALSE ) +
    theme_bw() +
    labs(title = '(A) Relationship', # useful for line breaks: '(A) Relationship between \n seed and final tempo'
        x = 'Seed tempo (ms)', 
        y = 'Final tempo (ms)') +
    theme(legend.position = 'none')+
    facet_grid(train_type~.)
  


  # *** drift plot 
  #     by exposure condition
  d <- compare %>%
    ggplot(aes(x = seed_int, y = drift)) +
    geom_point(aes(color = seed), size = 3, alpha = .75) +
    geom_line(aes(group=cohort), alpha = .25, lwd = 1, color = "#999999") +
    geom_hline(aes(yintercept = 0), color = "#999999") +
    geom_smooth(method = lm, se=F, lwd = 1.5, alpha = .75, color = 'black') + 
    labs(title = '(B) Chain Drift', x = 'Seed tempo (ms)', y = 'Drift (ms)') + 
    theme_bw() +
    theme(panel.grid.minor=element_blank(),
          panel.grid.major=element_blank(),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 11),
          axis.text.x = element_text(angle = 90, vjust=0.5),
          #legend.position='right', #c(.9,.9) # 'none'
          legend.title = element_text(size=12),
          legend.text = element_text(size = 11)) +
    scale_color_discrete(name="Seed\nTempo (ms)") +
    scale_x_continuous(breaks=unique(compare$seed_int)) +  
    scale_y_continuous(limits=c(-1500,3500), breaks=c(-1000,0,1000,2000,3000)) +  
    facet_grid(.~train_type) 
    
    # save the plot!
    ggsave("sr_drift_2016-11-20.png", width=6.5, height=3.25, dpi=100)

# save dim (sqaure): 600 x 300

# ***EP - SR: auto-regression & bias - df and plots -------
# alt: read in ar1 from file: ar1 <- read.csv("compiled data/ar1_2016-11-06.csv") %>% select(-X)
# save dim for by condition: 600x300

ar1 <- tempo_with_seeds %>%
  group_by(chain_name) %>%
  mutate(generation = as.numeric(as.character(generation))) %>% # convert gen to char before num to preserve '0' (i.e. factor values, not just indices wrt factor levels)
  arrange(generation) %>%
  mutate(next_gen = generation + 1,
         next_tempo = lead(tempo, 1),
         gen_plus2 = generation + 2,
         plus2_tempo = lead(tempo, 2),
         delta = next_tempo - tempo)

write.csv(ar1, "compiled data/ar1_2016-11-06.csv")
 
# *autoregression - by condition
ar1 %>%
  ggplot(aes(x = tempo, y = next_tempo)) +
  geom_point(aes(color = generation), size = 3, shape = 1) + 
  geom_smooth(aes(group = train_type), method = lm, se = FALSE, lwd=1.5, alpha=.75, color='black') +
  scale_colour_gradientn(colours = rainbow(n = 4, start = .5, end = 1)) +
  geom_abline(aes(slope=1,intercept=0),color="#999999") +
  theme_bw() +
  labs(title = '(A) Autoregression',
       x = 'Stimulus tempo (ms)', #expression(x[i])
       y =  'Reproduced tempo (ms)') + #expression(x[i+1]))
  facet_grid(.~train_type)
  

# *bias plot
ar1 %>%
  ggplot(aes(x = tempo, y = delta, color = generation)) +
  geom_point(aes(color = generation), size = 3, shape = 1) + 
  geom_smooth(aes(group = train_type), method = lm, se = FALSE, lwd=1.5, alpha=.75, color='black') +
  scale_colour_gradientn(colours = rainbow(n = 4, start = .5, end = 1)) +
  geom_hline(aes(yintercept=0),color="#999999") +
  theme_bw() +
  labs(title = '(B) Bias',
       x = 'Stimulus tempo (ms)', #expression(x[i])
       y =  'Reproduced - Stimulus tempo (ms)') + #expression(x[i+1] - x[i]))
  facet_grid(.~train_type)


# not used: markov assumption
ar1 %>%
  ggplot(aes(x = tempo, y = plus2_tempo, color = generation)) +
  geom_point() + geom_smooth(method = lm, se = FALSE) +
  facet_grid(.~train_type)

# ***EP - SR: auto-regression - lm -------
# start with ar1 df from earlier

data <- ar1
conds <- unique(data$train_type)

nrowOutput <- length(conds)
ncolOutput <- 11
output <- data.frame(matrix(ncol = ncolOutput, nrow = nrowOutput))
cnames <- c("train_type", "slope", "slope_p", "y_int", "y_int_p",
            "F_val", "F_numdf", "F_dendf", "F_sig", "rsq", "x_int")

colnames(output) <- cnames


n = 1

for (j in 1:length(conds)) {
  
  lm_out <- lm(tempo ~ next_tempo, 
               data = subset(data,train_type == conds[j]))
  
  output$train_type[n] <- as.character(conds[j]) 
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
write.csv(output,"tables/lm_sr_ar1_2016-11-06.csv")

# check that i'm fitting what i think i am:
dataplot <- data %>%
  ggplot(aes(x = tempo, y = next_tempo)) + 
  geom_point(alpha = .5) + geom_smooth(method = lm, se = FALSE) +
  facet_wrap(~train_type)

dataplot 

# compare to lm fits:
dataplot +
  geom_abline(data = output, aes(intercept = y_int, slope = slope), linetype = 2, size = 1.5) +
  facet_wrap(~train_type)

# plot residuals or res normed by seed (for last condition fit, at least)
# not working?
res <- data.frame(as.numeric(resid(lm_out))) %>%
  rename(residuals = as.numeric.resid.lm_out..)

data %>%
  filter(train_type == "synchronize") %>%
  bind_cols(res) %>%
  ggplot(aes(x = tempo, y = res)) + geom_point()
#ggplot(aes(x = seed_tempo, y = res/seed_tempo)) + geom_point()


# clear holding variables
#rm(data,conds,nrowOutput,ncolOutput,output,cnames,lm_out)
rm(data,conds,nrowOutput,ncolOutput,cnames,lm_out)

# EP - SR: bias - lm -------
# start with ar1 df from earlier

data <- ar1
conds <- unique(data$train_type)

nrowOutput <- length(conds)
ncolOutput <- 11
output <- data.frame(matrix(ncol = ncolOutput, nrow = nrowOutput))
cnames <- c("train_type", "slope", "slope_p", "y_int", "y_int_p",
            "F_val", "F_numdf", "F_dendf", "F_sig", "rsq", "x_int")

colnames(output) <- cnames


n = 1

for (j in 1:length(conds)) {
  
  lm_out <- lm(delta ~ tempo, 
               data = subset(data,train_type == conds[j]))
  
  output$train_type[n] <- as.character(conds[j]) 
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
write.csv(output,"tables/lm_sr_bias_2016-11-06.csv")

# check that i'm fitting what i think i am:
dataplot <- data %>%
  ggplot(aes(x = tempo, y = delta)) + 
  geom_point(alpha = .5) + geom_smooth(method = lm, se = FALSE) +
  facet_wrap(~train_type)

dataplot 

# compare to lm fits:
dataplot +
  geom_abline(data = output, aes(intercept = y_int, slope = slope), linetype = 2, size = 1.5) +
  facet_wrap(~train_type)

# clear holding variables
#rm(data,conds,nrowOutput,ncolOutput,output,cnames,lm_out)
rm(data,conds,nrowOutput,ncolOutput,cnames,lm_out)

# !EP - SR: dynamical systems plots -------
# use ar1 df from autoregression plots

# save dim with legend: ~800x700 (not totally decided)
# saved one for sync, one for observe

# return plot -- deciding: arrows or line segments?
ar1 %>%
  filter(train_type == "synchronize") %>%
  ggplot(aes(x = tempo, y = next_tempo, color = generation)) +
  geom_point(size = 3, shape = 1) + 
  # plot arrow from gen to gen:
   geom_segment(aes(xend=c(tail(tempo, n=-1), NA), 
                    yend=c(tail(next_tempo, n=-1), NA)), 
                arrow = arrow(angle = 45, length = unit(0.075, "inches"), type = "closed")) +
#   
#   # alt plot just line segments:
#   geom_segment(aes(xend=c(tail(tempo, n=-1), NA), 
#                    yend=c(tail(next_tempo, n=-1), NA))) + 
  
  scale_colour_gradientn(colours = rainbow(n = 4, start = .5, end = 1)) +
  theme_bw() +
  labs(title = '(B) Synchronize Return Plot',
       x = expression(x[i]), 
       y = expression(x[i+1])) +
  #theme(legend.position = 'none')+
  facet_grid(seed~cohort, scales="free")


# return plot by participant, lines by seed
ar1 %>%
  #filter(train_type == "synchronize") %>%
  ggplot(aes(x = tempo, y = next_tempo, color = seed)) +
  geom_point(size = 3, shape = 1) + 
  # plot arrow from gen to gen:
  geom_segment(aes(xend=c(tail(tempo, n=-1), NA), 
                   yend=c(tail(next_tempo, n=-1), NA)), 
               arrow = arrow(angle = 45, length = unit(0.075, "inches"), type = "closed")) +
  #   
  #   # alt plot just line segments:
  #   geom_segment(aes(xend=c(tail(tempo, n=-1), NA), 
  #                    yend=c(tail(next_tempo, n=-1), NA))) + 
  
  #scale_colour_gradientn(colours = rainbow(n = 4, start = .5, end = 1)) +
  theme_bw() +
  labs(title = '(C) Return Plot by Participant',
       x = expression(x[i]), 
       y = expression(x[i+1])) +
  #theme(legend.position = 'none')+
  facet_wrap(~participant, scales="free") # still kind diff, but better than other faceting...
  #facet_grid(cohort~train_type, scales="free") # difficult to see patterns


# anoter option: return plot with mean across cohorts
# still deciding: arrows or line segments?
ar1 %>%
  group_by(train_type, seed, seed_int, generation) %>%
  summarize(mean_tempo = mean(tempo),
            next_mean_tempo = mean(next_tempo)) %>%
  
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
  facet_grid(seed~train_type, scales="free")
  
  
# delta plot with summary by cohort
ar1 %>%
  group_by(train_type, seed, seed_int, generation) %>%
  summarize(mean_tempo = mean(tempo),
            delta_mean = mean(delta)) %>%
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
  facet_grid(seed~train_type)

# !WORKING TAP SUMMARY: simulations from linear fits ------------------------------
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

# !(TAPS &) TAP SUMMARY phase return plot & deltas ------------------------
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
             participant = as.factor(participant),
             session = as.factor(session),
             id = as.factor(id),
             block = as.factor(block),
             tap_int = as.numeric(tap_int)) %>%
      select(-X)
    

    taps_mt_summary <- read.csv(taps_mt_summary_input) %>%
      mutate(cohort = as.factor(cohort),
             participant = as.factor(participant),
             session = as.factor(session),
             id = as.factor(id),
             block = as.factor(block)) %>%  
      rename(tempo = median_tempo) %>%
      select(-X, -mean_tempo, -sd_taps, -cv_with_sd)
           

# MT: WIDE form df & normalized produced range (NPR) -------------------------------
# wide companion to long-form taps_mt_summary df
# EACH GENERATION

# first create wide format by creating individual dfs by tap_type, renaming variabils, then recombining
  
  smt <- taps_mt_summary %>%
    filter(tap_type == "smt") %>%
    rename(smt_block = block,
           smt_nints = nints,
           smt_mean = tempo,
           smt_cv = cv) %>%
    select(-tap_type, -IQR, - SIQR, -se)

  fast_mt <- taps_mt_summary %>%
    filter(tap_type == "fast mt") %>%
    rename(fast_block = block,
           fast_nints = nints,
           fast_mean = tempo,
           fast_cv = cv) %>%
    select(-tap_type, -IQR, - SIQR, -se)

  slow_mt<- taps_mt_summary %>%
    filter(tap_type == "slow mt") %>%
    rename(slow_block = block,
           slow_nints = nints,
           slow_mean = tempo,
           slow_cv = cv) %>%
    select(-tap_type, -IQR, - SIQR, -se)

  # join mt's and calculate produced range (pr) and normalized pr (npr)
   mt_wide <- smt %>%
    full_join(fast_mt, by = c("train_type","cohort", "participant", "session", "id","generation")) %>%
    full_join(slow_mt, by = c("train_type","cohort", "participant", "session", "id","generation")) %>%
    mutate(range = slow_mean - fast_mean,
           norm_range = range/smt_mean)
           
  write.csv(mt_wide, "tables/mt_wide_2016-11-07.csv")
           

  # plot check
  ggplot(mt_wide, aes(x = generation, y = range, group = cohort, color = train_type)) +
    geom_point(size = 3, alpha = .75) + 
    geom_line(alpha = .5, lwd = 2) +
    theme_bw() +
    facet_grid(cohort~train_type)
  
  # useful plot - are cohorts the same within a condition?
  ggplot(mt_wide, aes(x = generation, y = range, group = cohort, color = cohort)) +
    geom_point(size = 3, alpha = .75) + 
    geom_line(alpha = .5, lwd = 2) +
    theme_bw() +
    facet_grid(.~train_type)
   
  # checking out normed produced range
  ggplot(mt_wide, aes(x = generation, y = norm_range, group = cohort, color = cohort)) +
    geom_point() +
    geom_line() +
    theme_bw() +
    facet_grid(.~train_type)

  

# MT: WIDE form - summaries -------------------------------
# by participant, by condition, all (AND saving, for spss)
      
    # don't need every time: (1) summarize excluded (start from taps_mt) 
    # see also separate script from 2016-11-07
    excluded <- taps_mt %>%
      mutate(count = 1) %>%
      #group_by(train_type, tap_type) %>%
      group_by(tap_type) %>% # to summarize exclude by tap type, comment out above and uncomment this
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
    
    write.csv(excluded, "tables/mt_excluded_bycond&taptype_2016-11-07.csv")
    
  # (2) summarize by participant, add consistency
  # redundant with LONG form by participant summary (just wide instead of long)
    smt_summary <- smt %>%
      group_by(train_type, participant) %>%
      summarize(smt_n = n(),
                smt_avg = mean(smt_mean, na.rm=T),
                smt_sd = sd(smt_mean, na.rm=T),
                smt_avg_cv = mean(smt_cv, na.rm=T),
                smt_avg_cv_sd = sd(smt_cv, na.rm=T)) %>%
      mutate(smt_se = smt_sd/sqrt(smt_n),
             smt_avg_cv_se = smt_avg_cv_sd/sqrt(smt_n),
             smt_consistency = smt_sd/smt_avg)
    
    fast_summary <- fast_mt %>%
      group_by(train_type, participant) %>%
      summarize(fast_n = n(),
                fast_avg = mean(fast_mean, na.rm=T),
                fast_sd = sd(fast_mean, na.rm=T),
                fast_avg_cv = mean(fast_cv, na.rm=T),
                fast_avg_cv_sd = sd(fast_cv, na.rm=T)) %>%
      mutate(fast_se = fast_sd/sqrt(fast_n),
             fast_avg_cv_se = fast_avg_cv_sd/sqrt(fast_n),
             fast_consistency = fast_sd/fast_avg)
    
    slow_summary <- slow_mt %>%
      group_by(train_type, participant) %>%
      summarize(slow_n = n(),
                slow_avg = mean(slow_mean, na.rm=T),
                slow_sd = sd(slow_mean, na.rm=T),
                slow_avg_cv = mean(slow_cv, na.rm=T),
                slow_avg_cv_sd = sd(slow_cv, na.rm=T)) %>%
      mutate(slow_se = slow_sd/sqrt(slow_n),
             slow_avg_cv_se = slow_avg_cv_sd/sqrt(slow_n),
             slow_consistency = slow_sd/slow_avg)
    
    npr_summary <- mt_wide %>%
      group_by(train_type, participant) %>%
      summarize(npr_n = n(),
                range_avg = mean(range, na.rm=T),
                range_sd = sd(range, na.rm=T),
                npr_avg = mean(norm_range, na.rm=T),
                npr_sd = sd(norm_range, na.rm=T)) %>%
      mutate(range_se = range_sd/sqrt(npr_n),
             npr_se = npr_sd/sqrt(npr_n),
             npr_consistency = npr_sd/npr_avg)
    
    mt_summary_byparticipant <- fast_summary %>%
      full_join(smt_summary, by = c("train_type", "participant")) %>%
      full_join(slow_summary, by = c("train_type", "participant")) %>%
      full_join(npr_summary, by = c("train_type", "participant"))
    
    write.csv(mt_summary_byparticipant, "tables/mt_summary_byparticipant_wide_2016-11-07.csv")


  # don't need every time: (3) summarize by condition
  # feels like it makes sense to do separate tap_type dfs and then combine...(to get n's)
    smt_summary <- smt %>%
      group_by(train_type) %>%
      summarize(smt_n = n(),
                smt_avg = mean(smt_mean, na.rm=T),
                smt_sd = sd(smt_mean, na.rm=T),
                smt_avg_cv = mean(smt_cv, na.rm=T),
                smt_avg_cv_sd = sd(smt_cv, na.rm=T)) %>%
      mutate(smt_se = smt_sd/sqrt(smt_n),
             smt_avg_cv_se = smt_avg_cv_sd/sqrt(smt_n))
                
    fast_summary <- fast_mt %>%
      group_by(train_type) %>%
      summarize(fast_n = n(),
                fast_avg = mean(fast_mean, na.rm=T),
                fast_sd = sd(fast_mean, na.rm=T),
                fast_avg_cv = mean(fast_cv, na.rm=T),
                fast_avg_cv_sd = sd(fast_cv, na.rm=T)) %>%
      mutate(fast_se = fast_sd/sqrt(fast_n),
             fast_avg_cv_se = fast_avg_cv_sd/sqrt(fast_n))
    
    slow_summary <- slow_mt %>%
      group_by(train_type) %>%
      summarize(slow_n = n(),
                slow_avg = mean(slow_mean, na.rm=T),
                slow_sd = sd(slow_mean, na.rm=T),
                slow_avg_cv = mean(slow_cv, na.rm=T),
                slow_avg_cv_sd = sd(slow_cv, na.rm=T)) %>%
      mutate(slow_se = slow_sd/sqrt(slow_n),
             slow_avg_cv_se = slow_avg_cv_sd/sqrt(slow_n))
    
    npr_summary <- mt_wide %>%
      group_by(train_type) %>%
      summarize(npr_n = n(),
                range_avg = mean(range, na.rm=T),
                range_sd = sd(range, na.rm=T),
                npr_avg = mean(norm_range, na.rm=T),
                npr_sd = sd(norm_range, na.rm=T)) %>%
      mutate(range_se = range_sd/sqrt(npr_n),
             npr_se = npr_sd/sqrt(npr_n))
    
    mt_summary_bycondition <- fast_summary %>%
      full_join(smt_summary, by = c("train_type")) %>%
      full_join(slow_summary, by = c("train_type")) %>%
      full_join(npr_summary, by = c("train_type"))
    
    write.csv(mt_summary_bycondition, "tables/mt_summary_bycondition_2016-11-07.csv")

  # (4) summarize for whole experiment
    # to do...if needed...could also just type as needed...(should i write  function (mysummary) that reports mean, sd, se?)
    
    # for se's: n = ?? 
    report <- c(mean(mt_wide$fast_mean), sd(mt_wide$fast_mean), sd(mt_wide$fast_mean)/sqrt(75))


# MT: LONG form - summaries -------------------------------
# start with taps_mt_summary plus npr from mt_wide df
# by participant and all
# redundant with WIDE form by participant summary (just long instead of wide)
# easier for plotting and quick summaries

  # by participant (without npr)
  mt_parti <- taps_mt_summary %>%
    group_by(participant, tap_type,
             train_type, cohort) %>%
    summarize(sum_nints = sum(nints),
              nprods = n(),
              avg_tempo = mean(tempo, na.rm=T),
              sd_tempo = sd(tempo, na.rm=T),
              avg_cv = mean(cv, na.rm=T),
              sd_cv = sd(cv, na.rm=T)) %>%
    mutate(se_tempo = sd_tempo/sqrt(nprods),
           consistency_tempo = sd_tempo/avg_tempo,
           se_cv = sd_cv/sqrt(nprods)) %>%
    select(train_type, participant, cohort, tap_type, sum_nints, nprods,
           avg_tempo, sd_tempo, se_tempo, consistency_tempo,
           avg_cv, sd_cv, se_cv)


  # select columns from npr_wide df to add as cols to long
  npr_parti <- npr_summary %>%
    select(train_type, participant, npr_avg, npr_consistency)

  # by participant (with npr) - add columns for npr values (repeats for each other tap type)
  mt_parti <- mt_parti %>%
    full_join(npr_parti, by = c("train_type", "participant"))
  
  # and save
  write.csv(mt_parti,"tables/mt_summary_byparticipant_long_2016-11-07.csv")
  

  # not everytime: all
  summary_all <- taps_mt_summary %>%
    group_by(tap_type) %>%
    summarize(n = n(),
              grand_tempo = mean(tempo, na.rm=T),
              sd_tempo = sd(tempo, na.rm=T),
              grand_consistency = sd(tempo, na.rm=T)/mean(tempo, na.rm=T),
              grand_cv = mean(cv, na.rm=T),
              sd_cv = sd(cv, na.rm=T))
  
  # for plot comparing distribution of MT tempi by condition:
  taps_mt_summary %>%
    ggplot(aes(x=tap_type, y=tempo, color=tap_type)) + 
    geom_boxplot() +
    facet_grid(.~train_type)
  

# MT: shortcut data - load (dfs made so far) or clear dfs from creation ------------
  
  # clear accessory dfs 
  rm(fast_mt, fast_summary, npr_parti, npr_summary, smt, smt_summary, slow_mt, slow_summary)
  
  
  # load wide by participant and generation, or by participant
  mt_wide <- read.csv("tables/mt_wide_2016-11-07.csv")
  
  mt_summary_byparticipant <- read.csv("tables/mt_summary_byparticipant_wide_2016-11-07.csv") %>%
    mutate(participant=as.factor(participant)) %>%
    select(-X)

  # load long by participant
  mt_parti <- read.csv("tables/mt_summary_byparticipant_long_2016-11-07.csv") %>%
    mutate(participant=as.factor(participant)) %>%
    select(-X)

  
##### ***MT-SR ratio : df composition & shortcuts -----------------
# used mt_wide (cuz col labels) (mt_summary_byparticipant <- read.csv("tables/mt_summary_byparticipant_wide_2016-11-07.csv") %>% select(-X))
# used ar1 as SR df (ar1 <- read.csv("compiled data/ar1_2016-11-06.csv") %>% select(-X))
# creates indivs df (2100 rows for each participant*chain) - ar1 & mt data combined, pluse relative tempo

  # (1) shortcuts:

    # for spaghetti, ar1, and bias plots: 
    # 1323 rows = [1239 rows = (11Ps * 15gens * 7 seeds) + (1P*12g*7s)] + 84 seeds
      indivs <- read.csv("tables/indivs_mt_sr_bygen_2016-11-09.csv") %>%
        mutate(cohort = as.factor(cohort),
               participant = as.factor(participant),
               session = as.factor(session),
               seed = as.factor(seed),
               id = as.factor(id)) %>%
        select(-X)

      # for drift plots:
      # 84 rows = 1 for each chain
      ind_drift <- read.csv("tables/indivs_mt_sr_chains_2016-11-09.csv") %>%
        mutate(cohort = as.factor(cohort),
               participant = as.factor(participant),
               seed = as.factor(seed)) %>%
        select(-X)


  # (2) original df construction and saving: 

    # (a) indivs df --- by generation
      # kind of just renaming things, plus making a participant id
      # each participant:
        indivs_ar1 <- ar1 %>%
          mutate(participant = as.factor(participant),
                 stimulus_generation = generation,
                 generation = next_gen,
                 stimulus_tempo = tempo,
                 reproduced_tempo = next_tempo,
                 bias = reproduced_tempo - stimulus_tempo) %>%
          #filter(generation !=16) %>%
          select(train_type,cohort,participant, # shared with mt
                 session,id,seed,seed_int,chain_name,
                 stimulus_generation,stimulus_tempo, 
                 generation,reproduced_tempo,bias)
        
      # combining
        indivs <- indivs_ar1 %>%
          full_join(mt_summary_byparticipant, 
                    by = c("train_type", "participant")) %>%
          mutate(stim_rtempo = (stimulus_tempo - smt_avg)/smt_avg,
                 fast_rtempo = (fast_avg - smt_avg)/smt_avg,
                 slow_rtempo = (slow_avg - smt_avg)/smt_avg)
        
      write.csv(indivs, "tables/indivs_mt_sr_bygen_2016-11-09.csv")
        
        
    # (b) ind_drift df --- by chain (requires compare df)

        ind_drift <- compare %>%
          full_join(mt_summary_byparticipant, 
                    by = c("train_type", "participant")) %>%
          mutate(seed_rtempo = (seed_int - smt_avg)/smt_avg,
                 fast_rtempo = (fast_avg - smt_avg)/smt_avg,
                 slow_rtempo = (slow_avg - smt_avg)/smt_avg)
      
       write.csv(ind_drift, "tables/indivs_mt_sr_chains_2016-11-09.csv")


# MT: plots ------------------------
# use LONG to plot tap type on same plot (taps_mt_summary or mt_parti)
# use WIDE ... for stats. not really good for plots. 
  

  # *All MT on one plot (grey lines individual, colored by tap type)
    mt <- taps_mt_summary %>%
      mutate(tap_type = factor(taps_mt_summary$tap_type, levels = c("fast mt", "smt", "slow mt"))) %>% # handy trick for changing factor order for plots!
      ggplot(aes(x=generation, y=tempo, group=cohort, color=tap_type)) +
    #  geom_point(size = 1, alpha = .5, color = "#999999") + 
      geom_line(alpha = .5, lwd = 1, color = "#999999") +
      geom_smooth(aes(group=tap_type, color=tap_type), method=lm, lwd=1.5, alpha=.75, se=F) +
      theme_bw() + 
      labs(title = '(A) Self-paced Motor Tempi', x = 'Generation', y = 'Tempo (ms)') +
      theme(panel.grid.minor=element_blank(),
            panel.grid.major=element_blank(),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 11),
            #axis.text.x = element_text(angle = 90),
            legend.position='none', #c(.9,.9) # 'none'
            legend.title = element_text(size=12),
            legend.text = element_text(size = 11)) +
      scale_color_discrete(name="Task") +
      scale_x_continuous(limits=c(-1,16), breaks=c(0,5,10,15)) +  
      #scale_y_continuous(limits=c(-1.5,5.5), breaks=c(-1,0,1,2,3,4,5)) +    
      facet_grid(tap_type~train_type, scales="free")
    
    # save the plot!
    ggsave("mt_spag_2016-11-09.png", width=3.25, height=3.75, dpi=100)

  # *All variability on one plot (grey lines individual, colored by tap type)
    cv <- taps_mt_summary %>%
      mutate(tap_type = factor(taps_mt_summary$tap_type, levels = c("fast mt", "smt", "slow mt"))) %>% # handy trick for changing factor order for plots!
      ggplot(aes(x = generation, y = cv, group = cohort, color = tap_type)) +
      geom_point(size = 1, alpha = .5, color = "#999999") + 
      geom_line(alpha = .5, lwd = 1, color = "#999999") +
      geom_smooth(aes(group = tap_type, color = tap_type), method=lm, lwd = 1.5, alpha = .75, se = F) +
      theme_bw() + 
      labs(title = '(B) Self-paced Motor Variability', x = 'Generation', y = 'CV') +
      theme(panel.grid.minor=element_blank(),
            panel.grid.major=element_blank(),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 11),
            #axis.text.x = element_text(angle = 90),
            legend.position='none', #c(.9,.9) # 'none'
            legend.title = element_text(size=12),
            legend.text = element_text(size = 11)) +
      coord_cartesian(ylim=c(0,.5)) +
      scale_x_continuous(limits=c(-1,16), breaks=c(0,5,10,15)) +  
      facet_grid(tap_type~train_type, scales="free")
    
    # save the plot!
    ggsave("mt_cv_2016-11-09.png", width=3.25, height=3.75, dpi=100)


  
  # not using: SEPARATE PLOTS FOR EACH TAP TYPE:
    taps_mt_summary %>%
      filter(tap_type == "fast mt") %>%
      ggplot(aes(x = generation, y = tempo, group = cohort)) +
      geom_point(size = 1, alpha = .5, color = "#999999") + #color = "#999999"
      geom_line(alpha = .5, lwd = 1, color = "#999999") +
      geom_smooth(aes(group = train_type), method=lm, color = '#FF8080', lwd = 1.5, alpha = .75, se = F) +  
      theme_bw() +
      labs(title = '(A) Fastest', x = 'Generation', y = 'Mean tempo (ms)') +
      facet_grid(.~train_type)
    
    
    taps_mt_summary %>%
      filter(tap_type == "smt") %>%
      ggplot(aes(x = generation, y = tempo, group = cohort)) +
      geom_point(size = 1, alpha = .5, color = "#999999") + 
      geom_line(alpha = .5, lwd = 1, color = "#999999") +
      geom_smooth(aes(group = train_type), color = '#409FFF', lwd = 1.5, alpha = .75, se = F) +
      theme_bw() +
      labs(title = '(B) Spontaneous', x = 'Generation', y = 'Mean tempo (ms)') +
      facet_grid(train_type~.)
    
    
    taps_mt_summary %>%
      filter(tap_type == "slow mt") %>%
      ggplot(aes(x = generation, y = tempo, group = cohort)) +
      geom_point(size = 1, alpha = .5, color = "#999999") + 
      geom_line(alpha = .5, lwd = 1, color = "#999999") +
      geom_smooth(aes(group = train_type), color = '#00BF00', lwd = 1.5, alpha = .75, se = F) +
      theme_bw() +
      labs(title = '(C) Slowest', x = 'Generation', y = 'Mean tempo (ms)') +
      facet_grid(train_type~.)

  
  # not using: mean(median_tempo) experimenting with free scales:
  taps_mt_summary %>%
    ggplot(aes(x = generation, y = tempo, group = cohort, color = tap_type)) +
    geom_point() + geom_line() +
    facet_grid(tap_type~train_type, scales = "free")
  
  # not using; mean(median_tempo) experimenting with boxplots
  taps_mt_summary %>%
    ggplot(aes(x = tap_type, y = tempo, color = tap_type)) +
    geom_boxplot() +
    theme_bw() + facet_grid(.~train_type)
  
    
  # not using: boxplot of cvs by tap type/condition
    taps_mt_summary %>%
      ggplot(aes(x = tap_type, y = cv, color = tap_type)) +
      geom_boxplot() +
      theme_bw() + facet_grid(.~train_type)
  

# MT: plots 2 (+df, taps dynamics) ------------------------
# use taps_mt
# experimenting with return plots for SMT taps

# make df:
mt_taps_return <- taps_mt %>%
  filter(tap_include == 1,
         tap_type == "smt") %>%
  group_by(id) %>%
  arrange(tap_number) %>%
  mutate(next_number = tap_number + 1,
         next_int = lead(tap_int, 1),
         delta = next_int - tap_int) %>%
  select(train_type,cohort,participant,session,id,generation,block,tap_type,
         tap_number,tap_int,next_number,next_int,delta)


  # return plot for all -- kinda confusing to look at
  # saved just in case, dim: 700x400...i think...
  mt_taps_return %>%
    ggplot(aes(x=tap_int, y = next_int, color = tap_number)) +
    geom_point(size = 3, shape = 1) + 
    # plot arrow from gen to gen:
    geom_segment(aes(xend=c(tail(tap_int, n=-1), NA), 
                     yend=c(tail(next_int, n=-1), NA)), 
                 arrow = arrow(angle = 45, length = unit(0.075, "inches"), type = "closed")) +
    #   
    #   # alt plot just line segments:
    #   geom_segment(aes(xend=c(tail(tempo, n=-1), NA), 
    #                    yend=c(tail(next_tempo, n=-1), NA))) + 
    
    scale_colour_gradientn(colours = rainbow(n = 4, start = .5, end = 1)) +
    theme_bw() +
    labs(title = 'SMT Return Plot',
         x = expression(x[i]), 
         y = expression(x[i+1])) +
    #coord_cartesian(xlim=c(0,2000), ylim=c(0,2000)) +
    #theme(legend.position = 'none')+
    #facet_grid(participant~~session, scales="free")
    facet_wrap(~participant, scales="free")
  
  # return plot for one participant
  mt_taps_return %>%
    filter(participant==100) %>%
    ggplot(aes(x=tap_int, y = next_int, color = tap_number)) +
    geom_point(size = 3, shape = 1) + 
    # plot arrow from gen to gen:
    geom_segment(aes(xend=c(tail(tap_int, n=-1), NA), 
                     yend=c(tail(next_int, n=-1), NA)), 
                 arrow = arrow(angle = 45, length = unit(0.075, "inches"), type = "closed")) +
    #   # alt plot just line segments:
    #   geom_segment(aes(xend=c(tail(tempo, n=-1), NA), 
    #                    yend=c(tail(next_tempo, n=-1), NA))) + 
    scale_colour_gradientn(colours = rainbow(n = 4, start = .5, end = 1)) +
    theme_bw() +
    labs(title = 'SMT Return Plot',
         x = expression(x[i]), 
         y = expression(x[i+1])) +
    #theme(legend.position = 'none')+
    facet_wrap(~session, scales="free")


  # *delta plot for all -- also a little confusing
  # saved just in case, dim: 700x400
  mt_taps_return %>%
    ggplot(aes(x=tap_int, y = delta, color = tap_number)) +
    geom_point(size = 3, shape = 1) + 
    # plot arrow from gen to gen:
     geom_segment(aes(xend=c(tail(tap_int, n=-1), NA), 
                      yend=c(tail(delta, n=-1), NA)), 
                  arrow = arrow(angle = 45, length = unit(0.075, "inches"), type = "closed")) +
    #   
    #   # alt plot just line segments:
    #   geom_segment(aes(xend=c(tail(tempo, n=-1), NA), 
    #                    yend=c(tail(next_tempo, n=-1), NA))) + 
    
    scale_colour_gradientn(colours = rainbow(n = 4, start = .5, end = 1)) +
    theme_bw() +
    labs(title = 'SMT Delta Plot',
         x = expression(x[i]), 
         y = expression(x[i+1] - x[i])) +
    coord_cartesian(xlim=c(0,2000), ylim=c(-500,500)) +
    #theme(legend.position = 'none')+
    #facet_grid(participant~~session, scales="free")
    facet_wrap(~participant) # , scales="free"



# MT: plots 3 (+df, tempo dynamics) ------------------------
# use taps_mt_summary
# experimenting with return plots for SMT tempi
# (but there shouldn't be a relationship that changes meaningfully over gen?)
  
  # make df:
  mt_return <- taps_mt_summary %>%
    filter(tap_type == "smt") %>%
    group_by(participant) %>%
    arrange(generation) %>%
    mutate(next_generation = generation + 1,
           next_tempo = lead(tempo, 1),
           delta = next_tempo - tempo) %>%
    select(train_type,cohort,participant,session,id,generation,block,tap_type,
           tempo,next_generation,next_tempo,delta)


  # tempo * kind of cool?
  mt_return %>%
    ggplot(aes(x=tempo, y =next_tempo, color = generation)) +
    geom_point(size = 3, shape = 1) + 
    geom_segment(aes(xend=c(tail(tempo, n=-1), NA), 
                     yend=c(tail(next_tempo, n=-1), NA)), 
                 arrow = arrow(angle = 45, length = unit(0.075, "inches"), type = "closed")) +
    scale_colour_gradientn(colours = rainbow(n = 4, start = .5, end = 1)) +
    theme_bw() +
    labs(title = 'SMT TEMPO Return Plot',
         x = expression(x[i]), 
         y = expression(x[i+1])) +
    facet_wrap(~participant, scales="free") #
  
  # delta * also maybe kind of cool?
  mt_return %>%
    ggplot(aes(x=generation, y = delta, color = generation)) +
    geom_point(size = 3, shape = 1) + 
    scale_colour_gradientn(colours = rainbow(n = 4, start = .5, end = 1)) +
    theme_bw() +
    labs(title = 'SMT TEMPO Delta Plot',
         x = expression(x[i]), 
         y = expression(x[i+1] - x[i])) +
    facet_wrap(~participant) # , scales="free"
  

# ***MT-SR: plots 1 (by condition: relative tempo spaghetti and drift) ------------------------
# good plot formatting and saving
# indivs for (1) spaghetti and (2) bias
# ind_drift for (3) drift

  # *(1) spaghetti with relative tempo by condition 
    p <- indivs %>%
      ggplot(aes(x=stimulus_generation, y=stim_rtempo, group=chain_name)) +
      geom_line(alpha = .25, lwd = 1, color = "#999999") +
      geom_smooth(aes(group=seed, color=seed), lwd = 1.5, alpha = .75, se = F) +
      geom_hline(yintercept=0, linetype=2, alpha=.95) +
      theme_bw() +
      labs(title = '(A) Relative Tempo Chains',
           x = 'Iteration', 
           y = 'Relative Tempo') +
      theme(panel.grid.minor=element_blank(),
            panel.grid.major=element_blank(),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 11),
            #axis.text.x = element_text(angle = 90),
            #legend.position='right', #c(.9,.9) # 'none'
            legend.title = element_text(size=12),
            legend.text = element_text(size = 11)) +
      scale_color_discrete(name="Seed\nTempo (ms)") +
      scale_x_continuous(limits=c(-1,16), breaks=c(0,5,10,15)) +  
      scale_y_continuous(limits=c(-1.5,5.5), breaks=c(-1,0,1,2,3,4,5)) +  
      facet_grid(.~train_type) 
  
    # choose fast and slow lines either (a) by condition or (b) by overall sample: 
      
      # for fast and slow by condition, use avg_rlims_c. 
        avg_rlims_c <- indivs %>%
          group_by(train_type) %>%
          summarize(fast_avg = mean(fast_rtempo,na.rm=T),
                    slow_avg = mean(slow_rtempo,na.rm=T))
      
        # add hlines to plot: 
          p <- p + geom_hline(aes(yintercept=fast_avg), data=avg_rlims_c, linetype=2, alpha=.95)
          p <- p + geom_hline(aes(yintercept=slow_avg), data=avg_rlims_c, linetype=2, alpha=.95)
          
            
  #     # For overall, use avg_rfast and avg_rslow:
  #       avg_rfast <- mean(indivs$fast_rtempo, na.rm=T)
  #       avg_rslow <- mean(indivs$slow_rtempo, na.rm=T)
  #     
  #       # add hlines to plot:
  #         p <- p + geom_hline(yintercept=avg_rfast, linetype=2, alpha=.95) #yintercept=avg_rlim$fast_avg, color = '#FF8080', lwd = 1.5, alpha = .75
  #         p <- p + geom_hline(yintercept=avg_rslow, linetype=2, alpha=.95) #yintercept=avg_rlims$slow_avg, color = '#409FFF', lwd = 1.5, alpha = .75 
    
    # save the plot!
    ggsave("rtempo_spag_2016-11-20.png", width=6.5, height=3.25, dpi=100)


  # (2) bias by condition
    
    b <- indivs %>%
      ggplot(aes(x=stim_rtempo, y=bias, color = stimulus_generation)) +
      geom_point(aes(color = stimulus_generation), size = 3, shape = 1) + 
      geom_smooth(aes(group = train_type), method = lm, se = FALSE, lwd=1.5, alpha=.75, color='black') +
      scale_colour_gradientn(colours = rainbow(n = 4, start = .5, end = 1),
                             name="Generation") +
      geom_hline(aes(yintercept=0),color="#999999") + # no bias line
      geom_vline(aes(xintercept=0),linetype=2, alpha=.95) + # stim = smt line
      theme_bw() +
      labs(title = '(B) Bias by Relative Tempo',
           x = 'Relative Tempo', 
           y = 'Bias (ms)') +
      theme(panel.grid.minor=element_blank(),
            panel.grid.major=element_blank(),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 11),
            #axis.text.x = element_text(angle = 90),
            #legend.position='right', #c(.9,.9) # 'none'
            legend.title = element_text(size=12),
            legend.text = element_text(size = 11)) +
      scale_x_continuous(limits=c(-1.5,5.5), breaks=c(-1,0,1,2,3,4,5)) +  
      scale_y_continuous(limits=c(-1200,1200), breaks=c(-1000,-500,0,500,1000)) +  
      facet_grid(.~train_type) 
    
    # fast and slow lines by condition:    
      # for fast and slow by condition, use avg_rlims_c. 
      avg_rlims_c <- indivs %>%
        group_by(train_type) %>%
        summarize(fast_avg = mean(fast_rtempo,na.rm=T),
                  slow_avg = mean(slow_rtempo,na.rm=T))
      
      # add vlines to plot: 
      b <- b + geom_vline(aes(xintercept=fast_avg), data=avg_rlims_c, linetype=2, alpha=.95)
      b <- b + geom_vline(aes(xintercept=slow_avg), data=avg_rlims_c, linetype=2, alpha=.95)
    
    # save the plot!
    ggsave("rtempo_bias_2016-11-09.png", width=6.5, height=3.25, dpi=100)


  # *(3) drift by condition (instead of bias)
    
    d <- ind_drift %>%
      ggplot(aes(x=(seed_int-smt_avg)/smt_avg, y=drift)) +
      geom_point(aes(color=seed), size = 3) + 
      geom_line(aes(group=cohort), alpha = .5, lwd = 1, color = "#999999") +
      geom_smooth(aes(group=train_type), method = lm, se = FALSE, lwd=1.5, alpha=.75, color='black') +
      geom_hline(aes(yintercept=0),color="#999999") + # no drift line
      geom_vline(aes(xintercept=0),linetype=2, alpha=.95) + # stim = smt line
      theme_bw() +
      labs(title = '(B) Drift by Relative Seed Tempo',
           x = 'Relative Seed Tempo', 
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
      scale_x_continuous(limits=c(-1.5,5.5), breaks=c(-1,0,1,2,3,4,5)) +  
      scale_y_continuous(limits=c(-1500,3500), breaks=c(-1000,0,1000,2000,3000)) +  
      facet_grid(.~train_type) 
    
    # fast and slow lines by condition:
      
      # for fast and slow by condition, use avg_rlims_c. 
      avg_rlims_c <- indivs %>%
        group_by(train_type) %>%
        summarize(fast_avg = mean(fast_rtempo,na.rm=T),
                  slow_avg = mean(slow_rtempo,na.rm=T))
      
      # add vlines to plot: 
      d <- d + geom_vline(aes(xintercept=fast_avg), data=avg_rlims_c, linetype=2, alpha=.95)
      d <- d + geom_vline(aes(xintercept=slow_avg), data=avg_rlims_c, linetype=2, alpha=.95)
      
    # save the plot!
    ggsave("rtempo_drift_2016-11-20.png", width=6.5, height=3.25, dpi=100)


# MT-SR: plots 2 (separate participant: relative tempo spaghetti and drift) ------------------------
# BUT SEE PARTICIPANT PROFILE SCRIPT FOR BETTER VERSION...

  # (1) spaghetti with relative tempo by participant
  # save dim 400 x 734
  indivs %>%
    mutate(seed = as.factor(seed)) %>%
    ggplot(aes(x=generation, y=stim_rtempo, group=chain_name, color=seed)) +
    # re do line by participant...if facet that way...
    geom_line(alpha = .75, lwd = 1.5) + #, color = "#999999"
    #geom_smooth(aes(group=seed, color=seed), lwd = 1.5, alpha = .75, se = F) +
    geom_hline(yintercept=0) +
    theme_bw() +
    labs(title = '(A) Relative Tempo Chains',
         x = 'Generation', 
         y = 'Relative Tempo') +
    facet_grid(cohort~train_type) 
  #facet_wrap(~participant)
  
  # xx (2) bias # maybe add arrows for generation? or add one big line/arrow for seed to gen15?
  # save dim 400 x 734
  indivs %>%
    ggplot(aes(x=relative_tempo, y=bias, color = generation)) +
    geom_point(aes(color = generation), size = 3, shape = 1) + 
    geom_smooth(aes(group = participant), method = lm, se = FALSE, lwd=1, alpha=.75, color='black') +
    scale_colour_gradientn(colours = rainbow(n = 4, start = .5, end = 1)) +
    geom_hline(aes(yintercept=0),color="#999999") +
    geom_vline(aes(xintercept=1),color="#999999") +
    theme_bw() +
    coord_cartesian(xlim=c(0,6), ylim=c(-800,2000)) +
    labs(title = '(B) Bias',
         x = 'Stimulus/SMT', #expression(x[i]/smt[i+1])
         y =  'Reproduced - Stimulus tempo (ms)') + #expression(x[i+1] - x[i]))
    facet_grid(cohort~train_type)
  #facet_wrap(~participant)




##### *MT-SR: lm DRIFT against relative seed tempo: lm fit (by participant) -------
# for each participant, drift ~ seed_rtempo

    data <- ind_drift
    conds <- unique(data$participant)
    
    nrowOutput <- length(conds)
    ncolOutput <- 11
    output <- data.frame(matrix(ncol = ncolOutput, nrow = nrowOutput))
    cnames <- c("participant", "slope", "slope_p", "y_int", "y_int_p",
                "F_val", "F_numdf", "F_dendf", "F_sig", "rsq", "x_int")
    
    colnames(output) <- cnames
    
    
    n = 1
    
    for (j in 1:length(conds)) {
      
      lm_out <- lm(drift ~ seed_rtempo, 
                   data = subset(data,participant == conds[j]))
      
      output$participant[n] <- as.character(conds[j]) 
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
    write.csv(output,"tables/lm_drift_rtempo_byparticipant_2016-11-09.csv")

    # also save combo df with drift info:
    drift_fits <- output %>%
      mutate(participant = as.numeric(participant),
             train_type = as.factor(ifelse(participant < 1000,"synchronize",
                                           ifelse(participant > 1000, "observe", NA))),
             participant = as.factor(participant)) %>%
      full_join(ind_drift, by = c("train_type", "participant"))
    
    write.csv(drift_fits, "tables/rtempo_drift_fits_byP_2016-11-22.csv")


# MT-SR: lm DRIFT against relative seed tempo: lm fit (by condition) -------
# start with ... ind_drift
    
    data <- ind_drift
    conds <- unique(data$train_type)
    
    nrowOutput <- length(conds)
    ncolOutput <- 11
    output <- data.frame(matrix(ncol = ncolOutput, nrow = nrowOutput))
    cnames <- c("train_type", "slope", "slope_p", "y_int", "y_int_p",
                "F_val", "F_numdf", "F_dendf", "F_sig", "rsq", "x_int")
    
    colnames(output) <- cnames
    
    
    n = 1
    
    for (j in 1:length(conds)) {
      
      lm_out <- lm(drift ~ seed_rtempo, 
                   data = subset(data,train_type == conds[j]))
      
      output$train_type[n] <- as.character(conds[j]) 
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
    write.csv(output,"tables/lm_mt-sr_bycond_driftxrseed_2016-11-09.csv")


# MT-SR: DRIFT plots 1 (by condition) slope and x-intercept ----

  # plot slopes on a number line:
  drift_fits %>% 
  ggplot(aes(x = 0, y = slope, color=participant, label=factor(participant))) +
    geom_point(size=5,alpha=.5) +
    #geom_text(angle = 45, nudge_x = .025) + # meh maybe i should just have a legend...
    geom_vline(xintercept=0) + geom_hline(yintercept=0) +
    guides(color=FALSE) + 
    theme_bw() +
    theme(axis.ticks = element_blank(), axis.text.y = element_blank()) +
    facet_grid(~train_type) + 
    coord_flip()

  # plot x-intercepts with on a number line:
  drift_fits %>%  
    ggplot(aes(x = 0, y = x_int, color = as.factor(participant))) +
    geom_point(size=5,alpha=.5) +
    geom_vline(xintercept=0) +
    guides(color=FALSE) + 
    facet_grid(~train_type) + theme_bw() +
    coord_flip()

  # plot x-intercepts with the linear fit:
  p <- drift_fits %>%
    ggplot(aes(x=seed_rtempo, y=drift, group=cohort)) + 
    geom_point() + geom_line(color='grey') + #for point: aes(color=seed)
    geom_smooth(aes(color=cohort),method=lm, se=F) +
    facet_grid(~train_type)
    
  p + geom_point(aes(x=x_int, y=0))


# alt: plot slopes with box plot:
  output %>%
    mutate(participant = as.numeric(participant),
           train_type = as.factor(ifelse(participant < 1000,"synchronize",
                                         ifelse(participant > 1000, "observe", NA))),
           CE = 1) %>%
    ggplot(aes(x = CE, y = slope)) + 
    geom_boxplot() + 
    geom_point(aes(color = as.factor(participant)), size=5,alpha=.5) + guides(color=FALSE)+
    facet_grid(~train_type) +
    coord_flip()


# *MT-SR: DRIFT plots 2 (by condition) slope, xints, mts, & music years ----
# alt df, read in: party <- read.csv("tables/indivs_mt_sr_byP_2016-11-09.csv") %>% rename(slope=CE)
# for condition slope and intercept: cond_fits <- read.csv("tables/lm_mt-sr_bycond_driftxrseed_2016-11-09.csv") %>% select(train_type,cond_slope=slope, cond_x=x_int)
    
  # add cohort. convert music years and strategy into yes/no factors for plotting:
    info <- drift_fits %>%
      select(train_type,participant,cohort)
    
    info <- unique(info)
    write.csv(info,"tables/info.csv")
    # alt readin: info <- read.csv("tables/info.csv") %>% mutate(participant=as.factor(participant)) %>% select(-X)
      
    party_plot <- party %>%
      mutate(participant=as.factor(participant),
             music_years=as.factor(ifelse(is.na(music_years),"no",
                                          ifelse(music_years > 0, "yes"))),
             strategy=as.factor(strategy)) %>%
      left_join(info, by=c("train_type","participant")) %>%
      left_join(cond_fits, by="train_type")
    
    levels(party_plot$strategy) <- c("no", "yes")


  # slopes and NPR and music years    
  pp <- party_plot %>%
      ggplot(aes(x=npr, y=slope)) +
      geom_point(aes(color=music_years, shape=strategy), size = 3) + 
      geom_text(aes(label=participant), vjust = 0, nudge_y = -100) +
      geom_hline(aes(yintercept=0),color="#999999") + # degenerate slope line
      theme_bw() +
      labs(title = '(A) Slope & Tempo Limits',
           x = 'Relative Tempo-Limits Range', 
           y = 'Drift Slope') +
      theme(panel.grid.minor=element_blank(),
            panel.grid.major=element_blank(),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 11),
            #axis.text.x = element_text(angle = 90),
            #legend.position='right', #c(.9,.9) # 'none'
            legend.title = element_text(size=12),
            legend.text = element_text(size = 11)) +
      scale_color_discrete(name="Music\nTraining") +
      scale_shape_discrete(name="Strategy") +
      facet_grid(.~train_type)
  
    # kind of but not quite right for adding CONDITION fit:
    pp + geom_point(aes(x=npr, y=cond_slope))   

    # save the plot!
    ggsave("rtempo_slopes_mt_music_strats_2016-11-09.png", width=6.5, height=3.25, dpi=100)
    
    
    # x-int and NPR and music years
    party_plot %>%
      ggplot(aes(x=npr, y=attractor, color=music_years, shape=strategy)) +
      geom_point(size = 3) + 
      geom_text(aes(label=participant), vjust = 0, nudge_y = -2) +
      geom_hline(aes(yintercept=0),color="#999999") + # 0-drift tempo
      theme_bw() +
      labs(title = '(B) Intercept & Tempo Limits',
           x = 'Relative Tempo-Limits Range', 
           y = 'Relative Equilibrium Tempo') +
      theme(panel.grid.minor=element_blank(),
            panel.grid.major=element_blank(),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 11),
            #axis.text.x = element_text(angle = 90),
            #legend.position='right', #c(.9,.9) # 'none'
            legend.title = element_text(size=12),
            legend.text = element_text(size = 11)) +
      scale_color_discrete(name="Music\nTraining") +
      scale_shape_discrete(name="Strategy") +
      facet_grid(.~train_type)
    
    # save the plot!
    ggsave("rtempo_xints_mt_music_strat_2016-11-09.png", width=6.5, height=3.25, dpi=100)


  # ***best so far: an almost final plot...when people have negative slope, is the attractor 0?
  # color coding by NPR, still including music training (dropping strats)
  pp <- party_plot %>%
    ggplot(aes(x=slope, y=attractor, color=npr, shape=music_years)) + # move color shape to point to un-color labels
    geom_point(size = 3) + 
    geom_text(aes(label=participant), vjust = 0, nudge_y = -2) +
    geom_hline(aes(yintercept=0),color="#999999") + # 0-drift tempo
    geom_vline(aes(xintercept=0),color="#999999") +
    theme_bw() +
    labs(#title = 'Convergence Rate & Tempo',
         x = 'Slope', 
         y = 'X-Intercept') +
    theme(panel.grid.minor=element_blank(),
          panel.grid.major=element_blank(),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 11),
          #axis.text.x = element_text(angle = 90),
          #legend.position='right', #c(.9,.9) # 'none'
          legend.title = element_text(size=12),
          legend.text = element_text(size = 11)) +
    scale_colour_gradientn(colours=rainbow(4), name="NPR") +
    #scale_color_discrete(name="Music\nTraining") +
    scale_x_continuous(limits=c(-600,600), breaks=c(-500,-250,0,250,500)) +  
    scale_y_continuous(limits=c(-13.5,13.5), breaks=c(-12,-6,0,6,12)) +
    scale_shape_discrete(name="Music\nTraining") +
    facet_grid(.~train_type)
  
  pp + geom_point(aes(x=cond_slope, y=cond_x), size=3, shape=1, color="black")

  # save:
  ggsave("rtempo_slopes_xint_centered_2016-11-20.png", width=6.5, height=3.25, dpi=100)


    # ***a 2nd final plot...when people have negative slope, is the attractor 0?
    # color coding by SMT CONSISTENCY, still including music training (dropping strats)
    pp <- party_plot %>%
      ggplot(aes(x=slope, y=attractor, color=smt_consistency, shape=music_years)) + # move color shape to point to un-color labels
      geom_point(size = 3) + 
      geom_text(aes(label=participant), vjust = 0, nudge_y = -2) +
      geom_hline(aes(yintercept=0),color="#999999") + # 0-drift tempo
      geom_vline(aes(xintercept=0),color="#999999") +
      theme_bw() +
      labs(#title = 'Slope & X-Intercept',
        x = 'Slope', 
        y = 'X-Intercept') +
      theme(panel.grid.minor=element_blank(),
            panel.grid.major=element_blank(),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 11),
            #axis.text.x = element_text(angle = 90),
            #legend.position='right', #c(.9,.9) # 'none'
            legend.title = element_text(size=12),
            legend.text = element_text(size = 11)) +
      scale_colour_gradientn(colours=rainbow(4), name="SMT\ninconsistency") +
      #scale_color_discrete(name="Music\nTraining") +
      scale_shape_discrete(name="Music\nTraining") +
      facet_grid(.~train_type)
    
    pp + geom_point(aes(x=cond_slope, y=cond_x), size=3, shape=1, color="black")
    
  # save:
  ggsave("rtempo_slopes_xint_consistency_2016-11-09.png", width=6.5, height=3.25, dpi=100)


# ***BEST (11-22-16): a 3rd final plot...
# color coding by NPR, yes/no for pvalue of slope < .01

pdrift <- drift_fits %>%
  mutate(slope_sig = ifelse(slope_p < 0.01, "yes", "no"))

pp <- pdrift %>%
  ggplot(aes(x=slope, y=x_int, color=npr_avg, shape=slope_sig)) + # move color shape to point to un-color labels
  geom_point(size = 3) + 
  geom_text(aes(label=cohort), vjust = 0, nudge_y = -2) +
  geom_hline(aes(yintercept=0),color="#999999") + # 0-drift tempo
  geom_vline(aes(xintercept=0),color="#999999") +
  theme_bw() +
  labs(#title = 'Slope & X-Intercept',
    x = 'Slope', 
    y = 'X-Intercept') +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 11),
        #axis.text.x = element_text(angle = 90),
        #legend.position='right', #c(.9,.9) # 'none'
        legend.title = element_text(size=12),
        legend.text = element_text(size = 11)) +
  scale_colour_gradientn(colours=rainbow(4), name="NPR") +
  #scale_color_discrete(name="Music\nTraining") +
  scale_shape_discrete(name=(expression(slope!=0))) + #~p < .01
  scale_x_continuous(limits=c(-600,600), breaks=c(-500,-250,0,250,500)) +  
  scale_y_continuous(limits=c(-13.5,13.5), breaks=c(-12,-6,0,6,12)) +
  facet_grid(.~train_type)

pp + geom_point(data=cond_fits, aes(x=cond_slope, y=cond_x), size=3, shape=1, color="black")

# save:
ggsave("rtempo_slopes_xint_pval_2016-11-20.png", width=6.5, height=3.25, dpi=100)



  # older if revert to no music or strat...
  # just slopes and NPR
  drift_fits %>%
    ggplot(aes(x=npr_avg, y=slope)) +
    geom_point(size = 3) + 
    geom_text(aes(label=participant), vjust = 0, nudge_y = -100) +
    geom_hline(aes(yintercept=0),color="#999999") + # degenerate slope line
    theme_bw() +
    labs(title = '(A) Slopes by Relative Tempo Limits Range',
         x = 'Relative Tempo-Limits Range', 
         y = 'Drift Slope') +
    theme(panel.grid.minor=element_blank(),
          panel.grid.major=element_blank(),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 11),
          #axis.text.x = element_text(angle = 90),
          #legend.position='right', #c(.9,.9) # 'none'
          legend.title = element_text(size=12),
          legend.text = element_text(size = 11)) +
    scale_color_discrete(name="Participant") +
    facet_grid(.~train_type)
  
  # save the plot!
  ggsave("rtempo_slopes_mt_2016-11-09.png", width=6.5, height=3.25, dpi=100)
  


# MT-SR: DRIFT plots 3 - other drift param + survey explorations ----
# alt df, read in: party <- read.csv("tables/indivs_mt_sr_byP_2016-11-09.csv") %>% rename(slope=CE)
  
  
  # x-intercepts...less interesting than slopes?
  drift_fits %>%
    ggplot(aes(x=npr_avg, y=x_int)) + # or x=smt_avg?
    geom_point() + facet_grid(~train_type)
  
  
  # no apparent relatinoship between music years and slope
  # BUT HAVING MUSIC YEARS MEANS ATTRACTOR = 0
  party %>%
    mutate(music_years=ifelse(is.na(music_years),0,music_years)) %>%
    ggplot(aes(x=music_years, y=attractor, group=train_type)) + geom_point()
  
  party %>%
    mutate(music_years=as.factor(ifelse(is.na(music_years),"no",
                                        ifelse(music_years > 0, "yes")))) %>%
    ggplot(aes(x=music_years, y=attractor, group=train_type, color=train_type)) + 
    geom_point(aes(), size=3)
  
  
  # no apparent relatinoship between rhythm rating and slope
  # BUT MAYBE MORE RHYTHM = MORE 0 ATTRACTOR (faster) OR NEGATIVE
  party %>%
    ggplot(aes(x=rhythm, y=attractor, group=train_type)) + geom_point()
  
  
  # no apparent relationshipo between age and slope or smt (or attractor (aka xint))
  party %>%
    ggplot(aes(x=age, y=smt)) + geom_point(size=3) +
    geom_point(aes(x=age, y=slope, group=train_type, color=train_type),size=3)
  
  party %>%
    ggplot(aes(x=age, y=attractor, group=train_type, color=train_type)) + geom_point(size=3) +
    geom_hline(yintercept=0)

# *MT-SR: plots 4 - spaghetti and drift separated by participant ----
# shortcut????: drift_fits <- read.csv("tables/lm_drift_rtempo_byparticipant_2016-11-09.csv")
#   or use shortcuts for ind_drift and indivs, then run lm fits drift ~ rtempo by particpant above
    
  # *(1) spaghetti with relative tempo by participant 
    p <- indivs %>%
      ggplot(aes(x=stimulus_generation, y=stim_rtempo, group=chain_name, color=seed)) +
      geom_line(alpha = .75, lwd = 1.5) +
      #geom_smooth(aes(group=seed, color=seed), lwd = 1.5, alpha = .75, se = F) +
      #geom_hline(yintercept=0, linetype=2, alpha=.95) +
      theme_bw() +
      labs(title = '(A) Relative Tempo Chains for each participant',
           x = 'Iteration', 
           y = 'Relative Tempo') +
      theme(panel.grid.minor=element_blank(),
            panel.grid.major=element_blank(),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 11),
            #axis.text.x = element_text(angle = 90),
            #legend.position='right', #c(.9,.9) # 'none'
            legend.title = element_text(size=12),
            legend.text = element_text(size = 11)) +
      scale_color_discrete(name="Seed\nTempo (ms)") +
      scale_x_continuous(limits=c(-1,16), breaks=c(0,5,10,15)) +  
      scale_y_continuous(limits=c(-1.5,5.5), breaks=c(-1,0,1,2,3,4,5)) +  
      facet_grid(cohort~train_type) 
    
    
    # for fast and slow by train type x cohort, use avg_rlims_c. 
    avg_rlims_c <- indivs %>%
      group_by(train_type, cohort) %>%
      summarize(smt_avg = mean((smt_avg-smt_avg)/smt_avg),
                fast_avg = mean(fast_rtempo,na.rm=T),
                slow_avg = mean(slow_rtempo,na.rm=T))
    
    # add hlines to plot: 
    p <- p + geom_hline(aes(yintercept=smt_avg), data=avg_rlims_c, linetype=2, alpha=.95)
    p <- p + geom_hline(aes(yintercept=fast_avg), data=avg_rlims_c, linetype=2, alpha=.95)
    p <- p + geom_hline(aes(yintercept=slow_avg), data=avg_rlims_c, linetype=2, alpha=.95)
    
    
    # save the plot!
    ggsave("rtempo_spag_eachP_2016-11-20.png", width=6.5, height=11, dpi=100)


  # (2) drift plots for each participant with relative limits marked
  # to add regression equation...source(stat_smooth_func) before running and uncomment that line
      d <- drift_fits %>%
        ggplot(aes(x=seed_rtempo, y=drift)) +
        geom_point(aes(color=seed), size = 3) + 
        geom_line(aes(group=cohort), alpha = .5, lwd = 1, color = "#999999") +
        #stat_smooth_func(geom="text",method="lm",hjust=0,vjust=0,parse=TRUE) +
        geom_smooth(aes(group=train_type), method = lm, se = FALSE, lwd=1, alpha=.75, color='black') +
        geom_hline(aes(yintercept=0),color="#999999") + # no drift line
        geom_vline(aes(xintercept=0),linetype=2, alpha=.95) + # stim = smt line
        theme_bw() +
        labs(title = '(B) Drift for each participant',
             x = 'Relative Seed Tempo', 
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
        scale_x_continuous(limits=c(-1.5,5.5), breaks=c(-1,0,1,2,3,4,5)) +  
        scale_y_continuous(limits=c(-1500,3500), breaks=c(-1000,0,1000,2000,3000)) +  
        facet_grid(cohort~train_type) 
      
      # fast and slow lines by PARTICIPANT:
      # use avg_rlims_c from above

      # add vlines to plot: 
      d <- d + geom_vline(aes(xintercept=fast_avg, group=cohort), data=avg_rlims_c, linetype=2, alpha=.95)
      d <- d + geom_vline(aes(xintercept=slow_avg, group=cohort), data=avg_rlims_c, linetype=2, alpha=.95)
      

      # save the plot!
      ggsave("rtempo_drift_eachP_2016-11-20.png", width=6.5, height=11, dpi=100)


# original code to add regression eqtn to a plot (in progress): ----
    # alternative: stat_smooth_func -- custom code from the internet for this task (works with facets and groups)
    # saved in exp 3 folder.
    
    # replace "m" with lm_out or whatever has linear parameters in it:
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                     
                     list(        a = format(coef(m)[1], digits = 4),
                                  
                                  b = format(coef(m)[2], digits = 4),
                                  
                                  r2 = format(summary(m)$r.squared, digits = 3)))
    
    dftext <- data.frame(x = 70, y = 50, eq = as.character(as.expression(eq)))
    d + geom_text(aes(label = eq), data = dftext, parse = TRUE)
    
    
##### !individual bias against ratio: lm fit (by condition) -------
# for each condition, bias ~ stimulus_tempo:smt

data <- indivs
conds <- unique(data$train_type)

nrowOutput <- length(conds)
ncolOutput <- 11
output <- data.frame(matrix(ncol = ncolOutput, nrow = nrowOutput))
cnames <- c("train_type", "slope", "slope_p", "y_int", "y_int_p",
            "F_val", "F_numdf", "F_dendf", "F_sig", "rsq", "x_int")

colnames(output) <- cnames


n = 1

for (j in 1:length(conds)) {
  
  lm_out <- lm(bias ~ relative_tempo, 
               data = subset(data,train_type == conds[j]))
  
  output$train_type[n] <- as.character(conds[j]) 
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
write.csv(output,"tables/lm_bias_relativetempo_bycond_2016-11-08.csv")


##### !individual bias: lm fit -------
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

# !MT, SR, and SURVEY info -----
# use participant summary for MT (mt_parti)

# also try:
party <- read.csv("tables/indivs_mt_sr_byP_2016-11-09.csv") 

# currently: basic demo + music, dance, sleep, a few others.
survey <- read.csv(survey_input) %>% 
  filter(subject != 6600) %>%
  rename(participant = subject) %>%
  mutate(participant = as.factor(participant),
         cohort = as.factor(cohort)) %>%
  select(-X) 

# just smt right now...
mt_plus_survey <- survey %>%
  left_join(smt_parti, by = c("train_type", "participant", "cohort"))

ggplot(mt_plus_survey, aes(x=age,y=avg_tempo),color=train_type) +
  geom_point(size=2)




# REVISION EP - SR: linear fits - drift x seed - by condition -------
# adding SE for slopes
# start with compare df from earlier

  data <- compare
  conds <- unique(data$train_type)
  
  nrowOutput <- length(conds)
  ncolOutput <- 12
  output <- data.frame(matrix(ncol = ncolOutput, nrow = nrowOutput))
  cnames <- c("train_type", "slope", "slope_se", "slope_p", "y_int", "y_int_p",
              "F_val", "F_numdf", "F_dendf", "F_sig", "rsq", "x_int")
  
  colnames(output) <- cnames
  
  
  n = 1
  
  for (j in 1:length(conds)) {
    
    lm_out <- lm(drift ~ seed_int, 
                 data = subset(data,train_type == conds[j]))
    
    output$train_type[n] <- as.character(conds[j]) 
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
  write.csv(output,"tables/lm_sr_bycond_driftxseed_withSE_2016-12-06.csv")

# ***REVISIONS 2015-12-06 -- SLOPE confidence intervals -------
  # see Exp 2 script for more detail and other options
  
  
  # bookmarked useful web page: http://stattrek.com/regression/slope-confidence-interval.aspx?Tutorial=AP
  # also consider (future) Ng & Wilcox, 2010 comparing the regression slopes of independent groups -- using bootstrap estimates of SE to construct CIs
  # start with output from lm fit for drift~seed
  
    # output <- read.csv("tables/lm_sr_bycond_driftxseed_withSE_2016-12-06.csv") %>% select(-X)

  level = .99 # or .95
  
  slope_CIs <- output %>%
    select(train_type, slope, slope_se) %>%
    mutate(n = ifelse(train_type == "observe", 35,
                      ifelse(train_type == "synchronize", 49,NA)),
           level = level,
           alpha = 1 - level,
           crit_p = 1 - alpha/2,
           df = n - 2,
           crit_t = abs(qt(crit_p,df)),  # tvalues in R: http://stackoverflow.com/questions/11526041/critical-t-values-in-r
           me = crit_t*slope_se, # margin of error
           lowerCI = slope - me,
           upperCI = slope + me)
  # did not but could check these against an online calculator to verify that i did it correctly: http://www.danielsoper.com/statcalc/calculator.aspx?id=26
  # did this for exp 2
  
  write.csv(slope_CIs,"tables/lm_driftseed_slope99CIs_2016-12-06.csv") # 99% CI
  #write.csv(slope_CIs,"tables/lm_driftseed_slope95CIs_2016-12-06.csv") # 95% CI
  

# REVISIONS 2015-12-06 -- SLOPE t-tests -------
# issue: different dfs? soper online calculator accommodates this -- leads to similar results, anyway
# Soper, D.S. (2016). Significance of the Difference between Two Slopes Calculator [Software]. Available from http://www.danielsoper.com/statcalc
# but generally my degrees of freedom are off for the t-tests...should be (n1-2) + (n2-2)

    # holding df:
    nrow_pw <- 1 
    ncol_pw <- 8 
    
    pairwise_tests <- data.frame(matrix(ncol=ncol_pw, nrow=nrow_pw))
    cnames <- c("conds_compared", "t_val", "p_val", "df", "slope_1", "slope_2", "se_1", "se_2")
    colnames(pairwise_tests) <- cnames
    
    pairwise_tests[,1] <- c("aud_obs-vs-sync")
    
    ## the tests (and add to holding df)
    
    # within auditory, is sync vs obs effect different? 
    slope_1 <- output$slope[output$train_type == "observe"]
    se_1 <- output$slope_se[output$train_type == "observe"]
    
    slope_2 <- output$slope[output$train_type == "synchronize"]
    se_2 <- output$slope_se[output$train_type == "synchronize"]
    
    df = 80 # each sample size minus 2
    t_val <- (slope_1 - slope_2) / sqrt(se_1^2 + se_2^2)
    p_val = 2*pt(abs(t_val), df, lower=FALSE) # 2-tailed t-test. t(33) = 1.5561, p = .12922
    # TO DO verified result against http://www.danielsoper.com/statcalc/calculator.aspx?id=103 -- pvalues slightly different...but significance pattern pretty much the same
    
    pairwise_tests$t_val[pairwise_tests$conds_compared == "aud_obs-vs-sync"] = t_val
    pairwise_tests$p_val[pairwise_tests$conds_compared == "aud_obs-vs-sync"] = p_val
    pairwise_tests$df[pairwise_tests$conds_compared == "aud_obs-vs-sync"] = df
    pairwise_tests$slope_1[pairwise_tests$conds_compared == "aud_obs-vs-sync"] = slope_1
    pairwise_tests$se_1[pairwise_tests$conds_compared == "aud_obs-vs-sync"] = se_1
    pairwise_tests$slope_2[pairwise_tests$conds_compared == "aud_obs-vs-sync"] = slope_2
    pairwise_tests$se_2[pairwise_tests$conds_compared == "aud_obs-vs-sync"] = se_2
    
    write.csv(pairwise_tests, "tables/pairwise_tests_driftbyseed_2016-12-06.csv")

# REVISIONS 2015-12-06 -- SLOPE t-tests - compare Exp 2 & Exp 3 -------

  # read in data from each experiment:
    exp2 <- read.csv("C:/Users/Ellie/Google Drive/experiments & projects/iterated tapping - tempo/ANALYSIS - 10-2016 (diss)/Experiment 2 (IT02 -- lab iterations)/tables/lm_am_sr_bycond_driftxseed_plusSE_2016-12-06.csv") %>% select(-X)
    exp3 <- read.csv("tables/lm_sr_bycond_driftxseed_withSE_2016-12-06.csv") %>% select(-X)


  # holding df:
    nrow_pw <- 2
    ncol_pw <- 8 
    
    pairwise_tests <- data.frame(matrix(ncol=ncol_pw, nrow=nrow_pw))
    cnames <- c("conds_compared", "t_val", "p_val", "df", "slope_1", "slope_2", "se_1", "se_2")
    colnames(pairwise_tests) <- cnames
    
    pairwise_tests[,1] <- c("aud-obs_2vs3", "aud-syn_2vs3")
  
  
  ## the tests (and add to holding df)
  
  # for auditory observe, is effect different in exp 2 and exp 3?
    slope_1 <- exp2$slope[exp2$exp_name == "a_or"]
    se_1 <- exp2$slope_se[exp2$exp_name == "a_or"]
    
    slope_2 <- exp3$slope[exp3$train_type == "observe"]
    se_2 <- exp3$slope_se[exp3$train_type == "observe"]
    
    df = 66 # 35-2 + 35-2
    t_val <- (slope_1 - slope_2) / sqrt(se_1^2 + se_2^2)
    p_val = 2*pt(abs(t_val), df, lower=FALSE) # 2-tailed t-test. 
    # TO DO verified result against http://www.danielsoper.com/statcalc/calculator.aspx?id=103 -- pvalues slightly different...but significance pattern pretty much the same
    
    pairwise_tests$t_val[pairwise_tests$conds_compared == "aud-obs_2vs3"] = t_val
    pairwise_tests$p_val[pairwise_tests$conds_compared == "aud-obs_2vs3"] = p_val
    pairwise_tests$df[pairwise_tests$conds_compared == "aud-obs_2vs3"] = df
    pairwise_tests$slope_1[pairwise_tests$conds_compared == "aud-obs_2vs3"] = slope_1
    pairwise_tests$se_1[pairwise_tests$conds_compared == "aud-obs_2vs3"] = se_1
    pairwise_tests$slope_2[pairwise_tests$conds_compared == "aud-obs_2vs3"] = slope_2
    pairwise_tests$se_2[pairwise_tests$conds_compared == "aud-obs_2vs3"] = se_2
  
  # for auditory synchronize, is effect different in exp 2 and exp 3?
    slope_1 <- exp2$slope[exp2$exp_name == "a_sc"]
    se_1 <- exp2$slope_se[exp2$exp_name == "a_sc"]
    
    slope_2 <- exp3$slope[exp3$train_type == "synchronize"]
    se_2 <- exp3$slope_se[exp3$train_type == "synchronize"]
    
    df = 80 # 35-2 + 49-2
    t_val <- (slope_1 - slope_2) / sqrt(se_1^2 + se_2^2)
    p_val = 2*pt(abs(t_val), df, lower=FALSE) # 2-tailed t-test. 
    # TO DO verified result against http://www.danielsoper.com/statcalc/calculator.aspx?id=103 -- pvalues slightly different...but significance pattern pretty much the same
    
    pairwise_tests$t_val[pairwise_tests$conds_compared == "aud-syn_2vs3"] = t_val
    pairwise_tests$p_val[pairwise_tests$conds_compared == "aud-syn_2vs3"] = p_val
    pairwise_tests$df[pairwise_tests$conds_compared == "aud-syn_2vs3"] = df
    pairwise_tests$slope_1[pairwise_tests$conds_compared == "aud-syn_2vs3"] = slope_1
    pairwise_tests$se_1[pairwise_tests$conds_compared == "aud-syn_2vs3"] = se_1
    pairwise_tests$slope_2[pairwise_tests$conds_compared == "aud-syn_2vs3"] = slope_2
    pairwise_tests$se_2[pairwise_tests$conds_compared == "aud-syn_2vs3"] = se_2
  
  
  write.csv(pairwise_tests, "tables/compare_slopes_exps2&3_2016-12-06.csv")

# REVISIONS: stat tests to compare pearson r correlation between exp 2 and 3 -----

  # functions currently in exp 2 folder - copy paste to source
    source("compare_correlations_functions.R")


  # data stuff
    exp2 <- exp2 %>% 
      mutate(r = sqrt(rsq),
             n = 35)
    
    exp3 <- exp3 %>%
      mutate(r = sqrt(rsq),
             n = ifelse(train_type == "observe",35,
                        ifelse(train_type == "synchronize",49,NA)))


  # tests
    # compare OBSERVE -- exp 2 vs exp 3
    observe <- r.ind.ci(exp2$r[exp2$exp_name=="a_or"],exp3$r[exp3$train_type=="observe"],
                       exp2$n[exp2$exp_name=="a_or"],exp3$n[exp3$train_type=="observe"])
    
    # compare SYNC -- exp 2 vs exp 3
    synchronize <- r.ind.ci(exp2$r[exp2$exp_name=="a_sc"],exp3$r[exp3$train_type=="synchronize"],
                        exp2$n[exp2$exp_name=="a_sc"],exp3$n[exp3$train_type=="synchronize"])
    

  # combine & save data
    compare_exps <- rbind(observe, synchronize)
    colnames(compare_exps) <- c("low_ci", "hi_ci")
    
    write.csv(compare_exps, "tables/compare_r_exps2&3_2016-12-06.csv")


#inprog 2016-12-13 - whole chain fits #1

# final tempo by smt
ind_drift %>%
  ggplot(aes(x=smt_avg, y=final_tempo, color=seed, group=train_type)) +
  geom_point(size=3) + geom_smooth(method=lm) +
  facet_grid(~train_type)


# spaghetti -- what function fits these best?
indivs %>%
  ggplot(aes(x=generation, y=reproduced_tempo, color=seed, group=chain_name)) +
  geom_point(size=2) + geom_line() +
  #geom_smooth(aes(group=seed)) +
  facet_grid(cohort~train_type)

# some playing around with fits...
fit_chains <- lm(reproduced_tempo ~ generation + generation:train_type-1, 
                 data=indivs)
summary(fit_chains)

tmp <- (x$pred = exp(predict(fit_chains)) )

tmp2 <- exp(predict(fit_chains, interval = "confidence"))

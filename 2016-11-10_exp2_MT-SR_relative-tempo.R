# Load libraries 
library(stringr)
library(tidyr)
library(ggplot2)
library(plyr)

library(dplyr) # load dplyr after plyr 

##### 2016-11-10 MT-SR ratio : df re-composition -----------------
  
  # (1) shortcuts:
  
  # for spaghetti, ar1, and bias plots: 
  # 2240 rows = [2100 rows = (4conds*5cohorts*15gens*7 seeds)] + 140 seeds
  indivs <- read.csv("tables/indivs_mt_sr_bygen_2016-11-10.csv") %>%
    mutate(exp_name = as.factor(exp_name),
           mode = as.factor(mode),
           train_type = as.factor(train_type),
           cohort = as.factor(cohort),
           seed = as.factor(seed)) %>%
    select(-X)
  
  # for drift plots:
  # 140 rows = 1 for each chain
  ind_drift <- read.csv("tables/indivs_mt_sr_chains_2016-11-10.csv") %>%
    mutate(exp_name = as.factor(exp_name),
           mode = as.factor(mode),
           train_type = as.factor(train_type),
           cohort = as.factor(cohort),
           seed = as.factor(seed)) %>%
    select(-X)

  # (2) original df re-construction and saving: 
  # use mt_wide as MT df (mt_wide <- read.csv("tables/mt_wide_2016-10-31.csv") %>% select(-X))
  # use ar1 as SR df (ar1 <- read.csv("compiled data/ar1_2016-11-1.csv") %>% select(-X))
  # creates indivs df (2100 rows for each participant*chain) - ar1 & mt data combined, pluse relative tempo
  
    # (a) ind_chains df --- by generation
      # kind of just renaming things, plus making a participant id
      indivs_ar1 <- ar1 %>%
        mutate(stimulus_generation = generation,
               generation = next_gen,
               id = as.factor(paste(exp_name, cohort, generation, sep = "_")), # unique id for each participant
               stimulus_tempo = mean_tempo,
               reproduced_tempo = next_mean_tempo,
               bias = reproduced_tempo - stimulus_tempo,
               seed=as.factor(seed)) %>%
        #filter(generation !=16) %>%
        select(exp_name,mode,train_type,cohort,id, # shared with mt, plus generation
               seed,seed_int,chain_name,stimulus_generation,
               stimulus_tempo, generation, reproduced_tempo, bias)
      
#       #oops this is by participant/generation. i want by COHORT:
#       # making a matching participant id for mt df
#       indivs_mt <- mt_wide %>%
#         mutate(id = as.factor(paste(exp_name, cohort, generation, sep = "_")),
#                generation=as.integer(generation), # unique id for each participant
#                mode = as.factor(mode),
#                train_type = as.factor(train_type))
  
      # version with self-paced data by COHORT
      indivs_mt <- mt_wide %>%
        group_by(exp_name,mode,train_type,cohort) %>%
        summarize(smt_avg = mean(smt_mean, na.rm=T),
                  fast_avg = mean(fast_mean, na.rm=T),
                  slow_avg = mean(slow_mean, na.rm=T),
                  npr_avg = mean(norm_range, na.rm=T))

      # combining
      indivs <- indivs_ar1 %>%
        full_join(indivs_mt, by = c("exp_name", "mode", "train_type", "cohort")) %>%
        mutate(stim_rtempo = (stimulus_tempo - smt_avg)/smt_avg,
               fast_rtempo = (fast_avg - smt_avg)/smt_avg,
               slow_rtempo = (slow_avg - smt_avg)/smt_avg)

      
      write.csv(indivs,"tables/indivs_mt_sr_bygen_2016-11-10.csv")
      
  
      # (b) ind_drift df --- by chain (requires compare df)
      # load compare <- read.csv("compiled data/compare_mt_sr_2016-11-01.csv")
      # load mt_summary_bycohort <- read.csv("tables/mt_summary_bycohort_wide_2016-11-10.csv")
      
      ind_drift <- compare %>%
        full_join(mt_summary_bycohort, 
                  by = c("exp_name", "mode", "train_type", "cohort")) %>%
        mutate(seed_rtempo = (seed_int - smt_avg)/smt_avg,
               fast_rtempo = (fast_avg - smt_avg)/smt_avg,
               slow_rtempo = (slow_avg - smt_avg)/smt_avg)
      
      write.csv(ind_drift, "tables/indivs_mt_sr_chains_2016-11-10.csv")
  
  
  
##### 2016-11-10 MT-SR ratio : plots 1 (spaghetti and drift) -----------------
  
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
            plot.title = element_text(size=12),
            axis.title = element_text(size = 10),
            axis.text = element_text(size = 8),
            #axis.text.x = element_text(angle = 90),
            strip.text = element_text(size=8),
            #legend.position='right', #c(.9,.9) # 'none'
            legend.title = element_text(size=8),
            legend.text = element_text(size = 8)) +
      scale_color_discrete(name="Seed\nTempo (ms)") +
      scale_x_continuous(limits=c(-1,16), breaks=c(0,5,10,15)) +  
      scale_y_continuous(limits=c(-1.5,5.5), breaks=c(-1,0,1,2,3,4,5)) +  
      facet_grid(mode~train_type) 
    
    # choose fast and slow lines either (a) by condition or (b) overall: 
    
    # for fast and slow by condition, use avg_rlims_c. 
    avg_rlims_c <- indivs %>%
      group_by(exp_name, mode, train_type) %>%
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
    ggsave("rtempo_spag_big_2016-11-11.png", width=6.5, height=5.45, dpi=100) # change font sizes
    ggsave("rtempo_spag_small_2016-11-11.png", width=4, height=3.35, dpi=100)
  

  # *(2) drift by condition 
    
    d <- ind_drift %>%
      ggplot(aes(x=(seed_int-smt_avg)/smt_avg, y=drift)) +
      geom_point(aes(color=seed), size = 2) + 
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
            plot.title = element_text(size=12),
            axis.title = element_text(size = 10),
            axis.text = element_text(size = 8),
            #axis.text.x = element_text(angle = 90),
            strip.text = element_text(size=8),
            #legend.position='right', #c(.9,.9) # 'none'
            legend.title = element_text(size=8),
            legend.text = element_text(size = 8)) +
      scale_color_discrete(name="Seed\nTempo (ms)") +
      scale_x_continuous(limits=c(-1.5,2.5), breaks=c(-1,0,1,2)) +  
      scale_y_continuous(limits=c(-2000,2000), breaks=c(-1000,0,1000)) +  
      facet_grid(mode~train_type) 
    
    # fast and slow lines by condition:
    
    # for fast and slow by condition, use avg_rlims_c. 
    avg_rlims_c <- indivs %>%
      group_by(exp_name, mode, train_type) %>%
      summarize(fast_avg = mean(fast_rtempo,na.rm=T),
                slow_avg = mean(slow_rtempo,na.rm=T))
    
    # add vlines to plot: 
    d <- d + geom_vline(aes(xintercept=fast_avg), data=avg_rlims_c, linetype=2, alpha=.95)
    d <- d + geom_vline(aes(xintercept=slow_avg), data=avg_rlims_c, linetype=2, alpha=.95)
    
    # save the plot!
    ggsave("rtempo_drift_big_2016-11-20.png", width=6.5, height=5.45, dpi=100) # use 12/11 font size
    ggsave("rtempo_drift_small_2016-11-20.png", width=4.15, height=3.15, dpi=100) # use 10/8 font size
  
##### 2016-11-10 MT-SR ratio : lm fit by cond -----------------
# start with ... ind_drift
      
      data <- ind_drift
      conds <- unique(data$exp_name)
      
      nrowOutput <- length(conds)
      ncolOutput <- 11
      output <- data.frame(matrix(ncol = ncolOutput, nrow = nrowOutput))
      cnames <- c("exp_name", "slope", "slope_p", "y_int", "y_int_p",
                  "F_val", "F_numdf", "F_dendf", "F_sig", "rsq", "x_int")
      
      colnames(output) <- cnames
      
      
      n = 1
      
      for (j in 1:length(conds)) {
        
        lm_out <- lm(drift ~ seed_rtempo, 
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
      write.csv(output,"tables/lm_mt-sr_bycond_driftxrseed_2016-11-11.csv")
      
    
##### 2016-11-10 MT-SR ratio : lm drift fit by cohort -----------------
# for each cohort in exp (parallels participant in Exp 3), drift ~ seed_rtempo
    
  # need to add unique cohort column to pick out cohorts in each exp condition
    ind_drift <- ind_drift %>%
      mutate(cohort_unique = as.factor(paste(exp_name, cohort, sep = "_")))
  
  # linear fits
    data <- ind_drift
    conds <- unique(data$cohort_unique)
    
    nrowOutput <- length(conds)
    ncolOutput <- 11
    output <- data.frame(matrix(ncol = ncolOutput, nrow = nrowOutput))
    cnames <- c("cohort_unique", "slope", "slope_p", "y_int", "y_int_p",
                "F_val", "F_numdf", "F_dendf", "F_sig", "rsq", "x_int")
    
    colnames(output) <- cnames
    
    
    n = 1
    
    for (j in 1:length(conds)) {
      
      lm_out <- lm(drift ~ seed_rtempo, 
                   data = subset(data,cohort_unique == conds[j]))
      
      output$cohort_unique[n] <- as.character(conds[j]) 
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
    write.csv(output,"tables/lm_drift_rtempo_bycohort_2016-11-11.csv")
    
  # also save combo df with drift info:
    drift_fits <- output %>%
      full_join(ind_drift, by = c("cohort_unique"))
    
    write.csv(drift_fits, "tables/rtempo_drift_fits_bycohort_2016-11-11.csv")
    
##### 2016-11-10 MT-SR ratio : drift fit plots 2 (by condition) slope, xints, mts, & music years -----------------
# join 3 dfs by cohort for plots: MT, drift lm params (from relative tempo), and survey (NOT drift values)
#   20 rows = 1 per cohort
# plus 1 df by cond: drft lm params (by condition, from relative tempo)
    
  # (1) drift/relative tempo lm fit parameters by cohort
      # cohort_unique is only 'info' variable
      drift_lm_bycohort <- read.csv("tables/lm_drift_rtempo_bycohort_2016-11-11.csv") %>%
        select(cohort_unique, slope, x_int, slope_p, rsq) 
    
  # (2) motor tempo 
      mt_summary_bycohort <- read.csv("tables/mt_summary_bycohort_wide_2016-11-10.csv") %>%
        select(-X) %>%
        #create a cohort_unique for joining to drift lm df
        mutate(cohort_unique = as.factor(paste(exp_name, cohort, sep = "_")))
    
  # (3) survey basic info -- summarize over cohorts
      # 20 rows = 1 per cohort
        survey <- read.csv("b&s data/survey_ALL.csv") %>%
          mutate(cohort_unique = as.factor(paste(exp_name, cohort, sep = "_"))) %>%
          group_by(cohort_unique) %>%
          summarize(age_avg = mean(age, na.rm=T),
                    age_sd = sd(age, na.rm=T),
                    dance_years_avg = mean(dance_years, na.rm=T),
                    dance_years_sd = sd(dance_years, na.rm=T),
                    music_years_avg = mean(music_years, na.rm=T),
                    music_years_sd = sd(music_years, na.rm=T),
                    rhythm_avg = mean(rhythm))
      
  # (4)for condition slope and intercept: 
      # 4 rows = 1 per condition
      cond_fits <- read.csv("tables/lm_mt-sr_bycond_driftxrseed_2016-11-11.csv") %>% 
        select(exp_name,cond_slope=slope, cond_x=x_int)
      
  # COMBINE
    party <- mt_summary_bycohort %>%
      left_join(drift_lm_bycohort, by = "cohort_unique") %>%
      left_join(survey, by = "cohort_unique") %>%
      left_join(cond_fits, by = "exp_name", "mode", "train_type", "cohort")
  

  # ***when people have negative slope, is the attractor 0?
  # color coding by NPR (dropping strats)
  #   leaving out music_years_avg for now. can't plot continuous as shape (could add y/n for participant+bias plots)
  pp <- party %>%
    ggplot(aes(x=slope, y=x_int, color=npr_avg)) + # move color shape to point to un-color labels. shape=music_years
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
          plot.title = element_text(size=12),
          axis.title = element_text(size = 10),
          axis.text = element_text(size = 8),
          #axis.text.x = element_text(angle = 90),
          strip.text = element_text(size=8),
          #legend.position='right', #c(.9,.9) # 'none'
          legend.title = element_text(size=8),
          legend.text = element_text(size = 8)) +
    scale_colour_gradientn(colours=rainbow(4), name="NPR") +
    #scale_color_discrete(name="Music\nTraining") +
    scale_x_continuous(limits=c(-600,600), breaks=c(-500,-250,0,250,500)) +  
    scale_y_continuous(limits=c(-6.5,6.5), breaks=c(-5,0,5)) +
    facet_grid(mode~train_type)
  
  pp + geom_point(aes(x=cond_slope, y=cond_x), size=3, shape=1, color="black")

  # save:
  ggsave("rtempo_slopes_xint_2016-11-20.png", width=6.5, height=5.45, dpi=100)
##### MT-SR: lm bias by participant ----
# start with indivs df: bias ~ stim_rtempo
#  for each id ~ PARTICIPANTS (plus seeds)
    

    data <- indivs[indivs$generation!=16,] # subset to remove final stimulus generation for which there is no reproduction
    conds <- unique(data$id)
    
    nrowOutput <- length(conds)
    ncolOutput <- 11
    output <- data.frame(matrix(ncol = ncolOutput, nrow = nrowOutput))
    cnames <- c("participant", "slope", "slope_p", "y_int", "y_int_p",
                "F_val", "F_numdf", "F_dendf", "F_sig", "rsq", "x_int")
    
    colnames(output) <- cnames
    
    
    n = 1
    
    for (j in 1:length(conds)) {
      
      lm_out <- lm(bias ~ stim_rtempo, 
                   data = subset(data,id == conds[j]))
      
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
    write.csv(output,"tables/lm_bias_rtempo_byparticipant_2016-11-11.csv")
##### MT-SR: bias fit plots (by participant) w/survey ----
# join 3 dfs by participant for plots: MT, drift lm params (from relative tempo), and survey (NOT drift values)
#   300 rows = 1 per participant
#   plus 1 df by cond: drft lm params (by condition, from relative tempo)
    
  # (1) drift/relative tempo lm fit parameters by cohort
    drift_lm_byparticipant <- read.csv("tables/lm_bias_rtempo_byparticipant_2016-11-11.csv") %>%
      select(participant, slope, x_int, slope_p, rsq) 
    
  # (2) motor tempo 
    mt_wide <- read.csv("tables/mt_wide_2016-10-31.csv") %>%
      select(-X) %>%
      #create a participant for joining to drift lm df
      mutate(participant = as.factor(paste(exp_name, cohort, generation, sep = "_")))
    
  # (3) survey basic info -- summarize over cohorts
    # 300 rows = 1 per participant
    survey <- read.csv("b&s data/survey_ALL.csv") %>%
      # make unique ID for participants...
      mutate(participant = as.factor(paste(exp_name, cohort, generation, sep = "_"))) %>%
      select(-X, -exp_name, -mode, -train_type, -cohort, -generation, -id) # giving issues during combine
      
  # # (4)for condition slope and intercept: FOR DRIFT...UPDATE FOR BIAS?
    # # 4 rows = 1 per condition
    # cond_fits <- read.csv("tables/lm_mt-sr_bycond_driftxrseed_2016-11-11.csv") %>% 
    #   select(exp_name,cond_slope=slope, cond_x=x_int)
    
    # COMBINE & add music binary info
    party <- mt_wide %>%
      left_join(drift_lm_byparticipant, by = "participant") %>%
      left_join(survey, by = "participant") %>%
      #left_join(cond_fits, by = "exp_name", "mode", "train_type", "cohort") # leaving out - bias fits by cond or cohort not done
      mutate(music=as.factor(ifelse(is.na(music_years), "no", "yes"))) %>%
      rename(npr_avg = norm_range)

    
  # ***when people have negative slope, is the attractor 0?
  # color coding by NPR. music = y/n music traiing (dropping strats)
    
    pp <- party %>%
      ggplot(aes(x=slope, y=x_int, color=npr_avg, shape=music)) + # move color shape to point to un-color labels. 
      geom_point(size = 3, alpha=.5) + 
      #geom_text(aes(label=cohort), vjust = 0, nudge_y = -2) +
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
      scale_colour_gradientn(colours=rainbow(4), name="Tempo\nLimits\nRange") +
      #scale_color_discrete(name="Music\nTraining") +
      #scale_shape_discrete(name="Music\nTraining") +
      facet_grid(mode~train_type)
    
    #pp + geom_point(aes(x=cond_slope, y=cond_x), size=3, shape=1, color="black")
    
    # save:
    ggsave("rtempo_slopes_xint_biasByP_2016-11-11.png", width=6.5, height=5.45, dpi=100)
##### 2016-11-20 MT-SR ratio: drift fit plots 3 (by cohort - spaghetti & drift) -------
    
    # *(1) spaghetti with relative tempo by participant 
    p <- indivs %>%
      ggplot(aes(x=stimulus_generation, y=stim_rtempo, group=chain_name, color=seed)) +
      geom_line(alpha = .75, lwd = 1.5) +
      #geom_smooth(aes(group=seed, color=seed), lwd = 1.5, alpha = .75, se = F) +
      #geom_hline(yintercept=0, linetype=2, alpha=.95) +
      theme_bw() +
      labs(title = '(A) Relative Tempo Chains for each cohort',
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
      scale_color_discrete(name="Seed") +
      scale_x_continuous(limits=c(-1,16), breaks=c(0,5,10,15)) +  
      scale_y_continuous(limits=c(-1.5,5.5), breaks=c(-1,0,1,2,3,4,5)) +  
      facet_grid(mode~cohort~train_type) 
    
    
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
    ggsave("rtempo_spag_eachCohort_2016-11-20.png", width=6.5, height=11, dpi=100)
        
    
    # (2) drift plots for each participant with relative limits marked
    d <- ind_drift %>%
      ggplot(aes(x=seed_rtempo, y=drift)) +
      geom_point(aes(color=seed), size = 3) + 
      geom_line(aes(group=cohort), alpha = .5, lwd = 1, color = "#999999") +
      geom_smooth(aes(group=train_type), method = lm, se = FALSE, lwd=1, alpha=.75, color='black') +
      geom_hline(aes(yintercept=0),color="#999999") + # no drift line
      geom_vline(aes(xintercept=0),linetype=2, alpha=.95) + # stim = smt line
      theme_bw() +
      labs(title = '(B) Drift for each cohort',
           x = 'Relative Tempo', 
           y = 'Drift (ms)') +
      theme(panel.grid.minor=element_blank(),
            panel.grid.major=element_blank(),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 11),
            #axis.text.x = element_text(angle = 90),
            #legend.position='right', #c(.9,.9) # 'none'
            legend.title = element_text(size=12),
            legend.text = element_text(size = 11)) +
      scale_color_discrete(name="Seed") +
      scale_x_continuous(limits=c(-1.5,5.5), breaks=c(-1,0,1,2,3,4,5)) +  
      scale_y_continuous(limits=c(-1500,3500), breaks=c(-1000,0,1000,2000,3000)) +  
      facet_grid(mode~cohort~train_type) 
    
    # fast and slow lines by PARTICIPANT:
    # use avg_rlims_c from above
    
    # add vlines to plot: 
    d <- d + geom_vline(aes(xintercept=fast_avg, group=cohort), data=avg_rlims_c, linetype=2, alpha=.95)
    d <- d + geom_vline(aes(xintercept=slow_avg, group=cohort), data=avg_rlims_c, linetype=2, alpha=.95)
    
    # save the plot!
    ggsave("rtempo_drift_eachCohort_2016-11-20.png", width=6.5, height=11, dpi=100)

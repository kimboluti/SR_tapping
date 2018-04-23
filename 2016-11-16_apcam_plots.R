# Load libraries 
    library(stringr)
    library(tidyr)
    library(ggplot2)
    library(plyr)
    
    library(dplyr) # load dplyr after plyr 

# *** SR TAPS - adjusted means (AM) from stimulus generation -------
    taps_summary_am_input <- "compiled data/summary_am_2016-10-04.csv"

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
    
    
    compare %>%
      ggplot(aes(x = seed_int, y = drift_to_first, color = seed)) +
      geom_point(size = 3, alpha = .5) +
      #geom_smooth(aes(x = seed_int, y = drift, group = exp_name), method = lm, se = TRUE)+
      facet_grid(mode~train_type)
    
    

# *** SPAGHETTI DRIFT PLOTS *** -------
    tempo_with_seeds %>%
      ggplot(aes(x=generation, y=mean_tempo, group=chain_name, color=seed)) +
      geom_line(alpha = .25, lwd = 1.5, color="#999999") +
      geom_smooth(aes(group=seed, color=seed), lwd = 1.5, alpha = .75, se = F) +
      #geom_hline(yintercept=0, linetype=2, alpha=.95) +
      theme_bw() +
      labs(#title = '(A) Serial reproduction chains',
           x = 'Iteration', 
           y = 'Tempo (ms)') +
      theme(panel.grid.minor=element_blank(),
            panel.grid.major=element_blank(),
            axis.title = element_text(size = 20),
            axis.text = element_text(size = 16),
            #axis.text.x = element_text(angle = 90),
            #legend.position='right', #c(.9,.9) # 'none'
            legend.title = element_text(size=16),
            legend.text = element_text(size = 16),
            strip.text.x = element_text(size = 16),
            strip.text.y = element_text(size = 16)) +
      scale_color_discrete(name="Seed\nTempo (ms)") +
      scale_x_continuous(limits=c(-1,16), breaks=c(0,5,10,15)) +  
      #scale_y_continuous(limits=c(-1.5,5.5), breaks=c(-1,0,1,2,3,4,5)) +  
      facet_grid(mode~train_type) 
    
    # save the plot!
    ggsave("exp2_tempo_spag_2016-11-16.png", width=8.25, height=6.25, dpi=300)

# *** DRIFT PLOTS *** -------
    compare %>%
      ggplot(aes(x=seed_int, y=drift)) +
      geom_point(aes(color=seed), size = 3, alpha=.75) + 
      #geom_line(aes(group=cohort), alpha = .5, lwd = 1, color = "#999999") +
      geom_smooth(aes(group=train_type), method = lm, se = FALSE, lwd=1, alpha=.75, color='black') +
      geom_hline(aes(yintercept=0),color="#999999") + # no drift line
      
      theme_bw() +
      labs(#title = '(A) Serial reproduction chains',
        x = 'Seed Tempo (ms)', 
        y = 'Seed to Final Drift (ms)') +
      theme(panel.grid.minor=element_blank(),
            panel.grid.major=element_blank(),
            axis.title = element_text(size = 20),
            axis.text = element_text(size = 16),
            #axis.text.x = element_text(angle = 90),
            #legend.position='right', #c(.9,.9) # 'none'
            legend.title = element_text(size=16),
            legend.text = element_text(size = 16),
            strip.text.x = element_text(size = 16),
            strip.text.y = element_text(size = 16)) +
      scale_color_discrete(name="Seed\nTempo (ms)") +
      #scale_x_continuous(limits=c(-1.5,5.5), breaks=c(-1,0,1,2,3,4,5)) +  
      #scale_y_continuous(limits=c(-1500,3500), breaks=c(-1000,0,1000,2000,3000)) +  
      facet_grid(mode~train_type) 
    
    
    # save the plot!
    ggsave("exp2_drift_2016-11-16.png", width=8.25, height=6.25, dpi=300)

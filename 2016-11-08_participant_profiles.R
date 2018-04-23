##### individual participant profiles ----
TO DO: font sizes ~8-10?

plots (and analysis) for each participant in experiment 3

relative tempo not needed since doing single participant analysis? 
or still useful to put people on the same scale?

  11-13-16: thinking relative tempo. include table with smt to give the unit meaning


##### Base data for SR and MT -------
  # SR: tempo at each generation tempo
  # 1239 rows = (11Ps * 15gens * 7 seeds) + (1P*12g*7s)
    tempo_sr <- read.csv("compiled data/summary_ep_2016-11-05.csv") %>%
      mutate(cohort = as.factor(cohort),
             participant = as.factor(participant),
             session = as.factor(session),
             seed = as.factor(seed),
             id = as.factor(id)) %>%
      rename(tempo = median_tempo) %>%
      select(-X)
    
  # MT: long form reproductions by generation
  # 529 rows = (11Ps * 15gens * 3types) + (1P * 12gens * 3types) - a couple outliers 
    tempo_mt <- read.csv("compiled data/summary_ep_mt_2016-11-05.csv") %>%
      mutate(cohort = as.factor(cohort),
             participant = as.factor(participant),
             session = as.factor(session),
             id = as.factor(id),
             block = as.factor(block)) %>%  
      rename(tempo = median_tempo) %>%
      select(-X, -mean_tempo, -sd_taps, -cv_with_sd)

##### Shortcuts to data ----
  # SR reproductions with seeds (each generation tempo):
  # 1323 rows = tempo_sr + 84 seeds
    tempo_with_seeds <- read.csv("compiled data/sr_tempo_with_seeds.csv") %>%
      mutate(cohort = as.factor(cohort),
             participant = as.factor(participant),
             session = as.factor(session),
             seed = as.factor(seed),
             id = as.factor(id)) %>%
      select(-X)
  
  # SR drift (seed, first, and final tempo):
  # 84 rows = 84 chains
    chains <- read.csv("compiled data/chains_2016-11-08.csv") %>%
      mutate(cohort = as.factor(cohort),
             participant = as.factor(participant),
             seed = as.factor(seed)) %>%
      select(-X)
  
  # MT-SR:
  # 1246 rows = tempo_sr + 7 (why?)
    indivs <- read.csv("tables/indivs_mt_sr_bygen_2016-11-09.csv") %>%
      mutate(cohort = as.factor(cohort),
             participant = as.factor(participant),
             session = as.factor(session),
             seed = as.factor(seed),
             id = as.factor(id)) %>%
      select(-X)

##### participant df ----

    target <- "200"
    
  # target participant MT spaghetti:
  # 45 rows = 15 gen * 3 types
  p_mt_long <- tempo_mt %>% 
    filter(participant==target)

  # target participant chain drift:
  # 7 rows = 7 chains
    p_chains <- chains %>%
      filter(participant==target)
    
  # target participant SR and MT: -- should be good for spaghetti? and MT plots -- seed using stimulus_generation??
  # 105 rows = 15 gen * 7 chains
    p_indivs <- indivs %>%
      filter(participant==target) %>%
      mutate(relative_tempo = (stimulus_tempo - smt_avg) / smt_avg)
      
    
    something with range???
    
  # rm full dfs if desired...
    rm(chains,indivs,tempo_mt,tempo_sr,tempo_with_seeds)
      

##### Plots (run for each participant) ----

## (0) trying - visualize variability of MT -- boxplot?
box <- p_mt_long %>%
  mutate(tap_type = factor(p_mt_long$tap_type, levels = c("fast mt", "smt", "slow mt"))) %>%
  ggplot(aes(x=tap_type, y=tempo, color = tap_type)) +
  geom_boxplot() +
  geom_point(size = 2) + #geom_line(lwd = 1) +
  theme_bw() +
  labs(title = 'Self-paced motor tempo',
       x = 'Tap Type', 
       y = 'Tempo (ms)') +
  scale_color_discrete(name="Task") #+
  #scale_x_continuous(limits=c(-1,16), breaks=c(0,5,10,15))


## (1) MT timeseries (tempo~generation). color=tap_type. one panel. ----
# to do: saving line, to implement throughout...
  
  mt_ts <- p_mt_long %>%
    mutate(tap_type = factor(p_mt_long$tap_type, levels = c("fast mt", "smt", "slow mt"))) %>%
    ggplot(aes(x=generation, y=tempo, color = tap_type)) +
    geom_point(size = 2) + geom_line(lwd = 1) +
    theme_bw() +
    labs(title = 'Self-paced motor tempo',
         x = 'Generation', 
         y = 'Tempo (ms)') +
  scale_color_discrete(name="Task") +
  scale_x_continuous(limits=c(-1,16), breaks=c(0,5,10,15))

  # futz with this:
  ggsave("4400_mt_ts.png", width=4, height=3, dpi=100)

##? (2) MT return (tempo_next~tempo). color=generation. three columns (each tap type). ----
  
  smt_r <- p_mt_long %>%
    filter(tap_type == "smt") %>%
    group_by(participant) %>%
    arrange(generation) %>%
    mutate(next_generation = generation + 1,
           next_tempo = lead(tempo, 1),
           delta = next_tempo - tempo) %>%
    select(train_type,cohort,participant,session,id,generation,block,tap_type,
           tempo,next_generation,next_tempo,delta) %>%
    ggplot(aes(x=tempo, y =next_tempo, color = generation)) +
    geom_point(size=2, shape = 1) + 
    geom_segment(aes(xend=c(tail(tempo, n=-1), NA), 
                     yend=c(tail(next_tempo, n=-1), NA)), 
                 arrow = arrow(angle = 45, length = unit(0.075, "inches"), type = "closed")) +
    scale_colour_gradientn(colours = rainbow(n = 4, start = .5, end = 1), name="Generation") +
    theme_bw() +
    labs(title = 'SMT Return Plot',
         x = expression(x[i]), 
         y = expression(x[i+1]))    

  ggsave("4400_smt_return.png", width=4, height=3, dpi=100)

##? (3) MT bias (or do number line?). 3 columsn (each tap type) ----
# arrow from beginning to end? whatever. maybe this doesn't make sense.
# geom_curve(aes(x = x1, y = y1, xend = x2, yend = y2, colour = "curve"), data = df)


# ???bias equiv for smt:
mt_b <- p_mt_long %>%
  filter(tap_type == "smt") %>%
  group_by(participant) %>%
  arrange(generation) %>%
  mutate(next_generation = generation + 1,
         next_tempo = lead(tempo, 1),
         delta = next_tempo - tempo) %>%
  select(train_type,cohort,participant,session,id,generation,block,tap_type,
         tempo,next_generation,next_tempo,delta) %>%
  ggplot(aes(x=tempo, y = delta, color = generation)) +
  geom_point(size=2) + 
  scale_colour_gradientn(colours = rainbow(n = 4, start = .5, end = 1), name="Generation") +
  geom_smooth(aes(group=participant), method=lm, se=F, color="black") +
  theme_bw() +
  labs(title = 'SMT Bias Plot',
       x = expression(x[i]), 
       y = expression(x[i+1] - x[i]))

mt_b <- mt_b + geom_hline(aes(yintercept=0),color="#999999")
mt_b <- mt_b + geom_vline(xintercept=p_indivs$smt_avg, color = '#00BF00', lwd = 1.5, alpha = .75)
mt_b <- mt_b + geom_vline(xintercept=p_indivs$fast_avg, color = '#FF8080', lwd = 1.5, alpha = .75)
mt_b <- mt_b + geom_vline(xintercept=p_indivs$slow_avg, color = '#409FFF', lwd = 1.5, alpha = .75) 

ggsave("4400_smt_bias.png", width=4, height=3, dpi=100)

## (4) SR timeseries with hline at participant SMT, fast & slow ----
# to do: fix scales (incl add label to each generation). adjust colors for hlines

ts <- p_indivs %>%
  ggplot(aes(x=generation, y=reproduced_tempo, group=chain_name, color=seed)) +
  geom_point(size = 2) + geom_line(lwd = 1) +
  theme_bw() +
  labs(title = '(A) Serial Reproduction Chains',
       x = 'Generation', 
       y = 'Reproduced tempo (ms)') +
  scale_color_discrete(name="Seed (ms)") +
  scale_x_continuous(limits=c(-1,16), breaks=c(0,5,10,15))


ts <- ts + geom_hline(yintercept=p_indivs$smt_avg, color = '#00BF00', lwd = 1.5, alpha = .75)
ts <- ts + geom_hline(yintercept=p_indivs$fast_avg, color = '#FF8080', lwd = 1.5, alpha = .75)
ts <- ts + geom_hline(yintercept=p_indivs$slow_avg, color = '#409FFF', lwd = 1.5, alpha = .75) 

ggsave("4400_sr_ts.png", width=4, height=3, dpi=100)

## (5) SR drift with hline at 0 and vline at participant smt -- also fast and slow tempi? ----
# to do: fix scales. adjust colors for vlines, line fit
# do chains converge: is there a limit point (Y if slope < 0, N if slope >= 0)?

d <- p_chains %>%
ggplot(aes(x = seed_int, y = drift)) +
  geom_point(aes(color = seed), size = 3) +
  geom_line(aes(group=cohort), alpha = .5, lwd = 1, color = "#999999") +
  geom_hline(aes(yintercept = 0), color = "#999999") +
  geom_smooth(method = lm, se=F, lwd = 1.5, alpha = .75, color = 'black') + 
  # vertical line at x-intercept saved in output from lm, above
  # works with facet_wrap(~exp_name) but not as well with facet_grid(mode~train_type)
  #geom_vline(data=output, aes(xintercept=x_int, group=exp_name), color = "#999999") +
  theme_bw() +
  labs(title = '(B) SR Chain Drift',
       x = 'Seed tempo (ms)', 
       y = 'Final - Seed tempo (ms)') +
  scale_color_discrete(name="Seed (ms)") 


d <- d + geom_vline(xintercept=p_indivs$smt_avg, color = '#00BF00', lwd = 1.5, alpha = .75)
d <- d + geom_vline(xintercept=p_indivs$fast_avg, color = '#FF8080', lwd = 1.5, alpha = .75)
d <- d + geom_vline(xintercept=p_indivs$slow_avg, color = '#409FFF', lwd = 1.5, alpha = .75) 

ggsave("4400_sr_drift.png", width=4, height=3, dpi=100)

## (6) SR return plot (reproduced_tempo ~ stimulus_tempo) ----
# doesn't work to add smt line? fast and slow lines? with free scales
# reconsider coloring and faceting?

ar <- p_indivs %>%
ggplot(aes(x = stimulus_tempo, y = reproduced_tempo, color = generation)) +
  geom_point(size = 3, shape = 1) + 
  geom_segment(aes(xend=c(tail(stimulus_tempo, n=-1), NA), 
                   yend=c(tail(reproduced_tempo, n=-1), NA)), 
               arrow = arrow(angle = 45, length = unit(0.075, "inches"), type = "closed")) +
  scale_colour_gradientn(colours = rainbow(n = 4, start = .5, end = 1), 
                         name="Generation", breaks=c(0,5,10,15)) +
  geom_abline(aes(slope=1,intercept=0),color="#999999") +
  theme_bw() + 
  labs(title = '(A) Autoregression',
       x = 'Stimulus tempo (ms)', #expression(x[i])
       y =  'Reproduced tempo (ms)') + #expression(x[i+1]))
  facet_wrap(~seed, scales="free")

ggsave("4400_sr_ar.png", width=6, height=5, dpi=100)  

## (7) SR bias plot (bias ~ stimulus_tempo) ----
# is there bias in reproductions at each generation 
# separate by seed? to find repulsion points, or combine over seeds?


b <- p_indivs %>%
ggplot(aes(x = stimulus_tempo, y = bias, color = generation)) +
  geom_point(aes(color = generation), size = 3, shape = 19) + 
  geom_smooth(aes(group = train_type), method = lm, se = FALSE, lwd=1.5, alpha=.75, color='black') +
  scale_colour_gradientn(colours = rainbow(n = 4, start = .5, end = 1),
                         name="Generation", breaks=c(0,5,10,15)) +
  geom_hline(aes(yintercept=0),color="#999999") +
  theme_bw() +
  labs(title = '(B) Bias',
       x = 'Stimulus tempo (ms)', #expression(x[i])
       y =  'Reproduced - Stimulus tempo (ms)') + #expression(x[i+1] - x[i]))
  facet_wrap(~seed, scales="free")


b <- b + geom_hline(aes(yintercept=0),color="#999999")
b <- b + geom_vline(xintercept=p_indivs$smt_avg, color = '#00BF00', lwd = 1.5, alpha = .75)
b <- b + geom_vline(xintercept=p_indivs$fast_avg, color = '#FF8080', lwd = 1.5, alpha = .75)
b <- b + geom_vline(xintercept=p_indivs$slow_avg, color = '#409FFF', lwd = 1.5, alpha = .75) 


ggsave("4400_sr_b.png", width=6, height=5, dpi=100)  

## (8) SR estimate of limit points: linear fit for x-int, graphically compare to SMT... ----
# but not relevant for non-converging participants? or would it tell me if there's a repulsive point...
vectors by state (i.e., drift or bias by seed int)


# trying to plot 'portrait' in 1D state space: vectors (From stim to repro) on line

  
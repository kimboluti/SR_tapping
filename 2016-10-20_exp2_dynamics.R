
##### TAP SUMMARY: linear finite difference parameter -----
# in tempo_bychain_table from processing script

# hm curious. plotting boxes for a's by seed by condition
#   how similar is this to drift?
tempo_bychain_table %>%
  ggplot(aes(x = seed, y = a, color = seed)) +
  geom_point(aes(x = seed, y = a), size = 2, alpha = .75) +
  geom_boxplot(size = .5) + theme_bw() +
  facet_wrap(~exp_name)


##### prep return df - using wav_tempo (wtd avg across blocks)  -----

# read-in from saved
tempo_return <- read.csv('compiled data/tempo_return.csv')

# or create from processing script:
tempo_return <- taps_and_seeds_summary %>%
  group_by(chain_name) %>%
  mutate(generation = as.numeric(as.character(generation)), # convert gen to char before num to preserve '0' (i.e. factor values, not just indices wrt factor levels)
         cohort = as.numeric(cohort),
         seed_int = as.numeric(seed_int)) %>% 
  arrange(exp_name, cohort, seed_int, generation) %>%
  mutate(source_gen = generation - 1,
         source_wav_tempo = ifelse(source_gen == 0, seed_int, lag(wav_tempo)),
         delta_wav = wav_tempo - source_wav_tempo)

write.csv(tempo_return, "compiled data/tempo_return.csv")

##### plots -----
# S = tempo fcn
# i = generation
# x = tempo at timepoint
# save dim: 675 X 464

# (A) Spaghetti 
tempo_return %>%
  filter(exp_name == "a_or") %>%
  ggplot(aes(x = generation, y = wav_tempo, color = seed)) +
  geom_point(size = 3, alpha = .75) + #geom_smooth(method = lm, se = FALSE) +
  theme_bw() +
  labs(title = "(A) tempo by generation",
       x = 'generation', 
       y = 'reproduced tempo (ms)') + 
  theme(title = element_text(size = 26),
      panel.grid.minor=element_blank(),
      panel.grid.major=element_blank(),
      axis.title = element_text(size = 26),
      axis.text = element_text(size = 16),
      legend.position ='right', #c(.9,.9)
      legend.title = element_text(size=22),
      legend.text = element_text(size = 16)) +
  scale_color_discrete(name="seed tempo (ms)")
  
# (C) dS/dt
tempo_return %>%
  filter(exp_name == "a_or") %>%
  ggplot(aes(x = generation, y = delta_wav, color = seed)) +
  geom_point(size = 3, alpha = .75) + #geom_smooth(method = lm, se = FALSE) +
  theme_bw() +
  labs(title = "(C) tempo change by generation",
       x = 'generation', 
       y = 'reproduced - stimulus (ms)') + 
  theme(title = element_text(size = 26),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        axis.title = element_text(size = 26),
        axis.text = element_text(size = 16),
        legend.position ='right', #c(.9,.9)
        legend.title = element_text(size=22),
        legend.text = element_text(size = 16)) +
  scale_color_discrete(name="seed tempo (ms)")

# (B) x(i+1) = S(x(i))
tempo_return %>%
  filter(exp_name == "a_or") %>%
  ggplot(aes(x = source_wav_tempo, y = wav_tempo, color = seed)) +
  geom_point(size = 3, alpha = .75) + #geom_smooth(method = lm, se = FALSE) +
  theme_bw() +
  labs(title = "(B) relationship between reproductions",
       x = 'stimulus tempo (ms)', 
       y = 'reproduced tempo (ms)') + 
  theme(title = element_text(size = 26),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        axis.title = element_text(size = 26),
        axis.text = element_text(size = 16),
        legend.position ='right', #c(.9,.9)
        legend.title = element_text(size=22),
        legend.text = element_text(size = 16)) +
  scale_color_discrete(name="seed tempo (ms)")

# (D) dS/dx
tempo_return %>%
  filter(exp_name == "a_or") %>%
  ggplot(aes(x = source_wav_tempo, y = delta_wav, color = seed)) +
  geom_point(size = 3, alpha = .75) + #geom_smooth(method = lm, se = FALSE) +
  theme_bw() +
  labs(title = "(D) tempo change by stimulus tempo",
       x = 'stimulus tempo (ms)', 
       y = 'reproduced - stimulus (ms)') + 
  theme(title = element_text(size = 26),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        axis.title = element_text(size = 26),
        axis.text = element_text(size = 16),
        legend.position ='right', #c(.9,.9)
        legend.title = element_text(size=22),
        legend.text = element_text(size = 16)) +
  scale_color_discrete(name="seed tempo (ms)")

# compare to drift
tempo_bychain_table %>%
  filter(exp_name == "a_or") %>%
  ggplot(aes(x=seed_int, y=drift_means)) +
  geom_point(aes(color = seed)) +geom_smooth(method = lm, se = FALSE) +
  theme_bw()

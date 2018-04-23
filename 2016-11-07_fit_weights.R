# load data -------
# sr data:
ar1 <- read.csv("compiled data/ar1_2016-11-06.csv") %>% 
  select(-X) %>%
  mutate(participant=as.factor(participant))

# mt data:
mt_summary_byparticipant <- read.csv("tables/mt_summary_byparticipant_wide_2016-11-07.csv") %>% 
  select(-X) %>%
  mutate(participant=as.factor(participant))

#shortcut:
indivs <- read.csv("tables/indivs_mt_sr_bygen_2016-11-07.csv") %>%
  select(-X) %>%
  mutate(participant=as.factor(participant))

# testing -- solve for lambda in x&g equation -------

Reproduction = (1-lambda)SMT + lambda(Stimulus)
lambda = - [(Reproduction-SMT)/(SMT-Stimulus)]

indivs <- indivs %>%
  mutate(lambda = -(reproduced_tempo - smt_avg)/(smt_avg - stimulus_tempo))

# quick check: lambda by participant
report <- indivs %>%
  group_by(participant) %>%
  summarize(mean_lambda = mean(lambda, na.rm=T))

# quick check: lambda by condition
report <- indivs %>%
  group_by(train_type) %>%
  summarize(mean_lambda = mean(lambda, na.rm=T))
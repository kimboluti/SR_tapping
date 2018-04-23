#####################
#
#  B&S Import and composition for Iterated Tapping -- Experiment 3
#
#####################

##### set-up -------
# data already checked for missing/duplicates and csv files 'composed' from raw data
# only aud_sc needs to read in P113 with data in question instead of variable order

# Load libraries 
library(stringr)
library(tidyr)
library(ggplot2)
library(plyr)

library(dplyr) # load dplyr after plyr 

##### read-in data -------

survey_raw <- read.csv("b&S data/demo_all_session_1_full_b&s.csv") %>%
  select(Subject,session,cohort,Age,Gender,Handedness,English,
       Dance.Years, Music.Years, Rhythmic.Ability,
       Attention, Effort, Difficulty, Sleep, State) %>%
  mutate(train_type = ifelse(Subject < 1000, "synchronize", "observe")) %>%
  # also do extracting info stuff, as needed...and CLEAN UP (99s -> NA)
  arrange(Subject)


daily <- read.csv("b&S data/demo_all_dailystrats.csv") %>%
  select(Subject, session, cohort, attend, effort, diffic, underst, 
         health, sleep, caffamt) %>%
  mutate(train_type = ifelse(Subject < 1000, "synchronize", "observe")) %>%
  rename(subject = Subject) %>%
  # also do extracting info stuff, as needed...and CLEAN UP (99s -> NA)
  arrange(subject)

##### convert values -------
# eg 1/2 to male/female, etc
# UNSURE: currently 99 becomes NA for music_years, etc. should it just be 0?

survey <- survey_raw %>%
  filter(Subject !=6600) %>% # this participant dropped out after session 1
  rename(subject = Subject, age = Age, sex = Gender, hand = Handedness, english = English,
         dance_years = Dance.Years, music_years = Music.Years, rhythm = Rhythmic.Ability,
         attention = Attention, effort = Effort, difficulty = Difficulty, sleep = Sleep, state = State) %>%
  mutate(sex = as.factor(ifelse(sex == 2, 'female',
                                ifelse(sex == 1, 'male', 'no response'))),
         hand = as.factor(ifelse(hand == 1, 'right',
                                 ifelse(hand == 2, 'left',
                                        ifelse(hand == 3, 'ambi', 'no response')))),
         english = as.factor(ifelse(english == 1, 'native',
                                    ifelse(english == 2, 'non-native', 'no response'))),
         dance_years = ifelse(dance_years == "99", NA, dance_years),
         music_years = ifelse(music_years == "99", NA, music_years)) %>%
  select(train_type,cohort,subject,
         age,sex,hand,english,dance_years,music_years,
         rhythm,attention,effort,difficulty,sleep,state)
         

# save to file
write.csv(survey, "b&s data/survey_neat.csv")

# clear up the environment 
rm(survey_raw)

##### finally, some data! -------
# probably easiest to just look at the summary
# or summarize by variable of interest.
# some options, below.

summary(survey)
summary(survey[survey$subject != 6600,]) # without P6600


sex_count <- table(survey$sex)
english_count <- table(survey$english)
age_summary <- summary(survey$age)

# some music_years = 0 still snuck in...how does it look without?
filtered <- survey[survey$music_years != 0,]
summary(filtered$music_years)


# to compare across experiments
survey_summary <- survey %>%
  #filter(subject != 6600) %>%
  mutate(count_f = ifelse(sex == "female", 1,0)) %>%
  group_by(train_type) %>%
  summarize(n_female = sum(count_f),
            age_min = min(age),
            age_max = max(age),
            age_mean = mean(age),
            music_min = min(music_years, na.rm=T),
            music_max = max(music_years, na.rm=T),
            music_mean = mean(music_years, na.rm=T))


# some quick plots
survey %>%
  ggplot(aes(x=cohort, y=age)) +
  geom_boxplot() +
  facet_grid(train_type~.)

survey %>%
  ggplot(aes(x=cohort, y=music_years)) +
  geom_boxplot() +
  facet_grid(train_type~.)
  
survey %>%
  ggplot(aes(x=music_years)) +
  geom_histogram(binwidth=1) +
  facet_wrap(~exp_name)

  

 
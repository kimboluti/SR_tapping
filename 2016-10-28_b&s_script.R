#####################
#
#  B&S Import and composition for Iterated Tapping
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
# future: check experiment room...esp for tango

  # a-or:
    survey_a_or <- read.csv("b&S data/survey_aud_or.csv") %>%
      select(Subject,exp_name,Age,Gender,Handedness,English,
             Dance.Years, Music.Years, Rhythmic.Ability,
             Attention, Effort, Difficulty, Sleep, State) %>%
      mutate(Dance.Years = as.numeric(Dance.Years),
             Music.Years = as.numeric(Music.Years),
             Sleep = as.numeric(Sleep)) %>%
      arrange(Subject)

    levels(survey_a_or$exp_name) <- c("a_or", "a_sc", "v_or", "v_sc")


  # a-sc:  
    # errant participant data:
    a_sc_113 <- read.csv("b&S data/survey_aud_sc_113.csv") %>%
      select(Subject,exp_name,Age,Gender,Handedness,English,
             Dance.Years, Music.Years, Rhythmic.Ability,
             Attention, Effort, Difficulty, Sleep, State) %>%
      mutate(Dance.Years = as.numeric(Dance.Years),
             Music.Years = as.numeric(Music.Years),
             Sleep = as.numeric(Sleep))
      
    # main data set
    survey_a_sc <- read.csv("b&S data/survey_aud_sc.csv") %>%
      select(Subject,exp_name,Age,Gender,Handedness,English,
             Dance.Years, Music.Years, Rhythmic.Ability,
             Attention, Effort, Difficulty, Sleep, State) %>%
      mutate(Dance.Years = as.numeric(Dance.Years),
             Music.Years = as.numeric(Music.Years),
             Sleep = as.numeric(Sleep)) %>%
      merge(a_sc_113, all = TRUE) %>%
      arrange(Subject)

    levels(survey_a_sc$exp_name) <- c("a_sc", "a_or", "v_or", "v_sc")
    

  # v-or:
    survey_v_or <- read.csv("b&S data/survey_vis_or.csv") %>%
      select(Subject,exp_name,Age,Gender,Handedness,English,
             Dance.Years, Music.Years, Rhythmic.Ability,
             Attention, Effort, Difficulty, Sleep, State) %>%
      mutate(Dance.Years = as.numeric(Dance.Years),
             Music.Years = as.numeric(Music.Years),
             Sleep = as.numeric(Sleep)) %>%
      arrange(Subject)

    levels(survey_v_or$exp_name) <- c("v_or", "a_or", "a_sc", "v_sc")
    

  # v-sc:
    survey_v_sc<- read.csv("b&S data/survey_vis_sc.csv") %>%
      select(Subject,exp_name,Age,Gender,Handedness,English,
             Dance.Years, Music.Years, Rhythmic.Ability,
             Attention, Effort, Difficulty, Sleep, State) %>%
      mutate(Dance.Years = as.numeric(Dance.Years),
             Music.Years = as.numeric(Music.Years),
             Sleep = as.numeric(Sleep)) %>%
      arrange(Subject)

    levels(survey_v_sc$exp_name) <- c("v_sc", "a_or", "a_sc", "v_or")

##### combine all b&s -------
# should be 300 rows (each exp should have been 75 rows)
  
  survey_raw <- survey_a_or %>%  
    bind_rows(survey_a_sc) %>%
    bind_rows(survey_v_or) %>%
    bind_rows(survey_v_sc)


##### extracting info from subject number... -------

  info <- data.frame(matrix(ncol = 3, nrow = 300))
  cnames <- c("cohort", "tens", "ones")
  colnames(info) <- cnames
  
  for (i in 1:length(survey_raw$Subject)) {
    for (j in 1:ncol(info)) {
      info[i,j] <- strsplit(as.character(survey_raw$Subject),"")[[i]][j]
    }
  }
  

  cohort_info <- data.frame(info$cohort)
    colnames(cohort_info) <- "cohort"


  gen_info <- data.frame(as.numeric(paste(info$tens, info$ones, sep = "")))
    colnames(gen_info) <- "generation"


##### convert values -------
# eg 1/2 to male/female, etc
# UNSURE: currently 99 becomes NA for music_years, etc. should it just be 0?

survey <- survey_raw %>%
  bind_cols(cohort_info, gen_info) %>% # careful - be sure everything sorted by Subject before proceeding!
  rename(subject = Subject, age = Age, sex = Gender, hand = Handedness, english = English,
         dance_years = Dance.Years, music_years = Music.Years, rhythm = Rhythmic.Ability,
         attention = Attention, effort = Effort, difficulty = Difficulty, sleep = Sleep, state = State) %>%
  mutate(mode = as.factor(ifelse(exp_name == "a_or" | exp_name == "a_sc", "a",
                       ifelse(exp_name == "v_or" | exp_name == "v_sc", "v", NA))),
         train_type = as.factor(ifelse(exp_name == "a_or" | exp_name == "v_or", "or",
                                       ifelse(exp_name == "a_sc" | exp_name == "v_sc", "sc", NA))),
         id = paste(exp_name, cohort, generation, sep = "_"), # unique id for each participant, matches MT data
         sex = as.factor(ifelse(sex == 2, 'female',
                                ifelse(sex == 1, 'male', 'no response'))),
         hand = as.factor(ifelse(hand == 1, 'right',
                                 ifelse(hand == 2, 'left',
                                        ifelse(hand == 3, 'ambi', 'no response')))),
         english = as.factor(ifelse(english == 1, 'native',
                                    ifelse(english == 2, 'non-native', 'no response'))),
         dance_years = ifelse(dance_years == "99", NA, dance_years),
         music_years = ifelse(music_years == "99", NA, music_years)) %>%
  select(exp_name,mode,train_type,cohort,generation,subject,id,
         age,sex,hand,english,dance_years,music_years,
         rhythm,attention,effort,difficulty,sleep,state)
         

# save to file
write.csv(survey, "b&s data/survey_ALL.csv")

# clear up the environment 
rm(a_sc_113, survey_a_or, survey_a_sc, survey_v_or, survey_v_sc, survey_raw,
   i, j, cnames, 
   info, cohort_info, gen_info)


##### finally, some data! -------
# probably easiest to just look at the summary
# or summarize by variable of interest.
# some options, below.

summary(survey)

sex_count <- table(survey$sex)
english_count <- table(survey$english)
age_summary <- summary(survey$age)

# some music_years = 0 still snuck in...how does it look without?
filtered <- survey[survey$music_years != 0,]
summary(filtered$music_years)


# to compare across experiments
survey_summary <- survey %>%
  filter(music_years != 0) %>%
  group_by(exp_name) %>%
  summarize(age_min = min(age),
            age_max = max(age),
            age_mean = mean(age),
            music_min = min(music_years, na.rm=T),
            music_max = max(music_years, na.rm=T),
            music_mean = mean(music_years, na.rm=T))


# some quick plots
survey %>%
  ggplot(aes(x=cohort, y=age)) +
  geom_boxplot() +
  facet_grid(exp_name~.)

survey %>%
  ggplot(aes(x=cohort, y=music_years)) +
  geom_boxplot() +
  facet_grid(exp_name~.)
  
survey %>%
  ggplot(aes(x=music_years)) +
  geom_histogram(binwidth=1) +
  facet_wrap(~exp_name)

  

 

# re-create taps_summary with counts by generation (to check that same number of taps for all)

taps_summary_at <- taps_at_raw %>% 
  filter(rejects == "n") %>%
  group_by(mode,type, seed, chain_name,
           filename, starter, id, sourceId, generation) %>% 
  summarize(mean_tempo = mean(tap_int),
            sd_taps = sd(tap_int),
            median_tempo = median(tap_int), 
            mad_taps = median(abs(tap_int - median_tempo)),
            n = n()) %>%            
  mutate(cov = sd_taps/mean_tempo,
         comv = mad_taps/median_tempo) %>%
  ungroup() %>%
  mutate(seed = as.factor(seed)) %>%
  mutate(seed_int = ifelse(seed == 0, 300, 
                           ifelse(seed == 1, 600, 
                                  ifelse(seed == 2, 900, 
                                         ifelse(seed == 3, 1200, NA))))) %>%
  arrange(chain_name, generation)

# write to csv for further processing
write.csv(taps_summary_at, "compiled data/taps_summary_output_withcounts.csv")

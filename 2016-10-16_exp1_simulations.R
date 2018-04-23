
##### spaghetti lines by chain -----
    
    # predict using parameters in: lm_spag_bychain (13 rows) - output from exp1_processing
    # compare to data in: taps_summary (184 rows) - output from exp1_composition
    
    # prepare data frame with observed and predicted data
    preds <- taps_summary %>%
      select(mode:mean_tempo) %>%
      left_join(lm_spag_bychain, by = "chain_name") %>%
      mutate(mean_tempo_pred = slope*generation + y_int,
             residuals = mean_tempo - mean_tempo_pred)
    
    # compare predicted and observe tempi:
    ggplot(preds, aes(x = generation, group = chain_name, color = seed)) +
      # plot predicted lines
      #geom_point(aes(y = mean_tempo_pred), size = 3, shape = 19, alpha = .75) +
      geom_line(aes(y = mean_tempo_pred), linetype = 'solid') +
      # plot data points
      geom_point(aes(y = mean_tempo,), size = 3, shape = 5, alpha = .75) +
      geom_line(aes(y = mean_tempo), linetype = 'dashed') +
      facet_grid(seed~.) +
      theme_bw()
    
    # plot residuals
    ggplot(preds, aes(x = mean_tempo_pred, y = residuals, group = chain_name, color = seed)) +
      geom_point(size = 3, shape = 19, alpha = .75) +
      geom_line(aes(y = 0), linetype = 'solid') +
      facet_grid(seed~.) +
      theme_bw()
    
      # notes: ha. unsure that the 1200 resids are really random. 900 hints at a similar pattern...

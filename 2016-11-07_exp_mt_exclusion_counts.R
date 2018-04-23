
# exp 3 MT data
# number of taps excluded for each exclusion criterion



# table of counts of nints by tap type, after removing first tap:
    table(taps_mt[taps_mt$tap_incl_rmfirst==1,]$tap_type)
    fast: 5266
    slow: 5220
    smt: 5212

total taps in raw: 16229
after removing first: 15698

# exclusion by lower cut off:
after rmfirst and rm<100: 15675 (so 15698-15675 = 23 removed. 23/15698=.14%)
  
    tmp <- taps_mt %>%
      filter(tap_incl_rmfirst == 1,
             tap_incl_rm100ms ==1)
  
    # table of counts of nints by tap type, after rm first and rm <100ms:
    table(tmp$tap_type)
    fast:5264
    slow:5207
    smt:5204
  
    # even better: table of counts removed because of rm100ms <100
    table(taps_mt[taps_mt$tap_incl_rmfirst==1,]$tap_type) - table(tmp$tap_type)
  


# exclusion by upper cut off:
after rmfirst, and rm<100, and rm>upper: 15185 (so 15675-15185 = 490 removed. 490/15698=3.1%)

    tmp2 <- taps_mt %>%
    filter(tap_incl_rmfirst == 1,
           tap_incl_rm100ms == 1,
           tap_incl_rmupper == 1)
  
  # table of counts of nints by tap type, after rm first and rm <100ms:
    table(tmp2$tap_type)
    fast: 5106
    slow: 5040
    smt: 5039
    
  # even better: table of counts removed only because of rmupper:
  table(taps_mt[taps_mt$tap_incl_rmfirst==1 &
                  taps_mt$tap_incl_rm100ms==1,]$tap_type) - table(tmp2$tap_type)



# exclusion by number of intervals in a production:
after rmfirst, and rm<100, rm>upper AND rm_nints<10: 15179 (so 15185-15179 = 6 removed. 6/15698=reallysmall)

  tmp3 <- taps_mt %>%
    filter(tap_incl_rmfirst == 1,
           tap_incl_rm100ms == 1,
           tap_incl_rmupper == 1,
           tap_incl_rmnints == 1)
  
  # table of counts of nints by tap type, after all the removes:
  table(tmp3$tap_type)
  fast: 5105
  slow: 5040
  smt: 5034
  
  # even better: table of counts removed only because of rmnints < 10
  table(taps_mt[taps_mt$tap_incl_rmfirst==1 &
                  taps_mt$tap_incl_rm100ms==1 &
                  taps_mt$tap_incl_rmupper==1,]$tap_type) - table(tmp3$tap_type)

    
    counts <- taps_mt %>%
      filter(tap_number != 1) %>%
      group_by(train_type,participant,cohort,id,generation,session,block,tap_type) %>%
      summarize(nints = n())
    
    low_counts <- counts[counts$nints <= 10,] # 10->2 trials (nints(2212)=2, nints(503)=6).
    # 12-> 4 trials (many from 500...but they look like they should be cut...then again, i used 10 in exp 2)
    
    
    
clean up:
      
      rm(counts,low_counts,tmp,tmp2,tmp3)
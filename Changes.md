

How much speedup is needed to make this viable?

Most likely in the lme4/nlme fitting, but maybe generating the permutations.

Looks like can parallelize this. So get on a large machine and run in parallel.


Within the loop, there is a loop from 1:ageN and then set the first two names names of the result to
age and estimate. Is the same value of 2 for  ageN = 2 and setting the 2 names a coincidence or
supposed to be connected.  Probably a coincidence.



Rather than append  LMEMis_output_RP_oneIter to LMEMis_output_RP_allIter in each iteration,
+ pre-allocate and fill in rows
+ keep as a list until the very end and then use do.call(rbind, LMEMis_output_RP_allIter)


Set the names later.

Do the between at the end in vectorized form. 2 steps for the young and old corresponding to the two
values of ageArray.


+ dfMissing_pairedWide_RP is assigned but never used in each iteration.
  + And this involves the list[] which probably has a non-trivial overhead and uses non-standard evaluation.
  + So we can simplify this in the loop with
```
  fit.LMEMis_RP <- pairTrials_RandomPerm(dfMissing)[[2]]
```
  + Can also think about simplifying the function and not computing/returning the first element
  
Good to put names on the elements of the list returned by pairTrials_RandomPerm
  + this allows us to use 
  `pairTrials_RandomPerm(dfMissing)$fit`
  rather than `pairTrials_RandomPerm(dfMissing)[[2]]`


  

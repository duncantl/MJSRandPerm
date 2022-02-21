+ What is the distribution of the number of rows in the different datasets?
+ How much accuracy do you actually need? 
  + i.e. why 10,000 permutations
  + Power calculations.
+ Where did the 1,000 data sets come from
  + Simulated?
  + What about doing math to prove the properties you are looking to show?
     + Ultimately, simulation won't "prove" these.
+ Why emmeans? Why lmer?	 
   + emmeans seems to be doing non-standard evaluation with ls()/findAnywhere to find methods
+ Is the structure of the data sets the same or do they have different variables?
+ What is pairTrials_RandomPerm doing?
+ Why using pivot_wider?  
  + i.e., committed to tidyverse?
  + Seems to have unnecessary overhead.

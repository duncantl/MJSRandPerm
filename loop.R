# In this example, we have one dataset. We randomly permute this dataset 10,000 times,
# store emmeans results for each iteration in separate array, then calculate final emmeans summary
if(!exists("rpIter"))
    rpIter = 250 #DTL 10000

presentAvgValue = 5.5 # Value needed for emmeans calculations
ageN <- 2 # Value needed for extracting rows from emmeans output
ageArray <- c(-1.998, 0) # Array needed for extracting emmeans output 

#LMEMis_output_RP_allIter <- NULL # Temporary variable for storing emmeans for each random perm iteration

LMEMis_output_RP_allIter = vector("list", rpIter)

# Only need to compute this once, not each time in the loop, actually in the call to pairTrials_RandomPerm()
dfMissing_NoNA = dfMissing[complete.cases(dfMissing), ]


source("setNSFunctions.R")
origFuns = setNSFunctions()
Rprof("new.prof")


#LMEMis_output_RP_allIter = replicate(rpIter, {
for(i in 1:rpIter) {
  # Pair trials with random permutation function
  #list[dfMissing_pairedWide_RP, fit.LMEMis_RP] <- pairTrials_RandomPerm(dfMissing)
  fit.LMEMis_RP <- pairTrials_RandomPerm(, dfMissing_NoNA, formula = meanAmpNC_BMinusA ~ age + presentNumberAvg + (1|SUBJECTID))[[2]]  #XXX add the formula here.
    
  # Extract marginal means from LME model
  mLME <- emmeans::emmeans(fit.LMEMis_RP, pairwise ~ age, mode = "satterthwaite", 
                           lmerTest.limit = 240000, at = list(presentNumberAvg = presentAvgValue))
  
  # Extract output values for each emotion condition 
  LMEMis_output_RP_oneIter <- data.frame(summary(mLME, infer = c(TRUE, TRUE))$emmeans)
  # Check if each true population emotion condition value is located within the
  # model's 95% confidence interval for the emotion condition
#  for (ageNum in 1:ageN) {
#    LMEMis_output_RP_oneIter$inCL[ageNum] <- between(ageArray[ageNum], 
#                                                     LMEMis_output_RP_oneIter$lower.CL[ageNum], 
#                                                     LMEMis_output_RP_oneIter$upper.CL[ageNum])
#  }
    
#  names(LMEMis_output_RP_oneIter)[1:2] <- c('age', 'estimate') # Update column names
  
#  LMEMis_output_RP_oneIter$iteration <- i
    # LMEMis_output_RP_allIter <- bind_rows(LMEMis_output_RP_allIter, LMEMis_output_RP_oneIter)
  LMEMis_output_RP_allIter[[i]] <- LMEMis_output_RP_oneIter
} # , simplify = FALSE)


# Combine results.
LMEMis_output_RP_allIter2 = do.call(rbind, LMEMis_output_RP_allIter)
names(LMEMis_output_RP_allIter2)[1:2] = c('age', 'estimate')

LMEMis_output_RP_allIter2$iteration = rep(1:rpIter, sapply(LMEMis_output_RP_allIter, nrow))



LMEMis_output_RP_allIter2$inCL.vec = NA
idx = seq(1, by = 2, length = rpIter)
LMEMis_output_RP_allIter2$inCL.vec[idx] = ageArray[1] >= LMEMis_output_RP_allIter2$lower.CL[idx] & ageArray[1] <= LMEMis_output_RP_allIter2$upper.CL[idx]
LMEMis_output_RP_allIter2$inCL.vec[idx + 1L] = ageArray[2] >= LMEMis_output_RP_allIter2$lower.CL[idx + 1L] & ageArray[2] <= LMEMis_output_RP_allIter2$upper.CL[idx + 1L]


Rprof(NULL)
resetNSFunctions(origFuns)

# MJ and Serena script: Randomly pairing trial data and then fitting a
# linear mixed effects model

# Random permutation function starts on lines 160-214
# The random permutation function is called 10,000 times on line308

# This script imports a simulated mean amplitude file and induces missing trials
# based on the specified missingness pattern (e.g., more missing data in later 
# trials and in younger subjects) and percentage of subjects with low trial-count 
# (e.g., 6% of subjects are induced to have less than 10 trials/condition 
# remaining). Then, trial pairings are calculated by randomly permuting
# the dataset (10,000 times per dataset). Finally, a linear mixed effects (LME) 
# model is fitted to each randomly permuted trial-level dataset. 

# Requirements: 
  # - filename: One simulated sample's .csv file containing the following columns, 
  #   which are labelled based on the convention that lowercase variables describe 
  #   fixed effects (e.g., emotion) and capital-letter variables describe random 
  #   effects (e.g., SUBJECTID):
    # - SUBJECTID: Simulated subject ID (e.g., 01, 02, ...)
    # - age: Simulated age group (i.e., youngerAgeGroup, olderAgeGroup)
    # - emotion: Simulated emotion condition (i.e., A, B)
    # - ACTOR: Simulated stimulus actor ID (i.e., 01, 02, 03, 04, 05)
    # - presentNumber: Presentation number of specific stimulus (emotion 
    #   condition/actor) ranging from 1 to 10
    # - meanAmpNC: Simulated mean amplitude value (in units of microvolts)
  # - See instructions in steps 5 and 6 for specifying the missingness pattern 
  #   (more missing data in later trials and/or in younger subjects or data
  #   missing completely at random (MCAR)) and percentage of subjects with low
  #   trial-count, respectively

# Script Functions:
  # 1. Define function for inducing missing trials
  # 2. Load simulated data file
  # 5. Specify missingness pattern 
  # 6. Induce missing data based on specified missingness pattern and percentage
  #    of subjects with low trial-count
  # 7. Fit LME model with trial-level dataset after inducing trial missingness

# Outputs: 
  # - Estimated marginal means for each age group and model (LME)
  #   for a dataset that has been randomly permuted 10,000 times 

library(plyr) # V.1.8.6; ddply function
library(lme4) # V.1.1-25; used for creating lme models
library(lmerTest) # V.3.1-3; used for returning p-value for lme models
library(gsubfn) # V.0.7; list function for assigning multiple outputs
library(data.table) # V.1.13.2; fread function
library(dplyr) # V.1.0.2; select function
library(performance) # V.0.6.1; check_convergence function
library(afex) # V.0.28-1; ANOVA analysis
library(emmeans) # V.1.5.3; extract estimated marginal means
library(car) # V.3.0-10; contr.sum function
library(stringr) # V.1.4.0; str_sub function
library(MatchIt) # V.4.1.0; used for pairing functions
library(tidyr) # V.1.1.2; used for spread function
library(ggplot2) # V.3.3.2; used for plotting


# source("funs.R")


#-----------------------------------------------------------------------
# 2. LOAD SIMULATED DATA FILE

# Specify filepath of simulated data file
#filename <- 'T:/Projects/InProgress/LME_DifferenceWaves&LatencyERP/Data/01_SEREEGASimulation/MatlabAndRCode/00_SampleCodeForTestingDifferenceWavePairing/Sample0751-NCMeanAmpOutput.csv'

filename = "Sample0751-NCMeanAmpOutput.csv"


# Import data sample ("population" dataset)
dfOriginal <- fread(filename) 

# Specify desired columns as factors for subsequent analysis 
dfOriginal$ACTOR <- as.factor(dfOriginal$ACTOR)
dfOriginal$SUBJECTID <- as.factor(dfOriginal$SUBJECTID)
dfOriginal$emotion <- as.factor(dfOriginal$emotion)
dfOriginal$age <- as.factor(dfOriginal$age)

#-----------------------------------------------------------------------
# 5. SPECIFY MISSINGNESS PATTERN 

set.seed(20210329) # Specify seed for reproducible results

# Specify probability weight distribution for presentation numbers 6-10 vs.
# 1-5. These weights (i.e., presentNumberWeight6to10 and presentNumberWeight1to5)
# sum to 1 and are used to specify missingness pattern for the within-subjects 
# effect.  
# - For example, if presentNumberWeight6to10 = 0.7 and presentNumberWeight1to5 = 0.3,
#   then 70% of missing trials are from presentation numbers 6-10 and 30% of 
#   trials are from presentation numbers 1-5.
# - If both weight variables are equal to 0.5, an equal number of missing trials 
#   are drawn from each presentation number (MCAR missingness).
presentNumberWeight6to10 <- 0.7
presentNumberWeight1to5 <- 1-presentNumberWeight6to10

# Calculate the total number of trials per condition for each group of presentation
# numbers (i.e., 6-10 and 1-5). This value is used to scale each individual trial's 
# presentation number weight so that the weights will sum to 1 (see lines 292-294). 
emotionTrialN <- length(unique(dfOriginal$ACTOR)) * length(unique(dfOriginal$presentNumber))  
presentNumberTrials6to10 <- emotionTrialN/2 
presentNumberTrials1to5 <- emotionTrialN/2 

# Specify probability weight distribution for younger vs. older age group. These 
# weights are used to specify missingness pattern for the between-subjects
# effect (e.g., if ageWeightYounger = 0.7, then 70% of subjects selected for more
# missing trials and subsequent casewise deletion were from the younger age group).
ageWeightYounger <- 0.7
ageWeightOlder <- 1-ageWeightYounger

# Calculate the total number of subjects in the younger and older age groups. This
# value is used to scale each subject's age weight so that the weights will sum to 
# 1 (see lines 295-296).
subjectN <- length(unique(dfOriginal$SUBJECTID))
ageTrialsYounger <- subjectN/2
ageTrialsOlder <- subjectN/2

# Create probability weight columns for presentation number and age group based
# on values specified above
dfOriginal$presentNumberWeight <- ifelse(dfOriginal$presentNumber > 5,
                                         (presentNumberWeight6to10/presentNumberTrials6to10), 
                                         (presentNumberWeight1to5/presentNumberTrials1to5))
dfOriginal$ageWeight <- ifelse(dfOriginal$age == 'youngerAgeGroup', 
                               ageWeightYounger/ageTrialsYounger, ageWeightOlder/ageTrialsOlder)


#-----------------------------------------------------------------------
# 6. INDUCE MISSING DATA BASED ON SPECIFIED MISSINGNESS PATTERN AND 
#    PERCENTAGE OF SUBJECTS WITH LOW TRIAL-COUNT

# Define variables needed for induceMissingTrials function
emotionLabel <- c("A", "B") # Name of each emotion condition
emotionN <- length(emotionLabel) # Number of emotion conditions

# Specify percent of subjects with low trial-count who will be casewise deleted
caseDeletionPct <-  11

# Use induceMissingTrials function to remove trials based on specified missingness
# (step 5) and caseDeletionPct variable
dfMissing <- induceMissingTrials(dfOriginal, caseDeletionPct) [[1]]

#------------------------------------------------------------------------
# 7. PAIR TRIALS AND FIT LME MODEL WITH TRIAL-LEVEL DATASET AFTER INDUCING TRIAL MISSINGNESS

# In our full script, we call pairTrials_RandomPerm 120,000,000 times 
# (=10,000 permutations per dataset * 1,000 datasets * 4 types of induced missing trials for each dataset * 3 patterns of missing data)

# In this example, we have one dataset. We randomly permute this dataset 10,000 times,
# store emmeans results for each iteration in separate array, then calculate final emmeans summary
rpIter = 10 #DTL 10000
presentAvgValue = 5.5 # Value needed for emmeans calculations
ageN <- 2 # Value needed for extracting rows from emmeans output
ageArray <- c(-1.998, 0) # Array needed for extracting emmeans output 

#LMEMis_output_RP_allIter <- NULL # Temporary variable for storing emmeans for each random perm iteration

LMEMis_output_RP_allIter = vector("list", rpIter)

dfMissing_NoNA = dfMissing[complete.cases(dfMissing), ]

for (i in 1:rpIter) {
  # Pair trials with random permutation function
  #list[dfMissing_pairedWide_RP, fit.LMEMis_RP] <- pairTrials_RandomPerm(dfMissing)
  fit.LMEMis_RP <- pairTrials_RandomPerm(, dfMissing_NoNA)[[2]]
    
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
  
  LMEMis_output_RP_oneIter$iteration <- i
    # LMEMis_output_RP_allIter <- bind_rows(LMEMis_output_RP_allIter, LMEMis_output_RP_oneIter)
  LMEMis_output_RP_allIter[[i]] <- LMEMis_output_RP_oneIter
}


# Combine results.
LMEMis_output_RP_allIter2 = do.call(rbind, LMEMis_output_RP_allIter)
names(LMEMis_output_RP_allIter2)[1:2] = c('age', 'estimate')


LMEMis_output_RP_allIter2$inCL.vec = NA
idx = seq(1, by = 2, length = rpIter)
LMEMis_output_RP_allIter2$inCL.vec[idx] = ageArray[1] >= LMEMis_output_RP_allIter2$lower.CL[idx] & ageArray[1] <= LMEMis_output_RP_allIter2$upper.CL[idx]
LMEMis_output_RP_allIter2$inCL.vec[idx + 1L] = ageArray[2] >= LMEMis_output_RP_allIter2$lower.CL[idx + 1L] & ageArray[2] <= LMEMis_output_RP_allIter2$upper.CL[idx + 1L]




# We then extract summary values from LMEMis_output_RP_allIter

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

#-----------------------------------------------------------------------
# 1. DEFINE FUNCTION FOR INDUCING MISSING TRIALS

# induceMissingTrials: Function to randomly select subjects for inducing
# low trial counts and subsequent casewise deletion prior to ANOVA analysis. 
# In addition, missing trials are induced based on the specified probability
# weights from step 5 below. 
# - Format: 
#     list[dfMissing, subjectCaseDeletion, trialCount] <- induceMissingTrials(dfOriginal, caseDeletionPct) 
# - Inputs:
  # - dfOriginal: Dataframe with the simulated "population" dataset before any
  #   induced missing trials (see Outputs section at the top of the script for
  #   more information). 
  # - caseDeletionPct: Percent of subjects with low trial-count (i.e., less than
  #   10 trials/condition).
# - Outputs: List containing three elements:
  # - dfMissing: A copy of the dfOriginal after missing trials have been induced. 
  #   Rows with missing trials have a meanAmpNC value of NA.
  # - subjectCaseDeletion: Array of subject IDs that have been randomly selected
  #   for low trial counts.
  # - trialCount: Long dataframe listing the remaining number of trials per
  #   subject and emotion condition after inducing missing trials. It contains the 
  #   following columns: SUBJECTID, emotion, trialN (see Outputs section at the
  #   top of the script for more information). 
induceMissingTrials <- function(dfOriginal, caseDeletionPct) {
  # Create copy of the dfOriginal dataframe for inducing missing trials
  dfMissing <- data.frame(dfOriginal)
  
  # Extract age probability weights (one weight per subject)
  dfAgeWeight <- aggregate(ageWeight ~ SUBJECTID, dfOriginal, mean, 
                           na.action = na.omit)
  
  # Calculate the number of low trial-count subjects by multiplying caseDeletionPct
  # by the total number of subjects. If this value is not an integer, the output
  # is rounded up. 
  caseDeletionN <- ceiling((caseDeletionPct/100)*subjectN)
  
  # Randomly sample the subject IDs that will have low trial counts based on the 
  # specified age weights and the caseDeletionN variable
  subjectCaseDeletion <- sample(dfAgeWeight$SUBJECTID, caseDeletionN, 
                                replace = FALSE, prob=dfAgeWeight$ageWeight)
  
  # Calculate the maximum number of trials that can be removed from each condition
  # before the subject is considered to have a low trial count and would be
  # casewise deleted (e.g., if there are 50 trials, a maximum of 40 trials can
  # be removed for an included (not casewise deleted) subject)
  trialMissingThreshold <- emotionTrialN - 10
  
  # Loop through each subject and randomly select a subset of trials from each
  # condition to remove
  for (subject in dfAgeWeight$SUBJECTID) {
    
    # Generate the number of missing trials to induce for each emotion condition
    if (subject %in% subjectCaseDeletion) { 
      
      # For subjects with a low trial count, at least one emotion condition will 
      # have less than 10 trials remaining
      trialMissing <- c(sample(x=(trialMissingThreshold+1):emotionTrialN, size = 1),
                        sample(x=0:emotionTrialN, size = emotionN-1, replace = TRUE))                       
    } else { 
      # For subjects who do NOT have a low trial count, all emotion conditions
      # will have at least 10 trials
      trialMissing <- sample(x=0:trialMissingThreshold, size = emotionN, 
                             replace = TRUE)
    }
    
    # Shuffle order of emotion conditions and then loop through each condition
    # (This line is added so that one condition does not consistently have more
    # missing trials.)
    emotionLabelRand <- sample(emotionLabel)
    for (j in 1:length(emotionLabelRand)) {
      
      emotionTrialMissing <- trialMissing[j] # Extract the number of missing trials for this condition
      
      if (emotionTrialMissing != 0) { # If this emotion condition was not selected to have 0 missing trials
        
        # Find this subject and condition's rows in the dataframe
        subjectIndex <- which(dfMissing$SUBJECTID==subject & dfMissing$emotion==emotionLabelRand[j])
        
        # Extract the presentation number probability weights for this subject and condition
        subjectProbWeight <- dfMissing$presentNumberWeight[subjectIndex]
        
        # Randomly select the missing trials based on the specified probability 
        # weights and the number of missing trials
        subjectIndexMissing <- sample(subjectIndex, emotionTrialMissing, 
                                      replace = FALSE, prob=subjectProbWeight)
        
        # For these missing trials only, replace the meanAmpNC value with NA
        dfMissing[subjectIndexMissing,]$meanAmpNC <- NA
      }
    }
  }
  
  # Save the number of trials remaining for each subject and condition in a dataframe
  trialCount <- ddply(dfMissing, .(SUBJECTID, emotion), summarize, 
                      trialN = sum(!is.na(meanAmpNC)))
  names(trialCount)[3] <- 'trialN' # Update column name to trialN
  
  return(list(dfMissing, subjectCaseDeletion, trialCount)) # Return output variables
}

# pairTrials_RandomPerm: Function for pairing trials with exact match only
# - Format: 
#     list[dfMissing_pairedWide, fit.LMEMis] <- pairTrials_RandomPerm(dfMissing)
# - Inputs:
# - dfMissing: A copy of the dfOriginal after missing trials have been induced. 
#   Rows with missing trials have a meanAmpNC value of NA.
# - Output:
# - dfMissing_pairedWide: A wide dataframe with trials paired randomly. Missing trial pairings are designated with NA
pairTrials_RandomPerm <- function(dfMissing) {
  dfMissing_NoNA <- dfMissing[complete.cases(dfMissing), ] # Remove all NA rows (otherwise function will say that you've matched all pairs)
  
  # Initialize variable for storing paired data
  dfMissing_pairedWide <- NULL
  
  for (subject in unique(dfMissing_NoNA$SUBJECTID)) { 
    # Subset dfMissing_NoNA dataframe by subject
    dfMissing_NoNA_SubjectSubset <- dfMissing_NoNA[dfMissing_NoNA$SUBJECTID == subject,]
    
    # Find each emotion condition's rows
    emotionRows_A <- dfMissing_NoNA_SubjectSubset$emotion == "A"
    emotionRows_B <- dfMissing_NoNA_SubjectSubset$emotion == "B"

        # We sample both conditions A and B randomly so we get a random selection of actor/presentation number for both conditions
    # Otherwise, selecting 1:n may result in always pairing Actor 1/Trial 1 of Emotion A with a random trial from Emotion B
    dfMissing_NoNA_SubjectSubset$subclass[emotionRows_A] <- sample(1:sum(emotionRows_A))
    dfMissing_NoNA_SubjectSubset$subclass[emotionRows_B] <- sample(1:sum(emotionRows_B))
    
    # Convert dataframe with paired trials from long to wide
    dfMissing_pairedWide_SubjectSubset <- dfMissing_NoNA_SubjectSubset %>% 
      pivot_wider(names_from = emotion, names_sep = ".", values_from = c(meanAmpNC, ACTOR, presentNumber, presentNumberWeight))
    
    # If this subset does not have trials from both emotion conditions
    if (!("meanAmpNC.B" %in% colnames(dfMissing_pairedWide_SubjectSubset) & "meanAmpNC.A" %in% colnames(dfMissing_pairedWide_SubjectSubset))) { 
      next
    }
    
    # Calculate difference in amplitude between trial pairs
    dfMissing_pairedWide_SubjectSubset$meanAmpNC_BMinusA <- dfMissing_pairedWide_SubjectSubset$meanAmpNC.B - dfMissing_pairedWide_SubjectSubset$meanAmpNC.A
    
    # Remove all rows with NA
    dfMissing_pairedWide_SubjectSubset_NoNA <- dfMissing_pairedWide_SubjectSubset[complete.cases(dfMissing_pairedWide_SubjectSubset), ] # Remove all NA rows (otherwise function will say that you've matched all pairs)
    
    # Store this subject's paired trial dataframe in dfMissing_pairedWide
    dfMissing_pairedWide <- bind_rows(dfMissing_pairedWide, dfMissing_pairedWide_SubjectSubset_NoNA)
  }
  
  # Calculate average presentation number for each trial pair
  dfMissing_pairedWide$presentNumberAvg <- (dfMissing_pairedWide$presentNumber.A+dfMissing_pairedWide$presentNumber.B)/2

  # Fit LME model
  fit.LMEMis <- lmer(meanAmpNC_BMinusA ~ age + presentNumberAvg + (1|SUBJECTID), data=dfMissing_pairedWide, REML = TRUE)
  
  return(list(dfMissing_pairedWide, fit.LMEMis)) # Return dataframe with paired trials
  
}

#-----------------------------------------------------------------------
# 2. LOAD SIMULATED DATA FILE

# Specify filepath of simulated data file
filename <- 'T:/Projects/InProgress/LME_DifferenceWaves&LatencyERP/Data/01_SEREEGASimulation/MatlabAndRCode/00_SampleCodeForTestingDifferenceWavePairing/Sample0751-NCMeanAmpOutput.csv'

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
list[dfMissing, subjectCaseDeletion, trialCount] <- induceMissingTrials(dfOriginal,
                                                                        caseDeletionPct) 

#------------------------------------------------------------------------
# 7. PAIR TRIALS AND FIT LME MODEL WITH TRIAL-LEVEL DATASET AFTER INDUCING TRIAL MISSINGNESS

# In our full script, we call pairTrials_RandomPerm 120,000,000 times 
# (=10,000 permutations per dataset * 1,000 datasets * 4 types of induced missing trials for each dataset * 3 patterns of missing data)

# In this example, we have one dataset. We randomly permute this dataset 10,000 times,
# store emmeans results for each iteration in separate array, then calculate final emmeans summary
rpIter = 10 #DTL 10000
presentAvgValue = 5.5 # Value needed for emmeans calculations
ageN <- 2 # Value needed for extracting rows from emmeans output
ageArray <- c(-1.998, 0) # Array needed for extrating emmeans output 

LMEMis_output_RP_allIter <- NULL # Temporary variable for storing emmeans for each random perm iteration

for (i in 1:rpIter) {
  # Pair trials with random permutation function
  list[dfMissing_pairedWide_RP, fit.LMEMis_RP] <- pairTrials_RandomPerm(dfMissing)
  
  # Extract marginal means from LME model
  mLME <- emmeans::emmeans(fit.LMEMis_RP, pairwise~age, mode = "satterthwaite", 
                           lmerTest.limit = 240000, at = list(presentNumberAvg = c(presentAvgValue)))
  
  # Extract output values for each emotion condition 
  LMEMis_output_RP_oneIter <- data.frame(summary(mLME, infer = c(TRUE, TRUE))$emmeans)
  # Check if each true population emotion condition value is located within the
  # model's 95% confidence interval for the emotion condition
  for (ageNum in 1:ageN) {
    LMEMis_output_RP_oneIter$inCL[ageNum] <- between(ageArray[ageNum], 
                                                     LMEMis_output_RP_oneIter$lower.CL[ageNum], 
                                                     LMEMis_output_RP_oneIter$upper.CL[ageNum])
  }
  names(LMEMis_output_RP_oneIter)[1] <- c('age') # Update column names
  names(LMEMis_output_RP_oneIter)[2] <- c('estimate')
  
  LMEMis_output_RP_oneIter$iteration <- i
  LMEMis_output_RP_allIter <- bind_rows(LMEMis_output_RP_allIter, LMEMis_output_RP_oneIter)
}

# We then extract summary values from LMEMis_output_RP_allIter

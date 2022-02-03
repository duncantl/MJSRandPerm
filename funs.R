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
    #!! Better to put names on these elements.
  
}

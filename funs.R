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


# 2nd parameter Remove all NA rows (otherwise function will say that you've matched all pairs)
pairTrials_RandomPerm <-
    #
    # adding the formula as an argument here.
    # Would be better to explicitly pass it so that the environment
    # is that of the caller, i.e. the globalenv()
    #
    #
function(dfMissing, dfMissing_NoNA = dfMissing[complete.cases(dfMissing), ], formula = meanAmpNC_BMinusA ~ age + presentNumberAvg + (1|SUBJECTID))
{
  dfMissing_pairedWide = by(dfMissing_NoNA, dfMissing_NoNA$SUBJECTID, mkSubject)

    # data.table's rbindlist.  For 250 iterations of the loop, seems to save about 1 second overall.
  dfMissing_pairedWide =  structure(rbindlist(dfMissing_pairedWide), class = "data.frame")  # do.call(rbind, c(dfMissing_pairedWide, deparse.level = 0, make.row.names = FALSE, factor.exclude = FALSE))

  dfMissing_pairedWide = mkWide(dfMissing_pairedWide)
    
  # Remove all rows with NA
  ### Already done by mkSubject()
  # dfMissing_pairedWide <- dfMissing_pairedWide[complete.cases(dfMissing_pairedWide), ] # Remove all NA rows (otherwise function will say that you've matched all pairs)

  # Calculate difference in amplitude between trial pairs  
  dfMissing_pairedWide$meanAmpNC_BMinusA <- dfMissing_pairedWide$meanAmpNC.B - dfMissing_pairedWide$meanAmpNC.A
  
  # Calculate average presentation number for each trial pair
  dfMissing_pairedWide$presentNumberAvg <- (dfMissing_pairedWide$presentNumber.A + dfMissing_pairedWide$presentNumber.B)/2

#return(dfMissing_pairedWide)#XXXX

    # Since we kept SUBJECTID, age, emotion and ACTOR as character vectors to make rbind() etc. faster,
    # we need to convert them to factor()s for fitting the model.
    # Only SUBJECTID and age as ACTOR and emotion are not used in the model.
  dfMissing_pairedWide[ c("SUBJECTID", "age")] =  lapply(dfMissing_pairedWide[ c("SUBJECTID", "age")], factor)

#Just for testing making the data.frame but not fitting the model
#return(dfMissing_pairedWide)
  
  # Fit LME model
    # Don't seem to be using
    # c("ACTOR.A", "ACTOR.B", "presentNumberWeight.A", "presentNumberWeight.B", "ageWeight", "subclass")
    # So why computing these in mkSubject
    #  If we reduce the data frame to
    #    dfMissing_pairedWide = dfMissing_pairedWide[c("SUBJECTID", "meanAmpNC_BMinusA", "age", "presentNumberAvg")]
    # we get the same terms() object in the model. So it doesn't seem that the other variables not in the formula are being
    # included indirectly, and we wouldn't expect them to be.

# Could explicitly set the environment of the formula if it is not a parameter that the caller can control.    
#  formula = meanAmpNC_BMinusA ~ age + presentNumberAvg + (1|SUBJECTID)
#  environment(formula) = globalenv()
  fit.LMEMis <- lmer(formula, data = dfMissing_pairedWide, REML = TRUE)

# If the formula has this call frame as the environment, probably want to remove the unwanted variables here as they
# will continue to occupy memory.
#  on.exit(rm(dfMissing_pairedWide, uids, fit.LMEMis))
    
  list(df = dfMissing_pairedWide, fit = fit.LMEMis) # Return dataframe with paired trials
    # Better to put names on these elements. So I have.
    # But since the df is not being used, probably best not to return it. Also, it is captured by the environment
    # of the formula so I would remove it. That's what the on.exit() does.
    # or explicitly set the environment of the formula.
  
}


mkSubject =
    #
    #
function(dfMissing_NoNA_SubjectSubset)
{
    # Find each emotion condition's rows
    emotionRows_A <- dfMissing_NoNA_SubjectSubset$emotion == "A"
    numA = sum(emotionRows_A)
    numB = length(emotionRows_A) - numA

    # If this subset does not have trials from both emotion conditions, bail out.
    if (any(c(numA, numB) == 0))  
      return(NULL)    

    # We sample both conditions A and B randomly so we get a random selection of actor/presentation number for both conditions
    # Otherwise, selecting 1:n may result in always pairing Actor 1/Trial 1 of Emotion A with a random trial from Emotion B
# need to create this if using a tibble, not a data.frame    
# dfMissing_NoNA_SubjectSubset$subclass = 1L
    dfMissing_NoNA_SubjectSubset$subclass[emotionRows_A] <- sample.int(numA)
    dfMissing_NoNA_SubjectSubset$subclass[!emotionRows_A] <- sample.int(numB)

    x = dfMissing_NoNA_SubjectSubset 

    # Assuming that
    #  1) we don't want cases where there is only one record for given value of subclass.
    #      In pivot_wider() these give NA values for meanAmpNC.A or meanAmpNC.B  and so they are omitted
    #      in the next step in the original code.
    #  2)  for the remaining rows, there will be exactly two rows for each subclass.
    # If either of these is not true, need to rethink.
    #
    # With these,
    #   a) remove the rows corresponding to a value of subclass that has only 1 value in the dataset.
    #   b) order the rows by subclass, and within subclass by emotion.
    #   c) we now have the rows in pairs, i.e., 1, 2  and 3, 4, and 5, 6   have the same subclass
    #      So the result is to take the meanAmpNC, ACTOR, presentNumber, presentNumberWeight from the second row in each pair
    #      and append it to the first row in each pair
    #      BUT VECTORIZE this.
    #
    # This replaces pivot_wider.  It is much more specific/less general, but appears to give the same result.
    # 

    x = x[ x$subclass %in% x$subclass[duplicated(x$subclass)], ]
    x[ order(x$subclass, x$emotion) , ]
}


mkWide =
function(x)
{
    i = seq(1, nrow(x) - 1, by = 2)

    # x[i, c("meanAmpNC.B", "ACTOR.B", "presentNumber.B", "presentNumberWeight.B")] = x[i+1, c(6, 4, 5, 7)]
    # Omit ACTOR.B and presentNumberWeight.B as you don't use them.  Leave the ones for A as we would have to explicitly remove them.
    x[i, c("meanAmpNC.B", "presentNumber.B")] = x[i+1, c(6,  5)]
    names(x)[c(6, 4, 5, 7)] = c("meanAmpNC.A", "ACTOR.A", "presentNumber.A", "presentNumberWeight.A")

    x[i, ]


    # If we want the same order as pivot_wider() returns, but not necessary.
    #    return(x[i, c(1L, 2L, 8L, 9L, 4L, 10L, 5L, 11L, 6L, 12L, 7L, 13L) ])        
}

        




########################################
if(FALSE) {
# Unused.

mkRecordWide =
function(x)
{
    ans = x[1, c(1, 2, 8, 9,   6, 4, 5, 7)]
    names(ans)[5:8] = c("meanAmpNC.A", "ACTOR.A", "presentNumber.A", "presentNumberWeight.A")
    ans[ 1, c("meanAmpNC.B", "ACTOR.B", "presentNumber.B", "presentNumberWeight.B")] = x[2, c(6, 4, 5, 7) ]
    ans
}



}



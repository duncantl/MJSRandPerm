Big "thank you" to MJ & Serena for this example.

This is simulation that involves
+ 

See [Changes.md](Changes.md) for some of the very high-level changes related
to 
  + changing computations done in loops and moving them outside and after the loop in a vectorized
    manner
  + suggestions for simplifying/removing  code that is redundant

Most of these changes are in the updated script
[MJ&Serena_RandPermutationScript_20220202.R](MJ&Serena_RandPermutationScript_20220202.R).


## Timing the Original and New Script


The original script (with the number of iterations adjusted)
```r
source("origScript.R")
```

The modified version can be run 
```r
source("funs.R")
source("MJ&Serena_RandPermutationScript_20220202.R")
```

We probably have to run each of these several times to get the byte-code compilation.
This is a reason for separating the functions from origScript.R so that we
are not redefining them each time we source that script.



## Comparing Core Function between Original and Modified


Compare with original **function pairTrials_RandomPerm** - not the scripts
```r
library(plyr) 
library(lme4) 
library(lmerTest) 
library(gsubfn) 
library(data.table) 
library(dplyr) 
library(performance) 
library(afex) 
library(emmeans) 
library(car) 
library(stringr) 
library(MatchIt) 
library(tidyr) 

dfMissing = readRDS("dfMissing.rds")


e = new.env()
source("origFuns.R", e)
invisible(replicate(5, e$pairTrials_RandomPerm(dfMissing)))
tm.orig = replicate(10, system.time(e$pairTrials_RandomPerm(dfMissing)))

source("funs.R")
invisible(replicate(5, pairTrials_RandomPerm(dfMissing, formula = meanAmpNC_BMinusA ~ age + presentNumberAvg + (1|SUBJECTID))))
tm = replicate(10, system.time(pairTrials_RandomPerm(dfMissing, formula = meanAmpNC_BMinusA ~ age + presentNumberAvg + (1|SUBJECTID))))

summary(tm.orig[3,])/summary(tm[3,])
```

```
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  7.107   7.088   7.187   7.048   7.121   6.326 
```


If we change the pairTrials_RandomPerm() function in each of origFuns.R and funs.R 
to return the data frame just before fitting the lmer model, we get 
```r
summary(tm.orig[3,])/summary(tm[3,])
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  15.52   16.45   16.48   16.39   16.65   16.03 
```
So for creating the data.frame for the model fitting, we have a speedup of a factor of 16.
This declines to a factor of 7 when we fit the model.

When fitting the model, we should provide a control argument that provides
good starting points, rather than starting the search from scratch each time.





## mkSubject

+ check if we have A and B values for emotion **before** calling pivot_wider() rather than checking for
   meanAmpNC.A and meanAmpNC.B in the results of pivot_wider.  Doing unnecessary work in
   pivot_wider() that is both expensive and you'll discard.

+ Replace the pivot_wider() with vectorized operations that are more specific but avoids unnecessary
  computations by 
    + omitting subclass values with only one record before widening, and 
    + by doing this in base R

+ Instead of widening the data.frame in each mkResult() call, leave the entire data.frame
  and do the widening in pairTrials_RandomPerm()




## pairTrials_RandomPerm





## Environment of the Formula, Capturing Objects and Garbage Collection.

```r
z = pairTrials_RandomPerm(dfMissing, formula = meanAmpNC_BMinusA ~ age + presentNumberAvg + (1|SUBJECTID))
formula(z[[2]])
environment(formula(z[[2]]))
```


```r
z2 = pairTrials_RandomPerm(dfMissing)
formula(z2[[2]])
environment(formula(z2[[2]]))
ls(environment(formula(z2[[2]])))
```





###

```r
source("funs.R")
z = pairTrials_RandomPerm(dfMissing, formula = meanAmpNC_BMinusA ~ age + presentNumberAvg + (1|SUBJECTID))
```


### Current Profiling
Corresponding to git hash 9d6f3926fbb6 but with pairTrials_RandomPerm() return the data.frame and
not fitting the lmer model, so we can look just at the data.frame creation.

```r
Rprof("new.prof")
tm = replicate(10, pairTrials_RandomPerm(dfMissing, formula = meanAmpNC_BMinusA ~ age + presentNumberAvg + (1|SUBJECTID)))
Rprof(NULL)
head(summaryRprof("new.prof")$by.self, 10 )
```


```
                self.time self.pct total.time total.pct
"[.data.frame"       0.44    16.42       1.70     63.43
"[["                 0.30    11.19       1.12     41.79
"[[.data.frame"      0.30    11.19       0.82     30.60
"rbind"              0.28    10.45       0.66     24.63
"sys.call"           0.26     9.70       0.26      9.70
"%in%"               0.14     5.22       0.46     17.16
"FUN"                0.10     3.73       2.68    100.00
"sample.int"         0.10     3.73       0.10      3.73
"<Anonymous>"        0.08     2.99       0.76     28.36
"order"              0.08     2.99       0.24      8.96
"all"                0.06     2.24       0.06      2.24
"["                  0.04     1.49       1.74     64.93
"vapply"             0.04     1.49       0.10      3.73
"anyDuplicated"      0.04     1.49       0.06      2.24
"duplicated"         0.04     1.49       0.06      2.24
```

+ If we make dfMissing a tibble and modify mkSubject() to assign subclass first before doing it on subsets (which data.frame handles),
  the code doesn't call [.data.frame, but is slightly slower. So no gain with tibble, and the regular data.frame runs in about 91-93% of the time the tibble version does.

+ focusing on rbind(), add deparse.level = FALSE, make.row.names = FALSE, factor.exclude = FALSE
   + This reduces the % for rbind to 7.41

+ The [.data.frame calls are for 
   + `[.data.frame`(dfMissing, complete.cases(dfMissing), )  - one time subsetting by complete.cases() in the default value for the second parameter of pairsTrial_RandomPerm
   +  from by/tapply    `[.data.frame`(data, x, , drop = FALSE)
   +  x = x[ x$subclass %in% x$subclass[duplicated(x$subclass)], ]
   + x = x[ order(x$subclass, x$emotion) , ]
   + x[i, c("meanAmpNC.B", "presentNumber.B")] = x[i+1, c(6,  5)]
   + x[i, ] from mkWide

  The only one that seems like it could be improved is #5, adding meanAmpNC.B, presentNumber.B as
  columns, perhaps with cbind(), but that is [<-.data.frame, not [.data.frame so no!


+ sys.call() is being called from [[.data.frame to check for argument names not "exact" to issue a warning. This is unnecessary overhead that should be
  controllable. And should be found in names(list(...)).
  
+ Can we suppress the rownames on dfMissing and make a difference?  

```
                self.time self.pct total.time total.pct
"%in%"               0.32    12.40       0.40     15.50
"[[.data.frame"      0.24     9.30       0.78     30.23
"[.data.frame"       0.18     6.98       1.56     60.47
"[<-.factor"         0.18     6.98       0.20      7.75
"order"              0.12     4.65       0.32     12.40
"FUN"                0.10     3.88       2.58    100.00
"rbind"              0.10     3.88       0.72     27.91
"[.factor"           0.10     3.88       0.20      7.75
"[["                 0.08     3.10       0.86     33.33
"<Anonymous>"        0.08     3.10       0.82     31.78
```

+  [<-.factor seems to come from do.call(rbind, dfMissing_pairedWide)
   + caused by having to check the levels of the factors each time, yet we know they are the same.
   + XXX convert factors in dfMissing to character vectors. Change only when fitting model.  XXXX make ce
```   
dfMissing[ sapply(dfMissing, is.factor) ] = lapply(dfMissing[ sapply(dfMissing, is.factor) ], as.character)
```






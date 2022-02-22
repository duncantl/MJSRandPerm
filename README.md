## Big "thank you" to MJ & Serena for this example.


## Current Summary

+ We seem to have improved the creation of the data.frame for fitting each model by a factor of
   between 16 and 17.

+ When we combine this with fitting the model, the speedup is a factor of about 7.
   + The improvement from creating the data.frame gets diminished as the rest of the code takes a
     non-trivial amount of time.


+ Profiling the entire script
```
                       self.time self.pct total.time total.pct
"ls"                       11.92    18.67      11.94     18.70
"[.data.frame"              7.72    12.09      12.76     19.98
"stopifnot"                 4.50     7.05       4.54      7.11
"order"                     3.60     5.64       4.06      6.36
"rbind"                     3.54     5.54       4.04      6.33
"<Anonymous>"               1.52     2.38      34.30     53.71
"utils::getAnywhere"        1.52     2.38      15.08     23.61
".Call"                     1.34     2.10       1.52      2.38
".getClassesFromCache"      1.32     2.07       1.32      2.07
"data.frame"                1.24     1.94       3.62      5.67
"$"                         1.04     1.63       2.86      4.48
"initialize"                1.02     1.60       7.48     11.71
"%in%"                      0.94     1.47      13.50     21.14
"validObject"               0.86     1.35       1.90      2.98
".nextMethod"               0.82     1.28       2.46      3.85
"apply"                     0.80     1.25       2.24      3.51
"model.frame.default"       0.76     1.19       1.58      2.47
"jacobian.default"          0.60     0.94       1.26      1.97
"installClassMethod"        0.60     0.94       0.88      1.38
"getClassDef"               0.56     0.88       2.04      3.19
```
+ 2 iterations of the loop

+ collecting the call stacks for ls() just running loop.R, we have 289 calls to ls(), 9 unique call stacks .
   + 3 are happening in the byte-code compiler.
      + we can try to byte compile these ahead of time (e.g. put code in a package)
	    or just let this one-time events happen and diminish in signifance across more
		iterations of the loop.
   + 6 recover_data calling .get.outside.method("recover_data", cl),  "emm_basis"

   + debugging emm_basis, looking for emm_basis.lmerModLmerTest
      + doesn't find it so just calls UseMethod()
	  
   + Checking each of the 11 calls to  .get.outside.method,  we find all return NULL,
     so a complete waste of time in this case.
```r
val = vector("list", 300)
ctr = 1L
trace(emmeans:::.get.outside.method, exit = quote({val[[ctr ]] <<- returnValue(); ctr<<- ctr+1L}),
print = FALSE)
```
+ We replace .get.outside.method with a function that just returns NULL
```r
ns = getNamespace("emmeans")
unlockBinding(".get.outside.method", ns)
assign(".get.outside.method", function(generic, cls) NULL, ns)
```

+ Looking at stopifnot(). rpIter = 20 iterations of the loop
   + 2541 calls to stopifnot; 350 unique call stacks
   + srcfilecopy() called from source()
   + lmerControl() 
      + to check if object is a list.
	  + .makeCC x 3
   + We'll make stopifnot a no-op.  If we just return invisible(), we get an error
     in `KhatriRao(sm, t(mm)) : 'list' object cannot be coerced to type 'integer'`
	 So we force the evaluation of the arguments as this is probably lazy evaluation.
```
old.stopifnot = stopifnot
ns = getNamespace("base")
unlockBinding("stopifnot", ns)
assign("stopifnot", function (..., exprs, exprObject, local = TRUE) { list(...); if(!missing(exprs)) exprs; if(!missing(exprObject)) exprObject;  invisible()}, ns)
```

+ With .get.outside.method and stopifnot made degenerate, we get
```
                      self.time self.pct total.time total.pct
"[.data.frame"             0.50    12.95       0.90     23.32
"order"                    0.34     8.81       0.38      9.84
"rbind"                    0.32     8.29       0.36      9.33
"<Anonymous>"              0.20     5.18       1.58     40.93
".Call"                    0.16     4.15       0.16      4.15
"data.frame"               0.10     2.59       0.32      8.29
"validObject"              0.10     2.59       0.16      4.15
".identC"                  0.10     2.59       0.10      2.59
"initialize"               0.08     2.07       0.60     15.54
"apply"                    0.08     2.07       0.22      5.70
"model.frame.default"      0.08     2.07       0.20      5.18
".all.vars"                0.08     2.07       0.12      3.11
"genD.default"             0.06     1.55       0.24      6.22
"$"                        0.06     1.55       0.18      4.66
".adj.p.value"             0.06     1.55       0.06      1.55
".class1"                  0.04     1.04       0.14      3.63
"paste"                    0.04     1.04       0.10      2.59
"%in%"                     0.04     1.04       0.06      1.55
"getClassDef"              0.04     1.04       0.06      1.55
"jacobian.default"         0.04     1.04       0.06      1.55
```

+ Running
```r
rpIter = 250
e = new.env()
source("origFuns.R", e)
z = e$pairTrials_RandomPerm(dfMissing) # evaluate multiple times to byte compile
tm.orig = system.time(source("origScript_noFuns.R", e))

tm.new = system.time(source("MJ&Serena_RandPermutationScript_20220202.R"))

tm.orig/tm.new
    user   system  elapsed 
6.627625 4.354592 6.603736 

rpIter = 1000
```


   
## Some Suggestions/Questions

See [Changes.md](Changes.md) for some of the very high-level changes related
to 
  + changing computations done in loops and moving them outside and after the loop in a vectorized
    manner
  + suggestions for simplifying/removing  code that is redundant

Most of these current changes are in the updated script
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


Profiling data from the modified version that doesn't use the tibble or tidyverse functions.
There 
```
                      self.time self.pct total.time total.pct
"[<-.data.frame"           0.06    16.67       0.06     16.67
"exists"                   0.06    16.67       0.06     16.67
"withCallingHandlers"      0.06    16.67       0.06     16.67
".Call"                    0.02     5.56       0.18     50.00
"DataMask$new"             0.02     5.56       0.08     22.22
"findCenvVar"              0.02     5.56       0.08     22.22
"group_data"               0.02     5.56       0.06     16.67
"enexpr"                   0.02     5.56       0.02      5.56
"length"                   0.02     5.56       0.02      5.56
"new_data_frame"           0.02     5.56       0.02      5.56
"paste0"                   0.02     5.56       0.02      5.56
"standardGeneric"          0.02     5.56       0.02      5.56
```



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

+ The approach relies on 2 assumptions based that need to be true.
  + for each SUBJECTID and a given value of subclass
    + we either have a single record and so no A AND B ACTOR so we will discard that record for
      fitting the model
    + we have exactly 2 records, one for A and one for B and we combine those in the wide data.frame

  + Assuming this, 
    + for each SUBJECTID, we make a narrow data.frame with the pairs of records
       in successive rows - using order(SUBJECTID, subclass).
  	+ we combine these data.frames so we have all SUBJECTID and pairs of rows.
	+ we extract rows 1, 3, 5, 7, .... 
	+ we get meanAmpNC and presentNumber from rows 2, 4, 6, 8, .... and add them to rows 1, 3, 5, 7,
      .. as meanAmpNC.B, presentNumber.B
    + this is vectorized.	  
	  

+ [.data.frame is the resulting bottleneck function



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






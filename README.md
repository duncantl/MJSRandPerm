## Notes


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

Compare with original
```r
e = new.env()
source("origFuns.R", e)
tm.orig = replicate(10, system.time(e$pairTrials_RandomPerm(dfMissing)))
```

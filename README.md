

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
Corresponding to git hash 9d6f3926fbb6

```r
Rprof("new.prof")
tm = replicate(10, system.time(pairTrials_RandomPerm(dfMissing, formula = meanAmpNC_BMinusA ~ age + presentNumberAvg + (1|SUBJECTID))))
Rprof(NULL)
head(summaryRprof("new.prof")$by.self )
```

```
                self.time self.pct total.time total.pct
"gc"                 5.22    46.94       5.22     46.94
"[.data.frame"       0.46     4.14       2.08     18.71
"[[.data.frame"      0.32     2.88       1.04      9.35
"<Anonymous>"        0.28     2.52       1.94     17.45
"%in%"               0.28     2.52       0.52      4.68
".Call"              0.28     2.52       0.28      2.52
"sys.call"           0.20     1.80       0.20      1.80
"is"                 0.16     1.44       0.34      3.06
"NextMethod"         0.16     1.44       0.16      1.44
"[["                 0.14     1.26       1.18     10.61
```


Compare with original
```r
e = new.env()
source("origFuns.R", e)
tm.orig = replicate(10, system.time(e$pairTrials_RandomPerm(dfMissing)))
```

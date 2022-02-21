

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


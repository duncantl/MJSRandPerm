√ What packages does it load.
√ What functions does it define.
  + what global variables do these use?
  + are they defined in the script?
What top-level variables are assigned?  re-assigned?  
What files does it read
What files does it write
Does it produce plots
What are the types of the values?
What functions are called?




```
library(CodeDepends)
sc = readScript("MJ&Serena_RandPermutationScript_20220202.R")
unlist(sapply(info, slot, "libraries"))
```

```
funs = findFunctionDefs(e)
names(funs)
```

```
gv = lapply(funs, function(f) getGlobals(eval(f, globalenv())))
gvars = unique(unlist(lapply(gv, `[[`, "variables")))
```

varDefs = unlist(lapply(info, slot, "outputs"))



Variables that are redefined
```r
dsort(table(unlist(lapply(info, slot, "outputs"))))
```




Say how we find that induceMissingTrials() is called only once in the script,
but pairTrials_RandomPerm() is called inside the/a loop.




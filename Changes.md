

How much speedup is needed to make this viable?

Most likely in the lme4/nlme fitting, but maybe generating the permutations.

Looks like can parallelize this. So get on a large machine and run in parallel.


Within the loop, there is a loop from 1:ageN and then set the first two names names of the result to
age and estimate. Is the same value of 2 for  ageN = 2 and setting the 2 names a coincidence or
supposed to be connected.  Probably a coincidence.



Rather than append  LMEMis_output_RP_oneIter to LMEMis_output_RP_allIter in each iteration,
+ pre-allocate and fill in rows
+ keep as a list until the very end and then use do.call(rbind, LMEMis_output_RP_allIter)


Set the names later.

Do the between at the end in vectorized form. 2 steps for the young and old corresponding to the two
values of ageArray.


+ dfMissing_pairedWide_RP is assigned but never used in each iteration.
  + And this involves the list[] which probably has a non-trivial overhead and uses non-standard evaluation.
  + So we can simplify this in the loop with
```
  fit.LMEMis_RP <- pairTrials_RandomPerm(dfMissing)[[2]]
```
  + Can also think about simplifying the function and not computing/returning the first element
  
Good to put names on the elements of the list returned by pairTrials_RandomPerm
  + this allows us to use 
  `pairTrials_RandomPerm(dfMissing)$fit`
  rather than `pairTrials_RandomPerm(dfMissing)[[2]]`


  


These changes seem to ADD 10 seconds, i.e., for rpIter = 100
+ origScript.R takes 130 seconds
+ the modified script (commit 10264bb4a) takes 140.
Not sure these numbers are reliable, and they should change when we do a lot more iterations.


¿ Remove emmeans:: qualifier for speed ?




##
induceMissingTrials() seems to be only called once.



## Profiling

### ls()

```r
ls.stack = collectCallStacks(ls)
box = new.env(); system.time(source("MJ&Serena_RandPermutationScript_20220202.R", box))
ls.calls = ls.stack()
length(ls.calls)
```
1504 calls for 10 iterations.

table(sapply(ls.calls, length))

 14  19  21  30  62 
600 600 300   2   2 





Using lldb to print out the symbol in each call to .External

ll = readLines("lldb.external.output")
routines = ll[grep("^Command #2", ll) + 1]
dsort(table(routines))

This is for 10 iterations in the original script.
```
routines
    vctrs_new_data_frame                  vctrs_c 
                   22310                    19600 
        rlang_ext2_call2 rlang_ext_capturearginfo 
                   10000                     8000 
         rlang_ext2_exec          rlang_ext2_eval 
                    8000                     5100 
   rlang_ext_dots_values              vctrs_cbind 
                    4000                     1500 
    vctrs_recycle_common     rlang_ext_arg_match0 
                    1100                      700 
             C_compcases              vctrs_rbind 
                     521                      510 
           magrittr_pipe     rlang_ext2_eval_tidy 
                     500                      500 
             C_termsform        vctrs_cast_common 
                     110                      100 
       vctrs_type_common             C_modelframe 
                     100                       81 
           C_modelmatrix 
                      50 
```


Bases on a single iteration of the original script, we have
```
             "C_termsform"             "C_modelframe" 
                        12                          9 
             "C_compcases"     "vctrs_new_data_frame" 
                        53                       2951 
"rlang_ext_capturearginfo"          "rlang_ext2_eval" 
                      1700                        600 
    "rlang_ext_arg_match0"        "vctrs_type_common" 
                       250                        100 
       "vctrs_cast_common"                  "vctrs_c" 
                       100                       2050 
    "vctrs_recycle_common"            "magrittr_pipe" 
                       200                         50 
        "rlang_ext2_call2"    "rlang_ext_dots_values" 
                      1000                        400 
         "rlang_ext2_exec"     "rlang_ext2_eval_tidy" 
                       800                         50 
             "vctrs_cbind"              "vctrs_rbind" 
                       150                         51 
           "C_modelmatrix" 
                         5 
```



After profiling and seeing .External2 as the most time consuming function,
we used LLDB to get the names of the routines being called via .External2.
That shows a lot of vctrs, rlang, magrittr_pipe (see Changes.md).
So where are these coming from. Let's look at the functions used in
pairTrials_RandomPerm.


```
g = getGlobals(pairTrials_RandomPerm)
gfuns = unique(g$functions)
where = sapply(gfuns, find)
notFromBase = !sapply(where, function(x) length(x) == 1 && x == "package:base")
where[notFromBase]
```
```
$complete.cases
[1] "package:stats"

$`%>%`
[1] "package:tidyr"   "package:stringr" "package:dplyr"  

$pivot_wider
[1] "package:tidyr"

$bind_rows
[1] "package:dplyr"

$lmer
[1] "package:lmerTest" "package:afex"     "package:lme4"    

$list
[1] ".GlobalEnv"     "package:gsubfn" "package:base"  
```

There is only one use of %>% in the function
```
dfMissing_NoNA_SubjectSubset %>% 
      pivot_wider(names_from = emotion, names_sep = ".", values_from = c(meanAmpNC, ACTOR, presentNumber, presentNumberWeight))
```
and it is unnecessary as it is more typing than just adding it as the first argument in the call.



As we saw in the loop, we can avoid bind_rows  and concatenation at each iteration with
do.call(rbind, )



+ sample(1:sum(emotionRows_A)) can be sample(sum(emotionRows_A))

+ Might be better or worse to do table(emotion) and then access the "A" and "B" elements.
  + May be overhead.
  
+ Compute dfMissing_pairedWide_SubjectSubset$meanAmpNC_BMinusA   at the end of the loop on the
  entire combined data.frame.

In the test for meanAmpNC.B in the names, replace & with && since scalar comparisons.


?  When removing all rows with NA, are there specific columns that can actually contain a NA,  or
   can any column.
   
   
   
   
## Removing confusing multiple return assignments that are not used - use of list[]   

```
library(rstatic)
ast = to_ast(parse("origScript_noFuns.R"))
df = find_nodes(ast, is_symbol, "trialCount")   
df = find_nodes(ast, is_symbol, "subjectCaseDeletion")
```

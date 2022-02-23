library(codetools)
library(CodeAnalysis)

#
# MJ&Serena_RandPermutationScript_20220202.R
# loop.R
# funs.R
# vec.R
# setNSFunctions.R
#

findPos =
function(x)
{
    a = sapply(x, function(x) getAnywhere(x)$where[1])
    a[ !is.na(a) & ! (a %in% c("package:base", "package:stats")) ]
}

searchFuns = 
function(file)
{        
    f = getFunctionDefs(file)
    g = lapply(f, getGlobals)
    w = lapply(g, function(x) findPos(x$functions))
}

searchScript =
function(file)
{
    g = getGlobals(parse(file))
    findPos(unique(g2$functions))
}


w = lapply(c("funs.R", "vec.R", "setNSFunctions.R"), searchFuns)

table(unlist(w))

w2 = lapply(c("MJ&Serena_RandPermutationScript_20220202.R", "loop.R"), searchScript)

table(unlist(c(w, w2)))


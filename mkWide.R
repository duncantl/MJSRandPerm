mkWide =
    #
    # are there always going to be at most 2 rows for each subclass value?
    #
    #
    #
function(data, names_from, values_from, names_sep = ".")    
{
    p = split(data, data$subclass)
    p = p[ sapply(p, nrow) > 1 ]

    do.call(rbind, lapply(p, mkWideSub))
}

mkWideSub =
function(d)
{
    vars = c("emotion", "meanAmpNC", "ACTOR", "presentNumber", "presentNumberWeight")
    tmp = split(d[, vars], d$emotion)
    tmp2 = lapply(tmp, function(x) { names(x) = paste(names(x), x$emotion, sep = "."); x[, -1]})
    otherVars = c("SUBJECTID", "age", "ageWeight", "subclass")
    cbind(d[1, otherVars], tmp2$B, tmp2$A)
}

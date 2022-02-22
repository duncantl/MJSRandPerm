pairRecords1 = 
function(x)
{    
    ans = by(x, x$SUBJECTID, mkSubject)
      # data.table's rbindlist.  For 250 iterations of the loop, seems to save about 1 second overall.    
    structure(rbindlist(ans), class = "data.frame")
}

pairRecords2 =
function(x)
{
    x2 = x[ order(x[[1]], x[[3]]), ]
    tt = table(x[[1]], x[[3]])
    x2$subclass = unlist(apply(tt, 1, function(x) c(sample.int(x[1]), sample.int(x[2]))))
#    id = paste(x2[[1]], subclass, x2[[3]], sep = ".")
    ans = x2[ order(x2[[1]], x2$subclass, x2[[3]]) , ]
    # now remove the records that don't have a matching record.
    # as there were weren't enough B's for the As, or As for the Bs
    mask = unlist(apply(tt, 1, function(x){ mn = min(x); rep(c(TRUE, FALSE), c(2*mn, sum(x) - 2*mn))}))
#    mask = unlist(mapply(function(mn, num) rep(c(TRUE, FALSE), c(2*mn, num)), rowSums(tt), apply(tt, 1, min), SIMPLIFY = FALSE))
    # Instead of apply(tt, 1, min), we can vectorize this with
    # mn = tt[,1]
    # w = tt[,1] > tt[,2]
    # mn[w] = tt[w, 1]
    #
    # and rowsSums(tt) can be done tt[,1] + tt[,2].   But rowSums() may be slightly faster???
    #
    ans[mask,]
}

setNSFunctions =
function()
{
    oldFuns = list()

    ns = getNamespace("emmeans")

    oldFuns$.get.outside.method = list(ns = ns, fun = get(".get.outside.method", ns))
    unlockBinding(".get.outside.method", ns)
    assign(".get.outside.method", function(generic, cls) NULL, ns)
    lockBinding(".get.outside.method", ns)

    ns = getNamespace("base")
    oldFuns$stopifnot = list(ns = ns, fun = stopifnot)
    unlockBinding("stopifnot", ns)
    assign("stopifnot", function (..., exprs, exprObject, local = TRUE) { list(...); if(!missing(exprs)) exprs; if(!missing(exprObject)) exprObject;  invisible()}, ns)
    lockBinding("stopifnot", ns)
    
    ns = getNamespace("base")
    f = get("[[.data.frame", ns)
    oldFuns$"[[.data.frame" = list(ns = ns, fun = f)
    body(f) = body(f)[-3]

    unlockBinding("[[.data.frame", ns)
    assign("[[.data.frame", f, ns)
    lockBinding("[[.data.frame", ns)

    invisible(oldFuns)
}


resetNSFunctions =
function(prev)
{
   invisible( mapply(function(var, x)  {
                       unlockBinding(var, x$ns)
                       assign(var, x$fun, x$ns)
                       lockBinding(var, x$ns)        
              },  names(prev), prev))
}



import lldb

counts = {}

def update(frame, bp_loc, dict):
    ty = frame.FindVariable("call").GetValue()
    options = lldb.SBExpressionOptions()
    options.SetFetchDynamicValue(True)
    pname = frame.EvaluateExpression("call R_CHAR(STRING_ELT(Rf_deparse1s(CAR(CDR(call))), 0))", options) # .GetValue()
    name = pname.GetSummary()  # pname.GetData().GetString(lldb.SBError(), 0)
    # name="bob"
    ncalls = 1
    if name in counts:
        ncalls = counts[name] + 1
    counts[name] = ncalls
    return(False)

def reset():
    global counts
    counts = {}

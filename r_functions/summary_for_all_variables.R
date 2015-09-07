summary_for_all_variables <- function(x) {
        y=read.xls(x)
        all_summary=apply(y,2, FUN=summary)
}
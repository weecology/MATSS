#' @title generate_methods
#' 
#' @return a drake plan (i.e. a tibble) specifying the targets and commands 
#'   for analysis methods
#' 
#' @export
#' 
generate_methods <- function()
{
    drake::drake_plan(
        lda = function(dataset) {run_LDA(dataset, max_topics = 2, nseeds = 5)}
    )
}

#' @title collect
#' 
#' Define how results are collected
#' 
#' @return 
#' 
#' @export
#' 
collect <- function(list_of_results, plan)
{
    names(list_of_results) <- all.vars(match.call()$list_of_results)
    list_of_results
}


#' @title generate_analyses_section
#' 
#' @return the analyses chunk
#' @export
#' 

generate_analyses_section <- function(){
   drake::drake_plan(
       analysis = target(fun(data),
                      transform = cross(fun = !!rlang::syms(methods$target),
                                        data = !!rlang::syms(datasets$target))
    ),
    results = target(MATSS::collect(list(analysis), ignore(analyses)),
                     transform = combine(analysis, .by = fun))
   )
    
}

#' @title generate_reports_section
#' 
#' @return the reports chunk
#' @export
#' 
## Summary reports
# I don't quite understand the pathing here... - Hao
generate_reports_section <- function(){
    drake::drake_plan(
    lda_report = rmarkdown::render(
        knitr_in(report_path)
    )
)
}
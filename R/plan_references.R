#' @title Generate a Drake Plan for dataset references
#' 
#' @description Given N datasets, extract the citation from each one, and then 
#'   combine them into a single vector and remove duplicates.
#' 
#' @inheritParams build_analyses_plan
#' 
#' @return a drake plan (i.e. a tibble) specifying the targets and commands 
#'   for all the references and the combined vector
#' 
#' @export
#' 
build_references_plan <- function(datasets, ...)
{
    drake::drake_plan(
        citation_text = drake::target(dataset$metadata$citation,
                                      transform = map(dataset = !!rlang::syms(datasets$target))),
        citations = drake::target(unique(c(citation_text)),
                                  transform = combine(citation_text)), 
        ... = 
    )
}

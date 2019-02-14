#' @importFrom magrittr "%>%"

#' @title plan_datasets
#' 
#' @export
#' 
plan_datasets <- function()
{
    drake::drake_plan(
        portal_data = get_portal_rodents(),
        maizuru_data = get_maizuru_data(),
        jornada_data = get_jornada_data(),
        sgs_data = get_sgs_data()
    )
}


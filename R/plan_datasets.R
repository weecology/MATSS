#' @importFrom magrittr "%>%"

#' @title plan_datasets
#' 
#' @return a drake plan (i.e. a tibble) specifying the targets and commands 
#'   for gathering datasets
#' 
#' @export
#' 
plan_datasets <- function()
{
    drake::drake_plan(
        portal_data = get_portal_rodents(),
        maizuru_data = get_maizuru_data(),
        jornada_data = get_jornada_data(),
        sgs_data = get_sgs_data(),
        bbs_data = get_bbs_data(region = 7),
        sdl_data = get_sdl_data(),
        mtquad_data = get_mtquad_data()
    )
}
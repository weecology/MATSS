#' @title plan_datasets
#' 
#' @param data_path path to location of downloaded retriever datasets
#' 
#' @return a drake plan (i.e. a tibble) specifying the targets and commands 
#'   for gathering datasets
#' 
#' @export
#' 
plan_datasets <- function(data_path = get_default_data_path())
{
    drake::drake_plan(
        portal_data = get_portal_rodents(),
        maizuru_data = get_maizuru_data(),
        jornada_data = get_jornada_data(),
        sgs_data = get_sgs_data(),
        bbs_data = get_bbs_data(region = 7, path = !!data_path),
        sdl_data = get_sdl_data(path = !!data_path),
        mtquad_data = get_mtquad_data(path = !!data_path)
    )
}

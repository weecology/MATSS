#' @title Load BBS data from retriever
#'
#' @description All BBS data
#'
#'
#' @export
#'

get_bbs_data <- function()
{
  bbs_data <- rdataretriever::fetch('bbs')
  return(bbs_data)
}

#' @title Get long-term Montana plant quadrat data
#'
#' @description Load data, and save to data directory
#'
#'
#' @export
#'

get_montana_data <- function()
{

library(here)
folder_path <- here::here("data", "montanaquads")
dir.create(folder_path)
rdataretriever::install('mapped-plant-quads-mt', "csv", 
                        data_dir = folder_path)

return(rdataretriever::fetch('mapped-plant-quads-mt'))
}
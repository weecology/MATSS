#' @title Create a research compendium for MATSS
#'
#' @description Create an R package, using \code{\link[usethis]{use_package}}, 
#'   then perform the following additional actions:
#'   - add various package dependencies, including "MATSS"
#'   - add template analysis and pipeline files
#' 
#' @param name author name for the package
#' @param DEPLOY if `TRUE`, add preamble text to the readme
#' @inheritParams usethis::create_package
#' 
#' @return Path to the newly created package, invisibly.
#' 
#' @export
create_MATSS_compendium <- function(path, 
                                    name = usethis:::find_name(), 
                                    fields = NULL, 
                                    rstudio = rstudioapi::isAvailable(), 
                                    open = interactive(),
                                    DEPLOY = FALSE)
{
    # make new package, but don't open it
    usethis::create_package(path = path, 
                            fields = fields, 
                            rstudio = rstudio, 
                            open = FALSE)
    
    # get package name
    pkg_name <- fs::path_file(path)
    
    # track old project
    old_project <- usethis::proj_set(path, force = TRUE)
    on.exit(usethis::proj_set(old_project), add = TRUE)
    
    # add package dependencies
    add_dependency("drake")
    add_dependency("dplyr")
    add_dependency("ggplot2")
    add_dependency("here")
    add_dependency("MATSS")
    add_dependency("rlang")
    add_dependency("rmarkdown")
    add_dependency("tidyr")

    # add template files
    matss_citation <- utils::citation("MATSS")
    usethis::use_directory("analysis")
    usethis::use_template("template-functions.R", save_as = "R/analysis_functions.R", 
                          package = "MATSS")
    usethis::use_template("template-pipeline.R", save_as = "analysis/pipeline.R", 
                          data = list(package = pkg_name), 
                          package = "MATSS")
    usethis::use_template("template-report.Rmd", save_as = "analysis/report.Rmd", 
                          data = list(package = pkg_name, 
                                      author = name, 
                                      matss_ref_key = matss_citation$key), 
                          package = "MATSS")
    if (DEPLOY)
        preamble_text <- ":rotating_light: **THIS IS AN AUTO-GENERATED EXAMPLE COMPENDIUM** :rotating_light:\n"
    else
        preamble_text <- ""
    
    # add license
    license_text <- ""
    if (!interactive() || 
        usethis::ui_yeah(paste0("Use the MIT License for this compendium?\n", 
                                "(If unsure, please see ", 
                                usethis::ui_code("?usethis::use_mit_license()"),
                                ")")))
    {
        usethis::use_mit_license(name = name)
        license_text <- paste0("├── LICENSE.md\n", 
                               "├── LICENSE\n")
    }
    
    usethis::use_template("template-README.md", save_as = "README.md", 
                          data = list(package = pkg_name, 
                                      version = packageVersion("MATSS"), 
                                      citation_txt = citation("MATSS")$textVersion, 
                                      preamble = preamble_text, 
                                      license_file_contents = license_text), 
                          package = "MATSS")
    usethis::use_template("template-references.bib", save_as = "analysis/references.bib", 
                          data = list(bibentries = matss_citation %>% 
                                          utils::toBibtex() %>% 
                                          paste(collapse = "\n")), 
                          package = "MATSS")
    
    # add analysis folder to .gitignore and .Rbuildignore
    usethis::use_git_ignore("^/analysis/*.html")
    usethis::use_build_ignore("analysis")
    
    # open the package if desired
    if (open) {
        if (usethis::proj_activate(path)) {
            # Working directory/active project changed; so don't undo on exit
            on.exit()
        }
    }
    
    invisible(usethis::proj_get())
}

#' @title add a package dependency to the DESCRIPTION
#' 
#' @description We want to check if the package is installed from GitHub, so 
#'   that we can use either `use_package` or `use_dev_package`, as appropriate
#' 
#' @param pkg the name of the package
#' @noRd
add_dependency <- function(pkg = "MATSS")
{
    installed_from_github <- tryCatch(github_info <- !is.null(usethis:::package_remote(pkg)), 
                                      error = function(e) {FALSE}, 
                                      finally = TRUE)
    if (installed_from_github)
    {
        usethis::use_dev_package(pkg)
    } else {
        usethis::use_package(pkg)
    }
    return()
}

#' @noRd
write_Rprofile <- function(dir = ".", name = "Weecology Deploy Bot", 
                           email = "weecologydeploy@weecology.org", 
                           version = packageVersion("MATSS"))
{
    Rprofile_loc <- file.path(dir, ".Rprofile")
    Rprofile_exists <- file.exists(Rprofile_loc)
    if (Rprofile_exists)
        return()
    
    usethis::use_template("template-Rprofile", save_as = Rprofile_loc, 
                          data = list(name = name, 
                                      email = email, 
                                      version = version), 
                          package = "MATSS")
}

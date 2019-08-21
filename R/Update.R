#' @title Update the statsBordeaux package
#' @description Update the statsBordeaux package
#' @param force a boolean vector of length one. If TRUE, force installation, even if the
#' remote state has not changed since the previous install. Default to FALSE
#' @export
#' @examples
#' updateStatsBordeaux()
updateStatsBordeaux <- function(force = FALSE){
  if(!is.vector(force) | !is.logical(force) | length(force) != 1){
    stop("force must be a boolean vector of length one.")
  }
  devtools::install_github("rgriffier/statsBordeaux", force = force)
}

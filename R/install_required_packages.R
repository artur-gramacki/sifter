#' Install all required packages
#'
#' @return none
#' @export
#'
#' @examples
#' if (interactive()) {
#'   install_required_packages()
#' }
#'
install_required_packages <- function() {
  pkgs <- c("grDevices", "dendextend", "RColorBrewer", "factoextra",
            "cluster", "ggplot2", "graphics", "utils", "stats")

  to_install = !pkgs %in% installed.packages()
  if(any(to_install)) {
    install.packages(pkgs[to_install])
  }
}





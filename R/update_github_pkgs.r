#' Update github_install packages
#'
#' @return Returns invisibly the names of packages which need updating. THe
#'   function outputs the commands to run to actually update them (by
#'   reinstalling from github). Doesn't do this automatically because it would
#'   mean bringing in a lot of dependencies.
#' @importFrom utils packageDescription installed.packages
#' @md
#' @export
update_github_pkgs <- function() {
  pkgs <- utils::installed.packages(fields = "RemoteType")
  gh <- pkgs[pkgs[, "RemoteType"] %in% "github", "Package"]
  out <- lapply(gh, function(x) {
    repo <- utils::packageDescription(x, fields = "GithubRepo")
    username <- utils::packageDescription(x, fields = "GithubUsername")
    m <- paste0(username, "/", repo)
    sprintf("devtools::install_github(repo = %s)", m)
  })
  invisible(names(out))
}

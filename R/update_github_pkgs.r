#' Update github_install packages
#' @import utils
#' @export
update_github_pkgs <- function(no_action = FALSE) {
  requireNamespace("devtools")
  requireNamespace("utils")
  pkgs <- installed.packages(fields = "RemoteType")
  gh <- pkgs[pkgs[, "RemoteType"] %in% "github", "Package"]
  out <- lapply(gh, function(x) {
    repo <- utils::packageDescription(x, fields = "GithubRepo")
    username <- utils::packageDescription(x, fields = "GithubUsername")
    m <- paste0(username, "/", repo)
    if (no_action)
      message("Would update ", m)
    else
      devtools::install_github(repo = m)
  })
  invisible(names(out))
}

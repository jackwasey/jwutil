#' create environment from vector
#'
#' create an environment by inserting the value \code{val} with names taken from
#' \code{x}
#' @keywords internal
vec_to_env_true <- function(x, val = TRUE, env = new.env(hash = TRUE, parent = baseenv())) {
  lapply(x, function(y) env[[y]] <- val)
  env
}

vec_to_env_count <- function(x, env = new.env(hash = TRUE, parent = baseenv())) {
  for (i in 1L:length(x)) {
    env[[x[i]]] <- i
  }
  env
}

#' return a new environment with names and values swapped
#'
#' @param env environment with values being sequence numbers used to fill
#'   returned vector
#' @keywords internal
env_to_vec_flip <- function(env) {
  out <- character(length(env))
  # this assignment is very slow. Try vapply instead?
  lapply(ls(env), function(y) out[env[[y]]] <<- y)
  invisible(out)
}

vec_to_lookup_pair <- function(x, env = new.env(hash = TRUE, parent = baseenv())) {
  for (i in 1L:length(x)) {
    env[[x[i]]] <- i
  }
  invisible(list(env = env, vec = x))
}

#' in/match equivalent for two \code{Environment} arguments
#'
#' \code{x} and \code{table} are identical to match. Lookup is done based on
#' environment element names; contents are ignored.
#' @keywords internal
"%eine%" <- function(x, table) {
  vapply(ls(name = x),
         function(y) !is.null(table[[y]]),
         FUN.VALUE = logical(1L),
         USE.NAMES = FALSE)
}

"%ine%" <- function(x, table) {
  !is.null(table[[x]])
}

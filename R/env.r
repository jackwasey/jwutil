#' create environment from vector
#'
#' create an environment by inserting the value \code{val} with names taken from
#' \code{x}
#' @keywords internal
vec_to_env_true <-
  function(x, val = TRUE, env = new.env(hash = TRUE, parent = baseenv())) {
    stopifnot(is.environment(env))
    lapply(x, function(y) env[[y]] <- val)
    env
  }

vec_to_env_count <-
  function(x, env = new.env(hash = TRUE, parent = baseenv())) {
    stopifnot(is.environment(env))
    for (i in seq_along(x))
      env[[x[i]]] <- i
    env
  }

#' return a new environment with names and values swapped
#'
#' @param env environment with values being sequence numbers used to fill
#'   returned vector
#' @keywords internal
env_to_vec_flip <- function(env) {
  stopifnot(is.environment(env))
  out <- character(length(env))
  # this assignment is very slow. Try vapply instead?
  lapply(ls(env), function(y) out[env[[y]]] <<- y)
  invisible(out)
}

vec_to_lookup_pair <-
  function(x, env = new.env(hash = TRUE, parent = baseenv())) {
    stopifnot(is.environment(env))
    for (i in seq_along(x))
      env[[x[i]]] <- i
    list(env = env, vec = x)
  }

#' @title \%in\%/match equivalent for two \code{environment} arguments
#' @description \code{x} and \code{table} use the same as with
#'   \code{base::match}. Lookup is done based on environment element names;
#'   contents are ignored. For \code{\%ine\%}, \code{x} is a vector, and
#'   \code{table} is the environment to search. For \code{\%eine\%}, both
#'   arguments are environments.
#' @rdname eine
#' @usage x \%eine\% table
#' @keywords internal
"%eine%" <- function(x, table) {
  stopifnot(is.environment(x) && is.environment(table))
  vapply(ls(name = x),
         function(y) !is.null(table[[y]]),
         FUN.VALUE = logical(1L),
         USE.NAMES = FALSE)
}

#' @describeIn eine search vector of values in an environment
#' @export
"%ine%" <- function(x, table) {
  stopifnot(is.environment(table))
  stopifnot(is.vector(x))
  !is.null(table[[x]])
}

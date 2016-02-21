#dots formatting

#' @export
format.deparse <- function(x, ...) {
  format(vapply(x, deparse, "", nlines=1, width.cutoff=100), ... )
}

#' @export
`print....` <- function(x, ...) {
  invisible(cat("<...[", length(x), "]>\n"))
}

`%(d)%` <- function(f, d) {
  n <- names(formals(d))
  if("..." %in% n) {
    f %()% d } 
  else {
    f %()% d[intersect(names(d), names(formals(f)))];
  }
}

ifelsedf <- function(d, condf, truef, falsef) {
  cond <- condf %(d)% d
  whenFalse <- if(!all(cond)) falsef %(d)% d[!cond, ] else logical(0)
  whenTrue <- if(any(cond)) truef %(d)% d[cond, ] else logical(0)
  
  #type-stably allocate a sequence (assuming truef and falsef are type-stable)
  theMode <- mode(c(vector(mode(whenFalse), 0), vector(mode(whenTrue), 0)))
  
  out <- vector(theMode, length(cond))
  out[cond] <- whenTrue
  out[!cond] <- whenFalse
  out
}

# adapted from plyr::quickdf
df <- function(...) {
  list <- list(...)
  rows <- unique(unlist(lapply(list, NROW)))
  stopifnot(length(rows) == 1)
  names(list) <- make_names(list, "X")
  class(list) <- "data.frame"
  attr(list, "row.names") <- c(NA_integer_, -rows)
  list
}

make_names <- function (x, prefix = "X") 
{
  nm <- names(x)
  if (is.null(nm)) {
    nm <- rep.int("", length(x))
  }
  n <- sum(nm == "", na.rm = TRUE)
  nm[nm == ""] <- paste(prefix, seq_len(n), sep = "")
  nm
}

#' Format a dots object for printing. 
#' 
#' Constructs a string representation of a dots object. In this representation
#' an unevaluated promise is printed as "\code{envir ? expr}" and an evaluated
#' promise is shown as "\code{expr := value}".
#' @param x A dots object.
#'   
#' @param compact Implies \code{show.environments=FALSE} and
#'   \code{show.expressions=FALSE}.
#' @param show.environments Whether to show environments for unevaluated
#'   promises.
#' @param show.expressions Whether to show expressions for evaluated promises.
#' @param ... Further arguments passed to \link{format} methods.
#'   
#' @export
format.... <- function(x, 
                       compact = FALSE, 
                       show.environments = !compact, 
                       show.expressions = !compact,
                       ...) {
  dotdata <- unpack(x)
  doformat <- function(x) format(x, ...)
  
  contents <- paste0(
    ifelse(dotdata$name != "",
           paste0(dotdata$name, " = "),
           ""),
    ifelsedf(dotdata,
             function(envir) vapply(envir, is.null, FALSE),
             function(expr, value) 
               ifelsedf(df(expr=expr, value=value),
                        function(expr) vapply(expr, is.language, FALSE),
                        function(expr, value) paste0(
                          if (show.expressions && !is.missing(expr)) 
                            paste0(doformat(expr), " := ") else "",
                          doformat(value)),
                        function(value) doformat(value)),
             function(expr, envir) paste0(if (show.environments) doformat(envir) else "", 
                                          if (show.environments) " ? " else "? ",
                                          doformat(expr))),
    collapse=", "
  )
  chars = paste0("args(", contents, ")")
  format.default(chars, ...)
}

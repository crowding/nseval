#dots formatting



#' @export
format.deparse <- function(x, ...) {
  format(vapply(x, deparse, "", nlines=1, width.cutoff=100), ... )
}

#' @export
print.dots <- function(x, ...) {
  cat(format(x, ...), "\n")
  invisible(x)
}

#' @export
print.quotation <- function(x, ...) {
  cat(format(x, ...), "\n")
  invisible(x)
}

# call function using data frame as args, ignoring unmatched
`%(d)%` <- function(f, d) {
  n <- names(formals(f))
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

make_names <- function (x, prefix = "X"){
  nm <- names(x)
  if (is.null(nm)) {
    nm <- rep.int("", length(x))
  }
  n <- sum(nm == "", na.rm = TRUE)
  nm[nm == ""] <- paste(prefix, seq_len(n), sep = "")
  nm
}

one_line <- function(x, f, width, ...) {
  if (!(is.numeric(x) || is.character(x) || is.list(x))) {
    x <- list(x)
  }
  l <- lapply(x, f)
  vapply(l, function(x) toString(
    {
      if(length(x) > 1)
        paste0(x[[1]], "...")
      else if (length(x) == 1)
        x
      else "?NULL?"
    },
    width=width), ""
  )
}

format_robust <- function(x, ...) {
  tryCatch(format(x, ...), error=function(e) "?FORMAT?")
}

#' Format a sequence of objects to show one line each.
#'
#' Somewhat similar to format.AsIs but tries harder with language objects.
#' @param x An object
#'
#' @param width the width of line to produce.
#' @param ... parameters passed to "format"
#'
#' @export
format.oneline <- function(x, width=30, ...) {
  if ("one_line" %in% class(x)) {
    class(x) <- setdiff(class(x), "oneline")
  }
  one_line(x, format_robust, width=width, ...)
}

#' Format a dots object for printing.
#'
#' Constructs a string representation of a dots object. In this representation
#' an unevaluated promise is printed as `envir ? expr` and an evaluated
#' promise is shown as `expr := value`.
#' @param x A dots object.
#'
#' @param compact Implies `show.environments=FALSE` and
#'   `show.expressions=FALSE`.
#' @param show.environments Whether to show environments for forced
#'   quotations.
#' @param show.expressions Whether to show expressions for unforced
#'   quotations.
#' @param ... Further arguments passed to [format] methods.
#'
#' @export
format.dots <- function(x,
                       compact = FALSE,
                       show.environments = !compact,
                       show.expressions = !compact,
                       width=30,
                       ...) {
  contents <- mapply(
    x,
    names(x) %||% rep("", length(x)),
    FUN=function(x, n) {
      paste0(c(
        if (is.na(n)) "<NA> = " else if (n == "") "" else c(n, " = "),
        format.quotation.inner(x,
                               compact,
                               show.environments,
                               show.expressions,
                               width)),
        collapse="")
    })

  chars <- paste0("dots<< ",
                   paste0(contents, collapse=", "),
                   " >>")

  format.default(chars, ...)
}


format.quotation <- function(x,
                             compact = FALSE,
                             show.environments = !compact,
                             show.expressions = !compact,
                             width=30,
                             ...) {
  chars = paste0("quo<< ",
                 format.quotation.inner(
                   x, compact, show.environments, show.expressions, width=30),
                 " >>")
  format.default(chars, ...)
}


format.quotation.inner <- function(x,
                                   compact = FALSE,
                                   show.environments = !compact,
                                   show.expressions = !compact,
                                   width=30) {

  doformat <- function(x) {
    if (is.language(x)) {
      c("quote(", dodeparse(x), ")")
    } else {
      one_line(x, format, width=width)
    }
  }
  dodeparse <- function(x) {
    if (is.language(x)) {
      deparse(x, width.cutoff=width, nlines = 1)
    } else {
      doformat(x)
    }
  }
  contents <- paste0(c(
    if(forced(x)) {
      if (is.language(expr(x))) {
        if (show.expressions && !missing_(x))
          c(dodeparse(expr(x)), " := ", doformat(value))
      } else {
        doformat(value(x))
      }
    } else {
      c(if (show.environments) c(doformat(env(x)), " ? ") else "? ",
        dodeparse(expr(x)))
    }
  ), collapse="")
}

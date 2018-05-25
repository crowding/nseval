#' Formatting methods for dots and quotations.
#'
#' `format.dots` constructs a string representation of a dots
#' object. An un[forced] quotation is shown as `envir ? expr` and a
#' forced quotation is shown as `expr := value`.
#' @param x An object.
#' @param compact Implies `show.environments=FALSE` and
#'   `show.expressions=FALSE`.
#' @param show.environments Whether to show environments for unforced
#'   quotations.
#' @param show.expressions Whether to show expressions for forced
#'   quotations.
#' @param width the maximum length to use for each element.
#' @param ... Parameters passed to [base::format]
#' @rdname format.quotation
#' @export
format.dots <- function(x,
                        compact = FALSE,
                        show.environments = !compact,
                        show.expressions = !compact,
                        width = 30,
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


#' `format.quotation` constructs a string representation of a
#' quotation object.
#' @rdname format.quotation
#' @export
format.quotation <- function(x,
                             compact = FALSE,
                             show.environments = !compact,
                             show.expressions = !compact,
                             width = 30,
                             ...) {
  chars = paste0("quo<< ",
                 format.quotation.inner(
                   x, compact, show.environments, show.expressions, width = width),
                 " >>")
  format.default(chars, ...)
}

#' The method `format.oneline` formats a vector or list so that each
#' item fits on one line. It is similar to [format.AsIs] but tries
#' harder with language objects. The "oneline" class is used by
#' [as.data.frame.dots].
#' @export
#' @param ... Further arguments passed to [format].
#' @param x A vector or list.
#' @param max.width See [format].
#' @param width See [format].
format.oneline <- function(x, max.width=50, width=max.width, ...) {
  if ("oneline" %in% class(x)) {
    class(x) <- setdiff(class(x), "oneline")
  }
  one_line(x, format_robust, width=width, max.width=max.width, ...)
}

#' @export
#' @rdname format.quotation
print.dots <- function(x, ...) {
  cat(format(x, ...), "\n")
  invisible(x)
}

#' @export
#' @rdname format.quotation
print.quotation <- function(x, ...) {
  cat(format(x, ...), "\n")
  invisible(x)
}

one_line <- function(x, f, max.width=50, width=max.width, ...) {
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
    width=width),
    ""
  )
}

format_robust <- function(x, ...) {
  tryCatch(format(x, ...), error=function(e) "?FORMAT?")
}

format.name <- function(x, ...) {
  format(as.character(x))
}

oneline <- function(x) structure(x, class=union("oneline", class(x)))

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
    if (is.language(x) || is.character(x)) {
      deparse(x, width.cutoff=width, nlines = 1)
    } else {
      doformat(x)
    }
  }
  contents <- paste0(c(
    if(forced(x)) {
      if (is.language(expr(x))) {
        if (show.expressions) {
          c(dodeparse(expr(x)), " := ", doformat(value(x)))
        } else {
          doformat(value(x))
        }
      } else {
        doformat(value(x))
      }
    } else {
      c(if (show.environments) c(doformat(env(x)), " ? ") else "? ",
        dodeparse(expr(x)))
    }
  ), collapse="")
}

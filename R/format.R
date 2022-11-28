#' Formatting methods for dots and quotations.
#'
#' `format.dots` constructs a string representation of a dots
#' object.
#' @param x An object.
#' @param compact Implies `show.environments=FALSE` and
#'   `show.expressions=FALSE`.
#' @param show.environments Whether to show environments for unforced
#'   quotations.
#' @param show.expressions Whether to show expressions for forced
#'   quotations.
#' @param width See [base::format].
#' @rdname format
#' @export
format.dots <- function(x,
                        compact = FALSE,
                        show.environments = !compact,
                        show.expressions = !compact,
                        width = NULL,
                        ...) {
  contents <- mapply(
    x,
    names(x) %||% rep("", length(x)),
    ifelse(seq_along(x) == length(x), "", ", "),
    SIMPLIFY=FALSE,
    FUN=function(x, n, sep) {
      glue(
        if (is.na(n)) "<NA> = " else if (n == "") "" else paste0(n, " = "),
        format.quotation.inner(x,
                               compact,
                               show.environments=show.environments,
                               show.expressions=show.expressions,
                               width=width),
        sep)
    })

  chars <- do.call(glue, c("c.dots(", contents, ")"))
  chars
}


glue <- function(...) {
  # pastes multiline character vectors "end-to-end".
  # glue(c("a"), c("b", "c"), c("d", "e"), "f", c("g", "h"))
  #   -> c("ab", "cd", "efg", "h")
  args <- list(...)
  out <- rep("", sum(max(sapply(args, length), 1) - 1) + 1)
  ptr <- 1
  for (arg in args) {
    if(length(arg) > 0) {
      out[[ptr]] <- paste0(out[[ptr]], arg[[1]])
    }
    if (length(arg) > 1) {
      out[(ptr+1):(ptr+length(arg)-1)] <- arg[-1]
      ptr <- ptr + length(arg) - 1
    }
  }
  out
}

#' @rdname format
#' @description
#' `format.quotation` constructs a string representation of a
#' quotation object.
#' @export
format.quotation <- function(x,
                             compact = FALSE,
                             show.environments = !compact,
                             show.expressions = !compact,
                             width = NULL,
                             ...) {
  chars <- format.quotation.inner(
    x, compact = compact,
    show.environments = show.environments,
    show.expressions = show.expressions, width = width)
  chars # format.default(chars, ...)
}

#' @rdname format
#' @description
#' `format.oneline` formats a vector or list so that each item is
#' displayed on one line. It is similar to [format.AsIs] but tries
#' harder witlanguage objects. The "oneline" class is used by
#' [as.data.frame.dots].
#' @export
#' @param max.width See [base::format].
#' @param ... Further parameters passed along to [base::format].
format.oneline <- function(x, max.width=50, width=max.width, ...) {
  if ("oneline" %in% class(x)) {
    class(x) <- setdiff(class(x), "oneline")
  }
  one_line(x, format_robust, width=width, max.width=max.width, ...)
}

#' @export
#' @rdname format
print.dots <- function(x, ...) {
  cat(format(x, ...), sep="\n")
  invisible(x)
}

#' @export
#' @rdname format
print.quotation <- function(x, ...) {
  cat(format(x, ...), sep="\n")
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
                                   width=NULL) {

  doformat <- function(x) {
    if (is.language(x) || is.function(x)) {
      deparse(x,
              width.cutoff=if (is.null(width)) 60 else width,
              control=c("keepNA", "useSource", "quoteExpressions"))
    } else {
      one_line(x, format, width=width)
    }
  }
  dodeparse <- function(x) {
    if (is.language(x) || is.character(x) || is.list(x)) {
      deparse(x,
              width.cutoff=if (is.null(width)) 60 else width,
              control=c("keepNA", "useSource"))
    } else {
      doformat(x)
    }
  }
  contents <-
    if(forced(x)) {
      if (!identical(env(x), emptyenv())
          #&& show.environments && show.expressions
          ) {
        # weird primitive dispatch thing?!
        glue("weird_quo(", dodeparse(expr(x)),
             ", env=", dodeparse(env(x)),
             ", value=", dodeparse(value(x)))
      } else if (is.language(expr(x)) && show.expressions) {
        glue("forced_quo(", dodeparse(expr(x)),
             ", value=", dodeparse(value(x)), ")")
      } else {
        glue("forced_quo_(", dodeparse(value(x)), ")")
      }
    } else {
      glue("quo(",
           dodeparse(expr(x)),
           if (show.environments) paste0(", ", doformat(env(x))),
           ")")
    }
  contents
}

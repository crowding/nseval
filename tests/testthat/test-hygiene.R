context("Hygiene")

`%is%` <- expect_equal

test_that("can bquote quotations into expressions and eval hygienically", {

  a <- exp(1)
  b <- complex(re=0, imag=pi)
  f1 <- function(...) {
    b <- 0
    f2(..., in.f1=a^b)
  }
  f2 <- function(...) {
    a <- 10
    b <- 2
    f3(..., in.f2=a^b)
  }
  f3 <- function(...) {
    a <- 1
    f4(..., in.f3=a^b)
  }
  f4 <- function(...) {
    # All of the arguments to this function have the expression `a^b`,
    # but they refer to different local bindings of `a` and/or `b`.
    # exp(1), b=0 -> 1
    cat <- list; print <- identity # comment this out for output
    cat("# `substitute` thinks the call is: ")
    substituted <- substitute(f4(...))
    print(substituted)
    cat("# `match.call` thinks the call is:\n")
    matched <- match.call(expand.dots=TRUE, envir=environment())
    cat(deparse(matched), "\n")
    #
    cat("# `get_call` says the call is:\n")
    got <- get_call()
    print(as.data.frame(got))
    #
    # now try to splice these matched calls into another expression.
    cat("# evaluating `substitute`'s result gives:\n")
    subst.expr <- as.call(c(list(quote(c)), as.list(substituted[-1])))
    #subst.expr <- bquote( c( ..(as.expression(as.list(substituted[-1]))) ), splice=TRUE)
    print( eval(subst.expr) )
    #
    cat("# evaluating `match.call`'s result gives:\n")
    #match.expr <- bquote( c( ..(as.expression(as.list(matched[-1]))) ), splice=TRUE
    match.expr <- as.call(c(list(quote(c)), as.list(matched[-1])))
    print( eval(match.expr) )
    # and evaluating in the parent.frame there is an error.
    expect_error( eval(match.expr, parent.frame()) )
    #
    cat("# evaluating `get_call`'s result gives:\n")
    #got.expr <- bquote( c( ..(as.expression(got[-1])) ), splice=TRUE)
    got.expr <- as.call(c(list(quote(c)), as.list(got[-1])))
    print(eval( got.expr ))
    #
    cat("# evaluating `get_call`'s result in the parent frame gives:")
    print(eval( got.expr, parent.frame() ))
    #
    cat("# And standard R evaluation, for reference, gives:\n")
    print(c(...))
    eval( got.expr, parent.frame() ) %is% c(...)
  }
  f1(top.level=a^b)

})

test_that("quotation unwrapping", {
  # arg, arg_expr, arg_env, etc. should unwrap quotations if
  # they are found within a promise's "expr" argument.
  # This way we can have hygienic substitutions and unpack them too.

  first <- local({
    a <- "one"
    b <- "uno"
    quo(toupper(a))
  })

  second <- local({
    a <- "two"
    b <- "dos"
    quo(toupper(b))
  })

  third <- local({
    a <- "THREE"
    b <- "TRES"
    forced_quo(tolower(a))
  })

  pasta <- function(left, right) {
    browser()
    arg_expr(left) %is% quote(toupper(a))
    expr(arg(left)) %is% quote(toupper(a))
    arg_expr(right) %is% quote(toupper(b))
    expr(arg(right)) %is% quote(toupper(b))
    arg_env(left)$b %is% "uno"
    env(arg(right))$b %is% "uno"
    arg_env(left)$a %is% "two"
    env(arg(left))$a %is% "two"
    do(c, left, right) %is% c("ONE", "DOS")
    left %is% "ONE"
    right %is% "DOS"
  }
  eval(bquote(pasta(.(first), .(second))))

  pasty <- function(left, right) {
    arg_expr(left) %is% quote(toupper(a))
    expr(arg(left)) %is% quote(toupper(a))
    arg_expr(right) %is% quote(toupper(b))
    expr(arg(right)) %is% quote(toupper(b))
    arg_env(left)$b %is% "uno"
    env(arg(right))$b %is% "uno"
    arg_env(left)$a %is% "two"
    env(arg(left))$a %is% "two"
    do(c, left, right) %is% c("ONE", "DOS")
    left %is% "ONE"
    right %is% "DOS"
  }
  eval(bquote(pasty(.(first), .(second))))

  # check behavior with forced quotations
  pastf <- function(left, right) {
    expect_false(is_forced(left))
    expect_true(is_forced(right))
    expect_false(forced(arg(left)))
    expect_true(forced(arg(right)))
    arg_env(left)$b %is% "dos"
    arg_env(right) %is% emptyenv()

    force(left) %is% "DOS"
    force(right) %is% "three"

    expect_true(is_forced(left))
    expect_true(forced(arg(left)))
    arg_expr(left) %is% quote(toupper(b))
    arg_expr(right) %is% quote(tolower(a))
    expr(arg(left)) %is% quote(toupper(b))
    expr(arg(right)) %is% quote(tolower(a))
    arg_env(left)$b %is% "dos" ## even though forced! So we have a use
                               ## case for weird quotations
    env(arg(right))$a %is% "THREE"
  }
  eval(bquote(pastf( .(second), .(third) )))

})

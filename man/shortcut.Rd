% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shortcut.R
\name{arg_env}
\alias{arg_env}
\alias{arg_env_}
\alias{arg_expr}
\alias{arg_expr_}
\alias{arg_value}
\alias{arg_value_}
\alias{dots_envs}
\alias{dots_exprs}
\alias{is_forced}
\alias{is_forced_}
\alias{is_literal}
\alias{is_literal_}
\alias{is_missing}
\alias{is_missing_}
\alias{is_missing_.quotation}
\alias{is_promise}
\alias{is_promise_}
\alias{is_promise_.quotation}
\alias{is_default}
\alias{is_default_}
\alias{is_default_.quotation}
\title{Get information about currently bound arguments.}
\usage{
arg_env(sym, env = arg_env_(quote(sym), environment()))

arg_env_(sym, env = arg_env_(quote(sym), environment()))

arg_expr(sym, env = arg_env_(quote(sym), environment()))

arg_expr_(sym, env = arg_env_(quote(sym), environment()))

arg_value(
  sym,
  env = arg_env_(quote(sym), environment()),
  ifnotforced = stop("Variable is not forced, so has no value")
)

arg_value_(
  sym,
  env = arg_env_(quote(sym), environment()),
  ifnotforced = stop("Variable is not forced, so has no value")
)

dots_envs(...)

dots_exprs(...)

is_forced(...)

is_forced_(syms, envs)

is_literal(...)

is_literal_(syms, envs)

is_missing(...)

is_missing_(syms, envs, unwrap = TRUE)

\method{is_missing_}{quotation}(syms, ..., unwrap = TRUE)

is_promise(...)

is_promise_(syms, envs)

\method{is_promise_}{quotation}(syms, ...)

is_default(...)

is_default_(syms, envs)

\method{is_default_}{quotation}(syms, ...)
}
\arguments{
\item{sym}{For plain \code{arg_env}, etc, a bare name, which is
quoted. For the underscore versions \code{arg_env_}, something that
evaluates to a name or character.}

\item{env}{The environment to search in.}

\item{ifnotforced}{What to return if calling arg_value on a promise
that has not been forced.}

\item{...}{Bare variable names (for \verb{is_*}) or expressions (for
\verb{dots_*}). Not forced.}

\item{syms}{A character vector or list of symbols.}

\item{envs}{An environment or list of environments.}

\item{unwrap}{Whether to recursively \link{unwrap} before testing for
missingness.}
}
\value{
\code{arg_env} returns an environment.

\code{arg_expr} returns the expression bound to a named argument.

\code{arg_value} returns the value bound to a named argument.

\code{is_forced} and other \verb{is_*} return a logical vector with
optional names.
}
\description{
These are shortcut methods for querying current bindings.  For
example, \code{arg_env(x)} is equivalent to \code{env(arg(x))},
\code{is_forced(x, y)} is equivalent to \code{forced(arg_list(x,y))},
\code{dots_exprs(...)} is equivalent to \code{exprs(dots(...))}, and so
on. The shortcut forms skip the construction of the
intermediate \link{quotation} objects.

\code{dots_exprs(...)} quotes its arguments and returns a
list of expressions. It is equivalent to \code{exprs(dots(...))} (and
is nearly equivalent to \code{alist(...)}, one difference being that
dots_exprs will expand \code{...}.)

\code{is_literal(x)} returns TRUE if an argument \code{x} could be a
source literal. Specifically it tests whether \code{x} is bound to a
singleton vector or a \link{missing_value}. This check happens without
forcing \code{x}.

\code{is_missing(...)} checks whether an argument is
missing, without forcing. It is similar to \link{missing} but can take
multiple arguments, and can be called in more situations, such as
from a nested inner function.

\code{is_missing_(syms, envs)} is a normally evaluating version
of is_missing.  \code{syms} should be a symbol, character vector or
list of such. \code{envs} should be an environment, or list of
environments. Vector recycling rules apply, so you can call with
a vector of names and one env, or vice versa.

\code{is_promise} returns TRUE if the given variable is bound to
a promise. Not all arguments are bound to promises; byte-compiled
code often omits creating a promise for literal or missing arguments.

\code{is_default}  determines whether an argument is bound to the
function's default value for that argument. It must be called
before the arguments have been forced (afterwards it will return
FALSE).
}
\details{
Throughout this package, some functions come in two forms, a "bare"
version which quotes its first argument literally, and a
normally-evaluating version with a trailing underscore in its
name. So \code{is_forced(x)} chiecks whether "x" is a missing variable,
while \code{is_forced_(x, environment())} checks whether "x" contains
the \emph{name} of another variable which is missing. The following are
all equivalent:
\itemize{
\item \code{arg_env(x)}
\item \code{{y <- quo(x); arg_env_(y)}}
\item \code{arg_env_(quote(x), environment())}
\item \code{arg_env_(quo(x))}
\item \code{env(arg_(quo(x)))}.
}

When used with quotation objects, the \verb{is_*_} functions
with trailing underscore work at one level of indirection
compared to quotation methods. For example, \code{missing_(x)} tests
whether \code{expr(x)} is \verb{[missing_value()]}, whereas \code{is_missing_(x)}
assumes \code{expr(x)} is a \emph{name} and checks if that name refers to a
variable that is missing. The following are equivalent:
\itemize{
\item \code{is_missing(x)}
\item \code{is_missing_(quo(x))}
\item \code{missing_(arg(x))}
}

When used with a \code{quotation} or \code{dots}, is_missing(q)
looks for the variable(s) specified by expr(q) in environment env(q)]`.
}

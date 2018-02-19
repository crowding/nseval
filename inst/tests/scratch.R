
f <- function(x) is.missing(x)

vals <- lapply(1:3, function(x) if (x == 2) missing_value() else x)
vals

lapply(vals, function(x) is.missing(x))

a <- dots(one, , three)

is.missing(a[[2]])

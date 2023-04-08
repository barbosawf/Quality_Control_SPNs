x = c('A', 'A', 'B', 'B', 'B', NA, NA)
v = x
vctrs::vec_count(v)
by(v, list(num = v), length)
lengths(split(v, v))
table(v)
match(v, v)

aggregate(v, list(num = v), length)

fn <- function(x) {
  u <- unique.default(x)
  out <-
    list(x = u, freq = .Internal())
  class(out) <- "data.frame"
  attr(out, "row.names") <- seq_along(u)
  out
}

fn(v)

fn2 <- function(x) {
  u <- unique(x)
  out <- data.frame(x = u,
                    freq = tabulate(match(x, u),
                                    length(u)))
  out
}

fn2(v)


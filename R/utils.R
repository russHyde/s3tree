update_or_append <- function(x, ...) {
  UseMethod("update_or_append")
}

update_or_append.list <- function(x, ...) {
  dots <- list(...)
  to_append <- setdiff(names(dots), names(x))
  to_update <- intersect(names(dots), names(x))
  x[to_append] <- dots[to_append]
  x[to_update] <- dots[to_update]
  x
}

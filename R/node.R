# ---- node (branch or leaf) in an `s3_tree`

node <- function(name = "root", parent = NULL, children = NULL) {
  stopifnot(length(name) == 1)
  stopifnot(length(parent) <= 1)
  structure(
    list(
      name = name,
      parent = parent,
      children = children
    ),
    class = "Node"
  )
}

parent_name <- function(x, ...) {
  UseMethod("parent_name")
}

parent_name.Node <- function(x, ...) {
  x$parent
}

has_parent <- function(x, ...) {
  UseMethod("has_parent")
}

has_parent.Node <- function(x, ...) {
  length(parent_name(x)) == 1
}

children <- function(x, ...) {
  UseMethod("children")
}

children.Node <- function(x, ...) {
  x$children
}

append_child <- function(x, ...) {
  UseMethod("append_child")
}

append_child.Node <- function(x, child, ...) {
  if (!child %in% x$children) {
    x$children <- c(x$children, child)
  }
  x
}

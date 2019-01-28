# ---- node (branch or leaf) in an `s3_tree`

as_node <- function(x) {
  stopifnot(all(c("name", "parent", "children") %in% names(x)))
  structure(x, class = "Node")
}

node <- function(name = "root", parent = NULL, children = NULL, ...) {
  stopifnot(length(name) == 1)
  stopifnot(length(parent) <= 1)
  as_node(
    append(
      list(name = name, parent = parent, children = children),
      list(...)
    )
  )
}

as.list.Node <- function(x, ...) {
  attr(x, "class") <- NULL
  x
}

#' parent_name of a node
#' @param        x             a node.
#' @param        ...           further args - not currently used.
#' @export
parent_name <- function(x, ...) {
  UseMethod("parent_name")
}

#' parent_name of a node
#' @param        x             a node.
#' @param        ...           further args - not currently used.
#' @export
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

update_node <- function(x, ...) {
  UseMethod("update_node")
}

update_node.Node <- function(x, ...) {
  dots <- list(...)
  if ("children" %in% names(dots)) {
    stop("can't update `children` using `update_node`: see `add_child`")
  }
  if ("parent" %in% names(dots)) {
    stop("can't update `parent` using `update_node`")
  }

  as_node(update_or_append(as.list(x), ...))
}

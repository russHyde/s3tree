# ---- node (branch or leaf) in an `s3_tree`

#' Convert a `list` into a `Node`
#'
#' @param        x             A list of data. This must contain entries named
#'   `name`, `parent` and `children`.
#'
#' @export
#'
as_node <- function(x) {
  stopifnot(all(c("name", "parent", "children") %in% names(x)))
  structure(x, class = "Node")
}

#' Construct a `Node`
#'
#' @param        name          Name of the `Node`
#' @param        parent        Name of the (lone) parent of the `Node`
#' @param        children      Names of any children of the `Node`
#' @param        ...           Any other fields of data for storing with the
#'   `Node`. Provided as key=value pairs.
#'
#' @export
#'
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
#'
#' @export
#'
parent_name <- function(x, ...) {
  UseMethod("parent_name")
}

#' parent_name of a node
#' @param        x             a node.
#' @param        ...           further args - not currently used.
#'
#' @export
#'
parent_name.Node <- function(x, ...) {
  x$parent
}

#' Does this `Node` have a parent in the `Tree`?
#' @param        x             a `Node`
#' @param        ...           further args - not currently used.
#'
#' @export
#'
has_parent <- function(x, ...) {
  UseMethod("has_parent")
}

#' Does this `Node` have a parent in the `Tree`?
#' @param        x             a `Node`
#' @param        ...           further args - not currently used.
#'
#' @export
#'
has_parent.Node <- function(x, ...) {
  length(parent_name(x)) == 1
}

#' Extract the names of the children of a `Node`
#' @param        x             A `Node`
#' @param        ...           Other arguments - unused at present.
#' @export
children <- function(x, ...) {
  UseMethod("children")
}

#' Extract the names of the children of a `Node`
#' @param        x             A `Node`
#' @param        ...           Other arguments - unused at present.
#' @export
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

#' Update the data that is attached to a `Node`
#'
#' @param        x             a `Node`
#' @param        ...           a set of key=value pairs for appending as fields
#'   to the current `Node`.
#'
#' @export
#'
update_node <- function(x, ...) {
  UseMethod("update_node")
}

#' Update the data that is attached to a `Node`
#'
#' @param        x             a `Node`
#' @param        ...           a set of key=value pairs for appending as fields
#'   to the current `Node`.
#'
#' @export
#'
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

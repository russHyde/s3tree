# s3 tree

#' Construct an empty `Tree`
#'
#' @export
#'
tree <- function() {
  structure(
    list(
      nodes = list()
    ),
    class = "Tree"
  )
}

#' Obtain the `Node`s that are stored in a `Tree`
#'
#' @param        x             A `Tree` object.
#' @param        ...           Other arguments - unused at present.
#'
#' @export
#'
nodes <- function(x, ...) {
  UseMethod("nodes")
}

#' Obtain the `Node`s that are stored in a `Tree`
#'
#' @param        x             A `Tree` object.
#' @param        ...           Other arguments - unused at present.
#'
#' @export
#'
nodes.Tree <- function(x, ...) {
  x$nodes
}

#' Returns true for Trees that don't contain `Node`s
#'
#' @param        x             A `Tree` object.
#' @param        ...           Other arguments - unused at present.
#'
#' @export
#'
is_empty <- function(x, ...) {
  UseMethod("is_empty")
}
#' Returns true for Trees that don't contain `Node`s
#'
#' @param        x             A `Tree` object.
#' @param        ...           Other arguments - unused at present.
#'
#' @export
#'
is_empty.Tree <- function(x, ...) {
  length(nodes(x)) == 0
}

#' Get a `Node` using it's name from a `Tree`
#'
#' @param        x             A `Tree`
#' @param        ...           Other arguments.
#'
#' @export
#'
get_node <- function(x, ...) {
  UseMethod("get_node")
}

#' Get a `Node` using it's name from a `Tree`
#'
#' @param        x             A `Tree`.
#' @param        node_name     The name of a `Node` within the `Tree`.
#' @param        ...           Other arguments: unused at present.
#'
#' @export
#'
get_node.Tree <- function(x, node_name, ...) {
  stopifnot(node_name %in% names(nodes(x)))
  nodes(x)[[node_name]]
}

#' Obtain the `Node` that is the parent of a given `Node` in the `Tree`
#'
#' @param        x             A `Tree`.
#' @param        ...           Other arguments.
#'
#' @export
#'
get_parent <- function(x, ...) {
  UseMethod("get_parent")
}

#' Obtain the `Node` that is the parent of a given `Node` in the `Tree`
#'
#' @param        x             A `Tree`
#' @param        node          A `Node` that has a parent within the `Tree`.
#' @param        ...           Other arguments - currently unused.
#'
#' @export
#'
get_parent.Tree <- function(x, node, ...) {
  stopifnot(node$name %in% names(nodes(x)))
  stopifnot(has_parent(node))
  get_node(x, parent_name(node))
}

#' Add a `Node` to a `Tree`
#'
#' @param        x             Some `Tree`.
#' @param        ...           Further arguments - unused at present.
#'
#' @export
#'
add_node <- function(x, ...) {
  UseMethod("add_node")
}

#' Add a `Node` to a `Tree`
#'
#' @param        x             Some `Tree`.
#' @param        node          Some `Node` to be added to the `Tree`.
#' @param        ...           Further arguments - unused at present.
#'
#' @export
#'
add_node.Tree <- function(x, node, ...) {
  c_if <- function(input, test, value) {
    if (test) {
      c(input, value)
    } else {
      input
    }
  }
  can_add_node <- function() {
    errors <- c() %>%
      c_if(
        has_parent(node) && !parent_name(node) %in% names(nodes(x)),
        "can't add a `Node` that has parents if parents are absent"
      ) %>%
      c_if(
        node$name %in% names(nodes(x)),
        "can't duplicate node-names in a `Tree`"
      ) %>%
      c_if(
        !(is_empty(x) || has_parent(node)),
        "can't add a parent-less node to a non-empty `Tree`"
      )

    has_errors <- length(errors) > 0
    list(has_errors = has_errors, error_strings = errors)
  }
  validity <- can_add_node()
  if (validity$has_errors) {
    stop(validity$error_strings)
  }
  # update_parent
  if (has_parent(node)) {
    pname <- parent_name(node)
    x$nodes[[pname]] <- append_child(get_node(x, pname), child = node$name)
  }
  # append new node
  x$nodes <- append(nodes(x), stats::setNames(list(node), node$name))
  x
}

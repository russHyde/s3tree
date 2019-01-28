# s3 tree

tree <- function() {
  structure(
    list(
      nodes = list()
    ),
    class = "Tree"
  )
}

nodes <- function(x, ...) {
  UseMethod("nodes")
}

nodes.Tree <- function(x, ...) {
  x$nodes
}

is_empty <- function(x, ...) {
  UseMethod("is_empty")
}

is_empty.Tree <- function(x, ...) {
  length(nodes(x)) == 0
}

add_node <- function(x, ...) {
  UseMethod("add_node")
}

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
    x$nodes[[pname]] <- append_child(nodes(x)[[pname]], child = node$name)
  }
  # append new node
  x$nodes <- append(nodes(x), stats::setNames(list(node), node$name))
  x
}

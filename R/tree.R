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

# .x is the tree
# .f is applied to each of it's nodes
# if .field a string, the result will be appended/updated to that field of the
#   node, otherwise, a named list of results will be passed back
# if .f has an argument `tree`, the whole of the tree is passed into the
#   function
map_tree <- function(.x, .f, ..., .field = NULL) {
  values <- if ("tree" %in% methods::formalArgs(.f)){
    purrr::map(nodes(.x), .f, tree = .x, ...)
  } else {
    purrr::map(nodes(.x), .f, ...)
  }

  if (is.null(.field)) {
    values
  } else {
    new_nodes <- purrr::map2(nodes(.x), values, function(x, y) {
      args <- stats::setNames(list(x, y), c("x", .field))
      do.call(update_node, args)
    })
    .x$nodes <- new_nodes
    .x
  }
}

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
    x$nodes[[pname]] <- append_child(nodes(x)[[pname]], child = node$name)
  }
  # append new node
  x$nodes <- append(nodes(x), stats::setNames(list(node), node$name))
  x
}

#' Map a function over the `Node`s in a `Tree`
#'
#'
#' @param        .x            The `Tree`
#' @param        .f            The function to be applied to the `Node`s in the
#'   `Tree`. If .f has an argument called `tree` then `map_tree` will pass the
#'   `Tree` `.x` in as that argument automatically.
#' @param        .field        If a string is provided, the values of `.f` will
#'   be appended to the respective `Node`s in the `Tree`; .field gives the name
#'   of the field where this result is stored. If .field is NULL (as default)
#'   a named list of the results will be returned (names being the name of the
#'   relevant `Node`).
#' @param        ...           Further arguments to be passed into the function
#'   `.f`. Except: if the function `.f` takes an argument `tree`, then `.x` is
#'   passed in as the `tree` argument automatically.
#'
#' @export
#'
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

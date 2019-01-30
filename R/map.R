#' Map a function over the `Node`s in a `Tree`
#'
#' This function allows you to map a function over the nodes in a tree. If the
#' argument `.field` is `NULL`, a list of results is returned, otherwise, the
#' results for a given node is added to a field of that name.
#'
#' If the function `.f` has an argument named `tree`, then `.f` can use details
#' of the whole tree when evaluating the result for a given node. For example,
#' when evaluating a given node, you can extract values from it's parent or
#' children.
#'
#' The tree is evaluated top-down, so children are evaluated after their
#' parents.
#'
#' If `.field` is non-`NULL`, then the tree is modified as each node is
#' evaluated. This means you can use the computed value for a parent when
#' evaluating for a child.
#'
#' The tree is not currently modified when `.field` is non-null.
#'
#' @param        .x            The `Tree`
#' @param        .f            The function to be applied to the `Node`s in the
#'   `Tree`. If .f has an argument called `.tree` then `map_tree` will pass the
#'   `Tree` `.x` in as that argument automatically.
#' @param        .field        If a string is provided, the values of `.f` will
#'   be appended to the respective `Node`s in the `Tree`; `.field` gives the
#'   name of the field where this result is stored. If `.field` is `NULL` (as
#'   default) a named `list` of the results will be returned (names being the
#'   name of the relevant `Node`).
#' @param        ...           Further arguments to be passed into the function
#'   `.f`. Except: if the function `.f` takes an argument `.tree`, then `.x` is
#'   passed in as the `.tree` argument automatically.
#'
#' @export
#'
map_tree <- function(.x, .f, ..., .field = NULL) {
  tree_required_by_f <- ".tree" %in% methods::formalArgs(.f)
  tree_in_args <- ".tree" %in% names(list(...))

  if (tree_required_by_f && !tree_in_args) {
    return(
      map_tree(.x, .f, .tree = .x, ..., .field = .field)
    )
  }

  if (is.null(.field)) {
    return(
      purrr::map(nodes(.x), .f, ...)
    )
  }

  for (.node in nodes(.x)) {
    func_args <- if (tree_required_by_f) {
      # The tree gets updated as the function is applied to the nodes,
      # By passing the updated tree into the function .f, results for children
      #   can depend on the results for their parents
      dot_args <- list(...)
      dot_args[[".tree"]] <- NULL
      append(
        list(.node, .tree = .x),
        dot_args
      )
    } else {
      list(.node, ...)
    }
    result <- do.call(.f, func_args)
    .x$nodes[[.node$name]] <- do.call(
      "update_node",
      list(.node, result) %>% stats::setNames(c("x", .field))
    )
  }
  .x
}

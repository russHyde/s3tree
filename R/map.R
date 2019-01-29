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
  values <- if ("tree" %in% methods::formalArgs(.f)) {
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

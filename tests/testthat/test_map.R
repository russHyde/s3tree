context("Tests for mapping over a `Tree`")

# a function can be `map`ped across the nodes in a tree, returning
# - a named list if `.field = NULL`
# or another tree if `.field = "some_field_name"`

test_that("The `Node`s in a `Tree` can be mapped over", {
  t1 <- tree() %>%
    add_node(node(name = "root")) %>%
    add_node(node(name = "a", parent = "root")) %>%
    add_node(node(name = "b", parent = "a")) %>%
    add_node(node(name = "c", parent = "root"))

  t2 <- tree() %>%
    add_node(node(name = "root", alt_parent = NULL)) %>%
    add_node(node(name = "a", parent = "root", alt_parent = "root")) %>%
    add_node(node(name = "b", parent = "a", alt_parent = "a")) %>%
    add_node(node(name = "c", parent = "root", alt_parent = "root"))

  expect_equal(
    map_tree(t1, parent_name),
    list(root = NULL, a = "root", b = "a", c = "root"),
    info = "map over the nodes of a tree: returning a list"
  )

  expect_equal(
    map_tree(t1, parent_name, .field = "alt_parent"),
    t2,
    info = paste(
      "if `.field` is set, mapping over a `Tree` returns an updated `Tree`"
    )
  )
})

test_that(".. the `map` function can use pre-existing values in the `Tree`", {
  t1 <- tree() %>%
    add_node(node(name = "root")) %>%
    add_node(node(name = "a", parent = "root")) %>%
    add_node(node(name = "b", parent = "a")) %>%
    add_node(node(name = "c", parent = "root"))

  count_siblings <- function(node, tree) {
    n_siblings <- if (has_parent(node)) {
      parent <- nodes(tree)[[parent_name(node)]]
      length(parent$children)
    } else {
      1
    }
    n_siblings
  }

  expect_equal(
    map_tree(t1, count_siblings),
    list(root = 1, a = 2, b = 1, c = 2),
    info = paste(
      "map over nodes, using pre-existing info that can't be got from the",
      "node alone"
    )
  )
})

test_that(".. `map` results for `Node`s can be used by their children", {

})

context("Tests for mapping over a `Tree`")

t1 <- function() {
  # root/
  # |--- a/
  # |   |--- b/
  # |--- c/
  tree() %>%
    add_node(node(name = "root")) %>%
    add_node(node(name = "a", parent = "root")) %>%
    add_node(node(name = "b", parent = "a")) %>%
    add_node(node(name = "c", parent = "root"))
}

# a function can be `map`ped across the nodes in a tree, returning
# - a named list if `.field = NULL`
# or another tree if `.field = "some_field_name"`

test_that("The `Node`s in a `Tree` can be mapped over", {
  t2 <- tree() %>%
    add_node(node(name = "root", alt_parent = NULL)) %>%
    add_node(node(name = "a", parent = "root", alt_parent = "root")) %>%
    add_node(node(name = "b", parent = "a", alt_parent = "a")) %>%
    add_node(node(name = "c", parent = "root", alt_parent = "root"))

  expect_equal(
    map_tree(t1(), parent_name),
    list(root = NULL, a = "root", b = "a", c = "root"),
    info = "map over the nodes of a tree: returning a list"
  )

  expect_equal(
    map_tree(t1(), parent_name, .field = "alt_parent"),
    t2,
    info = paste(
      "if `.field` is set, mapping over a `Tree` returns an updated `Tree`"
    )
  )
})

test_that(".. the `map` function can use pre-existing values in the `Tree`", {
  count_siblings <- function(node, tree) {
    n_siblings <- if (has_parent(node)) {
      parent <- get_parent(tree, node)
      length(parent$children)
    } else {
      1
    }
    n_siblings
  }

  expect_equal(
    map_tree(t1(), count_siblings),
    list(root = 1, a = 2, b = 1, c = 2),
    info = paste(
      "map over nodes, using pre-existing info that can't be got from the",
      "node alone"
    )
  )
})

test_that(".. `map` results for `Node`s can be used by their children", {
  # update the field "name_sum" as you traverse down the `Tree`

  name_sum <- function(node, tree) {
    if (has_parent(node)) {
      nchar(node$name) + get_parent(tree, node)$name_sum
    } else {
      nchar(node$name)
    }
  }

  expected <- tree() %>%
    add_node(node(name = "root", name_sum = 4)) %>%
    add_node(node(name = "a", parent = "root", name_sum = 5)) %>%
    add_node(node(name = "b", parent = "a", name_sum = 6)) %>%
    add_node(node(name = "c", parent = "root", name_sum = 5))

  expect_equal(
    map_tree(t1(), name_sum, .field = "name_sum"),
    expected,
    info = "map over nodes, using results computed for higher levels"
  )
})

context("Tests for `node` function / `Node` class")

###############################################################################

test_that("A `Node` can be constructed using `node()`", {
  n <- node()
  expect_is(n, "Node")
})

test_that("A `Node` can be constructed from a list using `as_node()`", {
  my_list <- list(name = "me", parent = NULL, children = NULL)
  expect_is(
    as_node(my_list), "Node",
    info = "`as_node` converts a `list` to a `Node`."
  )

  expect_error(
    as_node(list(no = "parent", or = "children", OR = "name")),
    info = paste(
      "`name` / `parent` / `children` entries are required to turn a `list`",
      "into a `Node`."
    )
  )
})

test_that(".. it can be converted to a list", {
  node_vals <- list(name = "abc", parent = "root", children = NULL)
  node <- as_node(node_vals)

  expect_equal(
    object = as.list(node),
    expected = node_vals,
    info = "can convert `Node` to `list`"
  )
})

###############################################################################

test_that(" .. it must have a `name`", {
  n1 <- node()
  expect_true(
    "name" %in% names(n1),
    info = "`Node`s must have a `name` field"
  )
  expect_equal(
    n1$name, "root",
    info = "default `name` for a `Node`: `root`"
  )

  # a single string can be used as the non-default `name`
  expect_equal(
    node(name = "some_node")$name, "some_node",
    info = "user can specify `Node`s `name`"
  )

  # but NULL `name` and >=2 names are not valid
  expect_error(
    node(name = NULL),
    info = "expect non-NULL name for a `Node`"
  )
  expect_error(
    node(name = letters[1:2]),
    info = "expect a single name per `Node`"
  )
})

###############################################################################

test_that(" .. it must have a `parent` field", {
  n <- node()
  n1 <- node(parent = NULL)
  n2 <- node(parent = character(0))
  n3 <- node(parent = c("some_other_node"))

  expect_true(
    "parent" %in% names(n),
    info = "`Node`s must have a `parent` field"
  )
  expect_equal(
    parent_name(n3), n3$parent,
    info = "parent_name accessor"
  )

  expect_null(
    parent_name(n1), "node's `parent` may be NULL"
  )
  expect_equal(
    parent_name(n2), character(0),
    info = "node's `parent` may be empty"
  )
  expect_equal(
    parent_name(n3), "some_other_node",
    info = "node's `parent` should match the input"
  )
  expect_error(
    node(parent = c("node1", "node2")),
    info = "a node can have at most one `parent`"
  )

  expect_false(
    has_parent(n),
    info = "a node with no parents"
  )
  expect_true(
    has_parent(n3),
    info = "a node with parents"
  )
})

###############################################################################

test_that(".. it must have a `children` field", {
  # - which is either empty or contains a vector of names

  n0 <- node()
  n1 <- node(children = c("n2", "n3"))

  expect_true("children" %in% names(n0), "`Node` has a `children` field")

  expect_null(n0$children, info = "`children` can be NULL")
  expect_equal(
    n1$children, c("n2", "n3"),
    info = "`children` field can be a vector of strings"
  )

  expect_equal(children(n1), n1$children, info = "`children` accessor")
})

test_that(".. it can acquire additional `children`", {
  n0 <- node()
  n1 <- node(children = c("a", "b"))

  expect_equal(
    append_child(n0, "some_child"), node(children = "some_child"),
    info = "add a child to a `Node` that has no children"
  )

  expect_equal(
    append_child(n1, "c"), node(children = c("a", "b", "c")),
    info = "add a child to a `Node` that already has them"
  )

  expect_equal(
    append_child(n1, "b"), node(children = c("a", "b")),
    info = "don't add a child if it's already present in the `children` field"
  )
})

###############################################################################

test_that(".. it can store additional fields", {
  n0 <- node()

  expect_is(
    update_node(n0, my_field = 123),
    "Node",
    info = "a `Node` is still a `Node` after modifying a field"
  )
  expect_equal(
    update_node(n0), n0,
    info = "fields should only be modified if passed in"
  )
  expect_equal(
    update_node(n0, my_field = 123)$my_field, 123,
    info = "add a field to a `Node`"
  )
  expect_equal(
    object = n0 %>%
      update_node(my_field = 123) %>%
      update_node(my_field = 246) %>%
      `[[`("my_field"),
    expected = 246,
    info = "modify an existing field on a `Node`"
  )

  expect_equal(
    node(some_field = 123)$some_field, 123,
    info = "additional fields can be passed into `node` constructor"
  )
})

test_that(".. it must not use `update_node` to modify `parent` or `children`", {
  n0 <- node()

  expect_error(
    object = update_node(n0, children = letters[1:3]),
    info = "can't update children of a `Node` using `update_node`"
  )

  expect_error(
    object = update_node(n0, parent = "some_node"),
    info = "can't update parent of a `Node` using `update_node`"
  )
})

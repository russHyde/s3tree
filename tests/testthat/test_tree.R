context("Tests for `tree`")

test_that("A `Tree` should be of class `Tree`", {
  t1 <- tree()
  expect_is(t1, "Tree")
})

# .. it should store a list of nodes, accesible using nodes(tree) or tree$nodes
test_that(".. it should store a list of nodes", {
  t1 <- tree()
  expect_true("nodes" %in% names(t1), info = "`Tree` should store `nodes`")

  expect_is(t1$nodes, "list", info = "`nodes` is a list")

  expect_is(nodes(t1), "list", info = "nodes() as an accessor")
})


# .. it can add `Node`s to it's node list using add_node(tree, node); and
# returns a Tree

test_that(".. it can add `Node`s to it's `nodes`", {
  t1 <- tree()
  n1 <- node()
  expect_is(
    add_node(t1, n1), "Tree",
    info = "`add_node` returns a Tree"
  )
  expect_equal(
    length(nodes(add_node(t1, n1))),
    length(nodes(t1)) + 1,
    info = "add_node increments the number of nodes in a Tree"
  )
})

test_that(".. it knows if it is empty", {
  t1 <- tree()
  n0 <- node()

  expect_true(is_empty(t1), info = "empty `Tree`")
  expect_false(is_empty(add_node(t1, n0)), info = "non-empty `Tree`")
})

test_that(".. it's nodes should be named", {
  t1 <- tree()
  n0 <- node() # root
  n1 <- node(name = "alt_root")

  expect_equal(
    names(nodes(add_node(t1, n0))), "root",
    info = "`nodes` have names"
  )
  expect_equal(
    names(nodes(add_node(t1, n1))), "alt_root",
    info = "`nodes` with non-default names"
  )
})

# .. it can add `Node`s, but must add children after their parents

test_that(" .. it can add `Nodes` but only after their parents", {
  # a new Node can be added to a non-empty Tree if it's parent is present

  root <- node(name = "root") # root
  alt_root <- node(name = "alt_root")
  n1 <- node(name = "a", parent = "root")
  n2 <- node(name = "b", parent = "a")

  t3 <- tree() %>% add_node(root) %>% add_node(n1)
  expect_equal(
    names(nodes(t3)), c("root", "a"),
    info = "adding a `Node` to a non-empty `Tree`: parent is present"
  )

  expect_error(
    tree() %>% add_node(root) %>% add_node(n2),
    info = "adding a `Node` to a non-empty `Tree`: parent is absent"
  )

  expect_error(
    tree() %>% add_node(n1),
    info = "can't add a `Node` that has parents to an empty `Tree`"
  )

  expect_error(
    tree() %>% add_node(root) %>% add_node(alt_root),
    info = "only one `Node` is allowed to have no parents"
  )

  expect_error(
    tree() %>% add_node(root) %>% add_node(n1) %>% add_node(n1),
    info = "can't add the same `Node` twice"
  )
})

test_that(".. it updates a parent's data when adding a child node", {
  t1 <- tree()
  n0 <- node(name = "root")
  n1 <- node(name = "a", parent = "root")
  n0_with_children <- node(name = "root", children = "a")

  expect_equal(
    object = nodes(t1 %>% add_node(n0) %>% add_node(n1))[["root"]],
    expected = n0_with_children,
    info = paste(
      "children-field of parent should be updated when a new child is added"
    )
  )
})

# a function can be `map`ped across the nodes in a tree, returning
# - a named list if `.field = NULL`
# or another tree if `.field = "some_field_name"`

test_that(".. it's nodes can be mapped over", {
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

  t3 <- tree() %>%
    add_node(node(name = "root", n_siblings = 1)) %>%
    add_node(node(name = "a", parent = "root", n_siblings = 2)) %>%
    add_node(node(name = "b", parent = "a", n_siblings = 1)) %>%
    add_node(node(name = "c", parent = "root", n_siblings = 2))

  expect_equal(
    map_tree(t1, parent_name),
    list(root = NULL, a = "root", b = "a", c = "root"),
    info = "map over the nodes of a tree: returning a list"
  )

  expect_equal(
    map_tree(t1, parent_name, .field = "alt_parent"),
    t2,
    info = "if .field is set, mapping over a tree returns an updated tree"
   )

  count_siblings <- function(node, tree) {
    n_siblings <- if(has_parent(node)){
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
    info = "map over nodes, using info that can't be got from the node alone"
  )
})

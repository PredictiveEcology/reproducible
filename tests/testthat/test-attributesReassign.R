## attributesReassign(): restores attributes that were stripped, without
## touching the known/structural ones. Called once per object while unwrapping,
## so it is a hot path.

test_that("attributesReassign restores unknown attributes only", {
  obj <- 1:3
  atts <- list(class = "should-not-apply", myTag = "kept", other = 42)

  out <- attributesReassign(atts, obj)

  expect_identical(attr(out, "myTag"), "kept")
  expect_identical(attr(out, "other"), 42)
  ## `class` is a known attribute and must not be reassigned
  expect_false(identical(class(out), "should-not-apply"))
})

test_that("attributesReassign does not overwrite an attribute the object has", {
  obj <- structure(1:3, myTag = "original")

  out <- attributesReassign(list(myTag = "replacement"), obj)

  expect_identical(attr(out, "myTag"), "original")
})

test_that("attributesReassign is a no-op for empty or all-known attributes", {
  obj <- 1:3
  expect_identical(attributesReassign(list(), obj), obj)
  expect_identical(attributesReassign(list(class = "x", cpp = 1), obj), obj)
  expect_identical(attributesReassign(NULL, obj), obj)
})

test_that("attributesReassign matches the setdiff() formulation it replaced", {
  ## the old body, for reference
  old <- function(atts, obj) {
    attsNames <- setdiff(names(atts), knownAtts)
    if (length(attsNames))
      for (att in attsNames) {
        if (is.null(attr(obj, att))) attr(obj, att) <- atts[[att]]
      }
    obj
  }
  cases <- list(character(0), "class", c("class", "a"), c("a", "b"),
                c("cpp", "ptr", "z"),
                ## duplicated names: setdiff() dedupes and the replacement does
                ## not, so the loop runs twice -- but the second pass finds the
                ## attribute already set, so the result is the same.
                c("a", "a"))
  for (nms in cases) {
    atts <- stats::setNames(as.list(seq_along(nms)), nms)
    expect_identical(attributesReassign(atts, 1:3), old(atts, 1:3),
                     info = paste(nms, collapse = ","))
  }
})

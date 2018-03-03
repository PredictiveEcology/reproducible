all.equalWONewCache <- function(a, b) {
  attr(a, "newCache") <- NULL
  attr(b, "newCache") <- NULL
  all.equal(a,b)
}

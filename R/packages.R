utils::globalVariables(c(
  ".SD", "..colsToKeep", "Account", "availableOnCRAN", "availableOnGitHub", "AvailableVersion",
  "Branch", "compareVersionAvail", "compareVersionAvailGH", "correctVersion", "correctVersionAvail",
  "correctVersionAvailGH", "fullGit", "githubPkgName", "inequality", "instPkgs", "isGH",
  "minVersion", "Package", "Repo", "RepoWBranch", "Version", "versionOnGH"
))


#' `NA`-aware comparison of two vectors
#'
#' Copied from
#' <http://www.cookbook-r.com/Manipulating_data/Comparing_vectors_or_factors_with_NA/>.
#' This function returns `TRUE` wherever elements are the same, including `NA`'s,
#' and `FALSE` everywhere else.
#'
#' @export
#' @param v1 A vector
#' @param v2 A vector
#' @return
#' A logical vector, indicating positions where two vectors are same or differ.
#'
#' @examples
#' a <- c(NA, 1, 2, NA)
#' b <- c(1, NA, 2, NA)
#' compareNA(a, b)
#'
compareNA <- function(v1, v2) {
  same <- (v1 == v2) | (is.na(v1) & is.na(v2))
  same[is.na(same)] <- FALSE
  return(same)
}


# internetExists <- function() {
#   if (requireNamespace("RCurl", quietly = TRUE)) {
#     out <- RCurl::url.exists("www.google.com")
#   } else {
#     messagePrepInputs(RCurlMess)
#     out <- urlExists("https://www.google.com")
#     # if (.Platform$OS.type == "windows") {
#     #   ipmessage <- system("ipconfig", intern = TRUE)
#     # } else {
#     #   ipmessage <- system("ifconfig", intern = TRUE)
#     # }
#     # validIP <- "((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)[.]){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)"
#     # linesWithIP <- grep(validIP, ipmessage)
#     # out <- if (length(linesWithIP)) {
#     #   any(linesWithIP) && all(!grepl("127.0.0.1", ipmessage[linesWithIP]))
#     # } else {
#     #   FALSE
#     # }
#     # out
#   }
#   return(out)
# }

RCurlMess <- paste0(
  "install.packages('RCurl') may give a more reliable detection ",
  "of internet connection"
)


.readline <- function(prompt) {
  readline(prompt)
}

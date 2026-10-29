## A minimal HTTP/1.1 server in a separate R process, base R only (serverSocket()), for download
## tests that need what a real server does and a file:// url cannot: a Content-Length, an ETag,
## and a connection that drops part-way through the body.
##
## `body` is served at /<name>; everything else is a 404. The first `dropFirst` GETs send the
## headers and only a third of the body, then close the connection. Returns the base url and a
## function that stops the server.
startTestHttpServer <- function(body, name = "file.bin", dropFirst = 0L, etag = '"abc123"',
                                dir = tempfile("httpServer")) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  bodyFile <- file.path(dir, "body.bin")
  portFile <- file.path(dir, "port.txt")
  writeBin(body, bodyFile)
  script <- file.path(dir, "server.R")
  writeLines(c(
    sprintf('body <- readBin("%s", "raw", file.size("%s"))', bodyFile, bodyFile),
    'ss <- NULL',
    'for (try in 1:50) {',
    '  port <- sample(20000:60000, 1)',
    '  ss <- tryCatch(serverSocket(port), error = function(e) NULL)',
    '  if (!is.null(ss)) break',
    '}',
    sprintf('writeLines(as.character(port), "%s.tmp"); file.rename("%s.tmp", "%s")',
            portFile, portFile, portFile),
    'gets <- 0L',
    'repeat {',
    '  con <- socketAccept(ss, blocking = TRUE, open = "r+b", timeout = 300)',
    '  req <- character()',
    '  repeat {',
    '    l <- readLines(con, n = 1)',
    '    if (!length(l) || !nzchar(sub("\\r$", "", l))) break',
    '    req <- c(req, l)',
    '  }',
    '  if (!length(req)) { close(con); next }',
    '  parts <- strsplit(req[1], " ")[[1]]',
    '  if (identical(parts[2], "/quit")) { close(con); break }',
    sprintf('  if (!identical(parts[2], "/%s")) {', name),
    '    writeBin(charToRaw("HTTP/1.1 404 Not Found\\r\\nContent-Length: 0\\r\\nConnection: close\\r\\n\\r\\n"), con)',
    '    close(con); next',
    '  }',
    '  hdr <- paste0("HTTP/1.1 200 OK\\r\\nContent-Type: application/octet-stream\\r\\n",',
    '                "Content-Length: ", length(body), "\\r\\n",',
    sprintf('                "ETag: %s\\r\\nConnection: close\\r\\n\\r\\n")', gsub('"', '\\\\"', etag)),
    '  writeBin(charToRaw(hdr), con)',
    '  if (identical(parts[1], "GET")) {',
    '    gets <- gets + 1L',
    sprintf('    writeBin(if (gets <= %dL) body[seq_len(length(body) %%/%% 3)] else body, con)',
            as.integer(dropFirst)),
    '  }',
    '  flush(con); close(con)',
    '}'
  ), script)
  system2(file.path(R.home("bin"), "Rscript"), shQuote(script),
          stdout = file.path(dir, "server.log"), stderr = file.path(dir, "server.log"), wait = FALSE)
  deadline <- Sys.time() + 30
  while (!file.exists(portFile) && Sys.time() < deadline) Sys.sleep(0.1)
  if (!file.exists(portFile)) testthat::skip("test HTTP server did not start")
  base <- paste0("http://127.0.0.1:", readLines(portFile))
  list(url = paste0(base, "/", name),
       stop = function() try(suppressWarnings(readLines(paste0(base, "/quit"))), silent = TRUE))
}

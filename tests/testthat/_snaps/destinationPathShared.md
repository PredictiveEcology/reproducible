# C5: deleted local file + stale sidecar + file in destinationPathShared → relink, no download

    Code
      invisible(preProcess(url = fixSrc$url, targetFile = "foo.csv", destinationPath = dest, fun = NA, verbose = 1))
    Message
      Running `preProcess`
      Preparing: foo.csv
      Skipping download. All requested files already present
      ...using file(s) in getOption('reproducible.inputPaths')...
      Hardlinked version of file(s) created:
      ... no copy/copies made.

# C6: deleted local file + stale sidecar + nothing in shared → download proceeds

    Code
      invisible(preProcess(url = fixSrc$url, targetFile = "foo.csv", destinationPath = dest, fun = NA, verbose = 1))
    Message
      Running `preProcess`
      Preparing: foo.csv
      ...downloading...
      Downloading <file-url> ...
      Appending checksums to CHECKSUMS.txt. If you see this message repeatedly, you can specify targetFile (and optionally alsoExtract) so it knows what to look for.


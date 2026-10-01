# reproducible (development version)

## New features

* A `type = "dir"` row in the `reproducible.urlRemap` manifest is now answered from the manifest's own file rows under that folder's prefix, so `listGoogleDriveFolder()` and Drive-folder downloads work with buckets that deny anonymous ListBucket. The bucket listing is used only when no file row matches.


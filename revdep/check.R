#devtools::install_github("r-lib/revdepcheck")
library("revdepcheck")

revdep_check(num_workers = getOption("Ncpus", 1))

### email maintainers of revdep packages (need to edit: `revdep/email.yml`)
revdep_email(type = "broken") ## will send via gmail
revdep_email(type = "failed") ## will send via gmail

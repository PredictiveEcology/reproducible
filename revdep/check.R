if (packageVersion("devtools") < "1.13.5.9000") {
  library("devtools")

  revdep_check()
  revdep_check_save_summary()
  revdep_check_print_problems()
} else {
  #devtools::install_github("r-lib/revdepcheck")
  library("revdepcheck")

  revdep_check(num_workers = getOption("Ncpus", 1))

  ### email maintainers of revdep packages (need to edit: `revdep/email.yml`)
  #revdep_email(type = "broken") ## will send via gmail
  #revdep_email(type = "failed") ## will send via gmail
}

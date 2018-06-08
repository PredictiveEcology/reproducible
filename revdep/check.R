library("devtools")

revdep_check(env_vars = c(DISPLAY = ":0"))
revdep_check_save_summary()
revdep_check_print_problems()

# # devtools::load_all()
# # t.out_1 <- tinytest::run_test_file(file.path(getwd(), "inst", "tinytest", "test_lookup_by_match__lookup_next.R"), verbose = 3L)
# # t.out_1
# 
# #load(system.file("R", "sysdata.rda", package = "bgrsmisc"))
# 
# # lookup_by_match__lookup_next tests on ** KA6_Tbl_B4_KW_H2O_B ** ----
# message("Starting lookup_by_match__lookup_next tests on ** KA6_Tbl_B4_KW_H2O_B ** ...")
# 
# b4 <- KA6_Tbl_B4_KW_H2O_B
# 
# ## expect identical ----
# 
# message("* Run lookup_by_match_lookup_next tests for where == center | fwd - expect identical ...")
# 
# # define list of args lists
# # approach with lapply and function call
# listOfArgLists <- list(list(x = b4, par_oi = "nFK", by_par = c("Bodenart", "TRD_num"), by_val = list("Ss", 1.1),
#                             lookup_next = list(FALSE, TRUE), lookup_next_if = NA, no_match_retVal = 999,
#                             lookup_next_where = "center"),
#                        list(x = b4, par_oi = "nFK", by_par = c("Bodenart", "TRD_num"), by_val = list("Ss", 1.1),
#                             lookup_next = list(FALSE, TRUE), lookup_next_if = NA, no_match_retVal = 999,
#                             lookup_next_where = "fwd")
#                        )
# 
# 
# lapply(listOfArgLists, function(l) {
#   expect_identical(do.call(lookup_by_match, 
#                            c(list(verbose = FALSE), # add arg to suppress msg
#                              l)),
#                    4L)})
# 
# 
# message("* Run lookup_by_match_lookup_next tests for where == back - expect identical ...")
# 
# # define list of args lists
# # approach with lapply and function call
# listOfArgLists <- list(list(x = b4, par_oi = "nFK", by_par = c("Bodenart", "TRD_num"), by_val = list("Tt", 1.9),
#                             lookup_next = list(FALSE, TRUE), lookup_next_if = NA, no_match_retVal = 999,
#                             lookup_next_where = "back", lookup_next_max = 2L),
#                        list(x = b4, par_oi = "nFK", by_par = c("Bodenart", "TRD_num"), by_val = list("Tt", 1.7),
#                             lookup_next = list(FALSE, TRUE), lookup_next_if = NA, no_match_retVal = 999,
#                             lookup_next_where = "back", lookup_next_max = 2L)
# )
# 
# 
# lapply(listOfArgLists, function(l) {
#   expect_identical(do.call(lookup_by_match, 
#                            c(list(verbose = FALSE), # add arg to suppress msg
#                              l)),
#                    9L)})
# 
# 
# message("* Run lookup_by_match_lookup_next tests with NA in 'next', returns NA  - expect identical ...")
# 
# # define list of args lists
# # approach with lapply and function call
# listOfArgLists <- list(list(x = b4, par_oi = "nFK", by_par = c("Bodenart", "TRD_num"), by_val = list("Lt2", 1.7),
#                             lookup_next = list(FALSE, TRUE), lookup_next_if = 9, no_match_retVal = 999,
#                             lookup_next_where = "edges", lookup_next_max = 2L),
#                        list(x = b4, par_oi = "nFK", by_par = c("Bodenart", "TRD_num"), by_val = list("Lt2", 1.7),
#                             lookup_next = list(FALSE, TRUE), lookup_next_if = 9, no_match_retVal = 999,
#                             lookup_next_where = "fwd", lookup_next_max = 2L)
# )
# 
# 
# lapply(listOfArgLists, function(l) {
#   expect_identical(do.call(lookup_by_match, 
#                            c(list(verbose = FALSE), # add arg to suppress msg
#                              l)),
#                    NA)})
# 
# 
# message("* Run lookup_by_match_lookup_next tests with no match in 'next', returns lookup_next_if - expect identical ...")
# 
# # define list of args lists
# # approach with lapply and function call
# # produces warnings
# listOfArgLists <- list(list(x = b4[seq_len(nrow(b4)-1),], par_oi = "nFK", by_par = c("Bodenart", "TRD_num"), by_val = list("gS", 1.7),
#                             lookup_next_where = "fwd"),
#                        list(x = b4[seq_len(nrow(b4)-1),], par_oi = "nFK", by_par = c("Bodenart", "TRD_num"), by_val = list("gS", 1.7),
#                             lookup_next_where = "edges")
# )
# 
# 
# lapply(listOfArgLists, function(l) {
#   expect_identical(do.call(lookup_by_match, 
#                            c(list(lookup_next = list(FALSE, c(1.3, 1.5, 1.7, 1.9)), 
#                                   lookup_next_if = 5L, 
#                                   no_match_retVal = 999,
#                                   no_match_final = "allow",
#                                   verbose = FALSE), # add arg to suppress msg
#                              l)),
#                    5L)})
# 
# 
# ## expect message ----
# 
# message("* Run lookup_by_match_lookup_next tests with verbose output - expect message ...")
# 
# # define list of args lists
# # approach with lapply and function call
# listOfArgLists <- list(list(x = b4, par_oi = "nFK", by_par = c("Bodenart", "TRD_num"), by_val = list("Tt", 1.9),
#                             lookup_next = list(FALSE, TRUE), lookup_next_if = NA, no_match_retVal = 999,
#                             lookup_next_where = "back", lookup_next_max = 2L),
#                        list(x = b4, par_oi = "nFK", by_par = c("Bodenart", "TRD_num"), by_val = list("Tt", 1.7),
#                             lookup_next = list(FALSE, TRUE), lookup_next_if = NA, no_match_retVal = 999,
#                             lookup_next_where = "back", lookup_next_max = 2L)
# )
# 
# 
# lapply(listOfArgLists, function(l) {
#   expect_message(do.call(lookup_by_match, 
#                            c(list(verbose = TRUE), # add arg for verbose msg
#                              l)))})#,
#                    #9L)})
# 
# 
# message("* Run lookup_by_match_lookup_next tests with verbose output (2) - expect message ...")
# 
# # define list of args lists
# # approach with lapply and function call
# listOfArgLists <- list(list(x = b4, par_oi = "nFK", by_par = c("Bodenart", "TRD_num"), by_val = list("Ss", 1.1),
#                             lookup_next = list(FALSE, TRUE), lookup_next_if = NA, no_match_retVal = 999,
#                             lookup_next_where = "center"),
#                        list(x = b4, par_oi = "nFK", by_par = c("Bodenart", "TRD_num"), by_val = list("Ss", 1.1),
#                             lookup_next = list(FALSE, TRUE), lookup_next_if = NA, no_match_retVal = 999,
#                             lookup_next_where = "fwd")
# )
# 
# 
# lapply(listOfArgLists, function(l) {
#   expect_message(do.call(lookup_by_match, 
#                          c(list(verbose = TRUE), # add arg for verbose msg 
#                            l))#,
#                  #4L
#   )})
# 
# 
# ## expect warning ----
# 
# message("* Run lookup_by_match_lookup_next tests with index already on edge - expect warning ...")
# 
# # define list of args lists
# # approach with lapply and function call
# listOfArgLists <- list(list(x = b4, par_oi = "nFK", by_par = c("Bodenart", "TRD_num"), by_val = list("Ls2", 1.1),
#                             lookup_next = list(FALSE, TRUE), lookup_next_if = 16, no_match_retVal = 999,
#                             lookup_next_where = "back", lookup_next_max = 2L),
#                        list(x = b4, par_oi = "nFK", by_par = c("Bodenart", "TRD_num"), by_val = list("Ls3", 1.1),
#                             lookup_next = list(FALSE, TRUE), lookup_next_if = 16, no_match_retVal = 999,
#                             lookup_next_where = "back", lookup_next_max = 2L)
# )
# 
# 
# lapply(listOfArgLists, function(l) {
#   expect_warning(do.call(lookup_by_match,
#                          c(list(verbose = FALSE), # add arg to suppress msg
#                            l))#,
#                  #16L
#                  )})

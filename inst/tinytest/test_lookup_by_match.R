# # devtools::load_all()
# # t.out_1 <- tinytest::run_test_file(file.path(".", "inst", "tinytest", "test_lookup_by_match.R"), verbose = 3L)
# # t.out_1
# 
# message("Starting lookup_by_match tests ...")
# 
# #load(system.file("R", "sysdata.rda", package = "bgrsmisc"))
# 
# # # exp data moved to sysdata
# # bn <- c("a", "bb", "bb", "d", "e", "f", "g") ##
# # dtyp <- c("n", "cl", NA, "", "o", "cl", "n")
# # dtyp2 <- c(1, NA, 3, NA, 5, 6, 66)
# # dtyp3 <- c(rep(TRUE, 4), rep(FALSE, 3))
# # srcpar <- c("I1", "I2", "I3", "I4", "I5", "I6", "I7")
# # rx <- c("a$", "b$", "c$", "d$", "e$", "f$", "g$") ##
# # ordrd <- c(1,2,1,2,1,2,1)
# # cats <- c("a", "a", "b", "b", "c", "c", "d")
# # sparse <- c(NA, NA, NA, NA, 71, 72, NA)
# # Df <- data.frame(srcpar,bn,dtyp,dtyp2,dtyp3,rx,ordrd,cats,sparse,
# #                 row.names = srcpar,
# #                 stringsAsFactors = F);Df
# 
# # # list of default args
# # listOfDefaultArgs <- list(lookup_next = FALSE, # LIST! #list(rep(FALSE, length(by_par))), # just applicable to ordinal by_par data; list of FALSE or ordered vec of values of by_par
# #                           lookup_next_if = NA,
# #                           lookup_next_where = "center",
# #                           lookup_next_max = 1L, # default
# #                           no_match_retVal = NA,
# #                           no_match_final = "warn",
# #                           dups_final = "warn",
# #                           dups_strict_final = TRUE,
# #                           simplify = TRUE,
# #                           ret_inputClass = FALSE,
# #                           add_bypar = FALSE,
# #                           add_names_byval = FALSE,
# #                           add_names_paroi = FALSE,
# #                           no_match = "warn",
# #                           dups = "allow",
# #                           dups_strict = TRUE,
# #                           fixed = TRUE, # rep(TRUE, length(by_par)), # boolean vec of length length(by_par)
# #                           regex_pattern = "by_par", # by_par is regexpr to be applied for checking
# #                           is.na_regex_ret = FALSE, # return value if regex is missing / NA, NA is default by grepl as well
# #                           verbose = TRUE) # more detailed output of checkAndHandelBoolVec)
# # 
# # listOfDefaultArgs[["verbose"]] <- FALSE
# # as.list(by_val = "a", listOfDefaultArgs)
# 
# # data.frame ----
# 
# ##### no match ####
# 
# # define list of args lists
# # approach with lapply and function call
# listOfArgLists <- list(list(x = Df, par_oi = "srcpar", by_par = "bn", by_val = "h"),
#                        list(x = Df, par_oi = "srcpar", by_par = "bn", by_val = NA),
#                        list(x = Df, par_oi = "srcpar", by_par = "bn", by_val = 1),
#                        list(x = Df, par_oi = "srcpar", by_par = "bn", by_val = TRUE),
#                        list(x = Df, par_oi = "srcpar", by_par = "bn", by_val = character(0)))
# 
# message("* Run lookup_by_match tests: no match - expect identical ...")
# 
# message("... to different return values in case of no match!")
# lapply(listOfArgLists, function(l) {
#   expect_identical(do.call(lookup_by_match, 
#                            c(list(#no_match_retVal = NA, # default 
#                                   no_match_final = "allow", # add arg to suppress warn
#                                   verbose = FALSE), # add arg to suppress msg
#                                   
#                              l)),
#                    NA)
#   
#   expect_identical(do.call(lookup_by_match, 
#                            c(list(no_match_retVal = "NA",
#                                   no_match_final = "allow", # add arg to suppress warn
#                                   verbose = FALSE), # add arg to suppress msg
#                              l)),
#                    "NA")
#   
#   expect_identical(do.call(lookup_by_match, 
#                            c(list(no_match_retVal = "1",
#                                   no_match_final = "allow", # add arg to suppress warn
#                                   verbose = FALSE), # add arg to suppress msg
#                              l)),
#                    "1")
#   expect_identical(do.call(lookup_by_match, 
#                            c(list(no_match_retVal = TRUE,
#                                   no_match_final = "allow", # add arg to suppress warn
#                                   verbose = FALSE), # add arg to suppress msg
#                              l)),
#                    "TRUE")
#   expect_identical(do.call(lookup_by_match, 
#                            c(list(no_match_retVal = 99L,
#                                   no_match_final = "allow", # add arg to suppress warn
#                                   verbose = FALSE), # add arg to suppress msg
#                              l)),
#                    "99")
#   
# })
# 
# 
# # # with warnings
# # expect_identical(do.call(lookup_by_match, 
# #                          c(list(no_match_retVal = 1.0,
# #                                 no_match_final = "allow", # add arg to suppress warn
# #                                 verbose = FALSE), # add arg to suppress msg
# #                            l)),
# #                  "1")
# 
# message("Finished lookup_by_match tests!")
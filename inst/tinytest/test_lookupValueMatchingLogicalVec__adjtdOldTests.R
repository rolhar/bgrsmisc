# # # DEVEL # #
# # Just run this testfile in devel process:
# # devtools::load_all()
# # t.out_1 <- tinytest::run_test_file(file.path(".", "inst", "tinytest", "test_lookupValueMatchingLogicalVec__adjtdOldTests.R"), verbose = 3L)
# # t.out_1
# # # # # # # #
# 
# #load(system.file("R", "sysdata.rda", package = "bgrsmisc"))
# 
# # how to test multiple cases
# #
# # one approach in 20230715_test_testfile_.replaceByMatch.R
# # other approach online ...? With call???
# #
# # rm(list = ls())
# # source("20230429_selectFromOneParam_firstEdt_fct.R")
# # source("20230429_selectFromOneParamByParamsToList_secEdt_fct.R")
# # source("20230516_checkForDupsAndHandleBoolVec_methods.R")
# # source("20230429_calcSubsetBoolVec_methods.R")
# # source("20230429_selectByParamsToList_thirdEdt_lfd.R")
# # source("20230608_selectByMatch_methods.R")
# # source("20230702_replaceByMatch_methods.R")
# # source("20240224_checkAndHandleBoolVec_methods.R")
# # source("20240224_lookupSingleMachtingBoolVec_methods.R")
# #
# # # tiny test
# # t.out_1 <- tinytest::run_test_file("20230606_test_testfile_selectByParamsToList_and_internals.R", verbose = 2L)
# # t.out_1
# # t.out_2 <- tinytest::run_test_file("20230715_test_testfile_.replaceByMatch.R", verbose = 2L)
# # t.out_2
# # #as.data.frame(t.out_2)
# 
# message("Starting .lookupValueMatchingLogicalVec tests ...")
# 
# # # assignment Pair / matrix / df
# # # no NULL allowd!
# # #bn <- c("a", "bb", "bb", "d")
# # #bn <- c("a", "bb", "bb", "d", NA, "", "g") ##
# # bn <- c("a", "bb", "bb", "d", "e", "f", "g") ##
# # #dtyp <- c("n", "cl", "cl", "o")
# # dtyp <- c("n", "cl", NA, "", "o", "cl", "n")
# # dtyp2 <- c(1, NA, 3, NA, 5, 6, 66)
# # dtyp3 <- c(rep(TRUE, 4), rep(FALSE, 3))
# # #srcpar <- c("I1", "I2", "I3", "I4")
# # srcpar <- c("I1", "I2", "I3", "I4", "I5", "I6", "I7")
# # rx <- c("a$", "b$", "c$", "d$", "e$", "e$", "f$") ##
# # bncol <- "bn"
# # dtypcol <- "dtyp"
# # dtypcol2 <- "dtyp2"
# # dtypcol3 <- "dtyp3"
# # inpcol <- "srcpar"
# # rxcol <- "rx"
# # # P <- Pair(bn,dtyp)
# # # row.names(P) <- srcpar;P
# # # colnames(P) <- c(bncol,dtypcol);P
# # M <- matrix(c(srcpar,bn,dtyp, dtyp2, dtyp3, rx), ncol = 6);M
# # row.names(M) <- srcpar;M
# # colnames(M) <- c(inpcol,bncol,dtypcol, dtypcol2, dtypcol3, rxcol);M
# # Df <- data.frame(srcpar,bn,dtyp,dtyp2,dtyp3,rx,
# #                 row.names = srcpar,
# #                 stringsAsFactors = F);Df
# # L <- list(srcpar, bn, dtyp, dtyp2, dtyp3,rx)
# # names(L) <- c(inpcol,bncol,dtypcol,dtypcol2, dtypcol3,rxcol)
# 
# # P;
# # M;Df;L
# # M[,"rx"]
# # class(Df)
# # S3C <- structure(Df, class = c("palloc", "data.frame"))
# # summary(S3C)
# # class(S3C)
# # inherits(S3C,"data.frame")
# # str(S3C)
# # S3C[,"bn"]
# # isTRUE("palloc" %in% class(S3C))
# # ds <- Df[1:3,]
# # ds[(nrow(ds)+1):nrow(Df), ] <- NA
# # str(cbind(ds,Df))
# #
# 
# 
# #### data.frame ####
# 
# ##### no match ####
# 
# # define list of args lists
# # approach with lapply and function call
# listOfArgLists <- list(list(x = Df, by_par = "bn", by_val = "bbn"),
#                        list(x = Df, by_par = "bn", by_val = NA))
# 
# 
# ###### expect identical ----
# 
# message("* Run .lookupValueMatchingLogicalVec tests: no match - expect identical ...")
# 
# # # Alternative to lapply ... do.call and passing a list
# lapply(listOfArgLists, function(l) {
#   expect_identical(do.call(.lookupValueMatchingLogicalVec, c(list(no_match = "allow"), # add arg to suppress warn / msg
#                                                               l)),
#                    rep(FALSE, nrow(Df)))
# })
# 
# 
# # ###
# # regex no match
# 
# # define list of args lists
# listOfArgLists <- list(list(x = Df, by_par = "rx", by_val = "abcdg"),
#                        list(x = Df, by_par = "rx", by_val = c("x", "y")),
#                        list(x = Df, by_par = "rx", by_val = NA))
# 
# # # do.call and passing a list
# lapply(listOfArgLists, function(l) {
#   expect_identical(do.call(.lookupValueMatchingLogicalVec, c(list(no_match = "allow"), # add arg to suppress warn / msg
#                                                              l)),
#                    rep(FALSE, nrow(Df)))
# })
# 
# 
# # # produces warnings (2 warnings all together)
# # lapply(listOfArgLists, function(l) {
# #   expect_identical(do.call(.lookupValueMatchingLogicalVec, c(list(no_match = "warn"), # add arg to suppress warn / msg
# #                                                              l)),
# #                    rep(FALSE, nrow(Df)))
# # })
# 
# 
# ###### expect warning ####
# 
# message("* Run .lookupValueMatchingLogicalVec tests: no match - expect warning ...")
# 
# #l <- listOfArgLists[[1]]
# lapply(listOfArgLists, function(l) {
#   expect_warning(.lookupValueMatchingLogicalVec(x = l[["x"]], by_par = l[["by_par"]],
#                                                  by_val = l[["by_val"]]),
#                  strict = TRUE)
# })
# 
# 
# ###### expect error ####
# 
# message("* Run .lookupValueMatchingLogicalVec tests: no match - expect identical ...")
# 
# # # Alternative to lapply ... do.call and passing a list
# lapply(listOfArgLists, function(l) {
#   expect_error(do.call(.lookupValueMatchingLogicalVec, c(list(no_match = "error"), # add arg to suppress warn / msg
#                                                               l)))
# })
# 
# 
# ##### regex by_par matching by_val ####
# 
# ###### expect identical ####
# 
# message("* Run .lookupValueMatchingLogicalVec tests: regex by_par match by_val - expect identical ...")
# 
# # regex_pattern == "by_par"
# 
# # one match
# # rx == c("a$", "b$", "c$", "d$", "e$", "e$", "f$")
# listOfArgLists <- list(list(x = Df, by_par = "rx", by_val = "abcd", fixed = FALSE),
#                        list(x = Df, by_par = "rx", by_val = c("d", "g"), fixed = FALSE))
# 
# # # do.call and passing a list
# lapply(listOfArgLists, function(l) {
#   expect_identical(do.call(.lookupValueMatchingLogicalVec, c(#list(no_match = "allow"), # add arg to suppress warn / msg
#                                                              l)),
#                    c(F,F,F,T,F,F,F))
# })
# 
# 
# # multiple matches
# # rx == c("a$", "b$", "c$", "d$", "e$", "e$", "f$") ##
# listOfArgLists <- list(list(x = Df, by_par = "rx", by_val = "abcde", fixed = FALSE),
#                        list(x = Df, by_par = "rx", by_val = c("x", "e"), fixed = FALSE))
# 
# # # do.call and passing a list
# lapply(listOfArgLists, function(l) {
#   #expect_identical(do.call(.lookupValueMatchingLogicalVec, c(list(dups = "msg"), # add arg to suppress warn / msg
#   expect_identical(do.call(.lookupValueMatchingLogicalVec, c(list(dups = "allow"), # add arg to suppress warn / msg
#     l)),
#     c(F,F,F,F,T,T,F))
# })
# 
# 
# #...
# ##### regex by_val matching by_par ####
# 
# ###### expect identical ####
# 
# message("* Run .lookupValueMatchingLogicalVec tests: regex by_val matching by_par - expect identical ...")
# 
# # one match
# 
# #Df[["dtyp"]] # "n"  "cl" NA   ""   "o"  "cl" "n" 
# 
# listOfArgLists <- list(list(x = Df, by_par = "dtyp", by_val = "^o$", fixed = FALSE, regex_pattern="by_val"),
#                        list(x = Df, by_par = "bn", by_val = c("^e"), fixed = FALSE, regex_pattern="by_val"))
# 
# # # do.call and passing a list
# lapply(listOfArgLists, function(l) {
#   expect_identical(do.call(.lookupValueMatchingLogicalVec, c(#list(no_match = "allow"), # add arg to suppress warn / msg
#     l)),
#     c(F,F,F,F,T,F,F))
# })
# 
# 
# # multiple matches
# 
# listOfArgLists <- list(list(x = Df, by_par = "dtyp", by_val = "^n", fixed = FALSE, regex_pattern="by_val"),
#                        list(x = Df, by_par = "bn", by_val = c("^(a|g)"), fixed = FALSE, regex_pattern="by_val"))
# 
# # # do.call and passing a list
# lapply(listOfArgLists, function(l) {
#   #expect_identical(do.call(.lookupValueMatchingLogicalVec, c(list(dups = "msg"), # add arg to suppress warn / msg
#   expect_identical(do.call(.lookupValueMatchingLogicalVec, c(list(dups = "allow"), # add arg to suppress warn / msg
#                                                              l)),
#                    c(T,F,F,F,F,F,T))
# })
# 
# 
# ##### ________ ####
# 
# 
# ##### O to be continued ... ####
# 
# message("O To be continued .lookupValueMatchingLogicalVec tests ...!")
# 
# # arg.list <- list(b=2, c=5)
# # do.call(foo, c(list(a=1), arg.list))
# # do.call(foo2, arg.list)
# 
# #
# # # missing param
# # .lookupValueMatchingLogicalVec(x = Df, by_val = "b") # exp error
# # # .lookupValueMatchingLogicalVec(x = M, by_val = "b")
# # # .lookupValueMatchingLogicalVec(x = L, by_val = "b")
# #
# #
# #
# # # dups detected - warn
# # .lookupValueMatchingLogicalVec(x = Df, by_par = "bn", by_val = "bb")
# # # .lookupValueMatchingLogicalVec(x = M, by_par = "bn", by_val = "bb")
# # # .lookupValueMatchingLogicalVec(x = L, by_par = "bn", by_val = "bb")
# #
# # # list in by_val not supported - error
# # .lookupValueMatchingLogicalVec(x = Df, by_par = "bn", by_val = list("bb"))
# # # .lookupValueMatchingLogicalVec(x = M, by_par = "bn", by_val = list("bb"))
# # # .lookupValueMatchingLogicalVec(x = L, by_par = "bn", by_val = list("bb"))
# #
# # # list not supported / Error resp not working ...
# # .lookupValueMatchingLogicalVec(x = Df, by_par = "bn", by_val = list(c("bb", "d")))
# # # .lookupValueMatchingLogicalVec(x = M, by_par = "bn", by_val = list("bb"))
# # # .lookupValueMatchingLogicalVec(x = L, by_par = "bn", by_val = list("bb"))
# #
# # # dups - warn
# # .lookupValueMatchingLogicalVec(x = Df, by_par = "bn", by_val = c("bb", "d"))
# # # .lookupValueMatchingLogicalVec(x = M, by_par = "bn", by_val = c("bb", "d"))
# # # .lookupValueMatchingLogicalVec(x = L, by_par = "bn", by_val = c("bb", "d"))
# #
# # # by_par not matching / existent -> error
# # .lookupValueMatchingLogicalVec(x = Df, by_par = "bns", by_val = c("bb", "d"))
# # # .lookupValueMatchingLogicalVec(x = M, by_par = "bns", by_val = c("bb", "d"))
# # # .lookupValueMatchingLogicalVec(x = L, by_par = "bns", by_val = c("bb", "d"))
# #
# # # by_par not matching / existent -> error
# # .lookupValueMatchingLogicalVec(x = Df, by_par = NA, by_val = c("bb", "d"))
# # # .lookupValueMatchingLogicalVec(x = M, by_par = NA, by_val = c("bb", "d"))
# # # .lookupValueMatchingLogicalVec(x = L, by_par = NA, by_val = c("bb", "d"))
# #
# #
# #
# # # by_par not atomic - error
# # .lookupValueMatchingLogicalVec(x = Df, by_par = NULL, by_val = c("bb", "d"))
# # # .lookupValueMatchingLogicalVec(x = M, by_par = NULL, by_val = c("bb", "d"))
# # # .lookupValueMatchingLogicalVec(x = L, by_par = NULL, by_val = c("bb", "d"))
# #
# # # by_val not atomic - error
# # .lookupValueMatchingLogicalVec(x = Df, by_par = "bn", by_val = NULL)
# # # .lookupValueMatchingLogicalVec(x = M, by_par = "bn", by_val = NULL)
# # # .lookupValueMatchingLogicalVec(x = L, by_par = "bn", by_val = NULL)
# #
# # # by_val is list
# # # .lookupValueMatchingLogicalVec(x = Df, by_par = "bn", by_val = L)
# # # .lookupValueMatchingLogicalVec(x = M, by_par = "bn", by_val = L)
# # # .lookupValueMatchingLogicalVec(x = L, by_par = "bn", by_val = L)
# #
# # # double warnings
# # .lookupValueMatchingLogicalVec(x = Df, by_par = c("srcpar", "bn"), by_val = "d")
# # # .lookupValueMatchingLogicalVec(x = M, by_par = c("srcpar", "bn"), by_val = "d")
# # # .lookupValueMatchingLogicalVec(x = L, by_par = c("srcpar", "bn"), by_val = "d")
# #
# # # arg by_val missing - error
# # .lookupValueMatchingLogicalVec(x = Df, by_par = c("srcpar", "bn"))
# # # .lookupValueMatchingLogicalVec(x = M, by_par = c("srcpar", "bn"))
# # # .lookupValueMatchingLogicalVec(x = L, by_par = c("srcpar", "bn"))
# #
# # # ok FALSE FALSE FALSE FALSE FALSE FALSE  TRUE
# # .lookupValueMatchingLogicalVec(x = Df, by_par = "dtyp2", by_val = 66)
# # # .lookupValueMatchingLogicalVec(x = M, by_par = "dtyp2", by_val = 66)
# # # .lookupValueMatchingLogicalVec(x = L, by_par = "dtyp2", by_val = 66)
# #
# # # ok FALSE FALSE FALSE FALSE FALSE FALSE  TRUE
# # .lookupValueMatchingLogicalVec(x = Df, by_par = "dtyp2", by_val = 66L)
# # # .lookupValueMatchingLogicalVec(x = M, by_par = "dtyp2", by_val = 66L)
# # # .lookupValueMatchingLogicalVec(x = L, by_par = "dtyp2", by_val = 66L)
# #
# # # by_val is list - error
# # .lookupValueMatchingLogicalVec(x = Df, by_par = c("bn", "dtyp2"), by_val = list(c("bb", "g"), c(NA,1,66)))
# # # .lookupValueMatchingLogicalVec(x = M, by_par = c("bn", "dtyp2"), by_val = list(c("bb", "g"), c(NA,1,66)))
# # # .lookupValueMatchingLogicalVec(x = L, by_par = c("bn", "dtyp2"), by_val = list(c("bb", "g"), c(NA,1,66)))
# #
# # # by_val is list - error
# # .lookupValueMatchingLogicalVec(x = Df, by_par = c(2,4), by_val = list(c("bb", "g"), c(NA,1L,66L)))
# # # .lookupValueMatchingLogicalVec(x = M, by_par = c(2,4), by_val = list(c("bb", "g"), c(NA,1L,66L)))
# # # .lookupValueMatchingLogicalVec(x = L, by_par = c("bn", "dtyp2"), by_val = list(c("bb", "g"), c(NA,1L,66L)))
# #
# # # by_val is list - error
# # .lookupValueMatchingLogicalVec(x = Df, by_par = c(2,4), by_val = list(c("bb", "g"), c(NA,1L,66L)),dups = "error")
# # # .lookupValueMatchingLogicalVec(x = M, by_par = c(2,4), by_val = list(c("bb", "g"), c(NA,1L,66L)),dups = "error")
# # # .lookupValueMatchingLogicalVec(x = L, by_par = c(2,4), by_val = list(c("bb", "g"), c(NA,1L,66L)),dups = "error")
# # # .lookupValueMatchingLogicalVec(x = L, by_par = c("bn", "dtyp2"), by_val = list(c("bb", "g"), c(NA,1,66)),dups = "error")
# #
# # # by_val is list - error
# # .lookupValueMatchingLogicalVec(x = Df, by_par = c(2,4), by_val = list(c("bb"), c(66L)),dups = "error")
# # .lookupValueMatchingLogicalVec(x = Df, by_par = c(2,4), by_val = list(c("bb"), c(66L)),dups = "error")
# #
# # # by_val is list - error
# # .lookupValueMatchingLogicalVec(x = Df, by_par = c(2,4), by_val = list(c("bb", "g"), c(NA,3L,66L)),dups = "error")
# # # .lookupValueMatchingLogicalVec(x = M, by_par = c(2,4), by_val = list(c("bb", "g"), c(NA,3L,66L)),dups = "error")
# # # .lookupValueMatchingLogicalVec(x = L, by_par = c(2,4), by_val = list(c("bb", "g"), c(NA,3L,66L)),dups = "error")
# # # .lookupValueMatchingLogicalVec(x = L, by_par = c("bn", "dtyp2"), by_val = list(c("bb", "g"), c(NA,3,66)),dups = "error")
# #
# # # by_val is list - error
# # .lookupValueMatchingLogicalVec(x = Df, by_par = c(2,5), by_val = list(c("bb", "g"), c(NA,TRUE)),dups = "error")
# # # .lookupValueMatchingLogicalVec(x = M, by_par = c(2,5), by_val = list(c("bb", "g"), c(NA,TRUE)),dups = "error")
# # # .lookupValueMatchingLogicalVec(x = L, by_par = c(2,5), by_val = list(c("bb", "g"), c(NA,TRUE)),dups = "error")
# # # .lookupValueMatchingLogicalVec(x = L, by_par = c("bn", "dtyp3"), by_val = list(c("bb", "g"), c(NA,TRUE)),dups = "error")
# #
# # # no match - warn
# # .lookupValueMatchingLogicalVec(x = Df, by_par = "rx", by_val = "abc",dups = "error")
# #
# # # regex match
# # .lookupValueMatchingLogicalVec(x = Df, by_par = "rx", by_val = "abc",dups = "error", regex = T)
# #
# # # regex match
# # .lookupValueMatchingLogicalVec(x = Df, by_par = "rx", by_val = c("abc", "cc"),dups = "warn", regex = T)
# # # multiple regex match - warn
# # .lookupValueMatchingLogicalVec(x = Df, by_par = "rx", by_val = c("abc", "d"),dups = "warn", regex = T, dups_strict = F)
# # # multiple regex match - strict == FALSE, ok FALSE FALSE  TRUE  TRUE FALSE FALSE FALSE
# # .lookupValueMatchingLogicalVec(x = Df, by_par = "rx", by_val = c("abc", "d"),dups = "warn", regex = T, dups_strict = T)
# # # multiple match - msg
# # .lookupValueMatchingLogicalVec(x = Df, by_par = "bn", by_val = "bb", dups = "msg", regex = F)
# # # no match - warn
# # .lookupValueMatchingLogicalVec(x = Df, by_par = "bn", by_val = "bbb", dups = "msg", regex = F)
# # # multiple match - msg
# # .lookupValueMatchingLogicalVec(x = Df, by_par = "bn", by_val = c("bb", "d"), dups = "msg", regex = F)
# # # multiple match - tofalse
# # .lookupValueMatchingLogicalVec(x = Df, by_par = "bn", by_val = "bb", dups = "tofalse",
# #                                 dups_strict = TRUE, verbose = F)
# # # multiple match - msg
# # .lookupValueMatchingLogicalVec(x = Df, by_par = "bn", by_val = "bb", dups = "msg",
# #                                 dups_strict = FALSE, verbose = T)
# # # multiple match - msg
# # .lookupValueMatchingLogicalVec(x = Df, by_par = "bn", by_val = "bb", dups = "msg",
# #                                 dups_strict = TRUE, verbose = T)
# # # multiple match - msg
# # .lookupValueMatchingLogicalVec(x = Df, by_par = "bn", by_val = "bb", dups = "msg",
# #                                 dups_strict = FALSE, verbose = T)
# #
# 
# message("Finished .lookupValueMatchingLogicalVec tests!")
# 
# # # .checkAndHandleBoolVec(x = Df, c("srcpar", "bn"), sel = c(F,T,T,F,F,F,F),dups = "msg",dups_strict = F)
# # # .checkAndHandleBoolVec(x = Df, c("srcpar", "bn"), sel = c(F,T,T,F,F,F,F),dups = "msg",dups_strict = T)
# # # .checkAndHandleBoolVec(x = Df, c("srcpar", "bn"), sel = c(F,T,T,F,F,F,F),dups = "tofalse",dups_strict = T)
# # # .checkAndHandleBoolVec(x = Df, c("srcpar", "bn"), sel = c(F,T,T,F,F,F,F),dups = "first",dups_strict = F)
# # # .checkAndHandleBoolVec(x = Df, c("srcpar", "bn"), sel = c(F,F,F,F,F,F,F),dups = "first",dups_strict = F)
# #
# # # mean(replicate(100, system.time(.lookupValueMatchingLogicalVec(x = Df,
# # #                                                    by_par = "bn",
# # #                                                    by_val = c("bb", "g"),
# # #                                                    dups = "warn")
# # # )["elapsed"]), trim=0.05) # 0.0008555556;  0.0007555556
# # #
# # # mean(replicate(100, system.time(.lookupValueMatchingLogicalVec(x = Df,
# # #                                                                 by_par = "bn",
# # #                                                                 by_val = c("bb", "g"),
# # #                                                                 dups = "warn")
# # # )["elapsed"]), trim=0.05) # 0.005166667; 0.0008555556
# # #
# # # mean(replicate(100, system.time(.lookupValueMatchingLogicalVec(x = Df,
# # #                                                                 by_par = "bn",
# # #                                                                 by_val = c("bb", "g"),
# # #                                                                 dups = "warn",
# # #                                                                 verbose = FALSE)
# # # )["elapsed"]), trim=0.05) # 0.0006888889; 0.0007333333
# # #
# # # mean(replicate(100, system.time(.lookupValueMatchingLogicalVec(x = Df,
# # #                                                                 by_par = "bn",
# # #                                                                 by_val = c("bb", "g"),
# # #                                                                 dups = "warn",
# # #                                                                 call_chks = FALSE
# # #                                                                 #verbose = FALSE
# # #                                                                 )
# # # )["elapsed"]), trim=0.05) # 0.0001777778; 0.0001666667
# #
# #

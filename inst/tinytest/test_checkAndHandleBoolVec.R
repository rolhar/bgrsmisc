# # DEVEL # #
# Just run this testfile in devel process:
# devtools::load_all()
# t.out_1 <- tinytest::run_test_file(file.path(".", "inst", "tinytest", "test_checkAndHandleBoolVec.R"), verbose = 3L)
# t.out_1
# # # # # # #

#load(file.path(".", "inst", "tinytest", "sysdata.rda", package = "bgrsmisc"))
#load(file.path(".", "inst", "tinytest", "testdata.rda"))
load("testdata.rda")

# EXAMPLE DATA MOVED TO SYSDATA
# # assignment Pair / matrix / df
# # no NULL allowd!
# #bn <- c("a", "bb", "bb", "d")
# #bn <- c("a", "bb", "bb", "d", NA, "", "g") ## 
# bn <- c("a", "bb", "bb", "d", "e", "f", "g") ## 
# #dtyp <- c("n", "cl", "cl", "o")
# dtyp <- c("n", "cl", NA, "", "o", "cl", "n")
# dtyp2 <- c(1, NA, 3, NA, 5, 6, 66)
# dtyp3 <- c(rep(TRUE, 4), rep(FALSE, 3))
# #srcpar <- c("I1", "I2", "I3", "I4")
# srcpar <- c("I1", "I2", "I3", "I4", "I5", "I6", "I7")
# bncol <- "bn"
# dtypcol <- "dtyp"
# dtypcol2 <- "dtyp2"
# dtypcol3 <- "dtyp3"
# inpcol <- "srcpar"
# # P <- Pair(bn,dtyp)
# # row.names(P) <- srcpar;P
# # colnames(P) <- c(bncol,dtypcol);P
# M <- matrix(c(srcpar,bn,dtyp, dtyp2, dtyp3), ncol = 5);M
# row.names(M) <- srcpar;M
# colnames(M) <- c(inpcol,bncol,dtypcol, dtypcol2, dtypcol3);M
# Df <- data.frame(srcpar,bn,dtyp,dtyp2,dtyp3,
#                 row.names = srcpar,
#                 stringsAsFactors = F);Df
# L <- list(srcpar, bn, dtyp, dtyp2, dtyp3)
# names(L) <- c(inpcol,bncol,dtypcol,dtypcol2,dtypcol3)
# 
# #P;
# M;Df;L

# ###
# .checkAndHandleBoolVec
# ###

message("Starting .checkAndHandleBoolVec tests ...")

# # Produces failing test
# expect_identical(.checkAndHandleBoolVec(Df, by_par = "bn", sel = c(T,rep(F,nrow(Df)-1)), dups = "error"),
#                  rep(F,nrow(Df)))

# # just df method yet implemented
# # compare results between Df, M and L
# dupvec <- c("allow", "warn", "tofalse")
# for (i in seq_along(dupvec)) {
#   expect_identical(.checkAndHandleBoolVec(Df, by_par = "bn", sel = rep(T,nrow(Df)), dups = dupvec[i]),
#                    .checkAndHandleBoolVec(M, by_par = "bn", sel = rep(T,nrow(M)), dups = dupvec[i]))
#   expect_identical(.checkAndHandleBoolVec(Df, by_par = "bn", sel = rep(T,nrow(Df)), dups = dupvec[i]),
#                    .checkAndHandleBoolVec(L, by_par = "bn", sel = rep(T,length(L[[1]])), dups = dupvec[i]))
# }

# # ?
# expect_identical(.checkAndHandleBoolVec(Df, by_par = "bn", sel = rep(T,nrow(Df)), dups = "warn"),
#                  .checkAndHandleBoolVec(M, by_par = "bn", sel = rep(T,nrow(M)), dups = "warn"))
# expect_identical(.checkAndHandleBoolVec(Df, by_par = "bn", sel = rep(T,nrow(Df)), dups = "warn"),
#                  .checkAndHandleBoolVec(L, by_par = "bn", sel = rep(T,length(L[[1]])), dups = "warn"))

# compare result values ...
expect_identical(.checkAndHandleBoolVec(Df, by_par = "bn", sel = rep(T,nrow(Df)), dups = "allow"),
                 rep(T,nrow(Df)))
# expect_identical(.checkAndHandleBoolVec(Df, by_par = "bn", sel = rep(T,nrow(Df)), dups = "warn"),
#                  rep(T,nrow(Df)))
# expect_identical(.checkAndHandleBoolVec(Df, by_par = "bn", sel = rep(T,nrow(Df)), dups = "tofalse"),
#                  rep(F,nrow(Df)))

expect_identical(.checkAndHandleBoolVec(x = Df, by_par = "dtyp", sel = c(F,F,F,T,F,F,F),
                                        no_match = "warn", dups = "warn", dups_strict = TRUE,
                                        verbose = TRUE),
                 c(F,F,F,T,F,F,F))

expect_identical(.checkAndHandleBoolVec(x = Df, by_par = "dtyp", sel = c(F,F,T,F,F,F,F),
                                        no_match = "warn", dups = "warn", dups_strict = TRUE,
                                        verbose = TRUE),
                 c(F,F,T,F,F,F,F))

expect_identical(.checkAndHandleBoolVec(x = Df, by_par = "dtyp", sel = c(F,F,F,F,F,F,F),
                                        no_match = "allow", dups = "warn", dups_strict = TRUE,
                                        verbose = TRUE),
                 c(F,F,F,F,F,F,F))

expect_identical(.checkAndHandleBoolVec(x = Df, by_par = "dtyp", sel = c(F,T,F,F,F,T,F),
                                        no_match = "warn", dups = "allow", dups_strict = TRUE,
                                        verbose = TRUE),
                 c(F,T,F,F,F,T,F))
                 
expect_identical(.checkAndHandleBoolVec(x = Df, by_par = c("bn","dtyp3"), sel = c(F,T,T,F,F,F,F),
                                        no_match = "warn", dups = "allow", dups_strict = TRUE,
                                        verbose = TRUE),
                 c(F,T,T,F,F,F,F))

expect_identical(.checkAndHandleBoolVec(x = Df, by_par = c("bn","dtyp3"), sel = c(F,F,T,T,F,F,F),
                                        no_match = "warn", dups = "allow", dups_strict = TRUE,
                                        verbose = TRUE),
                 c(F,F,T,T,F,F,F))


# # produces warning - TODO disable
# #
# expect_identical(.checkAndHandleBoolVec(x = Df, by_par = c("bn","dtyp3"), sel = c(F,T,T,F,F,F,F),
#                                         no_match = "warn", dups = "warn", dups_strict = TRUE,
#                                         verbose = TRUE),
#                  c(F,T,T,F,F,F,F))
# expect_identical(.checkAndHandleBoolVec(x = Df, by_par = c("bn","dtyp3"), sel = c(F,F,T,T,F,F,F),
#                                         no_match = "warn", dups = "warn", dups_strict = FALSE,
#                                         verbose = TRUE),
#                  c(F,F,T,T,F,F,F))
# expect_identical(.checkAndHandleBoolVec(x = Df, by_par = "dtyp", sel = c(F,F,F,F,F,F,F),
#                                         #by_val = c("a"),
#                                         no_match = "warn", dups = "warn", dups_strict = TRUE,
#                                         verbose = TRUE),
#                  c(F,F,F,F,F,F,F))
# expect_identical(.checkAndHandleBoolVec(x = Df, by_par = "dtyp", sel = c(F,F,F,F,F,F,F),
#                                         by_val = c("a"),
#                                         no_match = "warn", dups = "warn", dups_strict = TRUE,
#                                         verbose = TRUE),
#                  c(F,F,F,F,F,F,F))
# expect_identical(.checkAndHandleBoolVec(x = Df, by_par = "dtyp", sel = c(F,F,F,F,F,F,F),
#                                         by_val = c("a", "b", "c"),
#                                         no_match = "warn", dups = "warn", dups_strict = TRUE,
#                                         verbose = TRUE),
#                  c(F,F,F,F,F,F,F))


# No dups possible ...
# Arg dups is ignored ...!!! --> good approach??? 
expect_identical(.checkAndHandleBoolVec(Df, by_par = "bn", sel = rep(F,nrow(Df)), 
                                        no_match = "allow", 
                                        dups = "error"),
                 rep(F,nrow(Df)))
expect_identical(.checkAndHandleBoolVec(Df, by_par = "bn", sel = c(T,rep(F,nrow(Df)-1)), dups = "error"),
                 c(T,rep(F,nrow(Df)-1)))

# Check warnings / messages / errors
# dups = 'warn'
expect_warning(.checkAndHandleBoolVec(Df, by_par = "bn", sel = rep(T,nrow(Df)), dups = "warn"))
#expect_warning(.checkAndHandleBoolVec(M, by_par = "bn", sel = rep(T,nrow(M)), dups = "warn"))
#expect_warning(.checkAndHandleBoolVec(L, by_par = "bn", sel = rep(T,length(L[[1]])), dups = "warn"))

# dups = 'tofalse'
expect_message(.checkAndHandleBoolVec(Df, by_par = "bn", sel = rep(T,nrow(Df)), dups = "tofalse"))
# #expect_warning(.checkAndHandleBoolVec(M, by_par = "bn", sel = rep(T,nrow(M)), dups = "tofalse"))
# #expect_warning(.checkAndHandleBoolVec(L, by_par = "bn", sel = rep(T,length(L[[1]])), dups = "tofalse"))

# # currently 'turned of' by methods code, 'allow' will never be matched in switch call ... 
# # dups = 'allow'
# expect_message(.checkAndHandleBoolVec(Df, by_par = "bn", sel = rep(T,nrow(Df)), dups = "allow"))
# # #expect_message(.checkAndHandleBoolVec(M, by_par = "bn", sel = rep(T,nrow(M)), dups = "allow", chk_def = T))
# # #expect_message(.checkAndHandleBoolVec(L, by_par = "bn", sel = rep(T,length(L[[1]])), dups = "allow", chk_def = T))

expect_warning(.checkAndHandleBoolVec(Df, by_par = "bn", sel = rep(F,nrow(Df)), 
                                        no_match = "warn"))
expect_error(.checkAndHandleBoolVec(Df, by_par = "bn", sel = rep(F,nrow(Df)), 
                                      no_match = "error"))
# # DEPRECTED # dups = NULL
# expect_message(.checkAndHandleBoolVec(Df, by_par = "bn", sel = rep(T,nrow(Df)), dups = NULL))
# expect_message(.checkAndHandleBoolVec(M, by_par = "bn", sel = rep(T,nrow(M)), dups = NULL))
# expect_message(.checkAndHandleBoolVec(L, by_par = "bn", sel = rep(T,length(L[[1]])), dups = NULL))

# dups = 'error'
expect_error(.checkAndHandleBoolVec(Df, by_par = "bn", sel = rep(T,nrow(Df)), dups = "error"))
#expect_error(.checkAndHandleBoolVec(M, by_par = "bn", sel = rep(T,nrow(M)), dups = "error"))
#expect_error(.checkAndHandleBoolVec(L, by_par = "bn", sel = rep(T,length(L[[1]])), dups = "error"))

# dups = inadequat value
expect_error(.checkAndHandleBoolVec(Df, by_par = "bn", sel = rep(T,nrow(Df)), dups = "any"))
#expect_error(.checkAndHandleBoolVec(M, by_par = "bn", sel = rep(T,nrow(M)), dups = "any"))
#expect_error(.checkAndHandleBoolVec(L, by_par = "bn", sel = rep(T,length(L[[1]])), dups = "any"))

message("Finished .checkAndHandleBoolVec tests ...")
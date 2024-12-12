# Prepare sysdata.rda
#
# This script to prepare data as well as input data stored in data-raw
#
#
# # # #
# Read KA6_Tbl_B4_KW_H2O_Bindung table ----
# #
#
#KA6_Tbl_B4_KW_H2O_B <- read.table(system.file("data-raw", "KA6_Tbl-B4_KWE_LF.txt", package = "bgrsqr"), header = TRUE, sep = "\t", dec = ",")
KA6_Tbl_B4_KW_H2O_B <- read.table(file.path(".", "data-raw", "KA6_Tbl-B4_KWE_LF.txt"), header = TRUE, sep = "\t", dec = ",")

# 
#
# # # #
# Example toy data ----
# #
# assignment Pair / Matrix / Df / List
# no NULL allowd!
# data vectors
bn <- c("a", "bb", "bb", "d", "e", "f", "g") ##
dtyp <- c("n", "cl", NA, "", "o", "cl", "n")
dtyp2 <- c(1, NA, 3, NA, 5, 6, 66)
dtyp3 <- c(rep(TRUE, 4), rep(FALSE, 3))
srcpar <- c("I1", "I2", "I3", "I4", "I5", "I6", "I7")
rx <- c("a$", "b$", "c$", "d$", "e$", "e$", "f$") ##
ordrd <- c(1,2,1,2,1,2,1)
cats <- c("a", "a", "b", "b", "c", "c", "d")
sparse <- c(NA, NA, NA, NA, 71, 72, NA)
# names
bncol <- "bn"
dtypcol <- "dtyp"
dtypcol2 <- "dtyp2"
dtypcol3 <- "dtyp3"
inpcol <- "srcpar"
rxcol <- "rx"
ordrdcol <- "ordrd"
catscol <- "cats"
sparsecol <- "sparse"
# Pairs obj
# P <- Pair(bn,dtyp)
# row.names(P) <- srcpar;P
# colnames(P) <- c(bncol,dtypcol);P
# Matrix obj
M <- matrix(c(srcpar,bn,dtyp, dtyp2, dtyp3, rx, ordrd, cats, sparse), ncol = 9)
row.names(M) <- srcpar
colnames(M) <- c(inpcol,bncol,dtypcol, dtypcol2, dtypcol3, rxcol, ordrdcol, catscol, sparsecol)
# data.frame obj
Df <- data.frame(srcpar,bn,dtyp,dtyp2,dtyp3,rx,ordrd,cats,sparse,
                 row.names = srcpar,
                 stringsAsFactors = F);Df
# List obj
L <- list(srcpar,bn,dtyp,dtyp2,dtyp3,rx,ordrd,cats,sparse)
names(L) <- c(inpcol,bncol,dtypcol, dtypcol2, dtypcol3, rxcol, ordrdcol, catscol, sparsecol)


# #
# Save sysdata.rda
# #
# 
# List all object to be added to sysdata,rda
# e.g. 
# save(VKR_8_3_T1, KA6_Tbl_B4_KW_H2O_B, file = file.path(".", "R", "sysdata.rda"))

#save(Df, L, M, KA6_Tbl_B4_KW_H2O_B, file = file.path(".", "R", "sysdata.rda"))
save(Df, L, M, KA6_Tbl_B4_KW_H2O_B, 
     file = file.path(".", "inst", "tinytest", "testdata.rda"))
# 
# 

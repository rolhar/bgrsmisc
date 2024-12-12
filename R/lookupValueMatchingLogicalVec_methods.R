# .lookupValueMatchingLogicalVec ####
# returns: logical vector

# HISTORY:
# 20240904: comments added

# Based on calcSubsetBoolVec (initial approach)
# DIFFS
# - just calcs one bool vec and hence allows ..
# - (+) separated checks and handling of single and combined bool vectors
# - (+) different / more handling options (dups = "msg" | "first" added), and
# - (+) feedback / handling of no_match cases as well (.switchNonMatchungActions)
# - (+) supports matches by regex in reference table or in by_val values (yet little tested)
# - (+) switch to turn off tests at all - deactivated again
# - (+) report 'multi'(ple) (but not duplicated) matching cases
# - (+) report values involved in multiple/duplicated matches with verbose = TRUE
# - (+)  .. and little speed up with verbose = FALSE
# - (-) currently just df-method

# TODO
# '-

# DONE
# call_chks deactivared! Just do it by no_match and dups ..!?!

# DEPRECATED
#source("20240224_checkAndHandleBoolVec_methods.R")

#calcSubsetBoolVec alternative names ... (!?!)
#.lookupBoolSingleValMatchingVec
#.lookupBoolVecOfAtomicValMatching
#.lookupBoolVecOfAtomicValMatches
#.lookupLogicalValueMatchingVec
#.lookupAtomicValueMatchingLogicalVec
#.lookupAtomicValMatchesLogicalVec
#.lookupValueMatchingLogicalVec
# # dups == 'allow', 'warn', 'break', 'error', NULL

## at by_par
##   \code{identical(length(by_par), 1L) && by_par %in% colnames(x)}. # This currently produces an Error! TODO
##   
##   

# former: .lookupBoolSingleValMatchingVec

## Generic .lookupValueMatchingLogicalVec ####

#' .lookupValueMatchingLogicalVec
#'
#' Calculates the logical vector corresponding to matches of given values in a
#' vector like object.
#'
#' @param x table-like object, currently supported: \code{data.frame}
#' @param by_par character, atomic vector with 
#'   \code{identical}(\code{length}(\code{by_par}), \code{1L}) 
#'   && \code{by_par} %in% \code{names}(\code{x}). 
#'   The \code{x}\code{[[}\code{by_par}\code{]]} values will be checked for 
#'   matches with value(s) defined by argument \code{by_val}
#' @param by_val atomic vector, value(s) were checked for matches with 
#'   \code{x[[by_par]]}; if \code{fixed} = \code{FALSE} then 
#'   \code{length}(\code{by_val}) == 1L is required
#' @param no_match character, indicating the handling of non-matching cases,
#'   supported are 'allow' (no checks), 'msg', 'warn' and 'error'
#' @param dups character, indicating the handling of multiple incl. 
#'   duplicated (dups_strict = FALSE) or only duplicated matches (dups_strict = TRUE). 
#'   Supported are 'allow' dups (no checks), throw a 'msg', 'warn' or 'error' 
#'   in case of dups as well as setting the whole matching vector 'tofalse' 
#'   or just select the 'first' match.
#' @param dups_strict logical, should the handling of 'duplicated' matches also be 
#'   applied to multiple (FALSE) or just to strictly duplicated (TRUE, default) matches
#' @param fixed logical, default TRUE, if FALSE, the argument defined by argument
#'   \code{regex_pattern} will be treated as regular expression; regex checks will
#'   be performed by \code{grepl} with default arguments except argument \code{fixed}
#' @param regex_pattern character, name of argument whose values were treated as 
#'   regular expressions; defined for \code{by_par} and \code{by_val}, defualt is by_par
#'   With 'by_val' one could explicitly define custom regex to be matched! With 'by_par'
#'   regex are expected to stored in \code{x}.
#' @param is.na_regex_ret atomic, default FALSE, return value if \code{regex_pattern} 
#'   value is missing / NA, \code{pattern} = \code{NA} defaults to return \code{NA} 
#'   in \code{grepl}.
#' @param verbose logical, should the output contain detailed information about affected values (default)
#' @param ... further arguments for methods
#'
#' @return logical vector of length identical to length(\code{x[[by_par]]})
#' @rdname lookupValueMatchingLogicalVec
#' @export
.lookupValueMatchingLogicalVec <- function(x, by_par, by_val, 
                                            #call_chks = TRUE, # switch to turn on (T) /off (F) possible checks
                                            no_match = "warn", 
                                            dups = "warn", 
                                            dups_strict = TRUE, # if FALSE, dups is applied to multiple machtes as well
                                           fixed = TRUE, 
                                           regex_pattern = "by_par", # by_par is regexpr to be applied for checking
                                            is.na_regex_ret = FALSE, # return value if regex_pattern is missing / NA, pattern = NA defaults to NA in grepl
                                            verbose = TRUE, # more detailed output of checkAndHandelBoolVec
                                            ...) {
  
  #stopifnot(!any(missing(by_par), missing(by_val)))
  # not needed, checked anyway due to method definition ...
  
  #stopifnot(is.character(by_par))
  # ? needed, or better allow e.g. integer as well which supports subset as well
  #stopifnot(is.character(by_par) || is.numeric(by_par) )
  # ? or not needed due to df methods ..!?!?
  
  # added 20230702
  stopifnot(is.atomic(by_par))
  
  # just supports character, numeric (resp. integer), NA, boolean and lists 
  # for arg by_val to be used for subsetting ...
  # stopifnot(is.character(by_val) || is.numeric(by_val) 
  #           || is.list(by_val) || is.na(by_val))
  #stopifnot(is.atomic(by_val) || is.list(by_val))
  stopifnot(is.atomic(by_val))
  
  UseMethod(".lookupValueMatchingLogicalVec")
  
} # END of method definition



## df-method .lookupValueMatchingLogicalVec ####

#' @rdname lookupValueMatchingLogicalVec
#' @export
.lookupValueMatchingLogicalVec.data.frame <- function(x,  # soil data input table
                                          by_par, # atomic, parameter for selection, colnames
                                          by_val, # atomic value(s) for selection
                                          #call_chks = TRUE, # FALSE disables checks at all and hence args no_match, dups_strict and dups are ignored
                                         no_match = "warn",
                                          dups = "warn",
                                         dups_strict = TRUE,
                                         fixed = TRUE,
                                         regex_pattern = "by_par", # by_par is regexpr to be applied for checking
                                         is.na_regex_ret = FALSE, # return value if regex_pattern is missing / NA, pattern = NA defaults to NA in grepl
                                         verbose = TRUE,
                                         ...) { 
 
  # For debuging
  #cat("matched_none_FLAG:", matched_none_FLAG, "\n")
  
  # #
  # # perform recursive call of fct ...
  # if (is.list(by_val) && (by_val_list_length <- length(by_val)) > 1L) {
  #   
  #   # # ... just copied up to now
  #   
  #   # sanity check on length of by_par and by_val-list
  #   stopifnot(identical(length(by_par), by_val_list_length))
  #   
  #   # recursive call returns list of boolean vec
  #   l <- lapply(seq_len(by_val_list_length),
  #               function(y) {
  #                 .lookupValueMatchingLogicalVec(x = x, by_par = by_par[y], by_val = by_val[[y]], 
  #                                    #matched_none_FLAG = matched_none_FLAG, 
  #                                    dups = "allow")
  #               })
  #   
  #   # create bool vec indicating if 'all tuple' of l are TRUE - all first list elements, all second and so on ...
  #   sel <- apply(matrix(unlist(l, use.names = FALSE),
  #                       ncol=by_val_list_length),
  #                1, all)
  #   
  #   # sanity check of sel ...
  #   stopifnot(identical(length(sel), nrow(x)))
  #   #return(sel) # return further down, first check about dups
  #   
  # # by_val is not a list with length > 1  
  # } else { # clac on single param / column
    
  # by_val is a list, but list has length == 1
  # # --> unlist to vec
  # if (is.list(by_val)) by_val <- unlist(by_val, recursive = FALSE, use.names = FALSE) 
  
  # sanity check; # (Not really needed,) Error in any case - ok!
  if (length(by_par) > 1L) {
    warning(".lookupValueMatchingLogicalVec: Argument by_par has length > 1. ",
         #"but argument by_val supports just one column as by_par value ...!")
         " Just first element of by_par will be used!")
    by_par <- by_par[1]
    # stop(".lookupValueMatchingLogicalVec: Argument by_par has length > 1, ",
    #      #"but argument by_val supports just one column as by_par value ...!")
    #      " - not supported!")
  }
  
  stopifnot(by_par %in% names(x))
  
  #sel <- x[ , by_par] %in% by_val
  if (!fixed) {
    # still nearly untested ...!
    
    if (identical(regex_pattern, "by_par")) {
      # regex_pattern is by_par
      sel <- sapply(x[[by_par]],
                    function(rx) {
                      if (is.na(rx)) is.na_regex_ret else { # pattern = NA returns NA; x = NA returns FALSE
                        # if (verbose) {
                        #   print("pattern = ")
                        #   print(rx)
                        #   print("x = ")
                        #   print(by_val)
                        # }
                        any(grepl(pattern = rx, x = by_val, # ignore.case = FALSE, 
                                  fixed = fixed))
                      }
                    }, USE.NAMES = FALSE)
    } else if (identical(regex_pattern, "by_val")) {
      if (length(by_val) > 1L) { 
        stop("With fixed = FALSE, length(by_val) > 1 is not supported!")
      } 
      sel <- grepl(pattern = by_val, x = x[[by_par]], fixed = fixed)
    } else {
      stop("Argument regex_pattern is not defined correctly!")
    }
    
  } else {
    sel <- x[[by_par]] %in% by_val # also dt obj returns a vector
  }
  
  
  # # final sanity check ... could be deleted possibly
  # stopifnot(identical(length(sel), nrow(x)))
  
  # Just useful to call function to check and handle no match cases or multiple matches
  # if ...
  # x) DEACTIVATED: call_chks == TRUE AND
  # a) any of these cases is not specified as 'allow'ed AND
  # b) and the count of TRUE in the boolean vector is different than 1.
  if #(call_chks &&
    (!identical(no_match, "allow") || !identical(dups, "allow")
    #) 
    ) {  # a)
  
    # extra if to avoid sum(sel) if no tests should be performed due to (efficiency)
    if (!identical(sum(sel), 1L)) { # b)
      
      sel <- .checkAndHandleBoolVec(x = x, by_par = by_par, sel = sel, 
                                    by_val = by_val,
                                    no_match = no_match, 
                                    dups = dups, 
                                    dups_strict = dups_strict,
                                    verbose = verbose)
      
    }
  } 
  
  return(sel)
  
  # END #
  
  # selsum <- sum(sel)
  # 
  # # sum(sel) != 1 ???
  # if (!identical(selsum, 1L)) {
  #   
  #   if (identical(selsum, 0L)) {
  #     
  #     # warn about completely mismatching
  #     message(".lookupValueMatchingLogicalVec: No matches detected on column(s) ",
  #             paste0(by_par, collapse = " | "),"!")
  #     
  #     
  #     # here call .switchNonMatchungActions as alternative
  #     # ...
  #   } else if (selsum > 1L) {
  #     
  #     # ...
  #     
  #   } else {
  #     stop(".lookupValueMatchingLogicalVec: Unexpected selsum value - Error ...!")
  #   }
  #   
  # }
  
} # END of data.frame method

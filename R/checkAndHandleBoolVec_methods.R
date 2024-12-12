# #############################-
# .checkAndHandleBoolVec and internal helpers ####
# #############################-
#
# #
# TODO
# O Clean and comment (20240312)
#
# #
# QUESTIONS
# ??? Switch to 'allow' is never be called by .checkAndHandleBoolVec!?! Should 'allow' produce a message???
#
# #
# SUMMARY
# .checkAndHandleBoolVec - S3 generic and methods
# incl. .switch<Xy>Actions helpers
#
# #
# DESCRIPTION:
# Checks values from vector / column "by_par" 
# selected by boolean / logical vector "sel"
# for duplicated selected values and / or
# for any selection / any TRUE value in "sel" at all
# and applies actions on "sel" defined by args 
# no_match, dups and dups_strict.
#
# Returns logical vector of length length(sel)
#
# #
# HISTORY
# started as 20240224_checkAndHandleBoolVec_methods.R
# ---
# * [Currently just df method implemented (and likely matrix fits as well)!S]
# ---
# * [Not Needed!?!: alternative without x and by_par ...? --> ??]
#

#########################################-


# #############################-
# .switch<Xy>Actions internal helpers ####
# #############################-

# helper function to distinguish between actions about dups handling indicated 
# by arg dups ...
# helper is identical for all dispatches resp. 
# independent of input / x (df or matrix)

#' .switchDuplActions
#'
#' Helper function to distinguish between actions about handling of multiple / duplicated matches
#'   indicated by argument \code{dups}.
#'   
#' @param x data.frame
#' @param sel logical vector with identical(lenght(sel), nrow(x))
#' @param by_par character representing colname(x), on which argument \code{sel} represents the matching
#' @param dups character, indicating the handling of multiple (dups_strict==F) / duplicated matches,
#'   supported are 'allow' (no checks), 'msg', 'warn', 'tofalse', 'first' and 'error'
#' @param dups_strict logical, should the handling be applied to multiple (FALSE) or duplicated (TRUE, default) matches
#' @param verbose logical, should the output contain detailed information about affected values (default)
#'
#' @return logical vector with length of length(sel), identical to sel or modified in case of 'tofalse' and 'first'
#' @rdname switchDuplActions
#' @keywords internal
.switchDuplActions <- function(x, sel, by_par, 
                               dups, dups_strict = TRUE,
                               verbose = TRUE) {
  
  # Prepare verbose output corresponding to parameters affected by 
  # multiple / duplicated matches
  if(verbose){
    
    # Handling data.frame and matrix objects
    if (identical(class(x), "data.frame") || identical(class(x), "matrix")) {
      # Get subset of obj x
      x_sel <- x[sel, by_par]
      # Switch output corresponding to length of by_par
      if(identical(length(by_par), 1L)) { # x_sel is vector
        verbose_txt <- paste0("\nIn parameter(s) '", by_par,
                              #paste0(by_par, collapse = " | "),
                              "' ", if (dups_strict) "duplicated" else "multiple",
                              " matches detected with values: ",
                              paste0(x_sel, collapse = " | "))
      } else { # x_sel is df / tbl
        verbose_txt <- paste0("\nIn parameter(s) '", paste0(by_par, collapse = " | "),
                              "' ", if (dups_strict) "duplicated" else "multiple", 
                              " matches detected with values:\n", # paste0(x_sel, collapse = "\n")
                              paste0(utils::capture.output(x_sel), collapse = "\n") )
        # use paste0(capture.output(df), collapse = "\n") likely better
      }
      
    # Objects of other classes not yet supported
    } else {
      stop(".switchDuplActions: class '", class(x), "' not yet supported!")
    }
    
  # Info about verbose option ...  
  } else {
    verbose_txt <- paste0(" For affected values in '", by_par, "' set 'verbose = TRUE'.")
  }
  
  # Switch reporting and handling corresponding to argument 'dups'
  switch(dups, # allow (no checks), msg, warn, tofalse, first, error
         # 'allow' is currently 'turned of' by .checkAndHandleBoolVec Code
         # allow currently (20240607) is never be called direktly by .checkAndHandleBoolVec
         allow = {
           message(".checkAndHandleBoolVec: Multiple or duplicated matches detected but 'allow'ed!")
           return(sel)
         },
         msg = {
           if (dups_strict) {
             message(".checkAndHandleBoolVec: Duplicated matches detected!", verbose_txt)
           } else {
             message(".checkAndHandleBoolVec: Multiple matches detected!", verbose_txt)
           }
           return(sel)
         },
         warn = {
           if (dups_strict) {
             warning(".checkAndHandleBoolVec: Duplicated matches detected!", verbose_txt)
           } else {
             warning(".checkAndHandleBoolVec: Multiple matches detected!", verbose_txt)
           }
           return(sel)
         },
         tofalse = {
           sel <- rep(FALSE, length(sel))
           if (dups_strict) {
             message(".checkAndHandleBoolVec: Duplicated matches detected and argument dups\n", 
                     "is defined as 'tofalse'. Hence the whole boolean vector is set to FALSE!",
                     verbose_txt)
           } else {
             message(".checkAndHandleBoolVec: Multiple matches detected and argument dups\n", 
                     "is defined as 'tofalse'. Hence the whole boolean vector is set to FALSE!",
                     verbose_txt)
           }
           return(sel)
         }, 
         first = {
           f <- which(sel)[1]
           sel <- rep(FALSE, length(sel))
           sel[f] <- TRUE
           if (dups_strict) {
             message(".checkAndHandleBoolVec: Duplicated matches detected and argument dups\n",
                     "is defined as 'first'. Hence all but the first matches are set to FALSE!",
                     verbose_txt)
           } else {
             message(".checkAndHandleBoolVec: Multiple matches detected and argument dups\n",
                     "is defined as 'first'. Hence all but the first matches are set to FALSE!",
                     verbose_txt)
           }
           return(sel)
         },
         error = {
           if (dups_strict) {
             stop(".checkAndHandleBoolVec: Error due to not accepted duplicated matches!", 
                  verbose_txt)
           } else {
             stop(".checkAndHandleBoolVec: Error due to not accepted multiple matches!", 
                  verbose_txt)
           }
         },
         stop(".checkAndHandleBoolVec: Argument 'dups' not specified correctly!")
  )
}

#### hh ####
# titke

#### tt ####

# helper function to distinguish between actions about 
# ...non matching handling indicated by arg no_match ...
# helper is identical for all dispatches resp. independent of input / x (df, matrix)

#' .switchNonMatchungActions
#' 
#' Helper function to distinguish between actions about handling of totally non-matching cases
#'   indicated by argument \code{no_match}.
#' @param sel logical FALSE vector with identical(lenght(sel), nrow(x))
#' @param by_par character representing colname(x), on which argument \code{sel} represents the matching
#' @param no_match character, indicating the handling of non-matching cases,
#'   supported are 'allow' (no checks), 'msg', 'warn' and 'error'
#' @param by_val atomic vector, optional, values which have been checked for matches,
#'   just for reporting in case of non-matching cases
#'
#' @return logical vector \code{sel}
#' @rdname switchNonMatchungActions
#' @keywords internal
.switchNonMatchungActions <- function(sel, by_par, no_match, by_val = NULL) {
  switch(no_match, # allow (no checks), msg, warn, error
         # allow currently (20240607) is never be called direktly by .checkAndHandleBoolVec
         allow = {
           message(".checkAndHandleBoolVec: No match(es)",
                   if(!is.null(by_val)) paste0(" for value(s) '", 
                                               paste0(by_val, collapse = " | "), "'"),
                   " detected but 'allow'ed!")
           return(sel)
         },
         msg = {
           message(".checkAndHandleBoolVec: No match(es)",
                   if(!is.null(by_val)) paste0(" for value(s) '", 
                                               paste0(by_val, collapse = " | "), "'"),
                   " in column(s) '", paste0(by_par, collapse = " | "), "' detected!")
           return(sel)
         },
         warn = {
           warning(".checkAndHandleBoolVec: No match(es)",
                   if(!is.null(by_val)) paste0(" for value(s) '", 
                                               paste0(by_val, collapse = " | "), "'"),
                   " in column(s) '", paste0(by_par, collapse = " | "), "' detected!")
           return(sel)
         },
         # tofalse = {
         #   sel <- rep(FALSE, length(sel))
         #   warning("Argument dups was set as 'tofalse' and ",
         #           "hence due to detected duplicates in column(s) '", 
         #           paste0(by_par, collapse = " | "), "' the whole boolean vector is set to FALSE!")
         #   return(sel)
         # },
         # first = {
         #   f <- which(sel)[1]
         #   sel <- rep(FALSE, length(sel))
         #   sel[f] <- TRUE
         #   warning("Argument dups was set as 'tofalse' and ",
         #           "hence due to detected duplicates in column(s) '", 
         #           paste0(by_par, collapse = " | "), "' the whole boolean vector is set to FALSE!")
         #   return(sel)
         # },
         error = {
           stop(".checkAndHandleBoolVec: Error due to not accepted missing match(es) ", 
                if(!is.null(by_val)) paste0(" for value(s) '", 
                                            paste0(by_val, collapse = " | "), "'"),
                "in selected data in column(s) '", paste0(by_par, collapse = " | "), "'!")
         },
         stop(".checkAndHandleBoolVec: Argument 'no_match' not specified correctly!")
  )
}

# #############################


# METHOD - generic

#' .checkAndHandleBoolVec
#' 
#' Wrapper to call '.swtichActions' functions
#' 
#' @param x table-like object, currently supported \code{data.frame}
#' @param by_par character representing colname(x), on which argument \code{sel} represents the matching
#' @param sel logical vector with identical(lenght(sel), nrow(x))
#' @param by_val atomic vector, optional, values which have been checked for matches,
#'   just for reporting in case of non-matching cases
#' @param no_match character, indicating the handling of non-matching cases,
#'   supported are 'allow' (no checks), 'msg', 'warn' and 'error'
#' @param dups character, indicating the handling of multiple (dups_strict==F) / duplicated matches,
#'   supported are 'allow' (no checks), 'msg', 'warn', 'tofalse', 'first' and 'error'
#' @param dups_strict logical, should the handling be applied to multiple (FALSE) or duplicated (TRUE, default) matches
#' @param verbose logical, should the output contain detailed information about affected values (default)
#' @param ... further arguments for methods
#'
#' @return logical vector with length of length(sel), identical to sel or modified in case of dups = 'tofalse' or 'first'
#' @rdname checkAndHandleBoolVec
#' @export
.checkAndHandleBoolVec <- function(x, by_par, sel, 
                                   by_val = NULL,
                                   no_match = "warn",
                                   dups = "warn", 
                                   dups_strict = TRUE,
                                   verbose = TRUE,
                                   ...) { 
  
  #stopifnot(!any(missing(by_par), missing(sel)))
  #not needed, checked anyway due to method definition ...
  
  #stopifnot(is.character(by_par))
  # ? needed, or better allow e.g. integer as well which supports subset as well
  #stopifnot(is.character(by_par) || is.numeric(by_par) )
  # or not needed due to df methods ..!?!?
  
  UseMethod(".checkAndHandleBoolVec")
}

# #############################


# .checkAndHandleBoolVec data.frame method

#' @rdname checkAndHandleBoolVec
#' @export
.checkAndHandleBoolVec.data.frame <- function(x, by_par, sel, 
                                              by_val = NULL,
                                              no_match = "warn",
                                              dups = "warn",
                                              dups_strict = TRUE, #,
                                              verbose = TRUE,
                                              ...) { 
  
  # sanity check  
  stopifnot(identical(length(sel), nrow(x)))
  
  #selsum <- sum(sel)
  
  if (!identical((selsum <- sum(sel)), 1L)) {
    
    if (identical(selsum, 0L) && !identical(no_match, "allow")) {
      
      # reporting about completely mismatching
      sel <- .switchNonMatchungActions(sel = sel, by_par = by_par, 
                                       no_match = no_match,
                                       by_val = by_val)
      
    } else if (selsum > 1L && !identical(dups, "allow"))  {
      
      # check for real duplicated matches ...
      # if (length (by_val) > 1) multiple legal matches are possible
      if (dups_strict && anyDuplicated(x[sel, by_par])) { # more efficient than any(dup...())
        
        sel <- .switchDuplActions(x, sel = sel, by_par = by_par, 
                                  dups = dups, dups_strict = dups_strict,
                                  verbose = verbose)
        
      } else if (!dups_strict) { # if added 20240301
        
        sel <- .switchDuplActions(x, sel = sel, by_par = by_par, 
                                  dups = dups, dups_strict = dups_strict,
                                  verbose = verbose)
        
      } # else {
        # # This is called on strict == TRUE but no duplicated but possibly multiple matches ...?? TODO
        # stop("Bug in .checkAndHandleBoolVec.data.frame method. 
        #      This pieceof code should never be called. Please report!")
      # }
      
    }      
    #else { # also called if dups / no_match == "allow" --> Problem
    #   stop("Unexpected Error")
    # }
    
  }
  
  # return
  return(sel)
  
  # # OLD
  # 
  # if (identical(dups, "allow") && !chk_def) {
  #   return(sel)
  #   
  # } else {
  #   
  #   
  #   
  #   # no matches / TRUEs documented in sel ...
  #   if ((selsum <- sum(sel)) == 0L) { # selsum == 0
  #     message(".checkForDupsAndHandleBoolVec: No matches detected!")
  #     return(sel)
  #     
  #     # just one match / TRUE in sel ... no dups possible!
  #   } else if (selsum == 1L) { # selsum == 1
  #     return(sel) # sum(sel) == 1
  #     
  #     # more than one match / TRUEs in sel ...  
  #   } else {
  #     
  #     # if duplicates present
  #     #if (any(duplicated(x[sel, by_par]))) {
  #     if (!anyDuplicated(x[sel, by_par]) > 0L) { # more efficient than any(dup...())
  #       # no dups
  #       return(sel)
  #       
  #     } else {
  #       
  #       return(.switchDuplAction(sel = sel, by_par = by_par, dups = dups))
  #       
  #     } 
  #   }
  # }
  
}
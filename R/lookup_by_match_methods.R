# # # # #
# lookup_by_match (Title) ----
# # # # #

# returns: looked up values, optionally simplified, optionally with names, ...
# possibly different formats, default ... ToDo

# HISTORY
# 20241128: optimising no_match_retVal with coercion (and warning / msgs)
#           respecting type of par_oi and lookup_next_if comparison
#           extended with isTRUE(all.equal(...))
# 20241105: lookup_next_where finished, but whole lookup_next still in progress
# 20240904: comments added

# Old name .lookupValuesByParamMatch

# # #
# Devel
#devtools::load_all()

# ###
# TODO
# ###
# # ! Documentation / rd-files: lookup_by_match and lookupValueMatchin... together?!?
# - duplicated title, args, details etc. (!?) need to be homogenized ..!?!
# # ! clean lookup_next
# # ? How are no_match, dups, dups_strict vs. no_match_final, ... handeld 
#    in case of length(by_par) == 1 ? Currently by overwriting no_match etc. like no_match = no_match_final
# # ! clean roxygen tags
# # ?(optional) lookup_next: add counter for warning to indicate that / if index has moved already (in case of  max > 1)
# # (check again postprocessing)
# # (What's better default: dups_strict_final = TRUE OR FALSE ???
#     - > if input is/are single atomic value(s) and table has just 'unique' rows 
#     -- > then any dups in result not possible ...
#     If input is multiple: then nonidentical dups are possible
#     If table has not only unique rows: then identical dups are possible
#     -- > likely FALSE is favorite!!!
# # #
# DONE
# # #
# # General
#   * res value could be integer(0) if unique(sel) = FALSE / !any(sel) / no matches of by_val in by_par = > managed by no_match_retVal
#   * msg in case of no_match_retValue
#   * option return type identical to class(x) / input
# # Postprocessing
#   * respect if identical(length(res), 0L) || identical(nrow(res), 0L)
#     = > no names possible ...
#   * add arg add_names_paroi
#   * What if type / class of no_match_retVal does not fit par_oi class???
#     -- > Explicit coercion of no_match_retVal to typeof par_oi, and messages
#           if verbose and warnings if par_oi is of type logical or
#           par_oi is of type integer and and no_match_retVal of type double.
# # lookup_next
#   * support more than one step ...!
#   * just recall main-function if new_byval_value is different from curr_byval_value
#   * What if "next" has no match? -- > lookup_next_if is returned [as] by no_match_retVal
#       --> Compare: no_match_retVal = lookup_next_if, # !!! ??? -- > returns original value !
#   * lookup_next_if comparison with res extended with isTRUE(all.equal(...))
# # ###

# # # #
# Details
# !!!
# * following args are extended in case of length == 1 and length(by_par) > 1:
#     lookup_next, no_match, dups, dups_strict, fixed, regex_pattern, is.na_regex_ret


# # #
# # exp data
# bn <- c("a", "bb", "bb", "d", "e", "f", "g") ##
# dtyp <- c("n", "cl", NA, "", "o", "cl", "n")
# dtyp2 <- c(1, NA, 3, NA, 5, 6, 66)
# dtyp3 <- c(rep(TRUE, 4), rep(FALSE, 3))
# srcpar <- c("I1", "I2", "I3", "I4", "I5", "I6", "I7")
# rx <- c("a$", "b$", "c$", "d$", "e$", "f$", "g$") ##
# ordrd <- c(1,2,1,2,1,2,1)
# cats <- c("a", "a", "b", "b", "c", "c", "d")
# sparse <- c(NA, NA, NA, NA, 71, 72, NA)
# Df <- data.frame(srcpar,bn,dtyp,dtyp2,dtyp3,rx,ordrd,cats,sparse,
#                 row.names = srcpar,
#                 stringsAsFactors = F);Df

# ###





# # # # 
## Generic lookup_by_match ####
# # # # 
 
# 25 args incl. '...'

#' lookup_by_match
#'
#' TODO (Calculates the logical vector corresponding to matches of given values in a
#' vector like object.)
#'
#' @param x table-like object, currently supported: \code{data.frame}
#' @param par_oi character, atomic vector of column names of \code{x} indicating
#'   from which column(s) the resulting values should be looked up. 
#'   The parameter of interest!
#' @param by_par character, atomic vector with 
#'   \code{identical}(\code{length}(\code{by_par}), \code{1L}) 
#'   && \code{by_par} %in% \code{names}(\code{x}). 
#'   The \code{x}\code{[[}\code{by_par}\code{]]} values will be checked for 
#'   matches with value(s) defined by argument \code{by_val}
#' @param by_val atomic vector, value(s) were checked for matches with \code{x[[by_par]]};
#'   if \code{fixed} = \code{FALSE} then \code{length}(\code{by_val}) == 1L is required
#' @param lookup_next list with logical values or logical atomic vector with 
#'   identical(length(lookup_next), length(by_par)) in case of list ... or ... , default FALSE, TODO
#' @param lookup_next_if single atomic values, indicating the return value for which 
#'   lookup_next should applied, default is NA
#' @param lookup_next_where character, indicating where should be looked for another value,
#'   possible strings: "center" (default), "edges", "back", "fwd"
#' @param lookup_next_max numeric, default is 1L, maximal number of calls for looking up a next value
#' @param no_match_retVal atomic default NA, value with is returned n case of no matches of by_val; 
#'   NULL returns (list of) vector(s) of length zero
#' @param no_match_final character, default "warn" todo
#' @param dups_final character, default "warn"todo
#' @param dups_strict_final logical, default FALSE, indicating if only real
#'   duplicated result values should be treated as 'dups' or if multiple result
#'   values should be tretaed as 'dups' as well (default). 
#'   FALSE ensures an output of length one / one row resp. list elements of length one.
#' @param simplify logical, default TRUE, ToDo
#' @param ret_inputClass logical, if FALSE, a list is returned, if TRUE an object od class(x), 
#'   argument is ignored if simplify = TRUE leads to a simplified return value
#' @param add_bypar logical, default FALSE, Todo
#' @param add_names_byval logical, default FALSE, TODO
#' @param add_names_paroi logical, default FALSE, indicating if (name of) par_oi should
#'   be added as name(s) to simplified vector results, ignored if add_names_byval = TRUE
#' @param no_match character, indicating the handling of non-matching cases,
#'   supported are 'allow' (no checks), 'msg', 'warn' and 'error'
#' @param dups character, indicating the handling of multiple or duplicated (dups_strict = FALSE) 
#'   or only duplicated matches (dups_strict = TRUE), supported are 'allow' (no checks), 
#'   'msg', 'warn', 'error' as well as 'tofalse' and 'first' 
#'   which sets the whole vector to FALSE or just selects the 'first' match
#' @param dups_strict logical, should the handling of 'duplicated' matches also be 
#'   applied to multiple (FALSE) or just to strictly duplicated (TRUE, default) matches
#' @param fixed logical, vector of length 1 or identical(lenght(fixed), lenght(by_par),
#'   default TRUE, if FALSE, the corresponding argument defined by argument
#'   \code{regex_pattern} will be treated as regular expression; regex checks will
#'   be performed by \code{grepl} with default arguments except argument \code{fixed}
#' @param regex_pattern character, name of argument whose values were treated as 
#'   regular expressions; defined for \code{by_par} and \code{by_val}, default is by_par
#' @param is.na_regex_ret atomic, default FALSE, return value if values corresponding to 
#'   \code{regex_pattern} are missing / NA, \code{pattern} = \code{NA} defaults to 
#'   return \code{NA} in \code{grepl}.
#' @param verbose logical, should the output contain detailed information about affected values (default)
#' @param ... further arguments for methods
#'
#' @details If lookup_next ends up in an empty result due to no match of by_val in by_par 
#'   the initial result, identical to lookup_next_if, is returned as no_match_retVal.
#' @return logical vector of length identical to \code{nrow}(\code{x})
#' @rdname lookupValueMatchingLogicalVec
#' @export
lookup_by_match <- function(x, # obj from which par_oi values will be selected by matches of by_val values in by_par column(s) / param(s) 
                           par_oi, # atomic vec
                           by_par, # atomic vec
                           by_val, # atomic || list
                           lookup_next = FALSE, # LIST! #list(rep(FALSE, length(by_par))), # just applicable to ordinal by_par data; list of FALSE or ordered vec of values of by_par
                           lookup_next_if = NA,
                           lookup_next_where = "center",
                           lookup_next_max = 1L, # default
                           no_match_retVal = NA,
                           no_match_final = "warn",
                           dups_final = "warn",
                           dups_strict_final = FALSE,
                           simplify = TRUE,
                           ret_inputClass = FALSE,
                           add_bypar = FALSE,
                           add_names_byval = FALSE,
                           add_names_paroi = FALSE,
                           #call_chks = TRUE, # ---
                           no_match = "warn",
                           dups = "allow",
                           dups_strict = TRUE,
                           fixed = TRUE, # rep(TRUE, length(by_par)), # boolean vec of length length(by_par)
                           regex_pattern = "by_par", # by_par is regexpr to be applied for checking
                           is.na_regex_ret = FALSE, # return value if regex is missing / NA, NA is default by grepl as well
                           verbose = TRUE, # more detailed output of checkAndHandelBoolVec
                           ...) {
  
  #stopifnot(!any(missing(by_par), missing(by_val)))
  # not needed, checked anyway due to method definition ...
  
  # just supports character, numeric (resp. integer), NA, boolean and lists 
  # for arg by_val to be used for subsetting ...
  # stopifnot(is.character(by_val) || is.numeric(by_val) 
  #           || is.list(by_val) || is.na(by_val))
  stopifnot(is.atomic(by_val) || is.list(by_val)) # not strict, should be ...
  #stopifnot(is.list(by_val)) # but not yet for testing ...!
  stopifnot(is.atomic(par_oi))
  stopifnot(is.atomic(by_par))
  
  # needed?
  stopifnot(!is.null(par_oi) && !is.null(by_par) && !is.null(by_val))
  # should not be NULL but you never know ...!?!
  
  stopifnot(identical(length(lookup_next), 1L) || identical(length(lookup_next), length(by_par)))
  stopifnot(identical(length(fixed), 1L) || identical(length(fixed), length(by_par)))
  
  UseMethod("lookup_by_match")
}


# # Test data - needs example data from above ..!

# x = Df
# par_oi = "dtyp2"
# by_par = c("ordrd", "cats")
# by_val = list(2, "b")
# lookup_next = list(FALSE, c("a", "b", "c", "d"))
# lookup_next_if = NA
# lookup_next_where = "edges"
# lookup_next_max = 1L # default
# no_match_retVal = NA
# no_match_final = "warn"
# dups_final = "warn"
# dups_strict_final = FALSE
# simplify = TRUE
# ret_inputClass = FALSE
# add_bypar = FALSE
# add_names_byval = FALSE
# add_names_paroi = FALSE
# #call_chks = TRUE
# no_match = "warn"
# dups = "allow"
# dups_strict = TRUE
# fixed = TRUE
# regex_pattern = "by_par"
# is.na_regex_ret = FALSE
# verbose = TRUE


# x = Df # obj from which par_oi values will be selected by matches of by_val values in by_par column(s) / param(s)
# par_oi = c("sparse")#,#"rx"), # atomic vec
# by_par = c("ordrd")#,# "cats"), # atomic vec
# by_val = list(2)#,# "e"), # atomic || list

## df-METHOD lookup_by_match ####

#' @rdname lookupValueMatchingLogicalVec
#' @export
lookup_by_match.data.frame <- function(x, par_oi, by_par, by_val, 
                            lookup_next = FALSE, # LIST! #list(rep(FALSE, length(by_par))), # just applicable to ordinal by_par data; list of FALSE or ordered vec of values of by_par 
                            lookup_next_if = NA,
                            lookup_next_where = "center",
                            lookup_next_max = 1L, # default
                            no_match_retVal = NA, # with add_bypar also bypar gets no_match_retVal
                            no_match_final = "warn",
                            dups_final = "warn",
                            dups_strict_final = FALSE,
                            simplify = TRUE, 
                            ret_inputClass = FALSE,
                            add_bypar = FALSE, 
                            add_names_byval = FALSE,
                            add_names_paroi = FALSE,
                                                 #call_chks = TRUE, # rep(TRUE, length(by_par)) 
                                                 no_match = "warn", #rep("warn", length(by_par)),
                                                 dups = "allow", #rep("allow", length(by_par)),
                                                 dups_strict = TRUE, # rep(TRUE, length(by_par)) 
                                                 fixed = TRUE, # rep(FALSE, length(by_par)), # boolean vec of length length(by_par); by_par is regexpr to be applied for checking
                            regex_pattern = "by_par",
                                                 is.na_regex_ret = FALSE, # return value if regex is missing / NA, NA is default by grepl as well
                                                 verbose = TRUE,
                            ...) { #, # more detailed output of checkAndHandelBoolVec
  
  # # # # # # #
  ## CALC selection vec for multiple or single by_par / params for lookup ####  
  # # # # # # #
  
  ### !! Multiple by_par / params for lookup ####  
  
  if (is.list(by_val) && (by_val_list_length <- length(by_val)) > 1L) {
    #if (islist <- is.list(by_val) && (by_val_list_length <- length(by_val)) > 1L) {# had problems ...!
  
    
    
    #if (length(by_par) > 1 || is.list(by_val)) {
    # message("calcSubsetBoolVec for class ", class(x), ": length(by_par) > 1 || is.list(by_val)\n",
    #         "Not yet implemented!")
    
    # sanity check on length of by_par and by_val-list
    stopifnot(identical(length(by_par), by_val_list_length))
    
    # need of extending args
    #if(identical(length(lookup_next), 1L)) lookup_next <- list(rep(lookup_next, length(by_par)))
    if(identical(length(lookup_next), 1L)) lookup_next <- as.list(rep(lookup_next, length(by_par)))
    #if(identical(length(call_chks), 1L)) call_chks <- rep(call_chks, length(by_par)) 
    if(identical(length(no_match), 1L)) no_match <- rep(no_match, length(by_par))
    if(identical(length(dups), 1L)) dups <- rep(dups, length(by_par))
    if(identical(length(dups_strict), 1L)) dups_strict <- rep(dups_strict, length(by_par)) 
    if(identical(length(fixed), 1L)) fixed <- rep(fixed, length(by_par))
    if(identical(length(regex_pattern), 1L)) regex_pattern <- rep(regex_pattern, length(by_par))
    if(identical(length(is.na_regex_ret), 1L)) is.na_regex_ret <- rep(is.na_regex_ret, length(by_par))
    
    #### lapply .lookupValueMatchingLogicalVec ####
    
    # call .lookupValueMatchingLogicalVec multiple times which returns list of boolean vec ...
    # no_match returns ... FALSE
    l <- lapply(seq_len(by_val_list_length),
                function(lfnr_bypar) {
                  .lookupValueMatchingLogicalVec(x = x, 
                                                  by_par = by_par[lfnr_bypar], 
                                                  by_val = by_val[[lfnr_bypar]],
                                                  #call_chks = call_chks[lfnr_bypar],
                                                  no_match = no_match[lfnr_bypar], # default no_match = "warn",  
                                                  dups = dups[lfnr_bypar], # default
                                                  dups_strict = dups_strict[lfnr_bypar], # if FALSE, dups is applied to multiple machtes as well
                                                  fixed = fixed[lfnr_bypar],
                                                 regex_pattern = regex_pattern[lfnr_bypar],
                                                  is.na_regex_ret = is.na_regex_ret[lfnr_bypar],
                                                  verbose = verbose) # default
                                                  # completed
                })
    
    
    # create bool vec indicating if 'a row tuple' / all values of XXX / TODO of l are TRUE 
    # -- >  Are all first list elements TRUE?, Are all second are TRUE? And so on ...
    sel <- apply(matrix(unlist(l, use.names = FALSE),
                        ncol=by_val_list_length),
                 1, all)
    
    # sanity check of sel ...
    stopifnot(identical(length(sel), nrow(x)))
    #return(sel) # return further down, first check about dups

    # ###
    # check sel by selsum, .checkAndHandleBoolVec etc. ...
    #
    # just check if any 'handling-rule' is != 'allow'
    if (!identical(no_match_final, "allow") || !identical(dups_final, "allow")) {
      
      # extra if to avoid sum(sel) if no tests should be performed due to (efficiency)
      if (!identical(sum(sel), 1L)) {
        
        sel <- .checkAndHandleBoolVec(x = x, 
                                       by_par = by_par, 
                                      by_val = by_val,
                                       sel= sel, 
                                       no_match = no_match_final,
                                       dups = dups_final, 
                                       dups_strict = dups_strict_final,
                                       verbose = verbose)
      }
    }
    
    # ???
    # where do this???
    #lookup_next = lookup_next[[lfnr_bypar]],
  #??? TODO
    
    
  
      
  
    # END: # Multiple by_par / params for lookup
    # calc'ed: sel
  
  
  # FOLLOWING:    
  # One single by_par / param for lookup
  # by_val is not a list with length > 1  
  # !(islist <- is.list(by_val) && (by_val_list_length <- length(by_val)) > 1L)    
  } else { 
    
    ### !! Just single by_par / params for lookup ####  
    
    # by_val is a list, but list has length == 1
    # --> unlist to vec
    if (is.list(by_val)) by_val <- unlist(by_val, recursive = FALSE, use.names = FALSE) 
  
    #### .lookupValueMatchingLogicalVec ####
    
    #sel <- .calcSubsetBoolVec(x = x, by_par = by_par, by_val = by_val, dups = dups)
    sel <- .lookupValueMatchingLogicalVec(x = x, 
                                           by_par = by_par, 
                                           by_val = by_val,
                                           #call_chks = TRUE,
                                           no_match = no_match_final, # default no_match = "warn",  
                                           dups = dups_final, # default
                                           dups_strict = dups_strict_final, # if FALSE, dups is applied to multiple machtes as well
                                           fixed = fixed,
                                          regex_pattern = regex_pattern,
                                           is.na_regex_ret = is.na_regex_ret,
                                           verbose = verbose) # default
  } 
  # END: # One single by_par / param for lookup
  # calc'ed: sel
  
  # # # # # # #
  ## APPLY logical selection vector ####
  # # # # # # #
  
  # # #
  # # First / old approach
  # # looked up value by sel = > result
  # res <- x[sel, if (add_bypar) c(by_par, par_oi) else par_oi]
  # # #
  #
  # # New approach - already respecting simplify here
  # if (!simplify) { 
  #   # forced not to coerce the result to the lowest possible dimension
  #   res <- x[sel, if (add_bypar) c(by_par, par_oi) else par_oi, 
  #            drop = simplify]
  # } else { 
  #   # result coerced to the lowest possible dimension if just one column is left
  #   # Even if this is currently the default, the arg is specified explicitly
  #   res <- x[sel, if (add_bypar) c(by_par, par_oi) else par_oi, 
  #            drop = simplify] # added 20241105
  # }
  # # -- > So if statement is obsolet ...
  # !!! Changed 20241105
  res <- x[sel, if (add_bypar) c(by_par, par_oi) else par_oi, 
           drop = simplify]
  # # # #

  # # # # # # #
  ## POSTPROC (1) general  ... ####
  # # # # # # #
  
  # = > integer etc (0) || df with zero rows || atomic (value || NA) || data.frame
  # = > This could be integer(0) / zero row df if !any(sel) == TRUE
  
  
  #
  # LOOKUP_NEXT (just) for atomic result == lookup_next_if
  #
  # lookup_next currently just supports one step of looking up for next value ...
  # but possibly in both directions
  
  # print("res")
  # print(res)
  # print("lookup_next")
  # print(lookup_next)
  # print("is.na(res)")
  # print(is.na(res))
  # print("any(!lookup_next %in% FALSE)")
  # print(any(!lookup_next %in% FALSE))
  # 
  
  # TODO / Approach
  # ??? (optional) lookup next procedure if res == NA = > thats lookup_next !?!
  # Check and perform lookup_next procedure ...
  
  ## Arguments
  # lookup_next character or logical: activate / do lookup next procedure: TRUE, FALSE, or vector = unique(<selected-by_par-param-values>) (default) / ordered vector of values of selected by_par param
  # lookup_next_if trigger-value; in case of which resulting / returned values lookup_next should be performed: e.g. NA
  #     = > lookup_next_fits_on -- > is.na(res)
  # lookup_next_where which direction to look for: center, edges, up/back/left, down/fwd/right
  # lookup_next_max number of steps to perform maximally; "step" could be derived by decreasing max
  # lookup_next_step ??
  # check identical(length(which(!lookup_next %in% FALSE)), 1L)
  # msg in case of lookup next
  
  # if 
  # a) result is found by match, 
  # b) is NA resp. result is identical to lookup_next_if, and 
  # c) in lookup_next values are provided for (one) by_par
  # tmp
  # lookup_next_if <- NA
  # lookup_next_where <- "edges"
  # lookup_next_max <- 2 # default
  
  
  # print("res")
  # print(res)
  
  # If result could be identical to NA ... because it has length == 1
  if (identical(length(res), 1L)) {
    # Need to (indirectly) unclass a NA-result, to possibly match with lookup_next_if = NA
    if (is.na(res)) res <- NA 
  }
  
  
  
  # Used 
  # a) as flag indicating that lookup_next should be tried ...
  # b) as helper indicating if recalling lookup_by_match fct is worth to be done 
  #   (new_byval_value != curr_byval_Value)
  new_byval_value <- NULL
  
  
  # # # # # # #
  ## LOOKUP next preparation or postprocessing ... ####
  # # # # # # #
  
  # is.atomic added 20240619
  # check if conditions for lookup_next are given ...
  if (is.atomic(res) && identical(length(res), 1L) && 
      # identical(res, lookup_next_if) && 
      # Does this should be more specific respecting types ..!? -- >
      (identical(res, lookup_next_if) || 
       isTRUE(all.equal(res, lookup_next_if,
                        check.attributes = FALSE, 
                        check.class = FALSE,
                        use.names = FALSE))) && 
      # -- > More flexible than just a call of identical (e.g. double vs integer)
      any(!lookup_next %in% FALSE) &&
      lookup_next_max > 0L) { 
    # length(res) > 0L # needed?
    # if(is.na(res) && any(!sapply(lookup_next, function(ln) ln %in% FALSE))) {
    # 
    
    #### lookupo_next conditions matched - prep and check if it is worth to recall main fct ! ----
    
    if (verbose) {
      message("Conditions for lookup_next matched with initial by_val as '",
              paste0(by_val, collapse = " | "), "' for by_par as '",  
              paste0(by_par, collapse = " | "),
              "' and (not desired) looked up value lookup_next_if as '",
              lookup_next_if, "'! Preparation starts ...")
    }
    
    # # 
    ### lookup next further prep ... ####
    # #
    
    # determine by_par index for lookup_next
    idx_bypar <- which(!lookup_next %in% FALSE)
    
    # samity checks
    # lookup_next just on one col / by_par allowed
    # stopifnot(length(which(lookup_next)), 1L)
    if(length(idx_bypar) > 1) stop("lookup_next just supports one element e defined by
                             argument lookup_next with !identical(e, FALSE).")
    
    # lookup_next just on by_val of length 1 allowed
    if(length(by_val[[idx_bypar]]) > 1) stop("lookup_next just supports single atomic
                                       corresponding by_val values!")
    
    # print("by_val")
    # print(by_val)
    
    # get by_par ordered reference vector
    if (isTRUE(lookup_next[[idx_bypar]])) {
      by_par_ordRefVec <- unique(x[[by_par[idx_bypar]]])
      if (verbose) message("lookup_next by_par_ordRefVec: ", 
                           paste0(by_par_ordRefVec, collapse = " | "))
    } else {
      stopifnot(all(lookup_next[[idx_bypar]] %in% x[[by_par[idx_bypar]]]))
      by_par_ordRefVec <- lookup_next[[idx_bypar]]
    }
    
    
    # adjust / shorten lookup_next_max 'counter'
    # Avoids infinity loop on trying to lookup_next result values
    # on e.g. lookup_next_where = center!
    # !!! lookup_next_max ...
    # must not be greater than length(by_par_ordRefVec) - 1 !!!
    if (lookup_next_max > (length(by_par_ordRefVec) - 1)) {
      lookup_next_max <- length(by_par_ordRefVec) - 1
      if (verbose) {
        message("lookup_next_max reduced to 'length(by_par_ordRefVec) - 1'.")
      }
    }
    
    # get current by_val value
    curr_byval_value <- by_val[[idx_bypar]]
    # tmp sanity
    stopifnot(curr_byval_value %in% by_par_ordRefVec)
    
    # #idx_byval_refV <- which(curr_byval_value %in% lookup_next[[idx_bypar]])
    # print("lookup_next[[idx_bypar]] %in% curr_byval_value")
    # print(lookup_next[[idx_bypar]] %in% curr_byval_value)
    
    # get index of current by_val value
    idx_byval_refV <- which(by_par_ordRefVec %in% curr_byval_value)
    
    # lookup_next_where ...
    
    # # # moved down:
    # # initialize new by_val vector
    # new_by_val <- by_val
    
    # # #
    #### lockup_next_where-switch ----
    # # #
    # Adjust by_val vector / list by replacing 
    # value, which has result res with identical(res,lookup_next_if), 
    # with new value from refVector, depending on lookup_next_where ...
    #new_by_val[[idx_bypar]] <- switch(
    new_byval_value <- switch(
      lookup_next_where,
      # get value on the left-side of the current value in the reference vector
      "back" = { # added 20241105
        # check if index already on the left edge ...
        # if so, further back is not possible!
        if (identical(idx_byval_refV, 1L)) {
          warning("lookup_next with argument lookup_next_where defined as '", 
                  lookup_next_where, "' not possible! Current by_val value '", 
                  curr_byval_value, "' already matches left / lower edge of ",
                  "the by_par reference vector '", 
                  paste0(by_par_ordRefVec, collapse = " | "), "'!")
          #by_par_ordRefVec[idx_byval_refV] # return unchanged value
          curr_byval_value # return unchanged value
        } else {
          # decrease the index
          by_par_ordRefVec[idx_byval_refV - 1]
        }
      },
      # get value on the right-side of the current value in the reference vector
      "fwd" = { # added 20241105
        # check if index already on the right edge ...
        # if so, further forward is not possible!
        if (identical(idx_byval_refV, length(by_par_ordRefVec))) {
          warning("lookup_next with argument lookup_next_where defined as '", 
                  lookup_next_where, "' not possible! Current by_val value '", 
                  curr_byval_value, "' already matches right / upper edge of ",
                  "the by_par reference vector '", 
                  paste0(by_par_ordRefVec, collapse = " | "), "'!")
          #by_par_ordRefVec[idx_byval_refV] # return unchanged value
          curr_byval_value # return unchanged value
        } else {
          # increase the index and get value
          by_par_ordRefVec[idx_byval_refV + 1]
        }
      }, 
      # get value from the direction to the closer edge in the reference vector
      "edges" = {
        # check if index already on one of the edges
        if (identical(idx_byval_refV, 1L) || 
            identical(idx_byval_refV, length(by_par_ordRefVec))) {
          warning("lookup_next with argument lookup_next_where defined as '", 
                  lookup_next_where, "' not possible! Current by_val value '", 
                  curr_byval_value, "' already matches one of the edges of ",
                  "the by_par reference vector '", 
                  paste0(by_par_ordRefVec, collapse = " | "), "'!")
          #by_par_ordRefVec[idx_byval_refV] # return unchanged value
          curr_byval_value # return unchanged value
        } else {
          # ["ln" refers to ... 'lookup_next'
          # Find center to get direction to move to edge:
          # first get length of reference vector ...
          ln_refVec_length <- length(by_par_ordRefVec)
          # divided by 2 and round up to find ln_mid
          ln_mid <- ceiling(ln_refVec_length/2) 
          # move to edge direction and get new byval value
          if (idx_byval_refV <= ln_mid) { 
            by_par_ordRefVec[idx_byval_refV - 1]
          } else { 
            by_par_ordRefVec[idx_byval_refV + 1]
          }
        }
      },
      # get value from the direction to the center in the reference vector
      "center" = {
        # find center
        ln_refVec_length <- length(by_par_ordRefVec)
        ln_mid <- ceiling(ln_refVec_length/2) # divided by 2 and round up to find ln_mid
        if(idx_byval_refV <= ln_mid){
          by_par_ordRefVec[idx_byval_refV + 1]
        } else {
          by_par_ordRefVec[idx_byval_refV - 1]
        } # Be careful, this is just stopped by lookup_next_max ... if center is reached!!
      },
      stop("Argument lookup_next_where was not defined correctly!")
    )
  
  } # END of checking lookup_next conditions ..
  # -- > is new_byval_value != NULL AND new_byval_value != curr_byval_value ...???
  
  if (!is.null(new_byval_value) # -- > lookup_next: all requirements fit 
      # and everything prepared successfully!
      && !identical(curr_byval_value, new_byval_value)) { # -- > worth to recall main fct!
    
    ### if --> lookup_next call ----
    
    # # initialize new_by_val
    # new_by_val <- by_val
    # if (!identical(curr_byval_value, new_byval_value)) {
    #   new_by_val[[idx_bypar]] <- new_byval_value
    # }
    new_by_val <- by_val
    new_by_val[[idx_bypar]] <- new_byval_value
    #print("new_by_val")
    #print(new_by_val)
    
    # print(idx_byval_refV)
    # print(ln_mid)
    
    if (verbose) {
      message("Method lookup_by_match will be recalled with new by_val as '",
              paste0(new_by_val, collapse = " | "), "' to perform 'lookup_next' ...")
    }
    
    # Done! [] ToDo / XXX ?? !!]
    # ! this is just called if 
    # !identical(curr_byval_value, new_by_val_value)
    
    # recall lookup_by_match
    res <- lookup_by_match(x = x, par_oi = par_oi, by_par = by_par, 
                           by_val = new_by_val, # !!!
                           lookup_next = lookup_next, 
                           lookup_next_if = lookup_next_if,
                           lookup_next_where = lookup_next_where,
                           lookup_next_max = lookup_next_max - 1, # !!!
                           #lookup_next_notEmpty = lookup_next_notEmpty,
                              no_match_retVal = lookup_next_if, # !!! ??? !!! ? return original value !?! right?
                                no_match_final = no_match_final,
                                dups_final = dups_final,
                                dups_strict_final = dups_strict_final,
                                simplify = simplify, 
                           ret_inputClass = ret_inputClass,
                           add_bypar = add_bypar,
                           add_names_byval = add_names_byval,
                           add_names_paroi = add_names_paroi,
                                #call_chks = call_chks, # rep(TRUE, length(by_par)) 
                                no_match = no_match, #rep("warn", length(by_par)),
                                dups = dups, #rep("allow", length(by_par)),
                                dups_strict = dups_strict, # rep(TRUE, length(by_par)) 
                                fixed = fixed, # rep(FALSE, length(by_par)), # boolean vec of length length(by_par); by_par is regexpr to be applied for checking
                           regex_pattern = regex_pattern,
                                is.na_regex_ret = is.na_regex_ret, # return value if regex is missing / NA, NA is default by grepl as well
                                verbose = verbose) #, # more detailed output of checkAndHandelBoolVec
    
    # if (identical(length(res), 0L) && lookup_next_notEmpty) {
    #   res <- lookup_next_if
    # }
    
  # END: lookup next procedure
  # calc'ed: (new) final res - to be returned
  
  } else { # go on with initial res - no lookup_next performed
    
    ### else --> post-processing / no (more) lookup next ... ####
    
    # Set res == no_match_retVal ([wrong:]if lookup of res was not successful)
    # == no res(ult) found in x in case of no_match
    # if result is "empty", e,g, integer(0) or df with 0 rows 
    # (in general case of no_match)
    # set no_match_retVal as result if defined ...
    
    # old apporach - not accepting results with length == 0L, e.g. integer(0)
    # if(identical(length(no_match_retVal), 0L)) stop("Argument no_match_retVal is not correctly defined,
    #                                                 it needs to have length > 0!")
    # etc
    
    # if result integer(0) etc or df with zero rows ...
    if ((identical(length(res), 0L) || identical(nrow(res), 0L))
        && !is.null(no_match_retVal)) {
      
      # Adjust typeof of no_match_retVal if necessary ...
      # ToTest
      #print("Check if no_match_retVal need to be coerced ..")
      #if (verbose) message("Check if no_match_retVal need to be coerced ..")
      if (!is.na(no_match_retVal)) {
        #typeof_par_oi <- typeof(x[[par_oi]])
        #print(typeof_par_oi)
        #if (verbose) message("typeof_par_oi: ", typeof_par_oi)
        if (!identical((typeof_no_match_retVal <- typeof(no_match_retVal)),
                       (typeof_par_oi <- typeof(x[[par_oi]])))) {
          
          if (verbose) message("Defined no_match_retVal '", no_match_retVal,
                               "' of type ", typeof_no_match_retVal, 
                               " will be coerced to ", typeof_par_oi, 
                               " as defined by typeof(x[[par_oi]]).")
          
          # In case of identical(typeof_no_match_retVal, "character") 
          # -- > already warning from asMethod !
          
          if (identical(typeof_par_oi, "logical") # nothing could doubtless be coerced to logical
              || (identical(typeof_no_match_retVal, "double") 
                  && identical(typeof_par_oi, "integer"))) {
            warning("Critical data manipulation or data loss due to coercion ",
                    "of no_match_retVal '", no_match_retVal, "' of type ", 
                    typeof(no_match_retVal), " to ", typeof_par_oi, 
                    " as defined by typeof(x[[par_oi]]).")
          }
            # --> In case of identical(typeof_no_match_retVal, "character") &&
            #   identical(typeof_par_oi, "logical") two warnings will be thrown:
            #   one from asMethod and one particulary defined above!
            
          no_match_retVal <- methods::as(no_match_retVal, typeof_par_oi)
          
        }
      }
      
      if (!is.atomic(res)) { # res is df, etc. ...
        #stopifnot(add_bypar) # formerly coded, why???
        if (add_bypar) res[1, by_par] <- NA
        res[1, par_oi] <- no_match_retVal
      } else { # res is atomic
        res <- no_match_retVal
      }
      
      if (verbose) {
        message("lookup_by_match: No match detected for fields '", paste0(by_par, collapse = " | "),
                "' and values '", paste0(by_val, collapse = " | "),
                "'!\nValue '", no_match_retVal, "' is returned as 'looked up' value for '",
                paste0(par_oi, collapse = " | "), "' because it was used to specify argument 'no_match_retVal'!")
      }
      
    } # END - add no_match_retVal
    
    
    # New structure
    
    # input vec | df
    # add_names_byval, add_names_paroi
    # ret_inputClas
    # !identical(length(res), 0L)
    
    # new 3
    
    if (is.atomic(res) && !identical(length(res), 0L)) {
      if (add_names_byval) {
        if (!identical(length(by_par), 1L) || (is.list(by_val) && length(by_val) > 1L)) {
          warning("Argument add_names_byval was set to / treated as FALSE due to use of multiple parameters for selection ...")
        } else { names(res) <- x[sel, by_par] }
      } else if (add_names_paroi) { names(res) <- par_oi }
    } else if (!is.atomic(res) && !ret_inputClass) {
      # (just) coerce to list
      res <- as.list(res) # (already) without vector elem names
      # add names?
      if (add_names_byval) {
        if (!identical(length(by_par), 1L) || (is.list(by_val) && length(by_val) > 1L)) {
          warning("Argument add_names_byval was set to / treated as FALSE due to use of multiple parameters for selection ...")
        } else {
          # names
          res <- lapply(res, function(y) {
            #names(y) <- x[ , by_par][sel]
            names(y) <- x[sel, by_par]
            y })
        }
      }
    }
    
      # if (identical(nrow(res), 0L) || !add_names_byval) {
      #   # (just) coerce to list
      #   res <- as.list(res) # (already) without vector elem names
      # } else if (add_names_byval) {
      #   if (!identical(length(by_par), 1L) || (is.list(by_val) && length(by_val) > 1L)) {
      #     warning("Argument add_names_byval was set to / treated as FALSE due to use of multiple parameters for selection ...")
      #   } else {
      #     # coerce to list
      #     res <- as.list(res) # (already) without vector elem names
      #     # names
      #     res <- lapply(res, function(y) {
      #       #names(y) <- x[ , by_par][sel]
      #       names(y) <- x[sel, by_par]
      #       y
      #     })
      #   }
      # }
      
    
    # new2:
    
    # if (!add_names_byval) {
    #   if (add_names_paroi && is.atomic(res) && !identical(length(res), 0L)) {
    #     names(res) <- par_oi
    #   } else if (!is.atomic(res) && !ret_inputClass)
    #     # coerce to list
    #     res <- as.list(res) # (already) without vector elem names
    # } else if (#add_names_byval && 
    #            (!identical(length(by_par), 1L) || (is.list(by_val) && length(by_val) > 1L))) {
    #   warning("Argument add_names_byval was set to / treated as FALSE due to use of multiple parameters for selection ...")
    #   #add_names_byval <- FALSE
    #   if (!is.atomic(res) && !ret_inputClass) {
    #     # coerce to list
    #     res <- as.list(res) # (already) without vector elem names
    #   }
    # } else if (#add_names_byval && 
    #            is.atomic(res) && !identical(length(res), 0L)) {
    #   names(res) <- x[sel, by_par]
    # } else if (#add_names_byval && 
    #            !is.atomic(res) && !identical(nrow(res), 0L) 
    #            && !ret_inputClass) {
    #   # coerce to list
    #   res <- as.list(res) # (already) without vector elem names
    #   # names
    #   res <- lapply(res, function(y) {
    #     #names(y) <- x[ , by_par][sel]
    #     names(y) <- x[sel, by_par]
    #     y
    #   })
    # }
      
    
    # new1  
    
    # # add_names_byval possible? Adjustemnt needed?
    # if (add_names_byval && 
    #     (!identical(length(by_par), 1L) || (is.list(by_val) && length(by_val) > 1L))) {
    #     warning("Argument add_names_byval was set to / treated as FALSE due to use of multiple parameters for selection ...")
    #     #add_names_byval <- FALSE
    # }
    # 
    # # Case of simplification - vector
    # if (add_names_byval && is.atomic(res) && !identical(length(res), 0L)) {
    #   names(res) <- x[sel, by_par]
    # } else if (is.atomic(res) && !identical(length(res), 0L)) {
    #   names(res) <- par_oi
    #   
    # # No simplification - currently "data.frame" %in% class(res)
    # } else if (!is.atomic(res) && !ret_inputClass) {
    #   
    #   if (add_names_byval && !identical(nrow(res), 0L)) {
    #     # coerce to list
    #     res <- as.list(res) # (already) without vector elem names
    #     # names
    #     res <- lapply(res, function(y) {
    #       #names(y) <- x[ , by_par][sel]
    #       names(y) <- x[sel, by_par]
    #       y
    #     })
    #   } else {
    #     # coerce to list
    #     res <- as.list(res) # (already) without vector elem names
    #   }
    #   
    # }
    
    # if !atomic && ret_inputClass = > nothing to do further ...
    
    # # OLD
    # # convert df to list
    # #if (is.data.frame(res)) { 
    # if (!is.atomic(res)) {  # may be supports other objects than dfs
    #   res <- as.list(res) # already without vector elem names
    # } 
    # else if (is.atomic(res) && !simplify) { # if res is atomic vector
    #   # coerce to list
    #   res <- list(res)
    #   # and set list name
    #   names(res) <- par_oi
    # }
    # 
    # if (add_names_byval) {
    #   
    #   if (!identical(length(by_par), 1L) || (is.list(by_val) && length(by_val) > 1L)) {
    #     warning("Argument add_names_byval was set to / treated as FALSE due to use of multiple parameters for selection ...")
    #     #add_names_byval <- FALSE
    #   }
    #   else if (is.list(res)) {
    #     res <- lapply(res, function(y) {
    #       #names(y) <- x[ , by_par][sel]
    #       names(y) <- x[sel, by_par]
    #       y
    #     })
    #   }
    #   else {
    #     #names(res) <- x[ , by_par][sel]
    #     names(res) <- x[sel, by_par]
    #   }
    #   
    # }
    # 
    # if (ret_inputClass && is.list(res)) {
    #   # data.frame
    #   res <- as.data.frame(res, row.names = NULL)
    #   #res <- data.frame(res)
    # }
    
  } # END: processing of initial res
  
  ## RETURN result ####
  return(res)

}


# # Test with currently loaded example data ----
# # 1) devtools::load_all()
# # 2) load example data
# # (3) source(file.path(dirname(getwd()), "lookup_by_match_methods.R"))
# # 3 ) run this
# lookup_by_match(x = x, # obj from which par_oi values will be selected by matches of by_val values in by_par column(s) / param(s)
#                 par_oi = par_oi, # atomic vec
#                 by_par = by_par, # atomic vec
#                 by_val = list(2, "c"), #by_val, # atomic || list
#                 lookup_next = lookup_next, # LIST! #list(rep(FALSE, length(by_par))), # just applicable to ordinal by_par data; list of FALSE or ordered vec of values of by_par
#                 lookup_next_where = "fwd",
#                 lookup_next_if = 6,
#                 lookup_next_max = 1,
#                 no_match_retVal = 99,
#                 no_match_final = no_match_final,
#                 dups_final = dups_final,
#                 dups_strict_final = dups_strict_final,
#                 simplify = simplify,
#                 add_bypar = add_bypar,
#                 add_names_byval = add_names_byval,
#                 #call_chks = TRUE,
#                 no_match = no_match,
#                 dups = dups,
#                 dups_strict = dups_strict,
#                 fixed = fixed, # rep(FALSE, length(by_par)), # boolean vec of length length(by_par); by_par is regexpr to be applied for checking
#                 regex_pattern = regex_pattern,
#                 is.na_regex_ret = is.na_regex_ret, # return value if regex is missing / NA, NA is default by grepl as well
#                 verbose = verbose)#, # more detailed output of checkAndHandelBoolVec


# lookup_by_match(x = Df, # obj from which par_oi values will be selected by matches of by_val values in by_par column(s) / param(s)
#                 par_oi = c("rx", "sparse"), # atomic vec
#                 by_par = c("ordrd", "cats"), # atomic vec
#                 by_val = list(1, "c"), # atomic || list
#                             lookup_next = list(FALSE, c("a","b", "c", "d")), # LIST! #list(rep(FALSE, length(by_par))), # just applicable to ordinal by_par data; list of FALSE or ordered vec of values of by_par
#                             no_match_retVal = 99,
#                             no_match_final = "warn",
#                             dups_final = "warn",
#                             dups_strict_final = FALSE,
#                             simplify = TRUE,
#                             add_bypar = FALSE,
#                             add_names_byval = FALSE,
#                             #call_chks = TRUE,
#                             no_match = "warn",
#                             dups = "allow",
#                             dups_strict = TRUE,
#                             fixed = TRUE, # rep(FALSE, length(by_par)), # boolean vec of length length(by_par); by_par is regexpr to be applied for checking
#                 regex_pattern = "by_par",
#                             is.na_regex_ret = FALSE, # return value if regex is missing / NA, NA is default by grepl as well
#                             verbose = TRUE)#, # more detailed output of checkAndHandelBoolVec
# 

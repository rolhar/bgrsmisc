% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lookupValueMatchingLogicalVec_methods.R,
%   R/lookup_by_match_methods.R
\name{.lookupValueMatchingLogicalVec}
\alias{.lookupValueMatchingLogicalVec}
\alias{.lookupValueMatchingLogicalVec.data.frame}
\alias{lookup_by_match}
\alias{lookup_by_match.data.frame}
\title{.lookupValueMatchingLogicalVec}
\usage{
.lookupValueMatchingLogicalVec(
  x,
  by_par,
  by_val,
  no_match = "warn",
  dups = "warn",
  dups_strict = TRUE,
  fixed = TRUE,
  regex_pattern = "by_par",
  is.na_regex_ret = FALSE,
  verbose = TRUE,
  ...
)

\method{.lookupValueMatchingLogicalVec}{data.frame}(
  x,
  by_par,
  by_val,
  no_match = "warn",
  dups = "warn",
  dups_strict = TRUE,
  fixed = TRUE,
  regex_pattern = "by_par",
  is.na_regex_ret = FALSE,
  verbose = TRUE,
  ...
)

lookup_by_match(
  x,
  par_oi,
  by_par,
  by_val,
  lookup_next = FALSE,
  lookup_next_if = NA,
  lookup_next_where = "center",
  lookup_next_max = 1L,
  no_match_retVal = NA,
  no_match_final = "warn",
  dups_final = "warn",
  dups_strict_final = FALSE,
  simplify = TRUE,
  ret_inputClass = FALSE,
  add_bypar = FALSE,
  add_names_byval = FALSE,
  add_names_paroi = FALSE,
  no_match = "warn",
  dups = "allow",
  dups_strict = TRUE,
  fixed = TRUE,
  regex_pattern = "by_par",
  is.na_regex_ret = FALSE,
  verbose = TRUE,
  ...
)

\method{lookup_by_match}{data.frame}(
  x,
  par_oi,
  by_par,
  by_val,
  lookup_next = FALSE,
  lookup_next_if = NA,
  lookup_next_where = "center",
  lookup_next_max = 1L,
  no_match_retVal = NA,
  no_match_final = "warn",
  dups_final = "warn",
  dups_strict_final = FALSE,
  simplify = TRUE,
  ret_inputClass = FALSE,
  add_bypar = FALSE,
  add_names_byval = FALSE,
  add_names_paroi = FALSE,
  no_match = "warn",
  dups = "allow",
  dups_strict = TRUE,
  fixed = TRUE,
  regex_pattern = "by_par",
  is.na_regex_ret = FALSE,
  verbose = TRUE,
  ...
)
}
\arguments{
\item{x}{table-like object, currently supported: \code{data.frame}}

\item{by_par}{character, atomic vector with
\code{identical}(\code{length}(\code{by_par}), \code{1L})
&& \code{by_par} \%in\% \code{names}(\code{x}).
The \code{x}\code{[[}\code{by_par}\code{]]} values will be checked for
matches with value(s) defined by argument \code{by_val}}

\item{by_val}{atomic vector, value(s) were checked for matches with \code{x[[by_par]]};
if \code{fixed} = \code{FALSE} then \code{length}(\code{by_val}) == 1L is required}

\item{no_match}{character, indicating the handling of non-matching cases,
supported are 'allow' (no checks), 'msg', 'warn' and 'error'}

\item{dups}{character, indicating the handling of multiple or duplicated (dups_strict = FALSE)
or only duplicated matches (dups_strict = TRUE), supported are 'allow' (no checks),
'msg', 'warn', 'error' as well as 'tofalse' and 'first'
which sets the whole vector to FALSE or just selects the 'first' match}

\item{dups_strict}{logical, should the handling of 'duplicated' matches also be
applied to multiple (FALSE) or just to strictly duplicated (TRUE, default) matches}

\item{fixed}{logical, vector of length 1 or identical(lenght(fixed), lenght(by_par),
default TRUE, if FALSE, the corresponding argument defined by argument
\code{regex_pattern} will be treated as regular expression; regex checks will
be performed by \code{grepl} with default arguments except argument \code{fixed}}

\item{regex_pattern}{character, name of argument whose values were treated as
regular expressions; defined for \code{by_par} and \code{by_val}, default is by_par}

\item{is.na_regex_ret}{atomic, default FALSE, return value if values corresponding to
\code{regex_pattern} are missing / NA, \code{pattern} = \code{NA} defaults to
return \code{NA} in \code{grepl}.}

\item{verbose}{logical, should the output contain detailed information about affected values (default)}

\item{...}{further arguments for methods}

\item{par_oi}{character, atomic vector of column names of \code{x} indicating
from which column(s) the resulting values should be looked up.
The parameter of interest!}

\item{lookup_next}{list with logical values or logical atomic vector with
identical(length(lookup_next), length(by_par)) in case of list ... or ... , default FALSE, TODO}

\item{lookup_next_if}{single atomic values, indicating the return value for which
lookup_next should applied, default is NA}

\item{lookup_next_where}{character, indicating where should be looked for another value,
possible strings: "center" (default), "edges", "back", "fwd"}

\item{lookup_next_max}{numeric, default is 1L, maximal number of calls for looking up a next value}

\item{no_match_retVal}{atomic default NA, value with is returned n case of no matches of by_val;
NULL returns (list of) vector(s) of length zero}

\item{no_match_final}{character, default "warn" todo}

\item{dups_final}{character, default "warn"todo}

\item{dups_strict_final}{logical, default FALSE, indicating if only real
duplicated result values should be treated as 'dups' or if multiple result
values should be tretaed as 'dups' as well (default).
FALSE ensures an output of length one / one row resp. list elements of length one.}

\item{simplify}{logical, default TRUE, ToDo}

\item{ret_inputClass}{logical, if FALSE, a list is returned, if TRUE an object od class(x),
argument is ignored if simplify = TRUE leads to a simplified return value}

\item{add_bypar}{logical, default FALSE, Todo}

\item{add_names_byval}{logical, default FALSE, TODO}

\item{add_names_paroi}{logical, default FALSE, indicating if (name of) par_oi should
be added as name(s) to simplified vector results, ignored if add_names_byval = TRUE}
}
\value{
logical vector of length identical to length(\code{x[[by_par]]})

logical vector of length identical to \code{nrow}(\code{x})
}
\description{
Calculates the logical vector corresponding to matches of given values in a
vector like object.

TODO (Calculates the logical vector corresponding to matches of given values in a
vector like object.)
}
\details{
If lookup_next ends up in an empty result due to no match of by_val in by_par
the initial result, identical to lookup_next_if, is returned as no_match_retVal.
}

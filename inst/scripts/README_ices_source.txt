BUNDLED ICES ROUTINES - PROVENANCE
==================================

The files R/ices_*.R in this package are NOT the work of the package
author. They are the ICES discard-raising and length/age allocation
routines, distributed by ICES as loose scripts rather than as a package,
and are included here so that rcef installs and runs without a separate
manual step.

Original file names, before the ices_ prefix was applied:

  ices_age_length_alloc.R          <- 0_Functions_age-length_alloc.R
  ices_discards_raising.R          <- 0_Functions_discards_raising.R
  ices_other.R                     <- 0_Functions_other.R
  ices_IntercatchCEF.R             <- funIntercatchCEF.R
  ices_ICoutCEF.R                  <- funICoutCEF.R
  ices_CompareIntercatchCEF.R      <- funCompareIntercatchCEF.R
  ices_MakeRelation.R              <- funMakeRelation.R
  ices_ReadIntercatch.R            <- funReadIntercatch.R
  ices_cef_add_strata.R            <- cef_add_strata.R
  ices_cef_allocate_age_strata.R   <- cef_allocate_age_strata.R
  ices_cef_compute_ref_numbers.R   <- cef_compute_ref_numbers.R
  ices_raise_discards_strata.R     <- raise_discards_strata.R
  ices_split_L_with_without_D.R    <- split_L_with_without_D.R
  ices_strata_coverage_summary.R   <- strata_coverage_summary.R
  ices_discard_raising_functions.R <- discard_raising_functions.R
  ices_sop.R                       <- sop.R
  ices_utils.R                     <- utils.R

TWO EDITS were made when folding them in, both marked in the files:

  1. ices_discard_raising_functions.R opened with a library() block.
     A package must not attach packages as a side effect of loading, so
     the block was commented out and the same packages declared in
     DESCRIPTION. The original lines are kept in the comment.

  2. The same file defined `%||%`, which rcef also defines in fn_utils.R.
     Two definitions of one operator in a single namespace is a collision
     waiting to be blamed on something else, so that copy was removed. The
     definitions were equivalent.

No other line was changed. In particular no algorithm, threshold or
default was touched.

THIS IS A SNAPSHOT. ICES revises these scripts on its own schedule. A
stock raised with a stale copy would be wrong in a way nothing in this
package could detect. Check what you have with:

    rcef::ices_routine_status()

and override with a newer copy using:

    rcef::use_external_ices("path/to/your/ICES/scripts")

BEFORE PUBLISHING THIS PACKAGE, confirm you are permitted to redistribute
these files, and under what terms. The package's own MIT licence covers
the author's code; it does not automatically extend to code written by
somebody else. If redistribution is not permitted, delete R/ices_*.R and
go back to sourcing them at run time - use_external_ices() already does
exactly that.

Also in this folder, and NOT part of the package namespace:
  1.diagnosis_discards.R, 1.diagnosis_discards_WORKED.R  worked examples
  ../rmd/*.Rmd                                           notebooks
These are top-level scripts, not function libraries. They are shipped for
reference and are never sourced by the package.


EDITS MADE FOR R CMD CHECK
==========================

Folding third-party scripts into a package surfaces documentation problems
that never mattered when they were sourced from a folder. These were fixed;
none of them touches code:

  * \codeq{seasonValue} -> \code{seasonValue} in ices_cef_add_strata.R.
    \codeq is not an Rd macro; it was a typo that made the package fail to
    install.
  * @examples blocks wrapped in \dontrun{}. They call the functions with
    objects that exist only in the original author's session, so at check
    time they errored. Wrapped, they stay visible as documentation without
    claiming to run.
  * \link{census_add_strata}, \link{census_remove_strata} and
    \link{convExchange_tidy} became \code{...}. Those functions do not
    exist in this revision, so the links were dead ends for the reader and
    errors for the checker.
  * seven non-ASCII characters (em dashes, one accented e) replaced with
    ASCII equivalents, in comments and two plot titles. R CMD check
    requires ASCII in R code for portability.

KNOWN, ACCEPTED WARNINGS
========================

R CMD check still reports these. They are upstream and were left alone
rather than papered over:

  1. "Undocumented arguments in documentation object ..." for
     cef_add_strata, check_group_conditions, dis_raising, funICoutCEF,
     funIntercatchCEF and split_L_with_without_D. The ICES roxygen
     documents fewer arguments than the functions take. Writing the
     missing entries would mean guessing at somebody else's intent, and a
     wrong description is worse than none.

  2. "'library' or 'require' calls in package code: dplyr, icesSD,
     icesVocab, rlang". Several ICES functions call library() inside their
     own bodies. That runs at CALL time, not load time, so importing rcef
     does not pollute anything - but calling catchRaising() will attach
     dplyr to the user's search path. Harmless in practice; would need
     upstream changes to fix properly.

Both would be resolved by upstream, and both are worth re-checking when a
newer ICES revision is folded in.

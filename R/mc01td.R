#' mc01td
#'
#' Checking stability of a given real polynomial
#' @examples 

#'   To determine whether or not a given polynomial P(x) with real
#'   coefficients is stable, either in the continuous-time or discrete-
#'   time case.
#' 
#'   A polynomial is said to be stable in the continuous-time case
#'   if all its zeros lie in the left half-plane, and stable in the
#'   discrete-time case if all its zeros lie inside the unit circle.
#' 
#' #'
#' @references \url{http://slicot.org/objects/software/shared/doc/MC01TD.html}
#' @export
mc01td <- function(dico, dp, p) {

    # In Parameters
    dico <- as.character(dico)
    dp <- as.integer(dp)

    stable <- as.logical(0)
    nz <- as.integer(0)
    iwarn <- as.integer(0)
    info <- as.integer(0)
    dwork <- array(as.double(1), c(2 * dp + 2))


    res <- suppressWarnings(.Fortran("MC01TD", DICO = dico, DP = dp, P = p, STABLE = stable, NZ = nz, DWORK = dwork, IWARN = iwarn,
        INFO = info))

    return(list(dp = res$DP, stable = res$STABLE, nz = res$NZ, iwarn = res$IWARN, info = res$INFO))
}

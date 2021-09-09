#' ab13ed
#'
#' Estimating the distance from a real matrix to the nearest complex matrix with an eigenvalue on the imaginary axis, using bisection
#' @examples 

#'   To estimate beta(A), the 2-norm distance from a real matrix A to
#'   the nearest complex matrix with an eigenvalue on the imaginary
#'   axis. The estimate is given as
#' 
#'          LOW <= beta(A) <= HIGH,
#' 
#'   where either
#' 
#'          (1 + TOL) * LOW >= HIGH,
#' 
#'   or
#' 
#'          LOW = 0   and   HIGH = delta,
#' 
#'   and delta is a small number approximately equal to the square root
#'   of machine precision times the Frobenius norm (Euclidean norm)
#'   of A. If A is stable in the sense that all eigenvalues of A lie
#'   in the open left half complex plane, then beta(A) is the distance
#'   to the nearest unstable complex matrix, i.e., the complex
#'   stability radius.
#' 
#' #'
#' @references \url{http://slicot.org/objects/software/shared/doc/AB13ED.html}
#' @export
ab13ed <- function(n, a, tol) {

    # In Parameters
    n <- as.integer(n)
    tol <- as.double(tol)

    low <- as.double(0)
    high <- as.double(0)
    info <- as.integer(0)
    ldwork <- as.integer(max(1, 3 * n * (n + 1)))
    dwork <- array(as.double(1), c(ldwork))
    lda <- dim(a)[1]


    res <- suppressWarnings(.Fortran("AB13ED", N = n, A = a, LDA = lda, LOW = low, HIGH = high, TOL = tol, DWORK = dwork, LDWORK = ldwork,
        INFO = info))

    return(list(low = res$LOW, high = res$HIGH, info = res$INFO))
}

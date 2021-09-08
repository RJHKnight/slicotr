#' ab13fd
#'
#' Computing the distance from a real matrix to the nearest complex matrix with an eigenvalue on the imaginary axis, using SVD
#' @examples 

#'   To compute beta(A), the 2-norm distance from a real matrix A to
#'   the nearest complex matrix with an eigenvalue on the imaginary
#'   axis. If A is stable in the sense that all eigenvalues of A lie
#'   in the open left half complex plane, then beta(A) is the complex
#'   stability radius, i.e., the distance to the nearest unstable
#'   complex matrix. The value of beta(A) is the minimum of the
#'   smallest singular value of (A - jwI), taken over all real w.
#'   The value of w corresponding to the minimum is also computed.
#' 
#' #'
#' @references \url{http://slicot.org/objects/software/shared/doc/AB13FD.html}
#' @export
ab13fd <- function(n, a, tol) {

    # In Parameters
    n <- as.integer(n)
    tol <- as.double(tol)

    beta <- as.double(0)
    omega <- as.double(0)
    info <- as.integer(0)
    lcwork <- as.integer(max(1, n * (n + 3)))
    ldwork <- as.integer(max(1, 3 * n * (n + 2)))
    dwork <- array(as.double(1), c(ldwork))
    lda <- dim(a)[1]


    res <- .Fortran("AB13FD", N = n, A = a, LDA = lda, BETA = beta, OMEGA = omega, TOL = tol, DWORK = dwork, LDWORK = ldwork, LCWORK = lcwork, INFO = info)

    return(list(beta = res$BETA, omega = res$OMEGA, info = res$INFO))
}

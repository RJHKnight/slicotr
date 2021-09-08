#' mb05md
#'
#' Matrix exponential for a real non-defective matrix
#' @examples 

#'   To compute exp(A*delta) where A is a real N-by-N non-defective
#'   matrix with real or complex eigenvalues and delta is a scalar
#'   value. The routine also returns the eigenvalues and eigenvectors
#'   of A as well as (if all eigenvalues are real) the matrix product
#'   exp(Lambda*delta) times the inverse of the eigenvector matrix
#'   of A, where Lambda is the diagonal matrix of eigenvalues.
#'   Optionally, the routine computes a balancing transformation to
#'   improve the conditioning of the eigenvalues and eigenvectors.
#' 
#' #'
#' @references \url{http://slicot.org/objects/software/shared/doc/MB05MD.html}
#' @export
mb05md <- function(balanc, n, delta, a) {

    # In Parameters
    balanc <- as.character(balanc)
    delta <- as.double(delta)
    n <- as.integer(n)

    info <- as.integer(0)
    dwork <- array(as.double(1), c(4 * n))
    iwork <- array(as.integer(1), c(n))
    lda <- dim(a)[1]
    v <- array(as.double(0), c(n, n))
    vali <- array(as.double(0), c(n))
    valr <- array(as.double(0), c(n))
    y <- array(as.double(0), c(n, n))
    ldv <- dim(v)[1]
    ldwork <- dim(dwork)[1]
    ldy <- dim(y)[1]


    res <- .Fortran("MB05MD", BALANC = balanc, N = n, DELTA = delta, A = a, LDA = lda, V = v, LDV = ldv, Y = y, LDY = ldy, VALR = valr, VALI = vali, IWORK = iwork, DWORK = dwork, LDWORK = ldwork, INFO = info)

    return(list(a = res$A, info = res$INFO, v = res$V, vali = res$VALI, valr = res$VALR, y = res$Y))
}

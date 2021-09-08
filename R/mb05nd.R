#' mb05nd
#'
#' Matrix exponential and integral for a real matrix
#' @examples 

#'   To compute
#' 
#'   (a)    F(delta) =  exp(A*delta) and
#' 
#'   (b)    H(delta) =  Int[F(s) ds] from s = 0 to s = delta,
#' 
#'   where A is a real N-by-N matrix and delta is a scalar value.
#' 
#' #'
#' @references \url{http://slicot.org/objects/software/shared/doc/MB05ND.html}
#' @export
mb05nd <- function(n, delta, tol, a) {

    # In Parameters
    delta <- as.double(delta)
    n <- as.integer(n)
    tol <- as.double(tol)



    info <- as.integer(0)
    dwork <- array(as.double(1), c(max(2 * n * n, 1)))
    iwork <- array(as.integer(1), c(n))
    ldex <- dim(ex)[1]
    ldexin <- dim(exint)[1]
    lda <- dim(a)[1]
    ldwork <- dim(dwork)[1]


    res <- .Fortran("MB05ND", N = n, DELTA = delta, EX = ex, EXINT = exint, TOL = tol, INFO = info, A = a, DWORK = dwork, IWORK = iwork, LDEX = ldex, LDEXIN = ldexin, LDA = lda, LDWORK = ldwork)

    return(list(ex = res$EX, exint = res$EXINT, info = res$INFO))
}

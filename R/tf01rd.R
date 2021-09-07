#' tf01rd
#'
#' Markov parameters of a multivariable system from the state-space representation
#' @examples 

#'   To compute N Markov parameters M(1), M(2),..., M(N) from the
#'   parameters (A,B,C) of a linear time-invariant system, where each
#'   M(k) is an NC-by-NB matrix and k = 1,2,...,N.
#' 
#'   All matrices are treated as dense, and hence TF01RD is not
#'   intended for large sparse problems.
#' 
#' #'
#' @references \url{http://slicot.org/objects/software/shared/doc/TF01RD.html}
#' @export
tf01rd <- function(na, nb, nc, n, a, b, c, ldwork) {

    # In Parameters
    n <- as.integer(n)
    na_ <- as.integer(na)
    nb <- as.integer(nb)
    nc <- as.integer(nc)
    ldwork <- as.integer(ldwork)

    # Out Parameters
    h <- array(as.double(0), c(nc, n * nb))
    info <- as.integer(0)

    # Hidden Parameters
    lda <- dim(a)[1]
    ldb <- dim(b)[1]
    ldc <- dim(c)[1]
    ldh <- dim(h)[1]
    dwork <- array(as.double(1), c(ldwork))

    res <- .Fortran("TF01RD", `NA` = na, NB = nb, NC = nc, N = n, A = a, B = b, C = c, LDWORK = ldwork, H = h, INFO = info, LDA = lda, LDB = ldb, LDC = ldc, LDH = ldh, DWORK = dwork)

    return(list(h = res$H, info = res$INFO))
}

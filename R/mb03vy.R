#' mb03vy
#'
#' Orthogonal matrices for reduction to periodic Hessenberg form of a product of matrices
#' @examples 

#'   To generate the real orthogonal matrices Q_1, Q_2, ..., Q_p,
#'   which are defined as the product of ihi-ilo elementary reflectors
#'   of order n, as returned by SLICOT Library routine MB03VD:
#' 
#'      Q_j = H_j(ilo) H_j(ilo+1) . . . H_j(ihi-1).
#' 
#' #'
#' @references \url{http://slicot.org/objects/software/shared/doc/MB03VY.html}
#' @export
mb03vy <- function(n, ilo, ihi, a, tau, ldwork) {

    # In Parameters
    ldwork <- as.integer(ldwork)
    n <- as.integer(n)
    ihi <- as.integer(ihi)
    ilo <- as.integer(ilo)

    info <- as.integer(0)
    dwork <- array(as.double(1), c(ldwork))
    lda1 <- dim(a)[1]
    lda2 <- dim(a)[2]
    p <- dim(a)[3]
    ldtau <- dim(tau)[1]


    res <- .Fortran("MB03VY", N = n, P = p, ILO = ilo, IHI = ihi, A = a, LDA1 = lda1, LDA2 = lda2, TAU = tau, LDTAU = ldtau, DWORK = dwork, LDWORK = ldwork, INFO = info)

    return(list(a = res$A, info = res$INFO))
}

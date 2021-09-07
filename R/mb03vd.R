#' mb03vd
#'
#' Periodic Hessenberg form of a product of p matrices using orthogonal similarity transformations
#' @examples 

#'   To reduce a product of p real general matrices A = A_1*A_2*...*A_p
#'   to upper Hessenberg form, H = H_1*H_2*...*H_p, where H_1 is
#'   upper Hessenberg, and H_2, ..., H_p are upper triangular, by using
#'   orthogonal similarity transformations on A,
#' 
#'           Q_1' * A_1 * Q_2 = H_1,
#'           Q_2' * A_2 * Q_3 = H_2,
#'                  ...
#'           Q_p' * A_p * Q_1 = H_p.
#' 
#' #'
#' @references \url{http://slicot.org/objects/software/shared/doc/MB03VD.html}
#' @export
mb03vd <- function(n, ilo, ihi, a) {

    # In Parameters
    n <- as.integer(n)
    ihi <- as.integer(ihi)
    ilo <- as.integer(ilo)

    # Out Parameters
    tau <- array(as.double(0), c(max(1, n - 1), p))
    info <- as.integer(0)

    # Hidden Parameters
    p <- dim(a)[3]
    lda1 <- dim(a)[1]
    lda2 <- dim(a)[2]
    ldtau <- dim(tau)[1]
    dwork <- array(as.double(1), c(n))

    res <- .Fortran("MB03VD", N = n, ILO = ilo, IHI = ihi, A = a, TAU = tau, INFO = info, P = p, LDA1 = lda1, LDA2 = lda2, LDTAU = ldtau, DWORK = dwork)

    return(list(a = res$A, tau = res$TAU, info = res$INFO))
}

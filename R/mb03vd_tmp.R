mb03vd <- function(n, p, ilo, a, tau, info) {

    # In Parameters
    n <- as.integer(n)
    ilo <- as.integer(ilo)

    # Hidden Parameters
    p <- dim(a)[3]

    # Out Parameters
    tau <- array(as.integer(0), c(max(1, n - 1), p))
    info <- as.integer(0)

    # Check dimensions of input params
    if (dim(a) != c(lda1, lda2, p))
        stop("Incorrect dimensions for matrix a")

    res <- .Fortran(MB03VD, N = n, P = p, ILO = ilo, A = a, TAU = tau, INFO = info)

    return(list(a = res$A, tau = res$TAU, info = res$INFO))
}

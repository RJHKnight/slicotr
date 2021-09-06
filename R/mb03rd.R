mb03rd <- function(jobx, sort, n, pmax, a, x, tol) {

    # In Parameters
    jobx <- as.character(jobx)
    n <- as.integer(n)
    pmax <- as.double(pmax)
    sort <- as.character(sort)
    tol <- as.double(tol)

    # Out Parameters
    nblcks <- as.integer(0)
    blsize <- array(as.integer(0), c(n))
    wr <- array(as.double(0), c(n))
    wi <- array(as.double(0), c(n))
    info <- as.integer(0)

    # Hidden Parameters
    lda <- dim(a)[1]
    ldx <- dim(x)[1]
    dwork <- array(as.double(1), c(n))

    res <- .Fortran("MB03RD", JOBX = jobx, SORT = sort, N = n, PMAX = pmax, A = a, X = x, TOL = tol, NBLCKS = nblcks, BLSIZE = blsize, WR = wr, WI = wi, INFO = info, LDA = lda, LDX = ldx, DWORK = dwork)

    return(list(a = res$A, x = res$X, nblcks = res$NBLCKS, blsize = res$BLSIZE, wr = res$WR, wi = res$WI, info = res$INFO))
}

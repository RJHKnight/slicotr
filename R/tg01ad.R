tg01ad <- function(job, l, n, m, p, thresh, a, e, b, c) {

    # In Parameters
    job <- as.character(job)
    l <- as.integer(l)
    m <- as.integer(m)
    n <- as.integer(n)
    p <- as.integer(p)
    thresh <- as.double(thresh)

    # Out Parameters
    lscale <- array(as.double(0), c(l))
    rscale <- array(as.double(0), c(n))
    info <- as.integer(0)

    # Hidden Parameters
    lda <- max(dim(a)[1], 1)
    lde <- max(dim(e)[1], 1)
    ldb <- max(dim(b)[1], 1)
    ldc <- max(dim(c)[1], 1)
    dwork <- array(as.double(1), c(3 * (l + n)))

    res <- .Fortran("TG01AD", JOB = job, L = l, N = n, M = m, P = p, THRESH = thresh, A = a, E = e, B = b, C = c, LSCALE = lscale, RSCALE = rscale, INFO = info, LDA = lda, LDE = lde, LDB = ldb, LDC = ldc, DWORK = dwork)

    return(list(a = res$A, e = res$E, b = res$B, c = res$C, lscale = res$LSCALE, rscale = res$RSCALE, info = res$INFO))
}

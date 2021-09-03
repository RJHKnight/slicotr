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
    lda <- dim(shape)[`NA`]
    lde <- dim(shape)[`NA`]
    ldb <- dim(shape)[`NA`]
    ldc <- dim(shape)[`NA`]
    dwork <- array(as.double(1), c(3 * (l + n)))

    res <- .Fortran("TG01AD", JOB = job, L = l, N = n, M = m, P = p, THRESH = thresh, A = a, LDA = lda, E = e, LDE = lde, B = b, LDB = ldb, C = c, LDC = ldc, LSCALE = lscale, RSCALE = rscale, DWORK = dwork,
        INFO = info)

    return(list(a = res$A, e = res$E, b = res$B, c = res$C, lscale = res$LSCALE, rscale = res$RSCALE, info = res$INFO))
}

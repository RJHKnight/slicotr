tb01id <- function(job, n, m, p, maxred, a, b, c) {

    # In Parameters
    job <- as.character(job)
    m <- as.integer(m)
    maxred <- as.double(maxred)
    n <- as.integer(n)
    p <- as.integer(p)

    # Out Parameters
    scale <- array(as.double(0), c(n))
    info <- as.integer(0)

    # Hidden Parameters
    lda <- dim(a)[1]
    ldb <- dim(b)[1]
    ldc <- dim(c)[1]

    res <- .Fortran("TB01ID", JOB = job, N = n, M = m, P = p, MAXRED = maxred, A = a, LDA = lda, B = b, LDB = ldb, C = c, LDC = ldc, SCALE = scale, INFO = info)

    return(list(maxred = res$MAXRED, a = res$A, b = res$B, c = res$C, scale = res$SCALE, info = res$INFO))
}

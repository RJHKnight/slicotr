tb01pd <- function(job, equil, n, m, p, a, b, c, tol, ldwork) {

    # In Parameters
    equil <- as.character(equil)
    job <- as.character(job)
    m <- as.integer(m)
    n <- as.integer(n)
    p <- as.integer(p)
    tol <- as.double(tol)
    ldwork <- as.integer(ldwork)

    # Out Parameters
    nr <- as.integer(0)
    info <- as.integer(0)

    # Hidden Parameters
    lda <- dim(a)[1]
    ldb <- dim(b)[1]
    ldc <- dim(c)[1]
    iwork <- array(as.integer(1), c(n + max(m, p)))
    dwork <- array(as.double(1), c(ldwork))

    res <- .Fortran("TB01PD", JOB = job, EQUIL = equil, N = n, M = m, P = p, A = a, B = b, C = c, TOL = tol, LDWORK = ldwork, NR = nr, INFO = info, LDA = lda, LDB = ldb, LDC = ldc, IWORK = iwork, DWORK = dwork)

    return(list(a = res$A, b = res$B, c = res$C, nr = res$NR, info = res$INFO))
}

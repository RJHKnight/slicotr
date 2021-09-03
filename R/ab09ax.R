ab09ax <- function(dico, job, ordsel, n, m, p, nr, a, b, c, tol, ldwork) {

    # In Parameters
    dico <- as.character(dico)
    job <- as.character(job)
    ldwork <- as.integer(ldwork)
    m <- as.integer(m)
    n <- as.integer(n)
    nr <- as.integer(nr)
    ordsel <- as.character(ordsel)
    p <- as.integer(p)
    tol <- as.double(tol)

    # Out Parameters
    hsv <- array(as.double(0), c(n))
    t <- array(as.double(0), c(n, n))
    ti <- array(as.double(0), c(n, n))
    iwarn <- as.integer(0)
    info <- as.integer(0)

    # Hidden Parameters
    lda <- dim(a)[1]
    ldb <- dim(b)[1]
    ldc <- dim(c)[1]
    ldt <- dim(t)[1]
    ldti <- dim(ti)[1]
    iwork <- array(as.integer(1), c(max(m, p)))
    dwork <- array(as.double(1), c(ldwork))

    res <- .Fortran("AB09AX", DICO = dico, JOB = job, ORDSEL = ordsel, N = n, M = m, P = p, NR = nr, A = a, LDA = lda, B = b, LDB = ldb, C = c, LDC = ldc, HSV = hsv, T = t, LDT = ldt, TI = ti, LDTI = ldti,
        TOL = tol, IWORK = iwork, DWORK = dwork, LDWORK = ldwork, IWARN = iwarn, INFO = info)

    return(list(nr = res$NR, a = res$A, b = res$B, c = res$C, hsv = res$HSV, t = res$T, ti = res$TI, iwarn = res$IWARN, info = res$INFO))
}

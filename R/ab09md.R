ab09md <- function(dico, job, equil, ordsel, n, m, p, nr, alpha, a, b, c, tol, ldwork) {

    # In Parameters
    alpha <- as.double(alpha)
    dico <- as.character(dico)
    equil <- as.character(equil)
    job <- as.character(job)
    ldwork <- as.integer(ldwork)
    m <- as.integer(m)
    n <- as.integer(n)
    nr <- as.integer(nr)
    ordsel <- as.character(ordsel)
    p <- as.integer(p)
    tol <- as.double(tol)

    # Out Parameters
    ns <- as.integer(0)
    hsv <- array(as.double(0), c(n))
    iwarn <- as.integer(0)
    info <- as.integer(0)

    # Hidden Parameters
    lda <- dim(a)[1]
    ldb <- dim(b)[1]
    ldc <- dim(c)[1]
    iwork <- array(as.integer(1), c(max(m, p)))
    dwork <- array(as.double(1), c(ldwork))

    res <- .Fortran("AB09MD", DICO = dico, JOB = job, EQUIL = equil, ORDSEL = ordsel, N = n, M = m, P = p, NR = nr, ALPHA = alpha, A = a, B = b, C = c, TOL = tol, LDWORK = ldwork, NS = ns, HSV = hsv, IWARN = iwarn, INFO = info, LDA = lda,
        LDB = ldb, LDC = ldc, IWORK = iwork, DWORK = dwork)

    return(list(nr = res$NR, a = res$A, b = res$B, c = res$C, ns = res$NS, hsv = res$HSV, iwarn = res$IWARN, info = res$INFO))
}

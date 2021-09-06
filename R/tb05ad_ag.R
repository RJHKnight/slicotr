tb05ad_ag <- function(n, m, p, a, b, c, ldwork, lzwork) {

    # In Parameters
    m <- as.integer(m)
    n <- as.integer(n)
    p <- as.integer(p)
    ldwork <- as.integer(ldwork)
    lzwork <- as.integer(lzwork)

    # Out Parameters
    rcond <- as.double(0)
    evre <- array(as.double(0), c(n))
    evim <- array(as.double(0), c(n))
    info <- as.integer(0)

    # Hidden Parameters
    baleig <- as.character("a")
    inita <- as.character("g")
    lda <- dim(a)[1]
    ldb <- dim(b)[1]
    ldc <- dim(c)[1]
    ldg <- as.integer(p)
    ldhinv <- as.integer(n)
    iwork <- array(as.integer(1), c(n))
    dwork <- array(as.double(1), c(ldwork))

    res <- .Fortran(" FORTRANNAME TB05AD", N = n, M = m, P = p, A = a, B = b, C = c, LDWORK = ldwork, LZWORK = lzwork, RCOND = rcond, EVRE = evre, EVIM = evim, INFO = info, BALEIG = baleig, INITA = inita, LDA = lda, LDB = ldb, LDC = ldc,
        LDG = ldg, LDHINV = ldhinv, IWORK = iwork, DWORK = dwork)

    return(list(a = res$A, b = res$B, c = res$C, rcond = res$RCOND, evre = res$EVRE, evim = res$EVIM, info = res$INFO))
}

tb05ad_ng <- function(n, m, p, a, b, c, ldwork, lzwork) {

    # In Parameters
    m <- as.integer(m)
    n <- as.integer(n)
    p <- as.integer(p)
    ldwork <- as.integer(ldwork)
    lzwork <- as.integer(lzwork)

    # Out Parameters
    info <- as.integer(0)

    # Hidden Parameters
    baleig <- "N"
    inita <- "G"
    lda <- dim(a)[1]
    ldb <- dim(b)[1]
    ldc <- dim(c)[1]
    rcond <- ldg <- p
    evre <- array(as.double(1), c(n))
    evim <- array(as.double(1), c(n))
    ldhinv <- n
    iwork <- array(as.integer(1), c(n))
    dwork <- array(as.double(1), c(ldwork))

    res <- .Fortran(" FORTRAN`NA`ME TB05AD", BALEIG = baleig, INITA = inita, N = n, M = m, P = p, A = a, LDA = lda, B = b, LDB = ldb, C = c, LDC = ldc, RCOND = rcond, LDG = ldg, EVRE = evre, EVIM = evim,
        LDHINV = ldhinv, IWORK = iwork, DWORK = dwork, LDWORK = ldwork, LZWORK = lzwork, INFO = info)

    return(list(a = res$A, b = res$B, c = res$C, info = res$INFO))
}

tg01fd_nn <- function(joba, l, n, m, p, a, e, b, c, tol, ldwork) {

    # In Parameters
    joba <- as.character(joba)
    l <- as.integer(l)
    ldwork <- as.integer(ldwork)
    m <- as.integer(m)
    n <- as.integer(n)
    p <- as.integer(p)
    tol <- as.double(tol)

    # Out Parameters
    ranke <- as.integer(0)
    rnka22 <- as.integer(0)
    info <- as.integer(0)

    # Hidden Parameters
    compq <- "N"
    compz <- "N"
    lda <- dim(shape)[`NA`]
    lde <- dim(shape)[`NA`]
    ldb <- dim(shape)[`NA`]
    ldc <- dim(shape)[`NA`]
    q <- array(as.double(1), c(0, 0))
    ldq <- 1
    z <- array(as.double(1), c(0, 0))
    ldz <- 1
    iwork <- array(as.integer(1), c(ldwork))
    dwork <- array(as.double(1), c(ldwork))

    res <- .Fortran("   FORTRAN`NA`ME TG01FD", COMPQ = compq, COMPZ = compz, JOBA = joba, L = l, N = n, M = m, P = p, A = a, LDA = lda, E = e, LDE = lde, B = b, LDB = ldb, C = c, LDC = ldc, Q = q,
        LDQ = ldq, Z = z, LDZ = ldz, RANKE = ranke, RNKA22 = rnka22, TOL = tol, IWORK = iwork, DWORK = dwork, LDWORK = ldwork, INFO = info)

    return(list(a = res$A, e = res$E, b = res$B, c = res$C, ranke = res$RANKE, rnka22 = res$RNKA22, info = res$INFO))
}

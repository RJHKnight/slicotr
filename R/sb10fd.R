sb10fd <- function(n, m, np, ncon, nmeas, gamma, a, b, c, d, tol, ldwork) {

    # In Parameters
    gamma <- as.double(gamma)
    ldwork <- as.integer(ldwork)
    m <- as.integer(m)
    n <- as.integer(n)
    np <- as.integer(np)
    tol <- as.double(tol)
    ncon <- as.integer(ncon)
    nmeas <- as.integer(nmeas)

    # Out Parameters
    ak <- array(as.double(0), c(n, n))
    bk <- array(as.double(0), c(n, nmeas))
    ck <- array(as.double(0), c(ncon, n))
    dk <- array(as.double(0), c(ncon, nmeas))
    rcond <- array(as.double(0), c(4))
    info <- as.integer(0)

    # Hidden Parameters
    lda <- dim(a)[1]
    ldb <- dim(b)[1]
    ldc <- dim(c)[1]
    ldd <- dim(d)[1]
    ldak <- dim(ak)[1]
    ldbk <- dim(bk)[1]
    ldck <- dim(ck)[1]
    lddk <- dim(dk)[1]
    iwork <- array(as.integer(1), c(max(2 * max(n, m - ncon), 2 * max(np - nmeas, ncon)), n * n))
    dwork <- array(as.double(1), c(ldwork))
    bwork <- array(as.logical(1), c(2 * n))

    res <- .Fortran("SB10FD", N = n, M = m, NP = np, NCON = ncon, NMEAS = nmeas, GAMMA = gamma, A = a, LDA = lda, B = b, LDB = ldb, C = c, LDC = ldc, D = d, LDD = ldd, AK = ak, LDAK = ldak, BK = bk,
        LDBK = ldbk, CK = ck, LDCK = ldck, DK = dk, LDDK = lddk, RCOND = rcond, TOL = tol, IWORK = iwork, DWORK = dwork, LDWORK = ldwork, BWORK = bwork, INFO = info)

    return(list(ak = res$AK, bk = res$BK, ck = res$CK, dk = res$DK, rcond = res$RCOND, info = res$INFO))
}

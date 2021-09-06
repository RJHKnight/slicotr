sb10dd <- function(n, m, np, ncon, nmeas, gamma, a, b, c, d, tol, ldwork) {

    # In Parameters
    gamma <- as.double(gamma)
    ldwork <- as.integer(ldwork)
    m <- as.integer(m)
    n <- as.integer(n)
    ncon <- as.integer(ncon)
    np <- as.integer(np)
    tol <- as.double(tol)
    nmeas <- as.integer(nmeas)

    # Out Parameters
    ak <- array(as.double(0), c(n, n))
    bk <- array(as.double(0), c(n, nmeas))
    ck <- array(as.double(0), c(ncon, n))
    dk <- array(as.double(0), c(ncon, nmeas))
    x <- array(as.double(0), c(n, n))
    z <- array(as.double(0), c(n, n))
    rcond <- array(as.double(0), c(8))
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
    ldx <- dim(x)[1]
    ldz <- dim(z)[1]
    iwork <- array(as.integer(1), c(max(2 * max(m, n), max(m, max(m + np, n * n)))))
    iwork <- array(as.integer(1), c(max(2 * max(max(max(max(n, m - ncon), np - nmeas), ncon), nmeas), n * n)))
    dwork <- array(as.double(1), c(ldwork))
    bwork <- array(as.logical(1), c(2 * n))

    res <- .Fortran("SB10DD", N = n, M = m, NP = np, NCON = ncon, NMEAS = nmeas, GAMMA = gamma, A = a, B = b, C = c, D = d, TOL = tol, LDWORK = ldwork, AK = ak, BK = bk, CK = ck, DK = dk, X = x, Z = z, RCOND = rcond, INFO = info, LDA = lda,
        LDB = ldb, LDC = ldc, LDD = ldd, LDAK = ldak, LDBK = ldbk, LDCK = ldck, LDDK = lddk, LDX = ldx, LDZ = ldz, IWORK = iwork, IWORK = iwork, DWORK = dwork, BWORK = bwork)

    return(list(gamma = res$GAMMA, ak = res$AK, bk = res$BK, ck = res$CK, dk = res$DK, x = res$X, z = res$Z, rcond = res$RCOND, info = res$INFO))
}

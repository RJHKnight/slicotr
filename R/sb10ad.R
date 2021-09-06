sb10ad <- function(job, n, m, np, ncon, nmeas, gamma, a, b, c, d, gtol, actol, liwork, ldwork) {

    # In Parameters
    actol <- as.double(actol)
    gamma <- as.double(gamma)
    gtol <- as.double(gtol)
    job <- as.integer(job)
    ldwork <- as.integer(ldwork)
    liwork <- as.integer(liwork)
    m <- as.integer(m)
    n <- as.integer(n)
    ncon <- as.integer(ncon)
    np <- as.integer(np)
    nmeas <- as.integer(nmeas)

    # Out Parameters
    ak <- array(as.double(0), c(n, n))
    bk <- array(as.double(0), c(n, nmeas))
    ck <- array(as.double(0), c(ncon, n))
    dk <- array(as.double(0), c(ncon, nmeas))
    ac <- array(as.double(0), c(2 * n, 2 * n))
    bc <- array(as.double(0), c(2 * n, m - ncon))
    cc <- array(as.double(0), c(np - nmeas, 2 * n))
    dc <- array(as.double(0), c(np - nmeas, m - ncon))
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
    ldac <- dim(ac)[1]
    ldbc <- dim(bc)[1]
    ldcc <- dim(cc)[1]
    lddc <- dim(dc)[1]
    iwork <- array(as.integer(1), c(liwork))
    dwork <- array(as.double(1), c(ldwork))
    lbwork <- as.integer(2 * n)
    bwork <- array(as.logical(1), c(lbwork))

    res <- .Fortran("SB10AD", JOB = job, N = n, M = m, NP = np, NCON = ncon, NMEAS = nmeas, GAMMA = gamma, A = a, B = b, C = c, D = d, GTOL = gtol, ACTOL = actol, LIWORK = liwork, LDWORK = ldwork, AK = ak, BK = bk, CK = ck, DK = dk,
        AC = ac, BC = bc, CC = cc, DC = dc, RCOND = rcond, INFO = info, LDA = lda, LDB = ldb, LDC = ldc, LDD = ldd, LDAK = ldak, LDBK = ldbk, LDCK = ldck, LDDK = lddk, LDAC = ldac, LDBC = ldbc, LDCC = ldcc, LDDC = lddc, IWORK = iwork,
        DWORK = dwork, LBWORK = lbwork, BWORK = bwork)

    return(list(gamma = res$GAMMA, ak = res$AK, bk = res$BK, ck = res$CK, dk = res$DK, ac = res$AC, bc = res$BC, cc = res$CC, dc = res$DC, rcond = res$RCOND, info = res$INFO))
}

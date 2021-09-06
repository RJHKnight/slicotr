ag08bd <- function(equil, l, n, m, p, a, e, b, c, d, tol, ldwork) {

    # In Parameters
    equil <- as.character(equil)
    l <- as.integer(l)
    ldwork <- as.integer(ldwork)
    m <- as.integer(m)
    n <- as.integer(n)
    p <- as.integer(p)
    tol <- as.double(tol)

    # Out Parameters
    nfz <- as.integer(0)
    nrank <- as.integer(0)
    niz <- as.integer(0)
    dinfz <- as.integer(0)
    nkror <- as.integer(0)
    ninfe <- as.integer(0)
    nkrol <- as.integer(0)
    infz <- array(as.integer(0), c(n + 1))
    kronr <- array(as.integer(0), c(n + m + 1))
    infe <- array(as.integer(0), c(1 + min(l + p, n + m)))
    kronl <- array(as.integer(0), c(l + p + 1))
    info <- as.integer(0)

    # Hidden Parameters
    lda <- max(dim(a)[1], 1)
    lde <- max(dim(e)[1], 1)
    ldb <- max(dim(b)[1], 1)
    ldc <- max(dim(c)[1], 1)
    ldd <- max(dim(d)[1], 1)
    iwork <- array(as.integer(1), c(ldwork))
    dwork <- array(as.double(1), c(ldwork))

    res <- .Fortran("AG08BD", EQUIL = equil, L = l, N = n, M = m, P = p, A = a, E = e, B = b, C = c, D = d, TOL = tol, LDWORK = ldwork, NFZ = nfz, NRANK = nrank, NIZ = niz, DINFZ = dinfz, NKROR = nkror, NINFE = ninfe, NKROL = nkrol,
        INFZ = infz, KRONR = kronr, INFE = infe, KRONL = kronl, INFO = info, LDA = lda, LDE = lde, LDB = ldb, LDC = ldc, LDD = ldd, IWORK = iwork, DWORK = dwork)

    return(list(a = res$A, e = res$E, nfz = res$NFZ, nrank = res$NRANK, niz = res$NIZ, dinfz = res$DINFZ, nkror = res$NKROR, ninfe = res$NINFE, nkrol = res$NKROL, infz = res$INFZ, kronr = res$KRONR, infe = res$INFE, kronl = res$KRONL,
        info = res$INFO))
}

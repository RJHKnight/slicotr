ab01nd <- function(jobz, n, m, a, b, tol, ldwork) {

    # In Parameters
    jobz <- as.character(jobz)
    ldwork <- as.integer(ldwork)
    m <- as.integer(m)
    n <- as.integer(n)
    tol <- as.double(tol)

    # Out Parameters
    ncont <- as.integer(0)
    indcon <- as.integer(0)
    nblk <- array(as.integer(0), c(n))
    z <- array(as.double(0), c(ldz, n))
    tau <- array(as.double(0), c(n))
    info <- as.integer(0)

    # Hidden Parameters
    lda <- dim(a)[1]
    ldb <- dim(b)[1]
    ldz <- as.integer(ifelse(jobz == "n", 1, n))
    iwork <- array(as.integer(1), c(m))
    dwork <- array(as.double(1), c(ldwork))

    res <- .Fortran("AB01ND", JOBZ = jobz, N = n, M = m, A = a, B = b, TOL = tol, LDWORK = ldwork, NCONT = ncont, INDCON = indcon, NBLK = nblk, Z = z, TAU = tau, INFO = info, LDA = lda, LDB = ldb, LDZ = ldz, IWORK = iwork, DWORK = dwork)

    return(list(a = res$A, b = res$B, ncont = res$NCONT, indcon = res$INDCON, nblk = res$NBLK, z = res$Z, tau = res$TAU, info = res$INFO))
}

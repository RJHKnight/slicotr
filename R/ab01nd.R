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
    ldz <- (`?`(jobz == "N", 1:n))
    iwork <- array(as.integer(1), c(m))
    dwork <- array(as.double(1), c(ldwork))

    res <- .Fortran("AB01ND", JOBZ = jobz, N = n, M = m, A = a, LDA = lda, B = b, LDB = ldb, NCONT = ncont, INDCON = indcon, NBLK = nblk, Z = z, LDZ = ldz, TAU = tau, TOL = tol, IWORK = iwork, DWORK = dwork,
        LDWORK = ldwork, INFO = info)

    return(list(a = res$A, b = res$B, ncont = res$NCONT, indcon = res$INDCON, nblk = res$NBLK, z = res$Z, tau = res$TAU, info = res$INFO))
}

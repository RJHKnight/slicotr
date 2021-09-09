#' sb10hd
#'
#' H2 optimal state controller for a continuous-time system
#' @examples 

#'   To compute the matrices of the H2 optimal n-state controller
#' 
#'            | AK | BK |
#'        K = |----|----|
#'            | CK | DK |
#' 
#'   for the system
#' 
#'                 | A  | B1  B2  |   | A | B |
#'             P = |----|---------| = |---|---| ,
#'                 | C1 |  0  D12 |   | C | D |
#'                 | C2 | D21 D22 |
#' 
#'   where B2 has as column size the number of control inputs (NCON)
#'   and C2 has as row size the number of measurements (NMEAS) being
#'   provided to the controller.
#' 
#'   It is assumed that
#' 
#'   (A1) (A,B2) is stabilizable and (C2,A) is detectable,
#' 
#'   (A2) The block D11 of D is zero,
#' 
#'   (A3) D12 is full column rank and D21 is full row rank.
#' 
#' #'
#' @references \url{http://slicot.org/objects/software/shared/doc/SB10HD.html}
#' @export
sb10hd <- function(n, m, np, ncon, nmeas, a, b, c, d, tol, ldwork) {

    # In Parameters
    ldwork <- as.integer(ldwork)
    m <- as.integer(m)
    n <- as.integer(n)
    ncon <- as.integer(ncon)
    np <- as.integer(np)
    tol <- as.double(tol)
    nmeas <- as.integer(nmeas)

    rcond <- array(as.double(0), c(4))
    info <- as.integer(0)
    ak <- array(as.double(0), c(n, n))
    bk <- array(as.double(0), c(n, nmeas))
    bwork <- array(as.logical(1), c(2 * n))
    ck <- array(as.double(0), c(ncon, n))
    dk <- array(as.double(0), c(ncon, nmeas))
    dwork <- array(as.double(1), c(ldwork))
    iwork <- array(as.integer(1), c(max(2 * n, n * n)))
    lda <- dim(a)[1]
    ldak <- dim(ak)[1]
    ldb <- dim(b)[1]
    ldbk <- dim(bk)[1]
    ldc <- dim(c)[1]
    ldck <- dim(ck)[1]
    ldd <- dim(d)[1]
    lddk <- dim(dk)[1]


    res <- suppressWarnings(.Fortran("SB10HD", N = n, M = m, NP = np, NCON = ncon, NMEAS = nmeas, A = a, LDA = lda, B = b, LDB = ldb,
        C = c, LDC = ldc, D = d, LDD = ldd, AK = ak, LDAK = ldak, BK = bk, LDBK = ldbk, CK = ck, LDCK = ldck, DK = dk, LDDK = lddk,
        RCOND = rcond, TOL = tol, IWORK = iwork, DWORK = dwork, LDWORK = ldwork, BWORK = bwork, INFO = info))

    return(list(rcond = res$RCOND, info = res$INFO, ak = res$AK, bk = res$BK, ck = res$CK, dk = res$DK))
}

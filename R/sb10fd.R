#' sb10fd
#'
#' H-infinity (sub)optimal state controller for a continuous-time system
#' @examples 

#'   To compute the matrices of an H-infinity (sub)optimal n-state
#'   controller
#' 
#'            | AK | BK |
#'        K = |----|----|,
#'            | CK | DK |
#' 
#'   using modified Glover's and Doyle's 1988 formulas, for the system
#' 
#'                 | A  | B1  B2  |   | A | B |
#'             P = |----|---------| = |---|---|
#'                 | C1 | D11 D12 |   | C | D |
#'                 | C2 | D21 D22 |
#' 
#'   and for a given value of gamma, where B2 has as column size the
#'   number of control inputs (NCON) and C2 has as row size the number
#'   of measurements (NMEAS) being provided to the controller.
#' 
#'   It is assumed that
#' 
#'   (A1) (A,B2) is stabilizable and (C2,A) is detectable,
#' 
#'   (A2) D12 is full column rank and D21 is full row rank,
#' 
#'   (A3) | A-j*omega*I  B2  | has full column rank for all omega,
#'        |    C1        D12 |
#' 
#'   (A4) | A-j*omega*I  B1  |  has full row rank for all omega.
#'        |    C2        D21 |
#' 
#' #'
#' @references \url{http://slicot.org/objects/software/shared/doc/SB10FD.html}
#' @export
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

    ak <- array(as.double(0), c(n, n))
    bk <- array(as.double(0), c(n, nmeas))
    ck <- array(as.double(0), c(ncon, n))
    dk <- array(as.double(0), c(ncon, nmeas))
    rcond <- array(as.double(0), c(4))
    info <- as.integer(0)
    bwork <- array(as.logical(1), c(2 * n))
    dwork <- array(as.double(1), c(ldwork))
    iwork <- array(as.integer(1), c(max(2 * max(n, m - ncon), 2 * max(np - nmeas, ncon)), n * n))
    ldak <- dim(ak)[1]
    ldbk <- dim(bk)[1]
    ldck <- dim(ck)[1]
    lddk <- dim(dk)[1]
    lda <- dim(a)[1]
    ldb <- dim(b)[1]
    ldc <- dim(c)[1]
    ldd <- dim(d)[1]


    res <- suppressWarnings(.Fortran("SB10FD", N = n, M = m, NP = np, NCON = ncon, NMEAS = nmeas, GAMMA = gamma, A = a, LDA = lda,
        B = b, LDB = ldb, C = c, LDC = ldc, D = d, LDD = ldd, AK = ak, LDAK = ldak, BK = bk, LDBK = ldbk, CK = ck, LDCK = ldck,
        DK = dk, LDDK = lddk, RCOND = rcond, TOL = tol, IWORK = iwork, DWORK = dwork, LDWORK = ldwork, BWORK = bwork, INFO = info))

    return(list(ak = res$AK, bk = res$BK, ck = res$CK, dk = res$DK, rcond = res$RCOND, info = res$INFO))
}

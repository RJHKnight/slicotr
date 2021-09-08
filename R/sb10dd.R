#' sb10dd
#'
#' H-infinity (sub)optimal controller for a discrete-time system
#' @examples 

#'   To compute the matrices of an H-infinity (sub)optimal n-state
#'   controller
#' 
#'                         | AK | BK |
#'                     K = |----|----|,
#'                         | CK | DK |
#' 
#'   for the discrete-time system
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
#'             j*Theta
#'   (A3) | A-e       *I  B2  | has full column rank for all
#'        |    C1         D12 |
#' 
#'        0 <= Theta < 2*Pi ,
#' 
#'             j*Theta
#'   (A4) | A-e       *I  B1  |  has full row rank for all
#'        |    C2         D21 |
#' 
#'        0 <= Theta < 2*Pi .
#' 
#' #'
#' @references \url{http://slicot.org/objects/software/shared/doc/SB10DD.html}
#' @export
sb10dd <- function(n, m, np, ncon, gamma, tol, ldwork, a, b, c, d, nmeas) {

    # In Parameters
    gamma <- as.double(gamma)
    ldwork <- as.integer(ldwork)
    m <- as.integer(m)
    n <- as.integer(n)
    ncon <- as.integer(ncon)
    np <- as.integer(np)
    tol <- as.double(tol)
    nmeas <- as.integer(nmeas)

    rcond <- array(as.double(0), c(8))
    info <- as.integer(0)
    ak <- array(as.double(0), c(n, n))
    bk <- array(as.double(0), c(n, nmeas))
    bwork <- array(as.logical(1), c(2 * n))
    ck <- array(as.double(0), c(ncon, n))
    dk <- array(as.double(0), c(ncon, nmeas))
    dwork <- array(as.double(1), c(ldwork))
    iwork <- array(as.integer(1), c(max(2 * max(m, n), max(m, max(m + np, n * n)))))
    x <- array(as.double(0), c(n, n))
    z <- array(as.double(0), c(n, n))
    lda <- dim(a)[1]
    ldak <- dim(ak)[1]
    ldb <- dim(b)[1]
    ldbk <- dim(bk)[1]
    ldc <- dim(c)[1]
    ldck <- dim(ck)[1]
    ldd <- dim(d)[1]
    lddk <- dim(dk)[1]
    ldx <- dim(x)[1]
    ldz <- dim(z)[1]


    res <- .Fortran("SB10DD", N = n, M = m, NP = np, NCON = ncon, GAMMA = gamma, RCOND = rcond, TOL = tol, LDWORK = ldwork, INFO = info, A = a, AK = ak, B = b, BK = bk, BWORK = bwork, C = c, CK = ck, D = d, DK = dk, DWORK = dwork, IWORK = iwork,
        NMEAS = nmeas, X = x, Z = z, LDA = lda, LDAK = ldak, LDB = ldb, LDBK = ldbk, LDC = ldc, LDCK = ldck, LDD = ldd, LDDK = lddk, LDX = ldx, LDZ = ldz)

    return(list(gamma = res$GAMMA, rcond = res$RCOND, info = res$INFO, ak = res$AK, bk = res$BK, ck = res$CK, dk = res$DK, x = res$X, z = res$Z))
}

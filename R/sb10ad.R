#' sb10ad
#'
#' H-infinity optimal controller using modified Glover's and Doyle's formulas (continuous-time)
#' @examples 

#'   To compute the matrices of an H-infinity optimal n-state
#'   controller
#' 
#'            | AK | BK |
#'        K = |----|----|,
#'            | CK | DK |
#' 
#'   using modified Glover's and Doyle's 1988 formulas, for the system
#' 
#'            | A  | B1  B2  |   | A | B |
#'        P = |----|---------| = |---|---|
#'            | C1 | D11 D12 |   | C | D |
#'            | C2 | D21 D22 |
#' 
#'   and for the estimated minimal possible value of gamma with respect
#'   to GTOL, where B2 has as column size the number of control inputs
#'   (NCON) and C2 has as row size the number of measurements (NMEAS)
#'   being provided to the controller, and then to compute the matrices
#'   of the closed-loop system
#' 
#'            | AC | BC |
#'        G = |----|----|,
#'            | CC | DC |
#' 
#'   if the stabilizing controller exists.
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
#' @references \url{http://slicot.org/objects/software/shared/doc/SB10AD.html}
#' @export
sb10ad <- function(job, n, m, np, ncon, gamma, gtol, actol, liwork, ldwork, a, b, c, d, nmeas) {

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

    rcond <- array(as.double(0), c(4))
    info <- as.integer(0)
    ac <- array(as.double(0), c(2 * n, 2 * n))
    ak <- array(as.double(0), c(n, n))
    bc <- array(as.double(0), c(2 * n, m - ncon))
    bk <- array(as.double(0), c(n, nmeas))
    cc <- array(as.double(0), c(np - nmeas, 2 * n))
    ck <- array(as.double(0), c(ncon, n))
    dc <- array(as.double(0), c(np - nmeas, m - ncon))
    dk <- array(as.double(0), c(ncon, nmeas))
    dwork <- array(as.double(1), c(ldwork))
    iwork <- array(as.integer(1), c(liwork))
    lbwork <- as.integer(2 * n)
    bwork <- array(as.logical(1), c(lbwork))
    lda <- dim(a)[1]
    ldac <- dim(ac)[1]
    ldak <- dim(ak)[1]
    ldb <- dim(b)[1]
    ldbc <- dim(bc)[1]
    ldbk <- dim(bk)[1]
    ldc <- dim(c)[1]
    ldcc <- dim(cc)[1]
    ldck <- dim(ck)[1]
    ldd <- dim(d)[1]
    lddc <- dim(dc)[1]
    lddk <- dim(dk)[1]


    res <- .Fortran("SB10AD", JOB = job, N = n, M = m, NP = np, NCON = ncon, GAMMA = gamma, RCOND = rcond, GTOL = gtol, ACTOL = actol, LIWORK = liwork, LDWORK = ldwork, INFO = info, A = a, AC = ac, AK = ak, B = b, BC = bc, BK = bk, C = c,
        CC = cc, CK = ck, D = d, DC = dc, DK = dk, DWORK = dwork, IWORK = iwork, LBWORK = lbwork, NMEAS = nmeas, BWORK = bwork, LDA = lda, LDAC = ldac, LDAK = ldak, LDB = ldb, LDBC = ldbc, LDBK = ldbk, LDC = ldc, LDCC = ldcc, LDCK = ldck,
        LDD = ldd, LDDC = lddc, LDDK = lddk)

    return(list(gamma = res$GAMMA, rcond = res$RCOND, info = res$INFO, ac = res$AC, ak = res$AK, bc = res$BC, bk = res$BK, cc = res$CC, ck = res$CK, dc = res$DC, dk = res$DK))
}

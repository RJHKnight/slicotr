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

    res <- .Fortran("SB10AD", JOB = job, N = n, M = m, NP = np, NCON = ncon, NMEAS = nmeas, GAMMA = gamma, A = a, B = b, C = c, D = d, GTOL = gtol, ACTOL = actol, LIWORK = liwork, LDWORK = ldwork,
        AK = ak, BK = bk, CK = ck, DK = dk, AC = ac, BC = bc, CC = cc, DC = dc, RCOND = rcond, INFO = info, LDA = lda, LDB = ldb, LDC = ldc, LDD = ldd, LDAK = ldak, LDBK = ldbk, LDCK = ldck,
        LDDK = lddk, LDAC = ldac, LDBC = ldbc, LDCC = ldcc, LDDC = lddc, IWORK = iwork, DWORK = dwork, LBWORK = lbwork, BWORK = bwork)

    return(list(gamma = res$GAMMA, ak = res$AK, bk = res$BK, ck = res$CK, dk = res$DK, ac = res$AC, bc = res$BC, cc = res$CC, dc = res$DC, rcond = res$RCOND, info = res$INFO))
}

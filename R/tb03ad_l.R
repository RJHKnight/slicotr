#' tb03ad_l
#'
#' Left/right polynomial matrix representation of a given state-space representation
#' @examples 

#'   To find a relatively prime left polynomial matrix representation
#'   inv(P(s))*Q(s) or right polynomial matrix representation
#'   Q(s)*inv(P(s)) with the same transfer matrix T(s) as that of a
#'   given state-space representation, i.e.
#' 
#'      inv(P(s))*Q(s) = Q(s)*inv(P(s)) = T(s) = C*inv(s*I-A)*B + D.
#' 
#' #'
#' @references \url{http://slicot.org/objects/software/shared/doc/TB03AD.html}
#' @export
tb03ad_l <- function(equil, n, m, p, a, b, c, d, tol, ldwork) {

    # In Parameters
    equil <- as.character(equil)
    ldwork <- as.integer(ldwork)
    m <- as.integer(m)
    n <- as.integer(n)
    p <- as.integer(p)
    tol <- as.double(tol)

    leri <- as.character("l")
    nr <- as.integer(0)
    index_bn <- array(as.integer(0), c(p))
    pcoeff <- array(as.double(0), c(p, p, n + 1))
    qcoeff <- array(as.double(0), c(p, m, n + 1))
    vcoeff <- array(as.double(0), c(p, n, n + 1))
    iwork <- array(as.integer(1), c(n + max(m, p)))
    info <- as.integer(0)
    dwork <- array(as.double(1), c(ldwork))
    ldpco1 <- dim(pcoeff)[1]
    ldpco2 <- dim(pcoeff)[2]
    ldqco1 <- dim(qcoeff)[1]
    ldqco2 <- dim(qcoeff)[2]
    ldvco1 <- dim(vcoeff)[1]
    ldvco2 <- dim(vcoeff)[2]
    lda <- dim(a)[1]
    ldb <- dim(b)[1]
    ldc <- dim(c)[1]
    ldd <- dim(d)[1]


    res <- suppressWarnings(.Fortran("TB03AD", LERI = leri, EQUIL = equil, N = n, M = m, P = p, A = a, LDA = lda, B = b, LDB = ldb,
        C = c, LDC = ldc, D = d, LDD = ldd, NR = nr, INDEX_BN = index_bn, PCOEFF = pcoeff, LDPCO1 = ldpco1, LDPCO2 = ldpco2, QCOEFF = qcoeff,
        LDQCO1 = ldqco1, LDQCO2 = ldqco2, VCOEFF = vcoeff, LDVCO1 = ldvco1, LDVCO2 = ldvco2, TOL = tol, IWORK = iwork, DWORK = dwork,
        LDWORK = ldwork, INFO = info))

    return(list(nr = res$NR, index_bn = res$INDEX_BN, pcoeff = res$PCOEFF, qcoeff = res$QCOEFF, vcoeff = res$VCOEFF, info = res$INFO,
        a = res$A, b = res$B, c = res$C))
}

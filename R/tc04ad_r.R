#' tc04ad_r
#'
#' State-space representation for a given left/right polynomial matrix representation
#' @examples 

#'   To find a state-space representation (A,B,C,D) with the same
#'   transfer matrix T(s) as that of a given left or right polynomial
#'   matrix representation, i.e.
#' 
#'      C*inv(sI-A)*B + D = T(s) = inv(P(s))*Q(s) = Q(s)*inv(P(s)).
#' 
#' #'
#' @references \url{http://slicot.org/objects/software/shared/doc/TC04AD.html}
#' @export
tc04ad_r <- function(m, p, index_bn, pcoeff, qcoeff, n, ldwork) {

    # In Parameters
    m <- as.integer(m)
    n <- as.integer(n)
    p <- as.integer(p)
    ldwork <- as.integer(ldwork)

    # Out Parameters
    rcond <- as.double(0)
    a <- array(as.double(0), c(n, n))
    b <- array(as.double(0), c(n, max(m, p)))
    c <- array(as.double(0), c(max(m, p), n))
    d <- array(as.double(0), c(max(m, p), max(m, p)))
    info <- as.integer(0)

    # Hidden Parameters
    leri <- as.character("r")
    ldpco1 <- dim(pcoeff)[1]
    ldpco2 <- dim(pcoeff)[2]
    ldqco1 <- dim(qcoeff)[1]
    ldqco2 <- dim(qcoeff)[2]
    lda <- dim(a)[1]
    ldb <- dim(b)[1]
    ldc <- dim(c)[1]
    ldd <- dim(d)[1]
    iwork <- array(as.integer(1), c(2 * max(m, p)))
    dwork <- array(as.double(1), c(ldwork))

    res <- .Fortran("TC04AD", M = m, P = p, INDEX_BN = index_bn, PCOEFF = pcoeff, QCOEFF = qcoeff, N = n, LDWORK = ldwork, RCOND = rcond, A = a, B = b, C = c, D = d, INFO = info, LERI = leri,
        LDPCO1 = ldpco1, LDPCO2 = ldpco2, LDQCO1 = ldqco1, LDQCO2 = ldqco2, LDA = lda, LDB = ldb, LDC = ldc, LDD = ldd, IWORK = iwork, DWORK = dwork)

    return(list(n = res$N, rcond = res$RCOND, a = res$A, b = res$B, c = res$C, d = res$D, info = res$INFO))
}

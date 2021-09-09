#' tb04ad_c
#'
#' Transfer matrix of a given state-space representation (A,B,C,D)
#' @examples 

#'   To find the transfer matrix T(s) of a given state-space
#'   representation (A,B,C,D). T(s) is expressed as either row or
#'   column polynomial vectors over monic least common denominator
#'   polynomials.
#' 
#' #'
#' @references \url{http://slicot.org/objects/software/shared/doc/TB04AD.html}
#' @export
tb04ad_c <- function(n, m, p, a, b, c, d, tol1, tol2, ldwork) {

    # In Parameters
    ldwork <- as.integer(ldwork)
    m <- as.integer(m)
    n <- as.integer(n)
    p <- as.integer(p)
    tol1 <- as.double(tol1)
    tol2 <- as.double(tol2)

    rowcol <- as.character("c")
    nr <- as.integer(0)
    iwork <- array(as.integer(1), c(n + max(m, p)))
    dwork <- array(as.double(1), c(ldwork))
    info <- as.integer(0)
    dcoeff <- array(as.double(0), c(max(1, m), n + 1))
    index_bn <- array(as.integer(0), c(m))
    ucoeff <- array(as.double(0), c(max(1, m), max(1, p), n + 1))
    lda <- dim(a)[1]
    ldb <- dim(b)[1]
    ldc <- dim(c)[1]
    ldd <- dim(d)[1]
    lddcoe <- dim(dcoeff)[1]
    lduco1 <- dim(ucoeff)[1]
    lduco2 <- dim(ucoeff)[2]


    res <- suppressWarnings(.Fortran("TB04AD", ROWCOL = rowcol, N = n, M = m, P = p, A = a, LDA = lda, B = b, LDB = ldb, C = c,
        LDC = ldc, D = d, LDD = ldd, NR = nr, INDEX_BN = index_bn, DCOEFF = dcoeff, LDDCOE = lddcoe, UCOEFF = ucoeff, LDUCO1 = lduco1,
        LDUCO2 = lduco2, TOL1 = tol1, TOL2 = tol2, IWORK = iwork, DWORK = dwork, LDWORK = ldwork, INFO = info))

    return(list(nr = res$NR, info = res$INFO, a = res$A, b = res$B, c = res$C, dcoeff = res$DCOEFF, index_bn = res$INDEX_BN, ucoeff = res$UCOEFF))
}

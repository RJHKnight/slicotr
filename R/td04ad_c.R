#' td04ad_c
#'
#' Minimal state-space representation for a proper transfer matrix
#' @examples 

#'   To find a minimal state-space representation (A,B,C,D) for a
#'   proper transfer matrix T(s) given as either row or column
#'   polynomial vectors over denominator polynomials, possibly with
#'   uncancelled common terms.
#' 
#' #'
#' @references \url{http://slicot.org/objects/software/shared/doc/TD04AD.html}
#' @export
td04ad_c <- function(m, p, nr, tol, ldwork, dcoeff, index_bn, ucoeff) {

    # In Parameters
    ldwork <- as.integer(ldwork)
    m <- as.integer(m)
    nr <- as.integer(nr)
    p <- as.integer(p)
    tol <- as.double(tol)

    rowcol <- as.character("c")
    iwork <- array(as.integer(1), c(nr + max(m, p)))
    dwork <- array(as.double(1), c(ldwork))
    info <- as.integer(0)
    a <- array(as.double(0), c(max(1, nr), max(1, nr)))
    b <- array(as.double(0), c(max(1, nr), max(m, p)))
    c <- array(as.double(0), c(max(1, max(m, p)), max(1, nr)))
    d <- array(as.double(0), c(max(1, max(m, p)), max(m, p)))
    lda <- dim(a)[1]
    ldb <- dim(b)[1]
    ldc <- dim(c)[1]
    ldd <- dim(d)[1]
    lddcoe <- dim(dcoeff)[1]
    lduco1 <- dim(ucoeff)[1]
    lduco2 <- dim(ucoeff)[2]


    res <- .Fortran("TD04AD", ROWCOL = rowcol, M = m, P = p, NR = nr, TOL = tol, IWORK = iwork, DWORK = dwork, LDWORK = ldwork, INFO = info, A = a, B = b, C = c, D = d, DCOEFF = dcoeff, INDEX_BN = index_bn, UCOEFF = ucoeff, LDA = lda,
        LDB = ldb, LDC = ldc, LDD = ldd, LDDCOE = lddcoe, LDUCO1 = lduco1, LDUCO2 = lduco2)

    return(list(nr = res$NR, info = res$INFO, a = res$A, b = res$B, c = res$C, d = res$D))
}

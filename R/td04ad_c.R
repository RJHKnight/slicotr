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
td04ad_c <- function(m, p, index_bn, dcoeff, ucoeff, nr, tol, ldwork) {

    # In Parameters
    ldwork <- as.integer(ldwork)
    m <- as.integer(m)
    nr <- as.integer(nr)
    p <- as.integer(p)
    tol <- as.double(tol)

    # Out Parameters
    a <- array(as.double(0), c(max(1, nr), max(1, nr)))
    b <- array(as.double(0), c(max(1, nr), max(m, p)))
    c <- array(as.double(0), c(max(1, max(m, p)), max(1, nr)))
    d <- array(as.double(0), c(max(1, max(m, p)), max(m, p)))
    info <- as.integer(0)

    # Hidden Parameters
    rowcol <- as.character("c")
    lddcoe <- dim(dcoeff)[1]
    lduco1 <- dim(ucoeff)[1]
    lduco2 <- dim(ucoeff)[2]
    lda <- dim(a)[1]
    ldb <- dim(b)[1]
    ldc <- dim(c)[1]
    ldd <- dim(d)[1]
    iwork <- array(as.integer(1), c(nr + max(m, p)))
    dwork <- array(as.double(1), c(ldwork))

    res <- .Fortran("TD04AD", M = m, P = p, INDEX_BN = index_bn, DCOEFF = dcoeff, UCOEFF = ucoeff, NR = nr, TOL = tol, LDWORK = ldwork, A = a, B = b, C = c, D = d, INFO = info, ROWCOL = rowcol,
        LDDCOE = lddcoe, LDUCO1 = lduco1, LDUCO2 = lduco2, LDA = lda, LDB = ldb, LDC = ldc, LDD = ldd, IWORK = iwork, DWORK = dwork)

    return(list(nr = res$NR, a = res$A, b = res$B, c = res$C, d = res$D, info = res$INFO))
}

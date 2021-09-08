#' ab09bd
#'
#' Singular Perturbation Approximation based model reduction for stable systems
#' @examples 

#'   To compute a reduced order model (Ar,Br,Cr,Dr) for a stable
#'   original state-space representation (A,B,C,D) by using either the
#'   square-root or the balancing-free square-root Singular
#'   Perturbation Approximation (SPA) model reduction method.
#' 
#' #'
#' @references \url{http://slicot.org/objects/software/shared/doc/AB09BD.html}
#' @export
ab09bd <- function(dico, job, equil, ordsel, n, m, p, nr, a, b, c, d, tol1, tol2, ldwork) {

    # In Parameters
    dico <- as.character(dico)
    equil <- as.character(equil)
    job <- as.character(job)
    ldwork <- as.integer(ldwork)
    m <- as.integer(m)
    n <- as.integer(n)
    nr <- as.integer(nr)
    ordsel <- as.character(ordsel)
    p <- as.integer(p)
    tol1 <- as.double(tol1)
    tol2 <- as.double(tol2)

    iwork <- array(as.integer(1), c(max(m, p)))
    dwork <- array(as.double(1), c(ldwork))
    iwarn <- as.integer(0)
    info <- as.integer(0)
    hsv <- array(as.double(0), c(n))
    lda <- dim(a)[1]
    ldb <- dim(b)[1]
    ldc <- dim(c)[1]
    ldd <- dim(d)[1]


    res <- .Fortran("AB09BD", DICO = dico, JOB = job, EQUIL = equil, ORDSEL = ordsel, N = n, M = m, P = p, NR = nr, A = a, LDA = lda, B = b, LDB = ldb, C = c, LDC = ldc, D = d, LDD = ldd, HSV = hsv, TOL1 = tol1, TOL2 = tol2, IWORK = iwork,
        DWORK = dwork, LDWORK = ldwork, IWARN = iwarn, INFO = info)

    return(list(nr = res$NR, iwarn = res$IWARN, info = res$INFO, a = res$A, b = res$B, c = res$C, d = res$D, hsv = res$HSV))
}

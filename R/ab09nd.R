#' ab09nd
#'
#' Singular perturbation approximation based model reduction for the stable part of a system
#' @examples 

#'   To compute a reduced order model (Ar,Br,Cr,Dr) for an original
#'   state-space representation (A,B,C,D) by using either the
#'   square-root or the balancing-free square-root Singular
#'   Perturbation Approximation (SPA) model reduction method for the
#'   ALPHA-stable part of the system.
#' 
#' #'
#' @references \url{http://slicot.org/objects/software/shared/doc/AB09ND.html}
#' @export
ab09nd <- function(dico, job, equil, ordsel, n, m, p, nr, alpha, a, b, c, d, tol1, tol2, ldwork) {

    # In Parameters
    alpha <- as.double(alpha)
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

    ns <- as.integer(0)
    iwork <- array(as.integer(1), c(max(m, p)))
    dwork <- array(as.double(1), c(ldwork))
    iwarn <- as.integer(0)
    info <- as.integer(0)
    hsv <- array(as.double(0), c(n))
    lda <- dim(a)[1]
    ldb <- dim(b)[1]
    ldc <- dim(c)[1]
    ldd <- dim(d)[1]


    res <- suppressWarnings(.Fortran("AB09ND", DICO = dico, JOB = job, EQUIL = equil, ORDSEL = ordsel, N = n, M = m, P = p, NR = nr,
        ALPHA = alpha, A = a, LDA = lda, B = b, LDB = ldb, C = c, LDC = ldc, D = d, LDD = ldd, NS = ns, HSV = hsv, TOL1 = tol1,
        TOL2 = tol2, IWORK = iwork, DWORK = dwork, LDWORK = ldwork, IWARN = iwarn, INFO = info))

    return(list(nr = res$NR, ns = res$NS, iwarn = res$IWARN, info = res$INFO, a = res$A, b = res$B, c = res$C, d = res$D, hsv = res$HSV))
}

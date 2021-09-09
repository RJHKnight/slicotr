#' ab09ad
#'
#' Balance & Truncate model reduction for stable systems
#' @examples 

#'   To compute a reduced order model (Ar,Br,Cr) for a stable original
#'   state-space representation (A,B,C) by using either the square-root
#'   or the balancing-free square-root Balance & Truncate (B & T)
#'   model reduction method.
#' 
#' #'
#' @references \url{http://slicot.org/objects/software/shared/doc/AB09AD.html}
#' @export
ab09ad <- function(dico, job, equil, ordsel, n, m, p, nr, a, b, c, tol, ldwork) {

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
    tol <- as.double(tol)

    iwork <- array(as.integer(1), c(max(m, p)))
    dwork <- array(as.double(1), c(ldwork))
    iwarn <- as.integer(0)
    info <- as.integer(0)
    hsv <- array(as.double(0), c(n))
    lda <- dim(a)[1]
    ldb <- dim(b)[1]
    ldc <- dim(c)[1]


    res <- suppressWarnings(.Fortran("AB09AD", DICO = dico, JOB = job, EQUIL = equil, ORDSEL = ordsel, N = n, M = m, P = p, NR = nr,
        A = a, LDA = lda, B = b, LDB = ldb, C = c, LDC = ldc, HSV = hsv, TOL = tol, IWORK = iwork, DWORK = dwork, LDWORK = ldwork,
        IWARN = iwarn, INFO = info))

    return(list(nr = res$NR, iwarn = res$IWARN, info = res$INFO, a = res$A, b = res$B, c = res$C, hsv = res$HSV))
}

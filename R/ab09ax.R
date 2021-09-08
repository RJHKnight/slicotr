#' ab09ax
#'
#' Balance & Truncate model reduction for stable systems with state matrix in real Schur canonical form
#' @examples 

#'   To compute a reduced order model (Ar,Br,Cr) for a stable original
#'   state-space representation (A,B,C) by using either the square-root
#'   or the balancing-free square-root Balance & Truncate model
#'   reduction method. The state dynamics matrix A of the original
#'   system is an upper quasi-triangular matrix in real Schur canonical
#'   form. The matrices of the reduced order system are computed using
#'   the truncation formulas:
#' 
#'        Ar = TI * A * T ,  Br = TI * B ,  Cr = C * T .
#' 
#' #'
#' @references \url{http://slicot.org/objects/software/shared/doc/AB09AX.html}
#' @export
ab09ax <- function(dico, job, ordsel, n, m, p, nr, tol, ldwork, a, b, c) {

    # In Parameters
    dico <- as.character(dico)
    job <- as.character(job)
    ldwork <- as.integer(ldwork)
    m <- as.integer(m)
    n <- as.integer(n)
    nr <- as.integer(nr)
    ordsel <- as.character(ordsel)
    p <- as.integer(p)
    tol <- as.double(tol)

    t <- array(as.double(0), c(n, n))
    ti <- array(as.double(0), c(n, n))
    iwork <- array(as.integer(1), c(max(m, p)))
    dwork <- array(as.double(1), c(ldwork))
    iwarn <- as.integer(0)
    info <- as.integer(0)
    hsv <- array(as.double(0), c(n))
    ldt <- dim(t)[1]
    ldti <- dim(ti)[1]
    lda <- dim(a)[1]
    ldb <- dim(b)[1]
    ldc <- dim(c)[1]


    res <- .Fortran("AB09AX", DICO = dico, JOB = job, ORDSEL = ordsel, N = n, M = m, P = p, NR = nr, T = t, TI = ti, TOL = tol, IWORK = iwork, DWORK = dwork, LDWORK = ldwork, IWARN = iwarn, INFO = info, A = a, B = b, C = c, HSV = hsv,
        LDT = ldt, LDTI = ldti, LDA = lda, LDB = ldb, LDC = ldc)

    return(list(nr = res$NR, t = res$T, ti = res$TI, iwarn = res$IWARN, info = res$INFO, a = res$A, b = res$B, c = res$C, hsv = res$HSV))
}

#' ag08bd
#'
#' Zeros and Kronecker structure of a descriptor system pencil
#' @examples 

#'   To extract from the system pencil
#' 
#'                     ( A-lambda*E B )
#'         S(lambda) = (              )
#'                     (      C     D )
#' 
#'   a regular pencil Af-lambda*Ef which has the finite Smith zeros of
#'   S(lambda) as generalized eigenvalues. The routine also computes
#'   the orders of the infinite Smith zeros and determines the singular
#'   and infinite Kronecker structure of system pencil, i.e., the right
#'   and left Kronecker indices, and the multiplicities of infinite
#'   eigenvalues.
#' 
#' #'
#' @references \url{http://slicot.org/objects/software/shared/doc/AG08BD.html}
#' @export
ag08bd <- function(equil, l, n, m, p, tol, ldwork, a, b, c, d, e) {

    # In Parameters
    equil <- as.character(equil)
    l <- as.integer(l)
    ldwork <- as.integer(ldwork)
    m <- as.integer(m)
    n <- as.integer(n)
    p <- as.integer(p)
    tol <- as.double(tol)

    nfz <- as.integer(0)
    nrank <- as.integer(0)
    niz <- as.integer(0)
    dinfz <- as.integer(0)
    nkror <- as.integer(0)
    ninfe <- as.integer(0)
    nkrol <- as.integer(0)
    info <- as.integer(0)
    dwork <- array(as.double(1), c(ldwork))
    infe <- array(as.integer(0), c(1 + min(l + p, n + m)))
    infz <- array(as.integer(0), c(n + 1))
    iwork <- array(as.integer(1), c(ldwork))
    kronl <- array(as.integer(0), c(l + p + 1))
    kronr <- array(as.integer(0), c(n + m + 1))
    lda <- max(dim(a)[1], 1)
    ldb <- max(dim(b)[1], 1)
    ldc <- max(dim(c)[1], 1)
    ldd <- max(dim(d)[1], 1)
    lde <- max(dim(e)[1], 1)


    res <- .Fortran("AG08BD", EQUIL = equil, L = l, N = n, M = m, P = p, NFZ = nfz, NRANK = nrank, NIZ = niz, DINFZ = dinfz, NKROR = nkror, NINFE = ninfe, NKROL = nkrol, TOL = tol, LDWORK = ldwork, INFO = info, A = a, B = b, C = c, D = d,
        DWORK = dwork, E = e, INFE = infe, INFZ = infz, IWORK = iwork, KRONL = kronl, KRONR = kronr, LDA = lda, LDB = ldb, LDC = ldc, LDD = ldd, LDE = lde)

    return(list(nfz = res$NFZ, nrank = res$NRANK, niz = res$NIZ, dinfz = res$DINFZ, nkror = res$NKROR, ninfe = res$NINFE, nkrol = res$NKROL, info = res$INFO, a = res$A, e = res$E, infe = res$INFE, infz = res$INFZ, kronl = res$KRONL,
        kronr = res$KRONR))
}

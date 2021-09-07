#' ab08nd
#'
#' Construction of a regular pencil for a given system such that its generalized eigenvalues are invariant zeros of the system
#' @examples 

#'   To construct for a linear multivariable system described by a
#'   state-space model (A,B,C,D) a regular pencil (A - lambda*B ) which
#'                                                  f          f
#'   has the invariant zeros of the system as generalized eigenvalues.
#'   The routine also computes the orders of the infinite zeros and the
#'   right and left Kronecker indices of the system (A,B,C,D).
#' 
#' #'
#' @references \url{http://slicot.org/objects/software/shared/doc/AB08ND.html}
#' @export
ab08nd <- function(equil, n, m, p, a, b, c, d, tol, ldwork) {

    # In Parameters
    equil <- as.character(equil)
    ldwork <- as.integer(ldwork)
    m <- as.integer(m)
    n <- as.integer(n)
    p <- as.integer(p)
    tol <- as.double(tol)

    # Out Parameters
    nu <- as.integer(0)
    rank_bn <- as.integer(0)
    dinfz <- as.integer(0)
    nkror <- as.integer(0)
    nkrol <- as.integer(0)
    infz <- array(as.integer(0), c(n))
    kronr <- array(as.integer(0), c(max(n, m) + 1))
    kronl <- array(as.integer(0), c(max(n, p) + 1))
    af <- array(as.double(0), c(max(1, n + m), n + min(p, m)))
    bf <- array(as.double(0), c(max(1, n + p), n + m))
    info <- as.integer(0)

    # Hidden Parameters
    lda <- dim(a)[1]
    ldb <- dim(b)[1]
    ldc <- dim(c)[1]
    ldd <- dim(d)[1]
    ldaf <- dim(af)[1]
    ldbf <- dim(bf)[1]
    iwork <- array(as.integer(1), c(max(m, p)))
    dwork <- array(as.double(1), c(ldwork))

    res <- .Fortran("AB08ND", EQUIL = equil, N = n, M = m, P = p, A = a, B = b, C = c, D = d, TOL = tol, LDWORK = ldwork, NU = nu, RANK_BN = rank_bn, DINFZ = dinfz, NKROR = nkror, NKROL = nkrol,
        INFZ = infz, KRONR = kronr, KRONL = kronl, AF = af, BF = bf, INFO = info, LDA = lda, LDB = ldb, LDC = ldc, LDD = ldd, LDAF = ldaf, LDBF = ldbf, IWORK = iwork, DWORK = dwork)

    return(list(nu = res$NU, rank_bn = res$RANK_BN, dinfz = res$DINFZ, nkror = res$NKROR, nkrol = res$NKROL, infz = res$INFZ, kronr = res$KRONR, kronl = res$KRONL, af = res$AF, bf = res$BF,
        info = res$INFO))
}

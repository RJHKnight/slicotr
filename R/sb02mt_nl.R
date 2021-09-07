#' sb02mt_nl
#'
#' Conversion of optimal problems with coupling weighting terms to standard problems
#' @examples 

#'   To compute the following matrices
#' 
#'              -1
#'       G = B*R  *B',
#' 
#'       -          -1
#'       A = A - B*R  *L',
#' 
#'       -          -1
#'       Q = Q - L*R  *L',
#' 
#'   where A, B, Q, R, L, and G are N-by-N, N-by-M, N-by-N, M-by-M,
#'   N-by-M, and N-by-N matrices, respectively, with Q, R and G
#'   symmetric matrices.
#' 
#'   When R is well-conditioned with respect to inversion, standard
#'   algorithms for solving linear-quadratic optimization problems will
#'   then also solve optimization problems with coupling weighting
#'   matrix L. Moreover, a gain in efficiency is possible using matrix
#'   G in the deflating subspace algorithms (see SLICOT Library routine
#'   SB02OD) or in the Newton's algorithms (see SLICOT Library routine
#'   SG02CD).
#' 
#' #'
#' @references \url{http://slicot.org/objects/software/shared/doc/SB02MT.html}
#' @export
sb02mt_nl <- function(uplo, n, m, a, b, q, r, l, ldwork) {

    # In Parameters
    ldwork <- as.integer(ldwork)
    m <- as.integer(m)
    n <- as.integer(n)
    uplo <- as.character(uplo)

    # Out Parameters
    ipiv <- array(as.integer(0), c(m))
    oufact <- as.integer(0)
    g <- array(as.double(0), c(n, n))
    info <- as.integer(0)

    # Hidden Parameters
    jobg <- as.character("g")
    jobl <- as.character("n")
    fact <- as.character("n")
    lda <- dim(a)[1]
    ldb <- dim(b)[1]
    ldq <- dim(q)[1]
    ldr <- dim(r)[1]
    ldl <- dim(l)[1]
    ldg <- dim(g)[1]
    iwork <- array(as.integer(1), c(m))
    dwork <- array(as.double(1), c(ldwork))

    res <- .Fortran("SB02MT", UPLO = uplo, N = n, M = m, A = a, B = b, Q = q, R = r, L = l, LDWORK = ldwork, IPIV = ipiv, OUFACT = oufact, G = g, INFO = info, JOBG = jobg, JOBL = jobl,
        FACT = fact, LDA = lda, LDB = ldb, LDQ = ldq, LDR = ldr, LDL = ldl, LDG = ldg, IWORK = iwork, DWORK = dwork)

    return(list(a = res$A, b = res$B, q = res$Q, r = res$R, l = res$L, ipiv = res$IPIV, oufact = res$OUFACT, g = res$G, info = res$INFO))
}

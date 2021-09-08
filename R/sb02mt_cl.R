#' sb02mt_cl
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
sb02mt_cl <- function(uplo, n, m, a, b, l, q, r) {

    # In Parameters
    m <- as.integer(m)
    n <- as.integer(n)
    uplo <- as.character(uplo)

    jobg <- as.character("g")
    jobl <- as.character("n")
    fact <- as.character("c")
    ipiv <- array(as.integer(0), c(1))
    oufact <- as.integer(0)
    iwork <- array(as.integer(1), c(m))
    ldwork <- as.integer(1)
    info <- as.integer(0)
    dwork <- array(as.double(1), c(ldwork))
    g <- array(as.double(0), c(n, n))
    lda <- dim(a)[1]
    ldb <- dim(b)[1]
    ldg <- dim(g)[1]
    ldl <- dim(l)[1]
    ldq <- dim(q)[1]
    ldr <- dim(r)[1]


    res <- .Fortran("SB02MT", JOBG = jobg, JOBL = jobl, FACT = fact, UPLO = uplo, N = n, M = m, IPIV = ipiv, OUFACT = oufact, IWORK = iwork, LDWORK = ldwork, INFO = info, A = a, B = b, DWORK = dwork, G = g, L = l, Q = q, R = r, LDA = lda,
        LDB = ldb, LDG = ldg, LDL = ldl, LDQ = ldq, LDR = ldr)

    return(list(ipiv = res$IPIV, oufact = res$OUFACT, info = res$INFO, a = res$A, b = res$B, g = res$G, l = res$L, q = res$Q, r = res$R))
}

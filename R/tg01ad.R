#' tg01ad
#'
#' Balancing the matrices of the system pencil corresponding to a descriptor triple (A-lambda E,B,C)
#' @examples 

#'   To balance the matrices of the system pencil
#' 
#'           S =  ( A  B ) - lambda ( E  0 ) :=  Q - lambda Z,
#'                ( C  0 )          ( 0  0 )
#' 
#'   corresponding to the descriptor triple (A-lambda E,B,C),
#'   by balancing. This involves diagonal similarity transformations
#'   (Dl*A*Dr - lambda Dl*E*Dr, Dl*B, C*Dr) applied to the system
#'   (A-lambda E,B,C) to make the rows and columns of system pencil
#'   matrices
#' 
#'                diag(Dl,I) * S * diag(Dr,I)
#' 
#'   as close in norm as possible. Balancing may reduce the 1-norms
#'   of the matrices of the system pencil S.
#' 
#'   The balancing can be performed optionally on the following
#'   particular system pencils
#' 
#'            S = A-lambda E,
#' 
#'            S = ( A-lambda E  B ),    or
#' 
#'            S = ( A-lambda E ).
#'                (     C      )
#' 
#' #'
#' @references \url{http://slicot.org/objects/software/shared/doc/TG01AD.html}
#' @export
tg01ad <- function(job, l, n, m, p, thresh, a, e, b, c) {

    # In Parameters
    job <- as.character(job)
    l <- as.integer(l)
    m <- as.integer(m)
    n <- as.integer(n)
    p <- as.integer(p)
    thresh <- as.double(thresh)

    lscale <- array(as.double(0), c(l))
    rscale <- array(as.double(0), c(n))
    dwork <- array(as.double(1), c(3 * (l + n)))
    info <- as.integer(0)
    lda <- max(dim(a)[1], 1)
    ldb <- max(dim(b)[1], 1)
    ldc <- max(dim(c)[1], 1)
    lde <- max(dim(e)[1], 1)


    res <- suppressWarnings(.Fortran("TG01AD", JOB = job, L = l, N = n, M = m, P = p, THRESH = thresh, A = a, LDA = lda, E = e,
        LDE = lde, B = b, LDB = ldb, C = c, LDC = ldc, LSCALE = lscale, RSCALE = rscale, DWORK = dwork, INFO = info))

    return(list(lscale = res$LSCALE, rscale = res$RSCALE, info = res$INFO, a = res$A, b = res$B, c = res$C, e = res$E))
}

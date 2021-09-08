#' ab01nd
#'
#' Controllable realization for multi-input systems using orthogonal state and input transformations
#' @examples 

#'   To find a controllable realization for the linear time-invariant
#'   multi-input system
#' 
#'           dX/dt = A * X + B * U,
#' 
#'   where A and B are N-by-N and N-by-M matrices, respectively,
#'   which are reduced by this routine to orthogonal canonical form
#'   using (and optionally accumulating) orthogonal similarity
#'   transformations.  Specifically, the pair (A, B) is reduced to
#'   the pair (Ac, Bc),  Ac = Z' * A * Z,  Bc = Z' * B,  given by
#' 
#'           [ Acont     *    ]         [ Bcont ]
#'      Ac = [                ],   Bc = [       ],
#'           [   0    Auncont ]         [   0   ]
#' 
#'      and
#' 
#'              [ A11 A12  . . .  A1,p-1 A1p ]         [ B1 ]
#'              [ A21 A22  . . .  A2,p-1 A2p ]         [ 0  ]
#'              [  0  A32  . . .  A3,p-1 A3p ]         [ 0  ]
#'      Acont = [  .   .   . . .    .     .  ],   Bc = [ .  ],
#'              [  .   .     . .    .     .  ]         [ .  ]
#'              [  .   .       .    .     .  ]         [ .  ]
#'              [  0   0   . . .  Ap,p-1 App ]         [ 0  ]
#' 
#'   where the blocks  B1, A21, ..., Ap,p-1  have full row ranks and
#'   p is the controllability index of the pair.  The size of the
#'   block  Auncont is equal to the dimension of the uncontrollable
#'   subspace of the pair (A, B).
#' 
#' #'
#' @references \url{http://slicot.org/objects/software/shared/doc/AB01ND.html}
#' @export
ab01nd <- function(jobz, n, m, tol, ldwork, a, b) {

    # In Parameters
    jobz <- as.character(jobz)
    ldwork <- as.integer(ldwork)
    m <- as.integer(m)
    n <- as.integer(n)
    tol <- as.double(tol)

    ncont <- as.integer(0)
    indcon <- as.integer(0)
    info <- as.integer(0)
    dwork <- array(as.double(1), c(ldwork))
    iwork <- array(as.integer(1), c(m))
    ldz <- as.integer(ifelse(jobz == "n", 1, n))
    nblk <- array(as.integer(0), c(n))
    tau <- array(as.double(0), c(n))
    z <- array(as.double(0), c(ldz, n))
    lda <- dim(a)[1]
    ldb <- dim(b)[1]


    res <- .Fortran("AB01ND", JOBZ = jobz, N = n, M = m, NCONT = ncont, INDCON = indcon, TOL = tol, LDWORK = ldwork, INFO = info, A = a, B = b, DWORK = dwork, IWORK = iwork, LDZ = ldz, NBLK = nblk, TAU = tau, Z = z, LDA = lda, LDB = ldb)

    return(list(ncont = res$NCONT, indcon = res$INDCON, info = res$INFO, a = res$A, b = res$B, nblk = res$NBLK, tau = res$TAU, z = res$Z))
}

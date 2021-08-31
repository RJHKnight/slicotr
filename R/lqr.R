lqr <- function(A, B, Q, R, fact = "N", jobl = "Z", uplo = "U")
{
  n_states <- nrow(B)
  n_inputs <- ncol(B)

  ldwork = as.integer(max(2, 3*n_inputs, n_states*n_inputs))

  # n,m,B,R,uplo=uplo,ldwork=ldwork)

  #      SUBROUTINE SB02MT( JOBG, JOBL, FACT, UPLO, N, M, A, LDA, B, LDB,
  #                   Q, LDQ, R, LDR, L, LDL, IPIV, OUFACT, G, LDG,
  #                   IWORK, DWORK, LDWORK, INFO )

  G <- matrix(0.0, nrow = n_states, ncol = n_inputs)

  N <- matrix(0.0, nrow = nrow(Q), ncol = ncol(R))

  res <- .Fortran("SB02MT",
                  "G",
                  "N",
                  "N",
                  "U",
                  n_states,
                  n_inputs,
                  as.single(A),
                  nrow(A),
                  as.single(B),
                  nrow(B),
                  as.single(Q),
                  nrow(Q),
                  matrix(R),
                  as.integer(1),
                  as.single(N),
                  nrow(N),
                  n_inputs,
                  as.integer(0),
                  as.single(G),
                  nrow(G),
                  rep(integer(0), n_inputs),
                  rep(0, ldwork),
                  ldwork,
                  as.integer(0),
                  PACKAGE = "slicotr")

  return (res)

}

tc01od_r <- function(m, p, indlim, pcoeff, qcoeff) {

    # In Parameters
    indlim <- as.integer(indlim)
    m <- as.integer(m)
    p <- as.integer(p)

    # Out Parameters
    info <- as.integer(0)

    # Hidden Parameters
    leri <- "R"
    ldpco1 <- dim(pcoeff)[1]
    ldpco2 <- dim(pcoeff)[2]
    ldqco1 <- dim(qcoeff)[1]
    ldqco2 <- dim(qcoeff)[2]

    res <- .Fortran("TC01OD", LERI = leri, M = m, P = p, INDLIM = indlim, PCOEFF = pcoeff, LDPCO1 = ldpco1, LDPCO2 = ldpco2, QCOEFF = qcoeff, LDQCO1 = ldqco1, LDQCO2 = ldqco2, INFO = info)

    return(list(info = res$INFO))
}

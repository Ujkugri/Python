##
## Exported symobls in package `parallel`
##

## Exported package methods

stopCluster <- function (cl = NULL) 
{
    cl <- defaultCluster(cl)
    if (identical(cl, get("default", envir = .reg))) 
        assign("default", NULL, envir = .reg)
    UseMethod("stopCluster")
}


mcMap <- function (f, ...) 
Map(f, ...)


clusterExport <- function (cl = NULL, varlist, envir = .GlobalEnv) 
{
    for (name in varlist) {
        clusterCall(cl, gets, name, get(name, envir = envir))
    }
}


makePSOCKcluster <- function (names, ...) 
{
    if (is.numeric(names)) {
        names <- as.integer(names[1L])
        if (is.na(names) || names < 1L) 
            stop("numeric 'names' must be >= 1")
        names <- rep("localhost", names)
    }
    .check_ncores(length(names))
    options <- addClusterOptions(defaultClusterOptions, list(...))
    cl <- vector("list", length(names))
    for (i in seq_along(cl)) cl[[i]] <- newPSOCKnode(names[[i]], 
        options = options, rank = i)
    class(cl) <- c("SOCKcluster", "cluster")
    cl
}


parSapplyLB <- function (cl = NULL, X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE, 
    chunk.size = NULL) 
{
    FUN <- match.fun(FUN)
    answer <- parLapplyLB(cl = cl, X = as.list(X), fun = FUN, 
        ..., chunk.size = chunk.size)
    if (USE.NAMES && is.character(X) && is.null(names(answer))) 
        names(answer) <- X
    if (!isFALSE(simplify) && length(answer)) 
        simplify2array(answer, higher = (simplify == "array"))
    else answer
}


clusterApplyLB <- function (cl = NULL, x, fun, ...) 
{
    argfun <- function(i) c(list(x[[i]]), list(...))
    dynamicClusterApply(cl, fun, length(x), argfun)
}


mclapply <- function (X, FUN, ..., mc.preschedule = TRUE, mc.set.seed = TRUE, 
    mc.silent = FALSE, mc.cores = 1L, mc.cleanup = TRUE, mc.allow.recursive = TRUE, 
    affinity.list = NULL) 
{
    cores <- as.integer(mc.cores)
    if (cores < 1L) 
        stop("'mc.cores' must be >= 1")
    if (cores > 1L) 
        stop("'mc.cores' > 1 is not supported on Windows")
    lapply(X, FUN, ...)
}


clusterSetRNGStream <- function (cl = NULL, iseed = NULL) 
{
    cl <- defaultCluster(cl)
    oldseed <- if (exists(".Random.seed", envir = .GlobalEnv, 
        inherits = FALSE)) 
        get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    else NULL
    RNGkind("L'Ecuyer-CMRG")
    if (!is.null(iseed)) 
        set.seed(iseed)
    nc <- length(cl)
    seeds <- vector("list", nc)
    seeds[[1L]] <- .Random.seed
    for (i in seq_len(nc - 1L)) seeds[[i + 1L]] <- nextRNGStream(seeds[[i]])
    if (!is.null(oldseed)) 
        assign(".Random.seed", oldseed, envir = .GlobalEnv)
    else rm(.Random.seed, envir = .GlobalEnv)
    for (i in seq_along(cl)) {
        expr <- substitute(assign(".Random.seed", seed, envir = .GlobalEnv), 
            list(seed = seeds[[i]]))
        sendCall(cl[[i]], eval, list(expr))
    }
    checkForRemoteErrors(lapply(cl, recvResult))
    invisible()
}


detectCores <- function (all.tests = FALSE, logical = TRUE) 
{
    res <- .Call(C_ncpus, FALSE)
    res[if (logical) 
        2L
    else 1L]
}


parSapply <- function (cl = NULL, X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE, 
    chunk.size = NULL) 
{
    FUN <- match.fun(FUN)
    answer <- parLapply(cl = cl, X = as.list(X), fun = FUN, ..., 
        chunk.size = chunk.size)
    if (USE.NAMES && is.character(X) && is.null(names(answer))) 
        names(answer) <- X
    if (!isFALSE(simplify) && length(answer)) 
        simplify2array(answer, higher = (simplify == "array"))
    else answer
}


parApply <- function (cl = NULL, X, MARGIN, FUN, ..., chunk.size = NULL) 
{
    cl <- defaultCluster(cl)
    FUN <- match.fun(FUN)
    dl <- length(dim(X))
    if (!dl) 
        stop("dim(X) must have a positive length")
    if (is.object(X)) 
        X <- if (dl == 2L) 
            as.matrix(X)
        else as.array(X)
    d <- dim(X)
    dn <- dimnames(X)
    ds <- seq_len(dl)
    if (is.character(MARGIN)) {
        if (is.null(dnn <- names(dn))) 
            stop("'X' must have named dimnames")
        MARGIN <- match(MARGIN, dnn)
        if (anyNA(MARGIN)) 
            stop("not all elements of 'MARGIN' are names of dimensions")
    }
    s.call <- ds[-MARGIN]
    s.ans <- ds[MARGIN]
    d.call <- d[-MARGIN]
    d.ans <- d[MARGIN]
    dn.call <- dn[-MARGIN]
    dn.ans <- dn[MARGIN]
    d2 <- prod(d.ans)
    if (d2 == 0L) {
        newX <- array(vector(typeof(X), 1L), dim = c(prod(d.call), 
            1L))
        ans <- FUN(if (length(d.call) < 2L) 
            newX[, 1]
        else array(newX[, 1L], d.call, dn.call), ...)
        return(if (is.null(ans)) ans else if (length(d.ans) < 
            2L) ans[1L][-1L] else array(ans, d.ans, dn.ans))
    }
    newX <- aperm(X, c(s.call, s.ans))
    dim(newX) <- c(prod(d.call), d2)
    ans <- vector("list", d2)
    arglist <- if (length(d.call) < 2L) {
        if (length(dn.call)) 
            dimnames(newX) <- c(dn.call, list(NULL))
        lapply(seq_len(d2), function(i) newX[, i])
    }
    else lapply(seq_len(d2), function(i) array(newX[, i], d.call, 
        dn.call))
    ans <- parLapply(cl = cl, X = arglist, fun = FUN, ..., chunk.size = chunk.size)
    ans.list <- is.recursive(ans[[1L]])
    l.ans <- length(ans[[1L]])
    ans.names <- names(ans[[1L]])
    if (!ans.list) 
        ans.list <- any(lengths(ans) != l.ans)
    if (!ans.list && length(ans.names)) {
        all.same <- vapply(ans, function(x) identical(names(x), 
            ans.names), NA)
        if (!all(all.same)) 
            ans.names <- NULL
    }
    len.a <- if (ans.list) 
        d2
    else length(ans <- unlist(ans, recursive = FALSE))
    if (length(MARGIN) == 1L && len.a == d2) {
        names(ans) <- if (length(dn.ans[[1L]])) 
            dn.ans[[1L]]
        return(ans)
    }
    if (len.a == d2) 
        return(array(ans, d.ans, dn.ans))
    if (len.a && len.a%%d2 == 0L) {
        if (is.null(dn.ans)) 
            dn.ans <- vector(mode = "list", length(d.ans))
        dn.ans <- c(list(ans.names), dn.ans)
        return(array(ans, c(len.a%/%d2, d.ans), if (!all(vapply(dn.ans, 
            is.null, NA))) dn.ans))
    }
    return(ans)
}


clusterMap <- function (cl = NULL, fun, ..., MoreArgs = NULL, RECYCLE = TRUE, 
    SIMPLIFY = FALSE, USE.NAMES = TRUE, .scheduling = c("static", 
        "dynamic")) 
{
    cl <- defaultCluster(cl)
    args <- list(...)
    if (length(args) == 0) 
        stop("need at least one argument")
    .scheduling <- match.arg(.scheduling)
    n <- lengths(args)
    if (RECYCLE) {
        vlen <- max(n)
        if (vlen && min(n) == 0L) 
            stop("zero-length inputs cannot be mixed with those of non-zero length")
        if (!all(n == vlen)) 
            for (i in seq_along(args)) args[[i]] <- rep(args[[i]], 
                length.out = vlen)
    }
    else vlen <- min(n)
    argfun <- function(i) c(lapply(args, function(x) x[[i]]), 
        MoreArgs)
    answer <- if (.scheduling == "dynamic") 
        dynamicClusterApply(cl, fun, vlen, argfun)
    else staticClusterApply(cl, fun, vlen, argfun)
    if (USE.NAMES && length(args)) {
        if (is.null(names1 <- names(args[[1L]])) && is.character(args[[1L]])) 
            names(answer) <- args[[1L]]
        else if (!is.null(names1)) 
            names(answer) <- names1
    }
    if (!isFALSE(SIMPLIFY) && length(answer)) 
        simplify2array(answer, higher = (SIMPLIFY == "array"))
    else answer
}


makeCluster <- function (spec, type = getClusterOption("type"), ...) 
{
    switch(type, PSOCK = makePSOCKcluster(names = spec, ...), 
        FORK = makeForkCluster(nnodes = spec, ...), SOCK = snow::makeSOCKcluster(names = spec, 
            ...), MPI = snow::makeMPIcluster(count = spec, ...), 
        NWS = snow::makeNWScluster(names = spec, ...), stop("unknown cluster type"))
}


clusterApply <- function (cl = NULL, x, fun, ...) 
{
    argfun <- function(i) c(list(x[[i]]), list(...))
    staticClusterApply(cl, fun, length(x), argfun)
}


nextRNGStream <- function (seed) 
{
    if (!is.integer(seed) || seed[1L]%%100L != 7L) 
        stop(gettextf("invalid value of %s", "'seed'"), domain = NA)
    .Call(C_nextStream, seed)
}


setDefaultCluster <- function (cl = NULL) 
{
    if (!is.null(cl)) 
        checkCluster(cl)
    assign("default", cl, envir = .reg)
}


clusterEvalQ <- function (cl = NULL, expr) 
clusterCall(cl, eval, substitute(expr), env = .GlobalEnv)


clusterCall <- function (cl = NULL, fun, ...) 
{
    cl <- defaultCluster(cl)
    for (i in seq_along(cl)) sendCall(cl[[i]], fun, list(...))
    checkForRemoteErrors(lapply(cl, recvResult))
}


mcmapply <- function (FUN, ..., MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE, 
    mc.preschedule = TRUE, mc.set.seed = TRUE, mc.silent = FALSE, 
    mc.cores = 1L, mc.cleanup = TRUE, affinity.list = NULL) 
{
    cores <- as.integer(mc.cores)
    if (cores < 1L) 
        stop("'mc.cores' must be >= 1")
    if (cores > 1L) 
        stop("'mc.cores' > 1 is not supported on Windows")
    mapply(FUN = FUN, ..., MoreArgs = MoreArgs, SIMPLIFY = SIMPLIFY, 
        USE.NAMES = USE.NAMES)
}


splitIndices <- function (nx, ncl) 
{
    i <- seq_len(nx)
    if (ncl == 0L) 
        list()
    else if (ncl == 1L || nx == 1L) 
        list(i)
    else {
        fuzz <- min((nx - 1L)/1000, 0.4 * nx/ncl)
        breaks <- seq(1 - fuzz, nx + fuzz, length.out = ncl + 
            1L)
        structure(split(i, cut(i, breaks)), names = NULL)
    }
}


pvec <- function (v, FUN, ..., mc.set.seed = TRUE, mc.silent = FALSE, 
    mc.cores = 1L, mc.cleanup = TRUE) 
{
    if (!is.vector(v)) 
        stop("'v' must be a vector")
    cores <- as.integer(mc.cores)
    if (cores < 1L) 
        stop("'mc.cores' must be >= 1")
    if (cores > 1L) 
        stop("'mc.cores' > 1 is not supported on Windows")
    FUN(v, ...)
}


nextRNGSubStream <- function (seed) 
{
    if (!is.integer(seed) || seed[1L]%%100L != 7L) 
        stop(gettextf("invalid value of %s", "'seed'"), domain = NA)
    .Call(C_nextSubStream, seed)
}


parCapply <- function (cl = NULL, x, FUN, ..., chunk.size = NULL) 
{
    cl <- defaultCluster(cl)
    nchunks <- staticNChunks(ncol(x), length(cl), chunk.size)
    do.call(c, clusterApply(cl = cl, x = splitCols(x, nchunks), 
        fun = apply, MARGIN = 2L, FUN = FUN, ...), quote = TRUE)
}


parRapply <- function (cl = NULL, x, FUN, ..., chunk.size = NULL) 
{
    cl <- defaultCluster(cl)
    nchunks <- staticNChunks(nrow(x), length(cl), chunk.size)
    do.call(c, clusterApply(cl = cl, x = splitRows(x, nchunks), 
        fun = apply, MARGIN = 1L, FUN = FUN, ...), quote = TRUE)
}


getDefaultCluster <- function () 
get("default", envir = .reg)


parLapplyLB <- function (cl = NULL, X, fun, ..., chunk.size = NULL) 
{
    cl <- defaultCluster(cl)
    nchunks <- dynamicNChunks(length(X), length(cl), chunk.size)
    do.call(c, clusterApplyLB(cl = cl, x = splitList(X, nchunks), 
        fun = lapply, FUN = fun, ...), quote = TRUE)
}


parLapply <- function (cl = NULL, X, fun, ..., chunk.size = NULL) 
{
    cl <- defaultCluster(cl)
    nchunks <- staticNChunks(length(X), length(cl), chunk.size)
    do.call(c, clusterApply(cl = cl, x = splitList(X, nchunks), 
        fun = lapply, FUN = fun, ...), quote = TRUE)
}


clusterSplit <- function (cl = NULL, seq) 
{
    cl <- defaultCluster(cl)
    lapply(splitIndices(length(seq), length(cl)), function(i) seq[i])
}


makeForkCluster <- function (nnodes = getOption("mc.cores", 2L), ...) 
stop("fork clusters are not supported on Windows")




## Package Data

# none


## Package Info

.skeleton_package_title = "Support for Parallel computation in R"

.skeleton_package_version = "3.6.0"

.skeleton_package_depends = ""

.skeleton_package_imports = ""


## Internal

.skeleton_version = 5


## EOF
##
## Exported symobls in package `base`
##

## Exported package methods

`!` <- function (x)  .Primitive("!")


`$` <- .Primitive("$")


`&` <- function (e1, e2)  .Primitive("&")


`(` <- .Primitive("(")


`*` <- function (e1, e2)  .Primitive("*")


`+` <- function (e1, e2)  .Primitive("+")


`-` <- function (e1, e2)  .Primitive("-")


`/` <- function (e1, e2)  .Primitive("/")


`:` <- .Primitive(":")


`<` <- function (e1, e2)  .Primitive("<")


`=` <- .Primitive("=")


`>` <- function (e1, e2)  .Primitive(">")


as.matrix.data.frame <- function (x, rownames.force = NA, ...) 
{
    dm <- dim(x)
    rn <- if (rownames.force %in% FALSE) 
        NULL
    else if (rownames.force %in% TRUE || .row_names_info(x) > 
        0L) 
        row.names(x)
    dn <- list(rn, names(x))
    if (any(dm == 0L)) 
        return(array(NA, dim = dm, dimnames = dn))
    p <- dm[2L]
    pseq <- seq_len(p)
    n <- dm[1L]
    X <- unclass(x)
    non.numeric <- non.atomic <- FALSE
    all.logical <- TRUE
    for (j in pseq) {
        xj <- X[[j]]
        if (inherits(xj, "data.frame")) 
            X[[j]] <- xj <- as.matrix(xj)
        j.logic <- is.logical(xj)
        if (all.logical && !j.logic) 
            all.logical <- FALSE
        if (length(levels(xj)) > 0L || !(j.logic || is.numeric(xj) || 
            is.complex(xj)) || (!is.null(cl <- attr(xj, "class")) && 
            any(cl %in% c("Date", "POSIXct", "POSIXlt")))) 
            non.numeric <- TRUE
        if (!is.atomic(xj) && !inherits(xj, "POSIXlt")) 
            non.atomic <- TRUE
    }
    if (non.atomic) {
        for (j in pseq) {
            xj <- X[[j]]
            if (!is.recursive(xj)) 
                X[[j]] <- as.list(as.vector(xj))
        }
    }
    else if (all.logical) {
    }
    else if (non.numeric) {
        for (j in pseq) {
            if (is.character(X[[j]])) 
                next
            else if (is.logical(xj <- X[[j]])) 
                xj <- as.character(xj)
            else {
                miss <- is.na(xj)
                xj <- if (length(levels(xj))) 
                  as.vector(xj)
                else format(xj)
                is.na(xj) <- miss
            }
            X[[j]] <- xj
        }
    }
    collabs <- as.list(dn[[2L]])
    for (j in pseq) {
        xj <- X[[j]]
        dj <- dim(xj)
        if (length(dj) == 2L && dj[2L] > 0L) {
            if (!length(dnj <- colnames(xj))) 
                dnj <- seq_len(dj[2L])
            collabs[[j]] <- if (length(collabs)) {
                if (dj[2L] > 1L) 
                  paste(collabs[[j]], dnj, sep = ".")
                else if (is.character(collabs[[j]])) 
                  collabs[[j]]
                else dnj
            }
            else dnj
        }
    }
    nc <- vapply(X, NCOL, numeric(1), USE.NAMES = FALSE)
    X <- unlist(X, recursive = FALSE, use.names = FALSE)
    dim(X) <- c(n, length(X)/n)
    dimnames(X) <- list(dn[[1L]], unlist(collabs[nc > 0], use.names = FALSE))
    X
}


`@` <- .Primitive("@")


F <- FALSE


I <- function (x) 
{
    structure(x, class = unique(c("AsIs", oldClass(x))))
}


registerS3methods <- function (info, package, env) 
{
    n <- NROW(info)
    if (n == 0L) 
        return()
    assignWrapped <- function(x, method, home, envir) {
        method <- method
        home <- home
        delayedAssign(x, get(method, envir = home), assign.env = envir)
    }
    overwrite <- matrix(NA_character_, 0, 2)
    .registerS3method <- function(genname, class, method, nm, 
        envir) {
        defenv <- if (!is.na(w <- .knownS3Generics[genname])) 
            asNamespace(w)
        else {
            if (is.null(genfun <- get0(genname, envir = parent.env(envir)))) 
                stop(gettextf("object '%s' not found whilst loading namespace '%s'", 
                  genname, package), call. = FALSE, domain = NA)
            if (.isMethodsDispatchOn() && methods::is(genfun, 
                "genericFunction")) 
                genfun <- genfun@default
            if (typeof(genfun) == "closure") 
                environment(genfun)
            else .BaseNamespaceEnv
        }
        if (is.null(table <- defenv[[".__S3MethodsTable__."]])) {
            table <- new.env(hash = TRUE, parent = baseenv())
            defenv[[".__S3MethodsTable__."]] <- table
        }
        if (!is.null(e <- table[[nm]]) && !identical(e, get(method, 
            envir = envir))) {
            current <- environmentName(environment(e))
            overwrite <<- rbind(overwrite, c(as.vector(nm), current))
        }
        assignWrapped(nm, method, home = envir, envir = table)
    }
    methname <- paste(info[, 1], info[, 2], sep = ".")
    z <- is.na(info[, 3])
    info[z, 3] <- methname[z]
    if (ncol(info) == 3L) 
        info <- cbind(info, NA_character_)
    Info <- cbind(info[, 1L:3L, drop = FALSE], methname, info[, 
        4L])
    loc <- names(env)
    if (any(notex <- match(info[, 3], loc, nomatch = 0L) == 0L)) {
        warning(sprintf(ngettext(sum(notex), "S3 method %s was declared in NAMESPACE but not found", 
            "S3 methods %s were declared in NAMESPACE but not found"), 
            paste(sQuote(info[notex, 3]), collapse = ", ")), 
            call. = FALSE, domain = NA)
        Info <- Info[!notex, , drop = FALSE]
    }
    eager <- is.na(Info[, 5L])
    delayed <- Info[!eager, , drop = FALSE]
    Info <- Info[eager, , drop = FALSE]
    l2 <- localGeneric <- Info[, 1] %in% loc
    if (.isMethodsDispatchOn()) 
        for (i in which(localGeneric)) {
            genfun <- get(Info[i, 1], envir = env)
            if (methods::is(genfun, "genericFunction")) {
                localGeneric[i] <- FALSE
                registerS3method(Info[i, 1], Info[i, 2], Info[i, 
                  3], env)
            }
        }
    if (any(localGeneric)) {
        lin <- Info[localGeneric, , drop = FALSE]
        S3MethodsTable <- env[[".__S3MethodsTable__."]]
        .Internal(importIntoEnv(S3MethodsTable, lin[, 4], env, 
            lin[, 3]))
    }
    fin <- Info[!l2, , drop = FALSE]
    for (i in seq_len(nrow(fin))) .registerS3method(fin[i, 1], 
        fin[i, 2], fin[i, 3], fin[i, 4], env)
    if (package != "MASS" && nrow(overwrite)) {
        .fmt <- function(o) {
            sprintf("  %s %s", format(c("method", o[, 1L])), 
                format(c("from", o[, 2L])))
        }
        overwrite <- overwrite[overwrite[, 2L] != package, , 
            drop = FALSE]
        if (Sys.getenv("_R_LOAD_CHECK_OVERWRITE_S3_METHODS_") %in% 
            c(package, "all")) {
            ind <- overwrite[, 2L] %in% unlist(tools:::.get_standard_package_names(), 
                use.names = FALSE)
            bad <- overwrite[ind, , drop = FALSE]
            if (nr <- nrow(bad)) {
                msg <- ngettext(nr, "Registered S3 method from a standard package overwritten by '%s':", 
                  "Registered S3 methods from standard package(s) overwritten by '%s':", 
                  domain = NA)
                msg <- paste(c(sprintf(msg, package), .fmt(bad)), 
                  collapse = "\n")
                message(msg, domain = NA)
                overwrite <- overwrite[!ind, , drop = FALSE]
            }
        }
        if ((nr <- nrow(overwrite)) && is.na(match(tolower(Sys.getenv("_R_S3_METHOD_REGISTRATION_NOTE_OVERWRITES_")), 
            c("0", "no", "false"))) && (!is.na(match(Sys.getenv("_R_CHECK_PACKAGE_NAME_"), 
            c("", package))))) {
            msg <- ngettext(nr, "Registered S3 method overwritten by '%s':", 
                "Registered S3 methods overwritten by '%s':", 
                domain = NA)
            msg <- paste(c(sprintf(msg, package), .fmt(overwrite)), 
                collapse = "\n")
            packageStartupMessage(msg, domain = NA)
        }
    }
    register_S3_method_delayed <- function(pkg, gen, cls, fun) {
        pkg <- pkg
        gen <- gen
        cls <- cls
        fun <- fun
        if (isNamespaceLoaded(pkg)) {
            registerS3method(gen, cls, fun, envir = asNamespace(pkg))
        }
        setHook(packageEvent(pkg, "onLoad"), function(...) {
            registerS3method(gen, cls, fun, envir = asNamespace(pkg))
        })
    }
    if (nrow(delayed)) {
        for (i in seq_len(nrow(delayed))) {
            gen <- delayed[i, 1L]
            cls <- delayed[i, 2L]
            fun <- get(delayed[i, 3L], envir = env)
            pkg <- delayed[i, 5L]
            register_S3_method_delayed(pkg, gen, cls, fun)
        }
    }
    nsI <- getNamespaceInfo(env, "S3methods")
    if (!is.null(p1 <- ncol(nsI)) && !is.null(p2 <- ncol(info)) && 
        p1 != p2) 
        stop(gettextf(paste("While loading namespace \"%s\": \"%s\" differ in ncol(.), env=%d, newNS=%d.", 
            "Maybe package installed with version of R newer than %s ?", 
            sep = "\n"), package, "S3methods", p1, p2, getRversion()), 
            domain = NA)
    setNamespaceInfo(env, "S3methods", rbind(info, nsI))
}


as.POSIXct.Date <- function (x, ...) 
.POSIXct(unclass(x) * 86400)


T <- TRUE


`[` <- .Primitive("[")


`^` <- function (e1, e2)  .Primitive("^")


c <- function (...)  .Primitive("c")


q <- function (save = "default", status = 0, runLast = TRUE) 
.Internal(quit(save, status, runLast))


t <- function (x) 
UseMethod("t")


`{` <- .Primitive("{")


`|` <- function (e1, e2)  .Primitive("|")


`~` <- .Primitive("~")


qr.solve <- function (a, b, tol = 1e-07) 
{
    if (!inherits(a, "qr")) 
        a <- qr(a, tol = tol)
    nc <- ncol(a$qr)
    nr <- nrow(a$qr)
    if (a$rank != min(nc, nr)) 
        stop("singular matrix 'a' in solve")
    if (missing(b)) {
        if (nc != nr) 
            stop("only square matrices can be inverted")
        b <- diag(1, nc)
    }
    res <- qr.coef(a, b)
    res[is.na(res)] <- 0
    res
}


str2expression <- function (text) 
.Internal(str2expression(text))


dynGet <- function (x, ifnotfound = stop(gettextf("%s not found", sQuote(x)), 
    domain = NA), minframe = 1L, inherits = FALSE) 
{
    n <- sys.nframe()
    myObj <- structure(list(.b = as.raw(7)), foo = 47L)
    while (n > minframe) {
        n <- n - 1L
        env <- sys.frame(n)
        r <- get0(x, envir = env, inherits = inherits, ifnotfound = myObj)
        if (!identical(r, myObj)) 
            return(r)
    }
    ifnotfound
}


as.single <- function (x, ...) 
UseMethod("as.single")


interaction <- function (..., drop = FALSE, sep = ".", lex.order = FALSE) 
{
    args <- list(...)
    narg <- length(args)
    if (narg < 1L) 
        stop("No factors specified")
    if (narg == 1L && is.list(args[[1L]])) {
        args <- args[[1L]]
        narg <- length(args)
    }
    for (i in narg:1L) {
        f <- as.factor(args[[i]])[, drop = drop]
        l <- levels(f)
        if1 <- as.integer(f) - 1L
        if (i == narg) {
            ans <- if1
            lvs <- l
        }
        else {
            if (lex.order) {
                ll <- length(lvs)
                ans <- ans + ll * if1
                lvs <- paste(rep(l, each = ll), rep(lvs, length(l)), 
                  sep = sep)
            }
            else {
                ans <- ans * length(l) + if1
                lvs <- paste(rep(l, length(lvs)), rep(lvs, each = length(l)), 
                  sep = sep)
            }
            if (anyDuplicated(lvs)) {
                ulvs <- unique(lvs)
                while ((i <- anyDuplicated(flv <- match(lvs, 
                  ulvs)))) {
                  lvs <- lvs[-i]
                  ans[ans + 1L == i] <- match(flv[i], flv[1:(i - 
                    1)]) - 1L
                  ans[ans + 1L > i] <- ans[ans + 1L > i] - 1L
                }
                lvs <- ulvs
            }
            if (drop) {
                olvs <- lvs
                lvs <- lvs[sort(unique(ans + 1L))]
                ans <- match(olvs[ans + 1L], lvs) - 1L
            }
        }
    }
    structure(as.integer(ans + 1L), levels = lvs, class = "factor")
}


as.POSIXlt.default <- function (x, tz = "", optional = FALSE, ...) 
{
    if (inherits(x, "POSIXlt")) 
        return(x)
    if (is.logical(x) && all(is.na(x))) 
        return(as.POSIXlt(as.POSIXct.default(x), tz = tz))
    if (optional) 
        as.POSIXlt.character(rep.int(NA_character_, length(x)), 
            tz = tz)
    else stop(gettextf("do not know how to convert '%s' to class %s", 
        deparse(substitute(x)), dQuote("POSIXlt")), domain = NA)
}


gamma <- function (x)  .Primitive("gamma")


as.package_version <- function (x) 
if (is.package_version(x)) x else package_version(x)


.GlobalEnv <- "<environment>"

getNativeSymbolInfo <- function (name, PACKAGE, unlist = TRUE, withRegistrationInfo = FALSE) 
{
    if (missing(PACKAGE)) 
        PACKAGE <- ""
    if (is.character(PACKAGE)) 
        pkgName <- PACKAGE
    else if (inherits(PACKAGE, "DLLInfo")) {
        pkgName <- PACKAGE[["path"]]
        PACKAGE <- PACKAGE[["info"]]
    }
    else if (inherits(PACKAGE, "DLLInfoReference")) {
        pkgName <- character()
    }
    else stop(gettextf("must pass a package name, %s or %s object", 
        dQuote("DLLInfo"), dQuote("DllInfoReference")), domain = NA)
    syms <- lapply(name, function(id) {
        v <- .Internal(getSymbolInfo(as.character(id), PACKAGE, 
            as.logical(withRegistrationInfo)))
        if (is.null(v)) {
            msg <- paste("no such symbol", id)
            if (length(pkgName) && nzchar(pkgName)) 
                msg <- paste(msg, "in package", pkgName)
            stop(msg, domain = NA)
        }
        names(v) <- c("name", "address", "dll", "numParameters")[seq_along(v)]
        v
    })
    if (length(name) == 1L && unlist) 
        syms <- syms[[1L]]
    else names(syms) <- name
    syms
}


print.srcfile <- function (x, ...) 
{
    cat(x$filename, "\n")
    invisible(x)
}


`is.na<-` <- function (x, value) 
UseMethod("is.na<-")


packageHasNamespace <- function (package, package.lib) 
file.exists(file.path(package.lib, package, "NAMESPACE"))


`!=` <- function (e1, e2)  .Primitive("!=")


.handleSimpleError <- function (h, msg, call) 
h(simpleError(msg, call))


`%%` <- function (e1, e2)  .Primitive("%%")


`&&` <- .Primitive("&&")


xtfrm.AsIs <- function (x) 
{
    if (length(cl <- class(x)) > 1) 
        oldClass(x) <- cl[-1L]
    NextMethod("xtfrm")
}


`colnames<-` <- function (x, value) 
{
    if (is.data.frame(x)) {
        names(x) <- value
    }
    else {
        dn <- dimnames(x)
        if (is.null(dn)) {
            if (is.null(value)) 
                return(x)
            if ((nd <- length(dim(x))) < 2L) 
                stop("attempt to set 'colnames' on an object with less than two dimensions")
            dn <- vector("list", nd)
        }
        if (length(dn) < 2L) 
            stop("attempt to set 'colnames' on an object with less than two dimensions")
        if (is.null(value)) 
            dn[2L] <- list(NULL)
        else dn[[2L]] <- value
        dimnames(x) <- dn
    }
    x
}


file.choose <- function (new = FALSE) 
.Internal(file.choose(new))


summary.connection <- function (object, ...) 
.Internal(summary.connection(object))


.C <- function (.NAME, ..., NAOK = FALSE, DUP = TRUE, PACKAGE, ENCODING)  .Primitive(".C")


subset.default <- function (x, subset, ...) 
{
    if (!is.logical(subset)) 
        stop("'subset' must be logical")
    x[subset & !is.na(subset)]
}


warningCondition <- function (message, ..., class = NULL, call = NULL) 
structure(list(message = as.character(message), call = call, 
    ...), class = c(class, "warning", "condition"))


utf8ToInt <- function (x) 
.Internal(utf8ToInt(x))


baseenv <- function ()  .Primitive("baseenv")


.set_row_names <- function (n) 
if (n > 0) c(NA_integer_, -n) else integer()


Ops.POSIXt <- function (e1, e2) 
{
    if (nargs() == 1L) 
        stop(gettextf("unary '%s' not defined for \"POSIXt\" objects", 
            .Generic), domain = NA)
    boolean <- switch(.Generic, `<` = , `>` = , `==` = , `!=` = , 
        `<=` = , `>=` = TRUE, FALSE)
    if (!boolean) 
        stop(gettextf("'%s' not defined for \"POSIXt\" objects", 
            .Generic), domain = NA)
    if (inherits(e1, "POSIXlt") || is.character(e1)) 
        e1 <- as.POSIXct(e1)
    if (inherits(e2, "POSIXlt") || is.character(e2)) 
        e2 <- as.POSIXct(e2)
    check_tzones(e1, e2)
    NextMethod(.Generic)
}


quote <- function (expr)  .Primitive("quote")


`::` <- function (pkg, name) 
{
    pkg <- as.character(substitute(pkg))
    name <- as.character(substitute(name))
    getExportedValue(pkg, name)
}


inverse.rle <- function (x, ...) 
{
    if (is.null(le <- x$lengths) || is.null(v <- x$values) || 
        length(le) != length(v)) 
        stop("invalid 'rle' structure")
    rep.int(v, le)
}


`<-` <- .Primitive("<-")


`<=` <- function (e1, e2)  .Primitive("<=")


`==` <- function (e1, e2)  .Primitive("==")


`>=` <- function (e1, e2)  .Primitive(">=")


memory.profile <- function () 
.Internal(memory.profile())


split.POSIXct <- function (x, f, drop = FALSE, ...) 
lapply(split.default(as.double(x), f, drop = drop, ...), .POSIXct, 
    attr(x, "tzone"), oldClass(x))


unique.numeric_version <- function (x, incomparables = FALSE, ...) 
x[!duplicated(x, incomparables, ...)]


length.POSIXlt <- function (x) 
length(unclass(x)[[1L]])


getOption <- function (x, default = NULL) 
{
    if (missing(default)) 
        .Internal(getOption(x))
    else {
        ans <- .Internal(getOption(x))
        if (is.null(ans)) 
            default
        else ans
    }
}


Im <- function (z)  .Primitive("Im")


qr.Q <- function (qr, complete = FALSE, Dvec) 
{
    if (!is.qr(qr)) 
        stop("argument is not a QR decomposition")
    dqr <- dim(qr$qr)
    n <- dqr[1L]
    cmplx <- mode(qr$qr) == "complex"
    if (missing(Dvec)) 
        Dvec <- rep.int(if (cmplx) 
            1 + (0+0i)
        else 1, if (complete) 
            n
        else min(dqr))
    D <- if (complete) 
        diag(Dvec, n)
    else {
        ncols <- min(dqr)
        diag(Dvec[seq_len(ncols)], nrow = n, ncol = ncols)
    }
    qr.qy(qr, D)
}


qr.R <- function (qr, complete = FALSE) 
{
    if (!is.qr(qr)) 
        stop("argument is not a QR decomposition")
    R <- qr$qr
    if (!complete) 
        R <- R[seq.int(min(dim(R))), , drop = FALSE]
    R[row(R) > col(R)] <- 0
    R
}


rapply <- function (object, f, classes = "ANY", deflt = NULL, how = c("unlist", 
    "replace", "list"), ...) 
{
    how <- match.arg(how)
    res <- .Internal(rapply(object, f, classes, deflt, how))
    if (how == "unlist") 
        unlist(res, recursive = TRUE)
    else res
}


qr.X <- function (qr, complete = FALSE, ncol = if (complete) nrow(R) else min(dim(R))) 
{
    if (!is.qr(qr)) 
        stop("argument is not a QR decomposition")
    pivoted <- !identical(qr$pivot, ip <- seq_along(qr$pivot))
    R <- qr.R(qr, complete = TRUE)
    if (pivoted && ncol < length(qr$pivot)) 
        stop("need larger value of 'ncol' as pivoting occurred")
    cmplx <- mode(R) == "complex"
    p <- as.integer(dim(R)[2L])
    if (is.na(p)) 
        stop("invalid NCOL(R)")
    if (ncol < p) 
        R <- R[, seq_len(ncol), drop = FALSE]
    else if (ncol > p) {
        tmp <- diag(if (!cmplx) 
            1
        else 1 + (0+0i), nrow(R), ncol)
        tmp[, seq_len(p)] <- R
        R <- tmp
    }
    res <- qr.qy(qr, R)
    cn <- colnames(res)
    if (pivoted) {
        res[, qr$pivot] <- res[, ip]
        if (!is.null(cn)) 
            colnames(res)[qr$pivot] <- cn[ip]
    }
    res
}


library.dynam <- function (chname, package, lib.loc, verbose = getOption("verbose"), 
    file.ext = .Platform$dynlib.ext, ...) 
{
    dll_list <- .dynLibs()
    if (missing(chname) || !nzchar(chname)) 
        return(dll_list)
    package
    lib.loc
    r_arch <- .Platform$r_arch
    chname1 <- paste0(chname, file.ext)
    for (pkg in find.package(package, lib.loc, verbose = verbose)) {
        DLLpath <- if (nzchar(r_arch)) 
            file.path(pkg, "libs", r_arch)
        else file.path(pkg, "libs")
        file <- file.path(DLLpath, chname1)
        if (file.exists(file)) 
            break
        else file <- ""
    }
    if (file == "") 
        if (.Platform$OS.type == "windows") 
            stop(gettextf("DLL %s not found: maybe not installed for this architecture?", 
                sQuote(chname)), domain = NA)
        else stop(gettextf("shared object %s not found", sQuote(chname1)), 
            domain = NA)
    file <- file.path(normalizePath(DLLpath, "/", TRUE), chname1)
    ind <- vapply(dll_list, function(x) x[["path"]] == file, 
        NA)
    if (length(ind) && any(ind)) {
        if (verbose) 
            if (.Platform$OS.type == "windows") 
                message(gettextf("DLL %s already loaded", sQuote(chname1)), 
                  domain = NA)
            else message(gettextf("shared object '%s' already loaded", 
                sQuote(chname1)), domain = NA)
        return(invisible(dll_list[[seq_along(dll_list)[ind]]]))
    }
    if (.Platform$OS.type == "windows") {
        PATH <- Sys.getenv("PATH")
        Sys.setenv(PATH = paste(gsub("/", "\\\\", DLLpath), PATH, 
            sep = ";"))
        on.exit(Sys.setenv(PATH = PATH))
    }
    if (verbose) 
        message(gettextf("now dyn.load(\"%s\") ...", file), domain = NA)
    dll <- if ("DLLpath" %in% names(list(...))) 
        dyn.load(file, ...)
    else dyn.load(file, DLLpath = DLLpath, ...)
    .dynLibs(c(dll_list, list(dll)))
    invisible(dll)
}


evalq <- function (expr, envir = parent.frame(), enclos = if (is.list(envir) || 
    is.pairlist(envir)) parent.frame() else baseenv()) 
.Internal(eval(substitute(expr), envir, enclos))


units.difftime <- function (x) 
attr(x, "units")


Re <- function (z)  .Primitive("Re")


package_version <- function (x, strict = TRUE) 
{
    if (is.list(x) && all(c("major", "minor") %in% names(x))) 
        return(R_system_version(paste(x[c("major", "minor")], 
            collapse = ".")))
    .make_numeric_version(x, strict, .standard_regexps()$valid_package_version, 
        "package_version")
}


is.name <- function (x)  .Primitive("is.symbol")


as.data.frame.matrix <- function (x, row.names = NULL, optional = FALSE, make.names = TRUE, 
    ..., stringsAsFactors = default.stringsAsFactors()) 
{
    d <- dim(x)
    nrows <- d[[1L]]
    ncols <- d[[2L]]
    ic <- seq_len(ncols)
    dn <- dimnames(x)
    if (is.null(row.names)) 
        row.names <- dn[[1L]]
    collabs <- dn[[2L]]
    if (any(empty <- !nzchar(collabs))) 
        collabs[empty] <- paste0("V", ic)[empty]
    value <- vector("list", ncols)
    if (mode(x) == "character" && stringsAsFactors) {
        for (i in ic) value[[i]] <- as.factor(x[, i])
    }
    else {
        for (i in ic) value[[i]] <- as.vector(x[, i])
    }
    autoRN <- (is.null(row.names) || length(row.names) != nrows)
    if (length(collabs) == ncols) 
        names(value) <- collabs
    else if (!optional) 
        names(value) <- paste0("V", ic)
    class(value) <- "data.frame"
    if (autoRN) 
        attr(value, "row.names") <- .set_row_names(nrows)
    else .rowNamesDF(value, make.names = make.names) <- row.names
    value
}


.NotYetImplemented <- function () 
stop(gettextf("'%s' is not implemented yet", as.character(sys.call(sys.parent())[[1L]])), 
    call. = FALSE)


LETTERS <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", 
"M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", 
"Z")


unique.array <- function (x, incomparables = FALSE, MARGIN = 1, fromLast = FALSE, 
    ...) 
{
    if (!isFALSE(incomparables)) 
        .NotYetUsed("incomparables != FALSE")
    dx <- dim(x)
    ndim <- length(dx)
    if (length(MARGIN) != 1L || (MARGIN > ndim)) 
        stop(gettextf("MARGIN = %s is invalid for dim = %s", 
            paste(MARGIN, collapse = ","), paste(dx, collapse = ",")), 
            domain = NA)
    temp <- if ((ndim > 1L) && (prod(dx[-MARGIN]) > 1L)) 
        asplit(x, MARGIN)
    else x
    args <- rep(alist(a = ), ndim)
    names(args) <- NULL
    args[[MARGIN]] <- !duplicated.default(temp, fromLast = fromLast, 
        ...)
    do.call("[", c(list(x), args, list(drop = FALSE)))
}


`[[` <- .Primitive("[[")


dimnames.data.frame <- function (x) 
list(row.names(x), names(x))


require <- function (package, lib.loc = NULL, quietly = FALSE, warn.conflicts, 
    character.only = FALSE, mask.ok, exclude, include.only, attach.required = missing(include.only)) 
{
    if (!character.only) 
        package <- as.character(substitute(package))
    loaded <- paste0("package:", package) %in% search()
    if (!loaded) {
        if (!quietly) 
            packageStartupMessage(gettextf("Loading required package: %s", 
                package), domain = NA)
        value <- tryCatch(library(package, lib.loc = lib.loc, 
            character.only = TRUE, logical.return = TRUE, warn.conflicts = warn.conflicts, 
            quietly = quietly, mask.ok = mask.ok, exclude = exclude, 
            include.only = include.only, attach.required = attach.required), 
            error = function(e) e)
        if (inherits(value, "error")) {
            if (!quietly) {
                msg <- conditionMessage(value)
                cat("Failed with error:  ", sQuote(msg), "\n", 
                  file = stderr(), sep = "")
                .Internal(printDeferredWarnings())
            }
            return(invisible(FALSE))
        }
        if (!value) 
            return(invisible(FALSE))
    }
    else value <- TRUE
    invisible(value)
}


parse <- function (file = "", n = NULL, text = NULL, prompt = "?", keep.source = getOption("keep.source"), 
    srcfile = NULL, encoding = "unknown") 
{
    keep.source <- isTRUE(keep.source)
    if (!is.null(text)) {
        if (length(text) == 0L) 
            return(expression())
        if (missing(srcfile)) {
            srcfile <- "<text>"
            if (keep.source) 
                srcfile <- srcfilecopy(srcfile, text)
        }
        file <- stdin()
    }
    else {
        if (is.character(file)) {
            if (file == "") {
                file <- stdin()
                if (missing(srcfile)) 
                  srcfile <- "<stdin>"
            }
            else {
                filename <- file
                file <- file(filename, "r")
                if (missing(srcfile)) 
                  srcfile <- filename
                if (keep.source) {
                  text <- readLines(file, warn = FALSE)
                  if (!length(text)) 
                    text <- ""
                  close(file)
                  file <- stdin()
                  srcfile <- srcfilecopy(filename, text, file.mtime(filename), 
                    isFile = TRUE)
                }
                else on.exit(close(file))
            }
        }
    }
    .Internal(parse(file, n, text, prompt, srcfile, encoding))
}


by <- function (data, INDICES, FUN, ..., simplify = TRUE) 
UseMethod("by")


gc <- function (verbose = getOption("verbose"), reset = FALSE, full = TRUE) 
{
    res <- .Internal(gc(verbose, reset, full))
    res <- matrix(res, 2L, 7L, dimnames = list(c("Ncells", "Vcells"), 
        c("used", "(Mb)", "gc trigger", "(Mb)", "limit (Mb)", 
            "max used", "(Mb)")))
    if (all(is.na(res[, 5L]))) 
        res[, -5L]
    else res
}


gl <- function (n, k, length = n * k, labels = seq_len(n), ordered = FALSE) 
{
    f <- rep_len(rep.int(seq_len(n), rep.int(k, n)), length)
    levels(f) <- as.character(labels)
    class(f) <- c(if (ordered) "ordered", "factor")
    f
}


ls <- function (name, pos = -1L, envir = as.environment(pos), all.names = FALSE, 
    pattern, sorted = TRUE) 
{
    if (!missing(name)) {
        pos <- tryCatch(name, error = function(e) e)
        if (inherits(pos, "error")) {
            name <- substitute(name)
            if (!is.character(name)) 
                name <- deparse(name)
            warning(gettextf("%s converted to character string", 
                sQuote(name)), domain = NA)
            pos <- name
        }
    }
    all.names <- .Internal(ls(envir, all.names, sorted))
    if (!missing(pattern)) {
        if ((ll <- length(grep("[", pattern, fixed = TRUE))) && 
            ll != length(grep("]", pattern, fixed = TRUE))) {
            if (pattern == "[") {
                pattern <- "\\["
                warning("replaced regular expression pattern '[' by  '\\\\['")
            }
            else if (length(grep("[^\\\\]\\[<-", pattern))) {
                pattern <- sub("\\[<-", "\\\\\\[<-", pattern)
                warning("replaced '[<-' by '\\\\[<-' in regular expression pattern")
            }
        }
        grep(pattern, all.names, value = TRUE)
    }
    else all.names
}


unique.warnings <- function (x, incomparables = FALSE, ...) 
x[!duplicated(x, incomparables, ...)]


pi <- 3.14159265358979


gzcon <- function (con, level = 6, allowNonCompressed = TRUE, text = FALSE) 
.Internal(gzcon(con, level, allowNonCompressed, text))


qr <- function (x, ...) 
UseMethod("qr")


lapply <- function (X, FUN, ...) 
{
    FUN <- match.fun(FUN)
    if (!is.vector(X) || is.object(X)) 
        X <- as.list(X)
    .Internal(lapply(X, FUN))
}


rm <- function (..., list = character(), pos = -1, envir = as.environment(pos), 
    inherits = FALSE) 
{
    dots <- match.call(expand.dots = FALSE)$...
    if (length(dots) && !all(vapply(dots, function(x) is.symbol(x) || 
        is.character(x), NA, USE.NAMES = FALSE))) 
        stop("... must contain names or character strings")
    names <- vapply(dots, as.character, "")
    if (length(names) == 0L) 
        names <- character()
    list <- .Primitive("c")(list, names)
    .Internal(remove(list, envir, inherits))
}


paste <- function (..., sep = " ", collapse = NULL) 
.Internal(paste(list(...), sep, collapse))


match <- function (x, table, nomatch = NA_integer_, incomparables = NULL) 
.Internal(match(x, table, nomatch, incomparables))


order <- function (..., na.last = TRUE, decreasing = FALSE, method = c("auto", 
    "shell", "radix")) 
{
    z <- list(...)
    decreasing <- as.logical(decreasing)
    if (length(z) == 1L && is.numeric(x <- z[[1L]]) && !is.object(x) && 
        length(x) > 0) {
        if (.Internal(sorted_fpass(x, decreasing, na.last))) 
            return(seq_along(x))
    }
    method <- match.arg(method)
    if (any(vapply(z, is.object, logical(1L)))) {
        z <- lapply(z, function(x) if (is.object(x)) 
            as.vector(xtfrm(x))
        else x)
        return(do.call("order", c(z, list(na.last = na.last, 
            decreasing = decreasing, method = method))))
    }
    if (method == "auto") {
        useRadix <- all(vapply(z, function(x) {
            (is.numeric(x) || is.factor(x) || is.logical(x)) && 
                is.integer(length(x))
        }, logical(1L)))
        method <- if (useRadix) 
            "radix"
        else "shell"
    }
    if (method != "radix" && !is.na(na.last)) {
        return(.Internal(order(na.last, decreasing, ...)))
    }
    if (method == "radix") {
        decreasing <- rep_len(as.logical(decreasing), length(z))
        return(.Internal(radixsort(na.last, decreasing, FALSE, 
            TRUE, ...)))
    }
    if (any(diff((l.z <- lengths(z)) != 0L))) 
        stop("argument lengths differ")
    na <- vapply(z, is.na, rep.int(NA, l.z[1L]))
    ok <- if (is.matrix(na)) 
        rowSums(na) == 0L
    else !any(na)
    if (all(!ok)) 
        return(integer())
    z[[1L]][!ok] <- NA
    ans <- do.call("order", c(z, list(decreasing = decreasing)))
    ans[ok[ans]]
}


xpdrows.data.frame <- function (x, old.rows, new.rows) 
{
    nc <- length(x)
    nro <- length(old.rows)
    nrn <- length(new.rows)
    nr <- nro + nrn
    for (i in seq_len(nc)) {
        y <- x[[i]]
        dy <- dim(y)
        cy <- oldClass(y)
        class(y) <- NULL
        if (length(dy) == 2L) {
            dny <- dimnames(y)
            if (length(dny[[1L]]) > 0L) 
                dny[[1L]] <- c(dny[[1L]], new.rows)
            z <- array(y[1L], dim = c(nr, nc), dimnames = dny)
            z[seq_len(nro), ] <- y
            class(z) <- cy
            x[[i]] <- z
        }
        else {
            ay <- attributes(y)
            if (length(names(y)) > 0L) 
                ay$names <- c(ay$names, new.rows)
            length(y) <- nr
            attributes(y) <- ay
            class(y) <- cy
            x[[i]] <- y
        }
    }
    nm <- c(old.rows, new.rows)
    if (any(duplicated(nm))) 
        nm <- make.unique(as.character(nm))
    attr(x, "row.names") <- nm
    x
}


rank <- function (x, na.last = TRUE, ties.method = c("average", "first", 
    "last", "random", "max", "min")) 
{
    nas <- is.na(x)
    nm <- names(x)
    ties.method <- match.arg(ties.method)
    if (is.factor(x)) 
        x <- as.integer(x)
    x <- x[!nas]
    y <- switch(ties.method, average = , min = , max = .Internal(rank(x, 
        length(x), ties.method)), first = sort.list(sort.list(x)), 
        last = sort.list(rev.default(sort.list(x, decreasing = TRUE))), 
        random = sort.list(order(x, stats::runif(sum(!nas)))))
    if (!is.na(na.last) && any(nas)) {
        yy <- NA
        NAkeep <- (na.last == "keep")
        if (NAkeep || na.last) {
            yy[!nas] <- y
            if (!NAkeep) 
                yy[nas] <- (length(y) + 1L):length(yy)
        }
        else {
            len <- sum(nas)
            yy[!nas] <- y + len
            yy[nas] <- seq_len(len)
        }
        y <- yy
        names(y) <- nm
    }
    else names(y) <- nm[!nas]
    y
}


`||` <- .Primitive("||")


seq.default <- function (from = 1, to = 1, by = ((to - from)/(length.out - 1)), 
    length.out = NULL, along.with = NULL, ...) 
{
    is.logint <- function(.) (is.integer(.) || is.logical(.)) && 
        !is.object(.)
    if ((One <- nargs() == 1L) && !missing(from)) {
        lf <- length(from)
        return(if (mode(from) == "numeric" && lf == 1L) {
            if (!is.finite(from)) stop("'from' must be a finite number")
            1L:from
        } else if (lf) 1L:lf else integer())
    }
    if (!missing(along.with)) {
        length.out <- length(along.with)
        if (One) 
            return(if (length.out) seq_len(length.out) else integer())
        intn1 <- is.integer(length.out)
    }
    else if (!missing(length.out)) {
        len <- length(length.out)
        if (!len) 
            stop("argument 'length.out' must be of length 1")
        if (len > 1L) {
            warning("first element used of 'length.out' argument")
            length.out <- length.out[1L]
        }
        if (!(intn1 <- is.logint(length.out))) 
            length.out <- as.numeric(ceiling(length.out))
    }
    chkDots(...)
    if (!missing(from) && length(from) != 1L) 
        stop("'from' must be of length 1")
    if (!missing(to) && length(to) != 1L) 
        stop("'to' must be of length 1")
    if (!missing(from) && !is.finite(if (is.character(from)) from <- as.numeric(from) else from)) 
        stop("'from' must be a finite number")
    if (!missing(to) && !is.finite(if (is.character(to)) to <- as.numeric(to) else to)) 
        stop("'to' must be a finite number")
    if (is.null(length.out)) 
        if (missing(by)) 
            from:to
        else {
            int <- is.logint(from) && is.logint(to)
            del <- to - if (int) 
                as.double(from)
            else from
            if (del == 0 && to == 0) 
                return(to)
            if (length(by) != 1L) 
                stop("'by' must be of length 1")
            if (!is.logint(by)) 
                int <- FALSE
            else if (!int) 
                storage.mode(by) <- "double"
            n <- del/by
            if (!is.finite(n)) {
                if (!is.na(by) && by == 0 && del == 0) 
                  return(from)
                stop("invalid '(to - from)/by'")
            }
            if (n < 0L) 
                stop("wrong sign in 'by' argument")
            if (n > .Machine$integer.max) 
                stop("'by' argument is much too small")
            dd <- abs(del)/max(abs(to), abs(from))
            if (dd < 100 * .Machine$double.eps) 
                return(from)
            if (int) {
                n <- as.integer(n)
                if (n >= 2L) 
                  cumsum(rep.int(c(from, by), c(1L, n)))
                else from + (0L:n) * by
            }
            else {
                n <- as.integer(n + 1e-10)
                x <- from + (0L:n) * by
                if (by > 0) 
                  pmin(x, to)
                else pmax(x, to)
            }
        }
    else if (!is.finite(length.out) || length.out < 0L) 
        stop("'length.out' must be a non-negative number")
    else if (length.out == 0L) 
        integer()
    else if (One) 
        seq_len(length.out)
    else if (missing(by)) {
        if (missing(to)) {
            to <- from + (length.out - 1)
            intdel <- intn1 && is.logint(from) && to <= .Machine$integer.max
            if (intdel) 
                storage.mode(to) <- "integer"
        }
        else intdel <- is.logint(to)
        if (missing(from)) {
            from <- to - (length.out - 1)
            if (intdel) {
                intdel <- intn1 && from >= -.Machine$integer.max
                if (intdel) 
                  storage.mode(from) <- "integer"
            }
        }
        else if (intdel) 
            intdel <- is.logint(from)
        if (length.out > 2L) 
            if (from == to) 
                rep.int(from, length.out)
            else {
                n1 <- length.out - 1L
                if (intdel && intn1 && from%%n1 == to%%n1) {
                  by <- to%/%n1 - from%/%n1
                  cumsum(rep.int(c(from, by), c(1L, n1)))
                }
                else {
                  if (intdel) 
                    storage.mode(from) <- "double"
                  by <- (to - from)/n1
                  as.vector(c(from, from + seq_len(length.out - 
                    2L) * by, to))
                }
            }
        else as.vector(c(from, to))[seq_len(length.out)]
    }
    else if (missing(to)) {
        int <- (intby <- is.logint(by)) && is.logint(from) && 
            (!(nby <- length(by)) || (naby <- is.na(by)) || ((to <- from + 
                (length.out - 1) * by) <= .Machine$integer.max && 
                to >= -.Machine$integer.max))
        if (int && length.out > 2L && nby == 1L && !naby) 
            cumsum(rep.int(c(from, by), c(1L, length.out - 1L)))
        else {
            if (intby && !(int || is.object(from))) 
                storage.mode(by) <- "double"
            from + (0L:(length.out - 1L)) * by
        }
    }
    else if (missing(from)) {
        int <- (intby <- is.logint(by)) && is.logint(to) && (!(nby <- length(by)) || 
            (naby <- is.na(by)) || ((from <- to - (length.out - 
            1) * by) >= -.Machine$integer.max && from <= .Machine$integer.max))
        if (int && length.out > 2L && nby == 1L && !naby) 
            cumsum(rep.int(c(as.integer(from), by), c(1L, length.out - 
                1L)))
        else {
            if (intby && !(int || is.object(to))) 
                storage.mode(by) <- "double"
            to - ((length.out - 1L):0L) * by
        }
    }
    else stop("too many arguments")
}


startsWith <- function (x, prefix) 
.Internal(startsWith(x, prefix))


`[<-.Date` <- function (x, ..., value) 
{
    if (!length(value)) 
        return(x)
    value <- unclass(as.Date(value))
    .Date(NextMethod(.Generic), oldClass(x))
}


.noGenerics <- graphics::.noGenerics # re-exported from graphics package

aperm.default <- function (a, perm = NULL, resize = TRUE, ...) 
.Internal(aperm(a, perm, resize))


kronecker <- methods::kronecker # re-exported from methods package

traceback <- function (x = NULL, max.lines = getOption("deparse.max.lines")) 
{
    n <- length(x <- .traceback(x))
    if (n == 0L) 
        cat(gettext("No traceback available"), "\n")
    else {
        for (i in 1L:n) {
            xi <- x[[i]]
            label <- paste0(n - i + 1L, ": ")
            m <- length(xi)
            srcloc <- if (!is.null(srcref <- attr(xi, "srcref"))) {
                srcfile <- attr(srcref, "srcfile")
                paste0(" at ", basename(srcfile$filename), "#", 
                  srcref[1L])
            }
            if (is.numeric(max.lines) && max.lines > 0L && max.lines < 
                m) {
                xi <- c(xi[seq_len(max.lines)], " ...")
                m <- length(xi)
            }
            if (!is.null(srcloc)) {
                xi[m] <- paste0(xi[m], srcloc)
            }
            if (m > 1) 
                label <- c(label, rep(substr("          ", 1L, 
                  nchar(label, type = "w")), m - 1L))
            cat(paste0(label, xi), sep = "\n")
        }
    }
    invisible(x)
}


as.character.hexmode <- function (x, ...) 
format.hexmode(x, ...)


simpleError <- function (message, call = NULL) 
{
    class <- c("simpleError", "error", "condition")
    structure(list(message = as.character(message), call = call), 
        class = class)
}


warnings <- function (...) 
{
    if (length(last.warning <- baseenv()[["last.warning"]])) 
        structure(last.warning, dots = list(...), class = "warnings")
}


as.Date.POSIXct <- function (x, tz = "UTC", ...) 
{
    if (tz == "UTC") {
        z <- floor(unclass(x)/86400)
        attr(z, "tzone") <- NULL
        .Date(z)
    }
    else as.Date(as.POSIXlt(x, tz = tz))
}


kappa.qr <- function (z, ...) 
{
    qr <- z$qr
    R <- qr[1L:min(dim(qr)), , drop = FALSE]
    R[lower.tri(R)] <- 0
    .kappa_tri(R, ...)
}


isNamespace <- function (ns) 
.Internal(isNamespaceEnv(ns))


`attr<-` <- function (x, which, value)  .Primitive("attr<-")


colMeans <- function (x, na.rm = FALSE, dims = 1L) 
{
    if (is.data.frame(x)) 
        x <- as.matrix(x)
    if (!is.array(x) || length(dn <- dim(x)) < 2L) 
        stop("'x' must be an array of at least two dimensions")
    if (dims < 1L || dims > length(dn) - 1L) 
        stop("invalid 'dims'")
    n <- prod(dn[id <- seq_len(dims)])
    dn <- dn[-id]
    z <- if (is.complex(x)) 
        .Internal(colMeans(Re(x), n, prod(dn), na.rm)) + (0+1i) * 
            .Internal(colMeans(Im(x), n, prod(dn), na.rm))
    else .Internal(colMeans(x, n, prod(dn), na.rm))
    if (length(dn) > 1L) {
        dim(z) <- dn
        dimnames(z) <- dimnames(x)[-id]
    }
    else names(z) <- dimnames(x)[[dims + 1L]]
    z
}


as.Date.POSIXlt <- function (x, ...) 
.Internal(POSIXlt2Date(x))


asNamespace <- function (ns, base.OK = TRUE) 
{
    if (is.character(ns) || is.name(ns)) 
        ns <- getNamespace(ns)
    if (!isNamespace(ns)) 
        stop("not a namespace")
    else if (!base.OK && isBaseNamespace(ns)) 
        stop("operation not allowed on base namespace")
    else ns
}


xtfrm.difftime <- function (x) 
as.numeric(x)


quit <- function (save = "default", status = 0, runLast = TRUE) 
.Internal(quit(save, status, runLast))


kappa.lm <- function (z, ...) 
kappa.qr(z$qr, ...)


setSessionTimeLimit <- function (cpu = Inf, elapsed = Inf) 
.Internal(setSessionTimeLimit(cpu, elapsed))


find.package <- function (package = NULL, lib.loc = NULL, quiet = FALSE, verbose = getOption("verbose")) 
{
    if (is.null(package) && is.null(lib.loc) && !verbose) {
        return(path.package())
    }
    if (length(package) == 1L && package %in% c("base", "tools", 
        "utils", "grDevices", "graphics", "stats", "datasets", 
        "methods", "grid", "parallel", "splines", "stats4", "tcltk", 
        "compiler")) 
        return(file.path(.Library, package))
    if (is.null(package)) 
        package <- .packages()
    if (!length(package)) 
        return(character())
    if (use_loaded <- is.null(lib.loc)) 
        lib.loc <- .libPaths()
    bad <- character()
    out <- character()
    for (pkg in package) {
        paths <- file.path(lib.loc, pkg)
        paths <- paths[file.exists(file.path(paths, "DESCRIPTION"))]
        if (use_loaded && isNamespaceLoaded(pkg)) {
            dir <- if (pkg == "base") 
                system.file()
            else .getNamespaceInfo(asNamespace(pkg), "path")
            paths <- c(dir, paths)
        }
        if (length(paths) && file.exists(file.path(paths[1], 
            "dummy_for_check"))) {
            bad <- c(bad, pkg)
            next
        }
        if (length(paths)) {
            paths <- unique(paths)
            valid_package_version_regexp <- .standard_regexps()$valid_package_version
            db <- lapply(paths, function(p) {
                pfile <- file.path(p, "Meta", "package.rds")
                info <- if (file.exists(pfile)) 
                  readRDS(pfile)$DESCRIPTION[c("Package", "Version")]
                else {
                  info <- tryCatch(read.dcf(file.path(p, "DESCRIPTION"), 
                    c("Package", "Version"))[1, ], error = identity)
                  if (inherits(info, "error") || (length(info) != 
                    2L) || anyNA(info)) 
                    c(Package = NA, Version = NA)
                  else info
                }
            })
            db <- do.call("rbind", db)
            ok <- (apply(!is.na(db), 1L, all) & (db[, "Package"] == 
                pkg) & (grepl(valid_package_version_regexp, db[, 
                "Version"])))
            paths <- paths[ok]
        }
        if (length(paths) == 0L) {
            bad <- c(bad, pkg)
            next
        }
        if (length(paths) > 1L) {
            if (verbose) 
                warning(gettextf("package %s found more than once, using the first from\n  %s", 
                  sQuote(pkg), paste(dQuote(paths), collapse = ",\n  ")), 
                  domain = NA)
            paths <- paths[1L]
        }
        out <- c(out, paths)
    }
    if (!quiet && length(bad)) {
        if (length(out) == 0L) 
            stop(packageNotFoundError(bad, lib.loc, sys.call()))
        for (pkg in bad) warning(gettextf("there is no package called %s", 
            sQuote(pkg)), domain = NA)
    }
    out
}


weekdays.POSIXt <- function (x, abbreviate = FALSE) 
{
    format(x, ifelse(abbreviate, "%a", "%A"))
}


eval <- function (expr, envir = parent.frame(), enclos = if (is.list(envir) || 
    is.pairlist(envir)) parent.frame() else baseenv()) 
.Internal(eval(expr, envir, enclos))


c.numeric_version <- function (..., recursive = FALSE) 
{
    x <- lapply(list(...), as.numeric_version)
    classes <- if (length(unique(lapply(x, class))) == 1L) 
        class(x[[1L]])
    else "numeric_version"
    structure(unlist(x, recursive = FALSE), class = classes)
}


split.Date <- function (x, f, drop = FALSE, ...) 
{
    lapply(split.default(unclass(x), f, drop = drop, ...), .Date, 
        oldClass(x))
}


readChar <- function (con, nchars, useBytes = FALSE) 
{
    if (is.character(con)) {
        con <- file(con, "rb")
        on.exit(close(con))
    }
    .Internal(readChar(con, as.integer(nchars), useBytes))
}


autoloader <- function (name, package, ...) 
{
    name <- paste0(name, "")
    rm(list = name, envir = .AutoloadEnv, inherits = FALSE)
    m <- match.call()
    m$name <- NULL
    m[[1L]] <- as.name("library")
    eval(m, .GlobalEnv)
    autoload(name, package, reset = TRUE, ...)
    where <- match(paste0("package:", package), search())
    if (exists(name, where = where, inherits = FALSE)) 
        eval(as.name(name), as.environment(where))
    else stop(gettextf("autoloader did not find '%s' in '%s'", 
        name, package), domain = NA)
}


`[.POSIXct` <- function (x, ..., drop = TRUE) 
.POSIXct(NextMethod("["), attr(x, "tzone"), oldClass(x))


untracemem <- function (x)  .Primitive("untracemem")


Encoding <- function (x) 
.Internal(Encoding(x))


debuggingState <- function (on = NULL) 
.Internal(debugOnOff(on))


`[.POSIXlt` <- function (x, i, j, drop = TRUE) 
{
    if (!(mj <- missing(j))) 
        if (!is.character(j) || (length(j) != 1L)) 
            stop("component subscript must be a character string")
    if (missing(i)) {
        if (mj) 
            x
        else unclass(x)[[j]]
    }
    else {
        if (is.character(i)) 
            i <- match(i, names(x), incomparables = c("", NA_character_))
        if (mj) 
            .POSIXlt(lapply(X = unclass(x), FUN = "[", i, drop = drop), 
                attr(x, "tzone"), oldClass(x))
        else unclass(x)[[j]][i]
    }
}


do.call <- function (what, args, quote = FALSE, envir = parent.frame()) 
{
    if (!is.list(args)) 
        stop("second argument must be a list")
    if (quote) 
        args <- lapply(args, enquote)
    .Internal(do.call(what, args, envir))
}


.cache_class <- function (class, extends)  .Primitive(".cache_class")


`regmatches<-` <- function (x, m, invert = FALSE, value) 
{
    if (!length(x)) 
        return(x)
    ili <- is.list(m)
    if (!ili && invert && any(m == -1L)) {
        y <- rep_len(list(character()), length(x))
        y[m > -1L] <- as.list(regmatches(x, m, FALSE))
    }
    else {
        y <- regmatches(x, m, !invert)
    }
    if (!ili && !invert) {
        value <- as.character(value)
        if (anyNA(value)) 
            stop("missing replacement values are not allowed")
        pos <- which(lengths(y) == 2L)
        np <- length(pos)
        nv <- length(value)
        if (np != nv) {
            if (!nv) 
                stop("must have replacement values for matches")
            value <- rep_len(value, np)
        }
        y <- y[pos]
        x[pos] <- paste0(sapply(y, `[`, 1L), value, sapply(y, 
            `[`, 2L))
        return(x)
    }
    value <- lapply(value, as.character)
    if (anyNA(value)) 
        stop("missing replacement values are not allowed")
    if (!length(value)) 
        stop("value does not provide any replacement values")
    value <- rep_len(value, length(x))
    y <- if (invert) {
        Map(function(u, v) {
            nu <- length(u)
            nv <- length(v)
            if (nv != (nu + 1L)) {
                if (!nv) 
                  stop("must have replacements for non-matches")
                v <- rep_len(v, nu + 1L)
            }
            paste0(v, c(u, ""), collapse = "")
        }, y, value, USE.NAMES = FALSE)
    }
    else {
        Map(function(u, v) {
            nu <- length(u)
            nv <- length(v)
            if (nv != (nu - 1L)) {
                if (!nv) 
                  stop("must have replacements for matches")
                v <- rep_len(v, nu - 1L)
            }
            paste0(u, c(v, ""), collapse = "")
        }, y, value, USE.NAMES = FALSE)
    }
    y <- unlist(y)
    names(y) <- names(x)
    y
}


.getRequiredPackages2 <- function (pkgInfo, quietly = FALSE, lib.loc = NULL, useImports = FALSE) 
{
    .findVersion <- function(pkg, lib.loc = NULL) {
        pfile <- system.file("Meta", "package.rds", package = pkg, 
            lib.loc = lib.loc)
        if (nzchar(pfile)) 
            as.numeric_version(readRDS(pfile)$DESCRIPTION["Version"])
    }
    pkgs <- unique(names(pkgInfo$Depends))
    pkgname <- pkgInfo$DESCRIPTION["Package"]
    for (pkg in setdiff(pkgs, "base")) {
        depends <- pkgInfo$Depends[names(pkgInfo$Depends) == 
            pkg]
        attached <- paste0("package:", pkg) %in% search()
        current <- .findVersion(pkg, lib.loc)
        if (is.null(current)) 
            stop(gettextf("package %s required by %s could not be found", 
                sQuote(pkg), sQuote(pkgname)), call. = FALSE, 
                domain = NA)
        have_vers <- lengths(depends) > 1L
        for (dep in depends[have_vers]) {
            target <- as.numeric_version(dep$version)
            sufficient <- do.call(dep$op, list(current, target))
            if (!sufficient) {
                if (is.null(lib.loc)) 
                  lib.loc <- .libPaths()
                allV <- lapply(lib.loc, .findVersion, pkg = pkg)
                versions <- do.call(c, allV[iV <- which(!vapply(allV, 
                  is.null, NA))])
                sufficient <- vapply(versions, dep$op, logical(1L), 
                  target)
                if (any(sufficient)) {
                  warning(gettextf("version %s of %s masked by %s in %s", 
                    versions[which(sufficient)[1L]], sQuote(pkg), 
                    current, lib.loc[iV[!sufficient][1L]]), call. = FALSE, 
                    domain = NA)
                }
                msg <- if (attached) 
                  "package %s %s is loaded, but %s %s is required by %s"
                else "package %s %s was found, but %s %s is required by %s"
                stop(gettextf(msg, sQuote(pkg), current, dep$op, 
                  target, sQuote(pkgname)), call. = FALSE, domain = NA)
            }
        }
        if (!attached) {
            if (!quietly) 
                packageStartupMessage(gettextf("Loading required package: %s", 
                  pkg), domain = NA)
            library(pkg, character.only = TRUE, logical.return = TRUE, 
                lib.loc = lib.loc, quietly = quietly) || stop(gettextf("package %s could not be loaded", 
                sQuote(pkg)), call. = FALSE, domain = NA)
        }
    }
    if (useImports) {
        nss <- names(pkgInfo$Imports)
        for (ns in nss) loadNamespace(ns, lib.loc)
    }
}


levels.default <- function (x) 
attr(x, "levels")


computeRestarts <- function (cond = NULL) 
{
    val <- NULL
    i <- 1L
    repeat {
        r <- .Internal(.getRestart(i))
        if (is.null(r)) 
            return(val)
        else if (is.null(cond) || is.null(r$test) || r$test(cond)) 
            val <- c(val, list(r))
        i <- i + 1L
    }
}


readBin <- function (con, what, n = 1L, size = NA_integer_, signed = TRUE, 
    endian = .Platform$endian) 
{
    if (is.character(con)) {
        con <- file(con, "rb")
        on.exit(close(con))
    }
    swap <- endian != .Platform$endian
    if (!is.character(what) || is.na(what) || length(what) != 
        1L || !any(what == c("numeric", "double", "integer", 
        "int", "logical", "complex", "character", "raw"))) 
        what <- typeof(what)
    .Internal(readBin(con, what, n, size, signed, swap))
}


`[[<-` <- .Primitive("[[<-")


as.double.difftime <- function (x, units = "auto", ...) 
{
    if (units != "auto") 
        units(x) <- units
    as.vector(x, "double")
}


sys.call <- function (which = 0L) 
.Internal(sys.call(which))


`length<-.difftime` <- function (x, value) 
.difftime(NextMethod(), attr(x, "units"), oldClass(x))


path.expand <- function (path) 
.Internal(path.expand(path))


fifo <- function (description, open = "", blocking = FALSE, encoding = getOption("encoding")) 
.Internal(fifo(description, open, blocking, encoding))


substitute <- function (expr, env)  .Primitive("substitute")


data.matrix <- function (frame, rownames.force = NA) 
{
    if (!is.data.frame(frame)) 
        return(as.matrix(frame))
    d <- dim(frame)
    rn <- if (rownames.force %in% FALSE) 
        NULL
    else if (rownames.force %in% TRUE) 
        row.names(frame)
    else {
        if (.row_names_info(frame) <= 0L) 
            NULL
        else row.names(frame)
    }
    for (i in seq_len(d[2L])) {
        xi <- frame[[i]]
        if (is.integer(xi) || is.numeric(xi)) 
            next
        if (is.logical(xi) || is.factor(xi)) {
            frame[[i]] <- as.integer(xi)
            next
        }
        frame[[i]] <- if (isS4(xi)) 
            methods::as(xi, "numeric")
        else as.numeric(xi)
    }
    intOK <- all(unlist(lapply(frame, is.integer)))
    x <- matrix(if (intOK) 
        NA_integer_
    else NA_real_, nrow = d[1L], ncol = d[2L], dimnames = list(rn, 
        names(frame)))
    for (i in seq_len(d[2L])) x[, i] <- frame[[i]]
    x
}


all.equal.formula <- function (target, current, ...) 
{
    if (length(target) != length(current)) 
        return(paste0("target, current differ in having response: ", 
            length(target) == 3L, ", ", length(current) == 3L))
    if (!identical(deparse(target), deparse(current))) 
        "formulas differ in contents"
    else TRUE
}


as.logical.factor <- function (x, ...) 
as.logical(levels(x))[x]


file <- function (description = "", open = "", blocking = TRUE, encoding = getOption("encoding"), 
    raw = FALSE, method = getOption("url.method", "default")) 
{
    .Internal(file(description, open, blocking, encoding, method, 
        raw))
}


srcfilealias <- function (filename, srcfile) 
{
    stopifnot(is.character(filename), length(filename) == 1L)
    e <- new.env(parent = emptyenv())
    e$filename <- filename
    e$original <- srcfile
    class(e) <- c("srcfilealias", "srcfile")
    return(e)
}


determinant <- function (x, logarithm = TRUE, ...) 
UseMethod("determinant")


`units<-.difftime` <- function (x, value) 
{
    from <- units(x)
    if (from == value) 
        return(x)
    if (!(value %in% c("secs", "mins", "hours", "days", "weeks"))) 
        stop("invalid units specified")
    sc <- cumprod(c(secs = 1, mins = 60, hours = 60, days = 24, 
        weeks = 7))
    newx <- unclass(x) * as.vector(sc[from]/sc[value])
    .difftime(newx, value)
}


print.noquote <- function (x, quote = FALSE, right = FALSE, ...) 
{
    if (copy <- !is.null(cl <- attr(x, "class"))) {
        isNQ <- cl == "noquote"
        if (missing(right)) 
            right <- any("right" == names(cl[isNQ]))
        if (copy <- any(isNQ)) {
            ox <- x
            cl <- cl[!isNQ]
            attr(x, "class") <- if (length(cl)) 
                cl
        }
    }
    print(x, quote = quote, right = right, ...)
    invisible(if (copy) ox else x)
}


by.default <- function (data, INDICES, FUN, ..., simplify = TRUE) 
{
    dd <- as.data.frame(data)
    if (length(dim(data))) 
        by(dd, INDICES, FUN, ..., simplify = simplify)
    else {
        if (!is.list(INDICES)) {
            IND <- vector("list", 1L)
            IND[[1L]] <- INDICES
            names(IND) <- deparse(substitute(INDICES))[1L]
        }
        else IND <- INDICES
        FUNx <- function(x) FUN(dd[x, ], ...)
        nd <- nrow(dd)
        structure(eval(substitute(tapply(seq_len(nd), IND, FUNx, 
            simplify = simplify)), dd), call = match.call(), 
            class = "by")
    }
}


Ops.ordered <- function (e1, e2) 
{
    ok <- switch(.Generic, `<` = , `>` = , `<=` = , `>=` = , 
        `==` = , `!=` = TRUE, FALSE)
    if (!ok) {
        warning(sprintf("'%s' is not meaningful for ordered factors", 
            .Generic))
        return(rep.int(NA, max(length(e1), if (!missing(e2)) length(e2))))
    }
    if (.Generic %in% c("==", "!=")) 
        return(NextMethod(.Generic))
    nas <- is.na(e1) | is.na(e2)
    ord1 <- FALSE
    ord2 <- FALSE
    if (nzchar(.Method[1L])) {
        l1 <- levels(e1)
        ord1 <- TRUE
    }
    if (nzchar(.Method[2L])) {
        l2 <- levels(e2)
        ord2 <- TRUE
    }
    if (all(nzchar(.Method)) && (length(l1) != length(l2) || 
        !all(l2 == l1))) 
        stop("level sets of factors are different")
    if (ord1 && ord2) {
        e1 <- as.integer(e1)
        e2 <- as.integer(e2)
    }
    else if (!ord1) {
        e1 <- match(e1, l2)
        e2 <- as.integer(e2)
    }
    else if (!ord2) {
        e2 <- match(e2, l1)
        e1 <- as.integer(e1)
    }
    value <- get(.Generic, mode = "function")(e1, e2)
    value[nas] <- NA
    value
}


getNamespace <- function (name) 
{
    ns <- .Internal(getRegisteredNamespace(name))
    if (!is.null(ns)) 
        ns
    else loadNamespace(name)
}


as.POSIXct.default <- function (x, tz = "", ...) 
{
    if (inherits(x, "POSIXct")) 
        return(x)
    if (is.character(x) || is.factor(x)) 
        return(as.POSIXct(as.POSIXlt(x, tz, ...), tz, ...))
    if (is.logical(x) && all(is.na(x))) 
        return(.POSIXct(as.numeric(x)))
    stop(gettextf("do not know how to convert '%s' to class %s", 
        deparse(substitute(x)), dQuote("POSIXct")), domain = NA)
}


conditionMessage.condition <- function (c) 
c$message


.OptRequireMethods <- function () 
{
    pkg <- "methods"
    if (pkg %in% getOption("defaultPackages")) 
        if (!require(pkg, quietly = TRUE, warn.conflicts = FALSE, 
            character.only = TRUE)) 
            warning("package \"methods\" in options(\"defaultPackages\") was not found", 
                call. = FALSE)
}


mean.POSIXct <- function (x, ...) 
.POSIXct(mean(unclass(x), ...), attr(x, "tzone"))


as.POSIXct.POSIXlt <- function (x, tz = "", ...) 
{
    tzone <- attr(x, "tzone")
    if (missing(tz) && !is.null(tzone)) 
        tz <- tzone[1L]
    y <- .Internal(as.POSIXct(x, tz))
    names(y) <- names(x$year)
    .POSIXct(y, tz)
}


mean.POSIXlt <- function (x, ...) 
as.POSIXlt(mean(as.POSIXct(x), ...))


as.character.octmode <- function (x, ...) 
format.octmode(x, ...)


.makeMessage <- function (..., domain = NULL, appendLF = FALSE) 
{
    args <- list(...)
    msg <- if (length(args)) {
        args <- lapply(list(...), as.character)
        if (is.null(domain) || !is.na(domain)) 
            args <- .Internal(gettext(domain, unlist(args)))
        paste(args, collapse = "")
    }
    else ""
    if (appendLF) 
        paste0(msg, "\n")
    else msg
}


browserSetDebug <- function (n = 1L) 
.Internal(browserSetDebug(n))


parent.env <- function (env) 
.Internal(parent.env(env))


print.listof <- function (x, ...) 
{
    nn <- names(x)
    ll <- length(x)
    if (length(nn) != ll) 
        nn <- paste("Component", seq.int(ll))
    for (i in seq_len(ll)) {
        cat(nn[i], ":\n")
        print(x[[i]], ...)
        cat("\n")
    }
    invisible(x)
}


close.srcfile <- function (con, ...) 
{
    srcfile <- con
    conn <- srcfile$conn
    if (is.null(conn)) 
        return()
    else {
        close(conn)
        rm(list = c("conn", "line"), envir = srcfile)
    }
}


anyNA.numeric_version <- function (x, recursive = FALSE) 
{
    anyNA(.encode_numeric_version(x))
}


`*.difftime` <- function (e1, e2) 
{
    if (inherits(e1, "difftime") && inherits(e2, "difftime")) 
        stop("both arguments of * cannot be \"difftime\" objects")
    if (inherits(e2, "difftime")) {
        tmp <- e1
        e1 <- e2
        e2 <- tmp
    }
    .difftime(e2 * unclass(e1), attr(e1, "units"))
}


transform.data.frame <- function (`_data`, ...) 
{
    e <- eval(substitute(list(...)), `_data`, parent.frame())
    tags <- names(e)
    inx <- match(tags, names(`_data`))
    matched <- !is.na(inx)
    if (any(matched)) {
        `_data`[inx[matched]] <- e[matched]
        `_data` <- data.frame(`_data`)
    }
    if (!all(matched)) 
        do.call("data.frame", c(list(`_data`), e[!matched]))
    else `_data`
}


interactive <- function ()  .Primitive("interactive")


unname <- function (obj, force = FALSE) 
{
    if (!is.null(names(obj))) 
        names(obj) <- NULL
    if (!is.null(dimnames(obj)) && (force || !is.data.frame(obj))) 
        dimnames(obj) <- NULL
    obj
}


remove <- function (..., list = character(), pos = -1, envir = as.environment(pos), 
    inherits = FALSE) 
{
    dots <- match.call(expand.dots = FALSE)$...
    if (length(dots) && !all(vapply(dots, function(x) is.symbol(x) || 
        is.character(x), NA, USE.NAMES = FALSE))) 
        stop("... must contain names or character strings")
    names <- vapply(dots, as.character, "")
    if (length(names) == 0L) 
        names <- character()
    list <- .Primitive("c")(list, names)
    .Internal(remove(list, envir, inherits))
}


sample <- function (x, size, replace = FALSE, prob = NULL) 
{
    if (length(x) == 1L && is.numeric(x) && is.finite(x) && x >= 
        1) {
        if (missing(size)) 
            size <- x
        sample.int(x, size, replace, prob)
    }
    else {
        if (missing(size)) 
            size <- length(x)
        x[sample.int(length(x), size, replace, prob)]
    }
}


getNamespaceVersion <- function (ns) 
{
    ns <- asNamespace(ns)
    if (isBaseNamespace(ns)) 
        c(version = paste(R.version$major, R.version$minor, sep = "."))
    else .getNamespaceInfo(ns, "spec")["version"]
}


removeTaskCallback <- function (id) 
{
    if (!is.character(id)) 
        id <- as.integer(id)
    .Call(.C_R_removeTaskCallback, id)
}


scale <- function (x, center = TRUE, scale = TRUE) 
UseMethod("scale")


is.data.frame <- function (x) 
inherits(x, "data.frame")


textConnectionValue <- function (con) 
.Internal(textConnectionValue(con))


transform <- function (`_data`, ...) 
UseMethod("transform")


lchoose <- function (n, k) 
.Internal(lchoose(n, k))


format.POSIXct <- function (x, format = "", tz = "", usetz = FALSE, ...) 
{
    if (!inherits(x, "POSIXct")) 
        stop("wrong class")
    if (missing(tz) && !is.null(tzone <- attr(x, "tzone"))) 
        tz <- tzone
    structure(format.POSIXlt(as.POSIXlt(x, tz), format, usetz, 
        ...), names = names(x))
}


is.call <- function (x)  .Primitive("is.call")


.packages <- function (all.available = FALSE, lib.loc = NULL) 
{
    if (is.null(lib.loc)) 
        lib.loc <- .libPaths()
    if (all.available) {
        ans <- character()
        for (lib in lib.loc[file.exists(lib.loc)]) {
            a <- list.files(lib, all.files = FALSE, full.names = FALSE)
            pfile <- file.path(lib, a, "Meta", "package.rds")
            ans <- c(ans, a[file.exists(pfile)])
        }
        return(unique(ans))
    }
    s <- search()
    invisible(.rmpkg(s[substr(s, 1L, 8L) == "package:"]))
}


row.names <- function (x) 
UseMethod("row.names")


`[.octmode` <- function (x, i) 
{
    cl <- oldClass(x)
    y <- NextMethod("[")
    oldClass(y) <- cl
    y
}


str2lang <- function (s) 
.Internal(str2lang(s))


print.factor <- function (x, quote = FALSE, max.levels = NULL, width = getOption("width"), 
    ...) 
{
    ord <- is.ordered(x)
    if (length(x) == 0L) 
        cat(if (ord) 
            "ordered"
        else "factor", "(0)\n", sep = "")
    else {
        xx <- character(length(x))
        xx[] <- as.character(x)
        keepAttrs <- setdiff(names(attributes(x)), c("levels", 
            "class"))
        attributes(xx)[keepAttrs] <- attributes(x)[keepAttrs]
        print(xx, quote = quote, ...)
    }
    maxl <- if (is.null(max.levels)) 
        TRUE
    else max.levels
    if (maxl) {
        n <- length(lev <- encodeString(levels(x), quote = ifelse(quote, 
            "\"", "")))
        colsep <- if (ord) 
            " < "
        else " "
        T0 <- "Levels: "
        if (is.logical(maxl)) 
            maxl <- {
                width <- width - (nchar(T0, "w") + 3L + 1L + 
                  3L)
                lenl <- cumsum(nchar(lev, "w") + nchar(colsep, 
                  "w"))
                if (n <= 1L || lenl[n] <= width) 
                  n
                else max(1L, which.max(lenl > width) - 1L)
            }
        drop <- n > maxl
        cat(if (drop) 
            paste(format(n), ""), T0, paste(if (drop) 
            c(lev[1L:max(1, maxl - 1)], "...", if (maxl > 1) lev[n])
        else lev, collapse = colsep), "\n", sep = "")
    }
    if (!isTRUE(val <- .valid.factor(x))) 
        warning(val)
    invisible(x)
}


format.POSIXlt <- function (x, format = "", usetz = FALSE, ...) 
{
    if (!inherits(x, "POSIXlt")) 
        stop("wrong class")
    if (any(f0 <- format == "")) {
        times <- unlist(unclass(x)[1L:3L])[f0]
        secs <- x$sec[f0]
        secs <- secs[!is.na(secs)]
        np <- getOption("digits.secs")
        np <- if (is.null(np)) 
            0L
        else min(6L, np)
        if (np >= 1L) 
            for (i in seq_len(np) - 1L) if (all(abs(secs - round(secs, 
                i)) < 1e-06)) {
                np <- i
                break
            }
        format[f0] <- if (all(times[!is.na(times)] == 0)) 
            "%Y-%m-%d"
        else if (np == 0L) 
            "%Y-%m-%d %H:%M:%S"
        else paste0("%Y-%m-%d %H:%M:%OS", np)
    }
    y <- .Internal(format.POSIXlt(x, format, usetz))
    names(y) <- names(x$year)
    y
}


switch <- function (EXPR, ...)  .Primitive("switch")


getElement <- function (object, name) 
{
    if (isS4(object)) 
        methods::slot(object, name)
    else object[[name, exact = TRUE]]
}


.amatch_costs <- function (x = NULL) 
{
    costs <- c(insertions = 1, deletions = 1, substitutions = 1)
    if (!is.null(x)) {
        x <- as.list(x)
        pos <- pmatch(names(x), names(costs))
        if (anyNA(pos)) {
            warning("unknown cost components ignored")
            x <- x[!is.na(pos)]
        }
        x <- unlist(x)
        if (!all(is.numeric(x)) || any(x < 0)) 
            stop("cost components must be non-negative")
        costs[pos] <- x
    }
    costs
}


lengths <- function (x, use.names = TRUE) 
.Internal(lengths(x, use.names))


`dimnames<-` <- function (x, value)  .Primitive("dimnames<-")


save <- function (..., list = character(), file = stop("'file' must be specified"), 
    ascii = FALSE, version = NULL, envir = parent.frame(), compress = isTRUE(!ascii), 
    compression_level, eval.promises = TRUE, precheck = TRUE) 
{
    opts <- getOption("save.defaults")
    if (missing(compress) && !is.null(opts$compress)) 
        compress <- opts$compress
    if (missing(compression_level) && !is.null(opts$compression_level)) 
        compression_level <- opts$compression_level
    if (missing(ascii) && !is.null(opts$ascii)) 
        ascii <- opts$ascii
    if (missing(version)) 
        version <- opts$version
    if (!is.null(version) && version < 2) 
        warning("Use of save versions prior to 2 is deprecated", 
            domain = NA)
    names <- as.character(substitute(list(...)))[-1L]
    if (missing(list) && !length(names)) 
        warning("nothing specified to be save()d")
    list <- c(list, names)
    if (!is.null(version) && version == 1) 
        .Internal(save(list, file, ascii, version, envir, eval.promises))
    else {
        if (precheck) {
            ok <- vapply(list, exists, NA, envir = envir)
            if (!all(ok)) {
                n <- sum(!ok)
                stop(sprintf(ngettext(n, "object %s not found", 
                  "objects %s not found"), paste(sQuote(list[!ok]), 
                  collapse = ", ")), domain = NA)
            }
        }
        if (is.character(file)) {
            if (!nzchar(file)) 
                stop("'file' must be non-empty string")
            if (!is.character(compress)) {
                if (!is.logical(compress)) 
                  stop("'compress' must be logical or character")
                compress <- if (compress) 
                  "gzip"
                else "no compression"
            }
            con <- switch(compress, bzip2 = {
                if (!missing(compression_level)) bzfile(file, 
                  "wb", compression = compression_level) else bzfile(file, 
                  "wb")
            }, xz = {
                if (!missing(compression_level)) xzfile(file, 
                  "wb", compression = compression_level) else xzfile(file, 
                  "wb", compression = 9)
            }, gzip = {
                if (!missing(compression_level)) gzfile(file, 
                  "wb", compression = compression_level) else gzfile(file, 
                  "wb")
            }, `no compression` = file(file, "wb"), stop(gettextf("'compress = \"%s\"' is invalid", 
                compress)))
            on.exit(close(con))
        }
        else if (inherits(file, "connection")) 
            con <- file
        else stop("bad file argument")
        if (isOpen(con) && !ascii && summary(con)$text != "binary") 
            stop("can only save to a binary connection")
        .Internal(saveToConn(list, con, ascii, version, envir, 
            eval.promises))
    }
}


is.single <- function (x)  .Primitive("is.single")


default.stringsAsFactors <- function () 
{
    val <- getOption("stringsAsFactors")
    if (is.null(val)) 
        val <- TRUE
    if (!is.logical(val) || is.na(val) || length(val) != 1L) 
        stop("options(\"stringsAsFactors\") not set to TRUE or FALSE")
    val
}


pmatch <- function (x, table, nomatch = NA_integer_, duplicates.ok = FALSE) 
.Internal(pmatch(as.character(x), as.character(table), nomatch, 
    duplicates.ok))


withRestarts <- function (expr, ...) 
{
    docall <- function(fun, args) {
        if ((is.character(fun) && length(fun) == 1L) || is.name(fun)) 
            fun <- get(as.character(fun), envir = parent.frame(), 
                mode = "function")
        do.call("fun", lapply(args, enquote))
    }
    makeRestart <- function(name = "", handler = function(...) NULL, 
        description = "", test = function(c) TRUE, interactive = NULL) {
        structure(list(name = name, exit = NULL, handler = handler, 
            description = description, test = test, interactive = interactive), 
            class = "restart")
    }
    makeRestartList <- function(...) {
        specs <- list(...)
        names <- names(specs)
        restarts <- vector("list", length(specs))
        for (i in seq_along(specs)) {
            spec <- specs[[i]]
            name <- names[i]
            if (is.function(spec)) 
                restarts[[i]] <- makeRestart(handler = spec)
            else if (is.character(spec)) 
                restarts[[i]] <- makeRestart(description = spec)
            else if (is.list(spec)) 
                restarts[[i]] <- docall("makeRestart", spec)
            else stop("not a valid restart specification")
            restarts[[i]]$name <- name
        }
        restarts
    }
    withOneRestart <- function(expr, restart) {
        doWithOneRestart <- function(expr, restart) {
            restart$exit <- environment()
            .Internal(.addRestart(restart))
            expr
        }
        restartArgs <- doWithOneRestart(return(expr), restart)
        docall(restart$handler, restartArgs)
    }
    withRestartList <- function(expr, restarts) {
        nr <- length(restarts)
        if (nr > 1L) 
            withOneRestart(withRestartList(expr, restarts[-nr]), 
                restarts[[nr]])
        else if (nr == 1L) 
            withOneRestart(expr, restarts[[1L]])
        else expr
    }
    restarts <- makeRestartList(...)
    if (length(restarts) == 0L) 
        expr
    else if (length(restarts) == 1L) 
        withOneRestart(expr, restarts[[1L]])
    else withRestartList(expr, restarts)
}


scan <- function (file = "", what = double(), nmax = -1L, n = -1L, sep = "", 
    quote = if (identical(sep, "\n")) "" else "'\"", dec = ".", 
    skip = 0L, nlines = 0L, na.strings = "NA", flush = FALSE, 
    fill = FALSE, strip.white = FALSE, quiet = FALSE, blank.lines.skip = TRUE, 
    multi.line = TRUE, comment.char = "", allowEscapes = FALSE, 
    fileEncoding = "", encoding = "unknown", text, skipNul = FALSE) 
{
    na.strings <- as.character(na.strings)
    if (!missing(n)) {
        if (missing(nmax)) 
            nmax <- n/pmax(length(what), 1L)
        else stop("either specify 'nmax' or 'n', but not both.")
    }
    if (missing(file) && !missing(text)) {
        file <- textConnection(text, encoding = "UTF-8")
        encoding <- "UTF-8"
        on.exit(close(file))
    }
    if (is.character(file)) 
        if (file == "") 
            file <- stdin()
        else {
            file <- if (nzchar(fileEncoding)) 
                file(file, "r", encoding = fileEncoding)
            else file(file, "r")
            on.exit(close(file))
        }
    if (!inherits(file, "connection")) 
        stop("'file' must be a character string or connection")
    .Internal(scan(file, what, nmax, sep, dec, quote, skip, nlines, 
        na.strings, flush, fill, strip.white, quiet, blank.lines.skip, 
        multi.line, comment.char, allowEscapes, encoding, skipNul))
}


is.null <- function (x)  .Primitive("is.null")


sink.number <- function (type = c("output", "message")) 
{
    type <- match.arg(type)
    .Internal(sink.number(type != "message"))
}


pretty <- function (x, ...) 
UseMethod("pretty")


Summary.Date <- function (..., na.rm) 
{
    ok <- switch(.Generic, max = , min = , range = TRUE, FALSE)
    if (!ok) 
        stop(gettextf("%s not defined for \"Date\" objects", 
            .Generic), domain = NA)
    .Date(NextMethod(.Generic), oldClass(list(...)[[1L]]))
}


library.dynam.unload <- function (chname, libpath, verbose = getOption("verbose"), file.ext = .Platform$dynlib.ext) 
{
    dll_list <- .dynLibs()
    if (missing(chname) || nchar(chname, "c") == 0L) 
        if (.Platform$OS.type == "windows") 
            stop("no DLL was specified")
        else stop("no shared object was specified")
    libpath <- normalizePath(libpath, "/", TRUE)
    chname1 <- paste0(chname, file.ext)
    file <- if (nzchar(.Platform$r_arch)) 
        file.path(libpath, "libs", .Platform$r_arch, chname1)
    else file.path(libpath, "libs", chname1)
    pos <- which(vapply(dll_list, function(x) x[["path"]] == 
        file, NA))
    if (!length(pos)) 
        if (.Platform$OS.type == "windows") 
            stop(gettextf("DLL %s was not loaded", sQuote(chname1)), 
                domain = NA)
        else stop(gettextf("shared object %s was not loaded", 
            sQuote(chname1)), domain = NA)
    if (!file.exists(file)) 
        if (.Platform$OS.type == "windows") 
            stop(gettextf("DLL %s not found", sQuote(chname1)), 
                domain = NA)
        else stop(gettextf("shared object '%s' not found", sQuote(chname1)), 
            domain = NA)
    if (verbose) 
        message(gettextf("now dyn.unload(\"%s\") ...", file), 
            domain = NA)
    dyn.unload(file)
    .dynLibs(dll_list[-pos])
    invisible(dll_list[[pos]])
}


letters <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", 
"m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", 
"z")


diff.difftime <- function (x, ...) 
.difftime(NextMethod("diff"), attr(x, "units"), oldClass(x))


browserCondition <- function (n = 1L) 
.Internal(browserCondition(n))


dir.exists <- function (paths) 
.Internal(dir.exists(paths))


print.srcref <- function (x, useSource = TRUE, ...) 
{
    cat(as.character(x, useSource = useSource), sep = "\n")
    invisible(x)
}


arrayInd <- function (ind, .dim, .dimnames = NULL, useNames = FALSE) 
{
    m <- length(ind)
    rank <- length(.dim)
    wh1 <- ind - 1L
    ind <- 1L + wh1%%.dim[1L]
    dnms <- if (useNames) {
        list(.dimnames[[1L]][ind], if (any(nzchar(nd <- names(.dimnames)))) nd else if (rank == 
            2L) c("row", "col") else paste0("dim", seq_len(rank)))
    }
    ind <- matrix(ind, nrow = m, ncol = rank, dimnames = dnms)
    if (rank >= 2L) {
        denom <- 1L
        for (i in 2L:rank) {
            denom <- denom * .dim[i - 1L]
            nextd1 <- wh1%/%denom
            ind[, i] <- 1L + nextd1%%.dim[i]
        }
    }
    storage.mode(ind) <- "integer"
    ind
}


casefold <- function (x, upper = FALSE) 
if (upper) toupper(x) else tolower(x)


anyNA <- function (x, recursive = FALSE)  .Primitive("anyNA")


suppressWarnings <- function (expr) 
{
    ops <- options(warn = -1)
    on.exit(options(ops))
    withCallingHandlers(expr, warning = function(w) invokeRestart("muffleWarning"))
}


seek <- function (con, ...) 
UseMethod("seek")


.readRDS <- function (...) 
.Defunct("readRDS")


is.language <- function (x)  .Primitive("is.language")


xtfrm.numeric_version <- function (x) 
{
    x <- .encode_numeric_version(x)
    NextMethod("xtfrm")
}


get0 <- function (x, envir = pos.to.env(-1L), mode = "any", inherits = TRUE, 
    ifnotfound = NULL) 
.Internal(get0(x, envir, mode, inherits, ifnotfound))


as.vector <- function (x, mode = "any") 
.Internal(as.vector(x, mode))


strftime <- function (x, format = "", tz = "", usetz = FALSE, ...) 
format(as.POSIXlt(x, tz = tz), format = format, usetz = usetz, 
    ...)


readRDS <- function (file, refhook = NULL) 
{
    if (is.character(file)) {
        con <- gzfile(file, "rb")
        on.exit(close(con))
    }
    else if (inherits(file, "connection")) 
        con <- if (inherits(file, "url")) 
            gzcon(file)
        else file
    else stop("bad 'file' argument")
    .Internal(unserializeFromConn(con, refhook))
}


`split<-.default` <- function (x, f, drop = FALSE, ..., value) 
{
    ix <- split(seq_along(x), f, drop = drop, ...)
    n <- length(value)
    j <- 0
    for (i in ix) {
        j <- j%%n + 1
        x[i] <- value[[j]]
    }
    x
}


is.pairlist <- function (x)  .Primitive("is.pairlist")


chartr <- function (old, new, x) 
{
    if (!is.character(x)) 
        x <- as.character(x)
    .Internal(chartr(old, new, x))
}


rep.Date <- function (x, ...) 
{
    .Date(NextMethod(), oldClass(x))
}


isFALSE <- function (x) 
is.logical(x) && length(x) == 1L && !is.na(x) && !x


as.pairlist <- function (x) 
.Internal(as.vector(x, "pairlist"))


as.octmode <- function (x) 
{
    if (inherits(x, "octmode")) 
        return(x)
    if (is.double(x) && all(is.na(x) | x == as.integer(x))) 
        x <- as.integer(x)
    if (is.integer(x)) 
        return(structure(x, class = "octmode"))
    if (is.character(x)) {
        z <- strtoi(x, 8L)
        if (!any(is.na(z) | z < 0)) 
            return(structure(z, class = "octmode"))
    }
    stop("'x' cannot be coerced to class \"octmode\"")
}


is.na.numeric_version <- function (x) 
is.na(.encode_numeric_version(x))


length <- function (x)  .Primitive("length")


formals <- function (fun = sys.function(sys.parent()), envir = parent.frame()) 
{
    if (is.character(fun)) 
        fun <- get(fun, mode = "function", envir = envir)
    .Internal(formals(fun))
}


rawConnectionValue <- function (con) 
.Internal(rawConnectionValue(con))


Math.POSIXt <- function (x, ...) 
{
    stop(gettextf("'%s' not defined for \"POSIXt\" objects", 
        .Generic), domain = NA)
}


Sys.chmod <- function (paths, mode = "0777", use_umask = TRUE) 
.Internal(Sys.chmod(paths, as.octmode(mode), use_umask))


formatC <- function (x, digits = NULL, width = NULL, format = NULL, flag = "", 
    mode = NULL, big.mark = "", big.interval = 3L, small.mark = "", 
    small.interval = 5L, decimal.mark = getOption("OutDec"), 
    preserve.width = "individual", zero.print = NULL, replace.zero = TRUE, 
    drop0trailing = FALSE) 
{
    if (is.object(x)) {
        if (!(is.atomic(x) || inherits(x, "vector"))) 
            warning("class of 'x' was discarded")
        x <- unclass(x)
    }
    flag <- as.character(flag)
    if (length(flag) != 1) 
        stop("'flag' must be a string, i.e., of length 1")
    nf <- strsplit(flag, "")[[1L]]
    if (!all(nf %in% c("0", "+", "-", " ", "#", "'", "I"))) 
        stop("'flag' should contain only characters from [0+- #'I]")
    format.char <- function(x, width, flag) {
        if (is.null(width)) 
            width <- 0L
        else if (width < 0L) {
            flag <- "-"
            width <- -width
        }
        format.default(x, width = width, justify = if (flag == 
            "-") 
            "left"
        else "right")
    }
    if (!(n <- length(x))) 
        return("")
    if (is.null(mode)) 
        mode <- storage.mode(x)
    else if (any(mode == c("double", "real", "integer"))) {
        if (mode == "real") 
            mode <- "double"
        storage.mode(x) <- mode
    }
    else if (mode != "character") 
        stop("'mode' must be \"double\" (\"real\"), \"integer\" or \"character\"")
    if (mode == "character" || (!is.null(format) && format == 
        "s")) {
        if (mode != "character") {
            warning("coercing argument to \"character\" for format=\"s\"")
            x <- as.character(x)
        }
        return(format.char(x, width = width, flag = flag))
    }
    if (missing(format) || is.null(format)) 
        format <- if (mode == "integer") 
            "d"
        else "g"
    else {
        if (any(format == c("f", "e", "E", "g", "G", "fg"))) {
            if (mode == "integer") 
                mode <- storage.mode(x) <- "double"
        }
        else if (format == "d") {
            if (mode != "integer") 
                mode <- storage.mode(x) <- "integer"
        }
        else stop("'format' must be one of {\"f\",\"e\",\"E\",\"g\",\"G\", \"fg\", \"s\"}")
    }
    some.special <- !all(Ok <- is.finite(x))
    if (some.special) {
        rQ <- as.character(x[!Ok])
        rQ[is.na(rQ)] <- "NA"
        x[!Ok] <- as.vector(0, mode = mode)
    }
    if (is.null(width) && is.null(digits)) 
        width <- 1L
    if (is.null(digits)) 
        digits <- if (mode == "integer") 
            2L
        else 4L
    else if (digits < 0L) 
        digits <- 6L
    else {
        maxDigits <- if (format != "f") 
            50L
        else ceiling(-(.Machine$double.neg.ulp.digits + .Machine$double.min.exp)/log2(10))
        if (digits > maxDigits) {
            warning(gettextf("'digits' reduced to %d", maxDigits), 
                domain = NA)
            digits <- maxDigits
        }
    }
    if (is.null(width)) 
        width <- digits + 1L
    else if (width == 0L) 
        width <- digits
    i.strlen <- pmax(abs(as.integer(width)), if (format == "fg" || 
        format == "f") {
        xEx <- as.integer(floor(log10(abs(x + (x == 0)))))
        as.integer(x < 0 | flag != "") + digits + if (format == 
            "f") {
            2L + pmax(xEx, 0L)
        }
        else {
            1L + pmax(xEx, digits, digits + (-xEx) + 1L) + length(nf)
        }
    }
    else rep.int(digits + 8L, n))
    if (digits > 0 && any(nf == "#")) 
        digits <- -digits
    attr(x, "Csingle") <- NULL
    r <- .Internal(formatC(x, as.character(mode), width, digits, 
        as.character(format), flag, i.strlen))
    if (some.special) 
        r[!Ok] <- format.char(rQ, width = width, flag = flag)
    if (nzchar(big.mark) || nzchar(small.mark) || decimal.mark != 
        "." || !is.null(zero.print) || drop0trailing) 
        r <- prettyNum(r, big.mark = big.mark, big.interval = big.interval, 
            small.mark = small.mark, small.interval = small.interval, 
            decimal.mark = decimal.mark, input.d.mark = ".", 
            preserve.width = preserve.width, zero.print = zero.print, 
            replace.zero = replace.zero, drop0trailing = drop0trailing, 
            is.cmplx = FALSE)
    if (!is.null(x.atr <- attributes(x))) 
        attributes(r) <- x.atr
    r
}


sign <- function (x)  .Primitive("sign")


sinh <- function (x)  .Primitive("sinh")


sink <- function (file = NULL, append = FALSE, type = c("output", "message"), 
    split = FALSE) 
{
    type <- match.arg(type)
    if (type == "message") {
        if (is.null(file)) 
            file <- stderr()
        else if (!inherits(file, "connection") || !isOpen(file)) 
            stop("'file' must be NULL or an already open connection")
        if (split) 
            stop("cannot split the message connection")
        .Internal(sink(file, FALSE, TRUE, FALSE))
    }
    else {
        closeOnExit <- FALSE
        if (is.null(file)) 
            file <- -1L
        else if (is.character(file)) {
            file <- file(file, if (append) 
                "a"
            else "w")
            closeOnExit <- TRUE
        }
        else if (!inherits(file, "connection")) 
            stop("'file' must be NULL, a connection or a character string")
        .Internal(sink(file, closeOnExit, FALSE, split))
    }
}


solve <- function (a, b, ...) 
UseMethod("solve")


.External.graphics <- function (.NAME, ..., PACKAGE)  .Primitive(".External.graphics")


as.data.frame.default <- function (x, ...) 
stop(gettextf("cannot coerce class %s to a data.frame", sQuote(deparse(class(x))[1L])), 
    domain = NA)


is.numeric.Date <- function (x) 
FALSE


all.equal.character <- function (target, current, ..., check.attributes = TRUE) 
{
    if (!is.logical(check.attributes)) 
        stop(gettextf("'%s' must be logical", "check.attributes"), 
            domain = NA)
    msg <- if (check.attributes) 
        attr.all.equal(target, current, ...)
    if (data.class(target) != data.class(current)) {
        msg <- c(msg, paste0("target is ", data.class(target), 
            ", current is ", data.class(current)))
        return(msg)
    }
    lt <- length(target)
    lc <- length(current)
    if (lt != lc) {
        if (!is.null(msg)) 
            msg <- msg[-grep("\\bLengths\\b", msg)]
        msg <- c(msg, paste0("Lengths (", lt, ", ", lc, ") differ (string compare on first ", 
            ll <- min(lt, lc), ")"))
        ll <- seq_len(ll)
        target <- target[ll]
        current <- current[ll]
    }
    nas <- is.na(target)
    nasc <- is.na(current)
    if (any(nas != nasc)) {
        msg <- c(msg, paste("'is.NA' value mismatch:", sum(nasc), 
            "in current", sum(nas), "in target"))
        return(msg)
    }
    ne <- !nas & (target != current)
    if (!any(ne) && is.null(msg)) 
        TRUE
    else if (sum(ne) == 1L) 
        c(msg, paste("1 string mismatch"))
    else if (sum(ne) > 1L) 
        c(msg, paste(sum(ne), "string mismatches"))
    else msg
}


unsplit <- function (value, f, drop = FALSE) 
{
    len <- length(if (is.list(f)) f[[1L]] else f)
    if (is.data.frame(value[[1L]])) {
        x <- value[[1L]][rep(NA, len), , drop = FALSE]
        rownames(x) <- unsplit(lapply(value, rownames), f, drop = drop)
    }
    else x <- value[[1L]][rep(NA, len)]
    split(x, f, drop = drop) <- value
    x
}


as.hexmode <- function (x) 
{
    if (inherits(x, "hexmode")) 
        return(x)
    if (is.double(x) && all(is.na(x) | x == as.integer(x))) 
        x <- as.integer(x)
    if (is.integer(x)) 
        return(structure(x, class = "hexmode"))
    if (is.character(x)) {
        z <- strtoi(x, 16L)
        if (!any(is.na(z) | z < 0)) 
            return(structure(z, class = "hexmode"))
    }
    stop("'x' cannot be coerced to class \"hexmode\"")
}


globalenv <- function ()  .Primitive("globalenv")


list.files <- function (path = ".", pattern = NULL, all.files = FALSE, full.names = FALSE, 
    recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE, 
    no.. = FALSE) 
.Internal(list.files(path, pattern, all.files, full.names, recursive, 
    ignore.case, include.dirs, no..))


loadingNamespaceInfo <- function () 
{
    dynGet("__LoadingNamespaceInfo__", stop("not loading a namespace"))
}


packageEvent <- function (pkgname, event = c("onLoad", "attach", "detach", "onUnload")) 
{
    event <- match.arg(event)
    pkgname <- strsplit(pkgname, "_", fixed = TRUE)[[1L]][1L]
    paste("UserHook", pkgname, event, sep = "::")
}


make.unique <- function (names, sep = ".") 
.Internal(make.unique(names, sep))


.decode_numeric_version <- function (x) 
{
    width <- attr(x, "width")
    y <- Map(function(elt, len) {
        if (is.na(elt)) 
            return(integer())
        first <- seq(from = 1L, length.out = len, by = width)
        last <- seq(from = width, length.out = len, by = width)
        strtoi(substring(elt, first, last), 8L)
    }, x, attr(x, "lens"))
    names(y) <- names(x)
    class(y) <- unique(c(attr(x, ".classes"), "numeric_version"))
    y
}


logical <- function (length = 0L) 
.Internal(vector("logical", length))


as.vector.factor <- function (x, mode = "any") 
{
    if (mode == "list") 
        as.list(x)
    else if (mode == "any" || mode == "character" || mode == 
        "logical") 
        as.vector(levels(x)[x], mode)
    else as.vector(unclass(x), mode)
}


noquote <- function (obj, right = FALSE) 
{
    if (!inherits(obj, "noquote")) 
        class(obj) <- c(attr(obj, "class"), if (right) c(right = "noquote") else "noquote")
    obj
}


as.qr <- function (x) 
stop("you cannot be serious", domain = NA)


`class<-` <- function (x, value)  .Primitive("class<-")


`[[.Date` <- function (x, ..., drop = TRUE) 
{
    .Date(NextMethod("[["), oldClass(x))
}


suppressPackageStartupMessages <- function (expr) 
withCallingHandlers(expr, packageStartupMessage = function(c) invokeRestart("muffleMessage"))


as.null.default <- function (x, ...) 
NULL


c.difftime <- function (..., recursive = FALSE) 
{
    coerceTimeUnit <- function(x) {
        switch(attr(x, "units"), secs = x, mins = 60 * x, hours = 60 * 
            60 * x, days = 60 * 60 * 24 * x, weeks = 60 * 60 * 
            24 * 7 * x)
    }
    args <- list(...)
    if (!length(args)) 
        return(.difftime(double(), "secs"))
    ind <- sapply(args, inherits, "difftime")
    pos <- which(!ind)
    units <- sapply(args[ind], attr, "units")
    if (all(units == (un1 <- units[1L]))) {
        if (length(pos)) 
            args[pos] <- lapply(args[pos], as.difftime, units = un1)
        .difftime(unlist(args), un1)
    }
    else {
        if (length(pos)) 
            args[pos] <- lapply(args[pos], as.difftime, units = "secs")
        args[ind] <- lapply(args[ind], coerceTimeUnit)
        .difftime(unlist(args), "secs")
    }
}


as.data.frame.data.frame <- function (x, row.names = NULL, ...) 
{
    cl <- oldClass(x)
    i <- match("data.frame", cl)
    if (i > 1L) 
        class(x) <- cl[-(1L:(i - 1L))]
    if (!is.null(row.names)) {
        nr <- .row_names_info(x, 2L)
        if (length(row.names) == nr) 
            attr(x, "row.names") <- row.names
        else stop(sprintf(ngettext(nr, "invalid 'row.names', length %d for a data frame with %d row", 
            "invalid 'row.names', length %d for a data frame with %d rows"), 
            length(row.names), nr), domain = NA)
    }
    x
}


summary.warnings <- function (object, ...) 
{
    msgs <- names(object)
    calls <- as.character(object)
    ss <- ": "
    c.m. <- paste(calls, msgs, sep = ss)
    if (length(i.no.call <- which(calls == "NULL"))) 
        c.m.[i.no.call] <- substr(c.m.[i.no.call], nchar(paste0("NULL", 
            ss)) + 1L, 100000L)
    tm <- table(c.m., deparse.level = 0L)
    structure(unique(object), counts = as.vector(tm), class = "summary.warnings")
}


file.show <- function (..., header = rep("", nfiles), title = "R Information", 
    delete.file = FALSE, pager = getOption("pager"), encoding = "") 
{
    files <- path.expand(c(...))
    nfiles <- length(files)
    if (nfiles == 0L) 
        return(invisible(NULL))
    if (l10n_info()[["UTF-8"]] && encoding == "UTF-8") 
        encoding <- ""
    if (l10n_info()[["Latin-1"]] && encoding == "latin1") 
        encoding <- ""
    if (!is.na(encoding) && nzchar(encoding)) {
        for (i in seq_along(files)) {
            f <- files[i]
            tf <- tempfile()
            tmp <- readLines(f, warn = FALSE)
            tmp2 <- try(iconv(tmp, encoding, "", "byte"))
            if (inherits(tmp2, "try-error")) 
                file.copy(f, tf)
            else writeLines(tmp2, tf)
            files[i] <- tf
            if (delete.file) 
                unlink(f)
        }
        delete.file <- TRUE
    }
    if (is.function(pager)) 
        pager(files, header = header, title = title, delete.file = delete.file)
    else .Internal(file.show(files, header, title, delete.file, 
        pager))
}


`.rowNamesDF<-` <- function (x, make.names = FALSE, value) 
{
    if (!is.data.frame(x)) 
        x <- as.data.frame(x)
    n <- .row_names_info(x, 2L)
    if (is.null(value)) {
        attr(x, "row.names") <- .set_row_names(n)
        return(x)
    }
    if (is.object(value) || !is.integer(value)) 
        value <- as.character(value)
    if (n == 0L) {
        if (!is.null(attr(x, "row.names")) && length(value) > 
            0L) 
            stop("invalid 'row.names' length")
    }
    else if (length(value) != n) {
        if (isFALSE(make.names)) 
            stop("invalid 'row.names' length")
        else if (is.na(make.names)) {
            attr(x, "row.names") <- .set_row_names(n)
            return(x)
        }
        else if (!isTRUE(make.names)) 
            stop("invalid 'make.names'")
        else if ((nv <- length(value)) < n) 
            value <- c(value, rep_len(value[nv], n - nv))
        else value <- value[seq_len(n)]
    }
    if (anyDuplicated(value)) {
        if (isFALSE(make.names)) {
            nonuniq <- sort(unique(value[duplicated(value)]))
            warning(ngettext(length(nonuniq), sprintf("non-unique value when setting 'row.names': %s", 
                sQuote(nonuniq[1L])), sprintf("non-unique values when setting 'row.names': %s", 
                paste(sQuote(nonuniq), collapse = ", "))), domain = NA, 
                call. = FALSE)
            stop("duplicate 'row.names' are not allowed")
        }
        else if (is.na(make.names)) {
            value <- .set_row_names(if (n == 0L && is.null(.row_names_info(x, 
                0L)) && length(x) > 0L) 
                length(x[[1L]])
            else n)
        }
        else if (!isTRUE(make.names)) 
            stop("invalid 'make.names'")
        else value <- make.names(value, unique = TRUE)
    }
    else if (anyNA(value)) {
        if (isFALSE(make.names)) 
            stop("missing values in 'row.names' are not allowed")
        if (is.na(make.names)) 
            value <- .set_row_names(n)
        else if (!isTRUE(make.names)) 
            stop("invalid 'make.names'")
        else value <- make.names(value, unique = TRUE)
    }
    attr(x, "row.names") <- value
    x
}


bindingIsLocked <- function (sym, env) 
{
    if (is.character(sym)) 
        sym <- as.name(sym)
    .Internal(bindingIsLocked(sym, env))
}


.maskedMsg <- function (same, pkg, by) 
{
    objs <- strwrap(paste(same, collapse = ", "), indent = 4L, 
        exdent = 4L)
    txt <- if (by) {
        ngettext(length(same), "The following object is masked _by_ %s:\n\n%s\n", 
            "The following objects are masked _by_ %s:\n\n%s\n")
    }
    else {
        ngettext(length(same), "The following object is masked from %s:\n\n%s\n", 
            "The following objects are masked from %s:\n\n%s\n")
    }
    sprintf(txt, pkg, paste(objs, collapse = "\n"))
}


.colSums <- function (x, m, n, na.rm = FALSE) 
.Internal(colSums(x, m, n, na.rm))


acosh <- function (x)  .Primitive("acosh")


bzfile <- function (description, open = "", encoding = getOption("encoding"), 
    compression = 9) 
.Internal(bzfile(description, open, encoding, compression))


intersect <- function (x, y) 
{
    y <- as.vector(y)
    unique(y[match(as.vector(x), y, 0L)])
}


by.data.frame <- function (data, INDICES, FUN, ..., simplify = TRUE) 
{
    if (!is.list(INDICES)) {
        IND <- vector("list", 1L)
        IND[[1L]] <- INDICES
        names(IND) <- deparse(substitute(INDICES))[1L]
    }
    else IND <- INDICES
    FUNx <- function(x) FUN(data[x, , drop = FALSE], ...)
    nd <- nrow(data)
    structure(eval(substitute(tapply(seq_len(nd), IND, FUNx, 
        simplify = simplify)), data), call = match.call(), class = "by")
}


summary.srcref <- function (object, useSource = FALSE, ...) 
{
    cat(as.character(object, useSource = useSource), sep = "\n")
    invisible(object)
}


`units<-` <- function (x, value) 
UseMethod("units<-")


withAutoprint <- function (exprs, evaluated = FALSE, local = parent.frame(), print. = TRUE, 
    echo = TRUE, max.deparse.length = Inf, width.cutoff = max(20, 
        getOption("width")), deparseCtrl = c("keepInteger", "showAttributes", 
        "keepNA"), ...) 
{
    if (!evaluated) {
        exprs <- substitute(exprs)
        if (is.call(exprs)) {
            if (exprs[[1]] == quote(`{`)) 
                exprs <- as.list(exprs[-1])
        }
    }
    source(exprs = exprs, local = local, print.eval = print., 
        echo = echo, max.deparse.length = max.deparse.length, 
        width.cutoff = width.cutoff, deparseCtrl = deparseCtrl, 
        ...)
}


as.table.default <- function (x, ...) 
{
    if (is.table(x)) 
        return(x)
    else if (is.array(x) || is.numeric(x)) {
        x <- as.array(x)
        structure(class = c("table", oldClass(x)), provideDimnames(x))
    }
    else stop("cannot coerce to a table")
}


kappa.default <- function (z, exact = FALSE, norm = NULL, method = c("qr", "direct"), 
    ...) 
{
    method <- match.arg(method)
    z <- as.matrix(z)
    norm <- if (!is.null(norm)) 
        match.arg(norm, c("2", "1", "O", "I"))
    else "2"
    if (exact && norm == "2") {
        s <- svd(z, nu = 0, nv = 0)$d
        max(s)/min(s[s > 0])
    }
    else {
        if (exact) 
            warning(gettextf("norm '%s' currently always uses exact = FALSE", 
                norm))
        d <- dim(z)
        if (method == "qr" || d[1L] != d[2L]) 
            kappa.qr(qr(if (d[1L] < d[2L]) 
                t(z)
            else z), exact = FALSE, norm = norm, ...)
        else .kappa_tri(z, exact = FALSE, norm = norm, ...)
    }
}


as.list.Date <- function (x, ...) 
lapply(unclass(x), .Date, oldClass(x))


.Primitive <- function (name)  .Primitive(".Primitive")


getDLLRegisteredRoutines.character <- function (dll, addNames = TRUE) 
{
    dlls <- getLoadedDLLs()
    w <- vapply(dlls, function(x) x[["name"]] == dll || x[["path"]] == 
        dll, NA)
    if (!any(w)) 
        stop(gettextf("No DLL currently loaded with name or path %s", 
            sQuote(dll)), domain = NA)
    dll <- which.max(w)
    if (sum(w) > 1L) 
        warning(gettextf("multiple DLLs match '%s'. Using '%s'", 
            names(dll), dlls[[dll]][["path"]]), domain = NA)
    getDLLRegisteredRoutines(dlls[[dll]], addNames)
}


.saveRDS <- function (...) 
.Defunct("saveRDS")


gctorture2 <- function (step, wait = step, inhibit_release = FALSE) 
.Internal(gctorture2(step, wait, inhibit_release))


Conj <- function (z)  .Primitive("Conj")


normalizePath <- function (path, winslash = "\\", mustWork = NA) 
.Internal(normalizePath(path.expand(path), winslash, mustWork))


sort <- function (x, decreasing = FALSE, ...) 
{
    if (!is.logical(decreasing) || length(decreasing) != 1L) 
        stop("'decreasing' must be a length-1 logical vector.\nDid you intend to set 'partial'?")
    UseMethod("sort")
}


addNA <- function (x, ifany = FALSE) 
{
    if (!is.factor(x)) 
        x <- factor(x)
    if (ifany && !anyNA(x)) 
        return(x)
    ll <- levels(x)
    if (!anyNA(ll)) 
        ll <- c(ll, NA)
    else if (!ifany && !anyNA(x)) 
        return(x)
    factor(x, levels = ll, exclude = NULL)
}


is.matrix <- function (x)  .Primitive("is.matrix")


`[<-.factor` <- function (x, ..., value) 
{
    lx <- levels(x)
    cx <- oldClass(x)
    if (is.factor(value)) 
        value <- levels(value)[value]
    m <- match(value, lx)
    if (any(is.na(m) & !is.na(value))) 
        warning("invalid factor level, NA generated")
    class(x) <- NULL
    x[...] <- m
    attr(x, "levels") <- lx
    class(x) <- cx
    x
}


writeLines <- function (text, con = stdout(), sep = "\n", useBytes = FALSE) 
{
    if (!is.character(text)) 
        stop("can only write character objects")
    if (is.character(con)) {
        con <- file(con, "w")
        on.exit(close(con))
    }
    .Internal(writeLines(text, con, sep, useBytes))
}


colSums <- function (x, na.rm = FALSE, dims = 1L) 
{
    if (is.data.frame(x)) 
        x <- as.matrix(x)
    if (!is.array(x) || length(dn <- dim(x)) < 2L) 
        stop("'x' must be an array of at least two dimensions")
    if (dims < 1L || dims > length(dn) - 1L) 
        stop("invalid 'dims'")
    n <- prod(dn[id <- seq_len(dims)])
    dn <- dn[-id]
    z <- if (is.complex(x)) 
        .Internal(colSums(Re(x), n, prod(dn), na.rm)) + (0+1i) * 
            .Internal(colSums(Im(x), n, prod(dn), na.rm))
    else .Internal(colSums(x, n, prod(dn), na.rm))
    if (length(dn) > 1L) {
        dim(z) <- dn
        dimnames(z) <- dimnames(x)[-id]
    }
    else names(z) <- dimnames(x)[[dims + 1L]]
    z
}


saveRDS <- function (object, file = "", ascii = FALSE, version = NULL, compress = TRUE, 
    refhook = NULL) 
{
    if (is.character(file)) {
        if (file == "") 
            stop("'file' must be non-empty string")
        object <- object
        mode <- if (ascii %in% FALSE) 
            "wb"
        else "w"
        con <- if (is.logical(compress)) 
            if (compress) 
                gzfile(file, mode)
            else file(file, mode)
        else switch(compress, bzip2 = bzfile(file, mode), xz = xzfile(file, 
            mode), gzip = gzfile(file, mode), stop("invalid 'compress' argument: ", 
            compress))
        on.exit(close(con))
    }
    else if (inherits(file, "connection")) {
        if (!missing(compress)) 
            warning("'compress' is ignored unless 'file' is a file name")
        con <- file
    }
    else stop("bad 'file' argument")
    .Internal(serializeToConn(object, con, ascii, version, refhook))
}


suppressMessages <- function (expr) 
withCallingHandlers(expr, message = function(c) invokeRestart("muffleMessage"))


print.data.frame <- function (x, ..., digits = NULL, quote = FALSE, right = TRUE, 
    row.names = TRUE, max = NULL) 
{
    n <- length(row.names(x))
    if (length(x) == 0L) {
        cat(sprintf(ngettext(n, "data frame with 0 columns and %d row", 
            "data frame with 0 columns and %d rows"), n), "\n", 
            sep = "")
    }
    else if (n == 0L) {
        print.default(names(x), quote = FALSE)
        cat(gettext("<0 rows> (or 0-length row.names)\n"))
    }
    else {
        if (is.null(max)) 
            max <- getOption("max.print", 99999L)
        if (!is.finite(max)) 
            stop("invalid 'max' / getOption(\"max.print\"): ", 
                max)
        omit <- (n0 <- max%/%length(x)) < n
        m <- as.matrix(format.data.frame(if (omit) 
            x[seq_len(n0), , drop = FALSE]
        else x, digits = digits, na.encode = FALSE))
        if (!isTRUE(row.names)) 
            dimnames(m)[[1L]] <- if (isFALSE(row.names)) 
                rep.int("", if (omit) 
                  n0
                else n)
            else row.names
        print(m, ..., quote = quote, right = right, max = max)
        if (omit) 
            cat(" [ reached 'max' / getOption(\"max.print\") -- omitted", 
                n - n0, "rows ]\n")
    }
    invisible(x)
}


`[[<-.factor` <- function (x, ..., value) 
{
    lx <- levels(x)
    cx <- oldClass(x)
    if (is.factor(value)) 
        value <- levels(value)[value]
    m <- match(value, lx)
    if (any(is.na(m) & !is.na(value))) 
        warning("invalid factor level, NA generated")
    class(x) <- NULL
    x[[...]] <- m
    attr(x, "levels") <- lx
    class(x) <- cx
    x
}


`$.DLLInfo` <- function (x, name) 
getNativeSymbolInfo(as.character(name), PACKAGE = x)


`%*%` <- function (x, y)  .Primitive("%*%")


undebug <- function (fun, signature = NULL) 
{
    if (is.null(signature)) 
        .Internal(undebug(fun))
    else if (requireNamespace("methods")) 
        methods::.undebugMethod(fun, signature = signature)
    else stop("failed to load methods package for undebugging by signature")
}


`$<-` <- .Primitive("$<-")


`%/%` <- function (e1, e2)  .Primitive("%/%")


tanh <- function (x)  .Primitive("tanh")


is.loaded <- function (symbol, PACKAGE = "", type = "") 
.Internal(is.loaded(symbol, PACKAGE, type))


curlGetHeaders <- function (url, redirect = TRUE, verify = TRUE) 
.Internal(curlGetHeaders(url, redirect, verify))


sqrt <- function (x)  .Primitive("sqrt")


sample.int <- function (n, size = n, replace = FALSE, prob = NULL, useHash = (!replace && 
    is.null(prob) && size <= n/2 && n > 1e+07)) 
{
    if (useHash) 
        .Internal(sample2(n, size))
    else .Internal(sample(n, size, replace, prob))
}


grep <- function (pattern, x, ignore.case = FALSE, perl = FALSE, value = FALSE, 
    fixed = FALSE, useBytes = FALSE, invert = FALSE) 
{
    if (!is.character(x)) 
        x <- structure(as.character(x), names = names(x))
    .Internal(grep(as.character(pattern), x, ignore.case, value, 
        perl, fixed, useBytes, invert))
}


xtfrm.POSIXct <- function (x) 
as.numeric(x)


is.logical <- function (x)  .Primitive("is.logical")


print.proc_time <- function (x, ...) 
{
    print(summary(x, ...))
    invisible(x)
}


Sys.readlink <- function (paths) 
.Internal(Sys.readlink(paths))


strsplit <- function (x, split, fixed = FALSE, perl = FALSE, useBytes = FALSE) 
.Internal(strsplit(x, as.character(split), fixed, perl, useBytes))


xtfrm.POSIXlt <- function (x) 
as.double(x)


Cstack_info <- function () 
.Internal(Cstack_info())


dir.create <- function (path, showWarnings = TRUE, recursive = FALSE, mode = "0777") 
.Internal(dir.create(path, showWarnings, recursive, as.octmode(mode)))


nlevels <- function (x) 
length(levels(x))


aperm <- function (a, perm, ...) 
UseMethod("aperm")


xtfrm <- function (x)  .Primitive("xtfrm")


Summary.data.frame <- function (..., na.rm) 
{
    args <- list(...)
    args <- lapply(args, function(x) {
        x <- as.matrix(x)
        if (!is.numeric(x) && !is.complex(x)) 
            stop("only defined on a data frame with all numeric variables")
        x
    })
    do.call(.Generic, c(args, na.rm = na.rm))
}


gsub <- function (pattern, replacement, x, ignore.case = FALSE, perl = FALSE, 
    fixed = FALSE, useBytes = FALSE) 
{
    if (!is.character(x)) 
        x <- as.character(x)
    .Internal(gsub(as.character(pattern), as.character(replacement), 
        x, ignore.case, perl, fixed, useBytes))
}


merge.data.frame <- function (x, y, by = intersect(names(x), names(y)), by.x = by, 
    by.y = by, all = FALSE, all.x = all, all.y = all, sort = TRUE, 
    suffixes = c(".x", ".y"), no.dups = TRUE, incomparables = NULL, 
    ...) 
{
    fix.by <- function(by, df) {
        if (is.null(by)) 
            by <- numeric()
        by <- as.vector(by)
        nc <- ncol(df)
        if (is.character(by)) {
            poss <- c("row.names", names(df))
            if (any(bad <- !charmatch(by, poss, 0L))) 
                stop(ngettext(sum(bad), "'by' must specify a uniquely valid column", 
                  "'by' must specify uniquely valid columns"), 
                  domain = NA)
            by <- match(by, poss) - 1L
        }
        else if (is.numeric(by)) {
            if (any(by < 0L) || any(by > nc)) 
                stop("'by' must match numbers of columns")
        }
        else if (is.logical(by)) {
            if (length(by) != nc) 
                stop("'by' must match number of columns")
            by <- seq_along(by)[by]
        }
        else stop("'by' must specify one or more columns as numbers, names or logical")
        if (any(bad <- is.na(by))) 
            stop(ngettext(sum(bad), "'by' must specify a uniquely valid column", 
                "'by' must specify uniquely valid columns"), 
                domain = NA)
        unique(by)
    }
    nx <- nrow(x <- as.data.frame(x))
    ny <- nrow(y <- as.data.frame(y))
    if (nx >= 2^31 || ny >= 2^31) 
        stop("long vectors are not supported")
    by.x <- fix.by(by.x, x)
    by.y <- fix.by(by.y, y)
    if ((l.b <- length(by.x)) != length(by.y)) 
        stop("'by.x' and 'by.y' specify different numbers of columns")
    if (l.b == 0L) {
        nm <- nm.x <- names(x)
        nm.y <- names(y)
        has.common.nms <- any(cnm <- nm.x %in% nm.y)
        if (has.common.nms) {
            names(x)[cnm] <- paste0(nm.x[cnm], suffixes[1L])
            cnm <- nm.y %in% nm
            names(y)[cnm] <- paste0(nm.y[cnm], suffixes[2L])
        }
        if (nx == 0L || ny == 0L) {
            res <- cbind(x[FALSE, ], y[FALSE, ])
        }
        else {
            ij <- expand.grid(seq_len(nx), seq_len(ny))
            res <- cbind(x[ij[, 1L], , drop = FALSE], y[ij[, 
                2L], , drop = FALSE])
        }
    }
    else {
        if (any(by.x == 0L)) {
            x <- cbind(Row.names = I(row.names(x)), x)
            by.x <- by.x + 1L
        }
        if (any(by.y == 0L)) {
            y <- cbind(Row.names = I(row.names(y)), y)
            by.y <- by.y + 1L
        }
        row.names(x) <- NULL
        row.names(y) <- NULL
        if (l.b == 1L) {
            bx <- x[, by.x]
            if (is.factor(bx)) 
                bx <- as.character(bx)
            by <- y[, by.y]
            if (is.factor(by)) 
                by <- as.character(by)
        }
        else {
            if (!is.null(incomparables)) 
                stop("'incomparables' is supported only for merging on a single column")
            bx <- x[, by.x, drop = FALSE]
            by <- y[, by.y, drop = FALSE]
            names(bx) <- names(by) <- paste0("V", seq_len(ncol(bx)))
            bz <- do.call("paste", c(rbind(bx, by), sep = "\r"))
            bx <- bz[seq_len(nx)]
            by <- bz[nx + seq_len(ny)]
        }
        comm <- match(bx, by, 0L)
        bxy <- bx[comm > 0L]
        xinds <- match(bx, bxy, 0L, incomparables)
        yinds <- match(by, bxy, 0L, incomparables)
        if (nx > 0L && ny > 0L) 
            m <- .Internal(merge(xinds, yinds, all.x, all.y))
        else m <- list(xi = integer(), yi = integer(), x.alone = seq_len(nx), 
            y.alone = seq_len(ny))
        nm <- nm.x <- names(x)[-by.x]
        nm.by <- names(x)[by.x]
        nm.y <- names(y)[-by.y]
        ncx <- ncol(x)
        if (all.x) 
            all.x <- (nxx <- length(m$x.alone)) > 0L
        if (all.y) 
            all.y <- (nyy <- length(m$y.alone)) > 0L
        lxy <- length(m$xi)
        has.common.nms <- any(cnm <- nm.x %in% nm.y)
        if (has.common.nms && nzchar(suffixes[1L])) 
            nm.x[cnm] <- paste0(nm.x[cnm], suffixes[1L])
        x <- x[c(m$xi, if (all.x) m$x.alone), c(by.x, seq_len(ncx)[-by.x]), 
            drop = FALSE]
        names(x) <- c(nm.by, nm.x)
        if (all.y) {
            ya <- y[m$y.alone, by.y, drop = FALSE]
            names(ya) <- nm.by
            xa <- x[rep.int(NA_integer_, nyy), nm.x, drop = FALSE]
            names(xa) <- nm.x
            x <- rbind(x, cbind(ya, xa))
        }
        if (has.common.nms && nzchar(suffixes[2L])) {
            cnm <- nm.y %in% nm
            nm.y[cnm] <- paste0(nm.y[cnm], suffixes[2L])
        }
        y <- y[c(m$yi, if (all.x) rep.int(1L, nxx), if (all.y) m$y.alone), 
            -by.y, drop = FALSE]
        if (all.x) {
            zap <- (lxy + 1L):(lxy + nxx)
            for (i in seq_along(y)) {
                if (is.matrix(y[[1]])) 
                  y[[1]][zap, ] <- NA
                else is.na(y[[i]]) <- zap
            }
        }
        if (has.common.nms) 
            names(y) <- nm.y
        if (no.dups && any((mi <- match(nm.by, names(y), 0L)) > 
            0L) && nzchar(suffixes[2L])) 
            names(y)[mi] <- paste0(names(y)[mi], suffixes[2L])
        nm <- c(names(x), names(y))
        if (any(d <- duplicated(nm))) 
            if (sum(d) > 1L) 
                warning("column names ", paste(sQuote(nm[d]), 
                  collapse = ", "), " are duplicated in the result", 
                  domain = NA)
            else warning("column name ", sQuote(nm[d]), " is duplicated in the result", 
                domain = NA)
        res <- cbind(x, y)
        if (sort) 
            res <- res[if (all.x || all.y) {
                x <- x[, seq_len(l.b), drop = FALSE]
                attributes(x) <- NULL
                do.call("order", x)
            }
            else sort.list(bx[m$xi]), , drop = FALSE]
    }
    attr(res, "row.names") <- .set_row_names(nrow(res))
    res
}


.S3PrimitiveGenerics <- c("anyNA", "as.character", "as.complex", "as.double", "as.environment", 
"as.integer", "as.logical", "as.call", "as.numeric", "as.raw", 
"c", "dim", "dim<-", "dimnames", "dimnames<-", "is.array", "is.finite", 
"is.infinite", "is.matrix", "is.na", "is.nan", "is.numeric", 
"length", "length<-", "levels<-", "names", "names<-", "rep", 
"seq.int", "xtfrm")


stop <- function (..., call. = TRUE, domain = NULL) 
{
    args <- list(...)
    if (length(args) == 1L && inherits(args[[1L]], "condition")) {
        cond <- args[[1L]]
        if (nargs() > 1L) 
            warning("additional arguments ignored in stop()")
        message <- conditionMessage(cond)
        call <- conditionCall(cond)
        .Internal(.signalCondition(cond, message, call))
        .Internal(.dfltStop(message, call))
    }
    else .Internal(stop(call., .makeMessage(..., domain = domain)))
}


sort.list <- function (x, partial = NULL, na.last = TRUE, decreasing = FALSE, 
    method = c("auto", "shell", "quick", "radix")) 
{
    decreasing <- as.logical(decreasing)
    if (is.null(partial) && is.numeric(x) && !is.object(x) && 
        length(x) > 0) {
        if (.Internal(sorted_fpass(x, decreasing, na.last))) 
            return(seq_along(x))
    }
    method <- match.arg(method)
    if (method == "auto" && (is.numeric(x) || is.factor(x) || 
        is.logical(x)) && is.integer(length(x))) 
        method <- "radix"
    if (!is.atomic(x)) 
        stop("'x' must be atomic for 'sort.list'\nHave you called 'sort' on a list?")
    if (!is.null(partial)) 
        .NotYetUsed("partial != NULL")
    if (method == "quick") {
        if (is.factor(x)) 
            x <- as.integer(x)
        if (is.numeric(x)) 
            return(sort(x, na.last = na.last, decreasing = decreasing, 
                method = "quick", index.return = TRUE)$ix)
        else stop("method = \"quick\" is only for numeric 'x'")
    }
    if (is.na(na.last)) {
        x <- x[!is.na(x)]
        na.last <- TRUE
    }
    if (method == "radix") {
        return(order(x, na.last = na.last, decreasing = decreasing, 
            method = "radix"))
    }
    .Internal(order(na.last, decreasing, x))
}


unloadNamespace <- function (ns) 
{
    if ((is.character(ns) && any(ns == loadedNamespaces())) || 
        (is.environment(ns) && any(getNamespaceName(ns) == loadedNamespaces()))) {
        runHook <- function(hookname, env, ...) {
            if (!is.null(fun <- env[[hookname]])) {
                res <- tryCatch(fun(...), error = identity)
                if (inherits(res, "error")) {
                  warning(gettextf("%s failed in %s() for '%s', details:\n  call: %s\n  error: %s", 
                    hookname, "unloadNamespace", nsname, deparse(conditionCall(res))[1L], 
                    conditionMessage(res)), call. = FALSE, domain = NA)
                }
            }
        }
        ns <- asNamespace(ns, base.OK = FALSE)
        nsname <- getNamespaceName(ns)
        pos <- match(paste0("package:", nsname), search())
        if (!is.na(pos)) 
            detach(pos = pos)
        users <- getNamespaceUsers(ns)
        if (length(users)) 
            stop(gettextf("namespace %s is imported by %s so cannot be unloaded", 
                sQuote(getNamespaceName(ns)), paste(sQuote(users), 
                  collapse = ", ")), domain = NA)
        nspath <- .getNamespaceInfo(ns, "path")
        hook <- getHook(packageEvent(nsname, "onUnload"))
        for (fun in rev(hook)) try(fun(nsname, nspath))
        runHook(".onUnload", ns, nspath)
        .Internal(unregisterNamespace(nsname))
        if (.isMethodsDispatchOn() && methods:::.hasS4MetaData(ns)) 
            methods::cacheMetaData(ns, FALSE, ns)
        .Internal(lazyLoadDBflush(paste0(nspath, "/R/", nsname, 
            ".rdb")))
    }
    invisible()
}


grouping <- function (...) 
{
    z <- list(...)
    if (any(vapply(z, is.object, logical(1L)))) {
        z <- lapply(z, function(x) if (is.object(x)) 
            as.vector(xtfrm(x))
        else x)
        return(do.call("grouping", z))
    }
    nalast <- TRUE
    decreasing <- rep_len(FALSE, length(z))
    group <- TRUE
    sortStr <- FALSE
    return(.Internal(radixsort(nalast, decreasing, group, sortStr, 
        ...)))
}


seek.connection <- function (con, where = NA, origin = "start", rw = "", ...) 
{
    origin <- pmatch(origin, c("start", "current", "end"))
    rw <- pmatch(rw, c("read", "write"), 0L)
    if (is.na(origin)) 
        stop("'origin' must be one of 'start', 'current' or 'end'")
    .Internal(seek(con, as.double(where), origin, rw))
}


as.matrix.default <- function (x, ...) 
{
    if (is.matrix(x)) 
        x
    else array(x, c(length(x), 1L), if (!is.null(names(x))) 
        list(names(x), NULL)
    else NULL)
}


`%o%` <- function (X, Y) 
outer(X, Y)


as.POSIXlt.numeric <- function (x, tz = "", origin, ...) 
{
    if (missing(origin)) 
        stop("'origin' must be supplied")
    as.POSIXlt(as.POSIXct(origin, tz = "UTC", ...) + x, tz = tz)
}


eval.parent <- function (expr, n = 1) 
{
    p <- parent.frame(n + 1)
    eval(expr, p)
}


`%x%` <- function (X, Y) 
kronecker(X, Y)


diff.POSIXt <- function (x, lag = 1L, differences = 1L, ...) 
{
    ismat <- is.matrix(x)
    r <- if (inherits(x, "POSIXlt")) 
        as.POSIXct(x)
    else x
    xlen <- if (ismat) 
        dim(x)[1L]
    else length(r)
    if (length(lag) != 1L || length(differences) > 1L || lag < 
        1L || differences < 1L) 
        stop("'lag' and 'differences' must be integers >= 1")
    if (lag * differences >= xlen) 
        return(.difftime(numeric(), "secs"))
    i1 <- -seq_len(lag)
    if (ismat) 
        for (i in seq_len(differences)) r <- r[i1, , drop = FALSE] - 
            r[-nrow(r):-(nrow(r) - lag + 1), , drop = FALSE]
    else for (i in seq_len(differences)) r <- r[i1] - r[-length(r):-(length(r) - 
        lag + 1L)]
    r
}


pushBack <- function (data, connection, newLine = TRUE, encoding = c("", 
    "bytes", "UTF-8")) 
{
    if (length(encoding) > 1L) 
        encoding <- encoding[1]
    if (nzchar(encoding)) 
        encoding <- match.arg(encoding)
    type <- match(encoding, c("", "bytes", "UTF-8"))
    .Internal(pushBack(data, connection, newLine, type))
}


transform.default <- function (`_data`, ...) 
transform.data.frame(data.frame(`_data`), ...)


srcfilecopy <- function (filename, lines, timestamp = Sys.time(), isFile = FALSE) 
{
    stopifnot(is.character(filename), length(filename) == 1L)
    e <- new.env(parent = emptyenv())
    if (any(grepl("\n", lines, fixed = TRUE, useBytes = TRUE))) 
        lines <- unlist(strsplit(sub("$", "\n", as.character(lines)), 
            "\n"))
    e$filename <- filename
    e$wd <- getwd()
    e$isFile <- isFile
    e$lines <- as.character(lines)
    e$fixedNewlines <- TRUE
    e$timestamp <- timestamp
    e$Enc <- "unknown"
    class(e) <- c("srcfilecopy", "srcfile")
    return(e)
}


qr.coef <- function (qr, y) 
{
    if (!is.qr(qr)) 
        stop("first argument must be a QR decomposition")
    n <- as.integer(nrow(qr$qr))
    if (is.na(n)) 
        stop("invalid nrow(qr$qr)")
    p <- as.integer(ncol(qr$qr))
    if (is.na(p)) 
        stop("invalid ncol(qr$qr)")
    k <- as.integer(qr$rank)
    if (is.na(k)) 
        stop("invalid ncol(qr$rank)")
    im <- is.matrix(y)
    if (!im) 
        y <- as.matrix(y)
    ny <- as.integer(ncol(y))
    if (is.na(ny)) 
        stop("invalid ncol(y)")
    if (nrow(y) != n) 
        stop("'qr' and 'y' must have the same number of rows")
    isC <- is.complex(qr$qr)
    coef <- matrix(if (isC) 
        NA_complex_
    else NA_real_, p, ny)
    ix <- if (p > n) 
        c(seq_len(n), rep(NA, p - n))
    else seq_len(p)
    if (!is.null(nam <- colnames(qr$qr))) 
        pivotted <- NA
    if (p == 0L) {
        pivotted <- FALSE
    }
    else if (isC) {
        coef[qr$pivot, ] <- .Internal(qr_coef_cmplx(qr, y))[ix, 
            ]
    }
    else if (isTRUE(attr(qr, "useLAPACK"))) {
        coef[qr$pivot, ] <- .Internal(qr_coef_real(qr, y))[ix, 
            ]
    }
    else if (k > 0L) {
        storage.mode(y) <- "double"
        z <- .Fortran(.F_dqrcf, as.double(qr$qr), n, k, as.double(qr$qraux), 
            y, ny, coef = matrix(0, nrow = k, ncol = ny), info = integer(1L), 
            NAOK = TRUE)[c("coef", "info")]
        if (z$info) 
            stop("exact singularity in 'qr.coef'")
        pivotted <- k < p
        if (pivotted) 
            coef[qr$pivot[seq_len(k)], ] <- z$coef
        else coef <- z$coef
    }
    if (!is.null(nam)) {
        if (is.na(pivotted)) 
            pivotted <- is.unsorted(qr$pivot)
        if (pivotted) 
            rownames(coef)[qr$pivot] <- nam
        else rownames(coef) <- nam
    }
    if (im && !is.null(nam <- colnames(y))) 
        colnames(coef) <- nam
    if (im) 
        coef
    else drop(coef)
}


restartFormals <- function (r) 
formals(r$handler)


print.rle <- function (x, digits = getOption("digits"), prefix = "", ...) 
{
    if (is.null(digits)) 
        digits <- getOption("digits")
    cat("", "Run Length Encoding\n", "  lengths:", sep = prefix)
    utils::str(x$lengths)
    cat("", "  values :", sep = prefix)
    utils::str(x$values, digits.d = digits)
    invisible(x)
}


format.octmode <- function (x, width = NULL, ...) 
{
    isna <- is.na(x)
    y <- as.integer(x[!isna])
    fmt <- if (!is.null(width)) 
        paste0("%0", width, "o")
    else "%o"
    ans <- rep.int(NA_character_, length(x))
    ans0 <- sprintf(fmt, y)
    if (is.null(width) && length(y) > 1L) {
        nc <- max(nchar(ans0))
        ans0 <- sprintf(paste0("%0", nc, "o"), y)
    }
    ans[!isna] <- ans0
    dim(ans) <- dim(x)
    dimnames(ans) <- dimnames(x)
    names(ans) <- names(x)
    ans
}


diff.Date <- function (x, lag = 1L, differences = 1L, ...) 
{
    ismat <- is.matrix(x)
    xlen <- if (ismat) 
        dim(x)[1L]
    else length(x)
    if (length(lag) != 1L || length(differences) > 1L || lag < 
        1L || differences < 1L) 
        stop("'lag' and 'differences' must be integers >= 1")
    if (lag * differences >= xlen) 
        return(.difftime(numeric(), units = "days"))
    r <- x
    i1 <- -seq_len(lag)
    if (ismat) 
        for (i in seq_len(differences)) r <- r[i1, , drop = FALSE] - 
            r[-nrow(r):-(nrow(r) - lag + 1L), , drop = FALSE]
    else for (i in seq_len(differences)) r <- r[i1] - r[-length(r):-(length(r) - 
        lag + 1L)]
    r
}


conditionCall.condition <- function (c) 
c$call


as.matrix <- function (x, ...) 
UseMethod("as.matrix")


enc2utf8 <- function (x)  .Primitive("enc2utf8")


sys.function <- function (which = 0L) 
.Internal(sys.function(which))


print.DLLInfo <- function (x, ...) 
{
    tmp <- as.data.frame.list(x[c("name", "path", "dynamicLookup")])
    names(tmp) <- c("DLL name", "Filename", "Dynamic lookup")
    write.dcf(tmp, ...)
    invisible(x)
}


UseMethod <- function (generic, object)  .Primitive("UseMethod")


write.dcf <- function (x, file = "", append = FALSE, useBytes = FALSE, indent = 0.1 * 
    getOption("width"), width = 0.9 * getOption("width"), keep.white = NULL) 
{
    if (file == "") 
        file <- stdout()
    else if (is.character(file)) {
        file <- file(file, if (append) 
            "a"
        else "w")
        on.exit(close(file))
    }
    if (!inherits(file, "connection")) 
        stop("'file' must be a character string or connection")
    escape_paragraphs <- function(s) gsub("\n \\.([^\n])", "\n  .\\1", 
        gsub("\n[ \t]*\n", "\n .\n ", s, perl = TRUE, useBytes = TRUE), 
        perl = TRUE, useBytes = TRUE)
    fmt <- function(tag, val, fold = TRUE) {
        s <- if (fold) 
            formatDL(rep.int(tag, length(val)), val, style = "list", 
                width = width, indent = indent)
        else {
            sprintf("%s: %s", tag, gsub("\n([^[:blank:]])", "\n \\1", 
                val))
        }
        escape_paragraphs(s)
    }
    if (!is.data.frame(x)) 
        x <- as.data.frame(x, stringsAsFactors = FALSE)
    nmx <- names(x)
    out <- matrix("", nrow(x), ncol(x))
    foldable <- is.na(match(nmx, keep.white))
    for (j in seq_along(x)) {
        xj <- x[[j]]
        if (is.atomic(xj)) {
            i <- !is.na(xj)
            out[i, j] <- fmt(nmx[j], xj[i], foldable[j])
        }
        else {
            nmxj <- nmx[j]
            fold <- foldable[j]
            i <- !vapply(xj, function(s) (length(s) == 1L) && 
                is.na(s), NA)
            out[i, j] <- vapply(xj[i], function(s) {
                paste(fmt(nmxj, s, fold), collapse = "\n")
            }, "")
        }
    }
    out <- t(out)
    is_not_empty <- nzchar(out)
    eor <- character(sum(is_not_empty))
    if (length(eor)) {
        eor[which(diff(c(col(out))[is_not_empty]) >= 1L)] <- "\n"
    }
    writeLines(paste0(c(out[is_not_empty]), eor), file, useBytes = useBytes)
}


split <- function (x, f, drop = FALSE, ...) 
UseMethod("split")


match.call <- function (definition = sys.function(sys.parent()), call = sys.call(sys.parent()), 
    expand.dots = TRUE, envir = parent.frame(2L)) 
{
    if (!missing(definition) && is.null(definition)) {
        definition <- sys.function(sys.parent())
    }
    .Internal(match.call(definition, call, expand.dots, envir))
}


is.numeric.POSIXt <- function (x) 
FALSE


sys.calls <- function () 
.Internal(sys.calls())


.AutoloadEnv <- "<environment>"

`length<-.Date` <- function (x, value) 
.Date(NextMethod(), oldClass(x))


format.numeric_version <- function (x, ...) 
{
    x <- unclass(x)
    y <- rep.int(NA_character_, length(x))
    names(y) <- names(x)
    ind <- lengths(x) > 0L
    y[ind] <- unlist(lapply(x[ind], paste, collapse = "."))
    y
}


char.expand <- function (input, target, nomatch = stop("no match")) 
{
    if (length(input) != 1L) 
        stop("'input' must have length 1")
    if (!(is.character(input) && is.character(target))) 
        stop("'input' and 'target' must be character vectors")
    y <- .Internal(charmatch(input, target, NA_integer_))
    if (anyNA(y)) 
        eval(nomatch)
    target[y]
}


signif <- function (x, digits = 6)  .Primitive("signif")


file.link <- function (from, to) 
{
    if (!(length(from))) 
        stop("no files to link from")
    if (!length(to)) 
        stop("no files to link to")
    .Internal(file.link(from, to))
}


.Devices <- pairlist("null device")


diff.default <- function (x, lag = 1L, differences = 1L, ...) 
{
    ismat <- is.matrix(x)
    xlen <- if (ismat) 
        dim(x)[1L]
    else length(x)
    if (length(lag) != 1L || length(differences) > 1L || lag < 
        1L || differences < 1L) 
        stop("'lag' and 'differences' must be integers >= 1")
    if (lag * differences >= xlen) 
        return(x[0L])
    r <- unclass(x)
    i1 <- -seq_len(lag)
    if (ismat) 
        for (i in seq_len(differences)) r <- r[i1, , drop = FALSE] - 
            r[-nrow(r):-(nrow(r) - lag + 1L), , drop = FALSE]
    else for (i in seq_len(differences)) r <- r[i1] - r[-length(r):-(length(r) - 
        lag + 1L)]
    class(r) <- oldClass(x)
    r
}


dyn.load <- function (x, local = TRUE, now = TRUE, ...) 
{
    inDL <- function(x, local, now, ..., DLLpath = "") .Internal(dyn.load(x, 
        local, now, DLLpath))
    inDL(x, as.logical(local), as.logical(now), ...)
}


.userHooksEnv <- "<environment>"

anyNA.POSIXlt <- function (x, recursive = FALSE) 
anyNA(as.POSIXct(x))


.subset <- function (x, ...)  .Primitive(".subset")


expm1 <- function (x)  .Primitive("expm1")


is.vector <- function (x, mode = "any") 
.Internal(is.vector(x, mode))


apply <- function (X, MARGIN, FUN, ...) 
{
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
        ans <- forceAndCall(1, FUN, if (length(d.call) < 2L) newX[, 
            1] else array(newX[, 1L], d.call, dn.call), ...)
        return(if (is.null(ans)) ans else if (length(d.ans) < 
            2L) ans[1L][-1L] else array(ans, d.ans, dn.ans))
    }
    newX <- aperm(X, c(s.call, s.ans))
    dim(newX) <- c(prod(d.call), d2)
    ans <- vector("list", d2)
    if (length(d.call) < 2L) {
        if (length(dn.call)) 
            dimnames(newX) <- c(dn.call, list(NULL))
        for (i in 1L:d2) {
            tmp <- forceAndCall(1, FUN, newX[, i], ...)
            if (!is.null(tmp)) 
                ans[[i]] <- tmp
        }
    }
    else for (i in 1L:d2) {
        tmp <- forceAndCall(1, FUN, array(newX[, i], d.call, 
            dn.call), ...)
        if (!is.null(tmp)) 
            ans[[i]] <- tmp
    }
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
        ans
    }
    else if (len.a == d2) 
        array(ans, d.ans, dn.ans)
    else if (len.a && len.a%%d2 == 0L) {
        if (is.null(dn.ans)) 
            dn.ans <- vector(mode = "list", length(d.ans))
        dn1 <- list(ans.names)
        if (length(dn.call) && !is.null(n1 <- names(dn <- dn.call[1])) && 
            nzchar(n1) && length(ans.names) == length(dn[[1]])) 
            names(dn1) <- n1
        dn.ans <- c(dn1, dn.ans)
        array(ans, c(len.a%/%d2, d.ans), if (!is.null(names(dn.ans)) || 
            !all(vapply(dn.ans, is.null, NA))) 
            dn.ans)
    }
    else ans
}


iconvlist <- function () 
{
    int <- .Internal(iconv(NULL, "", "", "", TRUE, FALSE))
    if (length(int)) 
        return(sort.int(int))
    icfile <- system.file("iconvlist", package = "utils")
    if (!nchar(icfile, type = "bytes")) 
        stop("'iconvlist' is not available on this system")
    ext <- readLines(icfile)
    if (!length(ext)) 
        stop("'iconvlist' is not available on this system")
    cnt <- grep("//$", ext)
    if (length(cnt)/length(ext) > 0.5) {
        ext <- grep("//$", ext, value = TRUE)
        ext <- sub("//$", "", ext)
    }
    sort.int(unlist(strsplit(ext, "[[:space:]]")))
}


chkDots <- function (..., which.call = -1, allowed = character(0)) 
{
    if (nx <- length(list(...))) 
        warning(sprintf(ngettext(nx, "In %s :\n extra argument %s will be disregarded", 
            "In %s :\n extra arguments %s will be disregarded"), 
            paste(deparse(sys.call(which.call), control = c()), 
                collapse = "\n"), paste(sQuote(names(list(...))), 
                collapse = ", ")), call. = FALSE, domain = NA)
}


.gt <- function (x, i, j) 
{
    xi <- x[i]
    xj <- x[j]
    if (xi == xj) 
        0L
    else if (xi > xj) 
        1L
    else -1L
}


is.na.data.frame <- function (x) 
{
    y <- if (length(x)) {
        do.call("cbind", lapply(x, "is.na"))
    }
    else matrix(FALSE, length(row.names(x)), 0)
    if (.row_names_info(x) > 0L) 
        rownames(y) <- row.names(x)
    y
}


library <- function (package, help, pos = 2, lib.loc = NULL, character.only = FALSE, 
    logical.return = FALSE, warn.conflicts, quietly = FALSE, 
    verbose = getOption("verbose"), mask.ok, exclude, include.only, 
    attach.required = missing(include.only)) 
{
    conf.ctrl <- getOption("conflicts.policy")
    if (is.character(conf.ctrl)) 
        conf.ctrl <- switch(conf.ctrl, strict = list(error = TRUE, 
            warn = FALSE), depends.ok = list(error = TRUE, generics.ok = TRUE, 
            can.mask = c("base", "methods", "utils", "grDevices", 
                "graphics", "stats"), depends.ok = TRUE), warning(gettextf("unknown conflict policy: %s", 
            sQuote(conf.ctrl)), call. = FALSE, domain = NA))
    if (!is.list(conf.ctrl)) 
        conf.ctrl <- NULL
    stopOnConflict <- isTRUE(conf.ctrl$error)
    if (missing(warn.conflicts)) 
        warn.conflicts <- if (isFALSE(conf.ctrl$warn)) 
            FALSE
        else TRUE
    if ((!missing(include.only)) && (!missing(exclude))) 
        stop(gettext("only one of 'include.only' and 'exclude' can be used"), 
            call. = FALSE, domain = NA)
    testRversion <- function(pkgInfo, pkgname, pkgpath) {
        if (is.null(built <- pkgInfo$Built)) 
            stop(gettextf("package %s has not been installed properly\n", 
                sQuote(pkgname)), call. = FALSE, domain = NA)
        R_version_built_under <- as.numeric_version(built$R)
        if (R_version_built_under < "3.0.0") 
            stop(gettextf("package %s was built before R 3.0.0: please re-install it", 
                sQuote(pkgname)), call. = FALSE, domain = NA)
        current <- getRversion()
        if (length(Rdeps <- pkgInfo$Rdepends2)) {
            for (dep in Rdeps) if (length(dep) > 1L) {
                target <- dep$version
                res <- if (is.character(target)) {
                  do.call(dep$op, list(as.numeric(R.version[["svn rev"]]), 
                    as.numeric(sub("^r", "", dep$version))))
                }
                else {
                  do.call(dep$op, list(current, as.numeric_version(target)))
                }
                if (!res) 
                  stop(gettextf("This is R %s, package %s needs %s %s", 
                    current, sQuote(pkgname), dep$op, target), 
                    call. = FALSE, domain = NA)
            }
        }
        if (R_version_built_under > current) 
            warning(gettextf("package %s was built under R version %s", 
                sQuote(pkgname), as.character(built$R)), call. = FALSE, 
                domain = NA)
        platform <- built$Platform
        r_arch <- .Platform$r_arch
        if (.Platform$OS.type == "unix") {
        }
        else {
            if (nzchar(platform) && !grepl("mingw", platform)) 
                stop(gettextf("package %s was built for %s", 
                  sQuote(pkgname), platform), call. = FALSE, 
                  domain = NA)
        }
        if (nzchar(r_arch) && file.exists(file.path(pkgpath, 
            "libs")) && !file.exists(file.path(pkgpath, "libs", 
            r_arch))) 
            stop(gettextf("package %s is not installed for 'arch = %s'", 
                sQuote(pkgname), r_arch), call. = FALSE, domain = NA)
    }
    checkNoGenerics <- function(env, pkg) {
        nenv <- env
        ns <- .getNamespace(as.name(pkg))
        if (!is.null(ns)) 
            nenv <- asNamespace(ns)
        if (exists(".noGenerics", envir = nenv, inherits = FALSE)) 
            TRUE
        else {
            !any(startsWith(names(env), ".__T"))
        }
    }
    checkConflicts <- function(package, pkgname, pkgpath, nogenerics, 
        env) {
        dont.mind <- c("last.dump", "last.warning", ".Last.value", 
            ".Random.seed", ".Last.lib", ".onDetach", ".packageName", 
            ".noGenerics", ".required", ".no_S3_generics", ".Depends", 
            ".requireCachedGenerics")
        sp <- search()
        lib.pos <- which(sp == pkgname)
        ob <- names(as.environment(lib.pos))
        if (!nogenerics) {
            these <- ob[startsWith(ob, ".__T__")]
            gen <- gsub(".__T__(.*):([^:]+)", "\\1", these)
            from <- gsub(".__T__(.*):([^:]+)", "\\2", these)
            gen <- gen[from != package]
            ob <- ob[!(ob %in% gen)]
        }
        ipos <- seq_along(sp)[-c(lib.pos, match(c("Autoloads", 
            "CheckExEnv"), sp, 0L))]
        cpos <- NULL
        conflicts <- vector("list", 0)
        for (i in ipos) {
            obj.same <- match(names(as.environment(i)), ob, nomatch = 0L)
            if (any(obj.same > 0)) {
                same <- ob[obj.same]
                same <- same[!(same %in% dont.mind)]
                Classobjs <- which(startsWith(same, ".__"))
                if (length(Classobjs)) 
                  same <- same[-Classobjs]
                same.isFn <- function(where) vapply(same, exists, 
                  NA, where = where, mode = "function", inherits = FALSE)
                same <- same[same.isFn(i) == same.isFn(lib.pos)]
                not.Ident <- function(ch, TRAFO = identity, ...) vapply(ch, 
                  function(.) !identical(TRAFO(get(., i)), TRAFO(get(., 
                    lib.pos)), ...), NA)
                if (length(same)) 
                  same <- same[not.Ident(same)]
                if (length(same) && identical(sp[i], "package:base")) 
                  same <- same[not.Ident(same, ignore.environment = TRUE)]
                if (length(same)) {
                  conflicts[[sp[i]]] <- same
                  cpos[sp[i]] <- i
                }
            }
        }
        if (length(conflicts)) {
            if (stopOnConflict) {
                emsg <- ""
                pkg <- names(conflicts)
                notOK <- vector("list", 0)
                for (i in seq_along(conflicts)) {
                  pkgname <- sub("^package:", "", pkg[i])
                  if (pkgname %in% canMaskEnv$canMask) 
                    next
                  same <- conflicts[[i]]
                  if (is.list(mask.ok)) 
                    myMaskOK <- mask.ok[[pkgname]]
                  else myMaskOK <- mask.ok
                  if (isTRUE(myMaskOK)) 
                    same <- NULL
                  else if (is.character(myMaskOK)) 
                    same <- setdiff(same, myMaskOK)
                  if (length(same)) {
                    notOK[[pkg[i]]] <- same
                    msg <- .maskedMsg(sort(same), pkg = sQuote(pkg[i]), 
                      by = cpos[i] < lib.pos)
                    emsg <- paste(emsg, msg, sep = "\n")
                  }
                }
                if (length(notOK)) {
                  msg <- gettextf("Conflicts attaching package %s:\n%s", 
                    sQuote(package), emsg)
                  stop(errorCondition(msg, package = package, 
                    conflicts = conflicts, class = "packageConflictError"))
                }
            }
            if (warn.conflicts) {
                packageStartupMessage(gettextf("\nAttaching package: %s\n", 
                  sQuote(package)), domain = NA)
                pkg <- names(conflicts)
                for (i in seq_along(conflicts)) {
                  msg <- .maskedMsg(sort(conflicts[[i]]), pkg = sQuote(pkg[i]), 
                    by = cpos[i] < lib.pos)
                  packageStartupMessage(msg, domain = NA)
                }
            }
        }
    }
    if (verbose && quietly) 
        message("'verbose' and 'quietly' are both true; being verbose then ..")
    if (!missing(package)) {
        if (is.null(lib.loc)) 
            lib.loc <- .libPaths()
        lib.loc <- lib.loc[dir.exists(lib.loc)]
        if (!character.only) 
            package <- as.character(substitute(package))
        if (length(package) != 1L) 
            stop("'package' must be of length 1")
        if (is.na(package) || (package == "")) 
            stop("invalid package name")
        pkgname <- paste0("package:", package)
        newpackage <- is.na(match(pkgname, search()))
        if (newpackage) {
            pkgpath <- find.package(package, lib.loc, quiet = TRUE, 
                verbose = verbose)
            if (length(pkgpath) == 0L) {
                if (length(lib.loc) && !logical.return) 
                  stop(packageNotFoundError(package, lib.loc, 
                    sys.call()))
                txt <- if (length(lib.loc)) 
                  gettextf("there is no package called %s", sQuote(package))
                else gettext("no library trees found in 'lib.loc'")
                if (logical.return) {
                  warning(txt, domain = NA)
                  return(FALSE)
                }
                else stop(txt, domain = NA)
            }
            which.lib.loc <- normalizePath(dirname(pkgpath), 
                "/", TRUE)
            pfile <- system.file("Meta", "package.rds", package = package, 
                lib.loc = which.lib.loc)
            if (!nzchar(pfile)) 
                stop(gettextf("%s is not a valid installed package", 
                  sQuote(package)), domain = NA)
            pkgInfo <- readRDS(pfile)
            testRversion(pkgInfo, package, pkgpath)
            if (is.character(pos)) {
                npos <- match(pos, search())
                if (is.na(npos)) {
                  warning(gettextf("%s not found on search path, using pos = 2", 
                    sQuote(pos)), domain = NA)
                  pos <- 2
                }
                else pos <- npos
            }
            deps <- unique(names(pkgInfo$Depends))
            depsOK <- isTRUE(conf.ctrl$depends.ok)
            if (depsOK) {
                canMaskEnv <- dynGet("__library_can_mask__", 
                  NULL)
                if (is.null(canMaskEnv)) {
                  canMaskEnv <- new.env()
                  canMaskEnv$canMask <- union("base", conf.ctrl$can.mask)
                  "__library_can_mask__" <- canMaskEnv
                }
                canMaskEnv$canMask <- unique(c(package, deps, 
                  canMaskEnv$canMask))
            }
            else canMaskEnv <- NULL
            if (attach.required) 
                .getRequiredPackages2(pkgInfo, quietly = quietly)
            cr <- conflictRules(package)
            if (missing(mask.ok)) 
                mask.ok <- cr$mask.ok
            if (missing(exclude)) 
                exclude <- cr$exclude
            if (packageHasNamespace(package, which.lib.loc)) {
                if (isNamespaceLoaded(package)) {
                  newversion <- as.numeric_version(pkgInfo$DESCRIPTION["Version"])
                  oldversion <- as.numeric_version(getNamespaceVersion(package))
                  if (newversion != oldversion) {
                    tryCatch(unloadNamespace(package), error = function(e) {
                      P <- if (!is.null(cc <- conditionCall(e))) 
                        paste("Error in", deparse(cc)[1L], ": ")
                      else "Error : "
                      stop(gettextf("Package %s version %s cannot be unloaded:\n %s", 
                        sQuote(package), oldversion, paste0(P, 
                          conditionMessage(e), "\n")), domain = NA)
                    })
                  }
                }
                tt <- tryCatch({
                  attr(package, "LibPath") <- which.lib.loc
                  ns <- loadNamespace(package, lib.loc)
                  env <- attachNamespace(ns, pos = pos, deps, 
                    exclude, include.only)
                }, error = function(e) {
                  P <- if (!is.null(cc <- conditionCall(e))) 
                    paste(" in", deparse(cc)[1L])
                  else ""
                  msg <- gettextf("package or namespace load failed for %s%s:\n %s", 
                    sQuote(package), P, conditionMessage(e))
                  if (logical.return) 
                    message(paste("Error:", msg), domain = NA)
                  else stop(msg, call. = FALSE, domain = NA)
                })
                if (logical.return && is.null(tt)) 
                  return(FALSE)
                attr(package, "LibPath") <- NULL
                {
                  on.exit(detach(pos = pos))
                  nogenerics <- !.isMethodsDispatchOn() || checkNoGenerics(env, 
                    package)
                  if (isFALSE(conf.ctrl$generics.ok) || (stopOnConflict && 
                    !isTRUE(conf.ctrl$generics.ok))) 
                    nogenerics <- TRUE
                  if (stopOnConflict || (warn.conflicts && !exists(".conflicts.OK", 
                    envir = env, inherits = FALSE))) 
                    checkConflicts(package, pkgname, pkgpath, 
                      nogenerics, ns)
                  on.exit()
                  if (logical.return) 
                    return(TRUE)
                  else return(invisible(.packages()))
                }
            }
            else stop(gettextf("package %s does not have a namespace and should be re-installed", 
                sQuote(package)), domain = NA)
        }
        if (verbose && !newpackage) 
            warning(gettextf("package %s already present in search()", 
                sQuote(package)), domain = NA)
    }
    else if (!missing(help)) {
        if (!character.only) 
            help <- as.character(substitute(help))
        pkgName <- help[1L]
        pkgPath <- find.package(pkgName, lib.loc, verbose = verbose)
        docFiles <- c(file.path(pkgPath, "Meta", "package.rds"), 
            file.path(pkgPath, "INDEX"))
        if (file.exists(vignetteIndexRDS <- file.path(pkgPath, 
            "Meta", "vignette.rds"))) 
            docFiles <- c(docFiles, vignetteIndexRDS)
        pkgInfo <- vector("list", 3L)
        readDocFile <- function(f) {
            if (basename(f) %in% "package.rds") {
                txt <- readRDS(f)$DESCRIPTION
                if ("Encoding" %in% names(txt)) {
                  to <- if (Sys.getlocale("LC_CTYPE") == "C") 
                    "ASCII//TRANSLIT"
                  else ""
                  tmp <- try(iconv(txt, from = txt["Encoding"], 
                    to = to))
                  if (!inherits(tmp, "try-error")) 
                    txt <- tmp
                  else warning("'DESCRIPTION' has an 'Encoding' field and re-encoding is not possible", 
                    call. = FALSE)
                }
                nm <- paste0(names(txt), ":")
                formatDL(nm, txt, indent = max(nchar(nm, "w")) + 
                  3L)
            }
            else if (basename(f) %in% "vignette.rds") {
                txt <- readRDS(f)
                if (is.data.frame(txt) && nrow(txt)) 
                  cbind(basename(gsub("\\.[[:alpha:]]+$", "", 
                    txt$File)), paste(txt$Title, paste0(rep.int("(source", 
                    NROW(txt)), ifelse(nzchar(txt$PDF), ", pdf", 
                    ""), ")")))
                else NULL
            }
            else readLines(f)
        }
        for (i in which(file.exists(docFiles))) pkgInfo[[i]] <- readDocFile(docFiles[i])
        y <- list(name = pkgName, path = pkgPath, info = pkgInfo)
        class(y) <- "packageInfo"
        return(y)
    }
    else {
        if (is.null(lib.loc)) 
            lib.loc <- .libPaths()
        db <- matrix(character(), nrow = 0L, ncol = 3L)
        nopkgs <- character()
        for (lib in lib.loc) {
            a <- .packages(all.available = TRUE, lib.loc = lib)
            for (i in sort(a)) {
                file <- system.file("Meta", "package.rds", package = i, 
                  lib.loc = lib)
                title <- if (nzchar(file)) {
                  txt <- readRDS(file)
                  if (is.list(txt)) 
                    txt <- txt$DESCRIPTION
                  if ("Encoding" %in% names(txt)) {
                    to <- if (Sys.getlocale("LC_CTYPE") == "C") 
                      "ASCII//TRANSLIT"
                    else ""
                    tmp <- try(iconv(txt, txt["Encoding"], to, 
                      "?"))
                    if (!inherits(tmp, "try-error")) 
                      txt <- tmp
                    else warning("'DESCRIPTION' has an 'Encoding' field and re-encoding is not possible", 
                      call. = FALSE)
                  }
                  txt["Title"]
                }
                else NA
                if (is.na(title)) 
                  title <- " ** No title available ** "
                db <- rbind(db, cbind(i, lib, title))
            }
            if (length(a) == 0L) 
                nopkgs <- c(nopkgs, lib)
        }
        dimnames(db) <- list(NULL, c("Package", "LibPath", "Title"))
        if (length(nopkgs) && !missing(lib.loc)) {
            pkglist <- paste(sQuote(nopkgs), collapse = ", ")
            msg <- sprintf(ngettext(length(nopkgs), "library %s contains no packages", 
                "libraries %s contain no packages"), pkglist)
            warning(msg, domain = NA)
        }
        y <- list(header = NULL, results = db, footer = NULL)
        class(y) <- "libraryIQR"
        return(y)
    }
    if (logical.return) 
        TRUE
    else invisible(.packages())
}


julian.POSIXt <- function (x, origin = as.POSIXct("1970-01-01", tz = "GMT"), ...) 
{
    origin <- as.POSIXct(origin)
    if (length(origin) != 1L) 
        stop("'origin' must be of length one")
    res <- difftime(as.POSIXct(x), origin, units = "days")
    structure(res, origin = origin)
}


path.package <- function (package = NULL, quiet = FALSE) 
{
    if (is.null(package)) 
        package <- .packages()
    if (length(package) == 0L) 
        return(character())
    s <- search()
    searchpaths <- lapply(seq_along(s), function(i) attr(as.environment(i), 
        "path"))
    searchpaths[[length(s)]] <- system.file()
    pkgs <- paste0("package:", package)
    pos <- match(pkgs, s)
    if (any(m <- is.na(pos))) {
        if (!quiet) {
            if (all(m)) 
                stop("none of the packages are loaded")
            else warning(sprintf(ngettext(as.integer(sum(m)), 
                "package %s is not loaded", "packages %s are not loaded"), 
                paste(package[m], collapse = ", ")), domain = NA)
        }
        pos <- pos[!m]
    }
    unlist(searchpaths[pos], use.names = FALSE)
}


as.data.frame.vector <- function (x, row.names = NULL, optional = FALSE, ..., nm = paste(deparse(substitute(x), 
    width.cutoff = 500L), collapse = " ")) 
{
    force(nm)
    nrows <- length(x)
    if (!(is.null(row.names) || (is.character(row.names) && length(row.names) == 
        nrows))) {
        warning(gettextf("'row.names' is not a character vector of length %d -- omitting it. Will be an error!", 
            nrows), domain = NA)
        row.names <- NULL
    }
    if (is.null(row.names)) {
        if (nrows == 0L) 
            row.names <- character()
        else if (length(row.names <- names(x)) != nrows || anyDuplicated(row.names)) 
            row.names <- .set_row_names(nrows)
    }
    if (!is.null(names(x))) 
        names(x) <- NULL
    value <- list(x)
    if (!optional) 
        names(value) <- nm
    structure(value, row.names = row.names, class = "data.frame")
}


proc.time <- function ()  .Primitive("proc.time")


table <- function (..., exclude = if (useNA == "no") c(NA, NaN), useNA = c("no", 
    "ifany", "always"), dnn = list.names(...), deparse.level = 1) 
{
    list.names <- function(...) {
        l <- as.list(substitute(list(...)))[-1L]
        nm <- names(l)
        fixup <- if (is.null(nm)) 
            seq_along(l)
        else nm == ""
        dep <- vapply(l[fixup], function(x) switch(deparse.level + 
            1, "", if (is.symbol(x)) as.character(x) else "", 
            deparse(x, nlines = 1)[1L]), "")
        if (is.null(nm)) 
            dep
        else {
            nm[fixup] <- dep
            nm
        }
    }
    miss.use <- missing(useNA)
    miss.exc <- missing(exclude)
    useNA <- if (miss.use && !miss.exc && !match(NA, exclude, 
        nomatch = 0L)) 
        "ifany"
    else match.arg(useNA)
    doNA <- useNA != "no"
    if (!miss.use && !miss.exc && doNA && match(NA, exclude, 
        nomatch = 0L)) 
        warning("'exclude' containing NA and 'useNA' != \"no\"' are a bit contradicting")
    args <- list(...)
    if (!length(args)) 
        stop("nothing to tabulate")
    if (length(args) == 1L && is.list(args[[1L]])) {
        args <- args[[1L]]
        if (length(dnn) != length(args)) 
            dnn <- if (!is.null(argn <- names(args))) 
                argn
            else paste(dnn[1L], seq_along(args), sep = ".")
    }
    bin <- 0L
    lens <- NULL
    dims <- integer()
    pd <- 1L
    dn <- NULL
    for (a in args) {
        if (is.null(lens)) 
            lens <- length(a)
        else if (length(a) != lens) 
            stop("all arguments must have the same length")
        fact.a <- is.factor(a)
        if (doNA) 
            aNA <- anyNA(a)
        if (!fact.a) {
            a0 <- a
            a <- factor(a, exclude = exclude)
        }
        add.na <- doNA
        if (add.na) {
            ifany <- (useNA == "ifany")
            anNAc <- anyNA(a)
            add.na <- if (!ifany || anNAc) {
                ll <- levels(a)
                if (add.ll <- !anyNA(ll)) {
                  ll <- c(ll, NA)
                  TRUE
                }
                else if (!ifany && !anNAc) 
                  FALSE
                else TRUE
            }
            else FALSE
        }
        if (add.na) 
            a <- factor(a, levels = ll, exclude = NULL)
        else ll <- levels(a)
        a <- as.integer(a)
        if (fact.a && !miss.exc) {
            ll <- ll[keep <- which(match(ll, exclude, nomatch = 0L) == 
                0L)]
            a <- match(a, keep)
        }
        else if (!fact.a && add.na) {
            if (ifany && !aNA && add.ll) {
                ll <- ll[!is.na(ll)]
                is.na(a) <- match(a0, c(exclude, NA), nomatch = 0L) > 
                  0L
            }
            else {
                is.na(a) <- match(a0, exclude, nomatch = 0L) > 
                  0L
            }
        }
        nl <- length(ll)
        dims <- c(dims, nl)
        if (prod(dims) > .Machine$integer.max) 
            stop("attempt to make a table with >= 2^31 elements")
        dn <- c(dn, list(ll))
        bin <- bin + pd * (a - 1L)
        pd <- pd * nl
    }
    names(dn) <- dnn
    bin <- bin[!is.na(bin)]
    if (length(bin)) 
        bin <- bin + 1L
    y <- array(tabulate(bin, pd), dims, dimnames = dn)
    class(y) <- "table"
    y
}


enc2native <- function (x)  .Primitive("enc2native")


as.data.frame.table <- function (x, row.names = NULL, ..., responseName = "Freq", stringsAsFactors = TRUE, 
    sep = "", base = list(LETTERS)) 
{
    ex <- quote(data.frame(do.call("expand.grid", c(dimnames(provideDimnames(x, 
        sep = sep, base = base)), KEEP.OUT.ATTRS = FALSE, stringsAsFactors = stringsAsFactors)), 
        Freq = c(x), row.names = row.names))
    names(ex)[3L] <- responseName
    eval(ex)
}


debug <- function (fun, text = "", condition = NULL, signature = NULL) 
{
    if (is.null(signature)) 
        .Internal(debug(fun, text, condition))
    else if (requireNamespace("methods")) 
        methods::.debugMethod(fun, text, condition, signature, 
            once = FALSE)
    else stop("failed to load the methods package for debugging by signature")
}


OlsonNames <- function (tzdir = NULL) 
{
    if (is.null(tzdir)) {
        if (.Platform$OS.type == "windows") 
            tzdir <- Sys.getenv("TZDIR", file.path(R.home("share"), 
                "zoneinfo"))
        else {
            tzdirs <- c(Sys.getenv("TZDIR"), file.path(R.home("share"), 
                "zoneinfo"), "/usr/share/zoneinfo", "/share/zoneinfo", 
                "/usr/share/lib/zoneinfo", "/usr/lib/zoneinfo", 
                "/usr/local/etc/zoneinfo", "/etc/zoneinfo", "/usr/etc/zoneinfo")
            tzdirs <- tzdirs[file.exists(tzdirs)]
            if (!length(tzdirs)) {
                warning("no Olson database found")
                return(character())
            }
            else tzdir <- tzdirs[1L]
        }
    }
    else if (!dir.exists(tzdir)) 
        stop(sprintf("%s is not a directory", sQuote(tzdir)), 
            domain = NA)
    x <- list.files(tzdir, recursive = TRUE)
    ver <- if (file.exists(vf <- file.path(tzdir, "VERSION"))) 
        readLines(vf, warn = FALSE)
    else if (file.exists(vf <- file.path(tzdir, "+VERSION"))) 
        readLines(vf, warn = FALSE)
    x <- setdiff(x, "VERSION")
    ans <- grep("^[ABCDEFGHIJKLMNOPQRSTUVWXYZ]", x, value = TRUE)
    if (!is.null(ver)) 
        attr(ans, "Version") <- ver
    ans
}


.Library <- "C:/PROGRA~1/R/R-36~1.0/library"


isOpen <- function (con, rw = "") 
{
    rw <- pmatch(rw, c("read", "write"), 0L)
    .Internal(isOpen(con, rw))
}


grepRaw <- function (pattern, x, offset = 1L, ignore.case = FALSE, value = FALSE, 
    fixed = FALSE, all = FALSE, invert = FALSE) 
{
    if (!is.raw(pattern)) 
        pattern <- charToRaw(as.character(pattern))
    if (!is.raw(x)) 
        x <- charToRaw(as.character(x))
    .Internal(grepRaw(pattern, x, offset, ignore.case, fixed, 
        value, all, invert))
}


nullfile <- function () 
if (.Platform$OS.type == "windows") "nul:" else "/dev/null"


rowsum <- function (x, group, reorder = TRUE, ...) 
UseMethod("rowsum")


as.list.data.frame <- function (x, ...) 
{
    x <- unclass(x)
    attr(x, "row.names") <- NULL
    x
}


as.list <- function (x, ...) 
UseMethod("as.list")


topenv <- function (envir = parent.frame(), matchThisEnv = getOption("topLevelEnvironment")) 
{
    .Internal(topenv(envir, matchThisEnv))
}


system.file <- function (..., package = "base", lib.loc = NULL, mustWork = FALSE) 
{
    if (nargs() == 0L) 
        return(file.path(.Library, "base"))
    if (length(package) != 1L) 
        stop("'package' must be of length 1")
    packagePath <- find.package(package, lib.loc, quiet = TRUE)
    ans <- if (length(packagePath)) {
        FILES <- file.path(packagePath, ...)
        present <- file.exists(FILES)
        if (any(present)) 
            FILES[present]
        else ""
    }
    else ""
    if (mustWork && identical(ans, "")) 
        stop("no file found")
    ans
}


as.character.factor <- function (x, ...) 
.Internal(asCharacterFactor(x))


print.table <- function (x, digits = getOption("digits"), quote = FALSE, na.print = "", 
    zero.print = "0", right = is.numeric(x) || is.complex(x), 
    justify = "none", ...) 
{
    d <- dim(x)
    if (any(d == 0)) {
        cat("< table of extent", paste(d, collapse = " x "), 
            ">\n")
        return(invisible(x))
    }
    xx <- format(unclass(x), digits = digits, justify = justify)
    if (any(ina <- is.na(x))) 
        xx[ina] <- na.print
    if (zero.print != "0" && any(i0 <- !ina & x == 0)) 
        xx[i0] <- zero.print
    print(xx, quote = quote, right = right, ...)
    invisible(x)
}


lockEnvironment <- function (env, bindings = FALSE) 
.Internal(lockEnvironment(env, bindings))


xtfrm.default <- function (x) 
if (is.numeric(x)) unclass(x) else as.vector(rank(x, ties.method = "min", 
    na.last = "keep"))


mem.limits <- function (nsize = NA, vsize = NA) 
.Defunct("gc")


all.vars <- function (expr, functions = FALSE, max.names = -1L, unique = TRUE) 
.Internal(all.names(expr, functions, max.names, unique))


format.default <- function (x, trim = FALSE, digits = NULL, nsmall = 0L, justify = c("left", 
    "right", "centre", "none"), width = NULL, na.encode = TRUE, 
    scientific = NA, big.mark = "", big.interval = 3L, small.mark = "", 
    small.interval = 5L, decimal.mark = getOption("OutDec"), 
    zero.print = NULL, drop0trailing = FALSE, ...) 
{
    justify <- match.arg(justify)
    if (is.list(x)) {
        if (missing(trim)) 
            trim <- TRUE
        if (missing(justify)) 
            justify <- "none"
        res <- lapply(X = x, FUN = function(xx, ...) format.default(unlist(xx), 
            ...), trim = trim, digits = digits, nsmall = nsmall, 
            justify = justify, width = width, na.encode = na.encode, 
            scientific = scientific, big.mark = big.mark, big.interval = big.interval, 
            small.mark = small.mark, small.interval = small.interval, 
            decimal.mark = decimal.mark, zero.print = zero.print, 
            drop0trailing = drop0trailing, ...)
        vapply(res, paste, "", collapse = ", ")
    }
    else {
        switch(mode(x), `NULL` = "NULL", character = {
            adj <- match(justify, c("left", "right", "centre", 
                "none")) - 1L
            .Internal(format(x, trim, digits, nsmall, width, 
                adj, na.encode, scientific, NA_character_))
        }, call = , expression = , `function` = , `(` = deparse(x, 
            backtick = TRUE), raw = as.character(x), S4 = {
            cld <- methods::getClassDef(cl <- class(x))
            pkg <- attr(cl, "package")
            paste0("<S4 class ", sQuote(cl), if (!is.null(pkg)) paste0(" [package ", 
                dQuote(pkg), "]"), if (!is.null(cld) && !is.null(sls <- cld@slots)) paste(" with", 
                length(sls), if (length(sls) == 1L) "slot" else "slots"), 
                ">")
        }, numeric = , logical = , complex = , environment = prettyNum(.Internal(format(x, 
            trim, digits, nsmall, width, 3L, na.encode, scientific, 
            decimal.mark)), big.mark = big.mark, big.interval = big.interval, 
            small.mark = small.mark, small.interval = small.interval, 
            decimal.mark = decimal.mark, input.d.mark = decimal.mark, 
            zero.print = zero.print, drop0trailing = drop0trailing, 
            is.cmplx = is.complex(x), preserve.width = if (trim) "individual" else "common"), 
            stop(gettextf("Found no format() method for class \"%s\"", 
                class(x)), domain = NA))
    }
}


choose <- function (n, k) 
.Internal(choose(n, k))


as.expression.default <- function (x, ...) 
.Internal(as.vector(x, "expression"))


bitwAnd <- function (a, b) 
.Internal(bitwiseAnd(a, b))


mem.maxVSize <- function (vsize = 0) 
.Internal(mem.maxVSize(vsize))


tabulate <- function (bin, nbins = max(1L, bin, na.rm = TRUE)) 
{
    if (!is.numeric(bin) && !is.factor(bin)) 
        stop("'bin' must be numeric or a factor")
    if (typeof(bin) != "integer") 
        bin <- as.integer(bin)
    if (nbins > .Machine$integer.max) 
        stop("attempt to make a table with >= 2^31 elements")
    nbins <- as.integer(nbins)
    if (is.na(nbins)) 
        stop(gettextf("invalid value of %s", "'nbins'"), domain = NA)
    .Internal(tabulate(bin, nbins))
}


cut.POSIXt <- function (x, breaks, labels = NULL, start.on.monday = TRUE, right = FALSE, 
    ...) 
{
    if (!inherits(x, "POSIXt")) 
        stop("'x' must be a date-time object")
    x <- as.POSIXct(x)
    if (inherits(breaks, "POSIXt")) {
        breaks <- sort(as.POSIXct(breaks))
    }
    else if (is.numeric(breaks) && length(breaks) == 1L) {
    }
    else if (is.character(breaks) && length(breaks) == 1L) {
        by2 <- strsplit(breaks, " ", fixed = TRUE)[[1L]]
        if (length(by2) > 2L || length(by2) < 1L) 
            stop("invalid specification of 'breaks'")
        valid <- pmatch(by2[length(by2)], c("secs", "mins", "hours", 
            "days", "weeks", "months", "years", "DSTdays", "quarters"))
        if (is.na(valid)) 
            stop("invalid specification of 'breaks'")
        start <- as.POSIXlt(min(x, na.rm = TRUE))
        incr <- 1
        if (valid > 1L) {
            start$sec <- 0L
            incr <- 60
        }
        if (valid > 2L) {
            start$min <- 0L
            incr <- 3600
        }
        if (valid > 3L) {
            start$hour <- 0L
            start$isdst <- -1L
            incr <- 86400
        }
        if (valid == 5L) {
            start$mday <- start$mday - start$wday
            if (start.on.monday) 
                start$mday <- start$mday + ifelse(start$wday > 
                  0L, 1L, -6L)
            incr <- 7 * 86400
        }
        if (valid == 8L) 
            incr <- 25 * 3600
        if (valid == 6L) {
            start$mday <- 1L
            end <- as.POSIXlt(max(x, na.rm = TRUE))
            step <- if (length(by2) == 2L) 
                as.integer(by2[1L])
            else 1L
            end <- as.POSIXlt(end + (31 * step * 86400))
            end$mday <- 1L
            end$isdst <- -1L
            breaks <- seq(start, end, breaks)
        }
        else if (valid == 7L) {
            start$mon <- 0L
            start$mday <- 1L
            end <- as.POSIXlt(max(x, na.rm = TRUE))
            step <- if (length(by2) == 2L) 
                as.integer(by2[1L])
            else 1L
            end <- as.POSIXlt(end + (366 * step * 86400))
            end$mon <- 0L
            end$mday <- 1L
            end$isdst <- -1L
            breaks <- seq(start, end, breaks)
        }
        else if (valid == 9L) {
            qtr <- rep(c(0L, 3L, 6L, 9L), each = 3L)
            start$mon <- qtr[start$mon + 1L]
            start$mday <- 1L
            maxx <- max(x, na.rm = TRUE)
            end <- as.POSIXlt(maxx)
            step <- if (length(by2) == 2L) 
                as.integer(by2[1L])
            else 1L
            end <- as.POSIXlt(end + (93 * step * 86400))
            end$mon <- qtr[end$mon + 1L]
            end$mday <- 1L
            end$isdst <- -1L
            breaks <- seq(start, end, paste(step * 3, "months"))
            lb <- length(breaks)
            if (maxx < breaks[lb - 1]) 
                breaks <- breaks[-lb]
        }
        else {
            if (length(by2) == 2L) 
                incr <- incr * as.integer(by2[1L])
            maxx <- max(x, na.rm = TRUE)
            breaks <- seq(start, maxx + incr, breaks)
            breaks <- breaks[seq_len(1 + max(which(breaks <= 
                maxx)))]
        }
    }
    else stop("invalid specification of 'breaks'")
    res <- cut(unclass(x), unclass(breaks), labels = labels, 
        right = right, ...)
    if (is.null(labels)) {
        levels(res) <- as.character(if (is.numeric(breaks)) x[!duplicated(res)] else breaks[-length(breaks)])
    }
    res
}


as.POSIXlt.Date <- function (x, ...) 
.Internal(Date2POSIXlt(x))


c.noquote <- function (..., recursive = FALSE) 
structure(NextMethod("c"), class = "noquote")


check_tzones <- function (...) 
{
    tzs <- unique(sapply(list(...), function(x) {
        y <- attr(x, "tzone")
        if (is.null(y)) 
            ""
        else y[1L]
    }))
    tzs <- tzs[nzchar(tzs)]
    if (length(tzs) > 1L) 
        warning("'tzone' attributes are inconsistent")
    if (length(tzs)) 
        tzs[1L]
    else NULL
}


file.size <- function (...) 
file.info(..., extra_cols = FALSE)$size


`[[.POSIXct` <- function (x, ..., drop = TRUE) 
.POSIXct(NextMethod("[["), attr(x, "tzone"), oldClass(x))


makeActiveBinding <- function (sym, fun, env) 
{
    if (is.character(sym)) 
        sym <- as.name(sym)
    .Internal(makeActiveBinding(sym, fun, env))
}


`:::` <- function (pkg, name) 
{
    pkg <- as.character(substitute(pkg))
    name <- as.character(substitute(name))
    get(name, envir = asNamespace(pkg), inherits = FALSE)
}


`[[.POSIXlt` <- function (x, i, drop = TRUE) 
{
    if (!missing(i) && is.character(i)) {
        i <- match(i, names(x), incomparables = c("", NA_character_))
    }
    .POSIXlt(lapply(X = unclass(x), FUN = "[[", i, drop = drop), 
        attr(x, "tzone"), oldClass(x))
}


c.Date <- function (..., recursive = FALSE) 
.Date(c(unlist(lapply(list(...), unclass))))


findInterval <- function (x, vec, rightmost.closed = FALSE, all.inside = FALSE, 
    left.open = FALSE) 
{
    if (!identical(FALSE, is.unsorted(vec))) 
        stop("'vec' must be sorted non-decreasingly and not contain NAs")
    .Internal(findInterval(as.double(vec), as.double(x), rightmost.closed, 
        all.inside, left.open))
}


all.equal.raw <- function (target, current, ..., check.attributes = TRUE) 
{
    if (!is.logical(check.attributes)) 
        stop(gettextf("'%s' must be logical", "check.attributes"), 
            domain = NA)
    msg <- if (check.attributes) 
        attr.all.equal(target, current, ...)
    if (data.class(target) != data.class(current)) {
        msg <- c(msg, paste0("target is ", data.class(target), 
            ", current is ", data.class(current)))
        return(msg)
    }
    lt <- length(target)
    lc <- length(current)
    if (lt != lc) {
        if (!is.null(msg)) 
            msg <- msg[-grep("\\bLengths\\b", msg)]
        msg <- c(msg, paste0("Lengths (", lt, ", ", lc, ") differ (comparison on first ", 
            ll <- min(lt, lc), " components)"))
        ll <- seq_len(ll)
        target <- target[ll]
        current <- current[ll]
    }
    nas <- is.na(target)
    nasc <- is.na(current)
    if (any(nas != nasc)) {
        msg <- c(msg, paste("'is.NA' value mismatch:", sum(nasc), 
            "in current", sum(nas), "in target"))
        return(msg)
    }
    ne <- !nas & (target != current)
    if (!any(ne) && is.null(msg)) 
        TRUE
    else if (sum(ne) == 1L) 
        c(msg, paste("1 element mismatch"))
    else if (sum(ne) > 1L) 
        c(msg, paste(sum(ne), "element mismatches"))
    else msg
}


parseNamespaceFile <- function (package, package.lib, mustExist = TRUE) 
{
    namespaceFilePath <- function(package, package.lib) file.path(package.lib, 
        package, "NAMESPACE")
    nativeRoutineMap <- function(useRegistration, symbolNames, 
        fixes) {
        proto <- list(useRegistration = FALSE, symbolNames = character())
        class(proto) <- "NativeRoutineMap"
        mergeNativeRoutineMaps(proto, useRegistration, symbolNames, 
            fixes)
    }
    mergeNativeRoutineMaps <- function(map, useRegistration, 
        symbolNames, fixes) {
        if (!useRegistration) 
            names(symbolNames) <- paste0(fixes[1L], names(symbolNames), 
                fixes[2L])
        else map$registrationFixes <- fixes
        map$useRegistration <- map$useRegistration || useRegistration
        map$symbolNames <- c(map$symbolNames, symbolNames)
        map
    }
    nsFile <- namespaceFilePath(package, package.lib)
    descfile <- file.path(package.lib, package, "DESCRIPTION")
    enc <- if (file.exists(descfile)) {
        read.dcf(file = descfile, "Encoding")[1L]
    }
    else NA_character_
    if (file.exists(nsFile)) 
        directives <- if (!is.na(enc) && !Sys.getlocale("LC_CTYPE") %in% 
            c("C", "POSIX")) {
            lines <- readLines(nsFile, warn = FALSE)
            tmp <- iconv(lines, from = enc, to = "")
            bad <- which(is.na(tmp))
            comm <- grep("^[[:space:]]*#", lines[bad], invert = TRUE, 
                useBytes = TRUE)
            if (length(bad[comm])) 
                stop("unable to re-encode some lines in NAMESPACE file")
            tmp <- iconv(lines, from = enc, to = "", sub = "byte")
            con <- textConnection(tmp)
            on.exit(close(con))
            parse(con, keep.source = FALSE, srcfile = NULL)
        }
        else parse(nsFile, keep.source = FALSE, srcfile = NULL)
    else if (mustExist) 
        stop(gettextf("package %s has no 'NAMESPACE' file", sQuote(package)), 
            domain = NA)
    else directives <- NULL
    exports <- character()
    exportPatterns <- character()
    exportClasses <- character()
    exportClassPatterns <- character()
    exportMethods <- character()
    imports <- list()
    importMethods <- list()
    importClasses <- list()
    dynlibs <- character()
    nS3methods <- 1000L
    S3methods <- matrix(NA_character_, nS3methods, 4L)
    nativeRoutines <- list()
    nS3 <- 0L
    parseDirective <- function(e) {
        asChar <- function(cc) {
            r <- as.character(cc)
            if (any(r == "")) 
                stop(gettextf("empty name in directive '%s' in 'NAMESPACE' file", 
                  as.character(e[[1L]])), domain = NA)
            r
        }
        evalToChar <- function(cc) {
            vars <- all.vars(cc)
            names(vars) <- vars
            as.character(eval(eval(call("substitute", cc, as.list(vars))), 
                .GlobalEnv))
        }
        switch(as.character(e[[1L]]), `if` = if (eval(e[[2L]], 
            .GlobalEnv)) parseDirective(e[[3L]]) else if (length(e) == 
            4L) parseDirective(e[[4L]]), `{` = for (ee in as.list(e[-1L])) parseDirective(ee), 
            `=` = , `<-` = {
                parseDirective(e[[3L]])
                if (as.character(e[[3L]][[1L]]) == "useDynLib") names(dynlibs)[length(dynlibs)] <<- asChar(e[[2L]])
            }, export = {
                exp <- e[-1L]
                exp <- structure(asChar(exp), names = names(exp))
                exports <<- c(exports, exp)
            }, exportPattern = {
                pat <- asChar(e[-1L])
                exportPatterns <<- c(pat, exportPatterns)
            }, exportClassPattern = {
                pat <- asChar(e[-1L])
                exportClassPatterns <<- c(pat, exportClassPatterns)
            }, exportClass = , exportClasses = {
                exportClasses <<- c(asChar(e[-1L]), exportClasses)
            }, exportMethods = {
                exportMethods <<- c(asChar(e[-1L]), exportMethods)
            }, import = {
                except <- e$except
                e$except <- NULL
                pkgs <- as.list(asChar(e[-1L]))
                if (!is.null(except)) {
                  pkgs <- lapply(pkgs, list, except = evalToChar(except))
                }
                imports <<- c(imports, pkgs)
            }, importFrom = {
                imp <- e[-1L]
                ivars <- imp[-1L]
                inames <- names(ivars)
                imp <- list(asChar(imp[1L]), structure(asChar(ivars), 
                  names = inames))
                imports <<- c(imports, list(imp))
            }, importClassFrom = , importClassesFrom = {
                imp <- asChar(e[-1L])
                pkg <- imp[[1L]]
                impClasses <- imp[-1L]
                imp <- list(asChar(pkg), asChar(impClasses))
                importClasses <<- c(importClasses, list(imp))
            }, importMethodsFrom = {
                imp <- asChar(e[-1L])
                pkg <- imp[[1L]]
                impMethods <- imp[-1L]
                imp <- list(asChar(pkg), asChar(impMethods))
                importMethods <<- c(importMethods, list(imp))
            }, useDynLib = {
                dyl <- as.character(e[2L])
                dynlibs <<- structure(c(dynlibs, dyl), names = c(names(dynlibs), 
                  ifelse(!is.null(names(e)) && nzchar(names(e)[2L]), 
                    names(e)[2L], "")))
                if (length(e) > 2L) {
                  symNames <- as.character(e[-c(1L, 2L)])
                  names(symNames) <- names(e[-c(1, 2)])
                  if (length(names(symNames)) == 0L) names(symNames) <- symNames else if (any(w <- names(symNames) == 
                    "")) {
                    names(symNames)[w] <- symNames[w]
                  }
                  dup <- duplicated(names(symNames))
                  if (any(dup)) warning(gettextf("duplicate symbol names %s in useDynLib(\"%s\")", 
                    paste(sQuote(names(symNames)[dup]), collapse = ", "), 
                    dyl), domain = NA, call. = FALSE)
                  symNames <- symNames[!dup]
                  fixes <- c("", "")
                  idx <- match(".fixes", names(symNames))
                  if (!is.na(idx)) {
                    if (nzchar(symNames[idx])) {
                      e <- parse(text = symNames[idx], keep.source = FALSE, 
                        srcfile = NULL)[[1L]]
                      if (is.call(e)) val <- eval(e, .GlobalEnv) else val <- as.character(e)
                      if (length(val)) fixes[seq_along(val)] <- val
                    }
                    symNames <- symNames[-idx]
                  }
                  useRegistration <- FALSE
                  idx <- match(".registration", names(symNames))
                  if (!is.na(idx)) {
                    useRegistration <- as.logical(symNames[idx])
                    symNames <- symNames[-idx]
                  }
                  nativeRoutines[[dyl]] <<- if (dyl %in% names(nativeRoutines)) mergeNativeRoutineMaps(nativeRoutines[[dyl]], 
                    useRegistration, symNames, fixes) else nativeRoutineMap(useRegistration, 
                    symNames, fixes)
                }
            }, S3method = {
                spec <- e[-1L]
                if (length(spec) != 2L && length(spec) != 3L) stop(gettextf("bad 'S3method' directive: %s", 
                  deparse(e)), call. = FALSE, domain = NA)
                nS3 <<- nS3 + 1L
                if (nS3 > nS3methods) {
                  old <- S3methods
                  nold <- nS3methods
                  nS3methods <<- nS3methods * 2L
                  new <- matrix(NA_character_, nS3methods, 4L)
                  ind <- seq_len(nold)
                  for (i in 1:4) new[ind, i] <- old[ind, i]
                  S3methods <<- new
                  rm(old, new)
                }
                if (is.call(gen <- spec[[1L]]) && identical(as.character(gen[[1L]]), 
                  "::")) {
                  pkg <- as.character(gen[[2L]])[1L]
                  gen <- as.character(gen[[3L]])[1L]
                  S3methods[nS3, c(seq_along(spec), 4L)] <<- c(gen, 
                    asChar(spec[-1L]), pkg)
                } else S3methods[nS3, seq_along(spec)] <<- asChar(spec)
            }, stop(gettextf("unknown namespace directive: %s", 
                deparse(e, nlines = 1L)), call. = FALSE, domain = NA))
    }
    for (e in directives) parseDirective(e)
    dynlibs <- dynlibs[!duplicated(dynlibs)]
    list(imports = imports, exports = exports, exportPatterns = unique(exportPatterns), 
        importClasses = importClasses, importMethods = importMethods, 
        exportClasses = unique(exportClasses), exportMethods = unique(exportMethods), 
        exportClassPatterns = unique(exportClassPatterns), dynlibs = dynlibs, 
        nativeRoutines = nativeRoutines, S3methods = unique(S3methods[seq_len(nS3), 
            , drop = FALSE]))
}


icuSetCollate <- function (...) 
.Internal(icuSetCollate(...))


print.numeric_version <- function (x, ...) 
{
    y <- as.character(x)
    if (!length(y)) 
        writeLines(gettext("<0 elements>"))
    else if (any("quote" == names(list(...)))) 
        print(ifelse(is.na(y), NA_character_, sQuote(y)), ...)
    else print(ifelse(is.na(y), NA_character_, sQuote(y)), quote = FALSE, 
        ...)
    invisible(x)
}


Sys.localeconv <- function () 
.Internal(Sys.localeconv())


conditionCall <- function (c) 
UseMethod("conditionCall")


`!.hexmode` <- function (a) 
as.hexmode(bitwNot(as.hexmode(a)))


margin.table <- function (x, margin = NULL) 
{
    if (!is.array(x)) 
        stop("'x' is not an array")
    if (length(margin)) {
        z <- apply(x, margin, sum)
        dim(z) <- dim(x)[margin]
        dimnames(z) <- dimnames(x)[margin]
    }
    else return(sum(x))
    class(z) <- oldClass(x)
    z
}


`<<-` <- .Primitive("<<-")


sprintf <- function (fmt, ...) 
.Internal(sprintf(fmt, ...))


duplicated.matrix <- function (x, incomparables = FALSE, MARGIN = 1L, fromLast = FALSE, 
    ...) 
{
    if (!isFALSE(incomparables)) 
        .NotYetUsed("incomparables != FALSE")
    dx <- dim(x)
    ndim <- length(dx)
    if (any(MARGIN > ndim)) 
        stop(gettextf("MARGIN = %s is invalid for dim = %s", 
            paste(MARGIN, collapse = ","), paste(dx, collapse = ",")), 
            domain = NA)
    temp <- if ((ndim > 1L) && (prod(dx[-MARGIN]) > 1L)) 
        asplit(x, MARGIN)
    else x
    res <- duplicated.default(temp, fromLast = fromLast, ...)
    dim(res) <- dim(temp)
    dimnames(res) <- dimnames(temp)
    res
}


.ArgsEnv <- "<environment>"

cbind.data.frame <- function (..., deparse.level = 1) 
data.frame(..., check.names = FALSE)


read.dcf <- function (file, fields = NULL, all = FALSE, keep.white = NULL) 
{
    if (is.character(file)) {
        file <- gzfile(file)
        on.exit(close(file))
    }
    if (!inherits(file, "connection")) 
        stop("'file' must be a character string or connection")
    if (!all) 
        return(.Internal(readDCF(file, fields, keep.white)))
    .assemble_things_into_a_data_frame <- function(tags, vals, 
        nums) {
        tf <- factor(tags, levels = unique(tags))
        cnts <- table(nums, tf)
        out <- array(NA_character_, dim = dim(cnts), dimnames = list(NULL, 
            levels(tf)))
        if (all(cnts <= 1L)) {
            out[cbind(nums, tf)] <- vals
            out <- as.data.frame(out, stringsAsFactors = FALSE)
        }
        else {
            levs <- colSums(cnts > 1L) == 0L
            if (any(levs)) {
                inds <- tf %in% levels(tf)[levs]
                out[cbind(nums[inds], tf[inds])] <- vals[inds]
            }
            out <- as.data.frame(out, stringsAsFactors = FALSE)
            for (l in levels(tf)[!levs]) {
                out[[l]] <- rep.int(list(NA_character_), nrow(cnts))
                i <- tf == l
                out[[l]][unique(nums[i])] <- split(vals[i], nums[i])
            }
        }
        out
    }
    ctype <- Sys.getlocale("LC_CTYPE")
    on.exit(Sys.setlocale("LC_CTYPE", ctype), add = TRUE)
    Sys.setlocale("LC_CTYPE", "C")
    lines <- readLines(file, skipNul = TRUE)
    ind <- grep("^[^[:blank:]][^:]*$", lines)
    if (length(ind)) {
        lines <- strtrim(lines[ind], 0.7 * getOption("width"))
        stop(gettextf("Invalid DCF format.\nRegular lines must have a tag.\nOffending lines start with:\n%s", 
            paste0("  ", lines, collapse = "\n")), domain = NA)
    }
    line_is_not_empty <- !grepl("^[[:space:]]*$", lines)
    nums <- cumsum(diff(c(FALSE, line_is_not_empty) > 0L) > 0L)
    nums <- nums[line_is_not_empty]
    lines <- lines[line_is_not_empty]
    line_is_escaped_blank <- grepl("^[[:space:]]+\\.[[:space:]]*$", 
        lines)
    if (any(line_is_escaped_blank)) 
        lines[line_is_escaped_blank] <- ""
    line_has_tag <- grepl("^[^[:blank:]][^:]*:", lines)
    pos <- which(diff(nums) > 0L) + 1L
    ind <- !line_has_tag[pos]
    if (any(ind)) {
        lines <- strtrim(lines[pos[ind]], 0.7 * getOption("width"))
        stop(gettextf("Invalid DCF format.\nContinuation lines must not start a record.\nOffending lines start with:\n%s", 
            paste0("  ", lines, collapse = "\n")), domain = NA)
    }
    lengths <- rle(cumsum(line_has_tag))$lengths
    pos <- cumsum(lengths)
    tags <- sub(":.*", "", lines[line_has_tag])
    lines[line_has_tag] <- sub("[^:]*:[[:space:]]*", "", lines[line_has_tag])
    fold <- is.na(match(tags, keep.white))
    foldable <- rep.int(fold, lengths)
    lines[foldable] <- sub("^[[:space:]]*", "", lines[foldable])
    lines[foldable] <- sub("[[:space:]]*$", "", lines[foldable])
    vals <- mapply(function(from, to) paste(lines[from:to], collapse = "\n"), 
        c(1L, pos[-length(pos)] + 1L), pos)
    vals[fold] <- trimws(vals[fold])
    out <- .assemble_things_into_a_data_frame(tags, vals, nums[pos])
    if (!is.null(fields)) 
        out <- out[fields]
    out
}


names <- function (x)  .Primitive("names")


append <- function (x, values, after = length(x)) 
{
    lengx <- length(x)
    if (!after) 
        c(values, x)
    else if (after >= lengx) 
        c(x, values)
    else c(x[1L:after], values, x[(after + 1L):lengx])
}


rowsum.data.frame <- function (x, group, reorder = TRUE, na.rm = FALSE, ...) 
{
    if (!is.data.frame(x)) 
        stop("not a data frame")
    if (length(group) != NROW(x)) 
        stop("incorrect length for 'group'")
    if (anyNA(group)) 
        warning("missing values for 'group'")
    ugroup <- unique(group)
    if (reorder) 
        ugroup <- sort(ugroup, na.last = TRUE, method = "quick")
    .Internal(rowsum_df(x, group, ugroup, na.rm, as.character(ugroup)))
}


mat.or.vec <- function (nr, nc) 
if (nc == 1L) numeric(nr) else matrix(0, nr, nc)


as.POSIXlt.POSIXct <- function (x, tz = "", ...) 
{
    if ((missing(tz) || is.null(tz)) && !is.null(tzone <- attr(x, 
        "tzone"))) 
        tz <- tzone[1L]
    .Internal(as.POSIXlt(x, tz))
}


`is.na<-.default` <- function (x, value) 
{
    x[value] <- NA
    x
}


`split<-` <- function (x, f, drop = FALSE, ..., value) 
UseMethod("split<-")


R.version.string <- "R version 3.6.0 (2019-04-26)"


tanpi <- function (x)  .Primitive("tanpi")


namespaceExport <- function (ns, vars) 
{
    namespaceIsSealed <- function(ns) environmentIsLocked(ns)
    if (namespaceIsSealed(ns)) 
        stop("cannot add to exports of a sealed namespace")
    ns <- asNamespace(ns, base.OK = FALSE)
    if (length(vars)) {
        addExports <- function(ns, new) {
            exports <- .getNamespaceInfo(ns, "exports")
            expnames <- names(new)
            objs <- names(exports)
            ex <- expnames %in% objs
            if (any(ex)) 
                warning(sprintf(ngettext(sum(ex), "previous export '%s' is being replaced", 
                  "previous exports '%s' are being replaced"), 
                  paste(sQuote(expnames[ex]), collapse = ", ")), 
                  call. = FALSE, domain = NA)
            list2env(as.list(new), exports)
        }
        makeImportExportNames <- function(spec) {
            old <- as.character(spec)
            new <- names(spec)
            if (is.null(new)) 
                new <- old
            else {
                change <- !nzchar(new)
                new[change] <- old[change]
            }
            names(old) <- new
            old
        }
        new <- makeImportExportNames(unique(vars))
        undef <- new[!new %in% names(ns)]
        undef <- undef[!vapply(undef, exists, NA, envir = ns)]
        if (length(undef)) {
            undef <- do.call("paste", as.list(c(undef, sep = ", ")))
            stop(gettextf("undefined exports: %s", undef), domain = NA)
        }
        if (.isMethodsDispatchOn()) 
            .mergeExportMethods(new, ns)
        addExports(ns, new)
    }
}


Summary.numeric_version <- function (..., na.rm) 
{
    ok <- switch(.Generic, max = , min = , range = TRUE, FALSE)
    if (!ok) 
        stop(gettextf("%s not defined for \"numeric_version\" objects", 
            .Generic), domain = NA)
    x <- do.call("c", lapply(list(...), as.numeric_version))
    v <- xtfrm(x)
    if (!na.rm && length(pos <- which(is.na(v)))) {
        y <- x[pos[1L]]
        if (as.character(.Generic) == "range") 
            c(y, y)
        else y
    }
    else switch(.Generic, max = x[which.max(v)], min = x[which.min(v)], 
        range = x[c(which.min(v), which.max(v))])
}


prmatrix <- function (x, rowlab = dn[[1]], collab = dn[[2]], quote = TRUE, 
    right = FALSE, na.print = NULL, ...) 
{
    x <- as.matrix(x)
    dn <- dimnames(x)
    .Internal(prmatrix(x, rowlab, collab, quote, right, na.print))
}


as.single.default <- function (x, ...) 
structure(.Internal(as.vector(x, "double")), Csingle = TRUE)


expand.grid <- function (..., KEEP.OUT.ATTRS = TRUE, stringsAsFactors = TRUE) 
{
    nargs <- length(args <- list(...))
    if (!nargs) 
        return(as.data.frame(list()))
    if (nargs == 1L && is.list(a1 <- args[[1L]])) 
        nargs <- length(args <- a1)
    if (nargs == 0L) 
        return(as.data.frame(list()))
    cargs <- vector("list", nargs)
    iArgs <- seq_len(nargs)
    nmc <- paste0("Var", iArgs)
    nm <- names(args)
    if (is.null(nm)) 
        nm <- nmc
    else if (any(ng0 <- nzchar(nm))) 
        nmc[ng0] <- nm[ng0]
    names(cargs) <- nmc
    rep.fac <- 1L
    d <- lengths(args)
    if (KEEP.OUT.ATTRS) {
        dn <- vector("list", nargs)
        names(dn) <- nmc
    }
    orep <- prod(d)
    if (orep == 0L) {
        for (i in iArgs) cargs[[i]] <- args[[i]][FALSE]
    }
    else {
        for (i in iArgs) {
            x <- args[[i]]
            if (KEEP.OUT.ATTRS) 
                dn[[i]] <- paste0(nmc[i], "=", if (is.numeric(x)) 
                  format(x)
                else x)
            nx <- length(x)
            orep <- orep/nx
            x <- x[rep.int(rep.int(seq_len(nx), rep.int(rep.fac, 
                nx)), orep)]
            if (stringsAsFactors && is.character(x) && !is.factor(x)) 
                x <- factor(x, levels = unique(x))
            cargs[[i]] <- x
            rep.fac <- rep.fac * nx
        }
    }
    if (KEEP.OUT.ATTRS) 
        attr(cargs, "out.attrs") <- list(dim = d, dimnames = dn)
    rn <- .set_row_names(as.integer(prod(d)))
    structure(cargs, class = "data.frame", row.names = rn)
}


vapply <- function (X, FUN, FUN.VALUE, ..., USE.NAMES = TRUE) 
{
    FUN <- match.fun(FUN)
    if (!is.vector(X) || is.object(X)) 
        X <- as.list(X)
    .Internal(vapply(X, FUN, FUN.VALUE, USE.NAMES))
}


dimnames <- function (x)  .Primitive("dimnames")


`@<-` <- .Primitive("@<-")


.popath <- "C:/Program Files/R/R-3.6.0/library/translations"


kappa <- function (z, ...) 
UseMethod("kappa")


identical <- function (x, y, num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, 
    ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE) 
.Internal(identical(x, y, num.eq, single.NA, attrib.as.set, ignore.bytecode, 
    ignore.environment, ignore.srcref))


chol.default <- function (x, pivot = FALSE, LINPACK = FALSE, tol = -1, ...) 
{
    if (is.complex(x)) 
        stop("complex matrices not permitted at present")
    .Internal(La_chol(as.matrix(x), pivot, tol))
}


sapply <- function (X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE) 
{
    FUN <- match.fun(FUN)
    answer <- lapply(X = X, FUN = FUN, ...)
    if (USE.NAMES && is.character(X) && is.null(names(answer))) 
        names(answer) <- X
    if (!isFALSE(simplify) && length(answer)) 
        simplify2array(answer, higher = (simplify == "array"))
    else answer
}


.format.zeros <- function (x, zero.print, nx = suppressWarnings(as.numeric(x)), 
    replace = FALSE, warn.non.fitting = TRUE) 
{
    if (!is.null(zero.print) && any(i0 <- nx == 0 & !is.na(nx))) {
        if (length(zero.print) > 1L) 
            stop("'zero.print' has length > 1")
        if (is.logical(zero.print)) 
            zero.print <- if (zero.print) 
                "0"
            else " "
        if (!is.character(zero.print)) 
            stop("'zero.print' must be character, logical or NULL")
        nz <- nchar(zero.print, "c")
        nc <- nchar(x[i0], "c")
        ind0 <- as.vector(regexpr("0", x[i0], fixed = TRUE))
        if (replace) {
            x[i0] <- zero.print
        }
        else {
            if (any(nc < nz) && warn.non.fitting) 
                warning("'zero.print' is truncated to fit into formatted zeros; consider 'replace=TRUE'")
            i2 <- pmin(nc, nz - 1L + ind0)
            i1 <- pmax(1L, i2 - nz + 1L)
            substr(x[i0], i1, i2) <- zero.print
            if (any(P <- nc > i2)) 
                substr(x[i0][P], i2[P] + 1L, nc[P]) <- strrep(" ", 
                  (nc - i2)[P])
        }
    }
    x
}


gctorture <- function (on = TRUE) 
.Internal(gctorture(on))


missing <- function (x)  .Primitive("missing")


abbreviate <- function (names.arg, minlength = 4L, use.classes = TRUE, dot = FALSE, 
    strict = FALSE, method = c("left.kept", "both.sides"), named = TRUE) 
{
    if (minlength <= 0L) {
        x <- rep.int("", length(names.arg))
        if (named) 
            names(x) <- names.arg
        return(x)
    }
    names.arg <- sub("^ +", "", sub(" +$", "", as.character(names.arg)))
    dups <- duplicated(names.arg)
    old <- names.arg
    if (any(dups)) 
        names.arg <- names.arg[!dups]
    x <- names.arg
    if (strict) {
        x[] <- .Internal(abbreviate(x, minlength, use.classes))
    }
    else {
        method <- match.arg(method)
        if (method == "both.sides") 
            chRev <- function(x) sapply(lapply(strsplit(x, NULL), 
                rev), paste, collapse = "")
        dup2 <- rep.int(TRUE, length(names.arg))
        these <- names.arg
        repeat {
            ans <- .Internal(abbreviate(these, minlength, use.classes))
            x[dup2] <- ans
            if (!any(dup2 <- duplicated(x))) 
                break
            if (method == "both.sides") {
                x[dup2] <- chRev(.Internal(abbreviate(chRev(names.arg[dup2]), 
                  minlength, use.classes)))
                if (!any(dup2 <- duplicated(x))) 
                  break
            }
            minlength <- minlength + 1
            dup2 <- dup2 | match(x, x[dup2], 0L)
            these <- names.arg[dup2]
        }
    }
    if (any(dups)) 
        x <- x[match(old, names.arg)]
    if (dot) {
        chgd <- x != old
        x[chgd] <- paste0(x[chgd], ".")
    }
    if (named) 
        names(x) <- old
    x
}


showConnections <- function (all = FALSE) 
{
    gc()
    set <- getAllConnections()
    if (!all) 
        set <- set[set > 2L]
    ans <- matrix("", length(set), 7L)
    for (i in seq_along(set)) ans[i, ] <- unlist(summary.connection(set[i]))
    rownames(ans) <- set
    colnames(ans) <- c("description", "class", "mode", "text", 
        "isopen", "can read", "can write")
    if (!all) 
        ans[ans[, 5L] == "opened", , drop = FALSE]
    else ans[, , drop = FALSE]
}


merge <- function (x, y, ...) 
UseMethod("merge")


nargs <- function ()  .Primitive("nargs")


is.R <- function () 
exists("version") && !is.null(vl <- version$language) && vl == 
    "R"


Sys.sleep <- function (time) 
.Internal(Sys.sleep(time))


reg.finalizer <- function (e, f, onexit = FALSE) 
.Internal(reg.finalizer(e, f, onexit))


isdebugged <- function (fun, signature = NULL) 
{
    if (is.null(signature)) 
        .Internal(isdebugged(fun))
    else if (requireNamespace("methods")) 
        methods::.isMethodDebugged(fun, signature)
    else stop("failed to load methods package for handling signature")
}


.C_R_getTaskCallbackNames <- structure(list(name = "R_getTaskCallbackNames", address = pointer("0x000000000df13bb0"), 
    dll = structure(list(name = "base", path = "base", dynamicLookup = FALSE, 
        handle = <pointer: (nil)>, info = pointer("0x000000000df73cc0")), class = "DLLInfo"), 
    numParameters = 0L), class = c("CallRoutine", "NativeSymbolInfo"
))


anyDuplicated.default <- function (x, incomparables = FALSE, fromLast = FALSE, ...) 
.Internal(anyDuplicated(x, incomparables, fromLast))


.fixupGFortranStderr <- function () 
{
    old <- Sys.getenv("GFORTRAN_STDERR_UNIT")
    if (nzchar(old) && old == "-1") {
        Sys.unsetenv("GFORTRAN_STDERR_UNIT")
        TRUE
    }
    else FALSE
}


mapply <- function (FUN, ..., MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE) 
{
    FUN <- match.fun(FUN)
    dots <- list(...)
    answer <- .Internal(mapply(FUN, dots, MoreArgs))
    if (USE.NAMES && length(dots)) {
        if (is.null(names1 <- names(dots[[1L]])) && is.character(dots[[1L]])) 
            names(answer) <- dots[[1L]]
        else if (!is.null(names1)) 
            names(answer) <- names1
    }
    if (!isFALSE(SIMPLIFY) && length(answer)) 
        simplify2array(answer, higher = (SIMPLIFY == "array"))
    else answer
}


asplit <- function (x, MARGIN) 
{
    dl <- length(dim(x))
    if (!dl) 
        stop("dim(x) must have a positive length")
    if (is.object(x)) 
        x <- if (dl == 2L) 
            as.matrix(x)
        else as.array(x)
    d <- dim(x)
    dn <- dimnames(x)
    ds <- seq_len(dl)
    if (is.character(MARGIN)) {
        if (is.null(dnn <- names(dn))) 
            stop("'x' must have named dimnames")
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
    newx <- aperm(x, c(s.call, s.ans))
    dim(newx) <- c(prod(d.call), d2)
    ans <- vector("list", d2)
    for (i in seq_len(d2)) {
        ans[[i]] <- array(newx[, i], d.call, dn.call)
    }
    array(ans, d.ans, dn.ans)
}


rep.factor <- function (x, ...) 
{
    y <- NextMethod()
    structure(y, class = class(x), levels = levels(x))
}


zapsmall <- function (x, digits = getOption("digits")) 
{
    if (length(digits) == 0L) 
        stop("invalid 'digits'")
    if (all(ina <- is.na(x))) 
        return(x)
    mx <- max(abs(x[!ina]))
    round(x, digits = if (mx > 0) max(0L, digits - as.numeric(log10(mx))) else digits)
}


outer <- function (X, Y, FUN = "*", ...) 
{
    if (is.array(X)) {
        dX <- dim(X)
        nx <- dimnames(X)
        no.nx <- is.null(nx)
    }
    else {
        dX <- length(X)
        no.nx <- is.null(names(X))
        if (!no.nx) 
            nx <- list(names(X))
    }
    if (is.array(Y)) {
        dY <- dim(Y)
        ny <- dimnames(Y)
        no.ny <- is.null(ny)
    }
    else {
        dY <- length(Y)
        no.ny <- is.null(names(Y))
        if (!no.ny) 
            ny <- list(names(Y))
    }
    robj <- if (is.character(FUN) && FUN == "*") {
        if (!missing(...)) 
            stop("using ... with FUN = \"*\" is an error")
        tcrossprod(as.vector(X), as.vector(Y))
    }
    else {
        FUN <- match.fun(FUN)
        Y <- rep(Y, rep.int(length(X), length(Y)))
        if (length(X)) 
            X <- rep(X, times = ceiling(length(Y)/length(X)))
        FUN(X, Y, ...)
    }
    dim(robj) <- c(dX, dY)
    if (!(no.nx && no.ny)) {
        if (no.nx) 
            nx <- vector("list", length(dX))
        else if (no.ny) 
            ny <- vector("list", length(dY))
        dimnames(robj) <- c(nx, ny)
    }
    robj
}


print.function <- function (x, useSource = TRUE, ...) 
print.default(x, useSource = useSource, ...)


qr.default <- function (x, tol = 1e-07, LAPACK = FALSE, ...) 
{
    x <- as.matrix(x)
    if (is.complex(x)) 
        return(structure(.Internal(La_qr_cmplx(x)), class = "qr"))
    if (LAPACK) 
        return(structure(.Internal(La_qr(x)), useLAPACK = TRUE, 
            class = "qr"))
    p <- as.integer(ncol(x))
    if (is.na(p)) 
        stop("invalid ncol(x)")
    n <- as.integer(nrow(x))
    if (is.na(n)) 
        stop("invalid nrow(x)")
    if (1 * n * p > 2147483647) 
        stop("too large a matrix for LINPACK")
    storage.mode(x) <- "double"
    res <- .Fortran(.F_dqrdc2, qr = x, n, n, p, as.double(tol), 
        rank = integer(1L), qraux = double(p), pivot = as.integer(seq_len(p)), 
        double(2L * p))[c(1, 6, 7, 8)]
    if (!is.null(cn <- colnames(x))) 
        colnames(res$qr) <- cn[res$pivot]
    class(res) <- "qr"
    res
}


backsolve <- function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE) 
{
    r <- as.matrix(r)
    x.mat <- is.matrix(x)
    if (!x.mat) 
        x <- as.matrix(x)
    z <- .Internal(backsolve(r, x, k, upper.tri, transpose))
    if (x.mat) 
        z
    else drop(z)
}


ISOdate <- function (year, month, day, hour = 12, min = 0, sec = 0, tz = "GMT") 
ISOdatetime(year, month, day, hour, min, sec, tz)


duplicated.array <- function (x, incomparables = FALSE, MARGIN = 1L, fromLast = FALSE, 
    ...) 
{
    if (!isFALSE(incomparables)) 
        .NotYetUsed("incomparables != FALSE")
    dx <- dim(x)
    ndim <- length(dx)
    if (any(MARGIN > ndim)) 
        stop(gettextf("MARGIN = %s is invalid for dim = %s", 
            paste(MARGIN, collapse = ","), paste(dx, collapse = ",")), 
            domain = NA)
    temp <- if ((ndim > 1L) && (prod(dx[-MARGIN]) > 1L)) 
        asplit(x, MARGIN)
    else x
    res <- duplicated.default(temp, fromLast = fromLast, ...)
    dim(res) <- dim(temp)
    dimnames(res) <- dimnames(temp)
    res
}


isS4 <- function (object)  .Primitive("isS4")


isBaseNamespace <- function (ns) 
identical(ns, .BaseNamespaceEnv)


errorCondition <- function (message, ..., class = NULL, call = NULL) 
structure(list(message = as.character(message), call = call, 
    ...), class = c(class, "error", "condition"))


.detach <- function (pos) 
.Internal(detach(pos))


Arg <- function (z)  .Primitive("Arg")


file.mtime <- function (...) 
file.info(..., extra_cols = FALSE)$mtime


setwd <- function (dir) 
.Internal(setwd(dir))


.kappa_tri <- function (z, exact = FALSE, LINPACK = TRUE, norm = NULL, ...) 
{
    if (exact) {
        stopifnot(is.null(norm) || identical("2", norm))
        kappa.default(z, exact = TRUE)
    }
    else {
        p <- as.integer(nrow(z))
        if (is.na(p)) 
            stop("invalid nrow(x)")
        if (p != ncol(z)) 
            stop("triangular matrix should be square")
        if (is.null(norm)) 
            norm <- "1"
        if (is.complex(z)) 
            1/.Internal(La_ztrcon(z, norm))
        else if (LINPACK) {
            if (norm == "I") 
                z <- t(z)
            storage.mode(z) <- "double"
            1/.Fortran(.F_dtrco, z, p, p, k = double(1), double(p), 
                1L)$k
        }
        else 1/.Internal(La_dtrcon(z, norm))
    }
}


`is.na<-.numeric_version` <- function (x, value) 
{
    x[value] <- rep.int(list(integer()), length(value))
    x
}


bitwNot <- function (a) 
.Internal(bitwiseNot(a))


.getRequiredPackages <- function (file = "DESCRIPTION", lib.loc = NULL, quietly = FALSE, 
    useImports = FALSE) 
{
    pkgInfo <- tools:::.split_description(tools:::.read_description(file))
    .getRequiredPackages2(pkgInfo, quietly, lib.loc, useImports)
    invisible()
}


getwd <- function () 
.Internal(getwd())


list2env <- function (x, envir = NULL, parent = parent.frame(), hash = (length(x) > 
    100), size = max(29L, length(x))) 
{
    if (is.null(envir)) 
        envir <- new.env(hash = hash, parent = parent, size = size)
    .Internal(list2env(x, envir))
}


set.seed <- function (seed, kind = NULL, normal.kind = NULL, sample.kind = NULL) 
{
    kinds <- c("Wichmann-Hill", "Marsaglia-Multicarry", "Super-Duper", 
        "Mersenne-Twister", "Knuth-TAOCP", "user-supplied", "Knuth-TAOCP-2002", 
        "L'Ecuyer-CMRG", "default")
    n.kinds <- c("Buggy Kinderman-Ramage", "Ahrens-Dieter", "Box-Muller", 
        "user-supplied", "Inversion", "Kinderman-Ramage", "default")
    s.kinds <- c("Rounding", "Rejection", "default")
    if (length(kind)) {
        if (!is.character(kind) || length(kind) > 1L) 
            stop("'kind' must be a character string of length 1 (RNG to be used).")
        if (is.na(i.knd <- pmatch(kind, kinds) - 1L)) 
            stop(gettextf("'%s' is not a valid abbreviation of an RNG", 
                kind), domain = NA)
        if (i.knd == length(kinds) - 1L) 
            i.knd <- -1L
    }
    else i.knd <- NULL
    if (!is.null(normal.kind)) {
        if (!is.character(normal.kind) || length(normal.kind) != 
            1L) 
            stop("'normal.kind' must be a character string of length 1")
        normal.kind <- pmatch(normal.kind, n.kinds) - 1L
        if (is.na(normal.kind)) 
            stop(gettextf("'%s' is not a valid choice", normal.kind), 
                domain = NA)
        if (normal.kind == 0L) 
            stop("buggy version of Kinderman-Ramage generator is not allowed", 
                domain = NA)
        if (normal.kind == length(n.kinds) - 1L) 
            normal.kind <- -1L
    }
    if (!is.null(sample.kind)) {
        if (!is.character(sample.kind) || length(sample.kind) != 
            1L) 
            stop("'sample.kind' must be a character string of length 1")
        sample.kind <- pmatch(sample.kind, s.kinds) - 1L
        if (is.na(sample.kind)) 
            stop(gettextf("'%s' is not a valid choice", sample.kind), 
                domain = NA)
        if (sample.kind == 0L) 
            warning("non-uniform 'Rounding' sampler used", domain = NA)
        if (sample.kind == length(s.kinds) - 1L) 
            sample.kind <- -1L
    }
    .Internal(set.seed(seed, i.knd, normal.kind, sample.kind))
}


`[[<-.POSIXlt` <- function (x, i, value) 
{
    cl <- oldClass(x)
    class(x) <- NULL
    if (!missing(i) && is.character(i)) {
        nms <- names(x$year)
        for (n in names(x)) names(x[[n]]) <- nms
    }
    value <- unclass(as.POSIXlt(value))
    for (n in names(x)) x[[n]][[i]] <- value[[n]]
    class(x) <- cl
    x
}


.doWrap <- function (vec, decr, nalast, noNA = NA) 
{
    if (length(vec) > 0 && is.numeric(vec)) {
        sorted <- .makeSortEnum(decr, nalast)
        if (is.na(noNA)) {
            if (is.na(nalast)) 
                noNA <- TRUE
            else if (nalast) 
                noNA <- !is.na(vec[length(vec)])
            else noNA <- !is.na(vec[1L])
        }
        .Internal(wrap_meta(vec, sorted, noNA))
    }
    else vec
}


is.primitive <- function (x) 
switch(typeof(x), special = , builtin = TRUE, FALSE)


grepl <- function (pattern, x, ignore.case = FALSE, perl = FALSE, fixed = FALSE, 
    useBytes = FALSE) 
{
    if (!is.character(x)) 
        x <- as.character(x)
    .Internal(grepl(as.character(pattern), x, ignore.case, FALSE, 
        perl, fixed, useBytes, FALSE))
}


Ops.data.frame <- function (e1, e2 = NULL) 
{
    isList <- function(x) !is.null(x) && is.list(x)
    unary <- nargs() == 1L
    lclass <- nzchar(.Method[1L])
    rclass <- !unary && (nzchar(.Method[2L]))
    value <- list()
    rn <- NULL
    FUN <- get(.Generic, envir = parent.frame(), mode = "function")
    f <- if (unary) 
        quote(FUN(left))
    else quote(FUN(left, right))
    lscalar <- rscalar <- FALSE
    if (lclass && rclass) {
        nr <- .row_names_info(e1, 2L)
        if (.row_names_info(e1) > 0L) 
            rn <- attr(e1, "row.names")
        cn <- names(e1)
        if (any(dim(e2) != dim(e1))) 
            stop(gettextf("%s only defined for equally-sized data frames", 
                sQuote(.Generic)), domain = NA)
    }
    else if (lclass) {
        nr <- .row_names_info(e1, 2L)
        if (.row_names_info(e1) > 0L) 
            rn <- attr(e1, "row.names")
        cn <- names(e1)
        rscalar <- length(e2) <= 1L
        if (isList(e2)) {
            if (rscalar) 
                e2 <- e2[[1L]]
            else if (length(e2) != ncol(e1)) 
                stop(gettextf("list of length %d not meaningful", 
                  length(e2)), domain = NA)
        }
        else {
            if (!rscalar) 
                e2 <- split(rep_len(as.vector(e2), prod(dim(e1))), 
                  rep.int(seq_len(ncol(e1)), rep.int(nrow(e1), 
                    ncol(e1))))
        }
    }
    else {
        nr <- .row_names_info(e2, 2L)
        if (.row_names_info(e2) > 0L) 
            rn <- attr(e2, "row.names")
        cn <- names(e2)
        lscalar <- length(e1) <= 1L
        if (isList(e1)) {
            if (lscalar) 
                e1 <- e1[[1L]]
            else if (length(e1) != ncol(e2)) 
                stop(gettextf("list of length %d not meaningful", 
                  length(e1)), domain = NA)
        }
        else {
            if (!lscalar) 
                e1 <- split(rep_len(as.vector(e1), prod(dim(e2))), 
                  rep.int(seq_len(ncol(e2)), rep.int(nrow(e2), 
                    ncol(e2))))
        }
    }
    for (j in seq_along(cn)) {
        left <- if (!lscalar) 
            e1[[j]]
        else e1
        right <- if (!rscalar) 
            e2[[j]]
        else e2
        value[[j]] <- eval(f)
    }
    if (.Generic %in% c("+", "-", "*", "^", "%%", "%/%", "/")) {
        if (length(value)) {
            names(value) <- cn
            data.frame(value, row.names = rn, check.names = FALSE)
        }
        else data.frame(row.names = rn, check.names = FALSE)
    }
    else {
        value <- unlist(value, recursive = FALSE, use.names = FALSE)
        matrix(if (is.null(value)) 
            logical()
        else value, nrow = nr, dimnames = list(rn, cn))
    }
}


format.difftime <- function (x, ...) 
paste(format(unclass(x), ...), units(x))


trigamma <- function (x)  .Primitive("trigamma")


`&.hexmode` <- function (a, b) 
as.hexmode(bitwAnd(as.hexmode(a), as.hexmode(b)))


character <- function (length = 0L) 
.Internal(vector("character", length))


.col <- function (dim) 
.Internal(col(dim))


.POSIXct <- function (xx, tz = NULL, cl = c("POSIXct", "POSIXt")) 
{
    class(xx) <- cl
    attr(xx, "tzone") <- tz
    xx
}


returnValue <- function (default = NULL) 
.Internal(returnValue(default))


.POSIXlt <- function (xx, tz = NULL, cl = c("POSIXlt", "POSIXt")) 
{
    class(xx) <- cl
    attr(xx, "tzone") <- tz
    xx
}


Math.difftime <- function (x, ...) 
{
    switch(.Generic, abs = , sign = , floor = , ceiling = , trunc = , 
        round = , signif = {
            units <- attr(x, "units")
            .difftime(NextMethod(), units)
        }, stop(gettextf("'%s' not defined for \"difftime\" objects", 
            .Generic), domain = NA))
}


version <- structure(list(platform = "x86_64-w64-mingw32", arch = "x86_64", 
    os = "mingw32", system = "x86_64, mingw32", status = "", 
    major = "3", minor = "6.0", year = "2019", month = "04", 
    day = "26", `svn rev` = "76424", language = "R", version.string = "R version 3.6.0 (2019-04-26)", 
    nickname = "Planting of a Tree"), class = "simple.list")


.F_dchdc <- NULL


jitter <- function (x, factor = 1, amount = NULL) 
{
    if (length(x) == 0L) 
        return(x)
    if (!is.numeric(x)) 
        stop("'x' must be numeric")
    z <- diff(r <- range(x[is.finite(x)]))
    if (z == 0) 
        z <- abs(r[1L])
    if (z == 0) 
        z <- 1
    if (is.null(amount)) {
        d <- diff(xx <- unique(sort.int(round(x, 3 - floor(log10(z))))))
        d <- if (length(d)) 
            min(d)
        else if (xx != 0) 
            xx/10
        else z/10
        amount <- factor/5 * abs(d)
    }
    else if (amount == 0) 
        amount <- factor * (z/50)
    x + stats::runif(length(x), -amount, amount)
}


isNamespaceLoaded <- function (name) 
.Internal(isRegisteredNamespace(name))


print.warnings <- function (x, tags, header = ngettext(n, "Warning message:\n", 
    "Warning messages:\n"), ...) 
{
    if (n <- length(x)) {
        if (length(header)) 
            cat(header)
        if (missing(tags) || length(tags) == 0) 
            tags <- if (n == 1L) 
                ""
            else paste0(seq_len(n), ": ")
        else if (length(tags <- as.character(tags)) != n) 
            stop("'tags' must be a character vector of the same length as 'x'")
        msgs <- names(x)
        for (i in seq_len(n)) {
            out <- if (length(x[[i]])) {
                temp <- deparse(x[[i]], width.cutoff = 50L, nlines = 2L)
                sm <- strsplit(msgs[i], "\n")[[1L]]
                nl <- if (nchar(tags[i], "w") + nchar(temp[1L], 
                  "w") + nchar(sm[1L], "w") <= 75L) 
                  " "
                else "\n  "
                paste0(tags[i], "In ", temp[1L], if (length(temp) > 
                  1L) 
                  " ...", " :", nl, msgs[i])
            }
            else paste0(tags[i], msgs[i])
            do.call("cat", c(list(out), attr(x, "dots"), fill = TRUE))
        }
    }
    invisible(x)
}


simpleWarning <- function (message, call = NULL) 
{
    class <- c("simpleWarning", "warning", "condition")
    structure(list(message = as.character(message), call = call), 
        class = class)
}


double <- function (length = 0L) 
.Internal(vector("double", length))


`[.numeric_version` <- function (x, i, j) 
{
    y <- if (missing(j)) 
        unclass(x)[i]
    else lapply(unclass(x)[i], "[", j)
    bad <- vapply(y, function(t) is.null(t) || anyNA(t), NA)
    if (any(bad)) 
        y[bad] <- rep.int(list(integer()), length(bad))
    class(y) <- class(x)
    y
}


as.expression <- function (x, ...) 
UseMethod("as.expression")


summary.srcfile <- function (object, ...) 
{
    cat(utils:::.normalizePath(object$filename, object$wd), "\n")
    if (inherits(object$timestamp, "POSIXt")) 
        cat("Timestamp: ", format(object$timestamp, usetz = TRUE), 
            "\n", sep = "")
    cat("Encoding: \"", object$encoding, "\"", sep = "")
    if (!is.null(object$Enc) && object$Enc != object$encoding && 
        object$Enc != "unknown") 
        cat(", re-encoded to \"", object$Enc, "\"", sep = "")
    cat("\n")
    invisible(object)
}


summary.data.frame <- function (object, maxsum = 7L, digits = max(3L, getOption("digits") - 
    3L), ...) 
{
    ncw <- function(x) {
        z <- nchar(x, type = "w")
        if (any(na <- is.na(z))) {
            z[na] <- nchar(encodeString(z[na]), "b")
        }
        z
    }
    z <- lapply(X = as.list(object), FUN = summary, maxsum = maxsum, 
        digits = 12L, ...)
    nv <- length(object)
    nm <- names(object)
    lw <- numeric(nv)
    nr <- if (nv) 
        max(vapply(z, function(x) NROW(x) + !is.null(attr(x, 
            "NAs")), integer(1)))
    else 0
    for (i in seq_len(nv)) {
        sms <- z[[i]]
        if (is.matrix(sms)) {
            cn <- paste(nm[i], gsub("^ +", "", colnames(sms), 
                useBytes = TRUE), sep = ".")
            tmp <- format(sms)
            if (nrow(sms) < nr) 
                tmp <- rbind(tmp, matrix("", nr - nrow(sms), 
                  ncol(sms)))
            sms <- apply(tmp, 1L, function(x) paste(x, collapse = "  "))
            wid <- sapply(tmp[1L, ], nchar, type = "w")
            blanks <- paste(character(max(wid)), collapse = " ")
            wcn <- ncw(cn)
            pad0 <- floor((wid - wcn)/2)
            pad1 <- wid - wcn - pad0
            cn <- paste0(substring(blanks, 1L, pad0), cn, substring(blanks, 
                1L, pad1))
            nm[i] <- paste(cn, collapse = "  ")
        }
        else {
            sms <- format(sms, digits = digits)
            lbs <- format(names(sms))
            sms <- paste0(lbs, ":", sms, "  ")
            lw[i] <- ncw(lbs[1L])
            length(sms) <- nr
        }
        z[[i]] <- sms
    }
    if (nv) {
        z <- unlist(z, use.names = TRUE)
        dim(z) <- c(nr, nv)
        if (anyNA(lw)) 
            warning("probably wrong encoding in names(.) of column ", 
                paste(which(is.na(lw)), collapse = ", "))
        blanks <- paste(character(max(lw, na.rm = TRUE) + 2L), 
            collapse = " ")
        pad <- floor(lw - ncw(nm)/2)
        nm <- paste0(substring(blanks, 1, pad), nm)
        dimnames(z) <- list(rep.int("", nr), nm)
    }
    else {
        z <- character()
        dim(z) <- c(nr, nv)
    }
    attr(z, "class") <- c("table")
    z
}


debugonce <- function (fun, text = "", condition = NULL, signature = NULL) 
{
    if (is.null(signature)) 
        .Internal(debugonce(fun, text, condition))
    else if (requireNamespace("methods")) 
        methods::.debugMethod(fun, text, condition, signature, 
            once = TRUE)
    else stop("failed to load the methods package for debugging by signature")
}


print <- function (x, ...) 
UseMethod("print")


.isMethodsDispatchOn <- function (onOff = NULL)  .Primitive(".isMethodsDispatchOn")


`[.listof` <- function (x, i, ...) 
structure(NextMethod("["), class = class(x))


forceAndCall <- function (n, FUN, ...)  .Primitive("forceAndCall")


close.srcfilealias <- function (con, ...) 
close(con$original, ...)


.gtn <- function (x, strictly) 
{
    n <- length(x)
    if (strictly) 
        !all(x[-1L] > x[-n])
    else !all(x[-1L] >= x[-n])
}


.mergeExportMethods <- function (new, ns) 
{
    newMethods <- new[startsWith(new, ".__M__")]
    nsimports <- parent.env(ns)
    for (what in newMethods) {
        if (!is.null(m1 <- nsimports[[what]])) {
            m2 <- get(what, envir = ns)
            ns[[what]] <- methods::mergeMethods(m1, m2)
        }
    }
}


.primTrace <- function (obj)  .Primitive(".primTrace")


seq.int <- function (from, to, by, length.out, along.with, ...)  .Primitive("seq.int")


`storage.mode<-` <- function (x, value)  .Primitive("storage.mode<-")


gregexpr <- function (pattern, text, ignore.case = FALSE, perl = FALSE, fixed = FALSE, 
    useBytes = FALSE) 
{
    if (!is.character(text)) 
        text <- as.character(text)
    .Internal(gregexpr(as.character(pattern), text, ignore.case, 
        perl, fixed, useBytes))
}


namespaceImportClasses <- function (self, ns, vars, from = NULL) 
{
    for (i in seq_along(vars)) vars[[i]] <- methods::classMetaName(vars[[i]])
    namespaceImportFrom(self, asNamespace(ns), vars, from = from)
}


contributors <- function () 
{
    outFile <- tempfile()
    outConn <- file(outFile, open = "w")
    writeLines(paste0("R is a project which is attempting to provide a ", 
        "modern piece of\nstatistical software for the ", "GNU suite of software.\n\n", 
        "The current R is the result of a collaborative ", "effort with\ncontributions from all over the ", 
        "world.\n\n"), outConn)
    writeLines(readLines(file.path(R.home("doc"), "AUTHORS")), 
        outConn)
    writeLines("", outConn)
    writeLines(readLines(file.path(R.home("doc"), "THANKS")), 
        outConn)
    close(outConn)
    file.show(outFile, delete.file = TRUE)
}


unix.time <- function (...) 
{
    .Deprecated("system.time")
    system.time(...)
}


ngettext <- function (n, msg1, msg2, domain = NULL) 
.Internal(ngettext(n, msg1, msg2, domain))


print.Dlist <- function (x, ...) 
{
    if (!is.list(x) && !is.matrix(x) && is.null(names(x))) 
        return(NextMethod())
    cat(formatDL(x, ...), sep = "\n")
    invisible(x)
}


Find <- function (f, x, right = FALSE, nomatch = NULL) 
{
    f <- match.fun(f)
    if ((pos <- Position(f, x, right, nomatch = 0L)) > 0L) 
        x[[pos]]
    else nomatch
}


all.equal.envRefClass <- function (target, current, ...) 
{
    if (!methods::is(target, "envRefClass")) 
        return("'target' is not an envRefClass")
    if (!methods::is(current, "envRefClass")) 
        return("'current' is not an envRefClass")
    if (!isTRUE(ae <- all.equal(class(target), class(current), 
        ...))) 
        return(sprintf("Classes differ: %s", paste(ae, collapse = " ")))
    getCl <- function(x) {
        cl <- tryCatch(x$getClass(), error = function(e) NULL)
        if (is.null(cl)) 
            class(x)
        else cl
    }
    if (!identical(cld <- getCl(target), c2 <- getCl(current))) {
        hasCA <- any("check.attributes" == names(list(...)))
        ae <- if (hasCA) 
            all.equal(cld, c2, ...)
        else all.equal(cld, c2, check.attributes = FALSE, ...)
        if (isTRUE(ae) && !hasCA) 
            ae <- all.equal(cld, c2, ...)
        return(sprintf("Class definitions are not identical%s", 
            if (isTRUE(ae)) "" else paste(":", ae, collapse = " ")))
    }
    if (!isS4(cld)) 
        return(if (identical(target, current)) TRUE else "different prototypical 'envRefClass' objects")
    flds <- names(cld@fieldClasses)
    asL <- function(O) sapply(flds, function(ch) O[[ch]], simplify = FALSE)
    n <- all.equal.list(asL(target), asL(current), ...)
    sns <- names(cld@slots)
    sns <- sns[sns != ".xData"]
    msg <- if (length(sns)) {
        L <- lapply(sns, function(sn) all.equal(methods::slot(target, 
            sn), methods::slot(current, sn), ...))
        unlist(L[vapply(L, is.character, NA)])
    }
    if (is.character(n)) 
        msg <- c(msg, n)
    if (is.null(msg)) 
        TRUE
    else msg
}


as.name <- function (x) 
.Internal(as.vector(x, "symbol"))


gettext <- function (..., domain = NULL) 
{
    args <- lapply(list(...), as.character)
    .Internal(gettext(domain, unlist(args)))
}


`[.factor` <- function (x, ..., drop = FALSE) 
{
    y <- NextMethod("[")
    attr(y, "contrasts") <- attr(x, "contrasts")
    attr(y, "levels") <- attr(x, "levels")
    class(y) <- oldClass(x)
    if (drop) 
        factor(y, exclude = if (anyNA(levels(x))) 
            NULL
        else NA)
    else y
}


`body<-` <- methods::`body<-` # re-exported from methods package

rawToChar <- function (x, multiple = FALSE) 
.Internal(rawToChar(x, multiple))


anyDuplicated.array <- function (x, incomparables = FALSE, MARGIN = 1L, fromLast = FALSE, 
    ...) 
{
    if (!isFALSE(incomparables)) 
        .NotYetUsed("incomparables != FALSE")
    dx <- dim(x)
    ndim <- length(dx)
    if (any(MARGIN > ndim)) 
        stop(gettextf("MARGIN = %s is invalid for dim = %s", 
            paste(MARGIN, collapse = ","), paste(dx, collapse = ",")), 
            domain = NA)
    temp <- if ((ndim > 1L) && (prod(dx[-MARGIN]) > 1L)) 
        asplit(x, MARGIN)
    else x
    anyDuplicated.default(temp, fromLast = fromLast)
}


unique.POSIXlt <- function (x, incomparables = FALSE, ...) 
x[!duplicated(x, incomparables, ...)]


as.list.POSIXct <- function (x, ...) 
{
    nms <- names(x)
    names(x) <- NULL
    y <- lapply(unclass(x), .POSIXct, attr(x, "tzone"), oldClass(x))
    names(y) <- nms
    y
}


as.data.frame.character <- function (x, ..., stringsAsFactors = default.stringsAsFactors()) 
{
    nm <- paste(deparse(substitute(x), width.cutoff = 500L), 
        collapse = " ")
    if (stringsAsFactors) 
        x <- factor(x)
    if (!"nm" %in% names(list(...))) 
        as.data.frame.vector(x, ..., nm = nm)
    else as.data.frame.vector(x, ...)
}


importIntoEnv <- function (impenv, impnames, expenv, expnames) 
{
    exports <- getNamespaceInfo(expenv, "exports")
    ex <- names(exports)
    if (!all(eie <- expnames %in% ex)) {
        miss <- expnames[!eie]
        if (all(startsWith(miss, ".__C__"))) {
            miss <- sub("^\\.__C__", "", miss)
            stop(sprintf(ngettext(length(miss), "class %s is not exported by 'namespace:%s'", 
                "classes %s are not exported by 'namespace:%s'"), 
                paste(paste0("\"", miss, "\""), collapse = ", "), 
                getNamespaceName(expenv)), call. = FALSE, domain = NA)
        }
        else {
            stop(sprintf(ngettext(length(miss), "object %s is not exported by 'namespace:%s'", 
                "objects %s are not exported by 'namespace:%s'"), 
                paste(sQuote(miss), collapse = ", "), getNamespaceName(expenv)), 
                call. = FALSE, domain = NA)
        }
    }
    expnames <- unlist(mget(expnames, envir = exports, inherits = FALSE), 
        recursive = FALSE)
    if (is.null(impnames)) 
        impnames <- character()
    if (is.null(expnames)) 
        expnames <- character()
    .Internal(importIntoEnv(impenv, impnames, expenv, expnames))
}


as.list.POSIXlt <- function (x, ...) 
{
    nms <- names(x)
    names(x) <- NULL
    y <- lapply(X = do.call(Map, c(list, unclass(x))), FUN = .POSIXlt, 
        attr(x, "tzone"), oldClass(x))
    names(y) <- nms
    y
}


isIncomplete <- function (con) 
.Internal(isIncomplete(con))


qr.qy <- function (qr, y) 
{
    if (!is.qr(qr)) 
        stop("argument is not a QR decomposition")
    if (is.complex(qr$qr)) 
        return(.Internal(qr_qy_cmplx(qr, as.matrix(y), FALSE)))
    if (isTRUE(attr(qr, "useLAPACK"))) 
        return(.Internal(qr_qy_real(qr, as.matrix(y), FALSE)))
    n <- as.integer(nrow(qr$qr))
    if (is.na(n)) 
        stop("invalid nrow(qr$qr)")
    k <- as.integer(qr$rank)
    ny <- as.integer(NCOL(y))
    if (is.na(ny)) 
        stop("invalid NCOL(y)")
    storage.mode(y) <- "double"
    if (NROW(y) != n) 
        stop("'qr' and 'y' must have the same number of rows")
    .Fortran(.F_dqrqy, as.double(qr$qr), n, k, as.double(qr$qraux), 
        y, ny, qy = y)$qy
}


bitwXor <- function (a, b) 
.Internal(bitwiseXor(a, b))


`[<-.data.frame` <- function (x, i, j, value) 
{
    if (!all(names(sys.call()) %in% c("", "value"))) 
        warning("named arguments are discouraged")
    nA <- nargs()
    if (nA == 4L) {
        has.i <- !missing(i)
        has.j <- !missing(j)
    }
    else if (nA == 3L) {
        if (is.atomic(value) && !is.null(names(value))) 
            names(value) <- NULL
        if (missing(i) && missing(j)) {
            i <- j <- NULL
            has.i <- has.j <- FALSE
            if (is.null(value)) 
                return(x[logical()])
        }
        else {
            if (is.numeric(i) && is.matrix(i) && ncol(i) == 2) {
                index <- rep.int(FALSE, prod(dim(x)))
                dim(index) <- dim(x)
                tryCatch(index[i] <- TRUE, error = function(e) stop(conditionMessage(e), 
                  call. = FALSE))
                o <- order(i[, 2], i[, 1])
                N <- length(value)
                if (length(o)%%N != 0L) 
                  warning("number of items to replace is not a multiple of replacement length")
                if (N < length(o)) 
                  value <- rep(value, length.out = length(o))
                value <- value[o]
                i <- index
            }
            if (is.logical(i) && is.matrix(i) && all(dim(i) == 
                dim(x))) {
                nreplace <- sum(i, na.rm = TRUE)
                if (!nreplace) 
                  return(x)
                N <- length(value)
                if (N > 1L && N < nreplace && (nreplace%%N) == 
                  0L) 
                  value <- rep(value, length.out = nreplace)
                if (N > 1L && (length(value) != nreplace)) 
                  stop("'value' is the wrong length")
                n <- 0L
                nv <- nrow(x)
                for (v in seq_len(dim(i)[2L])) {
                  thisvar <- i[, v, drop = TRUE]
                  nv <- sum(thisvar, na.rm = TRUE)
                  if (nv) {
                    if (is.matrix(x[[v]])) 
                      x[[v]][thisvar, ] <- if (N > 1L) 
                        value[n + seq_len(nv)]
                      else value
                    else x[[v]][thisvar] <- if (N > 1L) 
                      value[n + seq_len(nv)]
                    else value
                  }
                  n <- n + nv
                }
                return(x)
            }
            if (is.matrix(i)) 
                stop("unsupported matrix index in replacement")
            j <- i
            i <- NULL
            has.i <- FALSE
            has.j <- TRUE
        }
    }
    else stop("need 0, 1, or 2 subscripts")
    if ((has.j && !length(j)) || (has.i && !length(i) && !has.j)) 
        return(x)
    cl <- oldClass(x)
    class(x) <- NULL
    new.cols <- NULL
    nvars <- length(x)
    nrows <- .row_names_info(x, 2L)
    if (has.i && length(i)) {
        rows <- NULL
        if (anyNA(i)) 
            stop("missing values are not allowed in subscripted assignments of data frames")
        if (char.i <- is.character(i)) {
            rows <- attr(x, "row.names")
            ii <- match(i, rows)
            nextra <- sum(new.rows <- is.na(ii))
            if (nextra > 0L) {
                ii[new.rows] <- seq.int(from = nrows + 1L, length.out = nextra)
                new.rows <- i[new.rows]
            }
            i <- ii
        }
        if (!is.logical(i) && (char.i && nextra || all(i >= 0L) && 
            (nn <- max(i)) > nrows)) {
            if (is.null(rows)) 
                rows <- attr(x, "row.names")
            if (!char.i) {
                nrr <- (nrows + 1L):nn
                if (inherits(value, "data.frame") && (dim(value)[1L]) >= 
                  length(nrr)) {
                  new.rows <- attr(value, "row.names")[seq_along(nrr)]
                  repl <- duplicated(new.rows) | match(new.rows, 
                    rows, 0L)
                  if (any(repl)) 
                    new.rows[repl] <- nrr[repl]
                }
                else new.rows <- nrr
            }
            x <- xpdrows.data.frame(x, rows, new.rows)
            rows <- attr(x, "row.names")
            nrows <- length(rows)
        }
        iseq <- seq_len(nrows)[i]
        if (anyNA(iseq)) 
            stop("non-existent rows not allowed")
    }
    else iseq <- NULL
    if (has.j) {
        if (anyNA(j)) 
            stop("missing values are not allowed in subscripted assignments of data frames")
        if (is.character(j)) {
            if ("" %in% j) 
                stop("column name \"\" cannot match any column")
            jseq <- match(j, names(x))
            if (anyNA(jseq)) {
                n <- is.na(jseq)
                jseq[n] <- nvars + seq_len(sum(n))
                new.cols <- j[n]
            }
        }
        else if (is.logical(j) || min(j) < 0L) 
            jseq <- seq_along(x)[j]
        else {
            jseq <- j
            if (max(jseq) > nvars) {
                new.cols <- paste0("V", seq.int(from = nvars + 
                  1L, to = max(jseq)))
                if (length(new.cols) != sum(jseq > nvars)) 
                  stop("new columns would leave holes after existing columns")
                if (is.list(value) && !is.null(vnm <- names(value))) {
                  p <- length(jseq)
                  if (length(vnm) < p) 
                    vnm <- rep_len(vnm, p)
                  new.cols <- vnm[jseq > nvars]
                }
            }
        }
    }
    else jseq <- seq_along(x)
    if (has.i && !length(iseq) && all(1L <= jseq & jseq <= nvars)) 
        return(`class<-`(x, cl))
    if (anyDuplicated(jseq)) 
        stop("duplicate subscripts for columns")
    n <- length(iseq)
    if (n == 0L) 
        n <- nrows
    p <- length(jseq)
    if (is.null(value)) {
        value <- list(NULL)
    }
    m <- length(value)
    if (!is.list(value)) {
        if (p == 1L) {
            N <- NROW(value)
            if (N > n) 
                stop(sprintf(ngettext(N, "replacement has %d row, data has %d", 
                  "replacement has %d rows, data has %d"), N, 
                  n), domain = NA)
            if (N < n && N > 0L) 
                if (n%%N == 0L && length(dim(value)) <= 1L) 
                  value <- rep(value, length.out = n)
                else stop(sprintf(ngettext(N, "replacement has %d row, data has %d", 
                  "replacement has %d rows, data has %d"), N, 
                  nrows), domain = NA)
            if (!is.null(names(value))) 
                names(value) <- NULL
            value <- list(value)
        }
        else {
            if (m < n * p && (m == 0L || (n * p)%%m)) 
                stop(sprintf(ngettext(m, "replacement has %d item, need %d", 
                  "replacement has %d items, need %d"), m, n * 
                  p), domain = NA)
            value <- matrix(value, n, p)
            value <- split(c(value), col(value))
        }
        dimv <- c(n, p)
    }
    else {
        value <- unclass(value)
        lens <- vapply(value, NROW, 1L)
        for (k in seq_along(lens)) {
            N <- lens[k]
            if (n != N && length(dim(value[[k]])) == 2L) 
                stop(sprintf(ngettext(N, "replacement element %d is a matrix/data frame of %d row, need %d", 
                  "replacement element %d is a matrix/data frame of %d rows, need %d"), 
                  k, N, n), domain = NA)
            if (N > 0L && N < n && n%%N) 
                stop(sprintf(ngettext(N, "replacement element %d has %d row, need %d", 
                  "replacement element %d has %d rows, need %d"), 
                  k, N, n), domain = NA)
            if (N > 0L && N < n) 
                value[[k]] <- rep(value[[k]], length.out = n)
            if (N > n) {
                warning(sprintf(ngettext(N, "replacement element %d has %d row to replace %d rows", 
                  "replacement element %d has %d rows to replace %d rows"), 
                  k, N, n), domain = NA)
                value[[k]] <- value[[k]][seq_len(n)]
            }
        }
        dimv <- c(n, length(value))
    }
    nrowv <- dimv[1L]
    if (nrowv < n && nrowv > 0L) {
        if (n%%nrowv == 0L) 
            value <- value[rep_len(seq_len(nrowv), n), , drop = FALSE]
        else stop(sprintf(ngettext(nrowv, "%d row in value to replace %d rows", 
            "%d rows in value to replace %d rows"), nrowv, n), 
            domain = NA)
    }
    else if (nrowv > n) 
        warning(sprintf(ngettext(nrowv, "replacement data has %d row to replace %d rows", 
            "replacement data has %d rows to replace %d rows"), 
            nrowv, n), domain = NA)
    ncolv <- dimv[2L]
    jvseq <- seq_len(p)
    if (ncolv < p) 
        jvseq <- rep_len(seq_len(ncolv), p)
    else if (p != 0L && ncolv > p) {
        warning(sprintf(ngettext(ncolv, "provided %d variable to replace %d variables", 
            "provided %d variables to replace %d variables"), 
            ncolv, p), domain = NA)
        new.cols <- new.cols[seq_len(p)]
    }
    if (length(new.cols)) {
        nm <- names(x)
        rows <- .row_names_info(x, 0L)
        a <- attributes(x)
        a["names"] <- NULL
        x <- c(x, vector("list", length(new.cols)))
        attributes(x) <- a
        names(x) <- c(nm, new.cols)
        attr(x, "row.names") <- rows
    }
    if (has.i) 
        for (jjj in seq_len(p)) {
            jj <- jseq[jjj]
            vjj <- value[[jvseq[[jjj]]]]
            if (jj <= nvars) {
                if (length(dim(x[[jj]])) != 2L) 
                  x[[jj]][iseq] <- vjj
                else x[[jj]][iseq, ] <- vjj
            }
            else {
                x[[jj]] <- vjj[FALSE]
                if (length(dim(vjj)) == 2L) {
                  length(x[[jj]]) <- nrows * ncol(vjj)
                  dim(x[[jj]]) <- c(nrows, ncol(vjj))
                  x[[jj]][iseq, ] <- vjj
                }
                else {
                  length(x[[jj]]) <- nrows
                  x[[jj]][iseq] <- vjj
                }
            }
        }
    else if (p > 0L) 
        for (jjj in p:1L) {
            o <- order(jseq)
            jseq <- jseq[o]
            jvseq <- jvseq[o]
            jj <- jseq[jjj]
            v <- value[[jvseq[[jjj]]]]
            if (!is.null(v) && nrows > 0L && !length(v)) 
                length(v) <- nrows
            x[[jj]] <- v
            if (!is.null(v) && is.atomic(x[[jj]]) && !is.null(names(x[[jj]]))) 
                names(x[[jj]]) <- NULL
        }
    if (length(new.cols) > 0L) {
        new.cols <- names(x)
        if (anyDuplicated(new.cols)) 
            names(x) <- make.unique(new.cols)
    }
    class(x) <- cl
    x
}


format.factor <- function (x, ...) 
format(structure(as.character(x), names = names(x), dim = dim(x), 
    dimnames = dimnames(x)), ...)


Map <- function (f, ...) 
{
    f <- match.fun(f)
    mapply(FUN = f, ..., SIMPLIFY = FALSE)
}


getTaskCallbackNames <- function () 
.Call(.C_R_getTaskCallbackNames)


data.frame <- function (..., row.names = NULL, check.rows = FALSE, check.names = TRUE, 
    fix.empty.names = TRUE, stringsAsFactors = default.stringsAsFactors()) 
{
    data.row.names <- if (check.rows && is.null(row.names)) 
        function(current, new, i) {
            if (is.character(current)) 
                new <- as.character(new)
            if (is.character(new)) 
                current <- as.character(current)
            if (anyDuplicated(new)) 
                return(current)
            if (is.null(current)) 
                return(new)
            if (all(current == new) || all(current == "")) 
                return(new)
            stop(gettextf("mismatch of row names in arguments of 'data.frame', item %d", 
                i), domain = NA)
        }
    else function(current, new, i) {
        if (is.null(current)) {
            if (anyDuplicated(new)) {
                warning(gettextf("some row.names duplicated: %s --> row.names NOT used", 
                  paste(which(duplicated(new)), collapse = ",")), 
                  domain = NA)
                current
            }
            else new
        }
        else current
    }
    object <- as.list(substitute(list(...)))[-1L]
    mirn <- missing(row.names)
    mrn <- is.null(row.names)
    x <- list(...)
    n <- length(x)
    if (n < 1L) {
        if (!mrn) {
            if (is.object(row.names) || !is.integer(row.names)) 
                row.names <- as.character(row.names)
            if (anyNA(row.names)) 
                stop("row names contain missing values")
            if (anyDuplicated(row.names)) 
                stop(gettextf("duplicate row.names: %s", paste(unique(row.names[duplicated(row.names)]), 
                  collapse = ", ")), domain = NA)
        }
        else row.names <- integer()
        return(structure(list(), names = character(), row.names = row.names, 
            class = "data.frame"))
    }
    vnames <- names(x)
    if (length(vnames) != n) 
        vnames <- character(n)
    no.vn <- !nzchar(vnames)
    vlist <- vnames <- as.list(vnames)
    nrows <- ncols <- integer(n)
    for (i in seq_len(n)) {
        xi <- if (is.character(x[[i]]) || is.list(x[[i]])) 
            as.data.frame(x[[i]], optional = TRUE, stringsAsFactors = stringsAsFactors)
        else as.data.frame(x[[i]], optional = TRUE)
        nrows[i] <- .row_names_info(xi)
        ncols[i] <- length(xi)
        namesi <- names(xi)
        if (ncols[i] > 1L) {
            if (length(namesi) == 0L) 
                namesi <- seq_len(ncols[i])
            vnames[[i]] <- if (no.vn[i]) 
                namesi
            else paste(vnames[[i]], namesi, sep = ".")
        }
        else if (length(namesi)) {
            vnames[[i]] <- namesi
        }
        else if (fix.empty.names && no.vn[[i]]) {
            tmpname <- deparse(object[[i]], nlines = 1L)[1L]
            if (substr(tmpname, 1L, 2L) == "I(") {
                ntmpn <- nchar(tmpname, "c")
                if (substr(tmpname, ntmpn, ntmpn) == ")") 
                  tmpname <- substr(tmpname, 3L, ntmpn - 1L)
            }
            vnames[[i]] <- tmpname
        }
        if (mirn && nrows[i] > 0L) {
            rowsi <- attr(xi, "row.names")
            if (any(nzchar(rowsi))) 
                row.names <- data.row.names(row.names, rowsi, 
                  i)
        }
        nrows[i] <- abs(nrows[i])
        vlist[[i]] <- xi
    }
    nr <- max(nrows)
    for (i in seq_len(n)[nrows < nr]) {
        xi <- vlist[[i]]
        if (nrows[i] > 0L && (nr%%nrows[i] == 0L)) {
            xi <- unclass(xi)
            fixed <- TRUE
            for (j in seq_along(xi)) {
                xi1 <- xi[[j]]
                if (is.vector(xi1) || is.factor(xi1)) 
                  xi[[j]] <- rep(xi1, length.out = nr)
                else if (is.character(xi1) && inherits(xi1, "AsIs")) 
                  xi[[j]] <- structure(rep(xi1, length.out = nr), 
                    class = class(xi1))
                else if (inherits(xi1, "Date") || inherits(xi1, 
                  "POSIXct")) 
                  xi[[j]] <- rep(xi1, length.out = nr)
                else {
                  fixed <- FALSE
                  break
                }
            }
            if (fixed) {
                vlist[[i]] <- xi
                next
            }
        }
        stop(gettextf("arguments imply differing number of rows: %s", 
            paste(unique(nrows), collapse = ", ")), domain = NA)
    }
    value <- unlist(vlist, recursive = FALSE, use.names = FALSE)
    vnames <- unlist(vnames[ncols > 0L])
    if (fix.empty.names && any(noname <- !nzchar(vnames))) 
        vnames[noname] <- paste0("Var.", seq_along(vnames))[noname]
    if (check.names) {
        if (fix.empty.names) 
            vnames <- make.names(vnames, unique = TRUE)
        else {
            nz <- nzchar(vnames)
            vnames[nz] <- make.names(vnames[nz], unique = TRUE)
        }
    }
    names(value) <- vnames
    if (!mrn) {
        if (length(row.names) == 1L && nr != 1L) {
            if (is.character(row.names)) 
                row.names <- match(row.names, vnames, 0L)
            if (length(row.names) != 1L || row.names < 1L || 
                row.names > length(vnames)) 
                stop("'row.names' should specify one of the variables")
            i <- row.names
            row.names <- value[[i]]
            value <- value[-i]
        }
        else if (!is.null(row.names) && length(row.names) != 
            nr) 
            stop("row names supplied are of the wrong length")
    }
    else if (!is.null(row.names) && length(row.names) != nr) {
        warning("row names were found from a short variable and have been discarded")
        row.names <- NULL
    }
    class(value) <- "data.frame"
    if (is.null(row.names)) 
        attr(value, "row.names") <- .set_row_names(nr)
    else {
        if (is.object(row.names) || !is.integer(row.names)) 
            row.names <- as.character(row.names)
        if (anyNA(row.names)) 
            stop("row names contain missing values")
        if (anyDuplicated(row.names)) 
            stop(gettextf("duplicate row.names: %s", paste(unique(row.names[duplicated(row.names)]), 
                collapse = ", ")), domain = NA)
        row.names(value) <- row.names
    }
    value
}


Mod <- function (z)  .Primitive("Mod")


sQuote <- function (x, q = getOption("useFancyQuotes")) 
{
    if (!length(x)) 
        return(character())
    before <- after <- "'"
    if (!is.null(q)) {
        if (isTRUE(q)) {
            li <- l10n_info()
            if (li$"UTF-8") 
                q <- "UTF-8"
            if (!is.null(li$codepage) && li$codepage > 0L) {
                if (li$codepage >= 1250L && li$codepage <= 1258L || 
                  li$codepage == 874L) {
                  before <- rawToChar(as.raw(145))
                  after <- rawToChar(as.raw(146))
                }
                else {
                  z <- iconv(c(intToUtf8(8216), intToUtf8(8217)), 
                    "UTF-8", "")
                  before <- z[1L]
                  after <- z[2L]
                }
            }
        }
        if (identical(q, "TeX")) {
            before <- "`"
            after <- "'"
        }
        if (identical(q, "UTF-8")) {
            before <- intToUtf8(8216)
            after <- intToUtf8(8217)
        }
        if (is.character(q) && length(q) >= 4L) {
            before <- q[1L]
            after <- q[2L]
        }
    }
    paste0(before, x, after)
}


regexec <- function (pattern, text, ignore.case = FALSE, perl = FALSE, fixed = FALSE, 
    useBytes = FALSE) 
{
    if (!is.character(text)) 
        text <- as.character(text)
    if (!perl || fixed) 
        return(.Internal(regexec(as.character(pattern), text, 
            ignore.case, fixed, useBytes)))
    match_data_from_pos_and_len <- function(pos, len) {
        attr(pos, "match.length") <- len
        pos
    }
    m <- regexpr(pattern, text, ignore.case = ignore.case, useBytes = useBytes, 
        perl = TRUE)
    y <- vector("list", length(text))
    y[is.na(m)] <- list(match_data_from_pos_and_len(NA_integer_, 
        NA_integer_))
    ind <- !is.na(m) & (m == -1L)
    if (any(ind)) {
        y[ind] <- list(match_data_from_pos_and_len(-1L, -1L))
    }
    ind <- !is.na(m) & !ind
    if (any(ind)) {
        pos <- cbind(m[ind], attr(m, "capture.start")[ind, , 
            drop = FALSE])
        len <- cbind(attr(m, "match.length")[ind], attr(m, "capture.length")[ind, 
            , drop = FALSE])
        y[ind] <- Map(match_data_from_pos_and_len, split(pos, 
            row(pos)), split(len, row(len)))
    }
    if (identical(attr(m, "useBytes"), TRUE)) 
        y <- lapply(y, `attr<-`, "useBytes", TRUE)
    y
}


`-.Date` <- function (e1, e2) 
{
    coerceTimeUnit <- function(x) as.vector(round(switch(attr(x, 
        "units"), secs = x/86400, mins = x/1440, hours = x/24, 
        days = x, weeks = 7 * x)))
    if (!inherits(e1, "Date")) 
        stop("can only subtract from \"Date\" objects")
    if (nargs() == 1L) 
        stop("unary - is not defined for \"Date\" objects")
    if (inherits(e2, "Date")) 
        return(difftime(e1, e2, units = "days"))
    if (inherits(e2, "difftime")) 
        e2 <- coerceTimeUnit(e2)
    if (!is.null(attr(e2, "class"))) 
        stop("can only subtract numbers from \"Date\" objects")
    .Date(unclass(as.Date(e1)) - e2)
}


regexpr <- function (pattern, text, ignore.case = FALSE, perl = FALSE, fixed = FALSE, 
    useBytes = FALSE) 
{
    if (!is.character(text)) 
        text <- as.character(text)
    .Internal(regexpr(as.character(pattern), text, ignore.case, 
        perl, fixed, useBytes))
}


packageNotFoundError <- function (package, lib.loc, call = NULL) 
{
    if (length(package) == 1L) 
        msg <- gettextf("there is no package called %s", sQuote(package))
    else msg <- paste0(ngettext(length(package), "there is no package called", 
        "there are no packages called"), " ", paste(sQuote(package), 
        collapse = ", "))
    errorCondition(msg, package = package, lib.loc = lib.loc, 
        call = call, class = "packageNotFoundError")
}


`[.noquote` <- function (x, ...) 
{
    attr <- attributes(x)
    r <- unclass(x)[...]
    attributes(r) <- c(attributes(r), attr[is.na(match(names(attr), 
        c("dim", "dimnames", "names")))])
    r
}


sys.nframe <- function () 
.Internal(sys.nframe())


as.table <- function (x, ...) 
UseMethod("as.table")


sys.load.image <- function (name, quiet) 
{
    if (file.exists(name)) {
        load(name, envir = .GlobalEnv)
        if (!quiet) 
            message("[Previously saved workspace restored]", 
                "\n")
    }
}


source <- function (file, local = FALSE, echo = verbose, print.eval = echo, 
    exprs, spaced = use_file, verbose = getOption("verbose"), 
    prompt.echo = getOption("prompt"), max.deparse.length = 150, 
    width.cutoff = 60L, deparseCtrl = "showAttributes", chdir = FALSE, 
    encoding = getOption("encoding"), continue.echo = getOption("continue"), 
    skip.echo = 0, keep.source = getOption("keep.source")) 
{
    envir <- if (isTRUE(local)) 
        parent.frame()
    else if (isFALSE(local)) 
        .GlobalEnv
    else if (is.environment(local)) 
        local
    else stop("'local' must be TRUE, FALSE or an environment")
    if (!missing(echo)) {
        if (!is.logical(echo)) 
            stop("'echo' must be logical")
        if (!echo && verbose) {
            warning("'verbose' is TRUE, 'echo' not; ... coercing 'echo <- TRUE'")
            echo <- TRUE
        }
    }
    if (verbose) {
        cat("'envir' chosen:")
        print(envir)
    }
    if (use_file <- missing(exprs)) {
        ofile <- file
        from_file <- FALSE
        srcfile <- NULL
        if (is.character(file)) {
            have_encoding <- !missing(encoding) && encoding != 
                "unknown"
            if (identical(encoding, "unknown")) {
                enc <- utils::localeToCharset()
                encoding <- enc[length(enc)]
            }
            else enc <- encoding
            if (length(enc) > 1L) {
                encoding <- NA
                owarn <- options(warn = 2)
                for (e in enc) {
                  if (is.na(e)) 
                    next
                  zz <- file(file, encoding = e)
                  res <- tryCatch(readLines(zz, warn = FALSE), 
                    error = identity)
                  close(zz)
                  if (!inherits(res, "error")) {
                    encoding <- e
                    break
                  }
                }
                options(owarn)
            }
            if (is.na(encoding)) 
                stop("unable to find a plausible encoding")
            if (verbose) 
                cat(gettextf("encoding = \"%s\" chosen", encoding), 
                  "\n", sep = "")
            if (file == "") {
                file <- stdin()
                srcfile <- "<stdin>"
            }
            else {
                filename <- file
                file <- file(filename, "r", encoding = encoding)
                on.exit(close(file))
                if (isTRUE(keep.source)) {
                  lines <- readLines(file, warn = FALSE)
                  on.exit()
                  close(file)
                  srcfile <- srcfilecopy(filename, lines, file.mtime(filename)[1], 
                    isFile = TRUE)
                }
                else {
                  from_file <- TRUE
                  srcfile <- filename
                }
                loc <- utils::localeToCharset()[1L]
                encoding <- if (have_encoding) 
                  switch(loc, `UTF-8` = "UTF-8", `ISO8859-1` = "latin1", 
                    "unknown")
                else "unknown"
            }
        }
        else {
            lines <- readLines(file, warn = FALSE)
            srcfile <- if (isTRUE(keep.source)) 
                srcfilecopy(deparse(substitute(file)), lines)
            else deparse(substitute(file))
        }
        exprs <- if (!from_file) {
            if (length(lines)) 
                .Internal(parse(stdin(), n = -1, lines, "?", 
                  srcfile, encoding))
            else expression()
        }
        else .Internal(parse(file, n = -1, NULL, "?", srcfile, 
            encoding))
        on.exit()
        if (from_file) 
            close(file)
        if (verbose) 
            cat("--> parsed", length(exprs), "expressions; now eval(.)ing them:\n")
        if (chdir) {
            if (is.character(ofile)) {
                if (grepl("^(ftp|http|file)://", ofile)) 
                  warning("'chdir = TRUE' makes no sense for a URL")
                else if ((path <- dirname(ofile)) != ".") {
                  owd <- getwd()
                  if (is.null(owd)) 
                    stop("cannot 'chdir' as current directory is unknown")
                  on.exit(setwd(owd), add = TRUE)
                  setwd(path)
                }
            }
            else {
                warning("'chdir = TRUE' makes no sense for a connection")
            }
        }
    }
    else {
        if (!missing(file)) 
            stop("specify either 'file' or 'exprs' but not both")
        if (!is.expression(exprs)) 
            exprs <- as.expression(exprs)
    }
    Ne <- length(exprs)
    if (echo) {
        sd <- "\""
        nos <- "[^\"]*"
        oddsd <- paste0("^", nos, sd, "(", nos, sd, nos, sd, 
            ")*", nos, "$")
        trySrcLines <- function(srcfile, showfrom, showto) {
            tryCatch(suppressWarnings(getSrcLines(srcfile, showfrom, 
                showto)), error = function(e) character())
        }
    }
    yy <- NULL
    lastshown <- 0
    srcrefs <- attr(exprs, "srcref")
    if (verbose && !is.null(srcrefs)) {
        cat("has srcrefs:\n")
        utils::str(srcrefs)
    }
    for (i in seq_len(Ne + echo)) {
        tail <- i > Ne
        if (!tail) {
            if (verbose) 
                cat("\n>>>> eval(expression_nr.", i, ")\n\t\t =================\n")
            ei <- exprs[i]
        }
        if (echo) {
            nd <- 0
            srcref <- if (tail) 
                attr(exprs, "wholeSrcref")
            else if (i <= length(srcrefs)) 
                srcrefs[[i]]
            if (!is.null(srcref)) {
                if (i == 1) 
                  lastshown <- min(skip.echo, srcref[3L] - 1)
                if (lastshown < srcref[3L]) {
                  srcfile <- attr(srcref, "srcfile")
                  dep <- trySrcLines(srcfile, lastshown + 1, 
                    srcref[3L])
                  if (length(dep)) {
                    leading <- if (tail) 
                      length(dep)
                    else srcref[1L] - lastshown
                    lastshown <- srcref[3L]
                    while (length(dep) && grepl("^[[:blank:]]*$", 
                      dep[1L])) {
                      dep <- dep[-1L]
                      leading <- leading - 1L
                    }
                    dep <- paste0(rep.int(c(prompt.echo, continue.echo), 
                      c(leading, length(dep) - leading)), dep, 
                      collapse = "\n")
                    nd <- nchar(dep, "c")
                  }
                  else srcref <- NULL
                }
            }
            if (is.null(srcref)) {
                if (!tail) {
                  dep <- substr(paste(deparse(ei, width.cutoff = width.cutoff, 
                    control = deparseCtrl), collapse = "\n"), 
                    12L, 1000000L)
                  dep <- paste0(prompt.echo, gsub("\n", paste0("\n", 
                    continue.echo), dep))
                  nd <- nchar(dep, "c") - 1L
                }
            }
            if (nd) {
                do.trunc <- nd > max.deparse.length
                dep <- substr(dep, 1L, if (do.trunc) 
                  max.deparse.length
                else nd)
                cat(if (spaced) 
                  "\n", dep, if (do.trunc) 
                  paste(if (grepl(sd, dep) && grepl(oddsd, dep)) 
                    " ...\" ..."
                  else " ....", "[TRUNCATED] "), "\n", sep = "")
            }
        }
        if (!tail) {
            yy <- withVisible(eval(ei, envir))
            i.symbol <- mode(ei[[1L]]) == "name"
            if (!i.symbol) {
                curr.fun <- ei[[1L]][[1L]]
                if (verbose) {
                  cat("curr.fun:")
                  utils::str(curr.fun)
                }
            }
            if (verbose >= 2) {
                cat(".... mode(ei[[1L]])=", mode(ei[[1L]]), "; paste(curr.fun)=")
                utils::str(paste(curr.fun))
            }
            if (print.eval && yy$visible) {
                if (isS4(yy$value)) 
                  methods::show(yy$value)
                else print(yy$value)
            }
            if (verbose) 
                cat(" .. after ", sQuote(deparse(ei, control = unique(c(deparseCtrl, 
                  "useSource")))), "\n", sep = "")
        }
    }
    invisible(yy)
}


objects <- function (name, pos = -1L, envir = as.environment(pos), all.names = FALSE, 
    pattern, sorted = TRUE) 
{
    if (!missing(name)) {
        pos <- tryCatch(name, error = function(e) e)
        if (inherits(pos, "error")) {
            name <- substitute(name)
            if (!is.character(name)) 
                name <- deparse(name)
            warning(gettextf("%s converted to character string", 
                sQuote(name)), domain = NA)
            pos <- name
        }
    }
    all.names <- .Internal(ls(envir, all.names, sorted))
    if (!missing(pattern)) {
        if ((ll <- length(grep("[", pattern, fixed = TRUE))) && 
            ll != length(grep("]", pattern, fixed = TRUE))) {
            if (pattern == "[") {
                pattern <- "\\["
                warning("replaced regular expression pattern '[' by  '\\\\['")
            }
            else if (length(grep("[^\\\\]\\[<-", pattern))) {
                pattern <- sub("\\[<-", "\\\\\\[<-", pattern)
                warning("replaced '[<-' by '\\\\[<-' in regular expression pattern")
            }
        }
        grep(pattern, all.names, value = TRUE)
    }
    else all.names
}


is.table <- function (x) 
inherits(x, "table")


sys.status <- function () 
list(sys.calls = sys.calls(), sys.parents = sys.parents(), sys.frames = sys.frames())


array <- function (data = NA, dim = length(data), dimnames = NULL) 
{
    if (is.atomic(data) && !is.object(data)) 
        return(.Internal(array(data, dim, dimnames)))
    data <- as.vector(data)
    if (is.object(data)) {
        dim <- as.integer(dim)
        if (!length(dim)) 
            stop("'dim' cannot be of length 0")
        vl <- prod(dim)
        if (length(data) != vl) {
            if (vl > .Machine$integer.max) 
                stop("'dim' specifies too large an array")
            data <- rep_len(data, vl)
        }
        if (length(dim)) 
            dim(data) <- dim
        if (is.list(dimnames) && length(dimnames)) 
            dimnames(data) <- dimnames
        data
    }
    else .Internal(array(data, dim, dimnames))
}


search <- function () 
.Internal(search())


is.infinite <- function (x)  .Primitive("is.infinite")


.Machine <- list(double.eps = 2.22044604925031e-16, double.neg.eps = 1.11022302462516e-16, 
    double.xmin = 2.2250738585072e-308, double.xmax = 1.79769313486232e+308, 
    double.base = 2L, double.digits = 53L, double.rounding = 5L, 
    double.guard = 0L, double.ulp.digits = -52L, double.neg.ulp.digits = -53L, 
    double.exponent = 11L, double.min.exp = -1022L, double.max.exp = 1024L, 
    integer.max = 2147483647L, sizeof.long = 4L, sizeof.longlong = 8L, 
    sizeof.longdouble = 16L, sizeof.pointer = 8L)


`$<-.data.frame` <- function (x, name, value) 
{
    cl <- oldClass(x)
    class(x) <- NULL
    nrows <- .row_names_info(x, 2L)
    if (!is.null(value)) {
        N <- NROW(value)
        if (N > nrows) 
            stop(sprintf(ngettext(N, "replacement has %d row, data has %d", 
                "replacement has %d rows, data has %d"), N, nrows), 
                domain = NA)
        if (N < nrows) 
            if (N > 0L && (nrows%%N == 0L) && length(dim(value)) <= 
                1L) 
                value <- rep(value, length.out = nrows)
            else stop(sprintf(ngettext(N, "replacement has %d row, data has %d", 
                "replacement has %d rows, data has %d"), N, nrows), 
                domain = NA)
        if (is.atomic(value) && !is.null(names(value))) 
            names(value) <- NULL
    }
    x[[name]] <- value
    class(x) <- cl
    return(x)
}


weekdays.Date <- function (x, abbreviate = FALSE) 
format(x, ifelse(abbreviate, "%a", "%A"))


rownames <- function (x, do.NULL = TRUE, prefix = "row") 
{
    dn <- dimnames(x)
    if (!is.null(dn[[1L]])) 
        dn[[1L]]
    else {
        nr <- NROW(x)
        if (do.NULL) 
            NULL
        else if (nr > 0L) 
            paste0(prefix, seq_len(nr))
        else character()
    }
}


readRenviron <- function (path) 
.Internal(readRenviron(path))


textConnection <- function (object, open = "r", local = FALSE, encoding = c("", 
    "bytes", "UTF-8")) 
{
    env <- if (local) 
        parent.frame()
    else .GlobalEnv
    type <- match(match.arg(encoding), c("", "bytes", "UTF-8"))
    nm <- deparse(substitute(object))
    if (length(nm) != 1) 
        stop("argument 'object' must deparse to a single character string")
    .Internal(textConnection(nm, object, open, env, type))
}


.rowMeans <- function (x, m, n, na.rm = FALSE) 
.Internal(rowMeans(x, m, n, na.rm))


getSrcLines <- function (srcfile, first, last) 
{
    if (first > last) 
        return(character())
    if (inherits(srcfile, "srcfilealias")) 
        srcfile <- srcfile$original
    if (inherits(srcfile, "srcfilecopy")) {
        if (is.null(srcfile$fixedNewlines)) {
            lines <- srcfile$lines
            if (any(grepl("\n", lines, fixed = TRUE, useBytes = TRUE))) 
                srcfile$lines <- unlist(strsplit(sub("$", "\n", 
                  as.character(lines)), "\n"))
            srcfile$fixedNewlines <- TRUE
        }
        last <- min(last, length(srcfile$lines))
        if (first > last) 
            return(character())
        else return(srcfile$lines[first:last])
    }
    if (!.isOpen(srcfile)) 
        on.exit(close(srcfile))
    conn <- open(srcfile, first)
    lines <- readLines(conn, n = last - first + 1L, warn = FALSE)
    if (!is.null(Enc <- srcfile$Enc) && !(Enc %in% c("unknown", 
        "native.enc"))) 
        lines <- iconv(lines, "", Enc)
    srcfile$line <- first + length(lines)
    return(lines)
}


.Call <- function (.NAME, ..., PACKAGE)  .Primitive(".Call")


unclass <- function (x)  .Primitive("unclass")


.expand_R_libs_env_var <- function (x) 
{
    v <- paste(R.version[c("major", "minor")], collapse = ".")
    expand <- function(x, spec, expansion) gsub(paste0("(^|[^%])(%%)*%", 
        spec), sprintf("\\1\\2%s", expansion), x)
    x <- expand(x, "V", v)
    x <- expand(x, "v", sub("\\.[^.]*$", "", v))
    x <- expand(x, "p", R.version$platform)
    x <- expand(x, "a", R.version$arch)
    x <- expand(x, "o", R.version$os)
    gsub("%%", "%", x)
}


gc.time <- function (on = TRUE)  .Primitive("gc.time")


cummax <- function (x)  .Primitive("cummax")


gcinfo <- function (verbose) 
.Internal(gcinfo(verbose))


cummin <- function (x)  .Primitive("cummin")


.mergeImportMethods <- function (impenv, expenv, metaname) 
{
    impMethods <- impenv[[metaname]]
    if (!is.null(impMethods)) 
        impenv[[metaname]] <- methods:::.mergeMethodsTable2(impMethods, 
            newtable = expenv[[metaname]], expenv, metaname)
    impMethods
}


.subset2 <- function (x, ...)  .Primitive(".subset2")


addTaskCallback <- function (f, data = NULL, name = character()) 
{
    if (!is.function(f)) 
        stop("handler must be a function")
    val <- .Call(.C_R_addTaskCallback, f, data, !missing(data), 
        as.character(name))
    val + 1L
}


file.mode <- function (...) 
file.info(..., extra_cols = FALSE)$mode


.knownS3Generics <- c(Math = "base", Ops = "base", Summary = "base", Complex = "base", 
as.character = "base", as.data.frame = "base", as.environment = "base", 
as.matrix = "base", as.vector = "base", cbind = "base", labels = "base", 
print = "base", rbind = "base", rep = "base", seq = "base", seq.int = "base", 
solve = "base", summary = "base", t = "base", edit = "utils", 
str = "utils", contour = "graphics", hist = "graphics", identify = "graphics", 
image = "graphics", lines = "graphics", pairs = "graphics", plot = "graphics", 
points = "graphics", text = "graphics", add1 = "stats", AIC = "stats", 
anova = "stats", biplot = "stats", coef = "stats", confint = "stats", 
deviance = "stats", df.residual = "stats", drop1 = "stats", extractAIC = "stats", 
fitted = "stats", formula = "stats", logLik = "stats", model.frame = "stats", 
model.matrix = "stats", predict = "stats", profile = "stats", 
qqnorm = "stats", residuals = "stats", se.contrast = "stats", 
terms = "stats", update = "stats", vcov = "stats")


unlockBinding <- function (sym, env) 
{
    if (is.character(sym)) 
        sym <- as.name(sym)
    .Internal(unlockBinding(sym, env))
}


solve.qr <- function (a, b, ...) 
{
    if (!inherits(a, "qr")) 
        stop("this is the \"qr\" method for the generic function solve()")
    nc <- ncol(a$qr)
    nr <- nrow(a$qr)
    if (a$rank != min(nc, nr)) 
        stop("singular matrix 'a' in 'solve'")
    if (missing(b)) {
        if (nc != nr) 
            stop("only square matrices can be inverted")
        b <- diag(1, nc)
    }
    res <- qr.coef(a, b)
    res[is.na(res)] <- 0
    res
}


.deparseOpts <- function (control) 
{
    if (!length(control)) 
        return(0)
    opts <- pmatch(as.character(control), ..deparseOpts)
    if (anyNA(opts)) 
        stop(sprintf(ngettext(as.integer(sum(is.na(opts))), "deparse option %s is not recognized", 
            "deparse options %s are not recognized"), paste(sQuote(control[is.na(opts)]), 
            collapse = ", ")), call. = FALSE, domain = NA)
    if (any(opts == 1L)) 
        opts <- unique(c(opts[opts != 1L], 2L, 3L, 4L, 5L, 6L, 
            8L, 12L))
    if (10L %in% opts && 11L %in% opts) 
        stop("\"hexNumeric\" and \"digits17\" are mutually exclusive")
    sum(2^(opts - 2))
}


file.info <- function (..., extra_cols = TRUE) 
{
    res <- .Internal(file.info(fn <- c(...), extra_cols))
    res$mtime <- .POSIXct(res$mtime)
    res$ctime <- .POSIXct(res$ctime)
    res$atime <- .POSIXct(res$atime)
    class(res) <- "data.frame"
    attr(res, "row.names") <- fn
    res
}


.row <- function (dim) 
.Internal(row(dim))


round.POSIXt <- function (x, units = c("secs", "mins", "hours", "days", "months", 
    "years")) 
{
    .round_x_to_l_or_u <- function(lx, ll, lu) {
        cu <- as.POSIXct(lu)
        lu <- as.POSIXlt(cu)
        tu <- unclass(cu)
        tx <- unclass(as.POSIXct(lx))
        tl <- unclass(as.POSIXct(ll))
        up <- ((tu - tx) <= (tx - tl))
        up <- !is.na(up) & up
        y <- ll
        y[up] <- lu[up]
        y
    }
    units <- if (is.numeric(units) && units == 0) 
        "secs"
    else match.arg(units)
    if (units == "months") {
        x <- as.POSIXlt(x)
        ll <- trunc.POSIXt(x, "months")
        lu <- ll
        lu$mon <- lu$mon + 1L
        .round_x_to_l_or_u(x, ll, lu)
    }
    else if (units == "years") {
        x <- as.POSIXlt(x)
        ll <- trunc.POSIXt(x, "years")
        lu <- ll
        lu$year <- lu$year + 1L
        .round_x_to_l_or_u(x, ll, lu)
    }
    else trunc.POSIXt(as.POSIXct(x) + switch(units, secs = 0.5, 
        mins = 30, hours = 1800, days = 43200), units = units)
}


duplicated.POSIXlt <- function (x, incomparables = FALSE, ...) 
{
    x <- as.POSIXct(x)
    NextMethod("duplicated", x)
}


attachNamespace <- function (ns, pos = 2L, depends = NULL, exclude, include.only) 
{
    runHook <- function(hookname, env, libname, pkgname) {
        if (!is.null(fun <- env[[hookname]])) {
            res <- tryCatch(fun(libname, pkgname), error = identity)
            if (inherits(res, "error")) {
                stop(gettextf("%s failed in %s() for '%s', details:\n  call: %s\n  error: %s", 
                  hookname, "attachNamespace", nsname, deparse(conditionCall(res))[1L], 
                  conditionMessage(res)), call. = FALSE, domain = NA)
            }
        }
    }
    runUserHook <- function(pkgname, pkgpath) {
        hook <- getHook(packageEvent(pkgname, "attach"))
        for (fun in hook) try(fun(pkgname, pkgpath))
    }
    ns <- asNamespace(ns, base.OK = FALSE)
    nsname <- getNamespaceName(ns)
    nspath <- .getNamespaceInfo(ns, "path")
    attname <- paste0("package:", nsname)
    if (attname %in% search()) 
        stop("namespace is already attached")
    env <- attach(NULL, pos = pos, name = attname)
    on.exit(.Internal(detach(pos)))
    attr(env, "path") <- nspath
    exports <- getNamespaceExports(ns)
    importIntoEnv(env, exports, ns, exports)
    dimpenv <- .getNamespaceInfo(ns, "lazydata")
    dnames <- names(dimpenv)
    .Internal(importIntoEnv(env, dnames, dimpenv, dnames))
    if (length(depends) > 0L) 
        env$.Depends <- depends
    Sys.setenv(`_R_NS_LOAD_` = nsname)
    on.exit(Sys.unsetenv("_R_NS_LOAD_"), add = TRUE)
    runHook(".onAttach", ns, dirname(nspath), nsname)
    if (!missing(exclude) && length(exclude) > 0) 
        rm(list = exclude, envir = env)
    if (!missing(include.only)) {
        vars <- ls(env, all.names = TRUE)
        nf <- setdiff(include.only, vars)
        if (length(nf) > 0) {
            nf <- strwrap(paste(nf, collapse = ", "), indent = 4L, 
                exdent = 4L)
            stop(gettextf("not found in namespace %s: \n\n%s\n", 
                sQuote(nsname), nf), call. = FALSE, domain = NA)
        }
        rm(list = setdiff(vars, include.only), envir = env)
    }
    lockEnvironment(env, TRUE)
    runUserHook(nsname, nspath)
    on.exit()
    Sys.unsetenv("_R_NS_LOAD_")
    invisible(env)
}


dQuote <- function (x, q = getOption("useFancyQuotes")) 
{
    if (!length(x)) 
        return(character())
    before <- after <- "\""
    if (!is.null(q)) {
        if (isTRUE(q)) {
            li <- l10n_info()
            if (li$"UTF-8") 
                q <- "UTF-8"
            if (!is.null(li$codepage) && li$codepage > 0L) {
                if (li$codepage >= 1250L && li$codepage <= 1258L || 
                  li$codepage == 874L) {
                  before <- rawToChar(as.raw(147))
                  after <- rawToChar(as.raw(148))
                }
                else {
                  z <- iconv(c(intToUtf8(8220), intToUtf8(8221)), 
                    "UTF-8", "")
                  before <- z[1L]
                  after <- z[2L]
                }
            }
        }
        if (identical(q, "TeX")) {
            before <- "``"
            after <- "''"
        }
        if (identical(q, "UTF-8")) {
            before <- intToUtf8(8220)
            after <- intToUtf8(8221)
        }
        if (is.character(q) && length(q) >= 4L) {
            before <- q[3L]
            after <- q[4L]
        }
    }
    paste0(before, x, after)
}


bquote <- function (expr, where = parent.frame()) 
{
    unquote <- function(e) if (is.pairlist(e)) 
        as.pairlist(lapply(e, unquote))
    else if (length(e) <= 1L) 
        e
    else if (e[[1L]] == as.name(".")) 
        eval(e[[2L]], where)
    else as.call(lapply(e, unquote))
    unquote(substitute(expr))
}


builtins <- function (internal = FALSE) 
.Internal(builtins(internal))


.standard_regexps <- function () 
{
    list(valid_package_name = "[[:alpha:]][[:alnum:].]*[[:alnum:]]", 
        valid_package_version = "([[:digit:]]+[.-]){1,}[[:digit:]]+", 
        valid_R_system_version = "[[:digit:]]+\\.[[:digit:]]+\\.[[:digit:]]+", 
        valid_numeric_version = "([[:digit:]]+[.-])*[[:digit:]]+")
}


`length<-` <- function (x, value)  .Primitive("length<-")


format.summaryDefault <- function (x, digits = max(3L, getOption("digits") - 3L), ...) 
{
    xx <- x
    if (is.numeric(x) || is.complex(x)) {
        finite <- is.finite(x)
        xx[finite] <- zapsmall(x[finite])
    }
    class(xx) <- class(x)[-1]
    m <- match("NA's", names(x), 0)
    if (inherits(x, "Date") || inherits(x, "POSIXct")) {
        if (length(a <- attr(x, "NAs"))) 
            c(format(xx, digits = digits, ...), `NA's` = as.character(a))
        else format(xx, digits = digits)
    }
    else if (m && !is.character(x)) 
        xx <- c(format(xx[-m], digits = digits, ...), `NA's` = as.character(xx[m]))
    else format(xx, digits = digits, ...)
}


xzfile <- function (description, open = "", encoding = getOption("encoding"), 
    compression = 6) 
.Internal(xzfile(description, open, encoding, compression))


cut.Date <- function (x, breaks, labels = NULL, start.on.monday = TRUE, right = FALSE, 
    ...) 
{
    if (!inherits(x, "Date")) 
        stop("'x' must be a date-time object")
    x <- as.Date(x)
    if (inherits(breaks, "Date")) {
        breaks <- sort(as.Date(breaks))
    }
    else if (is.numeric(breaks) && length(breaks) == 1L) {
    }
    else if (is.character(breaks) && length(breaks) == 1L) {
        by2 <- strsplit(breaks, " ", fixed = TRUE)[[1L]]
        if (length(by2) > 2L || length(by2) < 1L) 
            stop("invalid specification of 'breaks'")
        valid <- pmatch(by2[length(by2)], c("days", "weeks", 
            "months", "years", "quarters"))
        if (is.na(valid)) 
            stop("invalid specification of 'breaks'")
        start <- as.POSIXlt(min(x, na.rm = TRUE))
        if (valid == 1L) 
            incr <- 1L
        if (valid == 2L) {
            start$mday <- start$mday - start$wday
            if (start.on.monday) 
                start$mday <- start$mday + ifelse(start$wday > 
                  0L, 1L, -6L)
            start$isdst <- -1L
            incr <- 7L
        }
        if (valid == 3L) {
            start$mday <- 1L
            start$isdst <- -1L
            end <- as.POSIXlt(max(x, na.rm = TRUE))
            step <- if (length(by2) == 2L) 
                as.integer(by2[1L])
            else 1L
            end <- as.POSIXlt(end + (31 * step * 86400))
            end$mday <- 1L
            end$isdst <- -1L
            breaks <- as.Date(seq(start, end, breaks))
        }
        else if (valid == 4L) {
            start$mon <- 0L
            start$mday <- 1L
            start$isdst <- -1L
            end <- as.POSIXlt(max(x, na.rm = TRUE))
            step <- if (length(by2) == 2L) 
                as.integer(by2[1L])
            else 1L
            end <- as.POSIXlt(end + (366 * step * 86400))
            end$mon <- 0L
            end$mday <- 1L
            end$isdst <- -1L
            breaks <- as.Date(seq(start, end, breaks))
        }
        else if (valid == 5L) {
            qtr <- rep(c(0L, 3L, 6L, 9L), each = 3L)
            start$mon <- qtr[start$mon + 1L]
            start$mday <- 1L
            start$isdst <- -1L
            maxx <- max(x, na.rm = TRUE)
            end <- as.POSIXlt(maxx)
            step <- if (length(by2) == 2L) 
                as.integer(by2[1L])
            else 1L
            end <- as.POSIXlt(end + (93 * step * 86400))
            end$mon <- qtr[end$mon + 1L]
            end$mday <- 1L
            end$isdst <- -1L
            breaks <- as.Date(seq(start, end, paste(step * 3L, 
                "months")))
            lb <- length(breaks)
            if (maxx < breaks[lb - 1]) 
                breaks <- breaks[-lb]
        }
        else {
            start <- as.Date(start)
            if (length(by2) == 2L) 
                incr <- incr * as.integer(by2[1L])
            maxx <- max(x, na.rm = TRUE)
            breaks <- seq(start, maxx + incr, breaks)
            breaks <- breaks[seq_len(1L + max(which(breaks <= 
                maxx)))]
        }
    }
    else stop("invalid specification of 'breaks'")
    res <- cut(unclass(x), unclass(breaks), labels = labels, 
        right = right, ...)
    if (is.null(labels)) {
        levels(res) <- as.character(if (is.numeric(breaks)) x[!duplicated(res)] else breaks[-length(breaks)])
    }
    res
}


nchar <- function (x, type = "chars", allowNA = FALSE, keepNA = NA) 
.Internal(nchar(x, type, allowNA, keepNA))


simplify2array <- function (x, higher = TRUE) 
{
    if (length(common.len <- unique(lengths(x))) > 1L) 
        return(x)
    if (common.len == 1L) 
        unlist(x, recursive = FALSE)
    else if (common.len > 1L) {
        n <- length(x)
        r <- unlist(x, recursive = FALSE, use.names = FALSE)
        if (higher && length(c.dim <- unique(lapply(x, dim))) == 
            1 && is.numeric(c.dim <- c.dim[[1L]]) && prod(d <- c(c.dim, 
            n)) == length(r)) {
            iN1 <- is.null(n1 <- dimnames(x[[1L]]))
            n2 <- names(x)
            dnam <- if (!(iN1 && is.null(n2))) 
                c(if (iN1) rep.int(list(n1), length(c.dim)) else n1, 
                  list(n2))
            array(r, dim = d, dimnames = dnam)
        }
        else if (prod(d <- c(common.len, n)) == length(r)) 
            array(r, dim = d, dimnames = if (!(is.null(n1 <- names(x[[1L]])) & 
                is.null(n2 <- names(x)))) 
                list(n1, n2))
        else x
    }
    else x
}


writeChar <- function (object, con, nchars = nchar(object, type = "chars"), 
    eos = "", useBytes = FALSE) 
{
    if (!is.character(object)) 
        stop("can only write character objects")
    if (is.character(con)) {
        con <- file(con, "wb")
        on.exit(close(con))
    }
    .Internal(writeChar(object, con, as.integer(nchars), eos, 
        useBytes))
}


La_library <- function () 
.Internal(La_library())


strptime <- function (x, format, tz = "") 
{
    y <- .Internal(strptime(as.character(x), format, tz))
    names(y$year) <- names(x)
    y
}


all.equal.numeric <- function (target, current, tolerance = sqrt(.Machine$double.eps), 
    scale = NULL, countEQ = FALSE, formatFUN = function(err, 
        what) format(err), ..., check.attributes = TRUE) 
{
    if (!is.numeric(tolerance)) 
        stop("'tolerance' should be numeric")
    if (!is.numeric(scale) && !is.null(scale)) 
        stop("'scale' should be numeric or NULL")
    if (!is.logical(check.attributes)) 
        stop(gettextf("'%s' must be logical", "check.attributes"), 
            domain = NA)
    msg <- if (check.attributes) 
        attr.all.equal(target, current, tolerance = tolerance, 
            scale = scale, ...)
    if (data.class(target) != data.class(current)) {
        msg <- c(msg, paste0("target is ", data.class(target), 
            ", current is ", data.class(current)))
        return(msg)
    }
    lt <- length(target)
    lc <- length(current)
    cplx <- is.complex(target)
    if (lt != lc) {
        if (!is.null(msg)) 
            msg <- msg[-grep("\\bLengths\\b", msg)]
        msg <- c(msg, paste0(if (cplx) "Complex" else "Numeric", 
            ": lengths (", lt, ", ", lc, ") differ"))
        return(msg)
    }
    target <- as.vector(target)
    current <- as.vector(current)
    out <- is.na(target)
    if (any(out != is.na(current))) {
        msg <- c(msg, paste("'is.NA' value mismatch:", sum(is.na(current)), 
            "in current", sum(out), "in target"))
        return(msg)
    }
    out <- out | target == current
    if (all(out)) 
        return(if (is.null(msg)) TRUE else msg)
    if (countEQ) {
        N <- length(out)
        sabst0 <- sum(abs(target[out]))
    }
    else sabst0 <- 0
    target <- target[!out]
    current <- current[!out]
    if (!countEQ) 
        N <- length(target)
    if (is.integer(target) && is.integer(current)) 
        target <- as.double(target)
    xy <- sum(abs(target - current))/N
    what <- if (is.null(scale)) {
        xn <- (sabst0 + sum(abs(target)))/N
        if (is.finite(xn) && xn > tolerance) {
            xy <- xy/xn
            "relative"
        }
        else "absolute"
    }
    else {
        stopifnot(all(scale > 0))
        xy <- xy/scale
        if (all(abs(scale - 1) < 1e-07)) 
            "absolute"
        else "scaled"
    }
    if (cplx) 
        what <- paste(what, "Mod")
    if (is.na(xy) || xy > tolerance) 
        msg <- c(msg, paste("Mean", what, "difference:", formatFUN(xy, 
            what)))
    if (is.null(msg)) 
        TRUE
    else msg
}


readline <- function (prompt = "") 
.Internal(readline(prompt))


duplicated.data.frame <- function (x, incomparables = FALSE, fromLast = FALSE, ...) 
{
    if (!isFALSE(incomparables)) 
        .NotYetUsed("incomparables != FALSE")
    if (length(x) != 1L) {
        if (any(i <- vapply(x, is.factor, NA))) 
            x[i] <- lapply(x[i], as.numeric)
        duplicated(do.call(Map, `names<-`(c(list, x), NULL)), 
            fromLast = fromLast)
    }
    else duplicated(x[[1L]], fromLast = fromLast, ...)
}


.__H__.rbind <- function (..., deparse.level = 1) 
{
    .Deprecated("base::rbind")
    .Internal(rbind(deparse.level, ...))
}


.fixupGFortranStdout <- function () 
{
    old <- Sys.getenv("GFORTRAN_STDOUT_UNIT")
    if (nzchar(old) && old == "-1") {
        Sys.unsetenv("GFORTRAN_STDOUT_UNIT")
        TRUE
    }
    else FALSE
}


registerS3method <- function (genname, class, method, envir = parent.frame()) 
{
    addNamespaceS3method <- function(ns, generic, class, method) {
        regs <- rbind(.getNamespaceInfo(ns, "S3methods"), c(generic, 
            class, method, NA_character_))
        setNamespaceInfo(ns, "S3methods", regs)
    }
    groupGenerics <- c("Math", "Ops", "Summary", "Complex")
    defenv <- if (genname %in% groupGenerics) 
        .BaseNamespaceEnv
    else {
        genfun <- get(genname, envir = envir)
        if (.isMethodsDispatchOn() && methods::is(genfun, "genericFunction")) 
            genfun <- methods::finalDefaultMethod(genfun@default)
        if (typeof(genfun) == "closure") 
            environment(genfun)
        else .BaseNamespaceEnv
    }
    if (is.null(table <- defenv[[".__S3MethodsTable__."]])) {
        table <- new.env(hash = TRUE, parent = baseenv())
        defenv[[".__S3MethodsTable__."]] <- table
    }
    if (is.character(method)) {
        assignWrapped <- function(x, method, home, envir) {
            method <- method
            home <- home
            delayedAssign(x, get(method, envir = home), assign.env = envir)
        }
        if (!exists(method, envir = envir)) {
            warning(gettextf("S3 method %s was declared but not found", 
                sQuote(method)), call. = FALSE)
        }
        else {
            assignWrapped(paste(genname, class, sep = "."), method, 
                home = envir, envir = table)
        }
    }
    else if (is.function(method)) 
        assign(paste(genname, class, sep = "."), method, envir = table)
    else stop("bad method")
    if (isNamespace(envir) && !identical(envir, .BaseNamespaceEnv)) 
        addNamespaceS3method(envir, genname, class, method)
}


as.list.default <- function (x, ...) 
if (typeof(x) == "list") x else .Internal(as.vector(x, "list"))


setNamespaceInfo <- function (ns, which, val) 
{
    ns <- asNamespace(ns, base.OK = FALSE)
    info <- ns[[".__NAMESPACE__."]]
    info[[which]] <- val
}


seq.Date <- function (from, to, by, length.out = NULL, along.with = NULL, 
    ...) 
{
    if (missing(from)) 
        stop("'from' must be specified")
    if (!inherits(from, "Date")) 
        stop("'from' must be a \"Date\" object")
    if (length(as.Date(from)) != 1L) 
        stop("'from' must be of length 1")
    if (!missing(to)) {
        if (!inherits(to, "Date")) 
            stop("'to' must be a \"Date\" object")
        if (length(as.Date(to)) != 1L) 
            stop("'to' must be of length 1")
    }
    if (!missing(along.with)) {
        length.out <- length(along.with)
    }
    else if (!is.null(length.out)) {
        if (length(length.out) != 1L) 
            stop("'length.out' must be of length 1")
        length.out <- ceiling(length.out)
    }
    status <- c(!missing(to), !missing(by), !is.null(length.out))
    if (sum(status) != 2L) 
        stop("exactly two of 'to', 'by' and 'length.out' / 'along.with' must be specified")
    if (missing(by)) {
        from <- unclass(as.Date(from))
        to <- unclass(as.Date(to))
        res <- seq.int(from, to, length.out = length.out)
        return(.Date(res))
    }
    if (length(by) != 1L) 
        stop("'by' must be of length 1")
    valid <- 0L
    if (inherits(by, "difftime")) {
        by <- switch(attr(by, "units"), secs = 1/86400, mins = 1/1440, 
            hours = 1/24, days = 1, weeks = 7) * unclass(by)
    }
    else if (is.character(by)) {
        by2 <- strsplit(by, " ", fixed = TRUE)[[1L]]
        if (length(by2) > 2L || length(by2) < 1L) 
            stop("invalid 'by' string")
        valid <- pmatch(by2[length(by2)], c("days", "weeks", 
            "months", "quarters", "years"))
        if (is.na(valid)) 
            stop("invalid string for 'by'")
        if (valid <= 2L) {
            by <- c(1, 7)[valid]
            if (length(by2) == 2L) 
                by <- by * as.integer(by2[1L])
        }
        else by <- if (length(by2) == 2L) 
            as.integer(by2[1L])
        else 1
    }
    else if (!is.numeric(by)) 
        stop("invalid mode for 'by'")
    if (is.na(by)) 
        stop("'by' is NA")
    if (valid <= 2L) {
        from <- unclass(as.Date(from))
        if (!is.null(length.out)) 
            res <- seq.int(from, by = by, length.out = length.out)
        else {
            to0 <- unclass(as.Date(to))
            res <- seq.int(0, to0 - from, by) + from
        }
        res <- .Date(res)
    }
    else {
        r1 <- as.POSIXlt(from)
        if (valid == 5L) {
            if (missing(to)) {
                yr <- seq.int(r1$year, by = by, length.out = length.out)
            }
            else {
                to0 <- as.POSIXlt(to)
                yr <- seq.int(r1$year, to0$year, by)
            }
            r1$year <- yr
            res <- as.Date(r1)
        }
        else {
            if (valid == 4L) 
                by <- by * 3
            if (missing(to)) {
                mon <- seq.int(r1$mon, by = by, length.out = length.out)
            }
            else {
                to0 <- as.POSIXlt(to)
                mon <- seq.int(r1$mon, 12 * (to0$year - r1$year) + 
                  to0$mon, by)
            }
            r1$mon <- mon
            res <- as.Date(r1)
        }
    }
    if (!missing(to)) {
        to <- as.Date(to)
        res <- if (by > 0) 
            res[res <= to]
        else res[res >= to]
    }
    res
}


getNamespaceInfo <- function (ns, which) 
{
    ns <- asNamespace(ns, base.OK = FALSE)
    get(which, envir = ns[[".__NAMESPACE__."]])
}


asinh <- function (x)  .Primitive("asinh")


sys.on.exit <- function () 
.Internal(sys.on.exit())


namespaceImportMethods <- function (self, ns, vars, from = NULL) 
{
    allVars <- character()
    generics <- character()
    packages <- character()
    allFuns <- methods:::.getGenerics(ns)
    allPackages <- attr(allFuns, "package")
    pkg <- methods::getPackageName(ns)
    found <- vars %in% allFuns
    if (!all(found)) {
        message(sprintf(ngettext(sum(!found), "No methods found in package %s for request: %s when loading %s", 
            "No methods found in package %s for requests: %s when loading %s"), 
            sQuote(pkg), paste(sQuote(vars[!found]), collapse = ", "), 
            sQuote(getNamespaceName(self))), domain = NA)
        vars <- vars[found]
    }
    found <- vars %in% allFuns
    if (!all(found)) 
        stop(sprintf(ngettext(sum(!found), "requested method not found in environment/package %s: %s when loading %s", 
            "requested methods not found in environment/package %s: %s when loading %s"), 
            sQuote(pkg), paste(sQuote(vars[!found]), collapse = ", "), 
            sQuote(getNamespaceName(self))), call. = FALSE, domain = NA)
    for (i in seq_along(allFuns)) {
        g <- allFuns[[i]]
        p <- allPackages[[i]]
        if (exists(g, envir = self, inherits = FALSE) || g %in% 
            vars) {
            tbl <- methods:::.TableMetaName(g, p)
            if (is.null(.mergeImportMethods(self, ns, tbl))) {
                allVars <- c(allVars, tbl)
                generics <- c(generics, g)
                packages <- c(packages, p)
            }
        }
        if (g %in% vars && !exists(g, envir = self, inherits = FALSE)) {
            if (!is.null(f <- get0(g, envir = ns)) && methods::is(f, 
                "genericFunction")) {
                allVars <- c(allVars, g)
                generics <- c(generics, g)
                packages <- c(packages, p)
            }
            else if (g %in% c("as.vector", "is.unsorted", "unlist")) {
            }
            else {
                fun <- methods::getFunction(g, mustFind = FALSE, 
                  where = self)
                if (is.primitive(fun) || methods::is(fun, "genericFunction")) {
                }
                else warning(gettextf("No generic function %s found corresponding to requested imported methods from package %s when loading %s (malformed exports?)", 
                  sQuote(g), sQuote(pkg), sQuote(from)), domain = NA, 
                  call. = FALSE)
            }
        }
    }
    namespaceImportFrom(self, asNamespace(ns), allVars, generics, 
        packages, from = from)
}


sort.int <- function (x, partial = NULL, na.last = NA, decreasing = FALSE, 
    method = c("auto", "shell", "quick", "radix"), index.return = FALSE) 
{
    decreasing <- as.logical(decreasing)
    if (is.null(partial) && !index.return && is.numeric(x)) {
        if (.Internal(sorted_fpass(x, decreasing, na.last))) {
            attr <- attributes(x)
            if (!is.null(attr) && !identical(names(attr), "names")) 
                attributes(x) <- list(names = names(x))
            return(x)
        }
    }
    method <- match.arg(method)
    if (method == "auto" && is.null(partial) && (is.numeric(x) || 
        is.factor(x) || is.logical(x)) && is.integer(length(x))) 
        method <- "radix"
    if (method == "radix") {
        if (!is.null(partial)) {
            stop("'partial' sorting not supported by radix method")
        }
        if (index.return && is.na(na.last)) {
            x <- x[!is.na(x)]
            na.last <- TRUE
        }
        o <- order(x, na.last = na.last, decreasing = decreasing, 
            method = "radix")
        y <- x[o]
        y <- .doSortWrap(y, decreasing, na.last)
        return(if (index.return) list(x = y, ix = o) else y)
    }
    else if (method == "auto" || !is.numeric(x)) 
        method <- "shell"
    if (isfact <- is.factor(x)) {
        if (index.return) 
            stop("'index.return' only for non-factors")
        lev <- levels(x)
        nlev <- nlevels(x)
        isord <- is.ordered(x)
        x <- c(x)
    }
    else if (!is.atomic(x)) 
        stop("'x' must be atomic")
    if (has.na <- any(ina <- is.na(x))) {
        nas <- x[ina]
        x <- x[!ina]
    }
    if (index.return && !is.na(na.last)) 
        stop("'index.return' only for 'na.last = NA'")
    if (!is.null(partial)) {
        if (index.return || decreasing || isfact || method != 
            "shell") 
            stop("unsupported options for partial sorting")
        if (!all(is.finite(partial))) 
            stop("non-finite 'partial'")
        y <- if (length(partial) <= 10L) {
            partial <- .Internal(qsort(partial, FALSE))
            .Internal(psort(x, partial))
        }
        else if (is.double(x)) 
            .Internal(qsort(x, FALSE))
        else .Internal(sort(x, FALSE))
    }
    else {
        nms <- names(x)
        switch(method, quick = {
            if (!is.null(nms)) {
                if (decreasing) x <- -x
                y <- .Internal(qsort(x, TRUE))
                if (decreasing) y$x <- -y$x
                names(y$x) <- nms[y$ix]
                if (!index.return) y <- y$x
            } else {
                if (decreasing) x <- -x
                y <- .Internal(qsort(x, index.return))
                if (decreasing) if (index.return) y$x <- -y$x else y <- -y
            }
        }, shell = {
            if (index.return || !is.null(nms)) {
                o <- sort.list(x, decreasing = decreasing)
                y <- if (index.return) list(x = x[o], ix = o) else x[o]
            } else y <- .Internal(sort(x, decreasing))
        })
    }
    if (!is.na(na.last) && has.na) 
        y <- if (!na.last) 
            c(nas, y)
        else c(y, nas)
    if (isfact) 
        y <- (if (isord) 
            ordered
        else factor)(y, levels = seq_len(nlev), labels = lev)
    if (is.null(partial)) {
        y <- .doSortWrap(y, decreasing, na.last)
    }
    y
}


cumsum <- function (x)  .Primitive("cumsum")


environmentName <- function (env) 
.Internal(environmentName(env))


as.list.environment <- function (x, all.names = FALSE, sorted = FALSE, ...) 
.Internal(env2list(x, all.names, sorted))


restartDescription <- function (r) 
r$description


`[<-` <- .Primitive("[<-")


.signalSimpleWarning <- function (msg, call) 
withRestarts({
    .Internal(.signalCondition(simpleWarning(msg, call), msg, 
        call))
    .Internal(.dfltWarn(msg, call))
}, muffleWarning = function() NULL)


autoload <- function (name, package, reset = FALSE, ...) 
{
    if (!reset && exists(name, envir = .GlobalEnv, inherits = FALSE)) 
        stop("an object with that name already exists")
    m <- match.call()
    m[[1L]] <- as.name("list")
    newcall <- eval(m, parent.frame())
    newcall <- as.call(c(as.name("autoloader"), newcall))
    newcall$reset <- NULL
    if (is.na(match(package, .Autoloaded))) 
        assign(".Autoloaded", c(package, .Autoloaded), envir = .AutoloadEnv)
    do.call("delayedAssign", list(name, newcall, .GlobalEnv, 
        .AutoloadEnv))
    invisible()
}


format.pval <- function (pv, digits = max(1L, getOption("digits") - 2L), eps = .Machine$double.eps, 
    na.form = "NA", ...) 
{
    if ((has.na <- any(ina <- is.na(pv)))) 
        pv <- pv[!ina]
    r <- character(length(is0 <- pv < eps))
    if (any(!is0)) {
        rr <- pv <- pv[!is0]
        expo <- floor(log10(ifelse(pv > 0, pv, 1e-50)))
        fixp <- expo >= -3 | (expo == -4 & digits > 1)
        if (any(fixp)) 
            rr[fixp] <- format(pv[fixp], digits = digits, ...)
        if (any(!fixp)) 
            rr[!fixp] <- format(pv[!fixp], digits = digits, ...)
        r[!is0] <- rr
    }
    if (any(is0)) {
        digits <- max(1L, digits - 2L)
        if (any(!is0)) {
            nc <- max(nchar(rr, type = "w"))
            if (digits > 1L && digits + 6L > nc) 
                digits <- max(1L, nc - 7L)
            sep <- if (digits == 1L && nc <= 6L) 
                ""
            else " "
        }
        else sep <- if (digits == 1) 
            ""
        else " "
        r[is0] <- paste("<", format(eps, digits = digits, ...), 
            sep = sep)
    }
    if (has.na) {
        rok <- r
        r <- character(length(ina))
        r[!ina] <- rok
        r[ina] <- na.form
    }
    r
}


as.data.frame <- function (x, row.names = NULL, optional = FALSE, ...) 
{
    if (is.null(x)) 
        return(as.data.frame(list()))
    UseMethod("as.data.frame")
}


`environment<-` <- function (fun, value)  .Primitive("environment<-")


as.call <- function (x)  .Primitive("as.call")


with <- function (data, expr, ...) 
UseMethod("with")


round.Date <- function (x, ...) 
{
    .Date(NextMethod(), oldClass(x))
}


emptyenv <- function ()  .Primitive("emptyenv")


`[.AsIs` <- function (x, i, ...) 
I(NextMethod("["))


as.list.function <- function (x, ...) 
c(formals(x), list(body(x)))


agrepl <- function (pattern, x, max.distance = 0.1, costs = NULL, ignore.case = FALSE, 
    fixed = TRUE, useBytes = FALSE) 
{
    pattern <- as.character(pattern)
    if (!is.character(x)) 
        x <- as.character(x)
    costs <- as.integer(.amatch_costs(costs))
    bounds <- .amatch_bounds(max.distance)
    .Internal(agrepl(pattern, x, ignore.case, FALSE, costs, bounds, 
        useBytes, fixed))
}


`comment<-` <- function (x, value) 
.Internal(`comment<-`(x, value))


seq_len <- function (length.out)  .Primitive("seq_len")


rowsum.default <- function (x, group, reorder = TRUE, na.rm = FALSE, ...) 
{
    if (!is.numeric(x)) 
        stop("'x' must be numeric")
    if (length(group) != NROW(x)) 
        stop("incorrect length for 'group'")
    if (anyNA(group)) 
        warning("missing values for 'group'")
    ugroup <- unique(group)
    if (reorder) 
        ugroup <- sort(ugroup, na.last = TRUE, method = "quick")
    .Internal(rowsum_matrix(x, group, ugroup, na.rm, as.character(ugroup)))
}


months.POSIXt <- function (x, abbreviate = FALSE) 
{
    format(x, ifelse(abbreviate, "%b", "%B"))
}


besselI <- function (x, nu, expon.scaled = FALSE) 
{
    .Internal(besselI(x, nu, 1 + as.logical(expon.scaled)))
}


besselJ <- function (x, nu) 
.Internal(besselJ(x, nu))


besselK <- function (x, nu, expon.scaled = FALSE) 
{
    .Internal(besselK(x, nu, 1 + as.logical(expon.scaled)))
}


besselY <- function (x, nu) 
.Internal(besselY(x, nu))


numeric <- function (length = 0L) 
.Internal(vector("double", length))


withCallingHandlers <- function (expr, ...) 
{
    handlers <- list(...)
    classes <- names(handlers)
    parentenv <- parent.frame()
    if (length(classes) != length(handlers)) 
        stop("bad handler specification")
    .Internal(.addCondHands(classes, handlers, parentenv, NULL, 
        TRUE))
    expr
}


open.srcfilecopy <- function (con, line, ...) 
{
    srcfile <- con
    oldline <- srcfile$line
    if (!is.null(oldline) && oldline > line) 
        close(srcfile)
    conn <- srcfile$conn
    if (is.null(conn)) {
        srcfile$conn <- conn <- textConnection(srcfile$lines, 
            open = "r")
        srcfile$line <- 1L
        oldline <- 1L
    }
    else if (!isOpen(conn)) {
        open(conn, open = "r")
        srcfile$line <- 1L
        oldline <- 1L
    }
    if (oldline < line) {
        readLines(conn, line - oldline, warn = FALSE)
        srcfile$line <- line
    }
    invisible(conn)
}


weekdays <- function (x, abbreviate) 
UseMethod("weekdays")


formatDL <- function (x, y, style = c("table", "list"), width = 0.9 * getOption("width"), 
    indent = NULL) 
{
    if (is.list(x)) {
        if (length(x) == 2L && diff(lengths(x)) == 0L) {
            y <- x[[2L]]
            x <- x[[1L]]
        }
        else stop("incorrect value for 'x'")
    }
    else if (is.matrix(x)) {
        if (NCOL(x) == 2L) {
            y <- x[, 2L]
            x <- x[, 1L]
        }
        else stop("incorrect value for 'x'")
    }
    else if (missing(y) && !is.null(nms <- names(x))) {
        y <- x
        x <- nms
    }
    else if (length(x) != length(y)) 
        stop("'x' and 'y' must have the same length")
    x <- as.character(x)
    if (!length(x)) 
        return(x)
    y <- as.character(y)
    style <- match.arg(style)
    if (is.null(indent)) 
        indent <- switch(style, table = width/3, list = width/9)
    indent <- min(indent, 0.5 * width)
    indentString <- strrep(" ", indent)
    if (style == "table") {
        i <- (nchar(x, type = "w") > indent - 3L)
        if (any(i)) 
            x[i] <- paste0(x[i], "\n", indentString)
        i <- !i
        if (any(i)) 
            x[i] <- formatC(x[i], width = indent, flag = "-")
        y <- lapply(strwrap(y, width = width - indent, simplify = FALSE), 
            paste, collapse = paste0("\n", indentString))
        r <- paste0(x, unlist(y))
    }
    else if (style == "list") {
        y <- strwrap(paste0(x, ": ", y), exdent = indent, width = width, 
            simplify = FALSE)
        r <- unlist(lapply(y, paste, collapse = "\n"))
    }
    r
}


qr.fitted <- function (qr, y, k = qr$rank) 
{
    if (!is.qr(qr)) 
        stop("argument is not a QR decomposition")
    if (is.complex(qr$qr)) 
        stop("not implemented for complex 'qr'")
    if (isTRUE(attr(qr, "useLAPACK"))) 
        stop("not supported for LAPACK QR")
    n <- as.integer(nrow(qr$qr))
    if (is.na(n)) 
        stop("invalid nrow(qr$qr)")
    k <- as.integer(k)
    if (k > qr$rank) 
        stop("'k' is too large")
    ny <- as.integer(NCOL(y))
    if (is.na(ny)) 
        stop("invalid NCOL(y)")
    if (NROW(y) != n) 
        stop("'qr' and 'y' must have the same number of rows")
    storage.mode(y) <- "double"
    .Fortran(.F_dqrxb, as.double(qr$qr), n, k, as.double(qr$qraux), 
        y, ny, xb = y)$xb
}


system2 <- function (command, args = character(), stdout = "", stderr = "", 
    stdin = "", input = NULL, env = character(), wait = TRUE, 
    minimized = FALSE, invisible = TRUE, timeout = 0) 
{
    if (!is.logical(wait) || is.na(wait)) 
        stop("'wait' must be TRUE or FALSE")
    if (!is.logical(minimized) || is.na(minimized)) 
        stop("'minimized' must be TRUE or FALSE")
    if (!is.logical(invisible) || is.na(invisible)) 
        stop("'invisible' must be TRUE or FALSE")
    command <- paste(c(shQuote(command), env, args), collapse = " ")
    if (is.null(stdout)) 
        stdout <- FALSE
    if (is.null(stderr)) 
        stderr <- FALSE
    if (length(stdout) != 1L) 
        stop("'stdout' must be of length 1")
    if (length(stderr) != 1L) 
        stop("'stderr' must be of length 1")
    if (!is.null(input)) {
        f <- tempfile()
        on.exit(unlink(f))
        writeLines(input, f)
    }
    else f <- stdin
    rf <- NULL
    if (.Platform$GUI == "Rgui") {
        if (isTRUE(stdout) && identical(stderr, "")) {
            rf <- tempfile()
            on.exit(unlink(rf), add = TRUE)
            stdout <- rf
            wait <- TRUE
        }
        else if (isTRUE(stderr) && identical(stdout, "")) {
            rf <- tempfile()
            on.exit(unlink(rf), add = TRUE)
            stderr <- rf
            wait <- TRUE
        }
    }
    flag <- if (isTRUE(stdout) || isTRUE(stderr)) 
        3L
    else if (wait) 
        ifelse(identical(stdout, "") || identical(stderr, ""), 
            2L, 1L)
    else 0L
    if (invisible) 
        flag <- 20L + flag
    else if (minimized) 
        flag <- 10L + flag
    if (.fixupGFortranStdout()) 
        on.exit(Sys.setenv(GFORTRAN_STDOUT_UNIT = "-1"), add = TRUE)
    if (.fixupGFortranStderr()) 
        on.exit(Sys.setenv(GFORTRAN_STDERR_UNIT = "-1"), add = TRUE)
    rval <- .Internal(system(command, flag, f, stdout, stderr, 
        timeout))
    if (is.null(rf)) 
        rval
    else {
        ans <- character(0)
        if (is.numeric(rval) && length(rval) > 0 && rval != 0) {
            rval <- as.integer(rval)
            attr(ans, "status") <- rval
            warning(gettextf("running command '%s' had status %d", 
                command, rval), domain = NA)
        }
        ans
    }
}


`mostattributes<-` <- function (x, value) 
{
    if (length(value)) {
        if (!is.list(value)) 
            stop("'value' must be a list")
        if (h.nam <- !is.na(inam <- match("names", names(value)))) {
            n1 <- value[[inam]]
            value <- value[-inam]
        }
        if (h.dim <- !is.na(idin <- match("dim", names(value)))) {
            d1 <- value[[idin]]
            value <- value[-idin]
        }
        if (h.dmn <- !is.na(idmn <- match("dimnames", names(value)))) {
            dn1 <- value[[idmn]]
            value <- value[-idmn]
        }
        attributes(x) <- value
        dm <- attr(x, "dim")
        L <- length(if (is.list(x)) unclass(x) else x)
        if (h.dim && L == prod(d1)) 
            attr(x, "dim") <- dm <- d1
        if (h.dmn && !is.null(dm)) {
            ddn <- vapply(dn1, length, 1, USE.NAMES = FALSE)
            if (all((dm == ddn)[ddn > 0])) 
                attr(x, "dimnames") <- dn1
        }
        if (h.nam && is.null(dm) && L == length(n1)) 
            attr(x, "names") <- n1
    }
    x
}


.External2 <- function (.NAME, ..., PACKAGE)  .Primitive(".External2")


as.factor <- function (x) 
{
    if (is.factor(x)) 
        x
    else if (!is.object(x) && is.integer(x)) {
        levels <- sort.int(unique.default(x))
        f <- match(x, levels)
        levels(f) <- as.character(levels)
        if (!is.null(nx <- names(x))) 
            names(f) <- nx
        class(f) <- "factor"
        f
    }
    else factor(x)
}


data.class <- function (x) 
{
    if (length(cl <- oldClass(x))) 
        cl[1L]
    else {
        l <- length(dim(x))
        if (l == 2L) 
            "matrix"
        else if (l) 
            "array"
        else mode(x)
    }
}


is.symbol <- function (x)  .Primitive("is.symbol")


`[.difftime` <- function (x, ..., drop = TRUE) 
.difftime(NextMethod("["), attr(x, "units"), oldClass(x))


`substring<-` <- function (text, first, last = 1000000L, value) 
.Internal(`substr<-`(text, as.integer(first), as.integer(last), 
    value))


memCompress <- function (from, type = c("gzip", "bzip2", "xz", "none")) 
{
    if (is.character(from)) 
        from <- charToRaw(paste(from, collapse = "\n"))
    else if (!is.raw(from)) 
        stop("'from' must be raw or character")
    type <- match(match.arg(type), c("none", "gzip", "bzip2", 
        "xz"))
    .Internal(memCompress(from, type))
}


simpleMessage <- function (message, call = NULL) 
structure(list(message = message, call = call), class = c("simpleMessage", 
    "message", "condition"))


La_version <- function () 
.Internal(La_version())


as.difftime <- function (tim, format = "%X", units = "auto") 
{
    if (inherits(tim, "difftime")) 
        return(tim)
    if (is.character(tim)) {
        difftime(strptime(tim, format = format), strptime("0:0:0", 
            format = "%X"), units = units)
    }
    else {
        if (!is.numeric(tim)) 
            stop("'tim' is not character or numeric")
        if (units == "auto") 
            stop("need explicit units for numeric conversion")
        if (!(units %in% c("secs", "mins", "hours", "days", "weeks"))) 
            stop("invalid units specified")
        .difftime(tim, units = units)
    }
}


make.names <- function (names, unique = FALSE, allow_ = TRUE) 
{
    names <- as.character(names)
    names2 <- .Internal(make.names(names, allow_))
    if (unique) {
        o <- order(names != names2)
        names2[o] <- make.unique(names2[o])
    }
    names2
}


as.null <- function (x, ...) 
UseMethod("as.null")


storage.mode <- function (x) 
switch(tx <- typeof(x), closure = , builtin = , special = "function", 
    tx)


`substr<-` <- function (x, start, stop, value) 
.Internal(`substr<-`(x, as.integer(start), as.integer(stop), 
    value))


getExportedValue <- function (ns, name) 
{
    ns <- asNamespace(ns)
    if (isBaseNamespace(ns)) 
        get(name, envir = ns, inherits = FALSE)
    else {
        if (!is.null(oNam <- .getNamespaceInfo(ns, "exports")[[name]])) {
            get0(oNam, envir = ns)
        }
        else {
            ld <- .getNamespaceInfo(ns, "lazydata")
            if (!is.null(obj <- ld[[name]])) 
                obj
            else {
                if (exists(name, envir = ld, inherits = FALSE)) 
                  NULL
                else stop(gettextf("'%s' is not an exported object from 'namespace:%s'", 
                  name, getNamespaceName(ns)), call. = FALSE, 
                  domain = NA)
            }
        }
    }
}


mean.Date <- function (x, ...) 
.Date(mean(unclass(x), ...))


atan2 <- function (y, x) 
.Internal(atan2(y, x))


requireNamespace <- function (package, ..., quietly = FALSE) 
{
    package <- as.character(package)[[1L]]
    ns <- .Internal(getRegisteredNamespace(package))
    res <- TRUE
    if (is.null(ns)) {
        if (!quietly) 
            packageStartupMessage(gettextf("Loading required namespace: %s", 
                package), domain = NA)
        value <- tryCatch(loadNamespace(package, ...), error = function(e) e)
        if (inherits(value, "error")) {
            if (!quietly) {
                msg <- conditionMessage(value)
                cat("Failed with error:  ", sQuote(msg), "\n", 
                  file = stderr(), sep = "")
                .Internal(printDeferredWarnings())
            }
            res <- FALSE
        }
    }
    invisible(res)
}


atanh <- function (x)  .Primitive("atanh")


is.nan <- function (x)  .Primitive("is.nan")


class <- function (x)  .Primitive("class")


`+.POSIXt` <- function (e1, e2) 
{
    coerceTimeUnit <- function(x) as.vector(switch(attr(x, "units"), 
        secs = x, mins = 60 * x, hours = 60 * 60 * x, days = 60 * 
            60 * 24 * x, weeks = 60 * 60 * 24 * 7 * x))
    if (nargs() == 1L) 
        return(e1)
    if (inherits(e1, "POSIXt") && inherits(e2, "POSIXt")) 
        stop("binary '+' is not defined for \"POSIXt\" objects")
    if (inherits(e1, "POSIXlt")) 
        e1 <- as.POSIXct(e1)
    if (inherits(e2, "POSIXlt")) 
        e2 <- as.POSIXct(e2)
    if (inherits(e1, "difftime")) 
        e1 <- coerceTimeUnit(e1)
    if (inherits(e2, "difftime")) 
        e2 <- coerceTimeUnit(e2)
    .POSIXct(unclass(e1) + unclass(e2), check_tzones(e1, e2))
}


`length<-.factor` <- function (x, value) 
{
    cl <- class(x)
    levs <- levels(x)
    x <- NextMethod()
    structure(x, levels = levs, class = cl)
}


agrep <- function (pattern, x, max.distance = 0.1, costs = NULL, ignore.case = FALSE, 
    value = FALSE, fixed = TRUE, useBytes = FALSE) 
{
    pattern <- as.character(pattern)
    if (!is.character(x)) 
        x <- as.character(x)
    costs <- as.integer(.amatch_costs(costs))
    bounds <- .amatch_bounds(max.distance)
    .Internal(agrep(pattern, x, ignore.case, value, costs, bounds, 
        useBytes, fixed))
}


which.min <- function (x) 
.Internal(which.min(x))


as.Date.default <- function (x, ...) 
{
    if (inherits(x, "Date")) 
        x
    else if (is.logical(x) && all(is.na(x))) 
        .Date(as.numeric(x))
    else stop(gettextf("do not know how to convert '%s' to class %s", 
        deparse(substitute(x)), dQuote("Date")), domain = NA)
}


Sys.glob <- function (paths, dirmark = FALSE) 
.Internal(Sys.glob(path.expand(paths), dirmark))


Sys.getpid <- function () 
.Internal(Sys.getpid())


which.max <- function (x) 
.Internal(which.max(x))


pretty.default <- function (x, n = 5L, min.n = n%/%3L, shrink.sml = 0.75, high.u.bias = 1.5, 
    u5.bias = 0.5 + 1.5 * high.u.bias, eps.correct = 0L, ...) 
{
    x <- x[is.finite(x <- as.numeric(x))]
    if (!length(x)) 
        return(x)
    z <- .Internal(pretty(min(x), max(x), n, min.n, shrink.sml, 
        c(high.u.bias, u5.bias), eps.correct))
    s <- seq.int(z$l, z$u, length.out = z$n + 1L)
    if (!eps.correct && z$n) {
        delta <- diff(range(z$l, z$u))/z$n
        if (any(small <- abs(s) < 1e-14 * delta)) 
            s[small] <- 0
    }
    s
}


on.exit <- function (expr = NULL, add = FALSE, after = TRUE)  .Primitive("on.exit")


tcrossprod <- function (x, y = NULL) 
.Internal(tcrossprod(x, y))


.Date <- function (xx, cl = "Date") 
{
    class(xx) <- cl
    xx
}


crossprod <- function (x, y = NULL) 
.Internal(crossprod(x, y))


anyDuplicated.data.frame <- function (x, incomparables = FALSE, fromLast = FALSE, ...) 
{
    if (!isFALSE(incomparables)) 
        .NotYetUsed("incomparables != FALSE")
    anyDuplicated(do.call(Map, `names<-`(c(list, x), NULL)), 
        fromLast = fromLast)
}


abs <- function (x)  .Primitive("abs")


months.Date <- function (x, abbreviate = FALSE) 
format(x, ifelse(abbreviate, "%b", "%B"))


`-.POSIXt` <- function (e1, e2) 
{
    coerceTimeUnit <- function(x) as.vector(switch(attr(x, "units"), 
        secs = x, mins = 60 * x, hours = 60 * 60 * x, days = 60 * 
            60 * 24 * x, weeks = 60 * 60 * 24 * 7 * x))
    if (!inherits(e1, "POSIXt")) 
        stop("can only subtract from \"POSIXt\" objects")
    if (nargs() == 1L) 
        stop("unary '-' is not defined for \"POSIXt\" objects")
    if (inherits(e2, "POSIXt")) 
        return(difftime(e1, e2))
    if (inherits(e2, "difftime")) 
        e2 <- coerceTimeUnit(e2)
    if (!is.null(attr(e2, "class"))) 
        stop("can only subtract numbers from \"POSIXt\" objects")
    e1 <- as.POSIXct(e1)
    .POSIXct(unclass(e1) - e2, attr(e1, "tzone"))
}


stdin <- function () 
.Internal(stdin())


all <- function (..., na.rm = FALSE)  .Primitive("all")


as.numeric <- function (x, ...)  .Primitive("as.double")


any <- function (..., na.rm = FALSE)  .Primitive("any")


`Encoding<-` <- function (x, value) 
.Internal(setEncoding(x, value))


isTRUE <- function (x) 
is.logical(x) && length(x) == 1L && !is.na(x) && x


charToRaw <- function (x) 
.Internal(charToRaw(x))


Sys.time <- function () 
.POSIXct(.Internal(Sys.time()))


as.Date.numeric <- function (x, origin, ...) 
{
    if (missing(origin)) 
        stop("'origin' must be supplied")
    as.Date(origin, ...) + x
}


shell <- function (cmd, shell, flag = "/c", intern = FALSE, wait = TRUE, 
    translate = FALSE, mustWork = FALSE, ...) 
{
    if (missing(shell)) {
        shell <- Sys.getenv("R_SHELL")
        if (!nzchar(shell)) 
            shell <- Sys.getenv("COMSPEC")
    }
    if (missing(flag) && any(!is.na(pmatch(c("bash", "tcsh", 
        "sh"), basename(shell))))) 
        flag <- "-c"
    cmd0 <- cmd
    if (translate) 
        cmd <- chartr("/", "\\", cmd)
    if (!is.null(shell)) 
        cmd <- paste(shell, flag, cmd)
    res <- system(cmd, intern = intern, wait = wait | intern, 
        show.output.on.console = wait, ...)
    if (!intern && res && !is.na(mustWork)) 
        if (mustWork) 
            if (res == -1L) 
                stop(gettextf("'%s' could not be run", cmd0), 
                  domain = NA)
            else stop(gettextf("'%s' execution failed with error code %d", 
                cmd0, res), domain = NA)
        else if (res == -1L) 
            warning(gettextf("'%s' could not be run", cmd0), 
                domain = NA)
        else warning(gettextf("'%s' execution failed with error code %d", 
            cmd0, res), domain = NA)
    if (intern) 
        res
    else invisible(res)
}


environment <- function (fun = NULL) 
.Internal(environment(fun))


cat <- function (..., file = "", sep = " ", fill = FALSE, labels = NULL, 
    append = FALSE) 
{
    if (is.character(file)) 
        if (file == "") 
            file <- stdout()
        else if (substring(file, 1L, 1L) == "|") {
            file <- pipe(substring(file, 2L), "w")
            on.exit(close(file))
        }
        else {
            file <- file(file, ifelse(append, "a", "w"))
            on.exit(close(file))
        }
    .Internal(cat(list(...), file, sep, fill, labels, append))
}


is.raw <- function (x)  .Primitive("is.raw")


mean.difftime <- function (x, ...) 
.difftime(mean(unclass(x), ...), attr(x, "units"))


readLines <- function (con = stdin(), n = -1L, ok = TRUE, warn = TRUE, encoding = "unknown", 
    skipNul = FALSE) 
{
    if (is.character(con)) {
        con <- file(con, "r")
        on.exit(close(con))
    }
    .Internal(readLines(con, n, ok, warn, encoding, skipNul))
}


.rowSums <- function (x, m, n, na.rm = FALSE) 
.Internal(rowSums(x, m, n, na.rm))


charmatch <- function (x, table, nomatch = NA_integer_) 
.Internal(charmatch(as.character(x), as.character(table), nomatch))


col <- function (x, as.factor = FALSE) 
{
    if (as.factor) {
        labs <- colnames(x, do.NULL = FALSE, prefix = "")
        res <- factor(.Internal(col(dim(x))), labels = labs)
        dim(res) <- dim(x)
        res
    }
    else .Internal(col(dim(x)))
}


cos <- function (x)  .Primitive("cos")


Sys.Date <- function () 
as.Date(as.POSIXlt(Sys.time()))


det <- function (x, ...) 
{
    z <- determinant(x, logarithm = TRUE, ...)
    c(z$sign * exp(z$modulus))
}


cut <- function (x, ...) 
UseMethod("cut")


dim <- function (x)  .Primitive("dim")


dir <- function (path = ".", pattern = NULL, all.files = FALSE, full.names = FALSE, 
    recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE, 
    no.. = FALSE) 
.Internal(list.files(path, pattern, all.files, full.names, recursive, 
    ignore.case, include.dirs, no..))


as.data.frame.raw <- function (x, row.names = NULL, optional = FALSE, ..., nm = paste(deparse(substitute(x), 
    width.cutoff = 500L), collapse = " ")) 
{
    force(nm)
    nrows <- length(x)
    if (!(is.null(row.names) || (is.character(row.names) && length(row.names) == 
        nrows))) {
        warning(gettextf("'row.names' is not a character vector of length %d -- omitting it. Will be an error!", 
            nrows), domain = NA)
        row.names <- NULL
    }
    if (is.null(row.names)) {
        if (nrows == 0L) 
            row.names <- character()
        else if (length(row.names <- names(x)) != nrows || anyDuplicated(row.names)) 
            row.names <- .set_row_names(nrows)
    }
    if (!is.null(names(x))) 
        names(x) <- NULL
    value <- list(x)
    if (!optional) 
        names(value) <- nm
    structure(value, row.names = row.names, class = "data.frame")
}


.methodsNamespace <- methods::.methodsNamespace # re-exported from methods package

as.symbol <- function (x) 
.Internal(as.vector(x, "symbol"))


`[[.factor` <- function (x, ...) 
{
    y <- NextMethod("[[")
    attr(y, "contrasts") <- attr(x, "contrasts")
    attr(y, "levels") <- attr(x, "levels")
    class(y) <- oldClass(x)
    y
}


as.data.frame.AsIs <- function (x, row.names = NULL, optional = FALSE, ...) 
{
    if (length(dim(x)) == 2L) 
        as.data.frame.model.matrix(x, row.names, optional)
    else {
        nrows <- length(x)
        nm <- paste(deparse(substitute(x), width.cutoff = 500L), 
            collapse = " ")
        if (is.null(row.names)) {
            autoRN <- FALSE
            if (nrows == 0L) 
                row.names <- character()
            else if (length(row.names <- names(x)) == nrows && 
                !anyDuplicated(row.names)) {
            }
            else {
                autoRN <- TRUE
                row.names <- .set_row_names(nrows)
            }
        }
        else autoRN <- is.integer(row.names) && length(row.names) == 
            2L && is.na(rn1 <- row.names[[1L]]) && rn1 < 0
        value <- list(x)
        if (!optional) 
            names(value) <- nm
        class(value) <- "data.frame"
        attr(value, "row.names") <- row.names
        value
    }
}


summary.factor <- function (object, maxsum = 100L, ...) 
{
    nas <- is.na(object)
    ll <- levels(object)
    if (ana <- any(nas)) 
        maxsum <- maxsum - 1L
    tbl <- table(object)
    tt <- c(tbl)
    names(tt) <- dimnames(tbl)[[1L]]
    if (length(ll) > maxsum) {
        drop <- maxsum:length(ll)
        o <- sort.list(tt, decreasing = TRUE)
        tt <- c(tt[o[-drop]], `(Other)` = sum(tt[o[drop]]))
    }
    if (ana) 
        c(tt, `NA's` = sum(nas))
    else tt
}


anyDuplicated.matrix <- function (x, incomparables = FALSE, MARGIN = 1L, fromLast = FALSE, 
    ...) 
{
    if (!isFALSE(incomparables)) 
        .NotYetUsed("incomparables != FALSE")
    dx <- dim(x)
    ndim <- length(dx)
    if (any(MARGIN > ndim)) 
        stop(gettextf("MARGIN = %s is invalid for dim = %s", 
            paste(MARGIN, collapse = ","), paste(dx, collapse = ",")), 
            domain = NA)
    temp <- if ((ndim > 1L) && (prod(dx[-MARGIN]) > 1L)) 
        asplit(x, MARGIN)
    else x
    anyDuplicated.default(temp, fromLast = fromLast)
}


getDLLRegisteredRoutines.DLLInfo <- function (dll, addNames = TRUE) 
{
    if (!inherits(dll, "DLLInfo")) 
        stop(gettextf("must specify DLL via a %s object. See getLoadedDLLs()", 
            dQuote("DLLInfo")), domain = NA)
    info <- dll[["info"]]
    els <- .Internal(getRegisteredRoutines(info))
    if (addNames) {
        els <- lapply(els, function(x) {
            if (length(x)) 
                names(x) <- vapply(x, function(z) z$name, "")
            x
        })
    }
    class(els) <- "DLLRegisteredRoutines"
    els
}


exp <- function (x)  .Primitive("exp")


print.Date <- function (x, max = NULL, ...) 
{
    if (is.null(max)) 
        max <- getOption("max.print", 9999L)
    if (max < length(x)) {
        print(format(x[seq_len(max)]), max = max + 1, ...)
        cat(" [ reached 'max' / getOption(\"max.print\") -- omitted", 
            length(x) - max, "entries ]\n")
    }
    else if (length(x)) 
        print(format(x), max = max, ...)
    else cat(class(x)[1L], "of length 0\n")
    invisible(x)
}


split.data.frame <- function (x, f, drop = FALSE, ...) 
lapply(split(x = seq_len(nrow(x)), f = f, drop = drop, ...), 
    function(ind) x[ind, , drop = FALSE])


rowSums <- function (x, na.rm = FALSE, dims = 1L) 
{
    if (is.data.frame(x)) 
        x <- as.matrix(x)
    if (!is.array(x) || length(dn <- dim(x)) < 2L) 
        stop("'x' must be an array of at least two dimensions")
    if (dims < 1L || dims > length(dn) - 1L) 
        stop("invalid 'dims'")
    p <- prod(dn[-(id <- seq_len(dims))])
    dn <- dn[id]
    z <- if (is.complex(x)) 
        .Internal(rowSums(Re(x), prod(dn), p, na.rm)) + (0+1i) * 
            .Internal(rowSums(Im(x), prod(dn), p, na.rm))
    else .Internal(rowSums(x, prod(dn), p, na.rm))
    if (length(dn) > 1L) {
        dim(z) <- dn
        dimnames(z) <- dimnames(x)[id]
    }
    else names(z) <- dimnames(x)[[1L]]
    z
}


rawShift <- function (x, n) 
.Internal(rawShift(x, n))


get <- function (x, pos = -1L, envir = as.environment(pos), mode = "any", 
    inherits = TRUE) 
.Internal(get(x, envir, mode, inherits))


open.srcfilealias <- function (con, line, ...) 
open(con$original, line, ...)


as.list.numeric_version <- function (x, ...) 
{
    nms <- names(x)
    names(x) <- NULL
    y <- lapply(seq_along(x), function(i) x[i])
    names(y) <- nms
    y
}


identity <- function (x) 
x


Summary.factor <- function (..., na.rm) 
stop(gettextf("%s not meaningful for factors", sQuote(.Generic)))


print.summaryDefault <- function (x, digits = max(3L, getOption("digits") - 3L), ...) 
{
    xx <- x
    if (is.numeric(x) || is.complex(x)) {
        finite <- is.finite(x)
        xx[finite] <- zapsmall(x[finite])
    }
    class(xx) <- class(x)[-1]
    m <- match("NA's", names(xx), 0)
    if (inherits(x, "Date") || inherits(x, "POSIXct")) {
        xx <- if (length(a <- attr(x, "NAs"))) 
            c(format(xx, digits = digits), `NA's` = as.character(a))
        else format(xx, digits = digits)
        print(xx, digits = digits, ...)
        return(invisible(x))
    }
    else if (m && !is.character(x)) 
        xx <- c(format(xx[-m], digits = digits), `NA's` = as.character(xx[m]))
    print.table(xx, digits = digits, ...)
    invisible(x)
}


toString.default <- function (x, width = NULL, ...) 
{
    string <- paste(x, collapse = ", ")
    if (missing(width) || is.null(width) || width == 0) 
        return(string)
    if (width < 0) 
        stop("'width' must be positive")
    if (nchar(string, type = "w") > width) {
        width <- max(6, width)
        string <- paste0(strtrim(string, width - 4), "....")
    }
    string
}


pushBackLength <- function (connection) 
.Internal(pushBackLength(connection))


extSoftVersion <- function () 
.Internal(eSoftVersion())


serialize <- function (object, connection, ascii = FALSE, xdr = TRUE, version = NULL, 
    refhook = NULL) 
{
    if (!is.null(connection)) {
        if (!inherits(connection, "connection")) 
            stop("'connection' must be a connection")
        if (missing(ascii)) 
            ascii <- summary(connection)$text == "text"
    }
    if (!ascii && inherits(connection, "sockconn")) 
        .Internal(serializeb(object, connection, xdr, version, 
            refhook))
    else {
        type <- if (is.na(ascii)) 
            2L
        else if (ascii) 
            1L
        else if (!xdr) 
            3L
        else 0L
        .Internal(serialize(object, connection, type, version, 
            refhook))
    }
}


is.complex <- function (x)  .Primitive("is.complex")


loadNamespace <- function (package, lib.loc = NULL, keep.source = getOption("keep.source.pkgs"), 
    partial = FALSE, versionCheck = NULL, keep.parse.data = getOption("keep.parse.data.pkgs")) 
{
    libpath <- attr(package, "LibPath")
    package <- as.character(package)[[1L]]
    loading <- dynGet("__NameSpacesLoading__", NULL)
    if (match(package, loading, 0L)) 
        stop("cyclic namespace dependency detected when loading ", 
            sQuote(package), ", already loading ", paste(sQuote(loading), 
                collapse = ", "), domain = NA)
    "__NameSpacesLoading__" <- c(package, loading)
    ns <- .Internal(getRegisteredNamespace(package))
    if (!is.null(ns)) {
        if (!is.null(zop <- versionCheck[["op"]]) && !is.null(zversion <- versionCheck[["version"]])) {
            current <- getNamespaceVersion(ns)
            if (!do.call(zop, list(as.numeric_version(current), 
                zversion))) 
                stop(gettextf("namespace %s %s is already loaded, but %s %s is required", 
                  sQuote(package), current, zop, zversion), domain = NA)
        }
        ns
    }
    else {
        runHook <- function(hookname, env, libname, pkgname) {
            if (!is.null(fun <- env[[hookname]])) {
                res <- tryCatch(fun(libname, pkgname), error = identity)
                if (inherits(res, "error")) {
                  stop(gettextf("%s failed in %s() for '%s', details:\n  call: %s\n  error: %s", 
                    hookname, "loadNamespace", pkgname, deparse(conditionCall(res))[1L], 
                    conditionMessage(res)), call. = FALSE, domain = NA)
                }
            }
        }
        runUserHook <- function(pkgname, pkgpath) {
            hooks <- getHook(packageEvent(pkgname, "onLoad"))
            for (fun in hooks) try(fun(pkgname, pkgpath))
        }
        makeNamespace <- function(name, version = NULL, lib = NULL) {
            impenv <- new.env(parent = .BaseNamespaceEnv, hash = TRUE)
            attr(impenv, "name") <- paste0("imports:", name)
            env <- new.env(parent = impenv, hash = TRUE)
            name <- as.character(as.name(name))
            version <- as.character(version)
            info <- new.env(hash = TRUE, parent = baseenv())
            env$.__NAMESPACE__. <- info
            info$spec <- c(name = name, version = version)
            setNamespaceInfo(env, "exports", new.env(hash = TRUE, 
                parent = baseenv()))
            dimpenv <- new.env(parent = baseenv(), hash = TRUE)
            attr(dimpenv, "name") <- paste0("lazydata:", name)
            setNamespaceInfo(env, "lazydata", dimpenv)
            setNamespaceInfo(env, "imports", list(base = TRUE))
            setNamespaceInfo(env, "path", normalizePath(file.path(lib, 
                name), "/", TRUE))
            setNamespaceInfo(env, "dynlibs", NULL)
            setNamespaceInfo(env, "S3methods", matrix(NA_character_, 
                0L, 4L))
            env$.__S3MethodsTable__. <- new.env(hash = TRUE, 
                parent = baseenv())
            .Internal(registerNamespace(name, env))
            env
        }
        sealNamespace <- function(ns) {
            namespaceIsSealed <- function(ns) environmentIsLocked(ns)
            ns <- asNamespace(ns, base.OK = FALSE)
            if (namespaceIsSealed(ns)) 
                stop(gettextf("namespace %s is already sealed in 'loadNamespace'", 
                  sQuote(getNamespaceName(ns))), call. = FALSE, 
                  domain = NA)
            lockEnvironment(ns, TRUE)
            lockEnvironment(parent.env(ns), TRUE)
        }
        addNamespaceDynLibs <- function(ns, newlibs) {
            dynlibs <- .getNamespaceInfo(ns, "dynlibs")
            setNamespaceInfo(ns, "dynlibs", c(dynlibs, newlibs))
        }
        bindTranslations <- function(pkgname, pkgpath) {
            std <- c("compiler", "foreign", "grDevices", "graphics", 
                "grid", "methods", "parallel", "splines", "stats", 
                "stats4", "tcltk", "tools", "utils")
            popath <- if (pkgname %in% std) 
                .popath
            else file.path(pkgpath, "po")
            if (!file.exists(popath)) 
                return()
            bindtextdomain(pkgname, popath)
            bindtextdomain(paste0("R-", pkgname), popath)
        }
        assignNativeRoutines <- function(dll, lib, env, nativeRoutines) {
            if (length(nativeRoutines) == 0L) 
                return(character())
            varnames <- character()
            symnames <- character()
            if (nativeRoutines$useRegistration) {
                fixes <- nativeRoutines$registrationFixes
                routines <- getDLLRegisteredRoutines.DLLInfo(dll, 
                  addNames = FALSE)
                lapply(routines, function(type) {
                  lapply(type, function(sym) {
                    varName <- paste0(fixes[1L], sym$name, fixes[2L])
                    if (exists(varName, envir = env, inherits = FALSE)) 
                      warning(gettextf("failed to assign RegisteredNativeSymbol for %s to %s since %s is already defined in the %s namespace", 
                        sym$name, varName, varName, sQuote(package)), 
                        domain = NA, call. = FALSE)
                    else {
                      env[[varName]] <- sym
                      varnames <<- c(varnames, varName)
                      symnames <<- c(symnames, sym$name)
                    }
                  })
                })
            }
            symNames <- nativeRoutines$symbolNames
            if (length(symNames)) {
                symbols <- getNativeSymbolInfo(symNames, dll, 
                  unlist = FALSE, withRegistrationInfo = TRUE)
                lapply(seq_along(symNames), function(i) {
                  varName <- names(symNames)[i]
                  origVarName <- symNames[i]
                  if (exists(varName, envir = env, inherits = FALSE)) 
                    if (origVarName != varName) 
                      warning(gettextf("failed to assign NativeSymbolInfo for %s to %s since %s is already defined in the %s namespace", 
                        origVarName, varName, varName, sQuote(package)), 
                        domain = NA, call. = FALSE)
                    else warning(gettextf("failed to assign NativeSymbolInfo for %s since %s is already defined in the %s namespace", 
                      origVarName, varName, sQuote(package)), 
                      domain = NA, call. = FALSE)
                  else {
                    assign(varName, symbols[[origVarName]], envir = env)
                    varnames <<- c(varnames, varName)
                    symnames <<- c(symnames, origVarName)
                  }
                })
            }
            names(symnames) <- varnames
            symnames
        }
        fp.lib.loc <- c(libpath, lib.loc)
        pkgpath <- find.package(package, fp.lib.loc, quiet = TRUE)
        if (length(pkgpath) == 0L) {
            cond <- packageNotFoundError(package, fp.lib.loc, 
                sys.call())
            withRestarts(stop(cond), retry_loadNamespace = function() NULL)
            pkgpath <- find.package(package, fp.lib.loc, quiet = TRUE)
            if (length(pkgpath) == 0L) 
                stop(cond)
        }
        bindTranslations(package, pkgpath)
        package.lib <- dirname(pkgpath)
        package <- basename(pkgpath)
        if (!packageHasNamespace(package, package.lib)) {
            hasNoNamespaceError <- function(package, package.lib, 
                call = NULL) {
                class <- c("hasNoNamespaceError", "error", "condition")
                msg <- gettextf("package %s does not have a namespace", 
                  sQuote(package))
                structure(list(message = msg, package = package, 
                  package.lib = package.lib, call = call), class = class)
            }
            stop(hasNoNamespaceError(package, package.lib))
        }
        nsInfoFilePath <- file.path(pkgpath, "Meta", "nsInfo.rds")
        nsInfo <- if (file.exists(nsInfoFilePath)) 
            readRDS(nsInfoFilePath)
        else parseNamespaceFile(package, package.lib, mustExist = FALSE)
        pkgInfoFP <- file.path(pkgpath, "Meta", "package.rds")
        if (file.exists(pkgInfoFP)) {
            pkgInfo <- readRDS(pkgInfoFP)
            version <- pkgInfo$DESCRIPTION["Version"]
            vI <- pkgInfo$Imports
            if (is.null(built <- pkgInfo$Built)) 
                stop(gettextf("package %s has not been installed properly\n", 
                  sQuote(basename(pkgpath))), call. = FALSE, 
                  domain = NA)
            R_version_built_under <- as.numeric_version(built$R)
            if (R_version_built_under < "3.0.0") 
                stop(gettextf("package %s was built before R 3.0.0: please re-install it", 
                  sQuote(basename(pkgpath))), call. = FALSE, 
                  domain = NA)
            dependsMethods <- "methods" %in% names(pkgInfo$Depends)
            if (dependsMethods) 
                loadNamespace("methods")
            if (!is.null(zop <- versionCheck[["op"]]) && !is.null(zversion <- versionCheck[["version"]]) && 
                !do.call(zop, list(as.numeric_version(version), 
                  zversion))) 
                stop(gettextf("namespace %s %s is being loaded, but %s %s is required", 
                  sQuote(package), version, zop, zversion), domain = NA)
        }
        checkLicense <- function(pkg, pkgInfo, pkgPath) {
            L <- tools:::analyze_license(pkgInfo$DESCRIPTION["License"])
            if (!L$is_empty && !L$is_verified) {
                site_file <- path.expand(file.path(R.home("etc"), 
                  "licensed.site"))
                if (file.exists(site_file) && pkg %in% readLines(site_file)) 
                  return()
                personal_file <- path.expand("~/.R/licensed")
                if (file.exists(personal_file)) {
                  agreed <- readLines(personal_file)
                  if (pkg %in% agreed) 
                    return()
                }
                else agreed <- character()
                if (!interactive()) 
                  stop(gettextf("package %s has a license that you need to accept in an interactive session", 
                    sQuote(pkg)), domain = NA)
                lfiles <- file.path(pkgpath, c("LICENSE", "LICENCE"))
                lfiles <- lfiles[file.exists(lfiles)]
                if (length(lfiles)) {
                  message(gettextf("package %s has a license that you need to accept after viewing", 
                    sQuote(pkg)), domain = NA)
                  readline("press RETURN to view license")
                  encoding <- pkgInfo$DESCRIPTION["Encoding"]
                  if (is.na(encoding)) 
                    encoding <- ""
                  if (encoding == "latin1") 
                    encoding <- "cp1252"
                  file.show(lfiles[1L], encoding = encoding)
                }
                else {
                  message(gettextf(paste("package %s has a license that you need to accept:", 
                    "according to the DESCRIPTION file it is", 
                    "%s", sep = "\n"), sQuote(pkg), pkgInfo$DESCRIPTION["License"]), 
                    domain = NA)
                }
                choice <- utils::menu(c("accept", "decline"), 
                  title = paste("License for", sQuote(pkg)))
                if (choice != 1) 
                  stop(gettextf("license for package %s not accepted", 
                    sQuote(package)), domain = NA, call. = FALSE)
                dir.create(dirname(personal_file), showWarnings = FALSE)
                writeLines(c(agreed, pkg), personal_file)
            }
        }
        if (!package %in% c("datasets", "grDevices", "graphics", 
            "methods", "stats", "tools", "utils") && isTRUE(getOption("checkPackageLicense", 
            FALSE))) 
            checkLicense(package, pkgInfo, pkgpath)
        if (dir.exists(file.path(pkgpath, "Meta"))) {
            ffile <- file.path(pkgpath, "Meta", "features.rds")
            features <- if (file.exists(ffile)) 
                readRDS(ffile)
            else NULL
            needsComp <- as.character(pkgInfo$DESCRIPTION["NeedsCompilation"])
            if (identical(needsComp, "yes") || file.exists(file.path(pkgpath, 
                "libs"))) {
                internalsID <- features$internalsID
                if (is.null(internalsID)) 
                  internalsID <- "0310d4b8-ccb1-4bb8-ba94-d36a55f60262"
                if (internalsID != .Internal(internalsID())) 
                  stop(gettextf("package %s was installed by an R version with different internals; it needs to be reinstalled for use with this R version", 
                    sQuote(package)), call. = FALSE, domain = NA)
            }
        }
        ns <- makeNamespace(package, version = version, lib = package.lib)
        on.exit(.Internal(unregisterNamespace(package)))
        for (i in nsInfo$imports) {
            if (is.character(i)) 
                namespaceImport(ns, loadNamespace(i, c(lib.loc, 
                  .libPaths()), versionCheck = vI[[i]]), from = package)
            else if (!is.null(i$except)) 
                namespaceImport(ns, loadNamespace(j <- i[[1L]], 
                  c(lib.loc, .libPaths()), versionCheck = vI[[j]]), 
                  from = package, except = i$except)
            else namespaceImportFrom(ns, loadNamespace(j <- i[[1L]], 
                c(lib.loc, .libPaths()), versionCheck = vI[[j]]), 
                i[[2L]], from = package)
        }
        for (imp in nsInfo$importClasses) namespaceImportClasses(ns, 
            loadNamespace(j <- imp[[1L]], c(lib.loc, .libPaths()), 
                versionCheck = vI[[j]]), imp[[2L]], from = package)
        for (imp in nsInfo$importMethods) namespaceImportMethods(ns, 
            loadNamespace(j <- imp[[1L]], c(lib.loc, .libPaths()), 
                versionCheck = vI[[j]]), imp[[2L]], from = package)
        "__LoadingNamespaceInfo__" <- list(libname = package.lib, 
            pkgname = package)
        env <- asNamespace(ns)
        env$.packageName <- package
        codename <- strsplit(package, "_", fixed = TRUE)[[1L]][1L]
        codeFile <- file.path(pkgpath, "R", codename)
        if (file.exists(codeFile)) {
            save.enc <- options(encoding = "native.enc")
            res <- try(sys.source(codeFile, env, keep.source = keep.source, 
                keep.parse.data = keep.parse.data))
            options(save.enc)
            if (inherits(res, "try-error")) 
                stop(gettextf("unable to load R code in package %s", 
                  sQuote(package)), call. = FALSE, domain = NA)
        }
        if (partial) 
            return(ns)
        dbbase <- file.path(pkgpath, "R", "sysdata")
        if (file.exists(paste0(dbbase, ".rdb"))) 
            lazyLoad(dbbase, env)
        dbbase <- file.path(pkgpath, "data", "Rdata")
        if (file.exists(paste0(dbbase, ".rdb"))) 
            lazyLoad(dbbase, .getNamespaceInfo(env, "lazydata"))
        registerS3methods(nsInfo$S3methods, package, env)
        dlls <- list()
        dynLibs <- nsInfo$dynlibs
        nativeRoutines <- list()
        for (i in seq_along(dynLibs)) {
            lib <- dynLibs[i]
            dlls[[lib]] <- library.dynam(lib, package, package.lib)
            routines <- assignNativeRoutines(dlls[[lib]], lib, 
                env, nsInfo$nativeRoutines[[lib]])
            nativeRoutines[[lib]] <- routines
            if (!is.null(names(nsInfo$dynlibs)) && nzchar(names(nsInfo$dynlibs)[i])) 
                env[[names(nsInfo$dynlibs)[i]]] <- dlls[[lib]]
            setNamespaceInfo(env, "DLLs", dlls)
        }
        addNamespaceDynLibs(env, nsInfo$dynlibs)
        setNamespaceInfo(env, "nativeRoutines", nativeRoutines)
        Sys.setenv(`_R_NS_LOAD_` = package)
        on.exit(Sys.unsetenv("_R_NS_LOAD_"), add = TRUE)
        runHook(".onLoad", env, package.lib, package)
        exports <- nsInfo$exports
        for (p in nsInfo$exportPatterns) exports <- c(ls(env, 
            pattern = p, all.names = TRUE), exports)
        if (.isMethodsDispatchOn() && methods:::.hasS4MetaData(ns) && 
            !identical(package, "methods")) {
            methods::cacheMetaData(ns, TRUE, ns)
            for (p in nsInfo$exportPatterns) {
                expp <- ls(ns, pattern = p, all.names = TRUE)
                newEx <- !(expp %in% exports)
                if (any(newEx)) 
                  exports <- c(expp[newEx], exports)
            }
            expClasses <- nsInfo$exportClasses
            pClasses <- character()
            aClasses <- methods::getClasses(ns)
            classPatterns <- nsInfo$exportClassPatterns
            if (!length(classPatterns)) 
                classPatterns <- nsInfo$exportPatterns
            pClasses <- unique(unlist(lapply(classPatterns, grep, 
                aClasses, value = TRUE)))
            if (length(pClasses)) {
                good <- vapply(pClasses, methods::isClass, NA, 
                  where = ns)
                if (!any(good) && length(nsInfo$exportClassPatterns)) 
                  warning(gettextf("'exportClassPattern' specified in 'NAMESPACE' but no matching classes in package %s", 
                    sQuote(package)), call. = FALSE, domain = NA)
                expClasses <- c(expClasses, pClasses[good])
            }
            if (length(expClasses)) {
                missingClasses <- !vapply(expClasses, methods::isClass, 
                  NA, where = ns)
                if (any(missingClasses)) 
                  stop(gettextf("in package %s classes %s were specified for export but not defined", 
                    sQuote(package), paste(expClasses[missingClasses], 
                      collapse = ", ")), domain = NA)
                expClasses <- paste0(methods::classMetaName(""), 
                  expClasses)
            }
            allGenerics <- unique(c(methods:::.getGenerics(ns), 
                methods:::.getGenerics(parent.env(ns))))
            expMethods <- nsInfo$exportMethods
            addGenerics <- expMethods[is.na(match(expMethods, 
                exports))]
            if (length(addGenerics)) {
                nowhere <- vapply(addGenerics, function(what) !exists(what, 
                  mode = "function", envir = ns), NA, USE.NAMES = FALSE)
                if (any(nowhere)) {
                  warning(gettextf("no function found corresponding to methods exports from %s for: %s", 
                    sQuote(package), paste(sQuote(sort(unique(addGenerics[nowhere]))), 
                      collapse = ", ")), domain = NA, call. = FALSE)
                  addGenerics <- addGenerics[!nowhere]
                }
                if (length(addGenerics)) {
                  addGenerics <- addGenerics[vapply(addGenerics, 
                    function(what) !is.primitive(get(what, mode = "function", 
                      envir = ns)), NA)]
                  ok <- vapply(addGenerics, methods:::.findsGeneric, 
                    1L, ns)
                  if (!all(ok)) {
                    bad <- sort(unique(addGenerics[!ok]))
                    msg <- ngettext(length(bad), "Function found when exporting methods from the namespace %s which is not S4 generic: %s", 
                      "Functions found when exporting methods from the namespace %s which are not S4 generic: %s")
                    stop(sprintf(msg, sQuote(package), paste(sQuote(bad), 
                      collapse = ", ")), domain = NA, call. = FALSE)
                  }
                  else if (any(ok > 1L)) 
                    addGenerics <- addGenerics[ok < 2L]
                }
                exports <- c(exports, addGenerics)
            }
            expTables <- character()
            if (length(allGenerics)) {
                expMethods <- unique(c(expMethods, exports[!is.na(match(exports, 
                  allGenerics))]))
                missingMethods <- !(expMethods %in% allGenerics)
                if (any(missingMethods)) 
                  stop(gettextf("in %s methods for export not found: %s", 
                    sQuote(package), paste(expMethods[missingMethods], 
                      collapse = ", ")), domain = NA)
                tPrefix <- methods:::.TableMetaPrefix()
                allMethodTables <- unique(c(methods:::.getGenerics(ns, 
                  tPrefix), methods:::.getGenerics(parent.env(ns), 
                  tPrefix)))
                needMethods <- (exports %in% allGenerics) & !(exports %in% 
                  expMethods)
                if (any(needMethods)) 
                  expMethods <- c(expMethods, exports[needMethods])
                pm <- allGenerics[!(allGenerics %in% expMethods)]
                if (length(pm)) {
                  prim <- vapply(pm, function(pmi) {
                    f <- methods::getFunction(pmi, FALSE, FALSE, 
                      ns)
                    is.primitive(f)
                  }, logical(1L))
                  expMethods <- c(expMethods, pm[prim])
                }
                for (i in seq_along(expMethods)) {
                  mi <- expMethods[[i]]
                  if (!(mi %in% exports) && exists(mi, envir = ns, 
                    mode = "function", inherits = FALSE)) 
                    exports <- c(exports, mi)
                  pattern <- paste0(tPrefix, mi, ":")
                  ii <- grep(pattern, allMethodTables, fixed = TRUE)
                  if (length(ii)) {
                    if (length(ii) > 1L) {
                      warning(gettextf("multiple methods tables found for %s", 
                        sQuote(mi)), call. = FALSE, domain = NA)
                      ii <- ii[1L]
                    }
                    expTables[[i]] <- allMethodTables[ii]
                  }
                  else {
                    warning(gettextf("failed to find metadata object for %s", 
                      sQuote(mi)), call. = FALSE, domain = NA)
                  }
                }
            }
            else if (length(expMethods)) 
                stop(gettextf("in package %s methods %s were specified for export but not defined", 
                  sQuote(package), paste(expMethods, collapse = ", ")), 
                  domain = NA)
            exports <- unique(c(exports, expClasses, expTables))
        }
        if (length(exports)) {
            stoplist <- c(".__NAMESPACE__.", ".__S3MethodsTable__.", 
                ".packageName", ".First.lib", ".onLoad", ".onAttach", 
                ".conflicts.OK", ".noGenerics")
            exports <- exports[!exports %in% stoplist]
        }
        namespaceExport(ns, exports)
        sealNamespace(ns)
        runUserHook(package, pkgpath)
        on.exit()
        Sys.unsetenv("_R_NS_LOAD_")
        ns
    }
}


all.equal.default <- function (target, current, ...) 
{
    if (is.language(target) || is.function(target)) 
        return(all.equal.language(target, current, ...))
    if (is.environment(target) || is.environment(current)) 
        return(all.equal.environment(target, current, ...))
    if (is.recursive(target)) 
        return(all.equal.list(target, current, ...))
    msg <- switch(mode(target), integer = , complex = , numeric = all.equal.numeric(target, 
        current, ...), character = all.equal.character(target, 
        current, ...), logical = , raw = all.equal.raw(target, 
        current, ...), S4 = attr.all.equal(target, current, ...), 
        if (data.class(target) != data.class(current)) {
            gettextf("target is %s, current is %s", data.class(target), 
                data.class(current))
        } else NULL)
    if (is.null(msg)) 
        TRUE
    else msg
}


print.condition <- function (x, ...) 
{
    msg <- conditionMessage(x)
    call <- conditionCall(x)
    cl <- class(x)[1L]
    if (!is.null(call)) 
        cat("<", cl, " in ", deparse(call), ": ", msg, ">\n", 
            sep = "")
    else cat("<", cl, ": ", msg, ">\n", sep = "")
    invisible(x)
}


RNGkind <- function (kind = NULL, normal.kind = NULL, sample.kind = NULL) 
{
    kinds <- c("Wichmann-Hill", "Marsaglia-Multicarry", "Super-Duper", 
        "Mersenne-Twister", "Knuth-TAOCP", "user-supplied", "Knuth-TAOCP-2002", 
        "L'Ecuyer-CMRG", "default")
    n.kinds <- c("Buggy Kinderman-Ramage", "Ahrens-Dieter", "Box-Muller", 
        "user-supplied", "Inversion", "Kinderman-Ramage", "default")
    s.kinds <- c("Rounding", "Rejection", "default")
    do.set <- length(kind) > 0L
    if (do.set) {
        if (!is.character(kind) || length(kind) > 1L) 
            stop("'kind' must be a character string of length 1 (RNG to be used).")
        if (is.na(i.knd <- pmatch(kind, kinds) - 1L)) 
            stop(gettextf("'%s' is not a valid abbreviation of an RNG", 
                kind), domain = NA)
        if (i.knd == length(kinds) - 1L) 
            i.knd <- -1L
    }
    else i.knd <- NULL
    if (!is.null(normal.kind)) {
        if (!is.character(normal.kind) || length(normal.kind) != 
            1L) 
            stop("'normal.kind' must be a character string of length 1")
        normal.kind <- pmatch(normal.kind, n.kinds) - 1L
        if (is.na(normal.kind)) 
            stop(gettextf("'%s' is not a valid choice", normal.kind), 
                domain = NA)
        if (normal.kind == 0L) 
            warning("buggy version of Kinderman-Ramage generator used", 
                domain = NA)
        if (normal.kind == length(n.kinds) - 1L) 
            normal.kind <- -1L
    }
    if (!is.null(sample.kind)) {
        if (!is.character(sample.kind) || length(sample.kind) != 
            1L) 
            stop("'sample.kind' must be a character string of length 1")
        sample.kind <- pmatch(sample.kind, s.kinds) - 1L
        if (is.na(sample.kind)) 
            stop(gettextf("'%s' is not a valid choice", sample.kind), 
                domain = NA)
        if (sample.kind == 0L) 
            warning("non-uniform 'Rounding' sampler used", domain = NA)
        if (sample.kind == length(s.kinds) - 1L) 
            sample.kind <- -1L
    }
    r <- 1L + .Internal(RNGkind(i.knd, normal.kind, sample.kind))
    r <- c(kinds[r[1L]], n.kinds[r[2L]], s.kinds[r[3L]])
    if (do.set || !is.null(normal.kind) || !is.null(sample.kind)) 
        invisible(r)
    else r
}


summary.table <- function (object, ...) 
{
    if (!inherits(object, "table")) 
        stop(gettextf("'object' must inherit from class %s", 
            dQuote("table")), domain = NA)
    n.cases <- sum(object)
    n.vars <- length(dim(object))
    y <- list(n.vars = n.vars, n.cases = n.cases)
    if (n.vars > 1) {
        m <- vector("list", length = n.vars)
        relFreqs <- object/n.cases
        for (k in 1L:n.vars) m[[k]] <- apply(relFreqs, k, sum)
        expected <- apply(do.call("expand.grid", m), 1L, prod) * 
            n.cases
        statistic <- sum((c(object) - expected)^2/expected)
        lm <- lengths(m)
        parameter <- prod(lm) - 1L - sum(lm - 1L)
        y <- c(y, list(statistic = statistic, parameter = parameter, 
            approx.ok = all(expected >= 5), p.value = stats::pchisq(statistic, 
                parameter, lower.tail = FALSE), call = attr(object, 
                "call")))
    }
    class(y) <- "summary.table"
    y
}


list <- function (...)  .Primitive("list")


print.difftime <- function (x, digits = getOption("digits"), ...) 
{
    if (is.array(x) || length(x) > 1L) {
        cat("Time differences in ", attr(x, "units"), "\n", sep = "")
        y <- unclass(x)
        attr(y, "units") <- NULL
        print(y, digits = digits, ...)
    }
    else cat("Time difference of ", format(unclass(x), digits = digits), 
        " ", attr(x, "units"), "\n", sep = "")
    invisible(x)
}


as.character.error <- function (x, ...) 
{
    msg <- conditionMessage(x)
    call <- conditionCall(x)
    if (!is.null(call)) 
        paste0("Error in ", deparse(call)[1L], ": ", msg, "\n")
    else paste0("Error: ", msg, "\n")
}


strtrim <- function (x, width) 
{
    if (!is.character(x)) 
        x <- as.character(x)
    .Internal(strtrim(x, width))
}


licence <- function () 
{
    cat("\nThis software is distributed under the terms of the GNU General\n")
    cat("Public License, either Version 2, June 1991 or Version 3, June 2007.\n")
    cat("The terms of version 2 of the license are in a file called COPYING\nwhich you should have received with\n")
    cat("this software and which can be displayed by RShowDoc(\"COPYING\").\n")
    cat("Version 3 of the license can be displayed by RShowDoc(\"GPL-3\").\n")
    cat("\n")
    cat("Copies of both versions 2 and 3 of the license can be found\n")
    cat("at https://www.R-project.org/Licenses/.\n")
    cat("\n")
    cat("A small number of files (the API header files listed in\n")
    cat("R_DOC_DIR/COPYRIGHTS) are distributed under the\n")
    cat("LESSER GNU GENERAL PUBLIC LICENSE, version 2.1 or later.\n")
    cat("This can be displayed by RShowDoc(\"LGPL-2.1\"),\n")
    cat("or obtained at the URI given.\n")
    cat("Version 3 of the license can be displayed by RShowDoc(\"LGPL-3\").\n")
    cat("\n")
    cat("'Share and Enjoy.'\n\n")
}


summary <- function (object, ...) 
UseMethod("summary")


row.names.data.frame <- function (x) 
as.character(attr(x, "row.names"))


packageStartupMessage <- function (..., domain = NULL, appendLF = TRUE) 
{
    call <- sys.call()
    msg <- .makeMessage(..., domain = domain, appendLF = appendLF)
    message(.packageStartupMessage(msg, call))
}


print.summary.warnings <- function (x, ...) 
{
    n <- length(x)
    cn <- attr(x, "counts")
    if (n == 0) 
        cat("No warnings\n")
    else if (n == 1) 
        print.warnings(x, header = paste(sum(cn), "identical warnings:\n"))
    else print.warnings(x, tags = paste0(format(cn), "x : "), 
        header = gettextf("Summary of (a total of %d) warning messages:\n", 
            sum(cn)))
    invisible(x)
}


pmax.int <- function (..., na.rm = FALSE) 
.Internal(pmax(na.rm, ...))


julian.Date <- function (x, origin = as.Date("1970-01-01"), ...) 
{
    if (length(origin) != 1L) 
        stop("'origin' must be of length one")
    structure(unclass(x) - unclass(origin), origin = origin)
}


`rownames<-` <- function (x, value) 
{
    if (is.data.frame(x)) {
        row.names(x) <- value
    }
    else {
        dn <- dimnames(x)
        if (is.null(dn)) {
            if (is.null(value)) 
                return(x)
            if ((nd <- length(dim(x))) < 1L) 
                stop("attempt to set 'rownames' on an object with no dimensions")
            dn <- vector("list", nd)
        }
        if (length(dn) < 1L) 
            stop("attempt to set 'rownames' on an object with no dimensions")
        if (is.null(value)) 
            dn[1L] <- list(NULL)
        else dn[[1L]] <- value
        dimnames(x) <- dn
    }
    x
}


license <- function () 
{
    cat("\nThis software is distributed under the terms of the GNU General\n")
    cat("Public License, either Version 2, June 1991 or Version 3, June 2007.\n")
    cat("The terms of version 2 of the license are in a file called COPYING\nwhich you should have received with\n")
    cat("this software and which can be displayed by RShowDoc(\"COPYING\").\n")
    cat("Version 3 of the license can be displayed by RShowDoc(\"GPL-3\").\n")
    cat("\n")
    cat("Copies of both versions 2 and 3 of the license can be found\n")
    cat("at https://www.R-project.org/Licenses/.\n")
    cat("\n")
    cat("A small number of files (the API header files listed in\n")
    cat("R_DOC_DIR/COPYRIGHTS) are distributed under the\n")
    cat("LESSER GNU GENERAL PUBLIC LICENSE, version 2.1 or later.\n")
    cat("This can be displayed by RShowDoc(\"LGPL-2.1\"),\n")
    cat("or obtained at the URI given.\n")
    cat("Version 3 of the license can be displayed by RShowDoc(\"LGPL-3\").\n")
    cat("\n")
    cat("'Share and Enjoy.'\n\n")
}


sys.save.image <- function (name) 
{
    closeAllConnections()
    save.image(name)
}


lazyLoadDBexec <- function (filebase, fun, filter) 
{
    glue <- function(..., sep = " ", collapse = NULL) .Internal(paste(list(...), 
        sep, collapse))
    readRDS <- function(file) {
        halt <- function(message) .Internal(stop(TRUE, message))
        gzfile <- function(description, open) .Internal(gzfile(description, 
            open, "", 6))
        close <- function(con) .Internal(close(con, "rw"))
        if (!is.character(file)) 
            halt("bad file name")
        con <- gzfile(file, "rb")
        on.exit(close(con))
        .Internal(unserializeFromConn(con, baseenv()))
    }
    `parent.env<-` <- function(env, value) .Internal(`parent.env<-`(env, 
        value))
    existsInFrame <- function(x, env) .Internal(exists(x, env, 
        "any", FALSE))
    environment <- function() .Internal(environment(NULL))
    mkenv <- function() .Internal(new.env(TRUE, baseenv(), 29L))
    mapfile <- glue(filebase, "rdx", sep = ".")
    datafile <- glue(filebase, "rdb", sep = ".")
    env <- mkenv()
    map <- readRDS(mapfile)
    vars <- names(map$variables)
    compressed <- map$compressed
    list2env(map$references, env)
    envenv <- mkenv()
    envhook <- function(n) {
        if (existsInFrame(n, envenv)) 
            envenv[[n]]
        else {
            e <- mkenv()
            envenv[[n]] <- e
            key <- env[[n]]
            ekey <- if (is.list(key)) 
                key$eagerKey
            else key
            data <- lazyLoadDBfetch(ekey, datafile, compressed, 
                envhook)
            parent.env(e) <- if (!is.null(data$enclos)) 
                data$enclos
            else emptyenv()
            list2env(data$bindings, e)
            if (!is.null(data$attributes)) 
                attributes(e) <- data$attributes
            if (!is.null(data$isS4) && data$isS4) 
                .Internal(setS4Object(e, TRUE, TRUE))
            if (is.list(key)) {
                expr <- quote(lazyLoadDBfetch(KEY, datafile, 
                  compressed, envhook))
                .Internal(makeLazy(names(key$lazyKeys), key$lazyKeys, 
                  expr, parent.env(environment()), e))
            }
            if (!is.null(data$locked) && data$locked) 
                .Internal(lockEnvironment(e, FALSE))
            e
        }
    }
    if (!missing(filter)) {
        use <- filter(vars)
        vars <- vars[use]
        vals <- map$variables[use]
        use <- NULL
    }
    else vals <- map$variables
    res <- fun(environment())
    map <- NULL
    vars <- NULL
    vals <- NULL
    rvars <- NULL
    mapfile <- NULL
    readRDS <- NULL
    res
}


max.col <- function (m, ties.method = c("random", "first", "last")) 
{
    ties.method <- match.arg(ties.method)
    tieM <- which(ties.method == eval(formals()[["ties.method"]]))
    .Internal(max.col(as.matrix(m), tieM))
}


invisible <- function (x)  .Primitive("invisible")


open.srcfile <- function (con, line, ...) 
{
    srcfile <- con
    oldline <- srcfile$line
    if (!is.null(oldline) && oldline > line) 
        close(srcfile)
    conn <- srcfile$conn
    if (is.null(conn)) {
        if (!is.null(srcfile$wd)) {
            olddir <- setwd(srcfile$wd)
            on.exit(setwd(olddir))
        }
        timestamp <- file.mtime(srcfile$filename)
        if (!is.null(srcfile$timestamp) && !is.na(srcfile$timestamp) && 
            (is.na(timestamp) || timestamp != srcfile$timestamp)) 
            warning(gettextf("Timestamp of %s has changed", sQuote(srcfile$filename)), 
                call. = FALSE, domain = NA)
        if (is.null(srcfile$encoding)) 
            encoding <- getOption("encoding")
        else encoding <- srcfile$encoding
        srcfile$conn <- conn <- file(srcfile$filename, open = "rt", 
            encoding = encoding)
        srcfile$line <- 1L
        oldline <- 1L
    }
    else if (!isOpen(conn)) {
        open(conn, open = "rt")
        srcfile$line <- 1
        oldline <- 1L
    }
    if (oldline < line) {
        readLines(conn, line - oldline, warn = FALSE)
        srcfile$line <- line
    }
    invisible(conn)
}


searchpaths <- function () 
{
    s <- search()
    paths <- lapply(seq_along(s), function(i) attr(as.environment(i), 
        "path"))
    paths[[length(s)]] <- system.file()
    m <- grep("^package:", s)
    if (length(m)) 
        paths[-m] <- as.list(s[-m])
    unlist(paths)
}


namespaceImportFrom <- function (self, ns, vars, generics, packages, from = "non-package environment", 
    except = character(0L)) 
{
    addImports <- function(ns, from, what) {
        imp <- structure(list(what), names = getNamespaceName(from))
        imports <- getNamespaceImports(ns)
        setNamespaceInfo(ns, "imports", c(imports, imp))
    }
    namespaceIsSealed <- function(ns) environmentIsLocked(ns)
    makeImportExportNames <- function(spec) {
        old <- as.character(spec)
        new <- names(spec)
        if (is.null(new)) 
            new <- old
        else {
            change <- !nzchar(new)
            new[change] <- old[change]
        }
        names(old) <- new
        old
    }
    whichMethodMetaNames <- function(impvars) {
        if (!.isMethodsDispatchOn()) 
            return(numeric())
        seq_along(impvars)[startsWith(impvars, ".__T__")]
    }
    genericPackage <- function(f) {
        if (methods::is(f, "genericFunction")) 
            f@package
        else if (is.primitive(f)) 
            "base"
        else "<unknown>"
    }
    if (is.character(self)) 
        self <- getNamespace(self)
    ns <- asNamespace(ns)
    nsname <- getNamespaceName(ns)
    impvars <- if (missing(vars)) {
        stoplist <- c(".__NAMESPACE__.", ".__S3MethodsTable__.", 
            ".packageName", ".First.lib", ".Last.lib", ".onLoad", 
            ".onAttach", ".onDetach", ".conflicts.OK", ".noGenerics")
        vars <- getNamespaceExports(ns)
        vars <- vars[!vars %in% stoplist]
    }
    else vars
    impvars <- impvars[!impvars %in% except]
    impvars <- makeImportExportNames(impvars)
    impnames <- names(impvars)
    if (anyDuplicated(impnames)) {
        stop(gettextf("duplicate import names %s", paste(sQuote(impnames[duplicated(impnames)]), 
            collapse = ", ")), domain = NA)
    }
    if (isNamespace(self)) {
        if (isBaseNamespace(self)) {
            impenv <- self
            msg <- gettext("replacing local value with import %s when loading %s")
            register <- FALSE
        }
        else {
            if (namespaceIsSealed(self)) 
                stop("cannot import into a sealed namespace")
            impenv <- parent.env(self)
            msg <- gettext("replacing previous import by %s when loading %s")
            register <- TRUE
        }
    }
    else if (is.environment(self)) {
        impenv <- self
        msg <- gettext("replacing local value with import %s when loading %s")
        register <- FALSE
    }
    else stop("invalid import target")
    which <- whichMethodMetaNames(impvars)
    if (length(which)) {
        delete <- integer()
        for (i in which) {
            methodsTable <- .mergeImportMethods(impenv, ns, impvars[[i]])
            if (is.null(methodsTable)) {
            }
            else {
                delete <- c(delete, i)
                if (!missing(generics)) {
                  genName <- generics[[i]]
                  fdef <- methods::getGeneric(genName, where = impenv, 
                    package = packages[[i]])
                  if (is.null(fdef)) 
                    warning(gettextf("found methods to import for function %s but not the generic itself", 
                      sQuote(genName)), call. = FALSE, domain = NA)
                  else methods:::.updateMethodsInTable(fdef, 
                    ns, TRUE)
                }
            }
        }
        if (length(delete)) {
            impvars <- impvars[-delete]
            impnames <- impnames[-delete]
        }
    }
    for (n in impnames) if (!is.null(genImp <- impenv[[n]])) {
        if (.isMethodsDispatchOn() && methods::isGeneric(n, ns)) {
            genNs <- genericPackage(get(n, envir = ns))
            if (identical(genNs, genericPackage(genImp))) 
                next
            genImpenv <- environmentName(environment(genImp))
            if (!identical(genNs, genImpenv) || methods::isGeneric(n, 
                impenv)) {
            }
            else next
        }
        if (identical(genImp, get(n, ns))) 
            next
        if (isNamespace(self) && !isBaseNamespace(self)) {
            current <- getNamespaceInfo(self, "imports")
            poss <- lapply(rev(current), "[", n)
            poss <- poss[!sapply(poss, is.na)]
            if (length(poss) >= 1L) {
                msg <- gettext("replacing previous import %s by %s when loading %s")
                prev <- names(poss)[1L]
                warning(sprintf(msg, sQuote(paste(prev, n, sep = "::")), 
                  sQuote(paste(nsname, n, sep = "::")), sQuote(from)), 
                  call. = FALSE, domain = NA)
            }
            else warning(sprintf(msg, sQuote(paste(nsname, n, 
                sep = "::")), sQuote(from)), call. = FALSE, domain = NA)
        }
        else {
            warning(sprintf(msg, sQuote(paste(nsname, n, sep = "::")), 
                sQuote(from)), call. = FALSE, domain = NA)
        }
    }
    importIntoEnv(impenv, impnames, ns, impvars)
    if (register) 
        addImports(self, ns, if (missing(vars)) 
            TRUE
        else impvars)
}


sys.frames <- function () 
.Internal(sys.frames())


Recall <- function (...) 
.Internal(Recall(...))


.difftime <- function (xx, units, cl = "difftime") 
{
    class(xx) <- cl
    attr(xx, "units") <- units
    xx
}


log <- function (x, base = exp(1))  .Primitive("log")


rawToBits <- function (x) 
.Internal(rawToBits(x))


max <- function (..., na.rm = FALSE)  .Primitive("max")


floor <- function (x)  .Primitive("floor")


Ops.numeric_version <- function (e1, e2) 
{
    if (nargs() == 1L) 
        stop(gettextf("unary '%s' not defined for \"numeric_version\" objects", 
            .Generic), domain = NA)
    boolean <- switch(.Generic, `<` = , `>` = , `==` = , `!=` = , 
        `<=` = , `>=` = TRUE, FALSE)
    if (!boolean) 
        stop(gettextf("'%s' not defined for \"numeric_version\" objects", 
            .Generic), domain = NA)
    if (!is.numeric_version(e1)) 
        e1 <- as.numeric_version(e1)
    if (!is.numeric_version(e2)) 
        e2 <- as.numeric_version(e2)
    n1 <- length(e1)
    n2 <- length(e2)
    if (!n1 || !n2) 
        return(logical())
    e <- split(.encode_numeric_version(c(e1, e2)), rep.int(c(1L, 
        2L), c(n1, n2)))
    e1 <- e[[1L]]
    e2 <- e[[2L]]
    NextMethod(.Generic)
}


close <- function (con, ...) 
UseMethod("close")


Math.Date <- function (x, ...) 
stop(gettextf("%s not defined for \"Date\" objects", .Generic), 
    domain = NA)


`dimnames<-.data.frame` <- function (x, value) 
{
    d <- dim(x)
    if (!is.list(value) || length(value) != 2L) 
        stop("invalid 'dimnames' given for data frame")
    value[[1L]] <- as.character(value[[1L]])
    value[[2L]] <- as.character(value[[2L]])
    if (d[[1L]] != length(value[[1L]]) || d[[2L]] != length(value[[2L]])) 
        stop("invalid 'dimnames' given for data frame")
    row.names(x) <- value[[1L]]
    names(x) <- value[[2L]]
    x
}


Math.data.frame <- function (x, ...) 
{
    mode.ok <- vapply(x, function(x) is.numeric(x) || is.complex(x), 
        NA)
    if (all(mode.ok)) {
        x[] <- lapply(X = x, FUN = .Generic, ...)
        return(x)
    }
    else {
        vnames <- names(x)
        if (is.null(vnames)) 
            vnames <- seq_along(x)
        stop("non-numeric variable(s) in data frame: ", paste(vnames[!mode.ok], 
            collapse = ", "))
    }
}


min <- function (..., na.rm = FALSE)  .Primitive("min")


as.array <- function (x, ...) 
UseMethod("as.array")


assign <- function (x, value, pos = -1, envir = as.environment(pos), inherits = FALSE, 
    immediate = TRUE) 
.Internal(assign(x, value, envir, inherits))


taskCallbackManager <- function (handlers = list(), registered = FALSE, verbose = FALSE) 
{
    suspended <- FALSE
    .verbose <- verbose
    add <- function(f, data = NULL, name = NULL, register = TRUE) {
        if (is.null(name)) 
            name <- as.character(length(handlers) + 1L)
        handlers[[name]] <<- list(f = f)
        if (!missing(data)) 
            handlers[[name]][["data"]] <<- data
        if (!registered && register) {
            register()
        }
        name
    }
    remove <- function(which) {
        if (length(which) != 1L) 
            stop("'which' must be of length 1")
        if (is.character(which)) {
            tmp <- match(which, names(handlers))
            if (is.na(tmp)) 
                stop(gettextf("no such element '%s'", which), 
                  domain = NA)
            which <- tmp
        }
        else if (is.numeric(which)) {
            which <- as.integer(which)
            if (which <= 0 || which > length(handlers)) 
                stop("invalid 'which' argument")
        }
        else stop("'which' must be character or numeric")
        handlers <<- handlers[-which]
        return(TRUE)
    }
    evaluate <- function(expr, value, ok, visible) {
        if (suspended) 
            return(TRUE)
        discard <- character()
        for (i in names(handlers)) {
            h <- handlers[[i]]
            if (length(h) > 1L) {
                val <- h[["f"]](expr, value, ok, visible, h[["data"]])
            }
            else {
                val <- h[["f"]](expr, value, ok, visible)
            }
            if (!val) {
                discard <- c(discard, i)
            }
        }
        if (length(discard)) {
            if (.verbose) 
                cat(gettextf("Removing %s", paste(discard, collapse = ", ")), 
                  "\n")
            idx <- is.na(match(names(handlers), discard))
            if (length(idx)) 
                handlers <<- handlers[idx]
            else handlers <<- list()
        }
        return(TRUE)
    }
    suspend <- function(status = TRUE) {
        suspended <<- status
    }
    register <- function(name = "R-taskCallbackManager", verbose = .verbose) {
        if (verbose) 
            cat(gettext("Registering 'evaluate' as low-level callback\n"))
        id <- addTaskCallback(evaluate, name = name)
        registered <<- TRUE
        id
    }
    list(add = add, evaluate = evaluate, remove = remove, register = register, 
        suspend = suspend, callbacks = function() handlers)
}


gettextf <- function (fmt, ..., domain = NULL) 
sprintf(gettext(fmt, domain = domain), ...)


.NotYetUsed <- function (arg, error = TRUE) 
{
    msg <- gettextf("argument '%s' is not used (yet)", arg)
    if (error) 
        stop(msg, domain = NA, call. = FALSE)
    else warning(msg, domain = NA, call. = FALSE)
}


setHook <- function (hookName, value, action = c("append", "prepend", "replace")) 
{
    action <- match.arg(action)
    old <- getHook(hookName)
    new <- switch(action, append = c(old, value), prepend = c(value, 
        old), replace = if (is.null(value) || is.list(value)) value else list(value))
    if (length(new)) 
        assign(hookName, new, envir = .userHooksEnv, inherits = FALSE)
    else if (exists(hookName, envir = .userHooksEnv, inherits = FALSE)) 
        remove(list = hookName, envir = .userHooksEnv, inherits = FALSE)
    invisible()
}


load <- function (file, envir = parent.frame(), verbose = FALSE) 
{
    if (is.character(file)) {
        con <- gzfile(file)
        on.exit(close(con))
        magic <- readChar(con, 5L, useBytes = TRUE)
        if (!length(magic)) 
            stop("empty (zero-byte) input file")
        if (!grepl("RD[ABX][2-9]\n", magic)) {
            if (grepl("RD[ABX][2-9]\r", magic)) 
                stop("input has been corrupted, with LF replaced by CR")
            warning(sprintf("file %s has magic number '%s'\n", 
                sQuote(basename(file)), gsub("[\n\r]*", "", magic)), 
                "  ", "Use of save versions prior to 2 is deprecated", 
                domain = NA, call. = FALSE)
            return(.Internal(load(file, envir)))
        }
    }
    else if (inherits(file, "connection")) {
        con <- if (inherits(file, "gzfile") || inherits(file, 
            "gzcon")) 
            file
        else gzcon(file)
    }
    else stop("bad 'file' argument")
    if (verbose) 
        cat("Loading objects:\n")
    .Internal(loadFromConn2(con, envir, verbose))
}


Negate <- function (f) 
{
    f <- match.fun(f)
    function(...) !f(...)
}


unique.matrix <- function (x, incomparables = FALSE, MARGIN = 1, fromLast = FALSE, 
    ...) 
{
    if (!isFALSE(incomparables)) 
        .NotYetUsed("incomparables != FALSE")
    dx <- dim(x)
    ndim <- length(dx)
    if (length(MARGIN) != 1L || (MARGIN > ndim)) 
        stop(gettextf("MARGIN = %s is invalid for dim = %s", 
            paste(MARGIN, collapse = ","), paste(dx, collapse = ",")), 
            domain = NA)
    temp <- if ((ndim > 1L) && (prod(dx[-MARGIN]) > 1L)) 
        asplit(x, MARGIN)
    else x
    args <- rep(alist(a = ), ndim)
    names(args) <- NULL
    args[[MARGIN]] <- !duplicated.default(temp, fromLast = fromLast, 
        ...)
    do.call("[", c(list(x), args, list(drop = FALSE)))
}


log2 <- function (x)  .Primitive("log2")


as.character.POSIXt <- function (x, ...) 
format(x, ...)


untrace <- function (what, signature = NULL, where = topenv(parent.frame())) 
{
    if (!.isMethodsDispatchOn()) 
        return(.primUntrace(what))
    tState <- tracingState(FALSE)
    on.exit(tracingState(tState))
    call <- sys.call()
    call[[1L]] <- quote(methods::.TraceWithMethods)
    call$where <- where
    call$untrace <- TRUE
    invisible(eval.parent(call))
}


logb <- function (x, base = exp(1)) 
if (missing(base)) log(x) else log(x, base)


is.array <- function (x)  .Primitive("is.array")


droplevels.factor <- function (x, exclude = if (anyNA(levels(x))) NULL else NA, ...) 
factor(x, exclude = exclude)


R.version <- structure(list(platform = "x86_64-w64-mingw32", arch = "x86_64", 
    os = "mingw32", system = "x86_64, mingw32", status = "", 
    major = "3", minor = "6.0", year = "2019", month = "04", 
    day = "26", `svn rev` = "76424", language = "R", version.string = "R version 3.6.0 (2019-04-26)", 
    nickname = "Planting of a Tree"), class = "simple.list")


R.Version <- function () 
.Internal(Version())


within.list <- function (data, expr, keepAttrs = TRUE, ...) 
{
    parent <- parent.frame()
    e <- evalq(environment(), data, parent)
    eval(substitute(expr), e)
    if (keepAttrs) {
        l <- as.list(e, all.names = TRUE)
        del <- setdiff(names(data), (nl <- names(l)))
        data[nl] <- l
        data[del] <- NULL
        data
    }
    else {
        as.list(e, all.names = TRUE)
    }
}


inherits <- function (x, what, which = FALSE) 
.Internal(inherits(x, what, which))


Sys.setenv <- function (...) 
{
    x <- list(...)
    nm <- names(x)
    if (is.null(nm) || "" %in% nm) 
        stop("all arguments must be named")
    .Internal(Sys.setenv(nm, as.character(unlist(x))))
}


comment <- function (x) 
.Internal(comment(x))


print.DLLInfoList <- function (x, ...) 
{
    if (length(x)) {
        m <- data.frame(Filename = sapply(x, function(x) x[["path"]]), 
            `Dynamic Lookup` = sapply(x, function(x) x[["dynamicLookup"]]))
        print(m, ...)
    }
    invisible(x)
}


vector <- function (mode = "logical", length = 0L) 
.Internal(vector(mode, length))


system <- function (command, intern = FALSE, ignore.stdout = FALSE, ignore.stderr = FALSE, 
    wait = TRUE, input = NULL, show.output.on.console = TRUE, 
    minimized = FALSE, invisible = TRUE, timeout = 0) 
{
    if (!is.logical(intern) || is.na(intern)) 
        stop("'intern' must be TRUE or FALSE")
    if (!is.logical(ignore.stdout) || is.na(ignore.stdout)) 
        stop("'ignore.stdout' must be TRUE or FALSE")
    if (!is.logical(ignore.stderr) || is.na(ignore.stderr)) 
        stop("'ignore.stderr' must be TRUE or FALSE")
    if (!is.logical(wait) || is.na(wait)) 
        stop("'wait' must be TRUE or FALSE")
    if (!is.logical(show.output.on.console) || is.na(show.output.on.console)) 
        stop("'show.output.on.console' must be TRUE or FALSE")
    if (!is.logical(minimized) || is.na(minimized)) 
        stop("'minimized' must be TRUE or FALSE")
    if (!is.logical(invisible) || is.na(invisible)) 
        stop("'invisible' must be TRUE or FALSE")
    stdout <- ifelse(ignore.stdout, FALSE, "")
    stderr <- ifelse(ignore.stderr, FALSE, "")
    f <- ""
    if (!is.null(input)) {
        f <- tempfile()
        on.exit(unlink(f))
        writeLines(input, f)
    }
    internNothing <- FALSE
    if (intern) {
        flag <- 3L
        if (stdout == "") 
            stdout <- TRUE
        if (!ignore.stderr && .Platform$GUI == "Rgui") 
            stderr <- TRUE
        if (identical(stdout, FALSE) && (identical(stderr, FALSE) || 
            .Platform$GUI != "Rgui")) {
            flag <- 1L
            internNothing <- TRUE
        }
    }
    else {
        flag <- if (wait) {
            if (show.output.on.console && (!ignore.stderr || 
                !ignore.stdout)) 
                2L
            else 1L
        }
        else 0L
    }
    if (invisible) 
        flag <- 20L + flag
    else if (minimized) 
        flag <- 10L + flag
    if (.fixupGFortranStdout()) 
        on.exit(Sys.setenv(GFORTRAN_STDOUT_UNIT = "-1"), add = TRUE)
    if (.fixupGFortranStderr()) 
        on.exit(Sys.setenv(GFORTRAN_STDERR_UNIT = "-1"), add = TRUE)
    rval <- .Internal(system(command, as.integer(flag), f, stdout, 
        stderr, timeout))
    if (!internNothing) 
        rval
    else {
        ans <- character(0)
        if (is.numeric(rval) && length(rval) > 0 && rval != 0) {
            rval <- as.integer(rval)
            attr(ans, "status") <- rval
            warning(gettextf("running command '%s' had status %d", 
                command, rval), domain = NA)
        }
        ans
    }
}


.First.sys <- function () 
{
    for (pkg in getOption("defaultPackages")) {
        res <- require(pkg, quietly = TRUE, warn.conflicts = FALSE, 
            character.only = TRUE)
        if (!res) 
            warning(gettextf("package %s in options(\"defaultPackages\") was not found", 
                sQuote(pkg)), call. = FALSE, domain = NA)
    }
}


save.image <- function (file = ".RData", version = NULL, ascii = FALSE, compress = !ascii, 
    safe = TRUE) 
{
    if (!is.character(file) || file == "") 
        stop("'file' must be non-empty string")
    opts <- getOption("save.image.defaults")
    if (is.null(opts)) 
        opts <- getOption("save.defaults")
    if (missing(safe) && !is.null(opts$safe)) 
        safe <- opts$safe
    if (missing(ascii) && !is.null(opts$ascii)) 
        ascii <- opts$ascii
    if (missing(compress) && !is.null(opts$compress)) 
        compress <- opts$compress
    if (missing(version)) 
        version <- opts$version
    if (safe) {
        outfile <- paste0(file, "Tmp")
        i <- 0
        while (file.exists(outfile)) {
            i <- i + 1
            outfile <- paste0(file, "Tmp", i)
        }
    }
    else outfile <- file
    on.exit(file.remove(outfile))
    save(list = names(.GlobalEnv), file = outfile, version = version, 
        ascii = ascii, compress = compress, envir = .GlobalEnv, 
        precheck = FALSE)
    if (safe) 
        if (!file.rename(outfile, file)) {
            on.exit()
            stop(gettextf("image could not be renamed and is left in %s", 
                outfile), domain = NA)
        }
    on.exit()
}


`[[<-.data.frame` <- function (x, i, j, value) 
{
    if (!all(names(sys.call()) %in% c("", "value"))) 
        warning("named arguments are discouraged")
    cl <- oldClass(x)
    class(x) <- NULL
    nrows <- .row_names_info(x, 2L)
    if (is.atomic(value) && !is.null(names(value))) 
        names(value) <- NULL
    if (nargs() < 4L) {
        nc <- length(x)
        if (!is.null(value)) {
            N <- NROW(value)
            if (N > nrows) 
                stop(sprintf(ngettext(N, "replacement has %d row, data has %d", 
                  "replacement has %d rows, data has %d"), N, 
                  nrows), domain = NA)
            if (N < nrows) 
                if (N > 0L && (nrows%%N == 0L) && length(dim(value)) <= 
                  1L) 
                  value <- rep(value, length.out = nrows)
                else stop(sprintf(ngettext(N, "replacement has %d row, data has %d", 
                  "replacement has %d rows, data has %d"), N, 
                  nrows), domain = NA)
        }
        x[[i]] <- value
        if (length(x) > nc) {
            nc <- length(x)
            if (names(x)[nc] == "") 
                names(x)[nc] <- paste0("V", nc)
            names(x) <- make.unique(names(x))
        }
        class(x) <- cl
        return(x)
    }
    if (missing(i) || missing(j)) 
        stop("only valid calls are x[[j]] <- value or x[[i,j]] <- value")
    rows <- attr(x, "row.names")
    nvars <- length(x)
    if (n <- is.character(i)) {
        ii <- match(i, rows)
        n <- sum(new.rows <- is.na(ii))
        if (n > 0L) {
            ii[new.rows] <- seq.int(from = nrows + 1L, length.out = n)
            new.rows <- i[new.rows]
        }
        i <- ii
    }
    if (all(i >= 0L) && (nn <- max(i)) > nrows) {
        if (n == 0L) {
            nrr <- (nrows + 1L):nn
            if (inherits(value, "data.frame") && (dim(value)[1L]) >= 
                length(nrr)) {
                new.rows <- attr(value, "row.names")[seq_len(nrr)]
                repl <- duplicated(new.rows) | match(new.rows, 
                  rows, 0L)
                if (any(repl)) 
                  new.rows[repl] <- nrr[repl]
            }
            else new.rows <- nrr
        }
        x <- xpdrows.data.frame(x, rows, new.rows)
        rows <- attr(x, "row.names")
        nrows <- length(rows)
    }
    iseq <- seq_len(nrows)[i]
    if (anyNA(iseq)) 
        stop("non-existent rows not allowed")
    if (is.character(j)) {
        if ("" %in% j) 
            stop("column name \"\" cannot match any column")
        jseq <- match(j, names(x))
        if (anyNA(jseq)) 
            stop(gettextf("replacing element in non-existent column: %s", 
                j[is.na(jseq)]), domain = NA)
    }
    else if (is.logical(j) || min(j) < 0L) 
        jseq <- seq_along(x)[j]
    else {
        jseq <- j
        if (max(jseq) > nvars) 
            stop(gettextf("replacing element in non-existent column: %s", 
                jseq[jseq > nvars]), domain = NA)
    }
    if (length(iseq) > 1L || length(jseq) > 1L) 
        stop("only a single element should be replaced")
    x[[jseq]][[iseq]] <- value
    class(x) <- cl
    x
}


`row.names<-.default` <- function (x, value) 
`rownames<-`(x, value)


as.array.default <- function (x, ...) 
{
    if (is.array(x)) 
        return(x)
    n <- names(x)
    dim(x) <- length(x)
    if (length(n)) 
        dimnames(x) <- list(n)
    return(x)
}


format.info <- function (x, digits = NULL, nsmall = 0L) 
.Internal(format.info(x, digits, nsmall))


`|.hexmode` <- function (a, b) 
as.hexmode(bitwOr(as.hexmode(a), as.hexmode(b)))


.F_dqrqy <- structure(list(name = "dqrqy", address = pointer("0x000000000df13c10"), 
    dll = structure(list(name = "base", path = "base", dynamicLookup = FALSE, 
        handle = <pointer: (nil)>, info = pointer("0x000000000df73cc0")), class = "DLLInfo"), 
    numParameters = 7L), class = c("FortranRoutine", "NativeSymbolInfo"
))


Sys.setFileTime <- function (path, time) 
{
    if (!is.character(path)) 
        stop("invalid 'path' argument")
    time <- as.POSIXct(time)
    if (anyNA(time)) 
        stop("invalid 'time' argument")
    .Internal(setFileTime(path, time))
}


.F_dqrxb <- structure(list(name = "dqrxb", address = pointer("0x000000000df13bf0"), 
    dll = structure(list(name = "base", path = "base", dynamicLookup = FALSE, 
        handle = <pointer: (nil)>, info = pointer("0x000000000df73cc0")), class = "DLLInfo"), 
    numParameters = 7L), class = c("FortranRoutine", "NativeSymbolInfo"
))


setequal <- function (x, y) 
{
    x <- as.vector(x)
    y <- as.vector(y)
    !(anyNA(match(x, y)) || anyNA(match(y, x)))
}


raw <- function (length = 0L) 
.Internal(vector("raw", length))


typeof <- function (x) 
.Internal(typeof(x))


rep <- function (x, ...)  .Primitive("rep")


rev <- function (x) 
UseMethod("rev")


paste0 <- function (..., collapse = NULL) 
.Internal(paste0(list(...), collapse))


rle <- function (x) 
{
    if (!is.vector(x) && !is.list(x)) 
        stop("'x' must be a vector of an atomic type")
    n <- length(x)
    if (n == 0L) 
        return(structure(list(lengths = integer(), values = x), 
            class = "rle"))
    y <- x[-1L] != x[-n]
    i <- c(which(y | is.na(y)), n)
    structure(list(lengths = diff(c(0L, i)), values = x[i]), 
        class = "rle")
}


conditionMessage <- function (c) 
UseMethod("conditionMessage")


.F_dqrcf <- structure(list(name = "dqrcf", address = pointer("0x000000000df136b0"), 
    dll = structure(list(name = "base", path = "base", dynamicLookup = FALSE, 
        handle = <pointer: (nil)>, info = pointer("0x000000000df73cc0")), class = "DLLInfo"), 
    numParameters = 8L), class = c("FortranRoutine", "NativeSymbolInfo"
))


row <- function (x, as.factor = FALSE) 
{
    if (as.factor) {
        labs <- rownames(x, do.NULL = FALSE, prefix = "")
        res <- factor(.Internal(row(dim(x))), labels = labs)
        dim(res) <- dim(x)
        res
    }
    else .Internal(row(dim(x)))
}


stderr <- function () 
.Internal(stderr())


.Last.value <- function (symbol) 
{
    if (is_identifier(symbol)) {
        symbol
    }
    else {
        paste0("`", symbol, "`")
    }
}


seq <- function (...) 
UseMethod("seq")


flush <- function (con) 
UseMethod("flush")


sin <- function (x)  .Primitive("sin")


intToUtf8 <- function (x, multiple = FALSE, allow_surrogate_pairs = FALSE) 
.Internal(intToUtf8(x, multiple, allow_surrogate_pairs))


acos <- function (x)  .Primitive("acos")


within <- function (data, expr, ...) 
UseMethod("within")


tan <- function (x)  .Primitive("tan")


as.matrix.noquote <- function (x, ...) 
noquote(NextMethod("as.matrix", x))


libcurlVersion <- function () 
.Internal(curlVersion())


sub <- function (pattern, replacement, x, ignore.case = FALSE, perl = FALSE, 
    fixed = FALSE, useBytes = FALSE) 
{
    if (!is.character(x)) 
        x <- as.character(x)
    .Internal(sub(as.character(pattern), as.character(replacement), 
        x, ignore.case, perl, fixed, useBytes))
}


sum <- function (..., na.rm = FALSE)  .Primitive("sum")


svd <- function (x, nu = min(n, p), nv = min(n, p), LINPACK = FALSE) 
{
    x <- as.matrix(x)
    if (any(!is.finite(x))) 
        stop("infinite or missing values in 'x'")
    dx <- dim(x)
    n <- dx[1L]
    p <- dx[2L]
    if (!n || !p) 
        stop("a dimension is zero")
    La.res <- La.svd(x, nu, nv)
    res <- list(d = La.res$d)
    if (nu) 
        res$u <- La.res$u
    if (nv) {
        if (is.complex(x)) 
            res$v <- Conj(t(La.res$vt))
        else res$v <- t(La.res$vt)
    }
    res
}


upper.tri <- function (x, diag = FALSE) 
{
    d <- dim(x)
    if (length(d) != 2L) 
        d <- dim(as.matrix(x))
    if (diag) 
        .row(d) <= .col(d)
    else .row(d) < .col(d)
}


isSymmetric.matrix <- function (object, tol = 100 * .Machine$double.eps, tol1 = 8 * 
    tol, ...) 
{
    if (!is.matrix(object)) 
        return(FALSE)
    d <- dim(object)
    if ((n <- d[1L]) != d[2L]) 
        return(FALSE)
    iCplx <- is.complex(object)
    if (n > 1L && length(tol1)) {
        Cj <- if (iCplx) 
            Conj
        else identity
        for (i in unique(c(1L, 2L, n - 1L, n))) if (is.character(all.equal(object[i, 
            ], Cj(object[, i]), tolerance = tol1, ...))) 
            return(FALSE)
    }
    test <- if (iCplx) 
        all.equal.numeric(object, Conj(t(object)), tolerance = tol, 
            ...)
    else all.equal(object, t(object), tolerance = tol, ...)
    isTRUE(test)
}


is.factor <- function (x) 
inherits(x, "factor")


mean <- function (x, ...) 
UseMethod("mean")


try <- function (expr, silent = FALSE, outFile = getOption("try.outFile", 
    default = stderr())) 
{
    tryCatch(expr, error = function(e) {
        call <- conditionCall(e)
        if (!is.null(call)) {
            if (identical(call[[1L]], quote(doTryCatch))) 
                call <- sys.call(-4L)
            dcall <- deparse(call)[1L]
            prefix <- paste("Error in", dcall, ": ")
            LONG <- 75L
            sm <- strsplit(conditionMessage(e), "\n")[[1L]]
            w <- 14L + nchar(dcall, type = "w") + nchar(sm[1L], 
                type = "w")
            if (is.na(w)) 
                w <- 14L + nchar(dcall, type = "b") + nchar(sm[1L], 
                  type = "b")
            if (w > LONG) 
                prefix <- paste0(prefix, "\n  ")
        }
        else prefix <- "Error : "
        msg <- paste0(prefix, conditionMessage(e), "\n")
        .Internal(seterrmessage(msg[1L]))
        if (!silent && isTRUE(getOption("show.error.messages"))) {
            cat(msg, file = outFile)
            .Internal(printDeferredWarnings())
        }
        invisible(structure(msg, class = "try-error", condition = e))
    })
}


rev.default <- function (x) 
if (length(x)) x[length(x):1L] else x


is.character <- function (x)  .Primitive("is.character")


Sys.getenv <- function (x = NULL, unset = "", names = NA) 
{
    if (is.null(x)) {
        x <- strsplit(.Internal(Sys.getenv(character(), "")), 
            "=", fixed = TRUE)
        v <- n <- character(LEN <- length(x))
        for (i in 1L:LEN) {
            n[i] <- x[[i]][1L]
            v[i] <- paste(x[[i]][-1L], collapse = "=")
        }
        if (isFALSE(names)) 
            v[sort.list(n)]
        else {
            v <- structure(v, names = n)
            structure(class = "Dlist", v[sort.list(n)])
        }
    }
    else {
        v <- .Internal(Sys.getenv(as.character(x), as.character(unset)))
        if (isTRUE(names) || (length(x) > 1L && !isFALSE(names))) 
            structure(v, names = x)
        else v
    }
}


`!.octmode` <- function (a) 
as.octmode(bitwNot(as.octmode(a)))


unz <- function (description, filename, open = "", encoding = getOption("encoding")) 
.Internal(unz(paste(description, filename, sep = ":"), open, 
    encoding))


as.data.frame.factor <- function (x, row.names = NULL, optional = FALSE, ..., nm = paste(deparse(substitute(x), 
    width.cutoff = 500L), collapse = " ")) 
{
    force(nm)
    nrows <- length(x)
    if (!(is.null(row.names) || (is.character(row.names) && length(row.names) == 
        nrows))) {
        warning(gettextf("'row.names' is not a character vector of length %d -- omitting it. Will be an error!", 
            nrows), domain = NA)
        row.names <- NULL
    }
    if (is.null(row.names)) {
        if (nrows == 0L) 
            row.names <- character()
        else if (length(row.names <- names(x)) != nrows || anyDuplicated(row.names)) 
            row.names <- .set_row_names(nrows)
    }
    if (!is.null(names(x))) 
        names(x) <- NULL
    value <- list(x)
    if (!optional) 
        names(value) <- nm
    structure(value, row.names = row.names, class = "data.frame")
}


url <- function (description, open = "", blocking = TRUE, encoding = getOption("encoding"), 
    method = getOption("url.method", "default"), headers = NULL) 
{
    method <- match.arg(method, c("default", "internal", "libcurl", 
        "wininet"))
    if (!is.null(headers)) {
        nh <- names(headers)
        if (length(nh) != length(headers) || any(nh == "") || 
            anyNA(headers) || anyNA(nh)) 
            stop("'headers' must have names and must not be NA")
        headers <- paste0(nh, ": ", headers)
        headers <- list(headers, paste0(headers, "\r\n", collapse = ""))
    }
    .Internal(url(description, open, blocking, encoding, method, 
        headers))
}


Ops.Date <- function (e1, e2) 
{
    if (nargs() == 1L) 
        stop(gettextf("unary %s not defined for \"Date\" objects", 
            .Generic), domain = NA)
    boolean <- switch(.Generic, `<` = , `>` = , `==` = , `!=` = , 
        `<=` = , `>=` = TRUE, FALSE)
    if (!boolean) 
        stop(gettextf("%s not defined for \"Date\" objects", 
            .Generic), domain = NA)
    if (is.character(e1)) 
        e1 <- as.Date(e1)
    if (is.character(e2)) 
        e2 <- as.Date(e2)
    NextMethod(.Generic)
}


as.environment <- function (x)  .Primitive("as.environment")


`[.data.frame` <- function (x, i, j, drop = if (missing(i)) TRUE else length(cols) == 
    1) 
{
    mdrop <- missing(drop)
    Narg <- nargs() - !mdrop
    has.j <- !missing(j)
    if (!all(names(sys.call()) %in% c("", "drop")) && !isS4(x)) 
        warning("named arguments other than 'drop' are discouraged")
    if (Narg < 3L) {
        if (!mdrop) 
            warning("'drop' argument will be ignored")
        if (missing(i)) 
            return(x)
        if (is.matrix(i)) 
            return(as.matrix(x)[i])
        nm <- names(x)
        if (is.null(nm)) 
            nm <- character()
        if (!is.character(i) && anyNA(nm)) {
            names(nm) <- names(x) <- seq_along(x)
            y <- NextMethod("[")
            cols <- names(y)
            if (anyNA(cols)) 
                stop("undefined columns selected")
            cols <- names(y) <- nm[cols]
        }
        else {
            y <- NextMethod("[")
            cols <- names(y)
            if (!is.null(cols) && anyNA(cols)) 
                stop("undefined columns selected")
        }
        if (anyDuplicated(cols)) 
            names(y) <- make.unique(cols)
        attr(y, "row.names") <- .row_names_info(x, 0L)
        attr(y, "class") <- oldClass(x)
        return(y)
    }
    if (missing(i)) {
        if (drop && !has.j && length(x) == 1L) 
            return(.subset2(x, 1L))
        nm <- names(x)
        if (is.null(nm)) 
            nm <- character()
        if (has.j && !is.character(j) && anyNA(nm)) {
            names(nm) <- names(x) <- seq_along(x)
            y <- .subset(x, j)
            cols <- names(y)
            if (anyNA(cols)) 
                stop("undefined columns selected")
            cols <- names(y) <- nm[cols]
        }
        else {
            y <- if (has.j) 
                .subset(x, j)
            else x
            cols <- names(y)
            if (anyNA(cols)) 
                stop("undefined columns selected")
        }
        if (drop && length(y) == 1L) 
            return(.subset2(y, 1L))
        if (anyDuplicated(cols)) 
            names(y) <- make.unique(cols)
        nrow <- .row_names_info(x, 2L)
        if (drop && !mdrop && nrow == 1L) 
            return(structure(y, class = NULL, row.names = NULL))
        else {
            attr(y, "class") <- oldClass(x)
            attr(y, "row.names") <- .row_names_info(x, 0L)
            return(y)
        }
    }
    xx <- x
    cols <- names(xx)
    x <- vector("list", length(x))
    x <- .Internal(copyDFattr(xx, x))
    oldClass(x) <- attr(x, "row.names") <- NULL
    if (has.j) {
        nm <- names(x)
        if (is.null(nm)) 
            nm <- character()
        if (!is.character(j) && anyNA(nm)) 
            names(nm) <- names(x) <- seq_along(x)
        x <- x[j]
        cols <- names(x)
        if (drop && length(x) == 1L) {
            if (is.character(i)) {
                rows <- attr(xx, "row.names")
                i <- pmatch(i, rows, duplicates.ok = TRUE)
            }
            xj <- .subset2(.subset(xx, j), 1L)
            return(if (length(dim(xj)) != 2L) xj[i] else xj[i, 
                , drop = FALSE])
        }
        if (anyNA(cols)) 
            stop("undefined columns selected")
        if (!is.null(names(nm))) 
            cols <- names(x) <- nm[cols]
        nxx <- structure(seq_along(xx), names = names(xx))
        sxx <- match(nxx[j], seq_along(xx))
    }
    else sxx <- seq_along(x)
    rows <- NULL
    if (is.character(i)) {
        rows <- attr(xx, "row.names")
        i <- pmatch(i, rows, duplicates.ok = TRUE)
    }
    for (j in seq_along(x)) {
        xj <- xx[[sxx[j]]]
        x[[j]] <- if (length(dim(xj)) != 2L) 
            xj[i]
        else xj[i, , drop = FALSE]
    }
    if (drop) {
        n <- length(x)
        if (n == 1L) 
            return(x[[1L]])
        if (n > 1L) {
            xj <- x[[1L]]
            nrow <- if (length(dim(xj)) == 2L) 
                dim(xj)[1L]
            else length(xj)
            drop <- !mdrop && nrow == 1L
        }
        else drop <- FALSE
    }
    if (!drop) {
        if (is.null(rows)) 
            rows <- attr(xx, "row.names")
        rows <- rows[i]
        if ((ina <- anyNA(rows)) | (dup <- anyDuplicated(rows))) {
            if (!dup && is.character(rows)) 
                dup <- "NA" %in% rows
            if (ina) 
                rows[is.na(rows)] <- "NA"
            if (dup) 
                rows <- make.unique(as.character(rows))
        }
        if (has.j && anyDuplicated(nm <- names(x))) 
            names(x) <- make.unique(nm)
        if (is.null(rows)) 
            rows <- attr(xx, "row.names")[i]
        attr(x, "row.names") <- rows
        oldClass(x) <- oldClass(xx)
    }
    x
}


mget <- function (x, envir = as.environment(-1L), mode = "any", ifnotfound, 
    inherits = FALSE) 
.Internal(mget(x, envir, mode, if (missing(ifnotfound)) list(function(x) stop(gettextf("value for %s not found", 
    sQuote(x)), call. = FALSE)) else ifnotfound, inherits))


print.eigen <- function (x, ...) 
{
    cat("eigen() decomposition\n")
    print(unclass(x), ...)
    invisible(x)
}


invokeRestartInteractively <- function (r) 
{
    if (!interactive()) 
        stop("not an interactive session")
    if (!isRestart(r)) {
        res <- findRestart(r)
        if (is.null(res)) 
            stop(gettextf("no 'restart' '%s' found", as.character(r)), 
                domain = NA)
        r <- res
    }
    if (is.null(r$interactive)) {
        pars <- names(restartFormals(r))
        args <- NULL
        if (length(pars)) {
            cat("Enter values for restart arguments:\n\n")
            for (p in pars) {
                if (p == "...") {
                  prompt <- "... (a list): "
                  args <- c(args, eval(parse(prompt = prompt)))
                }
                else {
                  prompt <- paste0(p, ": ")
                  args <- c(args, list(eval(parse(prompt = prompt))))
                }
            }
        }
    }
    else args <- r$interactive()
    .Internal(.invokeRestart(r, args))
}


getNamespaceExports <- function (ns) 
{
    ns <- asNamespace(ns)
    names(if (isBaseNamespace(ns)) .BaseNamespaceEnv else .getNamespaceInfo(ns, 
        "exports"))
}


subset.matrix <- function (x, subset, select, drop = FALSE, ...) 
{
    if (missing(select)) 
        vars <- TRUE
    else {
        nl <- as.list(1L:ncol(x))
        names(nl) <- colnames(x)
        vars <- eval(substitute(select), nl, parent.frame())
    }
    if (missing(subset)) 
        subset <- TRUE
    else if (!is.logical(subset)) 
        stop("'subset' must be logical")
    x[subset & !is.na(subset), vars, drop = drop]
}


forwardsolve <- function (l, x, k = ncol(l), upper.tri = FALSE, transpose = FALSE) 
{
    l <- as.matrix(l)
    x.mat <- is.matrix(x)
    if (!x.mat) 
        x <- as.matrix(x)
    z <- .Internal(backsolve(l, x, k, upper.tri, transpose))
    if (x.mat) 
        z
    else drop(z)
}


`dim<-` <- function (x, value)  .Primitive("dim<-")


solve.default <- function (a, b, tol = .Machine$double.eps, LINPACK = FALSE, ...) 
{
    if (is.complex(a) || (!missing(b) && is.complex(b))) {
        a <- as.matrix(a)
        if (missing(b)) {
            b <- diag(1 + (0+0i), nrow(a))
            colnames(b) <- rownames(a)
        }
        return(.Internal(La_solve_cmplx(a, b)))
    }
    if (inherits(a, "qr")) {
        warning("solve.default called with a \"qr\" object: use 'qr.solve'")
        return(solve.qr(a, b, tol))
    }
    a <- as.matrix(a)
    if (missing(b)) {
        b <- diag(1, nrow(a))
        colnames(b) <- rownames(a)
    }
    .Internal(La_solve(a, b, tol))
}


icuGetCollate <- function (type = c("actual", "valid")) 
{
    type <- match.arg(type)
    .Internal(icuGetCollate(match(type, c("actual", "valid"))))
}


dirname <- function (path) 
.Internal(dirname(path))


as.data.frame.model.matrix <- function (x, row.names = NULL, optional = FALSE, make.names = TRUE, 
    ...) 
{
    d <- dim(x)
    nrows <- d[[1L]]
    dn <- dimnames(x)
    row.names <- dn[[1L]]
    value <- list(x)
    if (!optional) 
        names(value) <- deparse(substitute(x))[[1L]]
    class(value) <- "data.frame"
    if (!is.null(row.names)) {
        row.names <- as.character(row.names)
        if (length(row.names) != nrows) 
            stop(sprintf(ngettext(length(row.names), "supplied %d row name for %d rows", 
                "supplied %d row names for %d rows"), length(row.names), 
                nrows), domain = NA)
        .rowNamesDF(value, make.names = make.names) <- row.names
    }
    else attr(value, "row.names") <- .set_row_names(nrows)
    value
}


Sys.info <- function () 
.Internal(Sys.info())


.find.package <- function (...) 
.Defunct("find.package")


as.Date.character <- function (x, format, tryFormats = c("%Y-%m-%d", "%Y/%m/%d"), 
    optional = FALSE, ...) 
{
    charToDate <- function(x) {
        xx <- x[1L]
        if (is.na(xx)) {
            j <- 1L
            while (is.na(xx) && (j <- j + 1L) <= length(x)) xx <- x[j]
            if (is.na(xx)) 
                f <- "%Y-%m-%d"
        }
        if (is.na(xx)) 
            strptime(x, f)
        else {
            for (ff in tryFormats) if (!is.na(strptime(xx, ff, 
                tz = "GMT"))) 
                return(strptime(x, ff))
            if (optional) 
                as.Date.character(rep.int(NA_character_, length(x)), 
                  "%Y-%m-%d")
            else stop("character string is not in a standard unambiguous format")
        }
    }
    res <- if (missing(format)) 
        charToDate(x)
    else strptime(x, format, tz = "GMT")
    as.Date(res)
}


.encode_numeric_version <- function (x) 
{
    strlpad <- function(x, char, width) paste0(strrep(char, width - 
        nchar(x)), x)
    strrpad <- function(x, char, width) paste0(x, strrep(char, 
        width - nchar(x)))
    if (!is.numeric_version(x)) 
        stop("wrong class")
    classes <- class(x)
    nms <- names(x)
    x <- unclass(x)
    lens <- vapply(x, length, 0L)
    y <- lapply(x, function(e) sprintf("%o", e))
    width <- max(nchar(unlist(y)), 0L)
    y <- vapply(y, function(e) paste(strlpad(e, "0", width), 
        collapse = ""), "")
    y <- strrpad(y, "0", max(nchar(y), 0L))
    structure(ifelse(lens > 0L, y, NA_character_), width = width, 
        lens = lens, .classes = classes, names = nms)
}


xor <- function (x, y) 
{
    (x | y) & !(x & y)
}


quarters.POSIXt <- function (x, ...) 
{
    x <- (as.POSIXlt(x)$mon)%/%3
    paste0("Q", x + 1)
}


allowInterrupts <- function (expr) 
{
    suspended <- .Internal(interruptsSuspended())
    if (suspended) {
        on.exit(.Internal(interruptsSuspended(suspended)))
        .Internal(interruptsSuspended(FALSE))
        expr
    }
    else expr
}


`%in%` <- function (x, table) 
match(x, table, nomatch = 0L) > 0L


julian <- function (x, ...) 
UseMethod("julian")


intToBits <- function (x) 
.Internal(intToBits(x))


validUTF8 <- function (x) 
.Internal(validUTF8(x))


capabilities <- function (what = NULL) 
{
    z <- .Internal(capabilities())
    if (!is.null(what)) 
        z <- z[match(what, names(z), 0L)]
    if (.Platform$OS.type == "windows") 
        return(z)
    nas <- names(z[is.na(z)])
    if (any(nas %in% c("X11", "jpeg", "png", "tiff"))) {
        z[nas] <- tryCatch(.Internal(capabilitiesX11()), error = function(e) FALSE)
    }
    z
}


`oldClass<-` <- function (x, value)  .Primitive("oldClass<-")


print.simple.list <- function (x, ...) 
print(noquote(cbind(`_` = unlist(x))), ...)


namespaceImport <- function (self, ..., from = NULL, except = character(0L)) 
for (ns in list(...)) namespaceImportFrom(self, asNamespace(ns), 
    from = from, except = except)


geterrmessage <- function () 
.Internal(geterrmessage())


delayedAssign <- function (x, value, eval.env = parent.frame(1), assign.env = parent.frame(1)) 
.Internal(delayedAssign(x, substitute(value), eval.env, assign.env))


print.DLLRegisteredRoutines <- function (x, ...) 
{
    n <- lengths(x)
    x <- x[n > 0]
    n <- max(n)
    d <- list()
    sapply(names(x), function(id) {
        d[[id]] <<- rep.int("", n)
        names <- vapply(x[[id]], function(x) x$name, "")
        if (length(names)) 
            d[[id]][seq_along(names)] <<- names
        d[[paste(id, "numParameters")]] <<- rep.int("", n)
        names <- sapply(x[[id]], function(x) x$numParameters)
        if (length(names)) 
            d[[paste(id, "numParameters")]][seq_along(names)] <<- names
    })
    print(as.data.frame(d), ...)
    invisible(x)
}


Sys.setlocale <- function (category = "LC_ALL", locale = "") 
{
    category <- match(category, c("LC_ALL", "LC_COLLATE", "LC_CTYPE", 
        "LC_MONETARY", "LC_NUMERIC", "LC_TIME", "LC_MESSAGES", 
        "LC_PAPER", "LC_MEASUREMENT"))
    if (is.na(category)) 
        stop("invalid 'category' argument")
    .Internal(Sys.setlocale(category, locale))
}


difftime <- function (time1, time2, tz, units = c("auto", "secs", "mins", 
    "hours", "days", "weeks")) 
{
    if (missing(tz)) {
        time1 <- as.POSIXct(time1)
        time2 <- as.POSIXct(time2)
    }
    else {
        time1 <- as.POSIXct(time1, tz = tz)
        time2 <- as.POSIXct(time2, tz = tz)
    }
    z <- unclass(time1) - unclass(time2)
    attr(z, "tzone") <- NULL
    units <- match.arg(units)
    if (units == "auto") 
        units <- if (all(is.na(z))) 
            "secs"
        else {
            zz <- min(abs(z), na.rm = TRUE)
            if (!is.finite(zz) || zz < 60) 
                "secs"
            else if (zz < 3600) 
                "mins"
            else if (zz < 86400) 
                "hours"
            else "days"
        }
    switch(units, secs = .difftime(z, units = "secs"), mins = .difftime(z/60, 
        units = "mins"), hours = .difftime(z/3600, units = "hours"), 
        days = .difftime(z/86400, units = "days"), weeks = .difftime(z/(7 * 
            86400), units = "weeks"))
}


`[.Date` <- function (x, ..., drop = TRUE) 
{
    .Date(NextMethod("["), oldClass(x))
}


..getNamespace <- function (name, where) 
{
    ns <- .Internal(getRegisteredNamespace(name))
    if (!is.null(ns)) 
        ns
    else tryCatch(loadNamespace(name), error = function(e) {
        warning(gettextf("namespace %s is not available and has been replaced\nby .GlobalEnv when processing object %s", 
            sQuote(name)[1L], sQuote(where)), domain = NA, call. = FALSE, 
            immediate. = TRUE)
        .GlobalEnv
    })
}


`levels<-.factor` <- function (x, value) 
{
    xlevs <- levels(x)
    if (is.list(value)) {
        nlevs <- rep.int(names(value), lapply(value, length))
        value <- unlist(value)
        m <- match(value, xlevs, nomatch = 0L)
        xlevs[m] <- nlevs[m > 0L]
    }
    else {
        if (length(xlevs) > length(value)) 
            stop("number of levels differs")
        nlevs <- xlevs <- as.character(value)
        nlevs <- nlevs[!is.na(nlevs)]
    }
    nlevs <- unique(nlevs)
    at <- attributes(x)
    at$levels <- nlevs
    y <- match(xlevs, nlevs)[x]
    attributes(y) <- at
    y
}


range <- function (..., na.rm = FALSE)  .Primitive("range")


unique.data.frame <- function (x, incomparables = FALSE, fromLast = FALSE, ...) 
{
    if (!isFALSE(incomparables)) 
        .NotYetUsed("incomparables != FALSE")
    x[!duplicated(x, fromLast = fromLast, ...), , drop = FALSE]
}


deparse <- function (expr, width.cutoff = 60L, backtick = mode(expr) %in% 
    c("call", "expression", "(", "function"), control = c("keepNA", 
    "keepInteger", "niceNames", "showAttributes"), nlines = -1L) 
.Internal(deparse(expr, width.cutoff, backtick, .deparseOpts(control), 
    nlines))


pairlist <- function (...) 
as.pairlist(list(...))


sinpi <- function (x)  .Primitive("sinpi")


summary.POSIXct <- function (object, digits = 15L, ...) 
{
    x <- summary.default(unclass(object), digits = digits, ...)
    if (m <- match("NA's", names(x), 0L)) {
        NAs <- as.integer(x[m])
        x <- x[-m]
        attr(x, "NAs") <- NAs
    }
    .POSIXct(x, tz = attr(object, "tzone"), cl = c("summaryDefault", 
        "table", oldClass(object)))
}


print.NativeRoutineList <- function (x, ...) 
{
    if (length(x)) {
        m <- data.frame(numParameters = sapply(x, function(x) x$numParameters), 
            row.names = sapply(x, function(x) x$name))
        print(m, ...)
    }
    invisible(x)
}


psigamma <- function (x, deriv = 0L) 
.Internal(psigamma(x, deriv))


file.access <- function (names, mode = 0) 
{
    res <- .Internal(file.access(names, mode))
    names(res) <- names
    res
}


is.environment <- function (x)  .Primitive("is.environment")


Math.factor <- function (x, ...) 
stop(gettextf("%s not meaningful for factors", sQuote(.Generic)))


getRversion <- function () 
package_version(R.version)


droplevels <- function (x, ...) 
UseMethod("droplevels")


determinant.matrix <- function (x, logarithm = TRUE, ...) 
{
    if ((n <- ncol(x)) != nrow(x)) 
        stop("'x' must be a square matrix")
    if (n < 1L) 
        return(structure(list(modulus = structure(if (logarithm) 0 else 1, 
            logarithm = logarithm), sign = 1L), class = "det"))
    if (is.complex(x)) 
        stop("'determinant' not currently defined for complex matrices")
    .Internal(det_ge_real(x, logarithm))
}


stdout <- function () 
.Internal(stdout())


summary.POSIXlt <- function (object, digits = 15, ...) 
summary(as.POSIXct(object), digits = digits, ...)


suspendInterrupts <- function (expr) 
{
    suspended <- .Internal(interruptsSuspended())
    if (suspended) 
        expr
    else {
        on.exit(.Internal(interruptsSuspended(suspended)))
        .Internal(interruptsSuspended(TRUE))
        expr
    }
}


tapply <- function (X, INDEX, FUN = NULL, ..., default = NA, simplify = TRUE) 
{
    FUN <- if (!is.null(FUN)) 
        match.fun(FUN)
    if (!is.list(INDEX)) 
        INDEX <- list(INDEX)
    INDEX <- lapply(INDEX, as.factor)
    nI <- length(INDEX)
    if (!nI) 
        stop("'INDEX' is of length zero")
    if (!all(lengths(INDEX) == length(X))) 
        stop("arguments must have same length")
    namelist <- lapply(INDEX, levels)
    extent <- lengths(namelist, use.names = FALSE)
    cumextent <- cumprod(extent)
    if (cumextent[nI] > .Machine$integer.max) 
        stop("total number of levels >= 2^31")
    storage.mode(cumextent) <- "integer"
    ngroup <- cumextent[nI]
    group <- as.integer(INDEX[[1L]])
    if (nI > 1L) 
        for (i in 2L:nI) group <- group + cumextent[i - 1L] * 
            (as.integer(INDEX[[i]]) - 1L)
    if (is.null(FUN)) 
        return(group)
    levels(group) <- as.character(seq_len(ngroup))
    class(group) <- "factor"
    ans <- split(X, group)
    names(ans) <- NULL
    index <- as.logical(lengths(ans))
    ans <- lapply(X = ans[index], FUN = FUN, ...)
    ansmat <- array(if (simplify && all(lengths(ans) == 1L)) {
        ans <- unlist(ans, recursive = FALSE, use.names = FALSE)
        if (!is.null(ans) && is.na(default) && is.atomic(ans)) 
            vector(typeof(ans))
        else default
    }
    else vector("list", prod(extent)), dim = extent, dimnames = namelist)
    if (length(ans)) {
        ansmat[index] <- ans
    }
    ansmat
}


.F_dqrqty <- structure(list(name = "dqrqty", address = pointer("0x000000000df13610"), 
    dll = structure(list(name = "base", path = "base", dynamicLookup = FALSE, 
        handle = <pointer: (nil)>, info = pointer("0x000000000df73cc0")), class = "DLLInfo"), 
    numParameters = 7L), class = c("FortranRoutine", "NativeSymbolInfo"
))


trace <- function (what, tracer, exit, at, print, signature, where = topenv(parent.frame()), 
    edit = FALSE) 
{
    if (nargs() > 1L && !.isMethodsDispatchOn()) {
        ns <- try(loadNamespace("methods"))
        if (isNamespace(ns)) 
            message("(loaded the methods namespace)", domain = NA)
        else stop("tracing functions requires the 'methods' package, but unable to load the 'methods' namespace")
    }
    else if (nargs() == 1L) 
        return(.primTrace(what))
    tState <- tracingState(FALSE)
    on.exit(tracingState(tState))
    call <- sys.call()
    call[[1L]] <- quote(methods::.TraceWithMethods)
    call$where <- where
    eval.parent(call)
}


callCC <- function (fun) 
{
    value <- NULL
    delayedAssign("throw", return(value))
    fun(function(v) {
        value <<- v
        throw
    })
}


.getNamespace <- function (name) 
.Internal(getRegisteredNamespace(name))


as.matrix.POSIXlt <- function (x, ...) 
{
    as.matrix(as.data.frame(unclass(x)), ...)
}


rowMeans <- function (x, na.rm = FALSE, dims = 1L) 
{
    if (is.data.frame(x)) 
        x <- as.matrix(x)
    if (!is.array(x) || length(dn <- dim(x)) < 2L) 
        stop("'x' must be an array of at least two dimensions")
    if (dims < 1L || dims > length(dn) - 1L) 
        stop("invalid 'dims'")
    p <- prod(dn[-(id <- seq_len(dims))])
    dn <- dn[id]
    z <- if (is.complex(x)) 
        .Internal(rowMeans(Re(x), prod(dn), p, na.rm)) + (0+1i) * 
            .Internal(rowMeans(Im(x), prod(dn), p, na.rm))
    else .Internal(rowMeans(x, prod(dn), p, na.rm))
    if (length(dn) > 1L) {
        dim(z) <- dn
        dimnames(z) <- dimnames(x)[id]
    }
    else names(z) <- dimnames(x)[[1L]]
    z
}


.isOpen <- function (srcfile) 
{
    conn <- srcfile$conn
    return(!is.null(conn) && isOpen(conn))
}


format.packageInfo <- function (x, ...) 
{
    if (!inherits(x, "packageInfo")) 
        stop("wrong class")
    vignetteMsg <- gettextf("Further information is available in the following vignettes in directory %s:", 
        sQuote(file.path(x$path, "doc")))
    headers <- sprintf("\n%s\n", c(gettext("Description:"), gettext("Index:"), 
        paste(strwrap(vignetteMsg), collapse = "\n")))
    formatDocEntry <- function(entry) {
        if (is.list(entry) || is.matrix(entry)) 
            formatDL(entry, style = "list")
        else entry
    }
    c(gettextf("\n\t\tInformation on package %s", sQuote(x$name)), 
        unlist(lapply(which(!vapply(x$info, is.null, NA)), function(i) c(headers[i], 
            formatDocEntry(x$info[[i]])))))
}


.F_dqrrsd <- structure(list(name = "dqrrsd", address = pointer("0x000000000df13870"), 
    dll = structure(list(name = "base", path = "base", dynamicLookup = FALSE, 
        handle = <pointer: (nil)>, info = pointer("0x000000000df73cc0")), class = "DLLInfo"), 
    numParameters = 7L), class = c("FortranRoutine", "NativeSymbolInfo"
))


print.default <- function (x, digits = NULL, quote = TRUE, na.print = NULL, print.gap = NULL, 
    right = FALSE, max = NULL, useSource = TRUE, ...) 
{
    args <- pairlist(digits = digits, quote = quote, na.print = na.print, 
        print.gap = print.gap, right = right, max = max, useSource = useSource, 
        ...)
    missings <- c(missing(digits), missing(quote), missing(na.print), 
        missing(print.gap), missing(right), missing(max), missing(useSource))
    .Internal(print.default(x, args, missings))
}


shQuote <- function (string, type = c("sh", "csh", "cmd", "cmd2")) 
{
    cshquote <- function(x) {
        xx <- strsplit(x, "'", fixed = TRUE)[[1L]]
        paste(paste0("'", xx, "'"), collapse = "\"'\"")
    }
    if (missing(type) && .Platform$OS.type == "windows") 
        type <- "cmd"
    type <- match.arg(type)
    if (type == "cmd") 
        paste0("\"", gsub("\"", "\\\\\"", string), "\"")
    else if (type == "cmd2") 
        gsub("([()%!^\"<>&|])", "^\\1", string)
    else if (!length(string)) 
        ""
    else if (!any(grepl("'", string))) 
        paste0("'", string, "'")
    else if (type == "sh") 
        paste0("\"", gsub("([\"$`\\])", "\\\\\\1", string), "\"")
    else if (!any(grepl("([$`])", string))) 
        paste0("\"", gsub("([\"!\\])", "\\\\\\1", string), "\"")
    else vapply(string, cshquote, "")
}


cut.default <- function (x, breaks, labels = NULL, include.lowest = FALSE, right = TRUE, 
    dig.lab = 3L, ordered_result = FALSE, ...) 
{
    if (!is.numeric(x)) 
        stop("'x' must be numeric")
    if (length(breaks) == 1L) {
        if (is.na(breaks) || breaks < 2L) 
            stop("invalid number of intervals")
        nb <- as.integer(breaks + 1)
        dx <- diff(rx <- range(x, na.rm = TRUE))
        if (dx == 0) {
            dx <- if (rx[1L] != 0) 
                abs(rx[1L])
            else 1
            breaks <- seq.int(rx[1L] - dx/1000, rx[2L] + dx/1000, 
                length.out = nb)
        }
        else {
            breaks <- seq.int(rx[1L], rx[2L], length.out = nb)
            breaks[c(1L, nb)] <- c(rx[1L] - dx/1000, rx[2L] + 
                dx/1000)
        }
    }
    else nb <- length(breaks <- sort.int(as.double(breaks)))
    if (anyDuplicated(breaks)) 
        stop("'breaks' are not unique")
    codes.only <- FALSE
    if (is.null(labels)) {
        for (dig in dig.lab:max(12L, dig.lab)) {
            ch.br <- formatC(0 + breaks, digits = dig, width = 1L)
            if (ok <- all(ch.br[-1L] != ch.br[-nb])) 
                break
        }
        labels <- if (ok) 
            paste0(if (right) 
                "("
            else "[", ch.br[-nb], ",", ch.br[-1L], if (right) 
                "]"
            else ")")
        else paste0("Range_", seq_len(nb - 1L))
        if (ok && include.lowest) {
            if (right) 
                substr(labels[1L], 1L, 1L) <- "["
            else substring(labels[nb - 1L], nchar(labels[nb - 
                1L], "c")) <- "]"
        }
    }
    else if (is.logical(labels) && !labels) 
        codes.only <- TRUE
    else if (length(labels) != nb - 1L) 
        stop("lengths of 'breaks' and 'labels' differ")
    code <- .bincode(x, breaks, right, include.lowest)
    if (codes.only) 
        code
    else factor(code, seq_along(labels), labels, ordered = ordered_result)
}


range.default <- function (..., na.rm = FALSE, finite = FALSE) 
{
    x <- c(..., recursive = TRUE)
    if (is.numeric(x)) {
        if (finite) 
            x <- x[is.finite(x)]
        else if (na.rm) 
            x <- x[!is.na(x)]
        c(min(x), max(x))
    }
    else {
        if (finite) 
            na.rm <- TRUE
        c(min(x, na.rm = na.rm), max(x, na.rm = na.rm))
    }
}


structure <- function (.Data, ...) 
{
    if (is.null(.Data)) 
        warning("Calling 'structure(NULL, *)' is deprecated, as NULL cannot have attributes.\n  Consider 'structure(list(), *)' instead.")
    attrib <- list(...)
    if (length(attrib)) {
        specials <- c(".Dim", ".Dimnames", ".Names", ".Tsp", 
            ".Label")
        replace <- c("dim", "dimnames", "names", "tsp", "levels")
        m <- match(names(attrib), specials)
        ok <- !is.na(m)
        names(attrib)[ok] <- replace[m[ok]]
        if ("factor" %in% attrib[["class", exact = TRUE]] && 
            typeof(.Data) == "double") 
            storage.mode(.Data) <- "integer"
        attributes(.Data) <- c(attributes(.Data), attrib)
        .Data
    }
    else .Data
}


mode <- function (x) 
{
    if (is.expression(x)) 
        return("expression")
    if (is.call(x)) 
        return(switch(deparse(x[[1L]])[1L], `(` = "(", "call"))
    if (is.name(x)) 
        "name"
    else switch(tx <- typeof(x), double = , integer = "numeric", 
        closure = , builtin = , special = "function", tx)
}


Ops.difftime <- function (e1, e2) 
{
    coerceTimeUnit <- function(x) {
        switch(attr(x, "units"), secs = x, mins = 60 * x, hours = 60 * 
            60 * x, days = 60 * 60 * 24 * x, weeks = 60 * 60 * 
            24 * 7 * x)
    }
    if (nargs() == 1L) {
        switch(.Generic, `+` = {
        }, `-` = {
            e1[] <- -unclass(e1)
        }, stop(gettextf("unary '%s' not defined for \"difftime\" objects", 
            .Generic), domain = NA, call. = FALSE))
        return(e1)
    }
    boolean <- switch(.Generic, `<` = , `>` = , `==` = , `!=` = , 
        `<=` = , `>=` = TRUE, FALSE)
    if (boolean) {
        if (inherits(e1, "difftime") && inherits(e2, "difftime")) {
            e1 <- coerceTimeUnit(e1)
            e2 <- coerceTimeUnit(e2)
        }
        NextMethod(.Generic)
    }
    else if (.Generic == "+" || .Generic == "-") {
        if (inherits(e1, "difftime") && !inherits(e2, "difftime")) 
            return(.difftime(NextMethod(.Generic), units = attr(e1, 
                "units")))
        if (!inherits(e1, "difftime") && inherits(e2, "difftime")) 
            return(.difftime(NextMethod(.Generic), units = attr(e2, 
                "units")))
        u1 <- attr(e1, "units")
        if (attr(e2, "units") == u1) {
            .difftime(NextMethod(.Generic), units = u1)
        }
        else {
            e1 <- coerceTimeUnit(e1)
            e2 <- coerceTimeUnit(e2)
            .difftime(NextMethod(.Generic), units = "secs")
        }
    }
    else {
        stop(gettextf("'%s' not defined for \"difftime\" objects", 
            .Generic), domain = NA)
    }
}


attributes <- function (x)  .Primitive("attributes")


as.POSIXct <- function (x, tz = "", ...) 
UseMethod("as.POSIXct")


all.names <- function (expr, functions = TRUE, max.names = -1L, unique = FALSE) 
.Internal(all.names(expr, functions, max.names, unique))


as.character <- function (x, ...)  .Primitive("as.character")


as.POSIXlt <- function (x, tz = "", ...) 
UseMethod("as.POSIXlt")


sort.default <- function (x, decreasing = FALSE, na.last = NA, ...) 
{
    if (is.object(x)) 
        x[order(x, na.last = na.last, decreasing = decreasing)]
    else sort.int(x, na.last = na.last, decreasing = decreasing, 
        ...)
}


simpleCondition <- function (message, call = NULL) 
{
    class <- c("simpleCondition", "condition")
    structure(list(message = as.character(message), call = call), 
        class = class)
}


within.data.frame <- function (data, expr, ...) 
{
    parent <- parent.frame()
    e <- evalq(environment(), data, parent)
    eval(substitute(expr), e)
    l <- as.list(e, all.names = TRUE)
    l <- l[!vapply(l, is.null, NA, USE.NAMES = FALSE)]
    del <- setdiff(names(data), (nl <- names(l)))
    data[nl] <- l
    data[del] <- NULL
    data
}


`row.names<-.data.frame` <- function (x, value) 
`.rowNamesDF<-`(x, value = value)


endsWith <- function (x, suffix) 
.Internal(endsWith(x, suffix))


print.connection <- function (x, ...) 
{
    usumm <- tryCatch(unlist(summary(x)), error = function(e) {
    })
    if (is.null(usumm)) {
        cl <- oldClass(x)
        cl <- cl[cl != "connection"]
        cat("A connection, ", if (length(cl)) 
            paste0("specifically, ", paste(sQuote(cl), collapse = ", "), 
                ", "), "but invalid.\n", sep = "")
    }
    else {
        cat("A connection with")
        print(cbind(` ` = usumm), ...)
    }
    invisible(x)
}


`&.octmode` <- function (a, b) 
as.octmode(bitwAnd(as.octmode(a), as.octmode(b)))


`parent.env<-` <- function (env, value) 
.Internal(`parent.env<-`(env, value))


split.default <- function (x, f, drop = FALSE, sep = ".", lex.order = FALSE, ...) 
{
    if (!missing(...)) 
        .NotYetUsed(deparse(...), error = FALSE)
    if (is.list(f)) 
        f <- interaction(f, drop = drop, sep = sep, lex.order = lex.order)
    else if (!is.factor(f)) 
        f <- as.factor(f)
    else if (drop) 
        f <- factor(f)
    storage.mode(f) <- "integer"
    if (is.null(attr(x, "class"))) 
        return(.Internal(split(x, f)))
    ind <- .Internal(split(seq_along(x), f))
    lapply(ind, function(i) x[i])
}


asS3 <- function (object, flag = TRUE, complete = TRUE) 
.Internal(setS4Object(object, !as.logical(flag), complete))


asS4 <- function (object, flag = TRUE, complete = TRUE) 
.Internal(setS4Object(object, flag, complete))


with.default <- function (data, expr, ...) 
eval(substitute(expr), data, enclos = parent.frame())


args <- function (name) 
.Internal(args(name))


break <- .Primitive("break")


R_system_version <- function (x, strict = TRUE) 
.make_numeric_version(x, strict, .standard_regexps()$valid_R_system_version, 
    c("R_system_version", "package_version"))


matrix <- function (data = NA, nrow = 1, ncol = 1, byrow = FALSE, dimnames = NULL) 
{
    if (is.object(data) || !is.atomic(data)) 
        data <- as.vector(data)
    .Internal(matrix(data, nrow, ncol, byrow, dimnames, missing(nrow), 
        missing(ncol)))
}


is.na.POSIXlt <- function (x) 
is.na(as.POSIXct(x))


basename <- function (path) 
.Internal(basename(path))


lbeta <- function (a, b) 
.Internal(lbeta(a, b))


asin <- function (x)  .Primitive("asin")


summary.default <- function (object, ..., digits, quantile.type = 7) 
{
    if (is.factor(object)) 
        return(summary.factor(object, ...))
    else if (is.matrix(object)) {
        if (missing(digits)) 
            return(summary.matrix(object, quantile.type = quantile.type, 
                ...))
        else return(summary.matrix(object, digits = digits, quantile.type = quantile.type, 
            ...))
    }
    value <- if (is.logical(object)) 
        c(Mode = "logical", {
            tb <- table(object, exclude = NULL, useNA = "ifany")
            if (!is.null(n <- dimnames(tb)[[1L]]) && any(iN <- is.na(n))) dimnames(tb)[[1L]][iN] <- "NA's"
            tb
        })
    else if (is.numeric(object)) {
        nas <- is.na(object)
        object <- object[!nas]
        qq <- stats::quantile(object, names = FALSE, type = quantile.type)
        qq <- c(qq[1L:3L], mean(object), qq[4L:5L])
        if (!missing(digits)) 
            qq <- signif(qq, digits)
        names(qq) <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", 
            "Max.")
        if (any(nas)) 
            c(qq, `NA's` = sum(nas))
        else qq
    }
    else if (is.recursive(object) && !is.language(object) && 
        (n <- length(object))) {
        sumry <- array("", c(n, 3L), list(names(object), c("Length", 
            "Class", "Mode")))
        ll <- numeric(n)
        for (i in 1L:n) {
            ii <- object[[i]]
            ll[i] <- length(ii)
            cls <- oldClass(ii)
            sumry[i, 2L] <- if (length(cls)) 
                cls[1L]
            else "-none-"
            sumry[i, 3L] <- mode(ii)
        }
        sumry[, 1L] <- format(as.integer(ll))
        sumry
    }
    else c(Length = length(object), Class = class(object), Mode = mode(object))
    class(value) <- c("summaryDefault", "table")
    value
}


file.append <- function (file1, file2) 
.Internal(file.append(file1, file2))


srcfile <- function (filename, encoding = getOption("encoding"), Enc = "unknown") 
{
    stopifnot(is.character(filename), length(filename) == 1L)
    e <- new.env(hash = FALSE, parent = emptyenv())
    e$wd <- getwd()
    e$filename <- filename
    e$timestamp <- file.mtime(filename)
    if (identical(encoding, "unknown")) 
        encoding <- "native.enc"
    e$encoding <- encoding
    e$Enc <- Enc
    class(e) <- "srcfile"
    return(e)
}


Sys.which <- function (names) 
.Internal(Sys.which(as.character(names)))


ncol <- function (x) 
dim(x)[2L]


return <- .Primitive("return")


atan <- function (x)  .Primitive("atan")


eapply <- function (env, FUN, ..., all.names = FALSE, USE.NAMES = TRUE) 
{
    FUN <- match.fun(FUN)
    .Internal(eapply(env, FUN, all.names, USE.NAMES))
}


rep.POSIXct <- function (x, ...) 
.POSIXct(NextMethod(), attr(x, "tzone"), oldClass(x))


rep.POSIXlt <- function (x, ...) 
.POSIXlt(lapply(X = unclass(x), FUN = rep, ...), attr(x, "tzone"), 
    oldClass(x))


socketConnection <- function (host = "localhost", port, server = FALSE, blocking = FALSE, 
    open = "a+", encoding = getOption("encoding"), timeout = getOption("timeout")) 
.Internal(socketConnection(host, port, server, blocking, open, 
    encoding, timeout))


format.Date <- function (x, ...) 
{
    xx <- format(as.POSIXlt(x), ...)
    names(xx) <- names(x)
    xx
}


getConnection <- function (what) 
.Internal(getConnection(what))


tryCatch <- function (expr, ..., finally) 
{
    tryCatchList <- function(expr, names, parentenv, handlers) {
        nh <- length(names)
        if (nh > 1L) 
            tryCatchOne(tryCatchList(expr, names[-nh], parentenv, 
                handlers[-nh]), names[nh], parentenv, handlers[[nh]])
        else if (nh == 1L) 
            tryCatchOne(expr, names, parentenv, handlers[[1L]])
        else expr
    }
    tryCatchOne <- function(expr, name, parentenv, handler) {
        doTryCatch <- function(expr, name, parentenv, handler) {
            .Internal(.addCondHands(name, list(handler), parentenv, 
                environment(), FALSE))
            expr
        }
        value <- doTryCatch(return(expr), name, parentenv, handler)
        if (is.null(value[[1L]])) {
            msg <- .Internal(geterrmessage())
            call <- value[[2L]]
            cond <- simpleError(msg, call)
        }
        else if (is.character(value[[1L]])) {
            msg <- value[[1L]]
            call <- value[[2L]]
            cond <- simpleError(msg, call)
        }
        else cond <- value[[1L]]
        value[[3L]](cond)
    }
    if (!missing(finally)) 
        on.exit(finally)
    handlers <- list(...)
    classes <- names(handlers)
    parentenv <- parent.frame()
    if (length(classes) != length(handlers)) 
        stop("bad handler specification")
    tryCatchList(expr, classes, parentenv, handlers)
}


attr <- function (x, which, exact = FALSE)  .Primitive("attr")


tracemem <- function (x)  .Primitive("tracemem")


as.data.frame.ordered <- function (x, row.names = NULL, optional = FALSE, ..., nm = paste(deparse(substitute(x), 
    width.cutoff = 500L), collapse = " ")) 
{
    force(nm)
    nrows <- length(x)
    if (!(is.null(row.names) || (is.character(row.names) && length(row.names) == 
        nrows))) {
        warning(gettextf("'row.names' is not a character vector of length %d -- omitting it. Will be an error!", 
            nrows), domain = NA)
        row.names <- NULL
    }
    if (is.null(row.names)) {
        if (nrows == 0L) 
            row.names <- character()
        else if (length(row.names <- names(x)) != nrows || anyDuplicated(row.names)) 
            row.names <- .set_row_names(nrows)
    }
    if (!is.null(names(x))) 
        names(x) <- NULL
    value <- list(x)
    if (!optional) 
        names(value) <- nm
    structure(value, row.names = row.names, class = "data.frame")
}


format.hexmode <- function (x, width = NULL, upper.case = FALSE, ...) 
{
    isna <- is.na(x)
    y <- as.integer(x[!isna])
    fmt0 <- if (upper.case) 
        "X"
    else "x"
    fmt <- if (!is.null(width)) 
        paste0("%0", width, fmt0)
    else paste0("%", fmt0)
    ans <- rep.int(NA_character_, length(x))
    ans0 <- sprintf(fmt, y)
    if (is.null(width) && length(y) > 1L) {
        nc <- max(nchar(ans0))
        ans0 <- sprintf(paste0("%0", nc, fmt0), y)
    }
    ans[!isna] <- ans0
    dim(ans) <- dim(x)
    dimnames(ans) <- dimnames(x)
    names(ans) <- names(x)
    ans
}


beta <- function (a, b) 
.Internal(beta(a, b))


seq.POSIXt <- function (from, to, by, length.out = NULL, along.with = NULL, 
    ...) 
{
    if (missing(from)) 
        stop("'from' must be specified")
    if (!inherits(from, "POSIXt")) 
        stop("'from' must be a \"POSIXt\" object")
    cfrom <- as.POSIXct(from)
    if (length(cfrom) != 1L) 
        stop("'from' must be of length 1")
    tz <- attr(cfrom, "tzone")
    if (!missing(to)) {
        if (!inherits(to, "POSIXt")) 
            stop("'to' must be a \"POSIXt\" object")
        if (length(as.POSIXct(to)) != 1) 
            stop("'to' must be of length 1")
    }
    if (!missing(along.with)) {
        length.out <- length(along.with)
    }
    else if (!is.null(length.out)) {
        if (length(length.out) != 1L) 
            stop("'length.out' must be of length 1")
        length.out <- ceiling(length.out)
    }
    status <- c(!missing(to), !missing(by), !is.null(length.out))
    if (sum(status) != 2L) 
        stop("exactly two of 'to', 'by' and 'length.out' / 'along.with' must be specified")
    if (missing(by)) {
        from <- unclass(cfrom)
        to <- unclass(as.POSIXct(to))
        res <- seq.int(from, to, length.out = length.out)
        return(.POSIXct(res, tz))
    }
    if (length(by) != 1L) 
        stop("'by' must be of length 1")
    valid <- 0L
    if (inherits(by, "difftime")) {
        by <- switch(attr(by, "units"), secs = 1, mins = 60, 
            hours = 3600, days = 86400, weeks = 7 * 86400) * 
            unclass(by)
    }
    else if (is.character(by)) {
        by2 <- strsplit(by, " ", fixed = TRUE)[[1L]]
        if (length(by2) > 2L || length(by2) < 1L) 
            stop("invalid 'by' string")
        valid <- pmatch(by2[length(by2)], c("secs", "mins", "hours", 
            "days", "weeks", "months", "years", "DSTdays", "quarters"))
        if (is.na(valid)) 
            stop("invalid string for 'by'")
        if (valid <= 5L) {
            by <- c(1, 60, 3600, 86400, 7 * 86400)[valid]
            if (length(by2) == 2L) 
                by <- by * as.integer(by2[1L])
        }
        else by <- if (length(by2) == 2L) 
            as.integer(by2[1L])
        else 1
    }
    else if (!is.numeric(by)) 
        stop("invalid mode for 'by'")
    if (is.na(by)) 
        stop("'by' is NA")
    if (valid <= 5L) {
        from <- unclass(as.POSIXct(from))
        if (!is.null(length.out)) 
            res <- seq.int(from, by = by, length.out = length.out)
        else {
            to0 <- unclass(as.POSIXct(to))
            res <- seq.int(0, to0 - from, by) + from
        }
        return(.POSIXct(res, tz))
    }
    else {
        r1 <- as.POSIXlt(from)
        if (valid == 7L) {
            if (missing(to)) {
                yr <- seq.int(r1$year, by = by, length.out = length.out)
            }
            else {
                to <- as.POSIXlt(to)
                yr <- seq.int(r1$year, to$year, by)
            }
            r1$year <- yr
        }
        else if (valid %in% c(6L, 9L)) {
            if (valid == 9L) 
                by <- by * 3
            if (missing(to)) {
                mon <- seq.int(r1$mon, by = by, length.out = length.out)
            }
            else {
                to0 <- as.POSIXlt(to)
                mon <- seq.int(r1$mon, 12 * (to0$year - r1$year) + 
                  to0$mon, by)
            }
            r1$mon <- mon
        }
        else if (valid == 8L) {
            if (!missing(to)) {
                length.out <- 2L + floor((unclass(as.POSIXct(to)) - 
                  unclass(as.POSIXct(from)))/(by * 86400))
            }
            r1$mday <- seq.int(r1$mday, by = by, length.out = length.out)
        }
        r1$isdst <- -1L
        res <- as.POSIXct(r1)
        if (!missing(to)) {
            to <- as.POSIXct(to)
            res <- if (by > 0) 
                res[res <= to]
            else res[res >= to]
        }
        res
    }
}


summary.Date <- function (object, digits = 12L, ...) 
{
    x <- summary.default(unclass(object), digits = digits, ...)
    if (m <- match("NA's", names(x), 0L)) {
        NAs <- as.integer(x[m])
        x <- x[-m]
        attr(x, "NAs") <- NAs
    }
    .Date(x, c("summaryDefault", "table", oldClass(object)))
}


next <- .Primitive("next")


Sys.getlocale <- function (category = "LC_ALL") 
{
    category <- match(category, c("LC_ALL", "LC_COLLATE", "LC_CTYPE", 
        "LC_MONETARY", "LC_NUMERIC", "LC_TIME", "LC_MESSAGES", 
        "LC_PAPER", "LC_MEASUREMENT"))
    if (is.na(category)) 
        stop("invalid 'category' argument")
    .Internal(Sys.getlocale(category))
}


lower.tri <- function (x, diag = FALSE) 
{
    d <- dim(x)
    if (length(d) != 2L) 
        d <- dim(as.matrix(x))
    if (diag) 
        .row(d) >= .col(d)
    else .row(d) > .col(d)
}


rbind <- methods::rbind # re-exported from methods package

union <- function (x, y) 
unique(c(as.vector(x), as.vector(y)))


write <- function (x, file = "data", ncolumns = if (is.character(x)) 1 else 5, 
    append = FALSE, sep = " ") 
cat(x, file = file, sep = c(rep.int(sep, ncolumns - 1), "\n"), 
    append = append)


units <- function (x) 
UseMethod("units")


cbind <- methods::cbind # re-exported from methods package

dim.data.frame <- function (x) 
c(.row_names_info(x, 2L), length(x))


t.data.frame <- function (x) 
{
    x <- as.matrix(x)
    NextMethod("t")
}


month.abb <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", 
"Oct", "Nov", "Dec")


`diag<-` <- function (x, value) 
{
    dx <- dim(x)
    if (length(dx) != 2L) 
        stop("only matrix diagonals can be replaced")
    len.i <- min(dx)
    len.v <- length(value)
    if (len.v != 1L && len.v != len.i) 
        stop("replacement diagonal has wrong length")
    if (len.i) {
        i <- seq_len(len.i)
        x[cbind(i, i)] <- value
    }
    x
}


Sys.unsetenv <- function (x) 
.Internal(Sys.unsetenv(as.character(x)))


dyn.unload <- function (x) 
.Internal(dyn.unload(x))


scale.default <- function (x, center = TRUE, scale = TRUE) 
{
    x <- as.matrix(x)
    nc <- ncol(x)
    if (is.logical(center)) {
        if (center) {
            center <- colMeans(x, na.rm = TRUE)
            x <- sweep(x, 2L, center, check.margin = FALSE)
        }
    }
    else {
        if (!is.numeric(center)) 
            center <- as.numeric(center)
        if (length(center) == nc) 
            x <- sweep(x, 2L, center, check.margin = FALSE)
        else stop("length of 'center' must equal the number of columns of 'x'")
    }
    if (is.logical(scale)) {
        if (scale) {
            f <- function(v) {
                v <- v[!is.na(v)]
                sqrt(sum(v^2)/max(1, length(v) - 1L))
            }
            scale <- apply(x, 2L, f)
            x <- sweep(x, 2L, scale, "/", check.margin = FALSE)
        }
    }
    else {
        if (!is.numeric(scale)) 
            scale <- as.numeric(scale)
        if (length(scale) == nc) 
            x <- sweep(x, 2L, scale, "/", check.margin = FALSE)
        else stop("length of 'scale' must equal the number of columns of 'x'")
    }
    if (is.numeric(center)) 
        attr(x, "scaled:center") <- center
    if (is.numeric(scale)) 
        attr(x, "scaled:scale") <- scale
    x
}


invokeRestart <- function (r, ...) 
{
    if (!isRestart(r)) {
        res <- findRestart(r)
        if (is.null(res)) 
            stop(gettextf("no 'restart' '%s' found", as.character(r)), 
                domain = NA)
        r <- res
    }
    .Internal(.invokeRestart(r, list(...)))
}


`is.na<-.factor` <- function (x, value) 
{
    lx <- levels(x)
    cx <- oldClass(x)
    class(x) <- NULL
    x[value] <- NA
    structure(x, levels = lx, class = cx)
}


as.data.frame.integer <- function (x, row.names = NULL, optional = FALSE, ..., nm = paste(deparse(substitute(x), 
    width.cutoff = 500L), collapse = " ")) 
{
    force(nm)
    nrows <- length(x)
    if (!(is.null(row.names) || (is.character(row.names) && length(row.names) == 
        nrows))) {
        warning(gettextf("'row.names' is not a character vector of length %d -- omitting it. Will be an error!", 
            nrows), domain = NA)
        row.names <- NULL
    }
    if (is.null(row.names)) {
        if (nrows == 0L) 
            row.names <- character()
        else if (length(row.names <- names(x)) != nrows || anyDuplicated(row.names)) 
            row.names <- .set_row_names(nrows)
    }
    if (!is.null(names(x))) 
        names(x) <- NULL
    value <- list(x)
    if (!optional) 
        names(value) <- nm
    structure(value, row.names = row.names, class = "data.frame")
}


all.equal.language <- function (target, current, ...) 
{
    mt <- mode(target)
    mc <- mode(current)
    if (mt == "expression" && mc == "expression") 
        return(all.equal.list(target, current, ...))
    ttxt <- paste(deparse(target), collapse = "\n")
    ctxt <- paste(deparse(current), collapse = "\n")
    msg <- c(if (mt != mc) paste0("Modes of target, current: ", 
        mt, ", ", mc), if (ttxt != ctxt) {
        if (pmatch(ttxt, ctxt, 0L)) "target is a subset of current" else if (pmatch(ctxt, 
            ttxt, 0L)) "current is a subset of target" else "target, current do not match when deparsed"
    })
    if (is.null(msg)) 
        TRUE
    else msg
}


bitwOr <- function (a, b) 
.Internal(bitwiseOr(a, b))


.Library.site <- character(0)


.packageStartupMessage <- function (message, call = NULL) 
structure(list(message = message, call = call), class = c("packageStartupMessage", 
    "simpleMessage", "message", "condition"))


close.connection <- function (con, type = "rw", ...) 
.Internal(close(con, type))


`row.names<-` <- function (x, value) 
UseMethod("row.names<-")


trimws <- function (x, which = c("both", "left", "right"), whitespace = "[ \t\r\n]") 
{
    which <- match.arg(which)
    mysub <- function(re, x) sub(re, "", x, perl = TRUE)
    switch(which, left = mysub(paste0("^", whitespace, "+"), 
        x), right = mysub(paste0(whitespace, "+$"), x), both = mysub(paste0(whitespace, 
        "+$"), mysub(paste0("^", whitespace, "+"), x)))
}


.Call.graphics <- function (.NAME, ..., PACKAGE)  .Primitive(".Call.graphics")


.C_R_addTaskCallback <- structure(list(name = "R_addTaskCallback", address = pointer("0x000000000df13510"), 
    dll = structure(list(name = "base", path = "base", dynamicLookup = FALSE, 
        handle = <pointer: (nil)>, info = pointer("0x000000000df73cc0")), class = "DLLInfo"), 
    numParameters = 4L), class = c("CallRoutine", "NativeSymbolInfo"
))


unserialize <- function (connection, refhook = NULL) 
{
    if (typeof(connection) != "raw" && !is.character(connection) && 
        !inherits(connection, "connection")) 
        stop("'connection' must be a connection")
    .Internal(unserialize(connection, refhook))
}


`[[.data.frame` <- function (x, ..., exact = TRUE) 
{
    na <- nargs() - !missing(exact)
    if (!all(names(sys.call()) %in% c("", "exact"))) 
        warning("named arguments other than 'exact' are discouraged")
    if (na < 3L) 
        (function(x, i, exact) if (is.matrix(i)) 
            as.matrix(x)[[i]]
        else .subset2(x, i, exact = exact))(x, ..., exact = exact)
    else {
        col <- .subset2(x, ..2, exact = exact)
        i <- if (is.character(..1)) 
            pmatch(..1, row.names(x), duplicates.ok = TRUE)
        else ..1
        col[[i, exact = exact]]
    }
}


.amatch_bounds <- function (x = 0.1) 
{
    if (!is.list(x)) {
        if (!is.numeric(x) || (x < 0)) 
            stop("match distance components must be non-negative")
        bounds <- c(as.double(x), rep.int(NA_real_, 4L))
    }
    else {
        table <- c("cost", "insertions", "deletions", "substitutions", 
            "all")
        pos <- pmatch(names(x), table)
        if (anyNA(pos)) {
            warning("unknown match distance components ignored")
            x <- x[!is.na(pos)]
        }
        names(x) <- table[pos]
        x <- unlist(x)
        if (!all(is.numeric(x)) || any(x < 0)) 
            stop("match distance components must be non-negative")
        if (!is.na(x["cost"])) {
            bounds <- rep.int(NA_real_, 5L)
        }
        else {
            if (is.na(x["all"])) 
                x["all"] <- 0.1
            bounds <- c(NA_real_, rep.int(x["all"], 4L))
        }
        names(bounds) <- table
        bounds[names(x)] <- x
    }
    bounds
}


.S3_methods_table <- structure(c("!", "!", "$", "$", "$<-", "&", "&", "*", "+", "+", 
"-", "-", "/", "[", "[", "[", "[", "[", "[", "[", "[", "[", "[", 
"[", "[", "[", "[", "[", "[", "[", "[<-", "[<-", "[<-", "[<-", 
"[<-", "[<-", "[[", "[[", "[[", "[[", "[[", "[[", "[[<-", "[[<-", 
"[[<-", "[[<-", "|", "|", "Math", "Math", "Math", "Math", "Math", 
"Ops", "Ops", "Ops", "Ops", "Ops", "Ops", "Ops", "Summary", "Summary", 
"Summary", "Summary", "Summary", "Summary", "Summary", "Summary", 
"all.equal", "all.equal", "all.equal", "all.equal", "all.equal", 
"all.equal", "all.equal", "all.equal", "all.equal", "all.equal", 
"all.equal", "anyDuplicated", "anyDuplicated", "anyDuplicated", 
"anyDuplicated", "anyNA", "anyNA", "aperm", "aperm", "as.Date", 
"as.Date", "as.Date", "as.Date", "as.Date", "as.Date", "as.POSIXct", 
"as.POSIXct", "as.POSIXct", "as.POSIXct", "as.POSIXlt", "as.POSIXlt", 
"as.POSIXlt", "as.POSIXlt", "as.POSIXlt", "as.POSIXlt", "as.array", 
"as.character", "as.character", "as.character", "as.character", 
"as.character", "as.character", "as.character", "as.character", 
"as.character", "as.character", "as.data.frame", "as.data.frame", 
"as.data.frame", "as.data.frame", "as.data.frame", "as.data.frame", 
"as.data.frame", "as.data.frame", "as.data.frame", "as.data.frame", 
"as.data.frame", "as.data.frame", "as.data.frame", "as.data.frame", 
"as.data.frame", "as.data.frame", "as.data.frame", "as.data.frame", 
"as.data.frame", "as.data.frame", "as.data.frame", "as.data.frame", 
"as.data.frame", "as.data.frame", "as.double", "as.double", "as.expression", 
"as.function", "as.list", "as.list", "as.list", "as.list", "as.list", 
"as.list", "as.list", "as.list", "as.list", "as.logical", "as.matrix", 
"as.matrix", "as.matrix", "as.matrix", "as.null", "as.single", 
"as.table", "as.vector", "by", "by", "c", "c", "c", "c", "c", 
"c", "c", "cbind", "chol", "close", "close", "close", "conditionCall", 
"conditionMessage", "cut", "cut", "cut", "determinant", "diff", 
"diff", "diff", "diff", "dim", "dimnames", "dimnames<-", "droplevels", 
"droplevels", "duplicated", "duplicated", "duplicated", "duplicated", 
"duplicated", "duplicated", "duplicated", "flush", "format", 
"format", "format", "format", "format", "format", "format", "format", 
"format", "format", "format", "format", "format", "format", "getDLLRegisteredRoutines", 
"getDLLRegisteredRoutines", "is.na", "is.na", "is.na", "is.na<-", 
"is.na<-", "is.na<-", "is.numeric", "is.numeric", "is.numeric", 
"isSymmetric", "julian", "julian", "kappa", "kappa", "kappa", 
"labels", "length", "length<-", "length<-", "length<-", "length<-", 
"length<-", "levels", "levels<-", "mean", "mean", "mean", "mean", 
"mean", "merge", "merge", "months", "months", "names", "names<-", 
"open", "open", "open", "open", "pretty", "print", "print", "print", 
"print", "print", "print", "print", "print", "print", "print", 
"print", "print", "print", "print", "print", "print", "print", 
"print", "print", "print", "print", "print", "print", "print", 
"print", "print", "print", "print", "print", "print", "print", 
"print", "print", "print", "print", "print", "qr", "quarters", 
"quarters", "range", "rbind", "rep", "rep", "rep", "rep", "rep", 
"rev", "round", "round", "row.names", "row.names", "row.names<-", 
"row.names<-", "rowsum", "rowsum", "scale", "seek", "seq", "seq", 
"seq", "solve", "solve", "sort", "sort", "split", "split", "split", 
"split", "split<-", "split<-", "subset", "subset", "subset", 
"summary", "summary", "summary", "summary", "summary", "summary", 
"summary", "summary", "summary", "summary", "summary", "summary", 
"summary", "t", "t", "toString", "transform", "transform", "trunc", 
"trunc", "truncate", "unique", "unique", "unique", "unique", 
"unique", "unique", "unique", "units", "units<-", "weekdays", 
"weekdays", "with", "within", "within", "xtfrm", "xtfrm", "xtfrm", 
"xtfrm", "xtfrm", "xtfrm", "xtfrm", "xtfrm", "hexmode", "octmode", 
"DLLInfo", "package_version", "data.frame", "hexmode", "octmode", 
"difftime", "Date", "POSIXt", "Date", "POSIXt", "difftime", "AsIs", 
"DLLInfoList", "Date", "Dlist", "POSIXct", "POSIXlt", "data.frame", 
"difftime", "factor", "hexmode", "listof", "noquote", "numeric_version", 
"octmode", "simple.list", "table", "warnings", "Date", "POSIXct", 
"POSIXlt", "data.frame", "factor", "numeric_version", "Date", 
"POSIXct", "POSIXlt", "data.frame", "factor", "numeric_version", 
"POSIXlt", "data.frame", "factor", "numeric_version", "hexmode", 
"octmode", "Date", "POSIXt", "data.frame", "difftime", "factor", 
"Date", "POSIXt", "data.frame", "difftime", "factor", "numeric_version", 
"ordered", "Date", "POSIXct", "POSIXlt", "data.frame", "difftime", 
"factor", "numeric_version", "ordered", "POSIXt", "character", 
"default", "envRefClass", "environment", "factor", "formula", 
"language", "list", "numeric", "raw", "array", "data.frame", 
"default", "matrix", "POSIXlt", "numeric_version", "default", 
"table", "POSIXct", "POSIXlt", "character", "default", "factor", 
"numeric", "Date", "POSIXlt", "default", "numeric", "Date", "POSIXct", 
"character", "default", "factor", "numeric", "default", "Date", 
"POSIXt", "condition", "default", "error", "factor", "hexmode", 
"numeric_version", "octmode", "srcref", "AsIs", "Date", "POSIXct", 
"POSIXlt", "array", "character", "complex", "data.frame", "default", 
"difftime", "factor", "integer", "list", "logical", "matrix", 
"model.matrix", "noquote", "numeric", "numeric_version", "ordered", 
"raw", "table", "ts", "vector", "POSIXlt", "difftime", "default", 
"default", "Date", "POSIXct", "POSIXlt", "data.frame", "default", 
"environment", "factor", "function", "numeric_version", "factor", 
"POSIXlt", "data.frame", "default", "noquote", "default", "default", 
"default", "factor", "data.frame", "default", "Date", "POSIXct", 
"POSIXlt", "difftime", "noquote", "numeric_version", "warnings", 
"data.frame", "default", "connection", "srcfile", "srcfilealias", 
"condition", "condition", "Date", "POSIXt", "default", "matrix", 
"Date", "POSIXt", "default", "difftime", "data.frame", "data.frame", 
"data.frame", "data.frame", "factor", "POSIXlt", "array", "data.frame", 
"default", "matrix", "numeric_version", "warnings", "connection", 
"AsIs", "Date", "POSIXct", "POSIXlt", "data.frame", "default", 
"difftime", "factor", "hexmode", "libraryIQR", "numeric_version", 
"octmode", "packageInfo", "summaryDefault", "DLLInfo", "character", 
"POSIXlt", "data.frame", "numeric_version", "default", "factor", 
"numeric_version", "Date", "POSIXt", "difftime", "matrix", "Date", 
"POSIXt", "default", "lm", "qr", "default", "POSIXlt", "Date", 
"POSIXct", "POSIXlt", "difftime", "factor", "default", "factor", 
"Date", "POSIXct", "POSIXlt", "default", "difftime", "data.frame", 
"default", "Date", "POSIXt", "POSIXlt", "POSIXlt", "connection", 
"srcfile", "srcfilealias", "srcfilecopy", "default", "AsIs", 
"DLLInfo", "DLLInfoList", "DLLRegisteredRoutines", "Date", "Dlist", 
"NativeRoutineList", "POSIXct", "POSIXlt", "by", "condition", 
"connection", "data.frame", "default", "difftime", "eigen", "factor", 
"function", "hexmode", "libraryIQR", "listof", "noquote", "numeric_version", 
"octmode", "packageInfo", "proc_time", "restart", "rle", "simple.list", 
"srcfile", "srcref", "summary.table", "summary.warnings", "summaryDefault", 
"table", "warnings", "default", "Date", "POSIXt", "default", 
"data.frame", "Date", "POSIXct", "POSIXlt", "factor", "numeric_version", 
"default", "Date", "POSIXt", "data.frame", "default", "data.frame", 
"default", "data.frame", "default", "default", "connection", 
"Date", "POSIXt", "default", "default", "qr", "POSIXlt", "default", 
"Date", "POSIXct", "data.frame", "default", "data.frame", "default", 
"data.frame", "default", "matrix", "Date", "POSIXct", "POSIXlt", 
"connection", "data.frame", "default", "factor", "matrix", "proc_time", 
"srcfile", "srcref", "table", "warnings", "data.frame", "default", 
"default", "data.frame", "default", "Date", "POSIXt", "connection", 
"POSIXlt", "array", "data.frame", "default", "matrix", "numeric_version", 
"warnings", "difftime", "difftime", "Date", "POSIXt", "default", 
"data.frame", "list", "AsIs", "Date", "POSIXct", "POSIXlt", "default", 
"difftime", "factor", "numeric_version"), .Dim = c(369L, 2L), .Dimnames = list(
    NULL, c("generic", "class")))


.row_names_info <- function (x, type = 1L) 
.Internal(shortRowNames(x, type))


standardGeneric <- function (f, fdef)  .Primitive("standardGeneric")


.F_dqrdc2 <- structure(list(name = "dqrdc2", address = pointer("0x000000000df13770"), 
    dll = structure(list(name = "base", path = "base", dynamicLookup = FALSE, 
        handle = <pointer: (nil)>, info = pointer("0x000000000df73cc0")), class = "DLLInfo"), 
    numParameters = 9L), class = c("FortranRoutine", "NativeSymbolInfo"
))


`[.Dlist` <- function (x, i, ...) 
structure(NextMethod("["), class = class(x))


as.data.frame.Date <- function (x, row.names = NULL, optional = FALSE, ..., nm = paste(deparse(substitute(x), 
    width.cutoff = 500L), collapse = " ")) 
{
    force(nm)
    nrows <- length(x)
    if (!(is.null(row.names) || (is.character(row.names) && length(row.names) == 
        nrows))) {
        warning(gettextf("'row.names' is not a character vector of length %d -- omitting it. Will be an error!", 
            nrows), domain = NA)
        row.names <- NULL
    }
    if (is.null(row.names)) {
        if (nrows == 0L) 
            row.names <- character()
        else if (length(row.names <- names(x)) != nrows || anyDuplicated(row.names)) 
            row.names <- .set_row_names(nrows)
    }
    if (!is.null(names(x))) 
        names(x) <- NULL
    value <- list(x)
    if (!optional) 
        names(value) <- nm
    structure(value, row.names = row.names, class = "data.frame")
}


R.home <- function (component = "home") 
{
    rh <- .Internal(R.home())
    switch(component, home = rh, bin = if (.Platform$OS.type == 
        "windows" && nzchar(p <- .Platform$r_arch)) file.path(rh, 
        component, p) else file.path(rh, component), share = if (nzchar(p <- Sys.getenv("R_SHARE_DIR"))) p else file.path(rh, 
        component), doc = if (nzchar(p <- Sys.getenv("R_DOC_DIR"))) p else file.path(rh, 
        component), include = if (nzchar(p <- Sys.getenv("R_INCLUDE_DIR"))) p else file.path(rh, 
        component), modules = if (nzchar(p <- .Platform$r_arch)) file.path(rh, 
        component, p) else file.path(rh, component), file.path(rh, 
        component))
}


unique.default <- function (x, incomparables = FALSE, fromLast = FALSE, nmax = NA, 
    ...) 
{
    if (is.factor(x)) {
        z <- .Internal(unique(x, incomparables, fromLast, min(length(x), 
            nlevels(x) + 1L)))
        return(factor(z, levels = seq_len(nlevels(x)), labels = levels(x), 
            ordered = is.ordered(x)))
    }
    z <- .Internal(unique(x, incomparables, fromLast, nmax))
    if (inherits(x, "POSIXct")) 
        structure(z, class = class(x), tzone = attr(x, "tzone"))
    else if (inherits(x, "Date")) 
        structure(z, class = class(x))
    else z
}


sys.frame <- function (which = 0L) 
.Internal(sys.frame(which))


sys.source <- function (file, envir = baseenv(), chdir = FALSE, keep.source = getOption("keep.source.pkgs"), 
    keep.parse.data = getOption("keep.parse.data.pkgs"), toplevel.env = as.environment(envir)) 
{
    if (!(is.character(file) && file.exists(file))) 
        stop(gettextf("'%s' is not an existing file", file))
    keep.source <- as.logical(keep.source)
    keep.parse.data <- as.logical(keep.parse.data)
    oop <- options(keep.source = keep.source, keep.parse.data = keep.parse.data, 
        topLevelEnvironment = toplevel.env)
    on.exit(options(oop))
    if (keep.source) {
        lines <- readLines(file, warn = FALSE)
        srcfile <- srcfilecopy(file, lines, file.mtime(file), 
            isFile = TRUE)
        exprs <- parse(text = lines, srcfile = srcfile, keep.source = TRUE)
    }
    else exprs <- parse(n = -1, file = file, srcfile = NULL, 
        keep.source = FALSE)
    if (length(exprs) == 0L) 
        return(invisible())
    if (chdir && (path <- dirname(file)) != ".") {
        owd <- getwd()
        if (is.null(owd)) 
            stop("cannot 'chdir' as current directory is unknown")
        on.exit(setwd(owd), add = TRUE)
        setwd(path)
    }
    for (i in seq_along(exprs)) eval(exprs[i], envir)
    invisible()
}


as.double.POSIXlt <- function (x, ...) 
as.double(as.POSIXct(x))


clearPushBack <- function (connection) 
.Internal(clearPushBack(connection))


.leap.seconds <- structure(c(78796800, 94694400, 126230400, 157766400, 189302400, 
220924800, 252460800, 283996800, 315532800, 362793600, 394329600, 
425865600, 489024000, 567993600, 631152000, 662688000, 709948800, 
741484800, 773020800, 820454400, 867715200, 915148800, 1136073600, 
1230768000, 1341100800, 1435708800, 1483228800), class = c("POSIXct", 
"POSIXt"))


.mapply <- function (FUN, dots, MoreArgs) 
.Internal(mapply(FUN, dots, MoreArgs))


bindtextdomain <- function (domain, dirname = NULL) 
.Internal(bindtextdomain(domain, dirname))


lgamma <- function (x)  .Primitive("lgamma")


aperm.table <- function (a, perm = NULL, resize = TRUE, keep.class = TRUE, ...) 
{
    r <- aperm.default(a, perm, resize = resize)
    if (keep.class) 
        class(r) <- class(a)
    r
}


getLoadedDLLs <- function () 
.Internal(getLoadedDLLs())


subset.data.frame <- function (x, subset, select, drop = FALSE, ...) 
{
    r <- if (missing(subset)) 
        rep_len(TRUE, nrow(x))
    else {
        e <- substitute(subset)
        r <- eval(e, x, parent.frame())
        if (!is.logical(r)) 
            stop("'subset' must be logical")
        r & !is.na(r)
    }
    vars <- if (missing(select)) 
        TRUE
    else {
        nl <- as.list(seq_along(x))
        names(nl) <- names(x)
        eval(substitute(select), nl, parent.frame())
    }
    x[r, vars, drop = drop]
}


as.Date.factor <- function (x, ...) 
as.Date(as.character(x), ...)


format.libraryIQR <- function (x, ...) 
{
    db <- x$results
    if (!nrow(db)) 
        return(character())
    libs <- db[, "LibPath"]
    libs <- factor(libs, levels = unique(libs))
    out <- lapply(split(1:nrow(db), libs), function(ind) db[ind, 
        c("Package", "Title"), drop = FALSE])
    c(unlist(Map(function(lib, sep) {
        c(gettextf("%sPackages in library %s:\n", sep, sQuote(lib)), 
            formatDL(out[[lib]][, "Package"], out[[lib]][, "Title"]))
    }, names(out), c("", rep.int("\n", length(out) - 1L)))), 
        x$footer)
}


unique <- function (x, incomparables = FALSE, ...) 
UseMethod("unique")


as.data.frame.difftime <- function (x, row.names = NULL, optional = FALSE, ..., nm = paste(deparse(substitute(x), 
    width.cutoff = 500L), collapse = " ")) 
{
    force(nm)
    nrows <- length(x)
    if (!(is.null(row.names) || (is.character(row.names) && length(row.names) == 
        nrows))) {
        warning(gettextf("'row.names' is not a character vector of length %d -- omitting it. Will be an error!", 
            nrows), domain = NA)
        row.names <- NULL
    }
    if (is.null(row.names)) {
        if (nrows == 0L) 
            row.names <- character()
        else if (length(row.names <- names(x)) != nrows || anyDuplicated(row.names)) 
            row.names <- .set_row_names(nrows)
    }
    if (!is.null(names(x))) 
        names(x) <- NULL
    value <- list(x)
    if (!optional) 
        names(value) <- nm
    structure(value, row.names = row.names, class = "data.frame")
}


merge.default <- function (x, y, ...) 
merge(as.data.frame(x), as.data.frame(y), ...)


ifelse <- function (test, yes, no) 
{
    if (is.atomic(test)) {
        if (typeof(test) != "logical") 
            storage.mode(test) <- "logical"
        if (length(test) == 1 && is.null(attributes(test))) {
            if (is.na(test)) 
                return(NA)
            else if (test) {
                if (length(yes) == 1) {
                  yat <- attributes(yes)
                  if (is.null(yat) || (is.function(yes) && identical(names(yat), 
                    "srcref"))) 
                    return(yes)
                }
            }
            else if (length(no) == 1) {
                nat <- attributes(no)
                if (is.null(nat) || (is.function(no) && identical(names(nat), 
                  "srcref"))) 
                  return(no)
            }
        }
    }
    else test <- if (isS4(test)) 
        methods::as(test, "logical")
    else as.logical(test)
    ans <- test
    len <- length(ans)
    ypos <- which(test)
    npos <- which(!test)
    if (length(ypos) > 0L) 
        ans[ypos] <- rep(yes, length.out = len)[ypos]
    if (length(npos) > 0L) 
        ans[npos] <- rep(no, length.out = len)[npos]
    ans
}


is.atomic <- function (x)  .Primitive("is.atomic")


`+.Date` <- function (e1, e2) 
{
    coerceTimeUnit <- function(x) as.vector(round(switch(attr(x, 
        "units"), secs = x/86400, mins = x/1440, hours = x/24, 
        days = x, weeks = 7 * x)))
    if (nargs() == 1L) 
        return(e1)
    if (inherits(e1, "Date") && inherits(e2, "Date")) 
        stop("binary + is not defined for \"Date\" objects")
    if (inherits(e1, "difftime")) 
        e1 <- coerceTimeUnit(e1)
    if (inherits(e2, "difftime")) 
        e2 <- coerceTimeUnit(e2)
    .Date(unclass(e1) + unclass(e2))
}


sys.parents <- function () 
.Internal(sys.parents())


body <- function (fun = sys.function(sys.parent())) 
{
    if (is.character(fun)) 
        fun <- get(fun, mode = "function", envir = parent.frame())
    .Internal(body(fun))
}


as.character.default <- function (x, ...) 
.Internal(as.vector(x, "character"))


rep.int <- function (x, times) 
.Internal(rep.int(x, times))


as.data.frame.array <- function (x, row.names = NULL, optional = FALSE, ...) 
{
    d <- dim(x)
    if (length(d) == 1L) {
        value <- as.data.frame.vector(drop(x), row.names, optional, 
            ...)
        if (!optional) 
            names(value) <- deparse(substitute(x))[[1L]]
        value
    }
    else if (length(d) == 2L) {
        as.data.frame.matrix(x, row.names, optional, ...)
    }
    else {
        dn <- dimnames(x)
        dim(x) <- c(d[1L], prod(d[-1L]))
        if (!is.null(dn)) {
            if (length(dn[[1L]])) 
                rownames(x) <- dn[[1L]]
            for (i in 2L:length(d)) if (is.null(dn[[i]])) 
                dn[[i]] <- seq_len(d[i])
            colnames(x) <- interaction(expand.grid(dn[-1L]))
        }
        as.data.frame.matrix(x, row.names, optional, ...)
    }
}


xtfrm.Date <- function (x) 
as.numeric(x)


norm <- function (x, type = c("O", "I", "F", "M", "2")) 
{
    if (identical("2", type)) {
        svd(x, nu = 0L, nv = 0L)$d[1L]
    }
    else .Internal(La_dlange(x, type))
}


`split<-.data.frame` <- function (x, f, drop = FALSE, ..., value) 
{
    ix <- split(seq_len(nrow(x)), f, drop = drop, ...)
    n <- length(value)
    j <- 0
    for (i in ix) {
        j <- j%%n + 1
        x[i, ] <- value[[j]]
    }
    x
}


as.list.factor <- function (x, ...) 
{
    res <- vector("list", length(x))
    for (i in seq_along(x)) res[[i]] <- x[i]
    res
}


retracemem <- function (x, previous = NULL)  .Primitive("retracemem")


local <- function (expr, envir = new.env()) 
eval.parent(substitute(eval(quote(expr), envir)))


expression <- function (...)  .Primitive("expression")


Reduce <- function (f, x, init, right = FALSE, accumulate = FALSE) 
{
    mis <- missing(init)
    len <- length(x)
    if (len == 0L) 
        return(if (mis) NULL else init)
    f <- match.fun(f)
    if (!is.vector(x) || is.object(x)) 
        x <- as.list(x)
    ind <- seq_len(len)
    if (mis) {
        if (right) {
            init <- x[[len]]
            ind <- ind[-len]
        }
        else {
            init <- x[[1L]]
            ind <- ind[-1L]
        }
    }
    if (!accumulate) {
        if (right) {
            for (i in rev(ind)) init <- forceAndCall(2, f, x[[i]], 
                init)
        }
        else {
            for (i in ind) init <- forceAndCall(2, f, init, x[[i]])
        }
        init
    }
    else {
        len <- length(ind) + 1L
        out <- vector("list", len)
        if (mis) {
            if (right) {
                out[[len]] <- init
                for (i in rev(ind)) {
                  init <- forceAndCall(2, f, x[[i]], init)
                  out[[i]] <- init
                }
            }
            else {
                out[[1L]] <- init
                for (i in ind) {
                  init <- forceAndCall(2, f, init, x[[i]])
                  out[[i]] <- init
                }
            }
        }
        else {
            if (right) {
                out[[len]] <- init
                for (i in rev(ind)) {
                  init <- forceAndCall(2, f, x[[i]], init)
                  out[[i]] <- init
                }
            }
            else {
                for (i in ind) {
                  out[[i]] <- init
                  init <- forceAndCall(2, f, init, x[[i]])
                }
                out[[len]] <- init
            }
        }
        if (all(lengths(out) == 1L)) 
            out <- unlist(out, recursive = FALSE)
        out
    }
}


signalCondition <- function (cond) 
{
    if (!inherits(cond, "condition")) 
        cond <- simpleCondition(cond)
    msg <- conditionMessage(cond)
    call <- conditionCall(cond)
    .Internal(.signalCondition(cond, msg, call))
}


validEnc <- function (x) 
.Internal(validEnc(x))


log10 <- function (x)  .Primitive("log10")


isSymmetric <- function (object, ...) 
UseMethod("isSymmetric")


log1p <- function (x)  .Primitive("log1p")


is.expression <- function (x)  .Primitive("is.expression")


is.unsorted <- function (x, na.rm = FALSE, strictly = FALSE) 
{
    if (length(x) <= 1L) 
        return(FALSE)
    if (!na.rm && anyNA(x)) 
        return(NA)
    if (na.rm && any(ii <- is.na(x))) 
        x <- x[!ii]
    .Internal(is.unsorted(x, strictly))
}


droplevels.data.frame <- function (x, except = NULL, exclude, ...) 
{
    ix <- vapply(x, is.factor, NA)
    if (!is.null(except)) 
        ix[except] <- FALSE
    x[ix] <- if (missing(exclude)) 
        lapply(x[ix], droplevels)
    else lapply(x[ix], droplevels, exclude = exclude)
    x
}


sys.parent <- function (n = 1L) 
.Internal(sys.parent(n))


call <- function (name, ...)  .Primitive("call")


is.object <- function (x)  .Primitive("is.object")


pos.to.env <- function (x)  .Primitive("pos.to.env")


as.POSIXlt.character <- function (x, tz = "", format, tryFormats = c("%Y-%m-%d %H:%M:%OS", 
    "%Y/%m/%d %H:%M:%OS", "%Y-%m-%d %H:%M", "%Y/%m/%d %H:%M", 
    "%Y-%m-%d", "%Y/%m/%d"), optional = FALSE, ...) 
{
    x <- unclass(x)
    if (!missing(format)) {
        res <- strptime(x, format, tz = tz)
        if (nzchar(tz)) 
            attr(res, "tzone") <- tz
        return(res)
    }
    xx <- x[!is.na(x)]
    if (!length(xx)) {
        res <- strptime(x, "%Y/%m/%d")
        if (nzchar(tz)) 
            attr(res, "tzone") <- tz
        return(res)
    }
    else for (f in tryFormats) if (all(!is.na(strptime(xx, f, 
        tz = tz)))) {
        res <- strptime(x, f, tz = tz)
        if (nzchar(tz)) 
            attr(res, "tzone") <- tz
        return(res)
    }
    if (optional) 
        as.POSIXlt.character(rep.int(NA_character_, length(x)), 
            tz = tz)
    else stop("character string is not in a standard unambiguous format")
}


`attributes<-` <- function (x, value)  .Primitive("attributes<-")


sweep <- function (x, MARGIN, STATS, FUN = "-", check.margin = TRUE, ...) 
{
    FUN <- match.fun(FUN)
    dims <- dim(x)
    if (check.margin) {
        dimmargin <- dims[MARGIN]
        dimstats <- dim(STATS)
        lstats <- length(STATS)
        if (lstats > prod(dimmargin)) {
            warning("STATS is longer than the extent of 'dim(x)[MARGIN]'")
        }
        else if (is.null(dimstats)) {
            cumDim <- c(1L, cumprod(dimmargin))
            upper <- min(cumDim[cumDim >= lstats])
            lower <- max(cumDim[cumDim <= lstats])
            if (lstats && (upper%%lstats != 0L || lstats%%lower != 
                0L)) 
                warning("STATS does not recycle exactly across MARGIN")
        }
        else {
            dimmargin <- dimmargin[dimmargin > 1L]
            dimstats <- dimstats[dimstats > 1L]
            if (length(dimstats) != length(dimmargin) || any(dimstats != 
                dimmargin)) 
                warning("length(STATS) or dim(STATS) do not match dim(x)[MARGIN]")
        }
    }
    perm <- c(MARGIN, seq_along(dims)[-MARGIN])
    FUN(x, aperm(array(STATS, dims[perm]), order(perm)), ...)
}


.C_R_removeTaskCallback <- structure(list(name = "R_removeTaskCallback", address = pointer("0x000000000df135b0"), 
    dll = structure(list(name = "base", path = "base", dynamicLookup = FALSE, 
        handle = <pointer: (nil)>, info = pointer("0x000000000df73cc0")), class = "DLLInfo"), 
    numParameters = 1L), class = c("CallRoutine", "NativeSymbolInfo"
))


.libPaths <- function (new) 
{
    if (!missing(new)) {
        new <- Sys.glob(path.expand(new))
        paths <- c(new, .Library.site, .Library)
        paths <- paths[dir.exists(paths)]
        .lib.loc <<- unique(normalizePath(paths, "/"))
    }
    else .lib.loc
}


.getNamespaceInfo <- function (ns, which) 
{
    ns[[".__NAMESPACE__."]][[which]]
}


trunc.POSIXt <- function (x, units = c("secs", "mins", "hours", "days", "months", 
    "years"), ...) 
{
    units <- match.arg(units)
    x <- as.POSIXlt(x)
    if (length(x$sec)) 
        switch(units, secs = {
            x$sec <- trunc(x$sec)
        }, mins = {
            x$sec[] <- 0
        }, hours = {
            x$sec[] <- 0
            x$min[] <- 0L
        }, days = {
            x$sec[] <- 0
            x$min[] <- 0L
            x$hour[] <- 0L
            x$isdst[] <- -1L
        }, months = {
            x$sec[] <- 0
            x$min[] <- 0L
            x$hour[] <- 0L
            x$mday[] <- 1L
            x$isdst[] <- -1L
            x <- as.POSIXlt(as.POSIXct(x))
        }, years = {
            x$sec[] <- 0
            x$min[] <- 0L
            x$hour[] <- 0L
            x$mday[] <- 1L
            x$mon[] <- 0L
            x$isdst[] <- -1L
            x <- as.POSIXlt(as.POSIXct(x))
        })
    x
}


encodeString <- function (x, width = 0L, quote = "", na.encode = TRUE, justify = c("left", 
    "right", "centre", "none")) 
{
    at <- attributes(x)
    x <- as.character(x)
    attributes(x) <- at
    oldClass(x) <- NULL
    justify <- match(match.arg(justify), c("left", "right", "centre", 
        "none")) - 1L
    .Internal(encodeString(x, width, quote, justify, na.encode))
}


trunc <- function (x, ...)  .Primitive("trunc")


system.time <- function (expr, gcFirst = TRUE) 
{
    ppt <- function(y) {
        if (!is.na(y[4L])) 
            y[1L] <- y[1L] + y[4L]
        if (!is.na(y[5L])) 
            y[2L] <- y[2L] + y[5L]
        paste(formatC(y[1L:3L]), collapse = " ")
    }
    if (gcFirst) 
        gc(FALSE)
    time <- proc.time()
    on.exit(message("Timing stopped at: ", ppt(proc.time() - 
        time)))
    expr
    new.time <- proc.time()
    on.exit()
    structure(new.time - time, class = "proc_time")
}


all.equal.POSIXt <- function (target, current, ..., tolerance = 0.001, scale) 
{
    target <- as.POSIXct(target)
    current <- as.POSIXct(current)
    check_tzones(target, current)
    attr(target, "tzone") <- attr(current, "tzone") <- NULL
    all.equal.numeric(target, current, ..., tolerance = tolerance, 
        scale = 1)
}


`[.hexmode` <- function (x, i) 
{
    cl <- oldClass(x)
    y <- NextMethod("[")
    oldClass(y) <- cl
    y
}


bitwShiftR <- function (a, n) 
.Internal(bitwiseShiftR(a, n))


nrow <- function (x) 
dim(x)[1L]


bitwShiftL <- function (a, n) 
.Internal(bitwiseShiftL(a, n))


print.restart <- function (x, ...) 
{
    cat(paste("<restart:", x[[1L]], ">\n"))
    invisible(x)
}


RNGversion <- function (vstr) 
{
    vnum <- as.numeric(strsplit(as.character(vstr), ".", fixed = TRUE)[[1L]])
    if (length(vnum) < 2L) 
        stop("malformed version string")
    if (vnum[1L] == 0 && vnum[2L] < 99) 
        RNGkind("Wichmann-Hill", "Buggy Kinderman-Ramage", "Rounding")
    else if (vnum[1L] == 0 || vnum[1L] == 1 && vnum[2L] <= 6) 
        RNGkind("Marsaglia-Multicarry", "Buggy Kinderman-Ramage", 
            "Rounding")
    else if (vnum[1L] <= 2 || vnum[1L] == 3 && vnum[2L] <= 5) 
        RNGkind("Mersenne-Twister", "Inversion", "Rounding")
    else RNGkind("Mersenne-Twister", "Inversion", "Rejection")
}


sort.POSIXlt <- function (x, decreasing = FALSE, na.last = NA, ...) 
x[order(as.POSIXct(x), na.last = na.last, decreasing = decreasing)]


print.POSIXct <- function (x, tz = "", usetz = TRUE, max = NULL, ...) 
{
    if (is.null(max)) 
        max <- getOption("max.print", 9999L)
    FORM <- if (missing(tz)) 
        function(z) format(z, usetz = usetz)
    else function(z) format(z, tz = tz, usetz = usetz)
    if (max < length(x)) {
        print(FORM(x[seq_len(max)]), max = max + 1, ...)
        cat(" [ reached 'max' / getOption(\"max.print\") -- omitted", 
            length(x) - max, "entries ]\n")
    }
    else if (length(x)) 
        print(FORM(x), max = max, ...)
    else cat(class(x)[1L], "of length 0\n")
    invisible(x)
}


`[.table` <- function (x, i, j, ..., drop = TRUE) 
{
    ret <- NextMethod()
    ldr <- length(dim(ret))
    if ((ldr > 1L) || (ldr == length(dim(x)))) 
        class(ret) <- "table"
    ret
}


print.POSIXlt <- function (x, tz = "", usetz = TRUE, max = NULL, ...) 
{
    if (is.null(max)) 
        max <- getOption("max.print", 9999L)
    FORM <- if (missing(tz)) 
        function(z) format(z, usetz = usetz)
    else function(z) format(z, tz = tz, usetz = usetz)
    if (max < length(x)) {
        print(FORM(x[seq_len(max)]), max = max + 1, ...)
        cat(" [ reached 'max' / getOption(\"max.print\") -- omitted", 
            length(x) - max, "entries ]\n")
    }
    else if (length(x)) 
        print(FORM(x), max = max, ...)
    else cat(class(x)[1L], "of length 0\n")
    invisible(x)
}


withVisible <- function (x) 
.Internal(withVisible(x))


file.create <- function (..., showWarnings = TRUE) 
.Internal(file.create(c(...), showWarnings))


getHook <- function (hookName) 
get0(hookName, envir = .userHooksEnv, inherits = FALSE, ifnotfound = list())


as.data.frame.list <- function (x, row.names = NULL, optional = FALSE, ..., cut.names = FALSE, 
    col.names = names(x), fix.empty.names = TRUE, stringsAsFactors = default.stringsAsFactors()) 
{
    new.nms <- !missing(col.names)
    if (cut.names) {
        maxL <- if (is.logical(cut.names)) 
            256L
        else as.integer(cut.names)
        if (any(long <- nchar(col.names, "bytes", keepNA = FALSE) > 
            maxL)) 
            col.names[long] <- paste(substr(col.names[long], 
                1L, maxL - 6L), "...")
        else cut.names <- FALSE
    }
    m <- match(names(formals(data.frame))[-1L], col.names, 0L)
    if (any.m <- any(m)) 
        col.names[m] <- paste0("..adfl.", col.names[m])
    if (new.nms || any.m || cut.names) 
        names(x) <- col.names
    if (is.null(check.n <- list(...)$check.names)) 
        check.n <- !optional
    alis <- c(list(check.names = check.n, fix.empty.names = fix.empty.names, 
        stringsAsFactors = stringsAsFactors), if (!is.null(row.names)) list(row.names = row.names))
    x <- do.call(data.frame, c(x, alis))
    if (any.m) 
        names(x) <- sub("^\\.\\.adfl\\.", "", names(x))
    x
}


row.names.default <- function (x) 
if (!is.null(dim(x))) rownames(x)


print.AsIs <- function (x, ...) 
{
    cl <- oldClass(x)
    oldClass(x) <- cl[cl != "AsIs"]
    NextMethod("print")
    invisible(x)
}


Filter <- function (f, x) 
{
    ind <- as.logical(unlist(lapply(x, f)))
    x[which(ind)]
}


options <- function (...) 
.Internal(options(...))


single <- function (length = 0L) 
structure(vector("double", length), Csingle = TRUE)


attr.all.equal <- function (target, current, ..., check.attributes = TRUE, check.names = TRUE) 
{
    if (!is.logical(check.attributes)) 
        stop(gettextf("'%s' must be logical", "check.attributes"), 
            domain = NA)
    if (!is.logical(check.names)) 
        stop(gettextf("'%s' must be logical", "check.names"), 
            domain = NA)
    msg <- NULL
    if (mode(target) != mode(current)) 
        msg <- paste0("Modes: ", mode(target), ", ", mode(current))
    if (length(target) != length(current)) 
        msg <- c(msg, paste0("Lengths: ", length(target), ", ", 
            length(current)))
    ax <- attributes(target)
    ay <- attributes(current)
    if (check.names) {
        nx <- names(target)
        ny <- names(current)
        if ((lx <- length(nx)) | (ly <- length(ny))) {
            ax$names <- ay$names <- NULL
            if (lx && ly) {
                if (is.character(m <- all.equal.character(nx, 
                  ny, check.attributes = check.attributes))) 
                  msg <- c(msg, paste("Names:", m))
            }
            else if (lx) 
                msg <- c(msg, "names for target but not for current")
            else msg <- c(msg, "names for current but not for target")
        }
    }
    else {
        ax[["names"]] <- NULL
        ay[["names"]] <- NULL
    }
    if (check.attributes && (length(ax) || length(ay))) {
        nx <- names(ax)
        ny <- names(ay)
        if (length(nx)) 
            ax <- ax[order(nx)]
        if (length(ny)) 
            ay <- ay[order(ny)]
        tt <- all.equal(ax, ay, ..., check.attributes = check.attributes)
        if (is.character(tt)) 
            msg <- c(msg, paste("Attributes: <", tt, ">"))
    }
    msg
}


`length<-.POSIXct` <- function (x, value) 
.POSIXct(NextMethod(), attr(x, "tzone"), oldClass(x))


.primUntrace <- function (obj)  .Primitive(".primUntrace")


factorial <- function (x) 
gamma(x + 1)


lfactorial <- function (x) 
lgamma(x + 1)


`length<-.POSIXlt` <- function (x, value) 
.POSIXlt(lapply(unclass(x), `length<-`, value), attr(x, "tzone"), 
    oldClass(x))


colnames <- function (x, do.NULL = TRUE, prefix = "col") 
{
    if (is.data.frame(x) && do.NULL) 
        return(names(x))
    dn <- dimnames(x)
    if (!is.null(dn[[2L]])) 
        dn[[2L]]
    else {
        nc <- NCOL(x)
        if (do.NULL) 
            NULL
        else if (nc > 0L) 
            paste0(prefix, seq_len(nc))
        else character()
    }
}


dontCheck <- function (x) 
x


.Options <- pairlist(prompt = "> ", continue = "+ ", expressions = 5000L, width = 80L, deparse.cutoff = 60L, digits = 7L, echo = FALSE, verbose = FALSE, check.bounds = FALSE, keep.source = FALSE, keep.source.pkgs = FALSE, keep.parse.data = TRUE, keep.parse.data.pkgs = FALSE, warning.length = 1000L, nwarnings = 50L, OutDec = ".", browserNLdisabled = FALSE, CBoundsCheck = FALSE, matprod = "default", PCRE_study = 10L, PCRE_use_JIT = TRUE, PCRE_limit_recursion = NA, warn = 0, timeout = 60, encoding = "native.enc", show.error.messages = TRUE, scipen = 0, max.print = 99999L, add.smooth = TRUE, stringsAsFactors = TRUE, showErrorCalls = TRUE, defaultPackages = c("datasets", 
"utils", "grDevices", "graphics", "stats", "methods"), papersize = "a4", pager = "internal", useFancyQuotes = FALSE, pdfviewer = "C:/PROGRA~1/R/R-36~1.0/bin/x64/open.exe", help_type = "html", help.try.all.packages = FALSE, help.search.types = c("vignette", 
"demo", "help"), citation.bibtex.max = 1L, internet.info = 2L, pkgType = "both", str = list(
    strict.width = "no", digits.d = 3L, vec.len = 4L), demo.ask = "default", example.ask = "default", HTTPUserAgent = "R (3.6.0 x86_64-w64-mingw32 x86_64 mingw32)", menu.graphics = TRUE, mailer = "mailto", install.packages.compile.from.source = "interactive", unzip = "internal", editor = "notepad", repos = c(CRAN = "@CRAN@"), locatorBell = TRUE, device.ask.default = FALSE, windowsTimeouts = c(100L, 
500L), device = function (file = if (onefile) "Rplots.pdf" else "Rplot%03d.pdf", 
    width, height, onefile, family, title, fonts, version, paper, 
    encoding, bg, fg, pointsize, pagecentre, colormodel, useDingbats, 
    useKerning, fillOddEven, compress) 
{
    initPSandPDFfonts()
    new <- list()
    if (!missing(width)) new$width <- width
    if (!missing(height)) new$height <- height
    if (!missing(onefile)) new$onefile <- onefile
    if (!missing(title)) new$title <- title
    if (!missing(fonts)) new$fonts <- fonts
    if (!missing(version)) new$version <- version
    if (!missing(paper)) new$paper <- paper
    if (!missing(encoding)) new$encoding <- encoding
    if (!missing(bg)) new$bg <- bg
    if (!missing(fg)) new$fg <- fg
    if (!missing(pointsize)) new$pointsize <- pointsize
    if (!missing(pagecentre)) new$pagecentre <- pagecentre
    if (!missing(colormodel)) new$colormodel <- colormodel
    if (!missing(useDingbats)) new$useDingbats <- useDingbats
    if (!missing(useKerning)) new$useKerning <- useKerning
    if (!missing(fillOddEven)) new$fillOddEven <- fillOddEven
    if (!missing(compress)) new$compress <- compress
    old <- check.options(new, name.opt = ".PDF.Options", envir = .PSenv)
    if (!missing(family) && (inherits(family, "Type1Font") || 
        inherits(family, "CIDFont"))) {
        enc <- family$encoding
        if (inherits(family, "Type1Font") && !is.null(enc) && 
            enc != "default" && (is.null(old$encoding) || old$encoding == 
            "default")) old$encoding <- enc
        family <- family$metrics
    }
    if (is.null(old$encoding) || old$encoding == "default") old$encoding <- guessEncoding()
    if (!missing(family)) {
        if (length(family) == 4L) {
            family <- c(family, "Symbol.afm")
        } else if (length(family) == 5L) {
        } else if (length(family) == 1L) {
            pf <- pdfFonts(family)[[1L]]
            if (is.null(pf)) stop(gettextf("unknown family '%s'", 
                family), domain = NA)
            matchFont(pf, old$encoding)
        } else stop("invalid 'family' argument")
        old$family <- family
    }
    version <- old$version
    versions <- c("1.1", "1.2", "1.3", "1.4", "1.5", "1.6", "1.7", 
        "2.0")
    if (version %in% versions) version <- as.integer(strsplit(version, 
        "[.]")[[1L]]) else stop("invalid PDF version")
    onefile <- old$onefile
    if (!checkIntFormat(file)) stop(gettextf("invalid 'file' argument '%s'", 
        file), domain = NA)
    .External(C_PDF, file, old$paper, old$family, old$encoding, 
        old$bg, old$fg, old$width, old$height, old$pointsize, 
        onefile, old$pagecentre, old$title, old$fonts, version[1L], 
        version[2L], old$colormodel, old$useDingbats, old$useKerning, 
        old$fillOddEven, old$compress)
    invisible()
}, contrasts = c(unordered = "contr.treatment", ordered = "contr.poly"
), na.action = "na.omit", show.coef.Pvalues = TRUE, show.signif.stars = TRUE, str.dendrogram.last = "`", ts.eps = 1e-05, ts.S.compat = FALSE)


all.equal.list <- function (target, current, ..., check.attributes = TRUE, use.names = TRUE) 
{
    if (!is.logical(check.attributes)) 
        stop(gettextf("'%s' must be logical", "check.attributes"), 
            domain = NA)
    if (!is.logical(use.names)) 
        stop(gettextf("'%s' must be logical", "use.names"), domain = NA)
    msg <- if (check.attributes) 
        attr.all.equal(target, current, ...)
    target <- unclass(target)
    current <- unclass(current)
    if (!is.list(target) && !is.vector(target)) 
        return(c(msg, "target is not list-like"))
    if (!is.list(current) && !is.vector(current)) 
        return(c(msg, "current is not list-like"))
    if ((n <- length(target)) != length(current)) {
        if (!is.null(msg)) 
            msg <- msg[-grep("\\bLengths\\b", msg)]
        n <- min(n, length(current))
        msg <- c(msg, paste("Length mismatch: comparison on first", 
            n, "components"))
    }
    iseq <- seq_len(n)
    if (use.names) 
        use.names <- (length(nt <- names(target)[iseq]) == n && 
            length(nc <- names(current)[iseq]) == n)
    for (i in iseq) {
        mi <- all.equal(target[[i]], current[[i]], check.attributes = check.attributes, 
            use.names = use.names, ...)
        if (is.character(mi)) 
            msg <- c(msg, paste0("Component ", if (use.names && 
                nt[i] == nc[i]) dQuote(nt[i]) else i, ": ", mi))
    }
    if (is.null(msg)) 
        TRUE
    else msg
}


levels <- function (x) 
UseMethod("levels")


.colMeans <- function (x, m, n, na.rm = FALSE) 
.Internal(colMeans(x, m, n, na.rm))


`levels<-` <- function (x, value)  .Primitive("levels<-")


labels <- function (object, ...) 
UseMethod("labels")


.dynLibs <- function (new) 
{
    if (!missing(new)) {
        class(new) <- "DLLInfoList"
        .Dyn.libs <<- new
    }
    else .Dyn.libs
}


isatty <- function (con) 
{
    if (!inherits(con, "terminal")) 
        FALSE
    else .Internal(isatty(con))
}


file.remove <- function (...) 
.Internal(file.remove(c(...)))


ordered <- function (x, ...) 
factor(x, ..., ordered = TRUE)


...length <- function ()  .Primitive("...length")


exists <- function (x, where = -1, envir = if (missing(frame)) as.environment(where) else sys.frame(frame), 
    frame, mode = "any", inherits = TRUE) 
.Internal(exists(x, envir, mode, inherits))


prettyNum <- function (x, big.mark = "", big.interval = 3L, small.mark = "", 
    small.interval = 5L, decimal.mark = getOption("OutDec"), 
    input.d.mark = decimal.mark, preserve.width = c("common", 
        "individual", "none"), zero.print = NULL, replace.zero = FALSE, 
    drop0trailing = FALSE, is.cmplx = NA, ...) 
{
    if (notChar <- !is.character(x)) {
        is.cmplx <- is.complex(x)
        x <- vapply(x, format, "", big.mark = big.mark, big.interval = big.interval, 
            small.mark = small.mark, small.interval = small.interval, 
            decimal.mark = decimal.mark, zero.print = zero.print, 
            drop0trailing = drop0trailing, ...)
    }
    nMark <- big.mark == "" && small.mark == "" && (notChar || 
        decimal.mark == input.d.mark)
    if (identical(big.mark, decimal.mark)) 
        warning(gettextf("'big.mark' and 'decimal.mark' are both '%s', which could be confusing", 
            big.mark), domain = NA)
    nZero <- is.null(zero.print) && !drop0trailing
    if (nMark && nZero) 
        return(x)
    if (nMark && !drop0trailing) 
        return(.format.zeros(x, zero.print, replace = replace.zero))
    if (is.na(is.cmplx)) {
        ina <- is.na(x) | x == "NA"
        is.cmplx <- if (all(ina)) 
            FALSE
        else any(grepl("[0-9].*[-+][0-9].*i$", x))
    }
    preserve.width <- match.arg(preserve.width)
    if (is.cmplx) {
        x <- .format.zeros(x, zero.print, replace = replace.zero)
        z.sp <- strsplit(sub("([0-9] *)([-+])( *[0-9])", "\\1::\\2::\\3", 
            x), "::", fixed = TRUE)
        i3 <- lengths(z.sp) == 3L
        if (any(i3)) {
            z.sp <- z.sp[i3]
            z.im <- vapply(z.sp, `[[`, "", 3L)
            has.i <- grep("i$", z.im)
            z.im[has.i] <- sub("i$", "", z.im[has.i])
            r <- lapply(list(vapply(z.sp, `[[`, "", 1L), z.im), 
                function(.) prettyNum(., big.mark = big.mark, 
                  big.interval = big.interval, small.mark = small.mark, 
                  small.interval = small.interval, decimal.mark = decimal.mark, 
                  input.d.mark = input.d.mark, preserve.width = preserve.width, 
                  zero.print = zero.print, replace.zero = replace.zero, 
                  drop0trailing = drop0trailing, is.cmplx = FALSE, 
                  ...))
            r[[2]][has.i] <- paste0(r[[2]][has.i], "i")
            x[i3] <- paste0(r[[1]], vapply(z.sp, `[[`, "", 2L), 
                r[[2]])
        }
        return(x)
    }
    if (nchar(input.d.mark) == 0) 
        stop("'input.d.mark' has no characters")
    x.sp <- strsplit(x, input.d.mark, fixed = TRUE)
    if (any(lengths(x.sp) > 2)) {
        x.sp <- lapply(x.sp, function(xs) {
            lx <- length(xs)
            if (lx <= 2) 
                xs
            else c(paste(xs[-lx], collapse = input.d.mark), xs[lx])
        })
    }
    B. <- vapply(x.sp, `[`, "", 1L)
    A. <- vapply(x.sp, `[`, "", 2L)
    if (any(iN <- is.na(A.))) 
        A.[iN] <- ""
    if (nzchar(big.mark) && length(i.big <- grep(paste0("[0-9]{", 
        big.interval + 1L, ",}"), B.))) {
        revStr <- function(cc) vapply(lapply(strsplit(cc, NULL), 
            rev), paste, "", collapse = "")
        B.[i.big] <- revStr(gsub(paste0("([0-9]{", big.interval, 
            "})\\B"), paste0("\\1", revStr(big.mark)), revStr(B.[i.big])))
    }
    if (nzchar(small.mark) && length(i.sml <- grep(paste0("[0-9]{", 
        small.interval + 1L, ",}"), A.))) {
        A.[i.sml] <- gsub(paste0("([0-9]{", small.interval, "}\\B)"), 
            paste0("\\1", small.mark), A.[i.sml])
    }
    if (drop0trailing) {
        a <- A.[!iN]
        if (length(hasE <- grep("e", a, fixed = TRUE))) {
            a[hasE] <- sub("e[+-]0+$", "", a[hasE])
            a[-hasE] <- sub("0+$", "", a[-hasE])
        }
        else a <- sub("0+$", "", a)
        A.[!iN] <- a
        iN <- !nzchar(A.)
    }
    A. <- .format.zeros(paste0(B., c(decimal.mark, "")[iN + 1L], 
        A.), zero.print, replace = replace.zero)
    if (preserve.width != "none") {
        nnc <- nchar(A., "c")
        d.len <- nnc - nchar(x, "c")
        if (any(ii <- d.len > 0L)) {
            switch(preserve.width, individual = {
                A.[ii] <- vapply(which(ii), function(i) sub(sprintf("^ {1,%d}", 
                  d.len[i]), "", A.[i]), "")
            }, common = {
                A. <- format(A., justify = "right")
            })
        }
    }
    attributes(A.) <- attributes(x)
    class(A.) <- NULL
    A.
}


isRestart <- function (x) 
inherits(x, "restart")


setTimeLimit <- function (cpu = Inf, elapsed = Inf, transient = FALSE) 
.Internal(setTimeLimit(cpu, elapsed, transient))


names.POSIXlt <- function (x) 
names(x$year)


.__H__.cbind <- function (..., deparse.level = 1) 
{
    .Deprecated("base::cbind")
    .Internal(cbind(deparse.level, ...))
}


list.dirs <- function (path = ".", full.names = TRUE, recursive = TRUE) 
.Internal(list.dirs(path, full.names, recursive))


truncate <- function (con, ...) 
UseMethod("truncate")


`formals<-` <- function (fun, envir = environment(fun), value) 
{
    if (!is.function(fun)) 
        warning("'fun' is not a function")
    bd <- body(fun)
    as.function(c(value, if (is.null(bd) || is.list(bd)) list(bd) else bd), 
        envir)
}


quarters <- function (x, abbreviate) 
UseMethod("quarters")


.External <- function (.NAME, ..., PACKAGE)  .Primitive(".External")


xtfrm.factor <- function (x) 
as.integer(x)


oldClass <- function (x)  .Primitive("oldClass")


as.function.default <- function (x, envir = parent.frame(), ...) 
if (is.function(x)) x else .Internal(as.function.default(x, envir))


chol <- function (x, ...) 
UseMethod("chol")


file.rename <- function (from, to) 
.Internal(file.rename(from, to))


loadedNamespaces <- function () 
names(.Internal(getNamespaceRegistry()))


all.equal.factor <- function (target, current, ..., check.attributes = TRUE) 
{
    if (!inherits(target, "factor")) 
        return("'target' is not a factor")
    if (!inherits(current, "factor")) 
        return("'current' is not a factor")
    msg <- if (check.attributes) 
        attr.all.equal(target, current, ...)
    n <- all.equal(as.character(target), as.character(current), 
        check.attributes = check.attributes, ...)
    if (is.character(n)) 
        msg <- c(msg, n)
    if (is.null(msg)) 
        TRUE
    else msg
}


.BaseNamespaceEnv <- "<environment>"

truncate.connection <- function (con, ...) 
{
    if (!isOpen(con)) 
        stop("can only truncate an open connection")
    .Internal(truncate(con))
}


is.finite <- function (x)  .Primitive("is.finite")


as.data.frame.numeric <- function (x, row.names = NULL, optional = FALSE, ..., nm = paste(deparse(substitute(x), 
    width.cutoff = 500L), collapse = " ")) 
{
    force(nm)
    nrows <- length(x)
    if (!(is.null(row.names) || (is.character(row.names) && length(row.names) == 
        nrows))) {
        warning(gettextf("'row.names' is not a character vector of length %d -- omitting it. Will be an error!", 
            nrows), domain = NA)
        row.names <- NULL
    }
    if (is.null(row.names)) {
        if (nrows == 0L) 
            row.names <- character()
        else if (length(row.names <- names(x)) != nrows || anyDuplicated(row.names)) 
            row.names <- .set_row_names(nrows)
    }
    if (!is.null(names(x))) 
        names(x) <- NULL
    value <- list(x)
    if (!optional) 
        names(value) <- nm
    structure(value, row.names = row.names, class = "data.frame")
}


Sys.junction <- function (from, to) 
{
    if (!(nf <- length(from))) 
        return(logical())
    if (!(nt <- length(to))) 
        stop("no files to link to")
    if (nt == 1 && dir.exists(to)) 
        to <- file.path(to, basename(from))
    else if (nf > nt) 
        stop("more 'from' files than 'to' files")
    else if (nf < nt) 
        stop("fewer 'from' files than 'to' files")
    okay <- rep(FALSE, nf)
    for (i in seq_len(nf)) {
        fr <- paste0("\\??\\", normalizePath(from[i]))
        link <- to[i]
        if (file.exists(link)) {
            warning(gettextf("link '%s' already exists", link), 
                domain = NA)
            next
        }
        if (!dir.create(link, showWarnings = FALSE)) {
            warning(gettextf("failed to create directory for link '%s", 
                link), domain = NA)
            next
        }
        if (.Internal(mkjunction(fr, link))) 
            okay[i] <- TRUE
    }
    okay
}


labels.default <- function (object, ...) 
{
    if (length(d <- dim(object))) {
        nt <- dimnames(object)
        if (is.null(nt)) 
            nt <- vector("list", length(d))
        for (i in seq_along(d)) if (!length(nt[[i]])) 
            nt[[i]] <- as.character(seq_len(d[i]))
    }
    else {
        nt <- names(object)
        if (!length(nt)) 
            nt <- as.character(seq_along(object))
    }
    nt
}


strwrap <- function (x, width = 0.9 * getOption("width"), indent = 0, exdent = 0, 
    prefix = "", simplify = TRUE, initial = prefix) 
{
    if (!is.character(x)) 
        x <- as.character(x)
    indentString <- strrep(" ", indent)
    exdentString <- strrep(" ", exdent)
    y <- list()
    enc <- Encoding(x)
    x <- enc2utf8(x)
    if (any(ind <- !validEnc(x))) 
        x[ind] <- iconv(x[ind], "UTF-8", "UTF-8", sub = "byte")
    z <- lapply(strsplit(x, "\n[ \t\n]*\n", perl = TRUE), strsplit, 
        "[ \t\n]", perl = TRUE)
    for (i in seq_along(z)) {
        yi <- character()
        for (j in seq_along(z[[i]])) {
            words <- z[[i]][[j]]
            nc <- nchar(words, type = "w")
            if (anyNA(nc)) {
                nc0 <- nchar(words, type = "b")
                nc[is.na(nc)] <- nc0[is.na(nc)]
            }
            if (any(nc == 0L)) {
                zLenInd <- which(nc == 0L)
                zLenInd <- zLenInd[!(zLenInd %in% (grep("[.?!][)\"']{0,1}$", 
                  words, perl = TRUE, useBytes = TRUE) + 1L))]
                if (length(zLenInd)) {
                  words <- words[-zLenInd]
                  nc <- nc[-zLenInd]
                }
            }
            if (!length(words)) {
                yi <- c(yi, "", initial)
                next
            }
            currentIndex <- 0L
            lowerBlockIndex <- 1L
            upperBlockIndex <- integer()
            lens <- cumsum(nc + 1L)
            first <- TRUE
            maxLength <- width - nchar(initial, type = "w") - 
                indent
            while (length(lens)) {
                k <- max(sum(lens <= maxLength), 1L)
                if (first) {
                  first <- FALSE
                  maxLength <- width - nchar(prefix, type = "w") - 
                    exdent
                }
                currentIndex <- currentIndex + k
                if (nc[currentIndex] == 0L) 
                  upperBlockIndex <- c(upperBlockIndex, currentIndex - 
                    1L)
                else upperBlockIndex <- c(upperBlockIndex, currentIndex)
                if (length(lens) > k) {
                  if (nc[currentIndex + 1L] == 0L) {
                    currentIndex <- currentIndex + 1L
                    k <- k + 1L
                  }
                  lowerBlockIndex <- c(lowerBlockIndex, currentIndex + 
                    1L)
                }
                if (length(lens) > k) 
                  lens <- lens[-seq_len(k)] - lens[k]
                else lens <- NULL
            }
            nBlocks <- length(upperBlockIndex)
            s <- paste0(c(initial, rep.int(prefix, nBlocks - 
                1L)), c(indentString, rep.int(exdentString, nBlocks - 
                1L)))
            initial <- prefix
            for (k in seq_len(nBlocks)) s[k] <- paste0(s[k], 
                paste(words[lowerBlockIndex[k]:upperBlockIndex[k]], 
                  collapse = " "))
            yi <- c(yi, s, prefix)
        }
        y <- if (length(yi)) 
            c(y, list(yi[-length(yi)]))
        else c(y, "")
    }
    if (length(pos <- which(enc == "latin1"))) {
        y[pos] <- lapply(y[pos], function(s) {
            e <- Encoding(s)
            if (length(p <- which(e == "UTF-8"))) 
                s[p] <- iconv(s[p], "UTF-8", "latin1", sub = "byte")
            s
        })
    }
    if (simplify) 
        y <- as.character(unlist(y))
    y
}


.Internal <- function (call)  .Primitive(".Internal")


tolower <- function (x) 
{
    if (!is.character(x)) 
        x <- as.character(x)
    .Internal(tolower(x))
}


gzfile <- function (description, open = "", encoding = getOption("encoding"), 
    compression = 6) 
.Internal(gzfile(description, open, encoding, compression))


.Fortran <- function (.NAME, ..., NAOK = FALSE, DUP = TRUE, PACKAGE, ENCODING)  .Primitive(".Fortran")


as.character.numeric_version <- function (x, ...) 
as.character(format(x))


env.profile <- function (env) 
.Internal(env.profile(env))


is.na <- function (x)  .Primitive("is.na")


Summary.POSIXct <- function (..., na.rm) 
{
    ok <- switch(.Generic, max = , min = , range = TRUE, FALSE)
    if (!ok) 
        stop(gettextf("'%s' not defined for \"POSIXt\" objects", 
            .Generic), domain = NA)
    args <- list(...)
    tz <- do.call("check_tzones", args)
    .POSIXct(NextMethod(.Generic), tz = tz, cl = oldClass(args[[1L]]))
}


open.connection <- function (con, open = "r", blocking = TRUE, ...) 
.Internal(open(con, open, blocking))


is.qr <- function (x) 
is.list(x) && inherits(x, "qr")


duplicated <- function (x, incomparables = FALSE, ...) 
UseMethod("duplicated")


writeBin <- function (object, con, size = NA_integer_, endian = .Platform$endian, 
    useBytes = FALSE) 
{
    swap <- endian != .Platform$endian
    if (!is.vector(object) || mode(object) == "list") 
        stop("can only write vector objects")
    if (is.character(con)) {
        con <- file(con, "wb")
        on.exit(close(con))
    }
    .Internal(writeBin(object, con, size, swap, useBytes))
}


flush.connection <- function (con) 
.Internal(flush(con))


rcond <- function (x, norm = c("O", "I", "1"), triangular = FALSE, ...) 
{
    norm <- match.arg(norm)
    stopifnot(is.matrix(x))
    if ({
        d <- dim(x)
        d[1L] != d[2L]
    }) 
        return(rcond(qr.R(qr(if (d[1L] < d[2L]) t(x) else x)), 
            norm = norm, ...))
    if (is.complex(x)) {
        if (triangular) 
            .Internal(La_ztrcon(x, norm))
        else .Internal(La_zgecon(x, norm))
    }
    else {
        if (triangular) 
            .Internal(La_dtrcon(x, norm))
        else .Internal(La_dgecon(x, norm))
    }
}


is.ordered <- function (x) 
inherits(x, "ordered")


`/.difftime` <- function (e1, e2) 
{
    if (inherits(e2, "difftime")) 
        stop("second argument of / cannot be a \"difftime\" object")
    .difftime(unclass(e1)/e2, attr(e1, "units"))
}


isSeekable <- function (con) 
.Internal(isSeekable(con))


as.ordered <- function (x) 
if (is.ordered(x)) x else ordered(x)


`[.simple.list` <- function (x, i, ...) 
structure(NextMethod("["), class = class(x))


iconv <- function (x, from = "", to = "", sub = NA, mark = TRUE, toRaw = FALSE) 
{
    if (!(is.character(x) || (is.list(x) && is.null(oldClass(x))))) 
        x <- as.character(x)
    .Internal(iconv(x, from, to, as.character(sub), mark, toRaw))
}


tempdir <- function (check = FALSE) 
.Internal(tempdir(check))


Summary.POSIXlt <- function (..., na.rm) 
{
    ok <- switch(.Generic, max = , min = , range = TRUE, FALSE)
    if (!ok) 
        stop(gettextf("'%s' not defined for \"POSIXt\" objects", 
            .Generic), domain = NA)
    args <- list(...)
    tz <- do.call("check_tzones", args)
    args <- lapply(args, as.POSIXct)
    val <- do.call(.Generic, c(args, na.rm = na.rm))
    as.POSIXlt(.POSIXct(val, tz))
}


is.numeric.difftime <- function (x) 
FALSE


print.octmode <- function (x, ...) 
{
    print(format(x), ...)
    invisible(x)
}


summary.proc_time <- function (object, ...) 
{
    if (!is.na(object[4L])) 
        object[1L] <- object[1L] + object[4L]
    if (!is.na(object[5L])) 
        object[2L] <- object[2L] + object[5L]
    object <- object[1L:3L]
    names(object) <- c(gettext("user"), gettext("system"), gettext("elapsed"))
    object
}


chol2inv <- function (x, size = NCOL(x), LINPACK = FALSE) 
.Internal(La_chol2inv(x, size))


as.character.Date <- function (x, ...) 
format(x, ...)


`$.package_version` <- function (x, name) 
{
    name <- pmatch(name, c("major", "minor", "patchlevel"))
    x <- unclass(x)
    switch(name, major = vapply(x, "[", 0L, 1L), minor = vapply(x, 
        "[", 0L, 2L), patchlevel = vapply(x, "[", 0L, 3L))
}


Sys.timezone <- function (location = TRUE) 
{
    if (!location) 
        .Deprecated(msg = "Sys.timezone(location = FALSE) is defunct and ignored")
    if (!is.na(tz <- get0(".sys.timezone", baseenv(), mode = "character", 
        inherits = FALSE, ifnotfound = NA_character_))) 
        return(tz)
    cacheIt <- function(tz) assign(".sys.timezone", tz, baseenv())
    tz <- Sys.getenv("TZ")
    if (nzchar(tz)) 
        return(tz)
    if (.Platform$OS.type == "windows") 
        return(.Internal(tzone_name()))
    tzdir <- Sys.getenv("TZDIR")
    if (nzchar(tzdir) && !dir.exists(tzdir)) 
        tzdir <- ""
    if (!nzchar(tzdir)) {
        if (dir.exists(tzdir <- "/usr/share/zoneinfo") || dir.exists(tzdir <- "/share/zoneinfo") || 
            dir.exists(tzdir <- "/usr/share/lib/zoneinfo") || 
            dir.exists(tzdir <- "/usrlib/zoneinfo") || dir.exists(tzdir <- "/usr/local/etc/zoneinfo") || 
            dir.exists(tzdir <- "/etc/zoneinfo") || dir.exists(tzdir <- "/usr/etc/zoneinfo")) {
        }
        else tzdir <- ""
    }
    if (nzchar(Sys.which("timedatectl"))) {
        inf <- system("timedatectl", intern = TRUE)
        lines <- grep("Time zone: ", inf)
        if (length(lines)) {
            tz <- sub(" .*", "", sub(" *Time zone: ", "", inf[lines[1L]]))
            if (nzchar(tzdir)) {
                if (file.exists(file.path(tzdir, tz))) {
                  cacheIt(tz)
                  return(tz)
                }
                else warning(sprintf("%s indicates the non-existent timezone name %s", 
                  sQuote("timedatectl"), sQuote(tz)), call. = FALSE, 
                  immediate. = TRUE, domain = NA)
            }
            else {
                cacheIt(tz)
                return(tz)
            }
        }
    }
    if (grepl("linux", R.Version()$platform, ignore.case = TRUE) && 
        file.exists("/etc/timezone")) {
        tz0 <- try(readLines("/etc/timezone"))
        if (!inherits(tz0, "try-error") && length(tz0) == 1L) {
            tz <- trimws(tz0)
            if (nzchar(tzdir)) {
                if (file.exists(file.path(tzdir, tz))) {
                  cacheIt(tz)
                  return(tz)
                }
                else warning(sprintf("%s indicates the non-existent timezone name %s", 
                  sQuote("/etc/timezone"), sQuote(tz)), call. = FALSE, 
                  immediate. = TRUE, domain = NA)
            }
            else {
                cacheIt(tz)
                return(tz)
            }
        }
    }
    if ((file.exists(lt0 <- "/etc/localtime") || file.exists(lt0 <- "/usr/local/etc/localtime") || 
        file.exists(lt0 <- "/usr/local/etc/zoneinfo/localtime") || 
        file.exists(lt0 <- "/var/db/timezone/localtime")) && 
        !is.na(lt <- Sys.readlink(lt0)) && nzchar(lt)) {
        tz <- NA_character_
        if ((nzchar(tzdir) && grepl(pat <- paste0("^", tzdir, 
            "/"), lt)) || grepl(pat <- "^/usr/share/zoneinfo.default/", 
            lt)) 
            tz <- sub(pat, "", lt)
        else if (grepl(pat <- ".*/zoneinfo/(.*)", lt)) 
            tz <- sub(pat, "\\1", lt)
        if (!is.na(tz)) {
            cacheIt(tz)
            return(tz)
        }
        else message("unable to deduce timezone name from ", 
            sQuote(lt))
    }
    if (nzchar(tzdir) && (is.na(lt <- Sys.readlink(lt0)) || !nzchar(lt))) {
        warning(sprintf("Your system is mis-configured: %s is not a symlink", 
            sQuote(lt0)), call. = FALSE, immediate. = TRUE, domain = NA)
        if (nzchar(Sys.which("cmp"))) {
            known <- dir(tzdir, recursive = TRUE)
            for (tz in known) {
                status <- system2("cmp", c("-s", lt0, file.path(tzdir, 
                  tz)))
                if (status == 0L) {
                  cacheIt(tz)
                  warning(sprintf("It is strongly recommended to set envionment variable TZ to %s (or equivalent)", 
                    sQuote(tz)), call. = FALSE, immediate. = TRUE, 
                    domain = NA)
                  return(tz)
                }
            }
            warning(sprintf("%s is not identical to any known timezone file", 
                sQuote(lt0)), call. = FALSE, immediate. = TRUE, 
                domain = NA)
        }
    }
    NA_character_
}


quarters.Date <- function (x, ...) 
{
    x <- (as.POSIXlt(x)$mon)%/%3L
    paste0("Q", x + 1L)
}


stopifnot <- function (..., exprs, local = TRUE) 
{
    n <- ...length()
    if (!missing(exprs)) {
        if (n) 
            stop("Must use 'exprs' or unnamed expressions, but not both")
        envir <- if (isTRUE(local)) 
            parent.frame()
        else if (isFALSE(local)) 
            .GlobalEnv
        else if (is.environment(local)) 
            local
        else stop("'local' must be TRUE, FALSE or an environment")
        exprs <- substitute(exprs)
        E1 <- if (is.call(exprs)) 
            exprs[[1]]
        cl <- if (is.symbol(E1) && (E1 == quote(`{`) || E1 == 
            quote(expression))) {
            exprs[[1]] <- quote(stopifnot)
            exprs
        }
        else as.call(c(quote(stopifnot), if (is.null(E1) && is.symbol(exprs) && 
            is.expression(E1 <- eval(exprs))) as.list(E1) else as.expression(exprs)))
        names(cl) <- NULL
        return(eval(cl, envir = envir))
    }
    Dparse <- function(call, cutoff = 60L) {
        ch <- deparse(call, width.cutoff = cutoff)
        if (length(ch) > 1L) 
            paste(ch[1L], "....")
        else ch
    }
    head <- function(x, n = 6L) x[seq_len(if (n < 0L) max(length(x) + 
        n, 0L) else min(n, length(x)))]
    abbrev <- function(ae, n = 3L) paste(c(head(ae, n), if (length(ae) > 
        n) "...."), collapse = "\n  ")
    for (i in seq_len(n)) {
        r <- ...elt(i)
        tmp <- if (FALSE) 
            eval(quote(1))
        if (!(is.logical(r) && !anyNA(r) && all(r))) {
            cl.i <- match.call()[[i + 1L]]
            msg <- if (is.call(cl.i) && identical(cl.i[[1]], 
                quote(all.equal)) && (is.null(ni <- names(cl.i)) || 
                length(cl.i) == 3L || length(cl.i <- cl.i[!nzchar(ni)]) == 
                3L)) 
                sprintf(gettext("%s and %s are not equal:\n  %s"), 
                  Dparse(cl.i[[2]]), Dparse(cl.i[[3]]), abbrev(r))
            else sprintf(ngettext(length(r), "%s is not TRUE", 
                "%s are not all TRUE"), Dparse(cl.i))
            stop(simpleError(msg, call = if (p <- sys.parent(1L)) 
                sys.call(p)))
        }
    }
    invisible()
}


as.data.frame.complex <- function (x, row.names = NULL, optional = FALSE, ..., nm = paste(deparse(substitute(x), 
    width.cutoff = 500L), collapse = " ")) 
{
    force(nm)
    nrows <- length(x)
    if (!(is.null(row.names) || (is.character(row.names) && length(row.names) == 
        nrows))) {
        warning(gettextf("'row.names' is not a character vector of length %d -- omitting it. Will be an error!", 
            nrows), domain = NA)
        row.names <- NULL
    }
    if (is.null(row.names)) {
        if (nrows == 0L) 
            row.names <- character()
        else if (length(row.names <- names(x)) != nrows || anyDuplicated(row.names)) 
            row.names <- .set_row_names(nrows)
    }
    if (!is.null(names(x))) 
        names(x) <- NULL
    value <- list(x)
    if (!optional) 
        names(value) <- nm
    structure(value, row.names = row.names, class = "data.frame")
}


packBits <- function (x, type = c("raw", "integer")) 
{
    type <- match.arg(type)
    .Internal(packBits(x, type))
}


c.POSIXct <- function (..., recursive = FALSE) 
.POSIXct(c(unlist(lapply(list(...), unclass))))


strrep <- function (x, times) 
{
    if (!is.character(x)) 
        x <- as.character(x)
    .Internal(strrep(x, as.integer(times)))
}


force <- function (x) 
x


c.POSIXlt <- function (..., recursive = FALSE) 
as.POSIXlt(do.call("c", lapply(list(...), as.POSIXct)))


`[<-.numeric_version` <- function (x, i, j, value) 
{
    y <- unclass(x)
    if (missing(j)) 
        y[i] <- unclass(as.numeric_version(value))
    else {
        if (!is.list(value)) 
            value <- list(value)
        value <- lapply(value, as.integer)
        if (any(vapply(value, function(e) anyNA(e) || any(e < 
            0L), NA))) 
            stop("invalid 'value'")
        if (!is.list(j)) 
            j <- list(j)
        y[i] <- Map(`[<-`, y[i], j, value)
    }
    class(y) <- class(x)
    y
}


mem.maxNSize <- function (nsize = 0) 
.Internal(mem.maxNSize(nsize))


format <- function (x, ...) 
UseMethod("format")


NextMethod <- function (generic = NULL, object = NULL, ...) 
.Internal(NextMethod(generic, object, ...))


..deparseOpts <- c("all", "keepInteger", "quoteExpressions", "showAttributes", 
"useSource", "warnIncomplete", "delayPromises", "keepNA", "S_compatible", 
"hexNumeric", "digits17", "niceNames")


.TAOCP1997init <- function (seed) 
{
    KK <- 100L
    LL <- 37L
    MM <- as.integer(2^30)
    KKK <- KK + KK - 1L
    KKL <- KK - LL
    ss <- seed - (seed%%2L) + 2L
    X <- integer(KKK)
    for (j in 1L:KK) {
        X[j] <- ss
        ss <- ss + ss
        if (ss >= MM) 
            ss <- ss - MM + 2L
    }
    X[2L] <- X[2L] + 1L
    ss <- seed
    T <- 69L
    while (T > 0) {
        for (j in KK:2L) X[j + j - 1L] <- X[j]
        for (j in seq(KKK, KKL + 1L, -2L)) X[KKK - j + 2L] <- X[j] - 
            (X[j]%%2L)
        for (j in KKK:(KK + 1L)) if (X[j]%%2L == 1L) {
            X[j - KKL] <- (X[j - KKL] - X[j])%%MM
            X[j - KK] <- (X[j - KK] - X[j])%%MM
        }
        if (ss%%2L == 1L) {
            for (j in KK:1L) X[j + 1L] <- X[j]
            X[1L] <- X[KK + 1L]
            if (X[KK + 1L]%%2L == 1L) 
                X[LL + 1L] <- (X[LL + 1L] - X[KK + 1L])%%MM
        }
        if (ss) 
            ss <- ss%/%2L
        else T <- T - 1L
    }
    rs <- c(X[(LL + 1L):KK], X[1L:LL])
    invisible(rs)
}


Vectorize <- function (FUN, vectorize.args = arg.names, SIMPLIFY = TRUE, USE.NAMES = TRUE) 
{
    arg.names <- as.list(formals(FUN))
    arg.names[["..."]] <- NULL
    arg.names <- names(arg.names)
    vectorize.args <- as.character(vectorize.args)
    if (!length(vectorize.args)) 
        return(FUN)
    if (!all(vectorize.args %in% arg.names)) 
        stop("must specify names of formal arguments for 'vectorize'")
    collisions <- arg.names %in% c("FUN", "SIMPLIFY", "USE.NAMES", 
        "vectorize.args")
    if (any(collisions)) 
        stop(sQuote("FUN"), " may not have argument(s) named ", 
            paste(sQuote(arg.names[collisions]), collapse = ", "))
    FUNV <- function() {
        args <- lapply(as.list(match.call())[-1L], eval, parent.frame())
        names <- if (is.null(names(args))) 
            character(length(args))
        else names(args)
        dovec <- names %in% vectorize.args
        do.call("mapply", c(FUN = FUN, args[dovec], MoreArgs = list(args[!dovec]), 
            SIMPLIFY = SIMPLIFY, USE.NAMES = USE.NAMES))
    }
    formals(FUNV) <- formals(FUN)
    FUNV
}


getNamespaceUsers <- function (ns) 
{
    nsname <- getNamespaceName(asNamespace(ns))
    users <- character()
    for (n in loadedNamespaces()) {
        inames <- names(getNamespaceImports(n))
        if (match(nsname, inames, 0L)) 
            users <- c(n, users)
    }
    users
}


cosh <- function (x)  .Primitive("cosh")


open <- function (con, ...) 
UseMethod("open")


qr.qty <- function (qr, y) 
{
    if (!is.qr(qr)) 
        stop("argument is not a QR decomposition")
    if (is.complex(qr$qr)) 
        return(.Internal(qr_qy_cmplx(qr, as.matrix(y), TRUE)))
    if (isTRUE(attr(qr, "useLAPACK"))) 
        return(.Internal(qr_qy_real(qr, as.matrix(y), TRUE)))
    n <- as.integer(nrow(qr$qr))
    if (is.na(n)) 
        stop("invalid nrow(qr$qr)")
    k <- as.integer(qr$rank)
    ny <- as.integer(NCOL(y))
    if (is.na(ny)) 
        stop("invalid NCOL(y)")
    if (NROW(y) != n) 
        stop("'qr' and 'y' must have the same number of rows")
    storage.mode(y) <- "double"
    .Fortran(.F_dqrqty, as.double(qr$qr), n, k, as.double(qr$qraux), 
        y, ny, qty = y)$qty
}


subset <- function (x, ...) 
UseMethod("subset")


print.summary.table <- function (x, digits = max(1L, getOption("digits") - 3L), ...) 
{
    if (!inherits(x, "summary.table")) 
        stop(gettextf("'x' must inherit from class %s", dQuote("summary.table")), 
            domain = NA)
    if (!is.null(x$call)) {
        cat("Call: ")
        print(x$call)
    }
    cat("Number of cases in table:", x$n.cases, "\n")
    cat("Number of factors:", x$n.vars, "\n")
    if (x$n.vars > 1) {
        cat("Test for independence of all factors:\n")
        ch <- x$statistic
        cat("\tChisq = ", format(round(ch, max(0, digits - log10(ch)))), 
            ", df = ", x$parameter, ", p-value = ", format.pval(x$p.value, 
                digits, eps = 0), "\n", sep = "")
        if (!x$approx.ok) 
            cat("\tChi-squared approximation may be incorrect\n")
    }
    invisible(x)
}


substr <- function (x, start, stop) 
{
    if (!is.character(x)) 
        x <- as.character(x)
    .Internal(substr(x, as.integer(start), as.integer(stop)))
}


cospi <- function (x)  .Primitive("cospi")


`[[<-.numeric_version` <- function (x, ..., value) 
{
    z <- unclass(x)
    if (nargs() < 4L) {
        if (length(..1) < 2L) {
            if (is.character(value) && length(value) == 1L) 
                value <- unclass(as.numeric_version(value))[[1L]]
            else if (!is.integer(value)) 
                stop("invalid 'value'")
        }
        else {
            value <- as.integer(value)
            if (length(value) != 1L) 
                stop("invalid 'value'")
        }
        z[[..1]] <- value
    }
    else {
        value <- as.integer(value)
        if (length(value) != 1L) 
            stop("invalid 'value'")
        z[[..1]][..2] <- value
    }
    structure(z, class = oldClass(x))
}


c.warnings <- function (..., recursive = FALSE) 
structure(NextMethod("c"), class = "warnings")


`[.warnings` <- function (x, ...) 
structure(NextMethod("["), class = "warnings")


print.packageInfo <- function (x, ...) 
{
    outFile <- tempfile("RpackageInfo")
    writeLines(format(x), outFile)
    file.show(outFile, delete.file = TRUE, title = gettextf("Documentation for package %s", 
        sQuote(x$name)))
    invisible(x)
}


NCOL <- function (x) 
if (length(d <- dim(x)) > 1L) d[2L] else 1L


is.numeric <- function (x)  .Primitive("is.numeric")


strtoi <- function (x, base = 0L) 
.Internal(strtoi(as.character(x), as.integer(base)))


.path.package <- function (...) 
.Defunct("path.package")


.GenericArgsEnv <- "<environment>"

rep_len <- function (x, length.out) 
.Internal(rep_len(x, length.out))


.make_numeric_version <- function (x, strict = TRUE, regexp, classes = NULL) 
{
    nms <- names(x)
    x <- as.character(x)
    y <- rep.int(list(integer()), length(x))
    valid_numeric_version_regexp <- sprintf("^%s$", regexp)
    if (length(x)) {
        ok <- grepl(valid_numeric_version_regexp, x)
        if (!all(ok) && strict) 
            stop(gettextf("invalid version specification %s", 
                paste(sQuote(unique(x[!ok])), collapse = ", ")), 
                call. = FALSE, domain = NA)
        y[ok] <- lapply(strsplit(x[ok], "[.-]"), as.integer)
    }
    names(y) <- nms
    class(y) <- unique(c(classes, "numeric_version"))
    y
}


date <- function () 
.Internal(date())


`names<-` <- function (x, value)  .Primitive("names<-")


closeAllConnections <- function () 
{
    i <- sink.number(type = "message")
    if (i > 0L) 
        sink(stderr(), type = "message")
    n <- sink.number()
    if (n > 0L) 
        for (i in seq_len(n)) sink()
    gc()
    set <- getAllConnections()
    set <- set[set > 2L]
    for (i in seq_along(set)) close(getConnection(set[i]))
    invisible()
}


as.logical <- function (x, ...)  .Primitive("as.logical")


getDLLRegisteredRoutines <- function (dll, addNames = TRUE) 
UseMethod("getDLLRegisteredRoutines")


round <- function (x, digits = 0)  .Primitive("round")


file.exists <- function (...) 
.Internal(file.exists(c(...)))


.F_dtrco <- structure(list(name = "dtrco", address = pointer("0x000000000df13c50"), 
    dll = structure(list(name = "base", path = "base", dynamicLookup = FALSE, 
        handle = <pointer: (nil)>, info = pointer("0x000000000df73cc0")), class = "DLLInfo"), 
    numParameters = 6L), class = c("FortranRoutine", "NativeSymbolInfo"
))


polyroot <- function (z) 
.Internal(polyroot(z))


.kronecker <- function (X, Y, FUN = "*", make.dimnames = FALSE, ...) 
{
    X <- as.array(X)
    Y <- as.array(Y)
    if (make.dimnames) {
        dnx <- dimnames(X)
        dny <- dimnames(Y)
    }
    dX <- dim(X)
    dY <- dim(Y)
    ld <- length(dX) - length(dY)
    if (ld < 0L) 
        dX <- dim(X) <- c(dX, rep.int(1, -ld))
    else if (ld > 0L) 
        dY <- dim(Y) <- c(dY, rep.int(1, ld))
    opobj <- outer(X, Y, FUN, ...)
    dp <- as.vector(t(matrix(1L:(2 * length(dX)), ncol = 2)[, 
        2:1]))
    opobj <- aperm(opobj, dp)
    dim(opobj) <- dX * dY
    if (make.dimnames && !(is.null(dnx) && is.null(dny))) {
        if (is.null(dnx)) 
            dnx <- vector("list", length(dX))
        else if (ld < 0L) 
            dnx <- c(dnx, vector("list", -ld))
        tmp <- which(sapply(dnx, is.null))
        dnx[tmp] <- lapply(tmp, function(i) rep.int("", dX[i]))
        if (is.null(dny)) 
            dny <- vector("list", length(dY))
        else if (ld > 0) 
            dny <- c(dny, vector("list", ld))
        tmp <- which(sapply(dny, is.null))
        dny[tmp] <- lapply(tmp, function(i) rep.int("", dY[i]))
        k <- length(dim(opobj))
        dno <- vector("list", k)
        for (i in 1L:k) {
            tmp <- outer(dnx[[i]], dny[[i]], FUN = "paste", sep = ":")
            dno[[i]] <- as.vector(t(tmp))
        }
        dimnames(opobj) <- dno
    }
    opobj
}


.traceback <- function (x = NULL) 
{
    if (is.null(x) && !is.null(x <- get0(".Traceback", envir = baseenv()))) {
    }
    else if (is.numeric(x)) 
        x <- .Internal(traceback(x))
    x
}


browser <- function (text = "", condition = NULL, expr = TRUE, skipCalls = 0L)  .Primitive("browser")


conflictRules <- function (pkg, mask.ok = NULL, exclude = NULL) 
{
    if ((!missing(mask.ok)) || (!missing(exclude))) 
        assign(pkg, list(mask.ok = mask.ok, exclude = exclude), 
            envir = data)
    else if (exists(pkg, envir = data, inherits = FALSE)) 
        get(pkg, envir = data, inherits = FALSE)
    else NULL
}


is.double <- function (x)  .Primitive("is.double")


months <- function (x, abbreviate) 
UseMethod("months")


toupper <- function (x) 
{
    if (!is.character(x)) 
        x <- as.character(x)
    .Internal(toupper(x))
}


duplicated.default <- function (x, incomparables = FALSE, fromLast = FALSE, nmax = NA, 
    ...) 
.Internal(duplicated(x, incomparables, fromLast, if (is.factor(x)) min(length(x), 
    nlevels(x) + 1L) else nmax))


.bincode <- function (x, breaks, right = TRUE, include.lowest = FALSE) 
.Internal(bincode(x, breaks, right, include.lowest))


qr.resid <- function (qr, y) 
{
    if (!is.qr(qr)) 
        stop("argument is not a QR decomposition")
    if (is.complex(qr$qr)) 
        stop("not implemented for complex 'qr'")
    if (isTRUE(attr(qr, "useLAPACK"))) 
        stop("not supported for LAPACK QR")
    k <- as.integer(qr$rank)
    if (k == 0) 
        return(y)
    n <- as.integer(nrow(qr$qr))
    if (is.na(n)) 
        stop("invalid nrow(qr$qr)")
    ny <- as.integer(NCOL(y))
    if (is.na(ny)) 
        stop("invalid NCOL(y)")
    if (NROW(y) != n) 
        stop("'qr' and 'y' must have the same number of rows")
    storage.mode(y) <- "double"
    .Fortran(.F_dqrrsd, as.double(qr$qr), n, k, as.double(qr$qraux), 
        y, ny, rsd = y)$rsd
}


toString <- function (x, ...) 
UseMethod("toString")


socketSelect <- function (socklist, write = FALSE, timeout = NULL) 
{
    if (is.null(timeout)) 
        timeout <- -1
    else if (timeout < 0) 
        stop("'timeout' must be NULL or a non-negative number")
    if (length(write) < length(socklist)) 
        write <- rep_len(write, length(socklist))
    .Internal(sockSelect(socklist, write, timeout))
}


print.libraryIQR <- function (x, ...) 
{
    s <- format(x)
    if (!length(s)) {
        message("no packages found")
    }
    else {
        outFile <- tempfile("RlibraryIQR")
        writeLines(s, outFile)
        file.show(outFile, delete.file = TRUE, title = gettext("R packages available"))
    }
    invisible(x)
}


prop.table <- function (x, margin = NULL) 
{
    if (length(margin)) 
        sweep(x, margin, margin.table(x, margin), "/", check.margin = FALSE)
    else x/sum(x)
}


new.env <- function (hash = TRUE, parent = parent.frame(), size = 29L) 
.Internal(new.env(hash, parent, size))


which <- function (x, arr.ind = FALSE, useNames = TRUE) 
{
    wh <- .Internal(which(x))
    if (arr.ind && !is.null(d <- dim(x))) 
        arrayInd(wh, d, dimnames(x), useNames = useNames)
    else wh
}


duplicated.numeric_version <- function (x, incomparables = FALSE, ...) 
{
    x <- .encode_numeric_version(x)
    NextMethod("duplicated")
}


is.element <- function (el, set) 
match(el, set, 0L) > 0L


as.raw <- function (x)  .Primitive("as.raw")


nzchar <- function (x, keepNA = FALSE)  .Primitive("nzchar")


replace <- function (x, list, values) 
{
    x[list] <- values
    x
}


complex <- function (length.out = 0L, real = numeric(), imaginary = numeric(), 
    modulus = 1, argument = 0) 
{
    if (missing(modulus) && missing(argument)) {
        .Internal(complex(length.out, real, imaginary))
    }
    else {
        n <- max(length.out, length(argument), length(modulus))
        rep_len(modulus, n) * exp((0+1i) * rep_len(argument, 
            n))
    }
}


dget <- function (file, keep.source = FALSE) 
eval(parse(file = file, keep.source = keep.source))


environmentIsLocked <- function (env) 
.Internal(environmentIsLocked(env))


.Deprecated <- function (new, package = NULL, msg, old = as.character(sys.call(sys.parent()))[1L]) 
{
    msg <- if (missing(msg)) {
        msg <- gettextf("'%s' is deprecated.\n", old)
        if (!missing(new)) 
            msg <- c(msg, gettextf("Use '%s' instead.\n", new))
        c(msg, if (!is.null(package)) gettextf("See help(\"Deprecated\") and help(\"%s-deprecated\").", 
            package) else gettext("See help(\"Deprecated\")"))
    }
    else as.character(msg)
    msg <- paste(msg, collapse = "")
    if (missing(new)) 
        new <- NULL
    warning(warningCondition(msg, old = old, new = new, package = package, 
        class = "deprecatedWarning"))
}


alist <- function (...) 
as.list(sys.call())[-1L]


is.list <- function (x)  .Primitive("is.list")


Ops.factor <- function (e1, e2) 
{
    ok <- switch(.Generic, `==` = , `!=` = TRUE, FALSE)
    if (!ok) {
        warning(gettextf("%s not meaningful for factors", sQuote(.Generic)))
        return(rep.int(NA, max(length(e1), if (!missing(e2)) length(e2))))
    }
    noNA.levels <- function(f) {
        r <- levels(f)
        if (any(ina <- is.na(r))) {
            n <- "  NA "
            while (n %in% r) n <- paste(n, ".")
            r[ina] <- n
        }
        r
    }
    if (nzchar(.Method[1L])) {
        if (!anyNA(levels(e1)) && is.character(e2) && length(e2) == 
            1L) {
            if (.Generic == "==") {
                leq <- (levels(e1) == e2)
                return(leq[e1])
            }
            else {
                leq <- (levels(e1) != e2)
                return(leq[e1])
            }
        }
        l1 <- noNA.levels(e1)
        e1 <- l1[e1]
    }
    if (nzchar(.Method[2L])) {
        if (!anyNA(levels(e2)) && is.character(e1) && length(e1) == 
            1L) {
            if (.Generic == "==") {
                leq <- (levels(e2) == e1)
                return(leq[e2])
            }
            else {
                leq <- (levels(e2) != e1)
                return(leq[e2])
            }
        }
        l2 <- noNA.levels(e2)
        e2 <- l2[e2]
    }
    if (all(nzchar(.Method)) && (length(l1) != length(l2) || 
        !all(sort.int(l2) == sort.int(l1)))) 
        stop("level sets of factors are different")
    value <- NextMethod(.Generic)
    nas <- is.na(e1) | is.na(e2)
    value[nas] <- NA
    value
}


setdiff <- function (x, y) 
{
    x <- as.vector(x)
    y <- as.vector(y)
    unique(if (length(x) || length(y)) 
        x[match(x, y, 0L) == 0L]
    else x)
}


pmin.int <- function (..., na.rm = FALSE) 
.Internal(pmin(na.rm, ...))


slice.index <- function (x, MARGIN) 
{
    d <- dim(x)
    if (is.null(d)) 
        d <- length(x)
    n <- length(d)
    if (!length(MARGIN) || any(MARGIN < 1L) || any(MARGIN > n)) 
        stop("incorrect value for 'MARGIN'")
    if (any(d == 0L)) 
        return(array(integer(), d))
    m <- MARGIN[1L]
    y <- rep.int(rep.int(1L:d[m], prod(d[seq_len(m - 1L)]) * 
        rep.int(1L, d[m])), prod(d[seq.int(from = m + 1L, length.out = n - 
        m)]))
    if (length(MARGIN) > 1L) {
        p <- d[m]
        for (m in MARGIN[-1L]) {
            y <- y + p * rep.int(rep.int(seq.int(0L, d[m] - 1L), 
                prod(d[seq_len(m - 1L)]) * rep.int(1L, d[m])), 
                prod(d[seq.int(from = m + 1L, length.out = n - 
                  m)]))
            p <- p * d[m]
        }
    }
    dim(y) <- d
    y
}


getAllConnections <- function () 
.Internal(getAllConnections())


duplicated.warnings <- function (x, incomparables = FALSE, ...) 
duplicated(paste(names(x), as.character(x)), incomparables, ...)


numeric_version <- function (x, strict = TRUE) 
.make_numeric_version(x, strict, .standard_regexps()$valid_numeric_version)


ceiling <- function (x)  .Primitive("ceiling")


diag <- function (x = 1, nrow, ncol, names = TRUE) 
{
    if (is.matrix(x)) {
        if (nargs() > 1L && (nargs() > 2L || any(names(match.call()) %in% 
            c("nrow", "ncol")))) 
            stop("'nrow' or 'ncol' cannot be specified when 'x' is a matrix")
        if ((m <- min(dim(x))) == 0L) 
            return(vector(typeof(x), 0L))
        y <- x[1 + 0L:(m - 1L) * (dim(x)[1L] + 1)]
        if (names) {
            nms <- dimnames(x)
            if (is.list(nms) && !any(vapply(nms, is.null, NA)) && 
                identical((nm <- nms[[1L]][seq_len(m)]), nms[[2L]][seq_len(m)])) 
                names(y) <- nm
        }
        return(y)
    }
    if (is.array(x) && length(dim(x)) != 1L) 
        stop("'x' is an array, but not one-dimensional.")
    if (missing(x)) 
        n <- nrow
    else if (length(x) == 1L && nargs() == 1L) {
        n <- as.integer(x)
        x <- 1
    }
    else n <- length(x)
    if (!missing(nrow)) 
        n <- nrow
    if (missing(ncol)) 
        ncol <- n
    .Internal(diag(x, n, ncol))
}


as.complex <- function (x, ...)  .Primitive("as.complex")


sequence <- function (nvec) 
unlist(lapply(nvec, seq_len))


diff <- function (x, ...) 
UseMethod("diff")


.doSortWrap <- function (vec, decr, nalast, noNA = NA) 
{
    if (length(vec) > 0 && is.numeric(vec)) {
        sorted <- .makeSortEnum(decr, nalast)
        if (is.na(noNA)) {
            if (is.na(nalast)) 
                noNA <- TRUE
            else if (nalast) 
                noNA <- !is.na(vec[length(vec)])
            else noNA <- !is.na(vec[1L])
        }
        .Internal(wrap_meta(vec, sorted, noNA))
    }
    else vec
}


anyDuplicated <- function (x, incomparables = FALSE, ...) 
UseMethod("anyDuplicated")


all.equal.environment <- function (target, current, all.names = TRUE, ...) 
{
    if (!is.environment(target)) 
        return("'target' is not an environment")
    if (!is.environment(current)) 
        return("'current' is not an environment")
    ae.run <- dynGet("__all.eq.E__", NULL)
    if (is.null(ae.run)) 
        "__all.eq.E__" <- environment()
    else {
        do1 <- function(em) {
            if (identical(target, em$target) && identical(current, 
                em$current)) 
                TRUE
            else if (!is.null(em$mm)) 
                do1(em$mm)
            else {
                e <- new.env(parent = emptyenv())
                e$target <- target
                e$current <- current
                em$mm <- e
                FALSE
            }
        }
        if (do1(ae.run)) 
            return(TRUE)
    }
    all.equal.list(as.list.environment(target, all.names = all.names, 
        sorted = TRUE), as.list.environment(current, all.names = all.names, 
        sorted = TRUE), ...)
}


pipe <- function (description, open = "", encoding = getOption("encoding")) 
.Internal(pipe(description, open, encoding))


ISOdatetime <- function (year, month, day, hour, min, sec, tz = "") 
{
    if (min(vapply(list(year, month, day, hour, min, sec), length, 
        1, USE.NAMES = FALSE)) == 0L) 
        .POSIXct(numeric(), tz = tz)
    else {
        x <- paste(year, month, day, hour, min, sec, sep = "-")
        as.POSIXct(strptime(x, "%Y-%m-%d-%H-%M-%OS", tz = tz), 
            tz = tz)
    }
}


replicate <- function (n, expr, simplify = "array") 
sapply(integer(n), eval.parent(substitute(function(...) expr)), 
    simplify = simplify)


rbind.data.frame <- function (..., deparse.level = 1, make.row.names = TRUE, stringsAsFactors = default.stringsAsFactors()) 
{
    match.names <- function(clabs, nmi) {
        if (identical(clabs, nmi)) 
            NULL
        else if (length(nmi) == length(clabs) && all(nmi %in% 
            clabs)) {
            m <- pmatch(nmi, clabs, 0L)
            if (any(m == 0L)) 
                stop("names do not match previous names")
            m
        }
        else stop("names do not match previous names")
    }
    allargs <- list(...)
    allargs <- allargs[lengths(allargs) > 0L]
    if (length(allargs)) {
        nr <- vapply(allargs, function(x) if (is.data.frame(x)) 
            .row_names_info(x, 2L)
        else if (is.list(x)) 
            length(x[[1L]])
        else length(x), 1L)
        if (any(nr > 0L)) 
            allargs <- allargs[nr > 0L]
        else return(allargs[[1L]])
    }
    n <- length(allargs)
    if (n == 0L) 
        return(structure(list(), class = "data.frame", row.names = integer()))
    nms <- names(allargs)
    if (is.null(nms)) 
        nms <- character(n)
    cl <- NULL
    perm <- rows <- vector("list", n)
    if (make.row.names) {
        rlabs <- rows
        autoRnms <- TRUE
        Make.row.names <- function(nmi, ri, ni, nrow) {
            if (nzchar(nmi)) {
                if (autoRnms) 
                  autoRnms <<- FALSE
                if (ni == 0L) 
                  character()
                else if (ni > 1L) 
                  paste(nmi, ri, sep = ".")
                else nmi
            }
            else if (autoRnms && nrow > 0L && identical(ri, seq_len(ni))) 
                as.integer(seq.int(from = nrow + 1L, length.out = ni))
            else {
                if (autoRnms && (nrow > 0L || !identical(ri, 
                  seq_len(ni)))) 
                  autoRnms <<- FALSE
                ri
            }
        }
    }
    nrow <- 0L
    value <- clabs <- NULL
    all.levs <- list()
    for (i in seq_len(n)) {
        xi <- allargs[[i]]
        nmi <- nms[i]
        if (is.matrix(xi)) 
            allargs[[i]] <- xi <- as.data.frame(xi, stringsAsFactors = stringsAsFactors)
        if (inherits(xi, "data.frame")) {
            if (is.null(cl)) 
                cl <- oldClass(xi)
            ri <- attr(xi, "row.names")
            ni <- length(ri)
            if (is.null(clabs)) 
                clabs <- names(xi)
            else {
                if (length(xi) != length(clabs)) 
                  stop("numbers of columns of arguments do not match")
                pi <- match.names(clabs, names(xi))
                if (!is.null(pi)) 
                  perm[[i]] <- pi
            }
            rows[[i]] <- seq.int(from = nrow + 1L, length.out = ni)
            if (make.row.names) 
                rlabs[[i]] <- Make.row.names(nmi, ri, ni, nrow)
            nrow <- nrow + ni
            if (is.null(value)) {
                value <- unclass(xi)
                nvar <- length(value)
                all.levs <- vector("list", nvar)
                has.dim <- facCol <- ordCol <- logical(nvar)
                for (j in seq_len(nvar)) {
                  xj <- value[[j]]
                  facCol[j] <- if (!is.null(levels(xj))) {
                    all.levs[[j]] <- levels(xj)
                    TRUE
                  }
                  else is.factor(xj)
                  ordCol[j] <- is.ordered(xj)
                  has.dim[j] <- length(dim(xj)) == 2L
                }
            }
            else for (j in seq_len(nvar)) {
                xij <- xi[[j]]
                if (is.null(pi) || is.na(jj <- pi[[j]])) 
                  jj <- j
                if (facCol[jj]) {
                  if (length(lij <- levels(xij))) {
                    all.levs[[jj]] <- unique(c(all.levs[[jj]], 
                      lij))
                    ordCol[jj] <- ordCol[jj] & is.ordered(xij)
                  }
                  else if (is.character(xij)) 
                    all.levs[[jj]] <- unique(c(all.levs[[jj]], 
                      xij))
                }
            }
        }
        else if (is.list(xi)) {
            ni <- range(lengths(xi))
            if (ni[1L] == ni[2L]) 
                ni <- ni[1L]
            else stop("invalid list argument: all variables should have the same length")
            rows[[i]] <- ri <- as.integer(seq.int(from = nrow + 
                1L, length.out = ni))
            nrow <- nrow + ni
            if (make.row.names) 
                rlabs[[i]] <- Make.row.names(nmi, ri, ni, nrow)
            if (length(nmi <- names(xi)) > 0L) {
                if (is.null(clabs)) 
                  clabs <- nmi
                else {
                  if (length(xi) != length(clabs)) 
                    stop("numbers of columns of arguments do not match")
                  pi <- match.names(clabs, nmi)
                  if (!is.null(pi)) 
                    perm[[i]] <- pi
                }
            }
        }
        else if (length(xi)) {
            rows[[i]] <- nrow <- nrow + 1L
            if (make.row.names) 
                rlabs[[i]] <- if (nzchar(nmi)) 
                  nmi
                else as.integer(nrow)
        }
    }
    nvar <- length(clabs)
    if (nvar == 0L) 
        nvar <- max(lengths(allargs))
    if (nvar == 0L) 
        return(structure(list(), class = "data.frame", row.names = integer()))
    pseq <- seq_len(nvar)
    if (is.null(value)) {
        value <- list()
        value[pseq] <- list(logical(nrow))
        all.levs <- vector("list", nvar)
        has.dim <- facCol <- ordCol <- logical(nvar)
    }
    names(value) <- clabs
    for (j in pseq) if (length(lij <- all.levs[[j]])) 
        value[[j]] <- factor(as.vector(value[[j]]), lij, ordered = ordCol[j])
    if (any(has.dim)) {
        rmax <- max(unlist(rows))
        for (i in pseq[has.dim]) if (!inherits(xi <- value[[i]], 
            "data.frame")) {
            dn <- dimnames(xi)
            rn <- dn[[1L]]
            if (length(rn) > 0L) 
                length(rn) <- rmax
            pi <- dim(xi)[2L]
            length(xi) <- rmax * pi
            value[[i]] <- array(xi, c(rmax, pi), list(rn, dn[[2L]]))
        }
    }
    for (i in seq_len(n)) {
        xi <- unclass(allargs[[i]])
        if (!is.list(xi)) 
            if (length(xi) != nvar) 
                xi <- rep(xi, length.out = nvar)
        ri <- rows[[i]]
        pi <- perm[[i]]
        if (is.null(pi)) 
            pi <- pseq
        for (j in pseq) {
            jj <- pi[j]
            xij <- xi[[j]]
            if (has.dim[jj]) {
                value[[jj]][ri, ] <- xij
                rownames(value[[jj]])[ri] <- rownames(xij)
            }
            else {
                value[[jj]][ri] <- if (is.factor(xij)) 
                  as.vector(xij)
                else xij
                if (!is.null(nm <- names(xij))) 
                  names(value[[jj]])[ri] <- nm
            }
        }
    }
    rlabs <- if (make.row.names && !autoRnms) {
        rlabs <- unlist(rlabs)
        if (anyDuplicated(rlabs)) 
            make.unique(as.character(rlabs), sep = "")
        else rlabs
    }
    if (is.null(cl)) {
        as.data.frame(value, row.names = rlabs, fix.empty.names = TRUE, 
            stringsAsFactors = stringsAsFactors)
    }
    else {
        structure(value, class = cl, row.names = if (is.null(rlabs)) 
            .set_row_names(nrow)
        else rlabs)
    }
}


lazyLoadDBfetch <- function (key, file, compressed, hook)  .Primitive("lazyLoadDBfetch")


conflicts <- function (where = search(), detail = FALSE) 
{
    if (length(where) < 1L) 
        stop("argument 'where' of length 0")
    z <- vector(length(where), mode = "list")
    names(z) <- where
    for (i in seq_along(where)) z[[i]] <- objects(pos = where[i])
    all <- unlist(z, use.names = FALSE)
    dups <- duplicated(all)
    dups <- all[dups]
    if (detail) {
        for (i in where) z[[i]] <- z[[i]][match(dups, z[[i]], 
            0L)]
        z[vapply(z, function(x) length(x) == 0L, NA)] <- NULL
        z
    }
    else dups
}


.rmpkg <- function (pkg) 
sub("package:", "", pkg, fixed = TRUE)


as.Date <- function (x, ...) 
UseMethod("as.Date")


as.data.frame.numeric_version <- function (x, row.names = NULL, optional = FALSE, ..., nm = paste(deparse(substitute(x), 
    width.cutoff = 500L), collapse = " ")) 
{
    force(nm)
    nrows <- length(x)
    if (!(is.null(row.names) || (is.character(row.names) && length(row.names) == 
        nrows))) {
        warning(gettextf("'row.names' is not a character vector of length %d -- omitting it. Will be an error!", 
            nrows), domain = NA)
        row.names <- NULL
    }
    if (is.null(row.names)) {
        if (nrows == 0L) 
            row.names <- character()
        else if (length(row.names <- names(x)) != nrows || anyDuplicated(row.names)) 
            row.names <- .set_row_names(nrows)
    }
    if (!is.null(names(x))) 
        names(x) <- NULL
    value <- list(x)
    if (!optional) 
        names(value) <- nm
    structure(value, row.names = row.names, class = "data.frame")
}


mean.default <- function (x, trim = 0, na.rm = FALSE, ...) 
{
    if (!is.numeric(x) && !is.complex(x) && !is.logical(x)) {
        warning("argument is not numeric or logical: returning NA")
        return(NA_real_)
    }
    if (na.rm) 
        x <- x[!is.na(x)]
    if (!is.numeric(trim) || length(trim) != 1L) 
        stop("'trim' must be numeric of length one")
    n <- length(x)
    if (trim > 0 && n) {
        if (is.complex(x)) 
            stop("trimmed means are not defined for complex data")
        if (anyNA(x)) 
            return(NA_real_)
        if (trim >= 0.5) 
            return(stats::median(x, na.rm = FALSE))
        lo <- floor(n * trim) + 1
        hi <- n + 1 - lo
        x <- sort.int(x, partial = unique(c(lo, hi)))[lo:hi]
    }
    .Internal(mean(x))
}


cumprod <- function (x)  .Primitive("cumprod")


as.double <- function (x, ...)  .Primitive("as.double")


as.data.frame.logical <- function (x, row.names = NULL, optional = FALSE, ..., nm = paste(deparse(substitute(x), 
    width.cutoff = 500L), collapse = " ")) 
{
    force(nm)
    nrows <- length(x)
    if (!(is.null(row.names) || (is.character(row.names) && length(row.names) == 
        nrows))) {
        warning(gettextf("'row.names' is not a character vector of length %d -- omitting it. Will be an error!", 
            nrows), domain = NA)
        row.names <- NULL
    }
    if (is.null(row.names)) {
        if (nrows == 0L) 
            row.names <- character()
        else if (length(row.names <- names(x)) != nrows || anyDuplicated(row.names)) 
            row.names <- .set_row_names(nrows)
    }
    if (!is.null(names(x))) 
        names(x) <- NULL
    value <- list(x)
    if (!optional) 
        names(value) <- nm
    structure(value, row.names = row.names, class = "data.frame")
}


as.data.frame.POSIXct <- function (x, row.names = NULL, optional = FALSE, ..., nm = paste(deparse(substitute(x), 
    width.cutoff = 500L), collapse = " ")) 
{
    force(nm)
    nrows <- length(x)
    if (!(is.null(row.names) || (is.character(row.names) && length(row.names) == 
        nrows))) {
        warning(gettextf("'row.names' is not a character vector of length %d -- omitting it. Will be an error!", 
            nrows), domain = NA)
        row.names <- NULL
    }
    if (is.null(row.names)) {
        if (nrows == 0L) 
            row.names <- character()
        else if (length(row.names <- names(x)) != nrows || anyDuplicated(row.names)) 
            row.names <- .set_row_names(nrows)
    }
    if (!is.null(names(x))) 
        names(x) <- NULL
    value <- list(x)
    if (!optional) 
        names(value) <- nm
    structure(value, row.names = row.names, class = "data.frame")
}


findRestart <- function (name, cond = NULL) 
{
    i <- 1L
    repeat {
        r <- .Internal(.getRestart(i))
        if (is.null(r)) 
            return(NULL)
        else if (name == r[[1L]] && (is.null(cond) || is.null(r$test) || 
            r$test(cond))) 
            return(r)
        else i <- i + 1L
    }
}


as.data.frame.POSIXlt <- function (x, row.names = NULL, optional = FALSE, ...) 
{
    value <- as.data.frame.POSIXct(as.POSIXct(x), row.names, 
        optional, ...)
    if (!optional) 
        names(value) <- deparse(substitute(x))[[1L]]
    value
}


as.data.frame.ts <- function (x, ...) 
{
    if (is.matrix(x)) 
        as.data.frame.matrix(x, ...)
    else as.data.frame.vector(x, ...)
}


findPackageEnv <- function (info) 
{
    if (info %in% search()) 
        return(as.environment(info))
    message(gettextf("Attempting to load the environment %s", 
        sQuote(info)), domain = NA)
    if (require(substr(info, 9L, 1000L), character.only = TRUE, 
        quietly = TRUE)) 
        return(as.environment(info))
    message("Specified environment not found: using '.GlobalEnv' instead")
    .GlobalEnv
}


pmax <- function (..., na.rm = FALSE) 
{
    elts <- list(...)
    if (length(elts) == 0L) 
        stop("no arguments")
    if (all(vapply(elts, function(x) is.atomic(x) && !is.object(x), 
        NA))) {
        mmm <- .Internal(pmax(na.rm, ...))
        mostattributes(mmm) <- attributes(elts[[1L]])
    }
    else {
        mmm <- elts[[1L]]
        has.na <- FALSE
        as <- methods::as
        asL <- function(x) if (isS4(x)) 
            as(x, "logical")
        else x
        for (each in elts[-1L]) {
            l1 <- length(each)
            l2 <- length(mmm)
            if (l2 && (l2 < l1 || !l1)) {
                if (l1%%l2) 
                  warning("an argument will be fractionally recycled")
                mmm <- rep(mmm, length.out = l1)
            }
            else if (l1 && (l1 < l2 || !l2)) {
                if (l2%%l1) 
                  warning("an argument will be fractionally recycled")
                each <- rep(each, length.out = l2)
            }
            na.m <- is.na(mmm)
            na.e <- is.na(each)
            if (has.na || (has.na <- any(na.m) || any(na.e))) {
                if (any(na.m <- asL(na.m))) 
                  mmm[na.m] <- each[na.m]
                if (any(na.e <- asL(na.e))) 
                  each[na.e] <- mmm[na.e]
            }
            nS4 <- !isS4(mmm)
            if (isS4(change <- mmm < each) && (nS4 || !isS4(each))) 
                change <- as(change, "logical")
            change <- change & !is.na(change)
            mmm[change] <- each[change]
            if (has.na && !na.rm) 
                mmm[na.m | na.e] <- NA
            if (nS4) 
                mostattributes(mmm) <- attributes(elts[[1L]])
        }
    }
    mmm
}


getNamespaceImports <- function (ns) 
{
    ns <- asNamespace(ns)
    if (isBaseNamespace(ns)) 
        NULL
    else .getNamespaceInfo(ns, "imports")
}


as.numeric_version <- function (x) 
{
    if (is.numeric_version(x)) 
        x
    else if (is.package_version(x)) {
        structure(x, class = c(class(x), "numeric_version"))
    }
    else if (is.list(x) && all(vapply(x, is.integer, NA))) {
        bad <- vapply(x, function(e) anyNA(e) || any(e < 0L), 
            NA)
        if (any(bad)) {
            x[bad] <- rep.int(list(integer()), sum(bad))
        }
        class(x) <- "numeric_version"
        x
    }
    else numeric_version(x)
}


pmin <- function (..., na.rm = FALSE) 
{
    elts <- list(...)
    if (length(elts) == 0L) 
        stop("no arguments")
    if (all(vapply(elts, function(x) is.atomic(x) && !is.object(x), 
        NA))) {
        mmm <- .Internal(pmin(na.rm, ...))
        mostattributes(mmm) <- attributes(elts[[1L]])
    }
    else {
        mmm <- elts[[1L]]
        has.na <- FALSE
        as <- methods::as
        asL <- function(x) if (isS4(x)) 
            as(x, "logical")
        else x
        for (each in elts[-1L]) {
            l1 <- length(each)
            l2 <- length(mmm)
            if (l2 && (l2 < l1 || !l1)) {
                if (l1%%l2) 
                  warning("an argument will be fractionally recycled")
                mmm <- rep(mmm, length.out = l1)
            }
            else if (l1 && (l1 < l2 || !l2)) {
                if (l2%%l1) 
                  warning("an argument will be fractionally recycled")
                each <- rep(each, length.out = l2)
            }
            na.m <- is.na(mmm)
            na.e <- is.na(each)
            if (has.na || (has.na <- any(na.m) || any(na.e))) {
                if (any(na.m <- asL(na.m))) 
                  mmm[na.m] <- each[na.m]
                if (any(na.e <- asL(na.e))) 
                  each[na.e] <- mmm[na.e]
            }
            nS4 <- !isS4(mmm)
            if (isS4(change <- mmm > each) && (nS4 || !isS4(each))) 
                change <- as(change, "logical")
            change <- change & !is.na(change)
            mmm[change] <- each[change]
            if (has.na && !na.rm) 
                mmm[na.m | na.e] <- NA
            if (nS4) 
                mostattributes(mmm) <- attributes(elts[[1L]])
        }
    }
    mmm
}


as.character.srcref <- function (x, useSource = TRUE, to = x, ...) 
{
    srcfile <- attr(x, "srcfile")
    if (!missing(to)) {
        if (!identical(srcfile, attr(to, "srcfile"))) 
            stop("'x' and 'to' must refer to same file")
        x[c(3L, 4L, 6L, 8L)] <- to[c(3L, 4L, 6L, 8L)]
    }
    if (!is.null(srcfile) && !inherits(srcfile, "srcfile")) {
        cat("forcing class on")
        print(utils::str(srcfile))
        class(srcfile) <- c("srcfilealias", "srcfile")
    }
    if (useSource) {
        if (inherits(srcfile, "srcfilecopy") || inherits(srcfile, 
            "srcfilealias")) 
            lines <- try(getSrcLines(srcfile, x[7L], x[8L]), 
                TRUE)
        else lines <- try(getSrcLines(srcfile, x[1L], x[3L]), 
            TRUE)
    }
    if (!useSource || inherits(lines, "try-error")) 
        lines <- paste0("<srcref: file \"", srcfile$filename, 
            "\" chars ", x[1L], ":", x[5L], " to ", x[3L], ":", 
            x[6L], ">")
    else if (length(lines)) {
        enc <- Encoding(lines)
        Encoding(lines) <- "latin1"
        if (length(lines) < x[3L] - x[1L] + 1L) 
            x[4L] <- .Machine$integer.max
        lines[length(lines)] <- substring(lines[length(lines)], 
            1L, x[4L])
        lines[1L] <- substring(lines[1L], x[2L])
        Encoding(lines) <- enc
    }
    lines
}


trunc.Date <- function (x, ...) 
round(x - 0.4999999)


print.hexmode <- function (x, ...) 
{
    print(format(x), ...)
    invisible(x)
}


provideDimnames <- function (x, sep = "", base = list(LETTERS), unique = TRUE) 
{
    dx <- dim(x)
    dnx <- dimnames(x)
    if (new <- is.null(dnx)) 
        dnx <- vector("list", length(dx))
    k <- length(M <- lengths(base))
    for (i in which(vapply(dnx, is.null, NA))) {
        ii <- 1L + (i - 1L)%%k
        ss <- seq_len(dx[i]) - 1L
        bi <- base[[ii]][1L + (ss%%M[ii])]
        dnx[[i]] <- if (unique) 
            make.unique(bi, sep = sep)
        else bi
        new <- TRUE
    }
    if (new) 
        dimnames(x) <- dnx
    x
}


match.arg <- function (arg, choices, several.ok = FALSE) 
{
    if (missing(choices)) {
        formal.args <- formals(sys.function(sysP <- sys.parent()))
        choices <- eval(formal.args[[as.character(substitute(arg))]], 
            envir = sys.frame(sysP))
    }
    if (is.null(arg)) 
        return(choices[1L])
    else if (!is.character(arg)) 
        stop("'arg' must be NULL or a character vector")
    if (!several.ok) {
        if (identical(arg, choices)) 
            return(arg[1L])
        if (length(arg) > 1L) 
            stop("'arg' must be of length 1")
    }
    else if (length(arg) == 0L) 
        stop("'arg' must be of length >= 1")
    i <- pmatch(arg, choices, nomatch = 0L, duplicates.ok = TRUE)
    if (all(i == 0L)) 
        stop(gettextf("'arg' should be one of %s", paste(dQuote(choices), 
            collapse = ", ")), domain = NA)
    i <- i[i > 0L]
    if (!several.ok && length(i) > 1) 
        stop("there is more than one match in 'match.arg'")
    choices[i]
}


enquote <- function (cl) 
as.call(list(quote(base::quote), cl))


file.path <- function (..., fsep = .Platform$file.sep) 
.Internal(file.path(list(...), fsep))


as.character.condition <- function (x, ...) 
{
    msg <- conditionMessage(x)
    call <- conditionCall(x)
    cl <- class(x)[1L]
    if (!is.null(call)) 
        paste0(cl, " in ", deparse(call)[1L], ": ", msg, "\n")
    else paste0(cl, ": ", msg, "\n")
}


.Platform <- list(OS.type = "windows", file.sep = "/", dynlib.ext = ".dll", 
    GUI = "RTerm", endian = "little", pkgType = "win.binary", 
    path.sep = ";", r_arch = "x64")


`|.octmode` <- function (a, b) 
as.octmode(bitwOr(as.octmode(a), as.octmode(b)))


memDecompress <- function (from, type = c("unknown", "gzip", "bzip2", "xz", "none"), 
    asChar = FALSE) 
{
    type <- match(match.arg(type), c("none", "gzip", "bzip2", 
        "xz", "unknown"))
    ans <- .Internal(memDecompress(from, type))
    if (asChar) 
        rawToChar(ans)
    else ans
}


format.data.frame <- function (x, ..., justify = "none") 
{
    nc <- length(x)
    if (!nc) 
        return(x)
    nr <- .row_names_info(x, 2L)
    rval <- vector("list", nc)
    for (i in seq_len(nc)) rval[[i]] <- format(x[[i]], ..., justify = justify)
    lens <- vapply(rval, NROW, 1)
    if (any(lens != nr)) {
        warning("corrupt data frame: columns will be truncated or padded with NAs")
        for (i in seq_len(nc)) {
            len <- NROW(rval[[i]])
            if (len == nr) 
                next
            if (length(dim(rval[[i]])) == 2L) {
                rval[[i]] <- if (len < nr) 
                  rbind(rval[[i]], matrix(NA, nr - len, ncol(rval[[i]])))
                else rval[[i]][seq_len(nr), ]
            }
            else {
                rval[[i]] <- if (len < nr) 
                  c(rval[[i]], rep.int(NA, nr - len))
                else rval[[i]][seq_len(nr)]
            }
        }
    }
    for (i in seq_len(nc)) {
        if (is.character(rval[[i]]) && inherits(rval[[i]], "character")) 
            oldClass(rval[[i]]) <- "AsIs"
    }
    as.data.frame.list(rval, row.names = row.names(x), col.names = names(x), 
        optional = TRUE, fix.empty.names = FALSE, cut.names = TRUE)
}


lazyLoad <- function (filebase, envir = parent.frame(), filter) 
{
    fun <- function(db) {
        vals <- db$vals
        vars <- db$vars
        expr <- quote(lazyLoadDBfetch(key, datafile, compressed, 
            envhook))
        .Internal(makeLazy(vars, vals, expr, db, envir))
    }
    lazyLoadDBexec(filebase, fun, filter)
}


srcref <- function (srcfile, lloc) 
{
    stopifnot(inherits(srcfile, "srcfile"), length(lloc) %in% 
        c(4L, 6L, 8L))
    if (length(lloc) == 4) 
        lloc <- c(lloc, lloc[c(2, 4)])
    if (length(lloc) == 6) 
        lloc <- c(lloc, lloc[c(1, 3)])
    structure(as.integer(lloc), srcfile = srcfile, class = "srcref")
}


as.POSIXlt.factor <- function (x, ...) 
{
    y <- as.POSIXlt(as.character(x), ...)
    names(y$year) <- names(x)
    y
}


t.default <- function (x) 
.Internal(t.default(x))


warning <- function (..., call. = TRUE, immediate. = FALSE, noBreaks. = FALSE, 
    domain = NULL) 
{
    args <- list(...)
    if (length(args) == 1L && inherits(args[[1L]], "condition")) {
        cond <- args[[1L]]
        if (nargs() > 1L) 
            cat(gettext("additional arguments ignored in warning()"), 
                "\n", sep = "", file = stderr())
        message <- conditionMessage(cond)
        call <- conditionCall(cond)
        withRestarts({
            .Internal(.signalCondition(cond, message, call))
            .Internal(.dfltWarn(message, call))
        }, muffleWarning = function() NULL)
        invisible(message)
    }
    else .Internal(warning(call., immediate., noBreaks., .makeMessage(..., 
        domain = domain)))
}


.__S3MethodsTable__. <- compiler::.__S3MethodsTable__. # re-exported from compiler package

substring <- function (text, first, last = 1000000L) 
{
    if (!is.character(text)) 
        text <- as.character(text)
    n <- max(lt <- length(text), length(first), length(last))
    if (lt && lt < n) 
        text <- rep_len(text, length.out = n)
    .Internal(substr(text, as.integer(first), as.integer(last)))
}


commandArgs <- function (trailingOnly = FALSE) 
{
    args <- .Internal(commandArgs())
    if (trailingOnly) {
        m <- match("--args", args, 0L)
        if (m) 
            args[-seq_len(m)]
        else character()
    }
    else args
}


`[[.numeric_version` <- function (x, ..., exact = NA) 
{
    if (length(list(...)) < 2L) 
        structure(list(unclass(x)[[..., exact = exact]]), class = oldClass(x))
    else unclass(x)[[..1, exact = exact]][..2]
}


file.symlink <- function (from, to) 
{
    if (!(length(from))) 
        stop("no files to link from")
    if (!(nt <- length(to))) 
        stop("no files/directory to link to")
    if (nt == 1 && file.exists(to) && file.info(to, extra_cols = FALSE)$isdir) 
        to <- file.path(to, basename(from))
    .Internal(file.symlink(from, to))
}


`[.DLLInfoList` <- function (x, ...) 
structure(NextMethod("["), class = class(x))


getCallingDLLe <- function (e) 
{
    if (is.null(env <- e$.__NAMESPACE__.)) 
        env <- baseenv()
    if (!is.null(Ds <- get0("DLLs", envir = env)) && length(Ds)) 
        Ds[[1L]]
}


Summary.difftime <- function (..., na.rm) 
{
    coerceTimeUnit <- function(x) {
        as.vector(switch(attr(x, "units"), secs = x, mins = 60 * 
            x, hours = 60 * 60 * x, days = 60 * 60 * 24 * x, 
            weeks = 60 * 60 * 24 * 7 * x))
    }
    ok <- switch(.Generic, max = , min = , sum = , range = TRUE, 
        FALSE)
    if (!ok) 
        stop(gettextf("'%s' not defined for \"difftime\" objects", 
            .Generic), domain = NA)
    x <- list(...)
    Nargs <- length(x)
    if (Nargs == 0) {
        .difftime(do.call(.Generic), "secs")
    }
    else {
        units <- sapply(x, attr, "units")
        if (all(units == units[1L])) {
            args <- c(lapply(x, as.vector), na.rm = na.rm)
        }
        else {
            args <- c(lapply(x, coerceTimeUnit), na.rm = na.rm)
            units <- "secs"
        }
        .difftime(do.call(.Generic, args), units[[1L]])
    }
}


NROW <- function (x) 
if (length(d <- dim(x))) d[1L] else length(x)


match.fun <- function (FUN, descend = TRUE) 
{
    if (is.function(FUN)) 
        return(FUN)
    if (!(is.character(FUN) && length(FUN) == 1L || is.symbol(FUN))) {
        FUN <- eval.parent(substitute(substitute(FUN)))
        if (!is.symbol(FUN)) 
            stop(gettextf("'%s' is not a function, character or symbol", 
                deparse(FUN)), domain = NA)
    }
    envir <- parent.frame(2)
    if (descend) 
        FUN <- get(as.character(FUN), mode = "function", envir = envir)
    else {
        FUN <- get(as.character(FUN), mode = "any", envir = envir)
        if (!is.function(FUN)) 
            stop(gettextf("found non-function '%s'", FUN), domain = NA)
    }
    return(FUN)
}


dput <- function (x, file = "", control = c("keepNA", "keepInteger", 
    "niceNames", "showAttributes")) 
{
    if (is.character(file)) 
        if (nzchar(file)) {
            file <- file(file, "wt")
            on.exit(close(file))
        }
        else file <- stdout()
    .Internal(dput(x, file, .deparseOpts(control)))
}


tempfile <- function (pattern = "file", tmpdir = tempdir(), fileext = "") 
.Internal(tempfile(pattern, tmpdir, fileext))


...elt <- function (n)  .Primitive("...elt")


print.by <- function (x, ..., vsep) 
{
    d <- dim(x)
    dn <- dimnames(x)
    dnn <- names(dn)
    if (missing(vsep)) 
        vsep <- strrep("-", 0.75 * getOption("width"))
    lapply(X = seq_along(x), FUN = function(i, x, vsep, ...) {
        if (i != 1L && !is.null(vsep)) 
            cat(vsep, "\n")
        ii <- i - 1L
        for (j in seq_along(dn)) {
            iii <- ii%%d[j] + 1L
            ii <- ii%/%d[j]
            cat(dnn[j], ": ", dn[[j]][iii], "\n", sep = "")
        }
        print(x[[i]], ...)
    }, x, vsep, ...)
    invisible(x)
}


summary.matrix <- function (object, ...) 
{
    summary.data.frame(as.data.frame.matrix(object), ...)
}


Summary.ordered <- function (..., na.rm) 
{
    ok <- switch(.Generic, max = , min = , range = TRUE, FALSE)
    if (!ok) 
        stop(gettextf("'%s' not defined for ordered factors", 
            .Generic), domain = NA)
    args <- list(...)
    levl <- lapply(args, levels)
    levset <- levl[[1]]
    if (!all(vapply(args, is.ordered, NA)) || !all(vapply(levl, 
        identical, NA, levset))) 
        stop(gettextf("'%s' is only meaningful for ordered factors if all arguments have the same level sets", 
            .Generic))
    codes <- lapply(args, as.integer)
    ind <- do.call(.Generic, c(codes, na.rm = na.rm))
    ordered(levset[ind], levels = levset)
}


.Script <- function (interpreter, script, args, ...) 
{
    if (.Platform$OS.type == "windows") {
        cmd <- paste(shQuote(file.path(R.home("bin"), "Rcmd")), 
            file.path("..", "share", interpreter, script), args)
        system(cmd, invisible = TRUE)
    }
    else system(paste(shQuote(file.path(R.home("bin"), "Rcmd")), 
        interpreter, shQuote(file.path(R.home("share"), interpreter, 
            script)), args), ...)
}


`mode<-` <- function (x, value) 
{
    if (storage.mode(x) == value) 
        return(x)
    if (is.factor(x)) 
        stop("invalid to change the storage mode of a factor")
    atr <- attributes(x)
    isSingle <- !is.null(attr(x, "Csingle"))
    setSingle <- value == "single"
    mde <- get(paste0("as.", value), mode = "function", envir = parent.frame())
    x <- mde(x)
    attributes(x) <- atr
    if (setSingle != isSingle) 
        attr(x, "Csingle") <- if (setSingle) 
            TRUE
    x
}


La.svd <- function (x, nu = min(n, p), nv = min(n, p)) 
{
    if (!is.logical(x) && !is.numeric(x) && !is.complex(x)) 
        stop("argument to 'La.svd' must be numeric or complex")
    if (any(!is.finite(x))) 
        stop("infinite or missing values in 'x'")
    x <- as.matrix(x)
    n <- nrow(x)
    p <- ncol(x)
    if (!n || !p) 
        stop("a dimension is zero")
    zero <- if (is.complex(x)) 
        0 + (0+0i)
    else 0
    if (nu || nv) {
        np <- min(n, p)
        if (nu <= np && nv <= np) {
            jobu <- "S"
            u <- matrix(zero, n, np)
            vt <- matrix(zero, np, p)
            nu0 <- nv0 <- np
        }
        else {
            jobu <- "A"
            u <- matrix(zero, n, n)
            vt <- matrix(zero, p, p)
            nu0 <- n
            nv0 <- p
        }
    }
    else {
        jobu <- "N"
        u <- matrix(zero, 1L, 1L)
        vt <- matrix(zero, 1L, 1L)
    }
    res <- if (is.complex(x)) 
        .Internal(La_svd_cmplx(jobu, x, double(min(n, p)), u, 
            vt))
    else .Internal(La_svd(jobu, x, double(min(n, p)), u, vt))
    res <- res[c("d", if (nu) "u", if (nv) "vt")]
    if (nu && nu < nu0) 
        res$u <- res$u[, seq_len(min(n, nu)), drop = FALSE]
    if (nv && nv < nv0) 
        res$vt <- res$vt[seq_len(min(p, nv)), , drop = FALSE]
    res
}


rawConnection <- function (object, open = "r") 
{
    .Internal(rawConnection(deparse(substitute(object)), object, 
        open))
}


format.AsIs <- function (x, width = 12, ...) 
{
    if (is.character(x) || (is.atomic(x) && is.matrix(x))) 
        return(format.default(x, ...))
    if (is.null(width)) 
        width <- 12L
    rvec <- vapply(x, function(y) {
        cl <- oldClass(y)
        if (m <- match("AsIs", cl, 0L)) 
            oldClass(y) <- cl[-m]
        toString(y, width = width, ...)
    }, "")
    dim(rvec) <- dim(x)
    dimnames(rvec) <- dimnames(x)
    format.default(rvec, justify = "right")
}


is.integer <- function (x)  .Primitive("is.integer")


prod <- function (..., na.rm = FALSE)  .Primitive("prod")


as.integer <- function (x, ...)  .Primitive("as.integer")


drop <- function (x) 
.Internal(drop(x))


Sys.umask <- function (mode = NA) 
.Internal(Sys.umask(if (is.na(mode)) NA_integer_ else as.octmode(mode)))


.doTrace <- function (expr, msg) 
{
    on <- tracingState(FALSE)
    if (on) {
        on.exit(tracingState(TRUE))
        if (!missing(msg)) {
            call <- deparse(sys.call(sys.parent(1L)))
            if (length(call) > 1L) 
                call <- paste(call[[1L]], "....")
            cat("Tracing", call, msg, "\n")
        }
        exprObj <- substitute(expr)
        eval.parent(exprObj)
    }
    NULL
}


browserText <- function (n = 1L) 
.Internal(browserText(n))


is.package_version <- function (x) 
inherits(x, "package_version")


bindingIsActive <- function (sym, env) 
{
    if (is.character(sym)) 
        sym <- as.name(sym)
    .Internal(bindingIsActive(sym, env))
}


file.copy <- function (from, to, overwrite = recursive, recursive = FALSE, 
    copy.mode = TRUE, copy.date = FALSE) 
{
    if (!(nf <- length(from))) 
        return(logical())
    if (!(nt <- length(to))) 
        stop("no files to copy to")
    if (nt == 1 && dir.exists(to)) {
        if (recursive && to %in% from) 
            stop("attempt to copy a directory to itself")
        if (.Platform$OS.type == "windows") {
            from <- gsub("/", "\\", from, fixed = TRUE)
            to <- gsub("/", "\\", to, fixed = TRUE)
        }
        return(.Internal(file.copy(from, to, overwrite, recursive, 
            copy.mode, copy.date)))
    }
    else if (nf > nt) 
        stop("more 'from' files than 'to' files")
    else if (recursive) 
        warning("'recursive' will be ignored as 'to' is not a single existing directory")
    if (nt > nf) 
        from <- rep_len(from, length.out = nt)
    okay <- file.exists(from)
    if (!overwrite) 
        okay[file.exists(to)] <- FALSE
    else {
        dirtofile <- dir.exists(from[okay]) & file.exists(to[okay]) & 
            !dir.exists(to[okay])
        if (any(dirtofile)) {
            warning("cannot overwrite a non-directory with a directory")
            okay[okay] <- !dirtofile
        }
    }
    if (any(from[okay] %in% to[okay])) 
        stop("file can not be copied both 'from' and 'to'")
    if (any(okay)) {
        okay[okay] <- file.create(to[okay])
        if (any(okay)) {
            okay[okay] <- file.append(to[okay], from[okay])
            if (copy.mode || copy.date) {
                fi <- file.info(from[okay], extra_cols = FALSE)
                if (copy.mode) 
                  Sys.chmod(to[okay], fi$mode, TRUE)
                if (copy.date) 
                  Sys.setFileTime(to[okay], fi$mtime)
            }
        }
    }
    okay
}


`names<-.POSIXlt` <- function (x, value) 
{
    names(x$year) <- value
    x
}


eigen <- function (x, symmetric, only.values = FALSE, EISPACK = FALSE) 
{
    x <- unname(as.matrix(x))
    n <- nrow(x)
    if (!n) 
        stop("0 x 0 matrix")
    if (n != ncol(x)) 
        stop("non-square matrix in 'eigen'")
    n <- as.integer(n)
    if (is.na(n)) 
        stop("invalid nrow(x)")
    complex.x <- is.complex(x)
    if (!all(is.finite(x))) 
        stop("infinite or missing values in 'x'")
    if (missing(symmetric)) 
        symmetric <- isSymmetric.matrix(x)
    if (symmetric) {
        z <- if (!complex.x) 
            .Internal(La_rs(x, only.values))
        else .Internal(La_rs_cmplx(x, only.values))
        ord <- rev(seq_along(z$values))
    }
    else {
        z <- if (!complex.x) 
            .Internal(La_rg(x, only.values))
        else .Internal(La_rg_cmplx(x, only.values))
        ord <- sort.list(Mod(z$values), decreasing = TRUE)
    }
    if (only.values) 
        list(values = z$values[ord], vectors = NULL)
    else structure(class = "eigen", list(values = z$values[ord], 
        vectors = z$vectors[, ord, drop = FALSE]))
}


is.function <- function (x)  .Primitive("is.function")


month.name <- c("January", "February", "March", "April", "May", "June", "July", 
"August", "September", "October", "November", "December")


digamma <- function (x)  .Primitive("digamma")


l10n_info <- function () 
.Internal(l10n_info())


detach <- function (name, pos = 2L, unload = FALSE, character.only = FALSE, 
    force = FALSE) 
{
    if (!missing(name)) {
        if (!character.only) 
            name <- substitute(name)
        pos <- if (is.numeric(name)) 
            name
        else {
            if (!is.character(name)) 
                name <- deparse(name)
            match(name, search())
        }
        if (is.na(pos)) 
            stop("invalid 'name' argument")
    }
    packageName <- search()[[pos]]
    if (!startsWith(packageName, "package:")) 
        return(invisible(.Internal(detach(pos))))
    pkgname <- .rmpkg(packageName)
    for (pkg in search()[-1L]) {
        if (startsWith(pkg, "package:") && exists(".Depends", 
            pkg, inherits = FALSE) && pkgname %in% get(".Depends", 
            pkg, inherits = FALSE)) 
            if (force) 
                warning(gettextf("package %s is required by %s, which may no longer work correctly", 
                  sQuote(pkgname), sQuote(.rmpkg(pkg))), call. = FALSE, 
                  domain = NA)
            else stop(gettextf("package %s is required by %s so will not be detached", 
                sQuote(pkgname), sQuote(.rmpkg(pkg))), call. = FALSE, 
                domain = NA)
    }
    env <- as.environment(pos)
    libpath <- attr(env, "path")
    hook <- getHook(packageEvent(pkgname, "detach"))
    for (fun in rev(hook)) try(fun(pkgname, libpath))
    ns <- .getNamespace(pkgname)
    if (!is.null(ns) && exists(".onDetach", mode = "function", 
        where = ns, inherits = FALSE)) {
        .onDetach <- get(".onDetach", mode = "function", pos = ns, 
            inherits = FALSE)
        if (!is.null(libpath)) {
            res <- tryCatch(.onDetach(libpath), error = identity)
            if (inherits(res, "error")) {
                warning(gettextf("%s failed in %s() for '%s', details:\n  call: %s\n  error: %s", 
                  ".onDetach", "detach", pkgname, deparse(conditionCall(res))[1L], 
                  conditionMessage(res)), call. = FALSE, domain = NA)
            }
        }
    }
    else if (exists(".Last.lib", mode = "function", where = pos, 
        inherits = FALSE)) {
        .Last.lib <- get(".Last.lib", mode = "function", pos = pos, 
            inherits = FALSE)
        if (!is.null(libpath)) {
            res <- tryCatch(.Last.lib(libpath), error = identity)
            if (inherits(res, "error")) {
                warning(gettextf("%s failed in %s() for '%s', details:\n  call: %s\n  error: %s", 
                  ".Last.lib", "detach", pkgname, deparse(conditionCall(res))[1L], 
                  conditionMessage(res)), call. = FALSE, domain = NA)
            }
        }
    }
    .Internal(detach(pos))
    if (isNamespaceLoaded(pkgname)) {
        if (unload) {
            tryCatch(unloadNamespace(pkgname), error = function(e) warning(gettextf("%s namespace cannot be unloaded:\n  ", 
                sQuote(pkgname)), conditionMessage(e), call. = FALSE, 
                domain = NA))
        }
    }
    else {
        if (.isMethodsDispatchOn() && methods:::.hasS4MetaData(env)) 
            methods::cacheMetaData(env, FALSE)
        .Internal(lazyLoadDBflush(paste0(libpath, "/R/", pkgname, 
            ".rdb")))
    }
    invisible()
}


as.data.frame.noquote <- function (x, row.names = NULL, optional = FALSE, ..., nm = paste(deparse(substitute(x), 
    width.cutoff = 500L), collapse = " ")) 
{
    force(nm)
    nrows <- length(x)
    if (!(is.null(row.names) || (is.character(row.names) && length(row.names) == 
        nrows))) {
        warning(gettextf("'row.names' is not a character vector of length %d -- omitting it. Will be an error!", 
            nrows), domain = NA)
        row.names <- NULL
    }
    if (is.null(row.names)) {
        if (nrows == 0L) 
            row.names <- character()
        else if (length(row.names <- names(x)) != nrows || anyDuplicated(row.names)) 
            row.names <- .set_row_names(nrows)
    }
    if (!is.null(names(x))) 
        names(x) <- NULL
    value <- list(x)
    if (!optional) 
        names(value) <- nm
    structure(value, row.names = row.names, class = "data.frame")
}


integer <- function (length = 0L) 
.Internal(vector("integer", length))


getCallingDLL <- function (f = sys.function(-1), doStop = FALSE) 
{
    e <- environment(f)
    if (!isNamespace(e)) {
        if (doStop) 
            stop("function is not in a namespace, so cannot locate associated DLL")
        else return(NULL)
    }
    if (is.null(r <- getCallingDLLe(e)) && doStop) 
        stop("looking for DLL for native routine call, but no DLLs in namespace of call")
    r
}


tracingState <- function (on = NULL) 
.Internal(traceOnOff(on))


as.function <- function (x, ...) 
UseMethod("as.function")


attach <- function (what, pos = 2L, name = deparse(substitute(what), backtick = FALSE), 
    warn.conflicts = TRUE) 
{
    checkConflicts <- function(env) {
        dont.mind <- c("last.dump", "last.warning", ".Last.value", 
            ".Random.seed", ".Last.lib", ".onDetach", ".packageName", 
            ".noGenerics", ".required", ".no_S3_generics", ".requireCachedGenerics")
        sp <- search()
        for (i in seq_along(sp)) {
            if (identical(env, as.environment(i))) {
                db.pos <- i
                break
            }
        }
        ob <- names(as.environment(db.pos))
        if (.isMethodsDispatchOn()) {
            these <- ob[startsWith(ob, ".__T__")]
            gen <- gsub(".__T__(.*):([^:]+)", "\\1", these)
            from <- gsub(".__T__(.*):([^:]+)", "\\2", these)
            gen <- gen[from != ".GlobalEnv"]
            ob <- ob[!(ob %in% gen)]
        }
        ipos <- seq_along(sp)[-c(db.pos, match(c("Autoloads", 
            "CheckExEnv"), sp, 0L))]
        for (i in ipos) {
            obj.same <- match(names(as.environment(i)), ob, nomatch = 0L)
            if (any(obj.same > 0L)) {
                same <- ob[obj.same]
                same <- same[!(same %in% dont.mind)]
                Classobjs <- which(startsWith(same, ".__"))
                if (length(Classobjs)) 
                  same <- same[-Classobjs]
                same.isFn <- function(where) vapply(same, exists, 
                  NA, where = where, mode = "function", inherits = FALSE)
                same <- same[same.isFn(i) == same.isFn(db.pos)]
                if (length(same)) {
                  pkg <- if (sum(sp == sp[i]) > 1L) 
                    sprintf("%s (pos = %d)", sp[i], i)
                  else sp[i]
                  message(.maskedMsg(sort(same), pkg, by = i < 
                    db.pos), domain = NA)
                }
            }
        }
    }
    if (pos == 1L) 
        stop("'pos=1' is not possible and has been warned about for years")
    if (is.character(what) && (length(what) == 1L)) {
        if (!file.exists(what)) 
            stop(gettextf("file '%s' not found", what), domain = NA)
        if (missing(name)) 
            name <- paste0("file:", what)
        value <- .Internal(attach(NULL, pos, name))
        load(what, envir = as.environment(pos))
    }
    else value <- .Internal(attach(what, pos, name))
    if (warn.conflicts && !exists(".conflicts.OK", envir = value, 
        inherits = FALSE)) {
        checkConflicts(value)
    }
    if (length(names(value)) && .isMethodsDispatchOn()) 
        methods::cacheMetaData(value, TRUE)
    invisible(value)
}


dump <- function (list, file = "dumpdata.R", append = FALSE, control = "all", 
    envir = parent.frame(), evaluate = TRUE) 
{
    if (is.character(file)) {
        ex <- sapply(list, exists, envir = envir)
        if (!any(ex)) 
            return(invisible(character()))
        if (nzchar(file)) {
            file <- file(file, if (append) 
                "a"
            else "w")
            on.exit(close(file), add = TRUE)
        }
        else file <- stdout()
    }
    opts <- .deparseOpts(control)
    .Internal(dump(list, file, envir, opts, evaluate))
}


as.POSIXct.numeric <- function (x, tz = "", origin, ...) 
{
    if (missing(origin)) 
        stop("'origin' must be supplied")
    .POSIXct(as.POSIXct(origin, tz = "GMT", ...) + x, tz)
}


is.recursive <- function (x)  .Primitive("is.recursive")


unlink <- function (x, recursive = FALSE, force = FALSE) 
.Internal(unlink(as.character(x), recursive, force))


seq_along <- function (along.with)  .Primitive("seq_along")


pcre_config <- function () 
.Internal(pcre_config())


`[<-.POSIXct` <- function (x, ..., value) 
{
    if (!length(value)) 
        return(x)
    value <- unclass(as.POSIXct(value))
    .POSIXct(NextMethod(.Generic), attr(x, "tzone"), oldClass(x))
}


shell.exec <- function (file) 
.Internal(shell.exec(file))


unlist <- function (x, recursive = TRUE, use.names = TRUE) 
{
    if (.Internal(islistfactor(x, recursive))) {
        URapply <- if (recursive) 
            function(x, Fn) .Internal(unlist(rapply(x, Fn, how = "list"), 
                recursive, FALSE))
        else function(x, Fn) .Internal(unlist(lapply(x, Fn), 
            recursive, FALSE))
        lv <- unique(URapply(x, levels))
        nm <- if (use.names) 
            names(.Internal(unlist(x, recursive, use.names)))
        res <- match(URapply(x, as.character), lv)
        structure(res, levels = lv, names = nm, class = "factor")
    }
    else .Internal(unlist(x, recursive, use.names))
}


`[<-.POSIXlt` <- function (x, i, j, value) 
{
    if (!(mj <- missing(j))) 
        if (!is.character(j) || (length(j) != 1L)) 
            stop("component subscript must be a character string")
    if (!length(value)) 
        return(x)
    cl <- oldClass(x)
    class(x) <- NULL
    if (missing(i)) {
        if (mj) 
            x <- as.POSIXlt(value)
        else x[[j]] <- value
    }
    else {
        ici <- is.character(i)
        nms <- names(x$year)
        if (mj) {
            value <- unclass(as.POSIXlt(value))
            if (ici) {
                for (n in names(x)) names(x[[n]]) <- nms
            }
            for (n in names(x)) x[[n]][i] <- value[[n]]
        }
        else {
            if (ici) {
                names(x[[j]]) <- nms
            }
            x[[j]][i] <- value
        }
    }
    class(x) <- cl
    x
}


all.equal <- function (target, current, ...) 
UseMethod("all.equal")


regmatches <- function (x, m, invert = FALSE) 
{
    if (length(x) != length(m)) 
        stop(gettextf("%s and %s must have the same length", 
            sQuote("x"), sQuote("m")), domain = NA)
    ili <- is.list(m)
    itype <- "chars"
    useBytes <- if (ili) 
        any(unlist(lapply(m, attr, "index.type")) == "bytes")
    else any(attr(m, "index.type") == "bytes")
    if (useBytes) {
        itype <- Encoding(x) <- "bytes"
    }
    if (!ili && isFALSE(invert)) {
        so <- m[ind <- (!is.na(m) & (m > -1L))]
        eo <- so + attr(m, "match.length")[ind] - 1L
        return(substring(x[ind], so, eo))
    }
    y <- if (is.na(invert)) {
        Map(function(u, so, ml) {
            if ((n <- length(so)) == 1L) {
                if (is.na(so)) 
                  return(NA_character_)
                else if (so == -1L) 
                  return(u)
            }
            eo <- so + ml - 1L
            if (n > 1L) {
                if (any(eo[-n] >= so[-1L])) 
                  stop(gettextf("need non-overlapping matches for %s", 
                    sQuote("invert = NA")), domain = NA)
            }
            beg <- c(1L, c(rbind(so, eo + 1L)))
            end <- c(c(rbind(so - 1L, eo)), nchar(u, itype))
            substring(u, beg, end)
        }, x, m, if (ili) 
            lapply(m, attr, "match.length")
        else attr(m, "match.length"), USE.NAMES = FALSE)
    }
    else if (invert) {
        Map(function(u, so, ml) {
            if ((n <- length(so)) == 1L) {
                if (is.na(so)) 
                  return(NA_character_)
                else if (so == -1L) 
                  return(u)
            }
            beg <- if (n > 1L) {
                eo <- so + ml - 1L
                if (any(eo[-n] >= so[-1L])) 
                  stop(gettextf("need non-overlapping matches for %s", 
                    sQuote("invert = TRUE")), domain = NA)
                c(1L, eo + 1L)
            }
            else {
                c(1L, so + ml)
            }
            end <- c(so - 1L, nchar(u, itype))
            substring(u, beg, end)
        }, x, m, if (ili) 
            lapply(m, attr, "match.length")
        else attr(m, "match.length"), USE.NAMES = FALSE)
    }
    else {
        Map(function(u, so, ml) {
            if (length(so) == 1L) {
                if (is.na(so) || (so == -1L)) 
                  return(character())
            }
            substring(u, so, so + ml - 1L)
        }, x, m, lapply(m, attr, "match.length"), USE.NAMES = FALSE)
    }
    names(y) <- names(x)
    y
}


is.numeric_version <- function (x) 
inherits(x, "numeric_version")


.Device <- "null device"


.tryResumeInterrupt <- function () 
{
    r <- findRestart("resume")
    if (!is.null(r)) 
        invokeRestart(r)
}


getNamespaceName <- function (ns) 
{
    ns <- asNamespace(ns)
    if (isBaseNamespace(ns)) 
        "base"
    else .getNamespaceInfo(ns, "spec")["name"]
}


parent.frame <- function (n = 1) 
.Internal(parent.frame(n))


factor <- function (x = character(), levels, labels = levels, exclude = NA, 
    ordered = is.ordered(x), nmax = NA) 
{
    if (is.null(x)) 
        x <- character()
    nx <- names(x)
    if (missing(levels)) {
        y <- unique(x, nmax = nmax)
        ind <- order(y)
        levels <- unique(as.character(y)[ind])
    }
    force(ordered)
    if (!is.character(x)) 
        x <- as.character(x)
    levels <- levels[is.na(match(levels, exclude))]
    f <- match(x, levels)
    if (!is.null(nx)) 
        names(f) <- nx
    if (missing(labels)) {
        levels(f) <- as.character(levels)
    }
    else {
        nlab <- length(labels)
        if (nlab == length(levels)) {
            nlevs <- unique(xlevs <- as.character(labels))
            at <- attributes(f)
            at$levels <- nlevs
            f <- match(xlevs, nlevs)[f]
            attributes(f) <- at
        }
        else if (nlab == 1L) 
            levels(f) <- paste0(labels, seq_along(levels))
        else stop(gettextf("invalid 'labels'; length %d should be 1 or %d", 
            nlab, length(levels)), domain = NA)
    }
    class(f) <- c(if (ordered) "ordered", "factor")
    f
}


message <- function (..., domain = NULL, appendLF = TRUE) 
{
    args <- list(...)
    cond <- if (length(args) == 1L && inherits(args[[1L]], "condition")) {
        if (nargs() > 1L) 
            warning("additional arguments ignored in message()")
        args[[1L]]
    }
    else {
        msg <- .makeMessage(..., domain = domain, appendLF = appendLF)
        call <- sys.call()
        simpleMessage(msg, call)
    }
    defaultHandler <- function(c) {
        cat(conditionMessage(c), file = stderr(), sep = "")
    }
    withRestarts({
        signalCondition(cond)
        defaultHandler(cond)
    }, muffleMessage = function() NULL)
    invisible()
}


lockBinding <- function (sym, env) 
{
    if (is.character(sym)) 
        sym <- as.name(sym)
    .Internal(lockBinding(sym, env))
}


.valid.factor <- function (object) 
{
    levs <- levels(object)
    if (!is.character(levs)) 
        return("factor levels must be \"character\"")
    if (d <- anyDuplicated(levs)) 
        return(sprintf("duplicated level [%d] in factor", d))
    TRUE
}


Position <- function (f, x, right = FALSE, nomatch = NA_integer_) 
{
    ind <- if (right) 
        rev(seq_along(x))
    else seq_along(x)
    for (i in ind) if (f(x[[i]])) 
        return(i)
    nomatch
}


rep.numeric_version <- function (x, ...) 
structure(NextMethod("rep"), class = oldClass(x))


.Defunct <- function (new, package = NULL, msg) 
{
    fname <- as.character(sys.call(sys.parent())[[1L]])
    if (missing(msg)) {
        msg <- gettextf("'%s' is defunct.\n", fname[length(fname)])
        if (!missing(new)) 
            msg <- c(msg, gettextf("Use '%s' instead.\n", new))
        msg <- c(msg, if (!is.null(package)) gettextf("See help(\"Defunct\") and help(\"%s-defunct\").", 
            package) else gettext("See help(\"Defunct\")"))
    }
    else msg <- as.character(msg)
    msg <- paste(msg, collapse = "")
    if (missing(new)) 
        new <- NULL
    stop(errorCondition(msg, old = fname, new = new, package = package, 
        class = "defunctError"))
}





## Package Info

.skeleton_package_title = "The R Base Package"

.skeleton_package_version = "3.6.0"

.skeleton_package_depends = ""

.skeleton_package_imports = ""


## Internal

.skeleton_version = 5


## EOF
##
## Exported symobls in package `survival`
##

## Exported package methods

yates_setup <- function (fit, ...) 
UseMethod("yates_setup", fit)


survexp <- function (formula, data, weights, subset, na.action, rmap, times, 
    method = c("ederer", "hakulinen", "conditional", "individual.h", 
        "individual.s"), cohort = TRUE, conditional = FALSE, 
    ratetable = survival::survexp.us, scale = 1, se.fit, model = FALSE, 
    x = FALSE, y = FALSE) 
{
    Call <- match.call()
    m <- match.call(expand.dots = FALSE)
    m <- m[c(1, match(c("formula", "data", "weights", "subset", 
        "na.action"), names(m), nomatch = 0))]
    m[[1L]] <- quote(stats::model.frame)
    Terms <- if (missing(data)) 
        terms(formula, "ratetable")
    else terms(formula, "ratetable", data = data)
    rate <- attr(Terms, "specials")$ratetable
    if (length(rate) > 1) 
        stop("Can have only 1 ratetable() call in a formula")
    if (length(rate) == 1) {
        if (!missing(rmap)) 
            stop("The ratetable() call in a formula is depreciated")
        stemp <- untangle.specials(Terms, "ratetable")
        rcall <- as.call(parse(text = stemp$var)[[1]])
        rcall[[1]] <- as.name("list")
        Terms <- Terms[-stemp$terms]
    }
    else if (!missing(rmap)) {
        rcall <- substitute(rmap)
        if (!is.call(rcall) || rcall[[1]] != as.name("list")) 
            stop("Invalid rcall argument")
    }
    else rcall <- NULL
    if (is.ratetable(ratetable)) {
        varlist <- names(dimnames(ratetable))
        if (is.null(varlist)) 
            varlist <- attr(ratetable, "dimid")
    }
    else if (inherits(ratetable, "coxph")) {
        varlist <- all.vars(delete.response(ratetable$terms))
    }
    else stop("Invalid rate table")
    temp <- match(names(rcall)[-1], varlist)
    if (any(is.na(temp))) 
        stop("Variable not found in the ratetable:", (names(rcall))[is.na(temp)])
    if (any(!(varlist %in% names(rcall)))) {
        to.add <- varlist[!(varlist %in% names(rcall))]
        temp1 <- paste(text = paste(to.add, to.add, sep = "="), 
            collapse = ",")
        if (is.null(rcall)) 
            rcall <- parse(text = paste("list(", temp1, ")"))[[1]]
        else {
            temp2 <- deparse(rcall)
            rcall <- parse(text = paste("c(", temp2, ",list(", 
                temp1, "))"))[[1]]
        }
    }
    newvar <- all.vars(rcall)
    if (length(newvar) > 0) {
        tform <- paste(paste(deparse(Terms), collapse = ""), 
            paste(newvar, collapse = "+"), sep = "+")
        m$formula <- as.formula(tform, environment(Terms))
    }
    m <- eval(m, parent.frame())
    n <- nrow(m)
    if (n == 0) 
        stop("Data set has 0 rows")
    if (!missing(se.fit) && se.fit) 
        warning("se.fit value ignored")
    weights <- model.extract(m, "weights")
    if (length(weights) == 0) 
        weights <- rep(1, n)
    if (class(ratetable) == "ratetable" && any(weights != 1)) 
        warning("weights ignored")
    if (any(attr(Terms, "order") > 1)) 
        stop("Survexp cannot have interaction terms")
    if (!missing(times)) {
        if (any(times < 0)) 
            stop("Invalid time point requested")
        if (length(times) > 1) 
            if (any(diff(times) < 0)) 
                stop("Times must be in increasing order")
    }
    Y <- model.extract(m, "response")
    no.Y <- is.null(Y)
    if (no.Y) {
        if (missing(times)) {
            if (is.ratetable(ratetable)) 
                stop("either a times argument or a response is needed")
        }
        else newtime <- times
    }
    else {
        if (is.matrix(Y)) {
            if (is.Surv(Y) && attr(Y, "type") == "right") 
                Y <- Y[, 1]
            else stop("Illegal response value")
        }
        if (any(Y < 0)) 
            stop("Negative follow up time")
        temp <- unique(Y)
        if (missing(times)) 
            newtime <- sort(temp)
        else newtime <- sort(unique(c(times, temp[temp < max(times)])))
    }
    if (!missing(method)) 
        method <- match.arg(method)
    else {
        if (!missing(conditional) && conditional) 
            method <- "conditional"
        else {
            if (no.Y) 
                method <- "ederer"
            else method <- "hakulinen"
        }
        if (!missing(cohort) && !cohort) 
            method <- "individual.s"
    }
    if (no.Y && (method != "ederer")) 
        stop("a response is required in the formula unless method='ederer'")
    ovars <- attr(Terms, "term.labels")
    rdata <- data.frame(eval(rcall, m), stringsAsFactors = TRUE)
    if (is.ratetable(ratetable)) {
        israte <- TRUE
        if (no.Y) {
            Y <- rep(max(times), n)
        }
        rtemp <- match.ratetable(rdata, ratetable)
        R <- rtemp$R
    }
    else if (inherits(ratetable, "coxph")) {
        israte <- FALSE
        Terms <- ratetable$terms
        if (any(names(m[, rate]) != attr(ratetable$terms, "term.labels"))) 
            stop("Unable to match new data to old formula")
    }
    else stop("Invalid ratetable")
    if (substring(method, 1, 10) == "individual") {
        if (no.Y) 
            stop("for individual survival an observation time must be given")
        if (israte) 
            temp <- survexp.fit(1:n, R, Y, max(Y), TRUE, ratetable)
        else {
            rmatch <- match(names(data), names(rdata))
            if (any(is.na(rmatch))) 
                rdata <- cbind(rdata, data[, is.na(rmatch)])
            temp <- survexp.cfit(1:n, rdata, Y, "individual", 
                ratetable)
        }
        if (method == "individual.s") 
            xx <- temp$surv
        else xx <- -log(temp$surv)
        names(xx) <- row.names(m)
        na.action <- attr(m, "na.action")
        if (length(na.action)) 
            return(naresid(na.action, xx))
        else return(xx)
    }
    if (length(ovars) == 0) 
        X <- rep(1, n)
    else {
        odim <- length(ovars)
        for (i in 1:odim) {
            temp <- m[[ovars[i]]]
            ctemp <- class(temp)
            if (!is.null(ctemp) && ctemp == "tcut") 
                stop("Can't use tcut variables in expected survival")
        }
        X <- strata(m[ovars])
    }
    if (israte) 
        temp <- survexp.fit(as.numeric(X), R, Y, newtime, method == 
            "conditional", ratetable)
    else {
        temp <- survexp.cfit(as.numeric(X), rdata, Y, method, 
            ratetable, weights)
        newtime <- temp$time
    }
    if (missing(times)) {
        n.risk <- temp$n
        surv <- temp$surv
    }
    else {
        if (israte) 
            keep <- match(times, newtime)
        else {
            n <- length(temp$time)
            keep <- approx(temp$time, 1:n, xout = times, yleft = 0, 
                method = "constant", f = 0, rule = 2)$y
        }
        if (is.matrix(temp$surv)) {
            surv <- (rbind(1, temp$surv))[keep + 1, , drop = FALSE]
            n.risk <- temp$n[pmax(1, keep), , drop = FALSE]
        }
        else {
            surv <- (c(1, temp$surv))[keep + 1]
            n.risk <- temp$n[pmax(1, keep)]
        }
        newtime <- times
    }
    newtime <- newtime/scale
    if (is.matrix(surv)) {
        dimnames(surv) <- list(NULL, levels(X))
        out <- list(call = Call, surv = drop(surv), n.risk = drop(n.risk), 
            time = newtime)
    }
    else {
        out <- list(call = Call, surv = c(surv), n.risk = c(n.risk), 
            time = newtime)
    }
    if (model) 
        out$model <- m
    else {
        if (x) 
            out$x <- X
        if (y) 
            out$y <- Y
    }
    if (israte && !is.null(rtemp$summ)) 
        out$summ <- rtemp$summ
    if (no.Y) 
        out$method <- "Ederer"
    else if (conditional) 
        out$method <- "conditional"
    else out$method <- "cohort"
    class(out) <- c("survexp", "survfit")
    out
}


survfit.formula <- function (formula, data, weights, subset, na.action, stype = 1, 
    ctype = 1, id, cluster, istate, timefix = TRUE, etype, error, 
    ...) 
{
    Call <- match.call()
    Call[[1]] <- as.name("survfit")
    indx <- match(c("formula", "data", "weights", "subset", "na.action", 
        "istate", "id", "cluster", "etype"), names(Call), nomatch = 0)
    if (indx[1] == 0) 
        stop("a formula argument is required")
    temp <- Call[c(1, indx)]
    temp[[1L]] <- quote(stats::model.frame)
    mf <- eval.parent(temp)
    Terms <- terms(formula, c("strata", "cluster"))
    ord <- attr(Terms, "order")
    if (length(ord) & any(ord != 1)) 
        stop("Interaction terms are not valid for this function")
    n <- nrow(mf)
    Y <- model.extract(mf, "response")
    if (!is.Surv(Y)) 
        stop("Response must be a survival object")
    casewt <- model.extract(mf, "weights")
    if (is.null(casewt)) 
        casewt <- rep(1, n)
    else {
        if (!is.numeric(casewt)) 
            stop("weights must be numeric")
        if (any(!is.finite(casewt))) 
            stop("weights must be finite")
        if (any(casewt < 0)) 
            stop("weights must be non-negative")
        casewt <- as.numeric(casewt)
    }
    if (!is.null(attr(Terms, "offset"))) 
        warning("Offset term ignored")
    id <- model.extract(mf, "id")
    istate <- model.extract(mf, "istate")
    cluster <- model.extract(mf, "cluster")
    temp <- untangle.specials(Terms, "cluster")
    if (length(temp$vars) > 0) {
        if (length(cluster) > 0) 
            stop("cluster appears as both an argument and a model term")
        if (length(temp$vars) > 1) 
            stop("can not have two cluster terms")
        cluster <- mf[[temp$vars]]
        Terms <- Terms[-temp$terms]
    }
    ll <- attr(Terms, "term.labels")
    if (length(ll) == 0) 
        X <- factor(rep(1, n))
    else X <- strata(mf[ll])
    if (!is.Surv(Y)) 
        stop("y must be a Surv object")
    etype <- model.extract(mf, "etype")
    if (!is.null(etype)) {
        if (attr(Y, "type") == "mcounting" || attr(Y, "type") == 
            "mright") 
            stop("cannot use both the etype argument and mstate survival type")
        if (length(istate)) 
            stop("cannot use both the etype and istate arguments")
        status <- Y[, ncol(Y)]
        etype <- as.factor(etype)
        temp <- table(etype, status == 0)
        if (all(rowSums(temp == 0) == 1)) {
            newlev <- levels(etype)[order(-temp[, 2])]
        }
        else newlev <- c(" ", levels(etype)[temp[, 1] > 0])
        status <- factor(ifelse(status == 0, 0, as.numeric(etype)), 
            labels = newlev)
        if (attr(Y, "type") == "right") 
            Y <- Surv(Y[, 1], status, type = "mstate")
        else if (attr(Y, "type") == "counting") 
            Y <- Surv(Y[, 1], Y[, 2], status, type = "mstate")
        else stop("etype argument incompatable with survival type")
    }
    if (!is.logical(timefix) || length(timefix) > 1) 
        stop("invalid value for timefix option")
    if (timefix) 
        newY <- aeqSurv(Y)
    else newY <- Y
    if (attr(Y, "type") == "left" || attr(Y, "type") == "interval") 
        temp <- survfitTurnbull(X, newY, casewt, ...)
    else if (attr(Y, "type") == "right" || attr(Y, "type") == 
        "counting") 
        temp <- survfitKM(X, newY, casewt, stype = stype, ctype = ctype, 
            id = id, cluster = cluster, ...)
    else if (attr(Y, "type") == "mright" || attr(Y, "type") == 
        "mcounting") 
        temp <- survfitCI(X, newY, weights = casewt, stype = stype, 
            ctype = ctype, id = id, cluster = cluster, istate = istate, 
            ...)
    else {
        stop("unrecognized survival type")
    }
    if (is.null(temp$states)) 
        class(temp) <- "survfit"
    else class(temp) <- c("survfitms", "survfit")
    if (!is.null(attr(mf, "na.action"))) 
        temp$na.action <- attr(mf, "na.action")
    temp$call <- Call
    temp
}


strata <- function (..., na.group = FALSE, shortlabel, sep = ", ") 
{
    words <- as.character((match.call())[-1])
    allf <- list(...)
    if (length(allf) == 1 && is.list(ttt <- unclass(allf[[1]]))) 
        allf <- ttt
    nterms <- length(allf)
    if (is.null(names(allf))) {
        argname <- words[1:nterms]
        if (missing(shortlabel)) 
            shortlabel <- all(sapply(allf, function(x) is.character(x) | 
                inherits(x, "factor")))
    }
    else {
        argname <- ifelse(names(allf) == "", words[1:nterms], 
            names(allf))
        if (missing(shortlabel)) 
            shortlabel <- FALSE
    }
    arglength <- sapply(allf, length)
    if (any(arglength != arglength[1])) 
        stop("all arguments must be the same length")
    if (!all(sapply(allf, is.atomic))) 
        stop("all arguments must be vectors")
    what <- allf[[1]]
    if (is.null(levels(what))) 
        what <- factor(what)
    levs <- unclass(what) - 1
    wlab <- levels(what)
    if (na.group && any(is.na(what))) {
        levs[is.na(levs)] <- length(wlab)
        wlab <- c(wlab, "NA")
    }
    if (shortlabel) 
        labs <- wlab
    else labs <- paste(argname[1], wlab, sep = "=")
    for (i in (1:nterms)[-1]) {
        what <- allf[[i]]
        if (is.null(levels(what))) 
            what <- factor(what)
        wlab <- levels(what)
        wlev <- unclass(what) - 1
        if (na.group && any(is.na(wlev))) {
            wlev[is.na(wlev)] <- length(wlab)
            wlab <- c(wlab, "NA")
        }
        if (!shortlabel) 
            wlab <- format(paste(argname[i], wlab, sep = "="))
        levs <- wlev + levs * (length(wlab))
        labs <- paste(rep(labs, rep(length(wlab), length(labs))), 
            rep(wlab, length(labs)), sep = sep)
    }
    levs <- levs + 1
    ulevs <- sort(unique(levs[!is.na(levs)]))
    levs <- match(levs, ulevs)
    labs <- labs[ulevs]
    factor(levs, labels = labs)
}


pyears <- function (formula, data, weights, subset, na.action, rmap, ratetable, 
    scale = 365.25, expect = c("event", "pyears"), model = FALSE, 
    x = FALSE, y = FALSE, data.frame = FALSE) 
{
    expect <- match.arg(expect)
    Call <- match.call()
    indx <- match(c("formula", "data", "weights", "subset", "na.action"), 
        names(Call), nomatch = 0)
    if (indx[1] == 0) 
        stop("A formula argument is required")
    m <- Call[c(1, indx)]
    m[[1L]] <- quote(stats::model.frame)
    Terms <- if (missing(data)) 
        terms(formula, "ratetable")
    else terms(formula, "ratetable", data = data)
    if (any(attr(Terms, "order") > 1)) 
        stop("Pyears cannot have interaction terms")
    rate <- attr(Terms, "specials")$ratetable
    if (length(rate) > 0 || !missing(rmap) || !missing(ratetable)) {
        has.ratetable <- TRUE
        if (length(rate) > 1) 
            stop("Can have only 1 ratetable() call in a formula")
        if (missing(ratetable)) 
            stop("No rate table specified")
        if (length(rate) == 1) {
            if (!missing(rmap)) 
                stop("The ratetable() call in a formula is depreciated")
            stemp <- untangle.specials(Terms, "ratetable")
            rcall <- as.call(parse(text = stemp$var)[[1]])
            rcall[[1]] <- as.name("list")
            Terms <- Terms[-stemp$terms]
        }
        else if (!missing(rmap)) {
            rcall <- substitute(rmap)
            if (!is.call(rcall) || rcall[[1]] != as.name("list")) 
                stop("Invalid rcall argument")
        }
        else rcall <- NULL
        if (is.ratetable(ratetable)) {
            varlist <- names(dimnames(ratetable))
            if (is.null(varlist)) 
                varlist <- attr(ratetable, "dimid")
        }
        else if (inherits(ratetable, "coxph")) {
            varlist <- all.vars(delete.response(ratetable$terms))
        }
        else stop("Invalid rate table")
        temp <- match(names(rcall)[-1], varlist)
        if (any(is.na(temp))) 
            stop("Variable not found in the ratetable:", (names(rcall))[is.na(temp)])
        if (any(!(varlist %in% names(rcall)))) {
            to.add <- varlist[!(varlist %in% names(rcall))]
            temp1 <- paste(text = paste(to.add, to.add, sep = "="), 
                collapse = ",")
            if (is.null(rcall)) 
                rcall <- parse(text = paste("list(", temp1, ")"))[[1]]
            else {
                temp2 <- deparse(rcall)
                rcall <- parse(text = paste("c(", temp2, ",list(", 
                  temp1, "))"))[[1]]
            }
        }
        newvar <- all.vars(rcall)
        if (length(newvar) > 0) {
            tform <- paste(paste(deparse(Terms), collapse = ""), 
                paste(newvar, collapse = "+"), sep = "+")
            m$formula <- as.formula(tform, environment(Terms))
        }
    }
    else has.ratetable <- FALSE
    m <- eval(m, parent.frame())
    Y <- model.extract(m, "response")
    if (is.null(Y)) 
        stop("Follow-up time must appear in the formula")
    if (!is.Surv(Y)) {
        if (any(Y < 0)) 
            stop("Negative follow up time")
        Y <- as.matrix(Y)
        if (ncol(Y) > 2) 
            stop("Y has too many columns")
    }
    else {
        stype <- attr(Y, "type")
        if (stype == "right") {
            if (any(Y[, 1] < 0)) 
                stop("Negative survival time")
            nzero <- sum(Y[, 1] == 0 & Y[, 2] == 1)
            if (nzero > 0) 
                warning(paste(nzero, "observations with an event and 0 follow-up time,", 
                  "any rate calculations are statistically questionable"))
        }
        else if (stype != "counting") 
            stop("Only right-censored and counting process survival types are supported")
    }
    n <- nrow(Y)
    if (is.null(n) || n == 0) 
        stop("Data set has 0 observations")
    weights <- model.extract(m, "weights")
    if (is.null(weights)) 
        weights <- rep(1, n)
    if (has.ratetable) {
        rdata <- data.frame(eval(rcall, m), stringsAsFactors = TRUE)
        if (is.ratetable(ratetable)) {
            israte <- TRUE
            rtemp <- match.ratetable(rdata, ratetable)
            R <- rtemp$R
        }
        else if (inherits(ratetable, "coxph")) {
            israte <- FALSE
            Terms <- ratetable$terms
            if (!is.null(attr(Terms, "offset"))) 
                stop("Cannot deal with models that contain an offset")
            strats <- attr(Terms, "specials")$strata
            if (length(strats)) 
                stop("pyears cannot handle stratified Cox models")
            if (any(names(m[, rate]) != attr(ratetable$terms, 
                "term.labels"))) 
                stop("Unable to match new data to old formula")
            R <- model.matrix.coxph(ratetable, data = rdata)
        }
        else stop("Invalid ratetable")
    }
    ovars <- attr(Terms, "term.labels")
    if (length(ovars) == 0) {
        X <- rep(1, n)
        ofac <- odim <- odims <- ocut <- 1
    }
    else {
        odim <- length(ovars)
        ocut <- NULL
        odims <- ofac <- double(odim)
        X <- matrix(0, n, odim)
        outdname <- vector("list", odim)
        names(outdname) <- attr(Terms, "term.labels")
        for (i in 1:odim) {
            temp <- m[[ovars[i]]]
            if (inherits(temp, "tcut")) {
                X[, i] <- temp
                temp2 <- attr(temp, "cutpoints")
                odims[i] <- length(temp2) - 1
                ocut <- c(ocut, temp2)
                ofac[i] <- 0
                outdname[[i]] <- attr(temp, "labels")
            }
            else {
                temp2 <- as.factor(temp)
                X[, i] <- temp2
                temp3 <- levels(temp2)
                odims[i] <- length(temp3)
                ofac[i] <- 1
                outdname[[i]] <- temp3
            }
        }
    }
    ocut <- c(ocut, 0)
    osize <- prod(odims)
    if (has.ratetable) {
        atts <- attributes(ratetable)
        datecheck <- function(x) inherits(x, c("Date", "POSIXt", 
            "date", "chron"))
        cuts <- lapply(attr(ratetable, "cutpoints"), function(x) if (!is.null(x) & 
            datecheck(x)) 
            ratetableDate(x)
        else x)
        if (is.null(atts$type)) {
            rfac <- atts$factor
            us.special <- (rfac > 1)
        }
        else {
            rfac <- 1 * (atts$type == 1)
            us.special <- (atts$type == 4)
        }
        if (any(us.special)) {
            if (sum(us.special) > 1) 
                stop("more than one type=4 in a rate table")
            if (is.null(atts$dimid)) 
                dimid <- names(atts$dimnames)
            else dimid <- atts$dimid
            cols <- match(c("age", "year"), dimid)
            if (any(is.na(cols))) 
                stop("ratetable does not have expected shape")
            bdate <- as.Date("1960-01-01") + (R[, cols[2]] - 
                R[, cols[1]])
            byear <- format(bdate, "%Y")
            offset <- as.numeric(bdate - as.Date(paste0(byear, 
                "-01-01")))
            R[, cols[2]] <- R[, cols[2]] - offset
            if (any(rfac > 1)) {
                temp <- which(us.special)
                nyear <- length(cuts[[temp]])
                nint <- rfac[temp]
                cuts[[temp]] <- round(approx(nint * (1:nyear), 
                  cuts[[temp]], nint:(nint * nyear))$y - 1e-04)
            }
        }
        docount <- is.Surv(Y)
        clist <- .C(Cpyears1, as.integer(n), as.integer(ncol(Y)), 
            as.integer(is.Surv(Y)), as.double(Y), as.double(weights), 
            as.integer(length(atts$dim)), as.integer(rfac), as.integer(atts$dim), 
            as.double(unlist(cuts)), as.double(ratetable), as.double(R), 
            as.integer(odim), as.integer(ofac), as.integer(odims), 
            as.double(ocut), as.integer(expect == "event"), as.double(X), 
            pyears = double(osize), pn = double(osize), pcount = double(if (docount) osize else 1), 
            pexpect = double(osize), offtable = double(1))[18:22]
    }
    else {
        docount <- as.integer(ncol(Y) > 1)
        clist <- .C(Cpyears2, as.integer(n), as.integer(ncol(Y)), 
            as.integer(docount), as.double(Y), as.double(weights), 
            as.integer(odim), as.integer(ofac), as.integer(odims), 
            as.double(ocut), as.double(X), pyears = double(osize), 
            pn = double(osize), pcount = double(if (docount) osize else 1), 
            offtable = double(1))[11:14]
    }
    has.tcut <- any(sapply(m, function(x) inherits(x, "tcut")))
    if (data.frame) {
        if (length(ovars) == 0) {
            keep <- TRUE
            df <- data.frame(pyears = clist$pyears/scale, n = clist$pn)
        }
        else {
            keep <- (clist$pyears > 0)
            names(outdname) <- ovars
            if (length(outdname) == 1) {
                df <- data.frame((outdname[[1]])[keep], pyears = clist$pyears[keep]/scale, 
                  n = clist$pn[keep])
                names(df) <- c(names(outdname), "pyears", "n")
            }
            else {
                df <- cbind(do.call("expand.grid", outdname)[keep, 
                  ], pyears = clist$pyears[keep]/scale, n = clist$pn[keep])
            }
        }
        row.names(df) <- 1:nrow(df)
        if (has.ratetable) 
            df$expected <- clist$pexpect[keep]
        if (expect == "pyears") 
            df$expected <- df$expected/scale
        if (docount) 
            df$event <- clist$pcount[keep]
        for (i in 1:length(ovars)) {
            temp <- m[[ovars[i]]]
            if (is.factor(temp)) 
                df[[ovars[i]]] <- factor(df[[ovars[i]]], levels(temp))
        }
        out <- list(call = Call, data = df, offtable = clist$offtable/scale, 
            tcut = has.tcut)
        if (has.ratetable && !is.null(rtemp$summ)) 
            out$summary <- rtemp$summ
    }
    else if (prod(odims) == 1) {
        out <- list(call = Call, pyears = clist$pyears/scale, 
            n = clist$pn, offtable = clist$offtable/scale, tcut = has.tcut)
        if (has.ratetable) {
            out$expected <- clist$pexpect
            if (expect == "pyears") 
                out$expected <- out$expected/scale
            if (!is.null(rtemp$summ)) 
                out$summary <- rtemp$summ
        }
        if (docount) 
            out$event <- clist$pcount
    }
    else {
        out <- list(call = Call, pyears = array(clist$pyears/scale, 
            dim = odims, dimnames = outdname), n = array(clist$pn, 
            dim = odims, dimnames = outdname), offtable = clist$offtable/scale, 
            tcut = has.tcut)
        if (has.ratetable) {
            out$expected <- array(clist$pexpect, dim = odims, 
                dimnames = outdname)
            if (expect == "pyears") 
                out$expected <- out$expected/scale
            if (!is.null(clist$summ)) 
                out$summary <- rtemp$summ
        }
        if (docount) 
            out$event <- array(clist$pcount, dim = odims, dimnames = outdname)
    }
    out$observations <- nrow(m)
    out$terms <- Terms
    na.action <- attr(m, "na.action")
    if (length(na.action)) 
        out$na.action <- na.action
    if (model) 
        out$model <- m
    else {
        if (x) 
            out$x <- X
        if (y) 
            out$y <- Y
    }
    class(out) <- "pyears"
    out
}


tcut <- function (x, breaks, labels, scale = 1) 
{
    x <- as.numeric(x)
    breaks <- as.numeric(breaks)
    if (length(breaks) == 1) {
        if (breaks < 1) 
            stop("Must specify at least one interval")
        if (missing(labels)) 
            labels <- paste("Range", seq(length = breaks))
        else if (length(labels) != breaks) 
            stop("Number of labels must equal number of intervals")
        r <- range(x[!is.na(x)])
        r[is.na(r)] <- 1
        if ((d <- diff(r)) == 0) {
            r[2] <- r[1] + 1
            d <- 1
        }
        breaks <- seq(r[1] - 0.01 * d, r[2] + 0.01 * d, length = breaks + 
            1)
    }
    else {
        if (is.na(adb <- all(diff(breaks) >= 0)) || !adb) 
            stop("breaks must be given in ascending order and contain no NA's")
        if (missing(labels)) 
            labels <- paste(format(breaks[-length(breaks)]), 
                "+ thru ", format(breaks[-1]), sep = "")
        else if (length(labels) != length(breaks) - 1) 
            stop("Number of labels must be 1 less than number of break points")
    }
    temp <- structure(x * scale, cutpoints = breaks * scale, 
        labels = labels)
    class(temp) <- "tcut"
    temp
}


survreg.fit <- function (x, y, weights, offset, init, controlvals, dist, scale = 0, 
    nstrat = 1, strata, parms = NULL, assign) 
{
    iter.max <- controlvals$iter.max
    eps <- controlvals$rel.tolerance
    toler.chol <- controlvals$toler.chol
    if (!is.matrix(x)) 
        stop("Invalid X matrix ")
    n <- nrow(x)
    nvar <- ncol(x)
    ny <- ncol(y)
    if (is.null(offset)) 
        offset <- rep(0, n)
    if (missing(weights) || is.null(weights)) 
        weights <- rep(1, n)
    else if (any(weights <= 0)) 
        stop("Invalid weights, must be >0")
    if (scale < 0) 
        stop("Invalid scale")
    if (scale > 0 && nstrat > 1) 
        stop("Cannot have both a fixed scale and strata")
    if (nstrat > 1 && (missing(strata) || length(strata) != n)) 
        stop("Invalid strata variable")
    if (nstrat == 1) 
        strata <- rep(1, n)
    if (scale > 0) 
        nstrat2 <- 0
    else nstrat2 <- nstrat
    if (is.character(dist)) {
        sd <- survreg.distributions[[dist]]
        if (is.null(sd)) 
            stop("Unrecognized distribution")
    }
    else sd <- dist
    if (!is.function(sd$density)) 
        stop("Missing density function in the definition of the distribution")
    dnum <- match(sd$name, c("Extreme value", "Logistic", "Gaussian"))
    if (is.na(dnum)) {
        dnum <- 4
        n2 <- n + sum(y[, ny] == 3)
        f.expr <- quote({
            if (length(parms)) temp <- sd$density(z, parms) else temp <- sd$density(z)
            if (!is.matrix(temp) || any(dim(temp) != c(n2, 5)) || 
                !is.numeric(temp)) stop("Density function returned an invalid matrix")
            as.vector(as.double(temp))
        })
        rho <- new.env()
    }
    else {
        f.expr <- 1
        rho <- 1
    }
    derfun <- function(y, eta, sigma, density, parms) {
        ny <- ncol(y)
        status <- y[, ny]
        z <- (y[, 1] - eta)/sigma
        dmat <- density(z, parms)
        dtemp <- dmat[, 3] * dmat[, 4]
        if (any(status == 3)) {
            z2 <- (y[, 2] - eta)/sigma
            dmat2 <- density(z2, parms)
        }
        else {
            dmat2 <- matrix(0, 1, 5)
            z2 <- 0
        }
        tdenom <- ((status == 0) * dmat[, 2]) + ((status == 1) * 
            1) + ((status == 2) * dmat[, 1]) + ((status == 3) * 
            ifelse(z > 0, dmat[, 2] - dmat2[, 2], dmat2[, 1] - 
                dmat[, 1]))
        tdenom <- 1/(tdenom * sigma)
        dg <- -tdenom * (((status == 0) * (0 - dmat[, 3])) + 
            ((status == 1) * dmat[, 4]) + ((status == 2) * dmat[, 
            3]) + ((status == 3) * (dmat2[, 3] - dmat[, 3])))
        ddg <- (tdenom/sigma) * (((status == 0) * (0 - dtemp)) + 
            ((status == 1) * dmat[, 5]) + ((status == 2) * dtemp) + 
            ((status == 3) * (dmat2[, 3] * dmat2[, 4] - dtemp)))
        list(dg = dg, ddg = ddg - dg^2)
    }
    rescaled <- FALSE
    if (is.null(init) && all(x[, 1] == 1) && ncol(x) > 1 && is.null(init)) {
        okay <- apply(x, 2, function(z) all(z == 0 | z == 1))
        if (!all(okay)) {
            rescaled <- TRUE
            center <- ifelse(okay, 0, colMeans(x))
            stdev <- ifelse(okay, 1, apply(x, 2, sd))
            x <- scale(x, center, stdev)
        }
    }
    nvar2 <- nvar + nstrat2
    meanonly <- (nvar == 1 && all(x == 1))
    if (!meanonly) {
        yy <- ifelse(y[, ny] != 3, y[, 1], (y[, 1] + y[, 2])/2)
        coef <- sd$init(yy, weights, parms)
        if (scale > 0) 
            vars <- log(scale)
        else vars <- log(4 * coef[2])/2
        coef <- c(coef[1], rep(vars, nstrat))
        deriv <- derfun(y, yy, exp(vars), sd$density, parms)
        wt <- -1 * deriv$ddg * weights
        coef[1] <- sum(weights * deriv$dg + wt * (yy - offset))/sum(wt)
        fit0 <- .Call(Csurvreg6, iter = as.integer(20), nvar = as.integer(1), 
            as.double(y), as.integer(ny), x = as.double(rep(1, 
                n)), as.double(weights), as.double(offset), coef = as.double(coef), 
            as.integer(nstrat2), as.integer(strata), as.double(eps), 
            as.double(toler.chol), as.integer(dnum), f.expr, 
            rho)
    }
    if (is.numeric(init)) {
        if (length(init) == nvar && (nvar2 > nvar)) {
            init <- c(init, fit0$coef[-1])
        }
        if (length(init) != nvar2) 
            stop("Wrong length for initial parameters")
        if (scale > 0) 
            init <- c(init, log(scale))
    }
    else {
        if (meanonly) {
            yy <- ifelse(y[, ny] != 3, y[, 1], (y[, 1] + y[, 
                2])/2)
            coef <- sd$init(yy, weights, parms)
            if (scale > 0) 
                vars <- rep(log(scale), nstrat)
            else vars <- rep(log(4 * coef[2])/2, nstrat)
        }
        else vars <- fit0$coef[-1]
        eta <- yy - offset
        deriv <- derfun(y, yy, exp(vars[strata]), sd$density, 
            parms)
        wt <- -1 * deriv$ddg * weights
        coef <- coxph.wtest(t(x) %*% (wt * x), c((wt * eta + 
            weights * deriv$dg) %*% x), toler.chol = toler.chol)$solve
        init <- c(coef, vars)
    }
    fit <- .Call(Csurvreg6, iter = as.integer(iter.max), as.integer(nvar), 
        as.double(y), as.integer(ny), as.double(x), as.double(weights), 
        as.double(offset), as.double(init), as.integer(nstrat2), 
        as.integer(strata), as.double(eps), as.double(toler.chol), 
        as.integer(dnum), f.expr, rho)
    if (iter.max > 1 && fit$flag > nvar2) {
        warning("Ran out of iterations and did not converge")
    }
    cname <- dimnames(x)[[2]]
    if (is.null(cname)) 
        cname <- paste("x", 1:ncol(x))
    if (scale == 0) 
        cname <- c(cname, rep("Log(scale)", nstrat))
    if (scale > 0) 
        fit$coef <- fit$coef[1:nvar2]
    names(fit$coef) <- cname
    if (meanonly) {
        coef0 <- fit$coef
        loglik <- rep(fit$loglik, 2)
    }
    else {
        coef0 <- fit0$coef
        names(coef0) <- c("Intercept", rep("Log(scale)", nstrat))
        loglik <- c(fit0$loglik, fit$loglik)
    }
    var.new <- matrix(fit$var, nvar2, dimnames = list(cname, 
        cname))
    coef.new <- fit$coef
    if (rescaled) {
        vtemp <- diag(c(1/stdev, rep(1, nrow(var.new) - length(stdev))))
        vtemp[1, 2:nvar] <- -center[2:nvar]/stdev[2:nvar]
        coef.new <- drop(vtemp %*% coef.new)
        var.new <- vtemp %*% var.new %*% t(vtemp)
        names(coef.new) <- names(fit$coef)
    }
    temp <- list(coefficients = coef.new, icoef = coef0, var = var.new, 
        loglik = loglik, iter = fit$iter, linear.predictors = c(x %*% 
            fit$coef[1:nvar] + offset), df = length(fit$coef), 
        score = fit$u)
    temp
}


survpenal.fit <- function (x, y, weights, offset, init, controlvals, dist, scale = 0, 
    nstrat = 1, strata, pcols, pattr, assign, parms = NULL) 
{
    iter.max <- controlvals$iter.max
    outer.max <- controlvals$outer.max
    eps <- controlvals$rel.tolerance
    toler.chol <- controlvals$toler.chol
    if (!is.matrix(x)) 
        stop("Invalid X matrix ")
    n <- nrow(x)
    nvar <- ncol(x)
    ny <- ncol(y)
    if (is.null(offset)) 
        offset <- rep(0, n)
    if (missing(weights) || is.null(weights)) 
        weights <- rep(1, n)
    else if (any(weights <= 0)) 
        stop("Invalid weights, must be >0")
    if (scale < 0) 
        stop("Invalid scale")
    if (scale > 0 && nstrat > 1) 
        stop("Cannot have both a fixed scale and strata")
    if (nstrat > 1 && (missing(strata) || length(strata) != n)) 
        stop("Invalid strata variable")
    if (nstrat == 1) 
        strata <- rep(1, n)
    if (scale > 0) 
        nstrat2 <- 0
    else nstrat2 <- nstrat
    if (is.character(dist)) {
        sd <- survreg.distributions[[dist]]
        if (is.null(sd)) 
            stop("Unrecognized distribution")
    }
    else sd <- dist
    dnum <- match(sd$name, c("Extreme value", "Logistic", "Gaussian"))
    if (is.na(dnum)) {
        dnum <- 4
        n2 <- n + sum(y[, ny] == 3)
        fdensity <- quote({
            if (length(parms)) temp <- sd$density(z, parms) else temp <- sd$density(z)
            if (!is.matrix(temp) || any(dim(temp) != c(n2, 5)) || 
                !is.numeric(temp)) stop("Density function returned an invalid matrix")
            as.vector(as.double(temp))
        })
    }
    else {
        fdensity <- 1
        n2 <- n
    }
    derfun <- function(y, eta, sigma, density, parms) {
        ny <- ncol(y)
        status <- y[, ny]
        z <- (y[, 1] - eta)/sigma
        dmat <- density(z, parms)
        dtemp <- dmat[, 3] * dmat[, 4]
        if (any(status == 3)) {
            z2 <- (y[, 2] - eta)/sigma
            dmat2 <- density(z2)
        }
        else {
            dmat2 <- matrix(0, 1, 5)
            z2 <- 0
        }
        tdenom <- ((status == 0) * dmat[, 2]) + ((status == 1) * 
            1) + ((status == 2) * dmat[, 1]) + ((status == 3) * 
            ifelse(z > 0, dmat[, 2] - dmat2[, 2], dmat2[, 1] - 
                dmat[, 1]))
        tdenom <- 1/(tdenom * sigma)
        dg <- -tdenom * (((status == 0) * (0 - dmat[, 3])) + 
            ((status == 1) * dmat[, 4]) + ((status == 2) * dmat[, 
            3]) + ((status == 3) * (dmat2[, 3] - dmat[, 3])))
        ddg <- (tdenom/sigma) * (((status == 0) * (0 - dtemp)) + 
            ((status == 1) * dmat[, 5]) + ((status == 2) * dtemp) + 
            ((status == 3) * (dmat2[, 3] * dmat2[, 4] - dtemp)))
        list(dg = dg, ddg = ddg - dg^2)
    }
    status <- y[, ny]
    npenal <- length(pattr)
    if (npenal == 0 || length(pcols) != npenal) 
        stop("Invalid pcols or pattr arg")
    sparse <- sapply(pattr, function(x) !is.null(x$sparse) && 
        x$sparse)
    if (sum(sparse) > 1) 
        stop("Only one sparse penalty term allowed")
    pterms <- rep(0, length(assign))
    names(pterms) <- names(assign)
    pindex <- rep(0, npenal)
    for (i in 1:npenal) {
        temp <- unlist(lapply(assign, function(x, y) (length(x) == 
            length(y) && all(x == y)), pcols[[i]]))
        if (sparse[i]) 
            pterms[temp] <- 2
        else pterms[temp] <- 1
        pindex[i] <- (seq(along.with = temp))[temp]
    }
    if ((sum(pterms == 2) != sum(sparse)) || (sum(pterms > 0) != 
        npenal)) 
        stop("pcols and assign arguments disagree")
    if (any(pindex != sort(pindex))) {
        temp <- order(pindex)
        pindex <- pindex[temp]
        pcols <- pcols[temp]
        pattr <- pattr[temp]
    }
    ptype <- any(sparse) + 2 * (any(!sparse))
    if (any(sparse)) {
        sparse.attr <- (pattr[sparse])[[1]]
        fcol <- unlist(pcols[sparse])
        if (length(fcol) > 1) 
            stop("Sparse term must be single column")
        frailx <- x[, fcol]
        x <- x[, -fcol, drop = FALSE]
        for (i in 1:length(assign)) {
            j <- assign[[i]]
            if (j[1] > fcol) 
                assign[[i]] <- j - 1
        }
        for (i in 1:npenal) {
            j <- pcols[[i]]
            if (j[1] > fcol) 
                pcol[[i]] <- j - 1
        }
        frailx <- match(frailx, sort(unique(frailx)))
        nfrail <- max(frailx)
        nvar <- nvar - 1
        pfun1 <- sparse.attr$pfun
        coxlist1 <- list(coef = 0, first = 0, second = 0, penalty = 0, 
            flag = F)
        f.expr1 <- quote({
            if (is.null(extra1)) temp <- pfun1(coef1, theta1, 
                n.eff) else temp <- pfun1(coef1, theta1, n.eff, 
                extra1)
            if (!is.null(temp$recenter)) coxlist1$coef <- coef1 - 
                as.double(temp$recenter) else coxlist1$coef <- coef1
            if (!temp$flag) {
                coxlist1$first <- -as.double(temp$first)
                coxlist1$second <- as.double(temp$second)
            } else {
                coxlist1$first <- double(nfrail)
                coxlist1$second <- double(nfrail)
            }
            coxlist1$penalty <- -as.double(temp$penalty)
            coxlist1$flag <- as.logical(temp$flag)
            if (any(names(coxlist1) != c("coef", "first", "second", 
                "penalty", "flag"))) stop("Invalid coxlist1")
            if (any(sapply(coxlist1, length) != c(rep(nfrail, 
                3), 1, 1))) stop("Incorrect length in coxlist1")
            coxlist1
        })
    }
    else {
        frailx <- 0
        nfrail <- 0
        f.expr1 <- NULL
        pfun1 <- NULL
        coxlist1 <- NULL
    }
    nvar2 <- nvar + nstrat2
    if (nvar2 == 0) {
        stop("Cannot fit a model with no coefficients other than sparse ones")
    }
    if (sum(!sparse) > 0) {
        full.imat <- !all(unlist(lapply(pattr, function(x) x$diag)))
        ipenal <- (1:length(pattr))[!sparse]
        if (full.imat) {
            coxlist2 <- list(coef = double(nvar), first = double(nvar), 
                second = double(nvar^2), penalty = 0, flag = rep(FALSE, 
                  nvar))
            length2 <- c(nvar, nvar, nvar * nvar, 1, nvar)
        }
        else {
            coxlist2 <- list(coef = double(nvar), first = double(nvar), 
                second = double(nvar), penalty = 0, flag = rep(FALSE, 
                  nvar))
            length2 <- c(nvar, nvar, nvar, 1, nvar)
        }
        f.expr2 <- quote({
            pentot <- 0
            newcoef <- coef2
            for (i in ipenal) {
                pen.col <- pcols[[i]]
                tcoef <- coef2[pen.col]
                if (is.null(extralist[[i]])) temp <- ((pattr[[i]])$pfun)(tcoef, 
                  thetalist[[i]], n.eff) else temp <- ((pattr[[i]])$pfun)(tcoef, 
                  thetalist[[i]], n.eff, extralist[[i]])
                if (!is.null(temp$recenter)) newcoef[pen.col] <- tcoef - 
                  temp$recenter
                if (temp$flag) coxlist2$flag[pen.col] <- TRUE else {
                  coxlist2$flag[pen.col] <- FALSE
                  coxlist2$first[pen.col] <- -temp$first
                  if (full.imat) {
                    tmat <- matrix(coxlist2$second, nvar, nvar)
                    tmat[pen.col, pen.col] <- temp$second
                    coxlist2$second <- c(tmat)
                  } else coxlist2$second[pen.col] <- temp$second
                }
                pentot <- pentot - temp$penalty
            }
            coxlist2$penalty <- as.double(pentot)
            coxlist2$coef <- newcoef
            if (any(sapply(coxlist2, length) != length2)) stop("Length error in coxlist2")
            coxlist2
        })
    }
    else {
        full.imat <- FALSE
        length2 <- 0
        f.expr2 <- NULL
        coxlist2 <- NULL
        ipenal <- NULL
    }
    rho <- new.env()
    cfun <- lapply(pattr, function(x) x$cfun)
    parmlist <- lapply(pattr, function(x, eps) c(x$cparm, eps2 = eps), 
        sqrt(eps))
    extralist <- lapply(pattr, function(x) x$pparm)
    iterlist <- vector("list", length(cfun))
    thetalist <- vector("list", length(cfun))
    printfun <- lapply(pattr, function(x) x$printfun)
    extra1 <- NULL
    theta1 <- NULL
    for (i in 1:length(cfun)) {
        temp <- (cfun[[i]])(parmlist[[i]], iter = 0)
        if (sparse[i]) {
            assign("theta1", temp$theta, rho)
            assign("extra1", extralist[[i]], rho)
        }
        thetalist[[i]] <- temp$theta
        iterlist[[i]] <- temp
    }
    temp1 <- c("x", "coef", "plik", "loglik", "status", "neff", 
        "df", "trH")
    temp2 <- c("frailx", "fcoef", "fit$loglik-fit$penalty", "fit$loglik", 
        "status", "n.eff")
    temp3 <- c("x[,pen.col]", "coef[pen.col]", "fit$loglik-fit$penalty", 
        "fit$loglik", "status", "n.eff")
    calls <- vector("expression", length(cfun))
    cargs <- lapply(pattr, function(x) x$cargs)
    for (i in 1:length(cfun)) {
        tempchar <- paste("(cfun[[", i, "]])(parmlist[[", i, 
            "]], iter,", "iterlist[[", i, "]]")
        temp2b <- c(temp2, paste("pdf[", i, "]"), paste("trH[", 
            i, "]"))
        temp3b <- c(temp3, paste("pdf[", i, "]"), paste("trH[", 
            i, "]"))
        if (length(cargs[[i]]) == 0) 
            calls[i] <- parse(text = paste(tempchar, ")"))
        else {
            temp <- match(cargs[[i]], temp1)
            if (any(is.na(temp))) 
                stop(paste((cargs[[i]])[is.na(temp)], "not matched"))
            if (sparse[i]) 
                temp4 <- paste(temp2b[temp], collapse = ",")
            else temp4 <- paste(temp3b[temp], collapse = ",")
            calls[i] <- parse(text = paste(paste(tempchar, temp4, 
                sep = ","), ")"))
        }
    }
    need.df <- any(!is.na(match(c("df", "trH"), unlist(cargs))))
    varnames <- dimnames(x)[[2]]
    for (i in 1:npenal) {
        if (!is.null(pattr[[i]]$varname)) 
            varnames[pcols[[i]]] <- pattr[[i]]$varname
    }
    nvar2 <- nvar + nstrat2
    nvar3 <- nvar2 + nfrail
    yy <- ifelse(status != 3, y[, 1], (y[, 1] + y[, 2])/2)
    coef <- sd$init(yy, weights, parms)
    if (scale > 0) 
        vars <- log(scale)
    else vars <- log(4 * coef[2])/2
    coef <- c(coef[1], rep(vars, nstrat))
    deriv <- derfun(y, yy, exp(vars), sd$density, parms)
    wt <- -1 * deriv$ddg * weights
    coef[1] <- sum(weights * deriv$dg + wt * (yy - offset))/sum(wt)
    fit0 <- .Call(Csurvreg6, iter = as.integer(20), nvar = as.integer(1), 
        as.double(y), as.integer(ny), x = as.double(rep(1, n)), 
        as.double(weights), as.double(offset), coef = as.double(coef), 
        as.integer(nstrat2), as.integer(strata), as.double(eps), 
        as.double(toler.chol), as.integer(dnum), fdensity, rho)
    temp <- mean(exp(fit0$coef[-1]))
    n.eff <- sd$variance(temp^2) * (solve(matrix(fit0$var, 1 + 
        nstrat2)))[1, 1]
    if (is.numeric(init)) {
        if (length(init) == nvar) {
            if (scale > 0) 
                init <- c(init, log(scale))
            else init <- c(rep(0, nfrail), init, fit0$coef[-1])
        }
        else if (length(init) == nvar2) 
            init <- c(rep(0, nfrail), init)
        else if (length(init) != nvar3) 
            stop("Wrong length for inital values")
        if (scale > 0) 
            init <- c(init, log(scale))
    }
    else {
        init <- c(rep(0, nfrail), fit0$coef[1], rep(0, nvar - 
            1), fit0$coef[-1])
    }
    if (nstrat2 > 0) 
        assign <- c(assign, list(sigma = (1 + nvar):nvar2))
    iter2 <- 0
    iterfail <- NULL
    thetasave <- unlist(thetalist)
    for (iterx in 1:outer.max) {
        fit <- .Call(Csurvreg7, iter = as.integer(iter.max), 
            as.integer(nvar), as.double(y), as.integer(ny), as.double(x), 
            as.double(weights), as.double(offset), coef = as.double(init), 
            as.integer(nstrat2), as.integer(strata), as.double(eps), 
            as.double(toler.chol), as.integer(dnum), fdensity, 
            rho, as.integer(ptype), as.integer(full.imat), as.integer(nfrail), 
            as.integer(frailx), f.expr1, f.expr2)
        iter <- iterx
        iter2 <- iter2 + fit$iter
        if (fit$flag == 1000) 
            iterfail <- c(iterfail, iter)
        if (nfrail > 0) {
            fcoef <- fit$coef[1:nfrail]
            coef <- fit$coef[nfrail + 1:nvar2]
        }
        else coef <- fit$coef[1:nvar2]
        if (nfrail > 0) 
            coxlist1 <- get("coxlist1", envir = rho)
        if (ptype > 1) 
            coxlist2 <- get("coxlist2", envir = rho)
        temp <- rep(FALSE, nvar2 + nfrail)
        if (nfrail > 0) 
            temp[1:nfrail] <- coxlist1$flag
        if (ptype > 1) 
            temp[nfrail + 1:nvar] <- coxlist2$flag
        hdiag <- ifelse(temp, 0, fit$hdiag)
        if (need.df) {
            if (nfrail > 0) 
                temp1 <- coxlist1$second
            else temp1 <- 0
            if (ptype > 1) {
                if (full.imat) {
                  temp2 <- matrix(0, nvar2, nvar2)
                  temp2[1:nvar, 1:nvar] <- coxlist2$second
                }
                else temp2 <- diag(c(coxlist2$second, rep(0, 
                  nstrat2)))
            }
            else temp2 <- 0
            dftemp <- coxpenal.df(matrix(fit$hmat, ncol = nvar2), 
                matrix(fit$hinv, ncol = nvar2), hdiag, assign, 
                ptype, nvar2, temp1, temp2, pindex[sparse])
            df <- dftemp$df
            var <- dftemp$var
            var2 <- dftemp$var2
            pdf <- df[pterms > 0]
            trH <- dftemp$trH[pterms > 0]
        }
        done <- TRUE
        for (i in 1:length(cfun)) {
            pen.col <- pcols[[i]]
            temp <- eval(calls[i])
            if (sparse[i]) 
                assign("theta1", temp$theta, rho)
            thetalist[[i]] <- temp$theta
            iterlist[[i]] <- temp
            done <- done & temp$done
        }
        if (done) 
            break
        if (iter == 1) {
            init <- coefsave <- fit$coef
            thetasave <- cbind(thetasave, unlist(thetalist))
        }
        else {
            temp <- unlist(thetalist)
            coefsave <- cbind(coefsave, fit$coef)
            howclose <- apply((thetasave - temp)^2, 2, sum)
            which <- min((1:iter)[howclose == min(howclose)])
            init <- coefsave[, which]
            thetasave <- cbind(thetasave, temp)
        }
    }
    if (!need.df) {
        if (nfrail > 0) 
            temp1 <- coxlist1$second
        else temp1 <- 0
        if (ptype > 1) {
            if (full.imat) {
                temp2 <- matrix(0, nvar2, nvar2)
                temp2[1:nvar, 1:nvar] <- coxlist2$second
            }
            else temp2 <- diag(c(coxlist2$second, rep(0, nstrat2)))
        }
        else temp2 <- 0
        dftemp <- coxpenal.df(matrix(fit$hmat, ncol = nvar2), 
            matrix(fit$hinv, ncol = nvar2), hdiag, assign, ptype, 
            nvar2, temp1, temp2, pindex[sparse])
        df <- dftemp$df
        trH <- dftemp$trH
        var <- dftemp$var
        var2 <- dftemp$var2
    }
    if (iter.max > 1 && length(iterfail) > 0) 
        warning(paste("Inner loop failed to coverge for iterations", 
            paste(iterfail, collapse = " ")))
    which.sing <- (hdiag[nfrail + 1:nvar] == 0)
    coef[which.sing] <- NA
    names(iterlist) <- names(pterms[pterms > 0])
    cname <- varnames
    cname <- c(cname, rep("Log(scale)", nstrat2))
    dimnames(var) <- list(cname, cname)
    names(coef) <- cname
    if (nfrail > 0) {
        lp <- offset + fcoef[frailx]
        lp <- lp + x %*% coef[1:nvar]
        list(coefficients = coef, icoef = fit0$coef, var = var, 
            var2 = var2, loglik = c(fit0$loglik, fit$loglik - 
                fit$penalty), iter = c(iter, iter2), linear.predictors = as.vector(lp), 
            frail = fcoef, fvar = dftemp$fvar, df = df, penalty = c(fit0$penalty, 
                -fit$penalty), pterms = pterms, assign2 = assign, 
            history = iterlist, printfun = printfun, score = fit$u)
    }
    else {
        list(coefficients = coef, icoef = fit0$coef, var = var, 
            var2 = var2, loglik = c(fit0$loglik, fit$loglik - 
                fit$penalty), iter = c(iter, iter2), linear.predictors = as.vector(x %*% 
                coef[1:nvar]), df = df, df2 = dftemp$df2, penalty = c(0, 
                -fit$penalty), pterms = pterms, assign2 = assign, 
            history = iterlist, printfun = printfun, score = fit$u)
    }
}


agreg.fit <- function (x, y, strata, offset, init, control, weights, method, 
    rownames, resid = TRUE) 
{
    n <- nrow(y)
    nvar <- ncol(x)
    event <- y[, 3]
    if (all(event == 0)) 
        stop("Can't fit a Cox model with 0 failures")
    if (length(strata) == 0) {
        sort.end <- order(-y[, 2]) - 1L
        sort.start <- order(-y[, 1]) - 1L
        newstrat <- n
    }
    else {
        sort.end <- order(strata, -y[, 2]) - 1L
        sort.start <- order(strata, -y[, 1]) - 1L
        newstrat <- cumsum(table(strata))
    }
    if (missing(offset) || is.null(offset)) 
        offset <- rep(0, n)
    if (missing(weights) || is.null(weights)) 
        weights <- rep(1, n)
    else if (any(weights <= 0)) 
        stop("Invalid weights, must be >0")
    else weights <- as.vector(weights)
    if (is.null(nvar) || nvar == 0) {
        nvar <- 1
        x <- matrix(as.double(1:n), ncol = 1)
        maxiter <- 0
        nullmodel <- TRUE
        if (length(init) != 0) 
            stop("Wrong length for inital values")
        init <- 0
    }
    else {
        nullmodel <- FALSE
        maxiter <- control$iter.max
        if (is.null(init)) 
            init <- rep(0, nvar)
        if (length(init) != nvar) 
            stop("Wrong length for inital values")
    }
    storage.mode(y) <- storage.mode(x) <- "double"
    storage.mode(offset) <- storage.mode(weights) <- "double"
    storage.mode(newstrat) <- "integer"
    agfit <- .Call(Cagfit4, y, x, newstrat, weights, offset, 
        as.double(init), sort.start, sort.end, as.integer(method == 
            "efron"), as.integer(maxiter), as.double(control$eps), 
        as.double(control$toler.chol), as.integer(1))
    var <- matrix(agfit$imat, nvar, nvar)
    coef <- agfit$coef
    if (agfit$flag[1] < nvar) 
        which.sing <- diag(var) == 0
    else which.sing <- rep(FALSE, nvar)
    if (maxiter > 1) {
        infs <- abs(agfit$u %*% var)
        if (any(!is.finite(coef)) || any(!is.finite(var))) 
            stop("routine failed due to numeric overflow.", "This should never happen.  Please contact the author.")
        if (agfit$iter > maxiter) 
            warning("Ran out of iterations and did not converge")
        infs <- (!is.finite(agfit$u) | infs > control$toler.inf * 
            (1 + abs(coef)))
        if (any(infs)) 
            warning(paste("Loglik converged before variable ", 
                paste((1:nvar)[infs], collapse = ","), "; beta may be infinite. "))
    }
    lp <- as.vector(x %*% coef + offset - sum(coef * colMeans(x)))
    if (resid) {
        score <- as.double(exp(lp))
        residuals <- .Call(Cagmart3, y, score, weights, newstrat, 
            cbind(sort.end, sort.start), as.integer(method == 
                "efron"))
        names(residuals) <- rownames
    }
    if (nullmodel) {
        rval <- list(loglik = agfit$loglik[2], linear.predictors = offset, 
            method = method, class = c("coxph.null", "coxph"))
        if (resid) 
            rval$residuals <- residuals
    }
    else {
        names(coef) <- dimnames(x)[[2]]
        if (maxiter > 0) 
            coef[which.sing] <- NA
        flag <- agfit$flag
        names(flag) <- c("rank", "rescale", "step halving")
        if (resid) {
            rval <- list(coefficients = coef, var = var, loglik = agfit$loglik, 
                score = agfit$sctest, iter = agfit$iter, linear.predictors = as.vector(lp), 
                residuals = residuals, means = colMeans(x), first = agfit$u, 
                info = flag, method = method, class = "coxph")
        }
        else {
            rval <- list(coefficients = coef, var = var, loglik = agfit$loglik, 
                score = agfit$sctest, iter = agfit$iter, linear.predictors = as.vector(lp), 
                means = colMeans(x), first = agfit$u, info = flag, 
                method = method, class = "coxph")
        }
        rval
    }
    rval
}


concordancefit <- function (y, x, strata, weights, ymin = NULL, ymax = NULL, timewt = c("n", 
    "S", "S/G", "n/G", "n/G2", "I"), cluster, influence = 0, 
    ranks = FALSE, reverse = FALSE, timefix = TRUE) 
{
    if (any(is.na(x)) || any(is.na(y))) 
        return(NULL)
    timewt <- match.arg(timewt)
    if (!is.Surv(y)) {
        if (is.factor(Y) && (is.ordered(Y) || length(levels(Y)) == 
            2)) 
            Y <- Surv(as.numeric(Y))
        else if (is.numeric(Y) && is.vector(Y)) 
            Y <- Surv(Y)
        else stop("left hand side of the formula must be a numeric vector,\n survival object, or an orderable factor")
        if (timefix) 
            Y <- aeqSurv(Y)
    }
    n <- length(y)
    if (length(x) != n) 
        stop("x and y are not the same length")
    if (missing(strata) || length(strata) == 0) 
        strata <- rep(1L, n)
    if (length(strata) != n) 
        stop("y and strata are not the same length")
    if (missing(weights) || length(weights) == 0) 
        weights <- rep(1, n)
    else if (length(weights) != n) 
        stop("y and weights are not the same length")
    type <- attr(y, "type")
    if (type %in% c("left", "interval")) 
        stop("left or interval censored data is not supported")
    if (type %in% c("mright", "mcounting")) 
        stop("multiple state survival is not supported")
    docount <- function(y, risk, wts, timeopt = "n") {
        n <- length(risk)
        if (sum(y[, ncol(y)]) < 2) 
            timeopt <- "n"
        sfit <- survfit(y ~ 1, weights = wts, se.fit = FALSE)
        etime <- sfit$time[sfit$n.event > 0]
        esurv <- sfit$surv[sfit$n.event > 0]
        if (length(etime) == 0) {
            return(list(count = rep(0, 6), influence = matrix(0, 
                n, 5), resid = NULL))
        }
        if (timeopt %in% c("S/G", "n/G", "n/G2")) {
            temp <- y
            temp[, ncol(temp)] <- 1 - temp[, ncol(temp)]
            gfit <- survfit(temp ~ 1, weights = wts, se.fit = FALSE)
            gsurv <- c(1, gfit$surv)
            gsurv <- gsurv[which(sfit$n.event > 0)]
        }
        npair <- (sfit$n.risk - sfit$n.event)[sfit$n.event > 
            0]
        temp <- ifelse(esurv == 0, 0, esurv/npair)
        timewt <- switch(timeopt, S = sum(wts) * temp, `S/G` = sum(wts) * 
            temp/gsurv, n = rep(1, length(npair)), `n/G` = 1/gsurv, 
            `n/G2` = 1/gsurv^2, I = rep(1, length(esurv)))
        if (!is.null(ymin)) 
            timewt[etime < ymin] <- 0
        if (!is.null(ymax)) 
            timewt[etime > ymax] <- 0
        timewt <- ifelse(is.finite(timewt), timewt, 0)
        if (ncol(y) == 2) {
            sort.stop <- order(-y[, 1], y[, 2], risk) - 1L
        }
        else {
            sort.stop <- order(-y[, 2], y[, 3], risk) - 1L
            sort.start <- order(-y[, 1]) - 1L
        }
        utemp <- match(risk, sort(unique(risk)))
        bindex <- btree(max(utemp))[utemp]
        storage.mode(y) <- "double"
        storage.mode(wts) <- "double"
        if (ncol(y) == 2) 
            fit <- .Call(Cconcordance3, y, bindex, wts, rev(timewt), 
                sort.stop, ranks)
        else fit <- .Call(Cconcordance4, y, bindex, wts, rev(timewt), 
            sort.start, sort.stop, ranks)
        dimnames(fit$influence) <- list(NULL, c("concordant", 
            "discordant", "tied.x", "tied.y", "tied.xy"))
        if (ranks) {
            if (ncol(y) == 2) 
                dtime <- y[y[, 2] == 1, 1]
            else dtime <- y[y[, 3] == 1, 2]
            temp <- data.frame(time = sort(dtime), fit$resid)
            names(temp) <- c("time", "rank", "timewt", "casewt", 
                "variance")
            fit$resid <- temp[temp[, 3] > 0, ]
        }
        fit
    }
    if (missing(strata) || length(strata) == 0 || all(strata == 
        strata[1])) {
        is.strata <- FALSE
        fit <- docount(y, x, weights, timewt)
        count2 <- fit$count[1:5]
        vcox <- fit$count[6]
        fit$count <- fit$count[1:5]
        imat <- fit$influence
        if (ranks) 
            resid <- fit$resid
    }
    else {
        is.strata <- TRUE
        strata <- as.factor(strata)
        ustrat <- levels(strata)[table(strata) > 0]
        tfit <- lapply(ustrat, function(i) {
            keep <- which(strata == i)
            docount(y[keep, , drop = F], x[keep], weights[keep], 
                timewt)
        })
        temp <- t(sapply(tfit, function(x) x$count))
        fit <- list(count = temp[, 1:5])
        count2 <- colSums(fit$count)
        vcox <- sum(temp[, 6])
        imat <- do.call("rbind", lapply(tfit, function(x) x$influence))
        index <- match(1:n, (1:n)[order(strata)])
        imat <- imat[index, ]
        if (ranks) {
            nr <- lapply(tfit, function(x) nrow(x$resid))
            resid <- do.call("rbind", lapply(tfit, function(x) x$resid))
            resid$strata <- rep(ustrat, nr)
        }
    }
    npair <- sum(count2[1:3])
    somer <- (count2[1] - count2[2])/npair
    dfbeta <- weights * ((imat[, 1] - imat[, 2])/npair - (somer/npair) * 
        rowSums(imat[, 1:3]))
    if (!missing(cluster) && length(cluster) > 0) {
        dfbeta <- tapply(dfbeta, cluster, sum)
        dfbeta <- ifelse(is.na(dfbeta), 0, dfbeta)
    }
    var.somer <- sum(dfbeta^2)
    rval <- list(concordance = (somer + 1)/2, count = fit$count, 
        n = n, var = var.somer/4, cvar = vcox/(4 * npair^2))
    if (is.matrix(rval$count)) 
        colnames(rval$count) <- c("concordant", "discordant", 
            "tied.x", "tied.y", "tied.xy")
    else names(rval$count) <- c("concordant", "discordant", "tied.x", 
        "tied.y", "tied.xy")
    if (influence == 1 || influence == 3) 
        rval$dfbeta <- dfbeta/2
    if (influence >= 2) 
        rval$influence <- imat
    if (ranks) 
        rval$ranks <- resid
    if (reverse) {
        rval$concordance <- 1 - rval$concordance
        if (!is.null(rval$dfbeta)) 
            rval$dfbeta <- -rval$dfbeta
        if (!is.null(rval$influence)) 
            rval$influence <- rval$influence[, c(2, 1, 3, 4, 
                5)]
        if (is.matrix(rval$count)) 
            rval$count <- rval$count[, c(2, 1, 3, 4, 5)]
        else rval$count <- rval$count[c(2, 1, 3, 4, 5)]
        if (ranks) 
            rval$ranks$rank <- -rval$ranks$rank
    }
    rval
}


survfit <- function (formula, ...) 
{
    UseMethod("survfit", formula)
}


coxph.wtest <- function (var, b, toler.chol = 1e-09) 
{
    if (is.matrix(b)) {
        nvar <- nrow(b)
        ntest <- ncol(b)
    }
    else {
        nvar <- length(b)
        ntest <- 1
    }
    if (length(var) == 0) {
        if (nvar == 0) 
            return(list(test = numeric(0), df = 0, solve = 0))
        else stop("Argument lengths do not match")
    }
    if (length(var) == 1) {
        if (nvar == 1) 
            return(list(test = b * b/var, df = 1, solve = b/var))
        else stop("Argument lengths do not match")
    }
    if (!is.matrix(var) || (nrow(var) != ncol(var))) 
        stop("First argument must be a square matrix")
    if (nrow(var) != nvar) 
        stop("Argument lengths do not match")
    temp <- .C(Ccoxph_wtest, df = as.integer(nvar), as.integer(ntest), 
        as.double(var), tests = as.double(b), solve = double(nvar * 
            ntest), as.double(toler.chol))
    if (ntest == 1) 
        list(test = temp$tests[1], df = temp$df, solve = temp$solve)
    else list(test = temp$tests[1:ntest], df = temp$df, solve = matrix(temp$solve, 
        nvar, ntest))
}


agexact.fit <- function (x, y, strata, offset, init, control, weights, method, 
    rownames, resid = TRUE) 
{
    if (!is.matrix(x)) 
        stop("Invalid formula for cox fitting function")
    if (!is.null(weights) && any(weights != 1)) 
        stop("Case weights are not supported for the exact method")
    n <- nrow(x)
    nvar <- ncol(x)
    if (ncol(y) == 3) {
        start <- y[, 1]
        stopp <- y[, 2]
        event <- y[, 3]
    }
    else {
        start <- rep(0, n)
        stopp <- y[, 1]
        event <- y[, 2]
    }
    if (length(strata) == 0) {
        sorted <- order(stopp, -event)
        newstrat <- as.integer(rep(0, n))
    }
    else {
        sorted <- order(strata, stopp, -event)
        strata <- (as.numeric(strata))[sorted]
        newstrat <- as.integer(c(1 * (diff(strata) != 0), 1))
    }
    if (is.null(offset)) 
        offset <- rep(0, n)
    sstart <- as.double(start[sorted])
    sstop <- as.double(stopp[sorted])
    sstat <- as.integer(event[sorted])
    if (is.null(nvar)) {
        stop("Cannot handle a null model + exact calculation (yet)")
    }
    if (!is.null(init)) {
        if (length(init) != nvar) 
            stop("Wrong length for inital values")
    }
    else init <- rep(0, nvar)
    agfit <- .C(Cagexact, iter = as.integer(control$iter.max), 
        as.integer(n), as.integer(nvar), sstart, sstop, sstat, 
        x = x[sorted, ], as.double(offset[sorted]), newstrat, 
        means = double(nvar), coef = as.double(init), u = double(nvar), 
        imat = double(nvar * nvar), loglik = double(2), flag = integer(1), 
        double(2 * nvar * nvar + nvar * 4 + n), integer(2 * n), 
        as.double(control$eps), as.double(control$toler.chol), 
        sctest = double(1))
    var <- matrix(agfit$imat, nvar, nvar)
    coef <- agfit$coef
    if (agfit$flag < nvar) 
        which.sing <- diag(var) == 0
    else which.sing <- rep(FALSE, nvar)
    infs <- abs(agfit$u %*% var)
    if (control$iter.max > 1) {
        if (agfit$flag == 1000) 
            warning("Ran out of iterations and did not converge")
        else {
            infs <- ((infs > control$eps) & infs > control$toler.inf * 
                abs(coef))
            if (any(infs)) 
                warning(paste("Loglik converged before variable ", 
                  paste((1:nvar)[infs], collapse = ","), "; beta may be infinite. "))
        }
    }
    names(coef) <- dimnames(x)[[2]]
    lp <- x %*% coef + offset - sum(coef * agfit$means)
    score <- as.double(exp(lp[sorted]))
    coef[which.sing] <- NA
    if (resid) {
        agres <- .C(Cagmart, as.integer(n), as.integer(0), sstart, 
            sstop, sstat, score, rep(1, n), newstrat, resid = double(n))
        resid <- double(n)
        resid[sorted] <- agres$resid
        names(resid) <- rownames
        rval <- list(coefficients = coef, var = var, loglik = agfit$loglik, 
            score = agfit$sctest, iter = agfit$iter, linear.predictors = lp, 
            residuals = resid, means = agfit$means, method = "coxph")
    }
    else {
        rval <- list(coefficients = coef, var = var, loglik = agfit$loglik, 
            score = agfit$sctest, iter = agfit$iter, linear.predictors = lp, 
            means = agfit$means, method = "coxph")
    }
    rval
}


tmerge <- function (data1, data2, id, ..., tstart, tstop, options) 
{
    Call <- match.call()
    new <- new.env(parent = parent.frame())
    assign("tdc", function(time, value = NULL) {
        x <- list(time = time, value = value)
        class(x) <- "tdc"
        x
    }, envir = new)
    assign("cumtdc", function(time, value = NULL) {
        x <- list(time = time, value = value)
        class(x) <- "cumtdc"
        x
    }, envir = new)
    assign("event", function(time, value = NULL, censor = NULL) {
        x <- list(time = time, value = value, censor = censor)
        class(x) <- "event"
        x
    }, envir = new)
    assign("cumevent", function(time, value = NULL, censor = NULL) {
        x <- list(time = time, value = value, censor = censor)
        class(x) <- "cumevent"
        x
    }, envir = new)
    if (missing(data1) || missing(data2) || missing(id)) 
        stop("the data1, data2, and id arguments are required")
    if (!inherits(data1, "data.frame")) 
        stop("data1 must be a data frame")
    tmerge.control <- function(idname = "id", tstartname = "tstart", 
        tstopname = "tstop", delay = 0, na.rm = TRUE, tdcstart = NA, 
        ...) {
        extras <- list(...)
        if (length(extras) > 0) 
            stop("unrecognized option(s):", paste(names(extras), 
                collapse = ", "))
        if (length(idname) != 1 || make.names(idname) != idname) 
            stop("idname option must be a valid variable name")
        if (!is.null(tstartname) && (length(tstartname) != 1 || 
            make.names(tstartname) != tstartname)) 
            stop("tstart option must be NULL or a valid variable name")
        if (length(tstopname) != 1 || make.names(tstopname) != 
            tstopname) 
            stop("tstop option must be a valid variable name")
        if (length(delay) != 1 || !is.numeric(delay) || delay < 
            0) 
            stop("delay option must be a number >= 0")
        if (length(na.rm) != 1 || !is.logical(na.rm)) 
            stop("na.rm option must be TRUE or FALSE")
        if (length(tdcstart) != 1) 
            stop("tdcstart must be a single value")
        list(idname = idname, tstartname = tstartname, tstopname = tstopname, 
            delay = delay, na.rm = na.rm, tdcstart = tdcstart)
    }
    tname <- attr(data1, "tname")
    firstcall <- is.null(tname)
    if (!firstcall && any(is.null(match(unlist(tname), names(data1))))) 
        stop("data1 does not match its own tname attribute")
    if (!missing(options)) {
        if (!is.list(options)) 
            stop("options must be a list")
        if (!is.null(tname)) {
            temp <- match(names(options), names(tname), nomatch = 0)
            topt <- do.call(tmerge.control, c(options, tname[temp == 
                0]))
            if (any(temp > 0)) {
                varname <- tname[c("idname", "tstartname", "tstopname")]
                temp2 <- match(varname, names(data1))
                names(data1)[temp2] <- varname
            }
        }
        else topt <- do.call(tmerge.control, options)
    }
    else if (length(tname)) 
        topt <- do.call(tmerge.control, tname)
    else topt <- tmerge.control()
    if (missing(id)) 
        stop("the id argument is required")
    if (missing(data1) || missing(data2)) 
        stop("two data sets are required")
    id <- eval(Call[["id"]], data2, enclos = emptyenv())
    if (is.null(id)) 
        stop("id variable not found in data2")
    if (any(is.na(id))) 
        stop("id variable cannot have missing values")
    if (firstcall) {
        if (!missing(tstop)) {
            tstop <- eval(Call[["tstop"]], data2)
            if (length(tstop) != length(id)) 
                stop("tstop and id must be the same length")
        }
        if (!missing(tstart)) {
            tstart <- eval(Call[["tstart"]], data2)
            if (length(tstart) == 1) 
                tstart <- rep(tstart, length(id))
            if (length(tstart) != length(id)) 
                stop("tstart and id must be the same length")
            if (any(tstart >= tstop)) 
                stop("tstart must be < tstop")
        }
    }
    else {
        if (!missing(tstart) || !missing(tstop)) 
            stop("tstart and tstop arguments only apply to the first call")
    }
    notdot <- c("data1", "data2", "id", "tstart", "tstop", "options")
    dotarg <- Call[is.na(match(names(Call), notdot))]
    dotarg[[1]] <- as.name("list")
    if (missing(data2)) 
        args <- eval(dotarg, envir = new)
    else args <- eval(dotarg, data2, enclos = new)
    argclass <- sapply(args, function(x) (class(x))[1])
    argname <- names(args)
    if (any(argname == "")) 
        stop("all additional argments must have a name")
    check <- match(argclass, c("tdc", "cumtdc", "event", "cumevent"))
    if (any(is.na(check))) 
        stop(paste("argument(s)", argname[is.na(check)], "not a recognized type"))
    tcount <- matrix(0L, length(argname), 8)
    dimnames(tcount) <- list(argname, c("early", "late", "gap", 
        "within", "boundary", "leading", "trailing", "tied"))
    tevent <- attr(data1, "tevent")
    tcens <- attr(data1, "tcensor")
    if (is.null(tcens)) 
        tcens <- vector("list", 0)
    newdata <- data1
    if (firstcall) {
        idname <- Call[["id"]]
        if (!is.name(idname)) 
            stop("on the first call 'id' must be a single variable name")
        indx <- match(c(topt$idname, topt$tstartname, topt$tstopname), 
            names(data1), nomatch = 0)
        if (any(indx[1:2] > 0) && FALSE) {
            overwrite <- c(topt$tstartname, topt$tstopname)[indx[2:3]]
            warning("overwriting data1 variables", paste(overwrite, 
                collapse = " "))
        }
        temp <- as.character(idname)
        if (!is.na(match(temp, names(data1)))) {
            data1[[topt$idname]] <- data1[[temp]]
            baseid <- data1[[temp]]
        }
        else stop("id variable not found in data1")
        if (any(duplicated(baseid))) 
            stop("for the first call (that establishes the time range) data1 must have no duplicate identifiers")
        if (length(baseid) == length(id) && all(baseid == id)) 
            newdata <- data1
        else {
            indx2 <- match(id, baseid)
            if (any(is.na(indx2))) 
                stop("'id' has values not in data1")
            newdata <- data1[indx2, ]
        }
        if (missing(tstop)) {
            if (length(argclass) == 0 || argclass[1] != "event") 
                stop("neither a tstop argument nor an initial event argument was found")
            tstop <- args[[1]][[1]]
        }
        if (any(is.na(tstop))) 
            stop("missing time value, when that variable defines the span")
        if (missing(tstart)) {
            indx <- which(tstop <= 0)
            if (length(indx) > 0) 
                stop("found an ending time of ", tstop[indx[1]], 
                  ", the default starting time of 0 is invalid")
            tstart <- rep(0, length(tstop))
        }
        if (any(tstart >= tstop)) 
            stop("tstart must be < tstop")
        newdata[[topt$tstartname]] <- tstart
        newdata[[topt$tstopname]] <- tstop
        n <- nrow(newdata)
        if (any(duplicated(id))) {
            indx1 <- match(id, unique(id))
            newdata <- newdata[order(indx1, tstop), ]
        }
        temp <- newdata[[topt$idname]]
        if (any(tstart >= tstop)) 
            stop("tstart must be < tstop")
        if (any(newdata$tstart[-n] > newdata$tstop[-1] & temp[-n] == 
            temp[-1])) 
            stop("first call has created overlapping or duplicated time intervals")
    }
    else {
        if (any(is.na(match(id, data1[[topt$idname]])))) 
            stop("id values were found in data2 which are not in data1")
    }
    saveid <- id
    for (ii in seq(along.with = args)) {
        argi <- args[[ii]]
        baseid <- newdata[[topt$idname]]
        dstart <- newdata[[topt$tstartname]]
        dstop <- newdata[[topt$tstopname]]
        argcen <- argi$censor
        etime <- argi$time
        if (length(etime) != length(saveid)) 
            stop("argument ", argname[ii], " is not the same length as id")
        if (!is.null(argi$value)) {
            if (length(argi$value) != length(saveid)) 
                stop("argument", argname[ii], "is not the same length as id")
            if (topt$na.rm) 
                keep <- !(is.na(etime) | is.na(argi$value))
            else keep <- !is.na(etime)
            if (!all(keep)) {
                etime <- etime[keep]
                argi$value <- argi$value[keep]
            }
        }
        else {
            keep <- !is.na(etime)
            etime <- etime[keep]
        }
        id <- saveid[keep]
        indx <- order(match(id, baseid), etime)
        id <- id[indx]
        etime <- etime[indx]
        if (!is.null(argi$value)) 
            yinc <- argi$value[indx]
        else yinc <- NULL
        if (topt$delay > 0 && argclass[ii] %in% c("tdc", "cumtdc")) {
            mintime <- tapply(dstart, baseid, min)
            index <- match(id, names(mintime))
            etime <- ifelse(etime <= mintime[index], etime, etime + 
                topt$delay)
        }
        indx1 <- neardate(id, baseid, etime, dstart, best = "prior")
        indx2 <- neardate(id, baseid, etime, dstop, best = "after")
        itype <- ifelse(is.na(indx1), 1, ifelse(is.na(indx2), 
            2, ifelse(indx2 > indx1, 3, ifelse(etime == dstart[indx1] | 
                etime == dstop[indx2], 5, 4))))
        subtype <- ifelse(itype != 5, 0, ifelse(indx1 == indx2 + 
            1, 1, ifelse(etime == dstart[indx1], 2, 3)))
        tcount[ii, 1:7] <- table(factor(itype + subtype, levels = c(1:4, 
            6:8)))
        tcount[ii, 8] <- sum(tapply(etime, id, function(x) sum(duplicated(x))))
        if (is.null(yinc)) 
            yinc <- rep(1, length(etime))
        indx4 <- which(itype == 4)
        n4 <- length(indx4)
        if (n4 > 0) {
            icount <- tapply(etime[indx4], indx1[indx4], function(x) sort(unique(x)))
            n.add <- sapply(icount, length)
            irep <- rep.int(1L, nrow(newdata))
            erow <- unique(indx1[indx4])
            irep[erow] <- 1 + n.add
            jrep <- rep(1:nrow(newdata), irep)
            newdata <- newdata[jrep, ]
            dstart <- dstart[jrep]
            dstop <- dstop[jrep]
            nfix <- length(erow)
            temp <- vector("list", nfix)
            iend <- (cumsum(irep))[irep > 1]
            for (j in 1:nfix) temp[[j]] <- -(seq(n.add[j] - 1, 
                0)) + iend[j]
            newrows <- unlist(temp)
            if (is.numeric(icount[[1]])) 
                icount <- unlist(icount)
            else icount <- do.call("c", icount)
            dstart[newrows] <- dstop[newrows - 1] <- icount
            newdata[[topt$tstartname]] <- dstart
            newdata[[topt$tstopname]] <- dstop
            for (ename in tevent) newdata[newrows - 1, ename] <- tcens[[ename]]
            baseid <- newdata[[topt$idname]]
            indx1 <- neardate(id, baseid, etime, dstart, best = "prior")
            indx2 <- neardate(id, baseid, etime, dstop, best = "after")
            subtype[itype == 4] <- 1
            itype[itype == 4] <- 5
        }
        newvar <- newdata[[argname[ii]]]
        if (argclass[ii] %in% c("cumtdc", "cumevent")) {
            if (is.logical(yinc)) 
                yinc <- as.numeric(yinc)
            if (!is.numeric(yinc)) 
                stop("invalid increment for cumtdc or cumevent")
            ykeep <- (yinc != 0)
            yinc <- unlist(tapply(yinc, match(id, baseid), cumsum))
        }
        if (argclass[ii] %in% c("event", "cumevent")) {
            if (is.null(newvar)) {
                if (is.numeric(yinc)) 
                  newvar <- rep(0L, nrow(newdata))
                else if (is.factor(yinc)) 
                  newvar <- factor(rep(levels(yinc)[1], nrow(newdata)), 
                    levels(yinc))
                else if (is.character(yinc)) 
                  newvar <- rep("", nrow(newdata))
                else if (is.logical(yinc)) 
                  newvar <- rep(FALSE, nrow(newdata))
                else stop("invalid value for a status variable")
            }
            else {
                if (class(newvar) != class(yinc)) 
                  stop("attempt to update an event variable with a different type")
                if (is.factor(newvar) && !all(levels(yinc) %in% 
                  levels(newvar))) 
                  stop("attemp to update an event variable and levels do not match")
            }
            keep <- (subtype == 1 | subtype == 3)
            if (argclass[ii] == "cumevent") 
                keep <- (keep & ykeep)
            newvar[indx2[keep]] <- yinc[keep]
            if (!(argname[ii] %in% tevent)) {
                tevent <- c(tevent, argname[[ii]])
                if (is.factor(yinc)) 
                  tcens <- c(tcens, list(levels(yinc)[1]))
                else if (is.logical(yinc)) 
                  tcens <- c(tcens, list(FALSE))
                else if (is.character(yinc)) 
                  tcens <- c(tcens, list(""))
                else tcens <- c(tcens, list(0))
                names(tcens) <- tevent
            }
        }
        else {
            if (argname[[ii]] %in% tevent) 
                stop("attempt to turn event variable", argname[[ii]], 
                  "into a tdc")
            keep <- itype != 2
            indx <- ifelse(subtype == 1, indx1, ifelse(subtype == 
                3, indx2 + 1L, indx2))
            if (is.na(topt$tdcstart)) 
                topt$tdcstart <- as.numeric(topt$tdcstart)
            if (is.null(newvar)) {
                if (is.null(argi$value)) 
                  newvar <- rep(0, nrow(newdata))
                else newvar <- rep(topt$tdcstart, nrow(newdata))
            }
            if (is.numeric(yinc)) {
                if (!is.numeric(newvar)) 
                  stop("data and options$tdcstart do not agree on data type")
                storage.mode(yinc) <- storage.mode(dstop) <- "double"
                storage.mode(newvar) <- storage.mode(etime) <- "double"
                newvar <- .Call(Ctmerge, match(baseid, baseid), 
                  dstop, newvar, match(id, baseid)[keep], etime[keep], 
                  yinc[keep], indx[keep])
            }
            else if (is.logical(yinc)) {
                yinc <- as.numeric(yinc)
                if (!is.numeric(newvar)) 
                  stop("data and options$tdcstart do not agree on data type")
                storage.mode(yinc) <- storage.mode(dstop) <- "double"
                storage.mode(newvar) <- storage.mode(etime) <- "double"
                newvar <- .Call(Ctmerge, match(baseid, baseid), 
                  dstop, newvar, match(id, baseid)[keep], etime[keep], 
                  yinc[keep], indx[keep])
                newvar <- as.logical(newvar)
            }
            else {
                if (!(is.factor(yinc) || is.character(yinc))) 
                  stop("the second argument of tdc must be numeric, character, or factor")
                newlev <- unique(c(levels(as.factor(yinc)), levels(as.factor(newvar))))
                y2 <- factor(yinc, levels = newlev)
                newvar <- factor(newvar, levels = newlev)
                storage.mode(dstop) <- storage.mode(etime) <- "double"
                new <- .Call(Ctmerge, match(baseid, baseid), 
                  dstop, as.numeric(newvar), match(id, baseid)[keep], 
                  etime[keep], as.numeric(y2[keep]), indx[keep])
                if (is.factor(yinc)) 
                  newvar <- factor(new, labels = newlev)
                else newvar <- newlev[new]
            }
        }
        newdata[[argname[ii]]] <- newvar
    }
    attr(newdata, "tname") <- topt[c("idname", "tstartname", 
        "tstopname")]
    attr(newdata, "tcount") <- rbind(attr(data1, "tcount"), tcount)
    if (length(tevent)) {
        attr(newdata, "tevent") <- tevent
        attr(newdata, "tcensor") <- tcens
    }
    row.names(newdata) <- NULL
    class(newdata) <- c("data.frame")
    newdata
}


neardate <- function (id1, id2, y1, y2, best = c("after", "prior"), nomatch = NA_integer_) 
{
    if (missing(id1)) 
        stop("id1 argument is required")
    if (missing(id2)) 
        stop("id2 argument is required")
    if (missing(y1)) 
        stop("y1 argument is required")
    if (missing(y2)) 
        stop("y2 argument is required")
    if (length(id1) != length(y1)) 
        stop("id1 and y1 have different lengths")
    if (length(id2) != length(y2)) 
        stop("id2 and y2 have different lengths")
    best <- match.arg(best)
    if (is.factor(y1) || is.factor(y2)) 
        stop("y1 and y2 must be sortable")
    if (inherits(y1, "POSIXt")) 
        if (!inherits(y2, "POSIXt")) 
            y2 <- as(y2, class(y1))
        else if (inherits(y2, "POSIXt")) 
            y1 <- as(y1, class(y2))
    alldate <- sort(unique(c(y1, y2)))
    y1 <- match(y1, alldate)
    y2 <- match(y2, alldate)
    rowid <- 1:length(y2)
    if (any(is.na(y2))) {
        toss <- is.na(y2)
        y2 <- y2[!toss]
        if (!missing(id2)) 
            id2 <- id2[!toss]
        rowid <- rowid[!toss]
    }
    n2 <- length(y2)
    if (n2 == 0) 
        stop("No valid entries in data set 2")
    indx1 <- match(id2, id1)
    toss <- is.na(indx1)
    if (any(toss)) {
        id2 <- id2[!toss]
        y2 <- y2[!toss]
        indx1 <- indx1[!toss]
        rowid <- rowid[!toss]
    }
    n2 <- length(y2)
    if (n2 == 0) 
        stop("No valid entries in data set 2")
    delta <- 1 + length(alldate)
    hash1 <- match(id1, id1) * delta + y1
    hash2 <- indx1 * delta + y2
    if (best == "prior") 
        indx2 <- approx(hash2, 1:n2, hash1, method = "constant", 
            yleft = NA, rule = 2, f = 0)$y
    else indx2 <- approx(hash2, 1:n2, hash1, method = "constant", 
        yright = NA, rule = 2, f = 1)$y
    temp <- (!is.na(indx2) & id1 == id2[indx2])
    ifelse(temp, rowid[ifelse(is.na(indx2), 1, indx2)], nomatch)
}


pspline <- function (x, df = 4, theta, nterm = 2.5 * df, degree = 3, eps = 0.1, 
    method, Boundary.knots = range(x), intercept = FALSE, penalty = TRUE, 
    combine, ...) 
{
    if (!missing(theta)) {
        method <- "fixed"
        if (theta <= 0 || theta >= 1) 
            stop("Invalid value for theta")
    }
    else if (df == 0 || (!missing(method) && method == "aic")) {
        method <- "aic"
        nterm <- 15
        if (missing(eps)) 
            eps <- 1e-05
    }
    else {
        method <- "df"
        if (df <= 1) 
            stop("Too few degrees of freedom")
        if (df > nterm) 
            stop("`nterm' too small for df=", df)
    }
    xname <- deparse(substitute(x))
    keepx <- !is.na(x)
    if (!all(keepx)) 
        x <- x[keepx]
    nterm <- round(nterm)
    if (nterm < 3) 
        stop("Too few basis functions")
    if (!missing(Boundary.knots)) {
        if (!is.numeric(Boundary.knots) || length(Boundary.knots) != 
            2 || Boundary.knots[1] >= Boundary.knots[2]) 
            stop("Invalid values for Boundary.knots")
        outl <- (x < Boundary.knots[1])
        outr <- (x > Boundary.knots[2])
        outside <- outl | outr
    }
    else outside <- FALSE
    dx <- (Boundary.knots[2] - Boundary.knots[1])/nterm
    knots <- c(Boundary.knots[1] + dx * ((-degree):(nterm - 1)), 
        Boundary.knots[2] + dx * (0:degree))
    if (any(outside)) {
        newx <- matrix(0, length(x), nterm + degree)
        if (any(outl)) {
            tt <- spline.des(knots, Boundary.knots[c(1, 1)], 
                degree + 1, 0:1)
            newx[outl, ] <- cbind(1, x[outl] - Boundary.knots[1]) %*% 
                tt$design
        }
        if (any(outr)) {
            tt <- spline.des(knots, Boundary.knots[c(2, 2)], 
                degree + 1, 0:1)
            newx[outr, ] <- cbind(1, x[outr] - Boundary.knots[2]) %*% 
                tt$design
        }
        if (any(inside <- !outside)) 
            newx[inside, ] <- spline.des(knots, x[inside], degree + 
                1)$design
    }
    else newx <- spline.des(knots, x, degree + 1, outer.ok = TRUE)$design
    if (!all(keepx)) {
        temp <- matrix(NA, length(keepx), ncol(newx))
        temp[keepx, ] <- newx
        newx <- temp
    }
    if (!missing(combine)) {
        if (any(combine != floor(combine) | combine < 0) || any(diff(combine) < 
            0)) 
            stop("combine must be an increasing vector of positive integers")
        if (!intercept) 
            combine <- c(0, combine)
        if (length(combine) != ncol(newx)) 
            stop("wrong length for combine")
        uc <- sort(unique(combine))
        tmat <- matrix(0, nrow = ncol(newx), ncol = length(uc))
        for (i in 1:length(uc)) tmat[combine == uc[i], i] <- 1
        newx <- newx %*% tmat
    }
    nvar <- ncol(newx)
    dmat <- diag(nvar)
    dmat <- apply(dmat, 2, diff, 1, 2)
    dmat <- t(dmat) %*% dmat
    if (intercept) 
        xnames <- paste("ps(", xname, ")", 1:nvar, sep = "")
    else {
        newx <- newx[, -1, drop = FALSE]
        dmat <- dmat[-1, -1, drop = FALSE]
        xnames <- paste("ps(", xname, ")", 1 + 2:nvar, sep = "")
    }
    if (!penalty) {
        attributes(newx) <- c(attributes(newx), list(intercept = intercept, 
            nterm = nterm, Boundary.knots = Boundary.knots))
        class(newx) <- "pspline"
        return(newx)
    }
    pfun <- function(coef, theta, n, dmat) {
        if (theta >= 1) 
            list(penalty = 100 * (1 - theta), flag = TRUE)
        else {
            if (theta <= 0) 
                lambda <- 0
            else lambda <- theta/(1 - theta)
            list(penalty = c(coef %*% dmat %*% coef) * lambda/2, 
                first = c(dmat %*% coef) * lambda, second = c(dmat * 
                  lambda), flag = FALSE)
        }
    }
    printfun <- function(coef, var, var2, df, history, cbase) {
        test1 <- coxph.wtest(var, coef)$test
        xmat <- cbind(1, cbase)
        xsig <- coxph.wtest(var, xmat)$solve
        cmat <- coxph.wtest(t(xmat) %*% xsig, t(xsig))$solve[2, 
            ]
        linear <- sum(cmat * coef)
        lvar1 <- c(cmat %*% var %*% cmat)
        lvar2 <- c(cmat %*% var2 %*% cmat)
        test2 <- linear^2/lvar1
        cmat <- rbind(c(linear, sqrt(lvar1), sqrt(lvar2), test2, 
            1, pchisq(test2, 1, lower.tail = FALSE)), c(NA, NA, 
            NA, test1 - test2, df - 1, pchisq(test1 - test2, 
                max(0.5, df - 1), lower.tail = FALSE)))
        dimnames(cmat) <- list(c("linear", "nonlin"), NULL)
        nn <- nrow(history$thetas)
        if (length(nn)) 
            theta <- history$thetas[nn, 1]
        else theta <- history$theta
        list(coef = cmat, history = paste("Theta=", format(theta)))
    }
    temp <- formals(printfun)
    temp$cbase <- knots[2:nvar] + (Boundary.knots[1] - knots[1])
    formals(printfun) <- temp
    environment(printfun) <- .GlobalEnv
    if (method == "fixed") {
        temp <- list(pfun = pfun, printfun = printfun, pparm = dmat, 
            diag = FALSE, cparm = list(theta = theta), varname = xnames, 
            cfun = function(parms, iter, old) list(theta = parms$theta, 
                done = TRUE))
    }
    else if (method == "df") {
        temp <- list(pfun = pfun, printfun = printfun, diag = FALSE, 
            cargs = ("df"), cparm = list(df = df, eps = eps, 
                thetas = c(1, 0), dfs = c(1, nterm), guess = 1 - 
                  df/nterm, ...), pparm = dmat, varname = xnames, 
            cfun = frailty.controldf)
    }
    else {
        temp <- list(pfun = pfun, printfun = printfun, pparm = dmat, 
            diag = FALSE, cargs = c("neff", "df", "plik"), cparm = list(eps = eps, 
                init = c(0.5, 0.95), lower = 0, upper = 1, ...), 
            varname = xnames, cfun = frailty.controlaic)
    }
    attributes(newx) <- c(attributes(newx), temp, list(intercept = intercept, 
        nterm = nterm, Boundary.knots = Boundary.knots))
    class(newx) <- c("pspline", "coxph.penalty")
    newx
}


survdiff <- function (formula, data, subset, na.action, rho = 0, timefix = TRUE) 
{
    call <- match.call()
    m <- match.call(expand.dots = FALSE)
    m$rho <- NULL
    if (!inherits(formula, "formula")) 
        stop("The 'formula' argument is not a formula")
    Terms <- if (missing(data)) 
        terms(formula, "strata")
    else terms(formula, "strata", data = data)
    m$formula <- Terms
    m[[1L]] <- quote(stats::model.frame)
    m <- eval(m, parent.frame())
    y <- model.extract(m, "response")
    if (!inherits(y, "Surv")) 
        stop("Response must be a survival object")
    if (attr(y, "type") != "right") 
        stop("Right censored data only")
    ny <- ncol(y)
    n <- nrow(y)
    if (!is.logical(timefix) || length(timefix) > 1) 
        stop("invalid value for timefix option")
    if (timefix) 
        y <- aeqSurv(y)
    offset <- attr(Terms, "offset")
    if (!is.null(offset)) {
        offset <- as.numeric(m[[offset]])
        if (length(attr(Terms, "factors")) > 0) 
            stop("Cannot have both an offset and groups")
        if (any(offset < 0 | offset > 1)) 
            stop("The offset must be a survival probability")
        expected <- sum(-log(offset))
        observed <- sum(y[, ny])
        if (rho != 0) {
            num <- sum(1/rho - ((1/rho + y[, ny]) * offset^rho))
            var <- sum(1 - offset^(2 * rho))/(2 * rho)
        }
        else {
            var <- sum(-log(offset))
            num <- var - observed
        }
        chi <- num * num/var
        rval <- list(n = n, obs = observed, exp = expected, var = var, 
            chisq = chi)
    }
    else {
        strats <- attr(Terms, "specials")$strata
        if (length(strats)) {
            temp <- untangle.specials(Terms, "strata", 1)
            dropx <- temp$terms
            if (length(temp$vars) == 1) 
                strata.keep <- m[[temp$vars]]
            else strata.keep <- strata(m[, temp$vars], shortlabel = TRUE)
        }
        else strata.keep <- rep(1, nrow(m))
        if (length(strats)) 
            ll <- attr(Terms[-dropx], "term.labels")
        else ll <- attr(Terms, "term.labels")
        if (length(ll) == 0) 
            stop("No groups to test")
        else groups <- strata(m[ll])
        fit <- survdiff.fit(y, groups, strata.keep, rho)
        if (is.matrix(fit$observed)) {
            otmp <- apply(fit$observed, 1, sum)
            etmp <- apply(fit$expected, 1, sum)
        }
        else {
            otmp <- fit$observed
            etmp <- fit$expected
        }
        df <- (etmp > 0)
        if (sum(df) < 2) 
            chi <- 0
        else {
            temp2 <- ((otmp - etmp)[df])[-1]
            vv <- (fit$var[df, df])[-1, -1, drop = FALSE]
            chi <- sum(solve(vv, temp2) * temp2)
        }
        rval <- list(n = table(groups), obs = fit$observed, exp = fit$expected, 
            var = fit$var, chisq = chi)
        if (length(strats)) 
            rval$strata <- table(strata.keep)
    }
    na.action <- attr(m, "na.action")
    if (length(na.action)) 
        rval$na.action <- na.action
    rval$call <- call
    class(rval) <- "survdiff"
    rval
}


Surv <- function (time, time2, event, type = c("right", "left", "interval", 
    "counting", "interval2", "mstate"), origin = 0) 
{
    if (missing(time)) 
        stop("Must have a time argument")
    if (inherits(time, "difftime")) 
        time <- unclass(time)
    if (!missing(time2) && inherits(time2, "difftime")) 
        time2 <- as.numeric(time2)
    if (!is.numeric(time)) 
        stop("Time variable is not numeric")
    nn <- length(time)
    ng <- (!missing(time)) + (!missing(time2)) + (!missing(event))
    mtype <- match.arg(type)
    if (missing(type) || mtype == "mstate") {
        if (ng == 1 || ng == 2) 
            type <- "right"
        else if (ng == 3) 
            type <- "counting"
        else stop("No time variable!")
    }
    else {
        type <- mtype
        if (ng != 3 && (type == "interval" || type == "counting")) 
            stop("Wrong number of args for this type of survival data")
        if (ng != 2 && (type == "right" || type == "left" || 
            type == "interval2")) 
            stop("Wrong number of args for this type of survival data")
    }
    if (ng == 1) {
        if (!is.numeric(time)) 
            stop("Time variable is not numeric")
        ss <- cbind(time = time - origin, status = 1)
        type <- "right"
    }
    else if (type == "right" || type == "left") {
        if (!is.numeric(time)) 
            stop("Time variable is not numeric")
        if (missing(event)) {
            event <- time2
            time2 <- NULL
        }
        if (length(event) != nn) 
            stop("Time and status are different lengths")
        if (mtype == "mstate" || (is.factor(event))) {
            mstat <- as.factor(event)
            status <- as.numeric(mstat) - 1
            type <- "mright"
        }
        else {
            if (is.logical(event)) 
                status <- as.numeric(event)
            else if (is.numeric(event)) {
                who2 <- !is.na(event)
                if (max(event[who2]) == 2) 
                  status <- event - 1
                else status <- event
                temp <- (status == 0 | status == 1)
                status <- ifelse(temp, status, NA)
                if (!all(temp[who2], na.rm = TRUE)) 
                  warning("Invalid status value, converted to NA")
            }
            else stop("Invalid status value, must be logical or numeric")
        }
        ss <- cbind(time = time - origin, status = status)
    }
    else if (type == "counting") {
        if (length(time2) != nn) 
            stop("Start and stop are different lengths")
        if (length(event) != nn) 
            stop("Start and event are different lengths")
        if (!is.numeric(time)) 
            stop("Start time is not numeric")
        if (!is.numeric(time2)) 
            stop("Stop time is not numeric")
        temp <- (time >= time2)
        if (any(temp & !is.na(temp))) {
            time[temp] <- NA
            warning("Stop time must be > start time, NA created")
        }
        if (mtype == "mstate" || is.factor(event)) {
            mstat <- as.factor(event)
            status <- as.numeric(mstat) - 1
            type <- "mcounting"
        }
        else {
            if (is.logical(event)) 
                status <- as.numeric(event)
            else if (is.numeric(event)) {
                who2 <- !is.na(event)
                if (max(event[who2]) == 2) 
                  status <- event - 1
                else status <- event
                temp <- (status == 0 | status == 1)
                status <- ifelse(temp, status, NA)
                if (!all(temp[who2], na.rm = TRUE)) 
                  warning("Invalid status value, converted to NA")
            }
            else stop("Invalid status value")
        }
        ss <- cbind(start = time - origin, stop = time2 - origin, 
            status = status)
    }
    else {
        if (type == "interval2") {
            if (!is.numeric(time2)) 
                stop("Time2 must be numeric")
            if (length(time2) != nn) 
                stop("time and time2 are different lengths")
            backwards <- (!is.na(time) & !is.na(time2) & time > 
                time2)
            time <- ifelse(is.finite(time), time, NA)
            time2 <- ifelse(is.finite(time2), time2, NA)
            unknown <- (is.na(time) & is.na(time2))
            status <- ifelse(is.na(time), 2, ifelse(is.na(time2), 
                0, ifelse(time == time2, 1, 3)))
            time <- ifelse(status != 2, time, time2)
            if (any(backwards)) {
                warning("Invalid interval: start > stop, NA created")
                status[backwards] <- NA
            }
            if (any(unknown)) 
                status[unknown] <- NA
            type <- "interval"
        }
        else {
            if (length(event) != nn) 
                stop("Time and status are different lengths")
            if (!is.numeric(event)) 
                stop("Invalid status value, must be logical or numeric")
            temp <- (event == 0 | event == 1 | event == 2 | event == 
                3)
            status <- ifelse(temp, event, NA)
            if (!all(temp, na.rm = TRUE)) 
                warning("Status must be 0, 1, 2 or 3; converted to NA")
            if (any(event == 3, na.rm = T)) {
                if (!is.numeric(time2)) 
                  stop("Time2 must be numeric")
                if (length(time2) != nn) 
                  stop("time and time2 are different lengths")
                temp <- (status == 3 & time > time2)
                if (any(temp & !is.na(temp))) {
                  status[temp] <- NA
                  warning("Invalid interval: start > stop, NA created")
                }
            }
            else time2 <- 1
        }
        ss <- cbind(time1 = time - origin, time2 = ifelse(!is.na(status) & 
            status == 3, time2 - origin, 1), status = status)
    }
    inputAttributes <- list()
    if (!is.null(attributes(time))) 
        inputAttributes$time <- attributes(time)
    if (!missing(time2) && !is.null(attributes(time2))) 
        inputAttributes$time2 <- attributes(time2)
    if (!missing(event) && !is.null(attributes(event))) 
        inputAttributes$event <- attributes(event)
    cname <- dimnames(ss)[[2]]
    if (length(cname) == 0) {
        if (ncol(ss) == 2) 
            cname <- c("time", "status")
        else if (type == "counting") 
            cname <- c("start", "stop", "status")
        else cname <- c("time1", "time2", "status")
    }
    dimnames(ss) <- list(NULL, cname)
    attr(ss, "type") <- type
    if (type == "mright" || type == "mcounting") {
        states <- levels(mstat)[-1]
        if (any(is.na(states) | states == "")) 
            stop("each state must have a non-blank name")
        attr(ss, "states") <- states
    }
    if (length(inputAttributes) > 0) 
        attr(ss, "inputAttributes") <- inputAttributes
    class(ss) <- "Surv"
    ss
}


is.Surv <- function (x) 
inherits(x, "Surv")


cch <- function (formula, data = sys.parent(), subcoh, id, stratum = NULL, 
    cohort.size, method = c("Prentice", "SelfPrentice", "LinYing", 
        "I.Borgan", "II.Borgan"), robust = FALSE) 
{
    call <- match.call()
    if (is.data.frame(data)) {
        if (inherits(id, "formula")) 
            id <- stats::model.frame(id, data, na.action = na.fail)[, 
                1]
        if (inherits(subcoh, "formula")) 
            subcoh <- stats::model.frame(subcoh, data, na.action = na.fail)[, 
                1]
        if (inherits(stratum, "formula")) 
            stratum <- stats::model.frame(stratum, data, na.action = na.fail)[, 
                1]
    }
    if (length(id) != length(unique(id))) 
        stop("Multiple records per id not allowed")
    if (is.logical(subcoh)) 
        subcoh <- as.numeric(subcoh)
    tt <- table(subcoh)
    if (min(charmatch(names(tt), c("0", "1"), 0)) == 0) 
        stop("Permissible values for subcohort indicator are 0/1 or TRUE/FALSE")
    if (length(id) > sum(cohort.size)) 
        stop("Number of records greater than cohort size")
    nn <- cohort.size
    method <- match.arg(method)
    stratified <- method %in% c("I.Borgan", "II.Borgan")
    if (!is.null(stratum)) 
        stratum <- factor(stratum)
    if (stratified) {
        if (robust) 
            warning("`robust' not implemented for stratified analysis.")
        if (is.null(stratum)) 
            stop("method (", method, ") requires 'stratum'")
        if (length(cohort.size) != length(levels(stratum))) 
            stop("cohort.size and stratum do not match")
        if (!(all(levels(stratum) %in% names(cohort.size)))) 
            warning("stratum levels and names(cohort.size) do not agree")
        subcohort.sizes <- table(stratum)
    }
    else if (!stratified) {
        if (!(method == "LinYing") && robust) 
            warning("`robust' ignored for  method (", method, 
                ")")
        if (!is.null(stratum)) 
            warning("'stratum' ignored for method (", method, 
                ")")
        if (length(cohort.size) != 1) 
            stop("cohort size must be a scalar for unstratified analysis")
        subcohort.sizes <- length(id)
    }
    if (any(subcohort.sizes > cohort.size)) 
        stop("Population smaller than sample in some strata")
    m <- match.call(expand.dots = FALSE)
    m$method <- m$cohort.size <- m$id <- m$subcoh <- m$stratum <- m$robust <- NULL
    m[[1L]] <- quote(stats::model.frame)
    m <- eval(m, sys.parent())
    Terms <- attr(m, "terms")
    Y <- model.extract(m, "response")
    if (!inherits(Y, "Surv")) 
        stop("Response must be a survival object")
    type <- attr(Y, "type")
    itype <- charmatch(type, c("right", "counting"), nomatch = 0)
    cens <- switch(itype + 1, stop(paste("Cox model doesn't support \"", 
        type, "\" survival data", sep = "")), Y[, 2], Y[, 3])
    if (any(!subcoh & !cens)) 
        stop(sum(!subcoh & !cens), "censored observations not in subcohort")
    cc <- cens + 1 - subcoh
    texit <- switch(itype + 1, stop(), Y[, 1], Y[, 2])
    tenter <- switch(itype + 1, stop(), rep(0, length(texit)), 
        Y[, 1])
    X <- model.matrix(Terms, m)
    X <- X[, 2:ncol(X)]
    fitter <- get(method)
    if (ncol(Y) == 3) 
        dtime <- unique(Y[cens == 1, 2])
    else dtime <- unique(Y[cens == 1, 1])
    if (length(dtime) > 1) 
        delta <- min(diff(sort(dtime)))/2
    else delta <- 1
    if (stratified) 
        out <- fitter(tenter = tenter, texit = texit, cc = cc, 
            id = id, X = X, stratum = as.numeric(stratum), stratum.sizes = cohort.size, 
            delta)
    else out <- fitter(tenter = tenter, texit = texit, cc = cc, 
        id = id, X = X, ntot = nn, robust = robust, delta)
    out$method <- method
    names(out$coefficients) <- dimnames(X)[[2]]
    if (!is.null(out$var)) 
        dimnames(out$var) <- list(dimnames(X)[[2]], dimnames(X)[[2]])
    if (!is.null(out$naive.var)) 
        dimnames(out$naive.var) <- list(dimnames(X)[[2]], dimnames(X)[[2]])
    out$call <- call
    out$cohort.size <- cohort.size
    out$stratified <- stratified
    if (stratified) {
        out$stratum <- stratum
        out$subcohort.size <- subcohort.sizes
    }
    else {
        out$subcohort.size <- tt[2]
    }
    class(out) <- "cch"
    out
}


survfitCI <- function (X, Y, weights, id, istate, cluster, stype = 1, ctype = 1, 
    se.fit = TRUE, conf.int = 0.95, conf.type = c("log", "log-log", 
        "plain", "none", "logit", "arcsin"), conf.lower = c("usual", 
        "peto", "modified"), influence = FALSE, start.time, p0, 
    type) 
{
    if (!missing(type)) {
        if (!is.character(type)) 
            stop("type argument must be character")
        temp <- charmatch(type, c("kaplan-meier", "fleming-harrington", 
            "fh2"))
        if (is.na(temp)) 
            stop("invalid value for 'type'")
        type <- c(1, 3, 4)[temp]
    }
    else {
        if (!(ctype %in% 1:2)) 
            stop("ctype must be 1 or 2")
        if (!(stype %in% 1:2)) 
            stop("stype must be 1 or 2")
        type <- as.integer(2 * stype + ctype - 2)
    }
    conf.type <- match.arg(conf.type)
    conf.lower <- match.arg(conf.lower)
    if (conf.lower != "usual") 
        warning("conf.lower is ignored for multi-state data")
    if (is.logical(conf.int)) {
        if (!conf.int) 
            conf.type <- "none"
        conf.int <- 0.95
    }
    has.cluster <- !(missing(cluster) || length(cluster) == 0)
    has.id <- !(missing(id) || length(id) == 0)
    if (has.id) 
        id <- as.factor(id)
    if (has.cluster) {
        if (is.factor(cluster)) {
            clname <- levels(cluster)
            cluster <- as.integer(cluster)
        }
        else {
            clname <- sort(unique(cluster))
            cluster <- match(cluster, clname)
        }
        ncluster <- length(clname)
    }
    else {
        if (has.id) {
            clname <- levels(id)
            cluster <- as.integer(id)
            ncluster <- length(clname)
        }
        else {
            ncluster <- 0
            clname <- NULL
        }
    }
    if (is.logical(influence)) {
        if (!influence) 
            influence <- 0L
        else influence <- 3L
    }
    else if (!is.numeric(influence)) 
        stop("influence argument must be numeric or logical")
    if (!(influence %in% 0:3)) 
        stop("influence argument must be 0, 1, 2, or 3")
    else influence <- as.integer(influence)
    if (!se.fit) {
        ncluster <- 0L
        influence <- 0L
    }
    type <- attr(Y, "type")
    if (type != "mright" && type != "mcounting") 
        stop(paste("multi-state computation doesn't support \"", 
            type, "\" survival data", sep = ""))
    if (!missing(start.time)) {
        if (!is.numeric(start.time) || length(start.time) != 
            1 || !is.finite(start.time)) 
            stop("start.time must be a single numeric value")
        toss <- which(Y[, ncol(Y) - 1] <= start.time)
        if (length(toss)) {
            n <- nrow(Y)
            if (length(toss) == n) 
                stop("start.time has removed all observations")
            Y <- Y[-toss, , drop = FALSE]
            X <- X[-toss]
            weights <- weights[-toss]
            if (length(id) == n) 
                id <- id[-toss]
            if (!missing(istate) && length(istate) == n) 
                istate <- istate[-toss]
        }
    }
    n <- nrow(Y)
    status <- Y[, ncol(Y)]
    ncurve <- length(levels(X))
    state.names <- attr(Y, "states")
    nstate <- length(state.names)
    if (missing(istate) || is.null(istate)) {
        istate <- rep(nstate + 1L, n)
        state.names <- c(state.names, "")
    }
    else {
        if (is.factor(istate) || is.character(istate)) {
            temp <- as.factor(istate)
            appear <- (levels(temp))[unique(as.numeric(temp))]
            state.names <- unique(c(attr(Y, "states"), appear))
            istate <- as.numeric(factor(as.character(temp), levels = state.names))
        }
        else {
            if (!is.numeric(istate) || any(istate != floor(istate)) || 
                any(istate < 1)) 
                stop("istate should be a vector of positive integers or a factor")
            if (max(istate) > nstate) 
                state.names <- c(state.names, (1 + nstate):max(istate))
        }
    }
    if (length(id) == 0) 
        id <- 1:n
    if (length(istate) == 1) 
        istate <- rep(istate, n)
    if (length(istate) != n) 
        stop("wrong length for istate")
    states <- 1:length(state.names)
    if (!missing(p0)) {
        if (length(p0) != length(state.names)) 
            stop("wrong length for p0")
        if (!is.numeric(p0) || abs(1 - sum(p0)) > sqrt(.Machine$double.eps)) 
            stop("p0 must be a numeric vector that adds to 1")
    }
    else p0 <- NULL
    curves <- vector("list", ncurve)
    names(curves) <- levels(X)
    if (ncol(Y) == 2) {
        indx <- which(status == istate & status != 0)
        if (length(indx)) {
            warning("an observation transitions to it's starting state, transition ignored")
            status[indx] <- 0
        }
        if (length(id) && any(duplicated(id))) 
            stop("Cannot have duplicate id values with (time, status) data")
        nst <- length(state.names)
        transitions <- table(factor(istate, 1:nst), factor(Y[, 
            2], 1:nstate))
        dimnames(transitions) <- list(from = state.names, to = state.names[1:nstate])
        t0 <- min(0, Y[, 1])
        entry <- rep(t0 - 1, nrow(Y))
        for (i in levels(X)) {
            indx <- which(X == i)
            curves[[i]] <- docurve2(entry[indx], Y[indx, 1], 
                status[indx], istate[indx], weights[indx], states, 
                id[indx], se.fit, influence, p0)
        }
    }
    else {
        if (missing(id) || is.null(id)) 
            stop("the id argument is required for start:stop data")
        indx <- order(id, Y[, 2])
        indx1 <- indx[-length(indx)]
        indx2 <- indx[-1]
        same <- (id[indx1] == id[indx2])
        if (any(same & X[indx1] != X[indx2])) {
            who <- min(which(same & X[indx1] != X[indx2]))
            stop("subject is in two different groups, id ", id[indx1[who]])
        }
        if (any(same & Y[indx1, 2] != Y[indx2, 1])) {
            who <- min(which(same & Y[indx1, 2] != Y[indx2, 1]))
            stop("gap in follow-up, id ", id[indx1[who]])
        }
        if (any(Y[, 1] == Y[, 2])) 
            stop("cannot have start time == stop time")
        indx <- order(id, Y[, 2])
        uid <- unique(id)
        temp <- (istate[indx])[match(uid, id[indx])]
        istate <- temp[match(id, uid)]
        last <- !duplicated(id[indx], fromLast = TRUE)
        extra <- (Y[indx, 3] == 0 & !last)
        if (any(extra)) {
            e2 <- indx[extra]
            Y <- cbind(Y[-(1 + e2), 1], Y[-e2, 2])
            status <- status[-e2]
            X <- X[-e2]
            id <- id[-e2]
            istate <- istate[-e2]
            weights <- weights[-e2]
            indx <- order(id, Y[, 2])
        }
        nst <- length(state.names)
        first <- (!duplicated(id[indx]))
        last <- (!duplicated(id[indx], fromLast = TRUE))
        transitions <- table(factor(istate[indx[first]], 1:nst), 
            factor(status[indx[first]], 1:nstate))
        if (any(!last)) 
            transitions <- transitions + table(factor(status[indx[!last]], 
                1:nst), factor(status[indx[!first]], 1:nstate))
        dimnames(transitions) = list(from = state.names, to = state.names[1:nstate])
        for (i in levels(X)) {
            indx <- which(X == i)
            curves[[i]] <- docurve2(Y[indx, 1], Y[indx, 2], status[indx], 
                istate[indx], weights[indx], states, id[indx], 
                se.fit, influence, p0)
        }
    }
    grabit <- function(clist, element) {
        temp <- (clist[[1]][[element]])
        if (is.matrix(temp)) {
            do.call("rbind", lapply(clist, function(x) x[[element]]))
        }
        else {
            xx <- as.vector(unlist(lapply(clist, function(x) x[element])))
            if (class(temp) == "table") 
                matrix(xx, byrow = T, ncol = length(temp))
            else xx
        }
    }
    if (length(curves) == 1) {
        keep <- c("n", "time", "n.risk", "n.event", "n.censor", 
            "pstate", "p0", "cumhaz", "influence")
        if (se.fit) 
            keep <- c(keep, "std.err", "sp0")
        kfit <- (curves[[1]])[match(keep, names(curves[[1]]), 
            nomatch = 0)]
        names(kfit$p0) <- state.names
        if (se.fit) 
            kfit$logse <- FALSE
    }
    else {
        kfit <- list(n = as.vector(table(X)), time = grabit(curves, 
            "time"), n.risk = grabit(curves, "n.risk"), n.event = grabit(curves, 
            "n.event"), n.censor = grabit(curves, "n.censor"), 
            pstate = grabit(curves, "pstate"), p0 = grabit(curves, 
                "p0"), transitions = transitions, strata = unlist(lapply(curves, 
                function(x) length(x$time))))
        kfit$p0 <- matrix(kfit$p0, ncol = nst, byrow = TRUE, 
            dimnames = list(names(curves), state.names))
        if (se.fit) {
            kfit$std.err <- grabit(curves, "std.err")
            kfit$sp0 <- matrix(grabit(curves, "sp0"), ncol = nst, 
                byrow = TRUE)
            kfit$logse <- FALSE
        }
        kfit$cumhaz <- array(unlist(lapply(curves, function(x) x$cumhaz)), 
            dim = c(nst, nst, length(kfit$time)))
        if (influence) 
            kfit$influence <- lapply(curves, function(x) x$influence)
        if (!missing(start.time)) 
            kfit$start.time <- start.time
    }
    kfit$transitions <- transitions
    if (se.fit) {
        kfit$conf.int <- conf.int
        kfit$conf.type <- conf.type
        if (conf.type != "none") {
            ci <- survfit_confint(kfit$pstate, kfit$std.err, 
                logse = FALSE, conf.type, conf.int)
            kfit <- c(kfit, ci, conf.type = conf.type, conf.int = conf.int)
        }
    }
    kfit$states <- state.names
    kfit$type <- attr(Y, "type")
    kfit
}


finegray <- function (formula, data, weights, subset, na.action = na.pass, 
    etype, prefix = "fg", count = "", id, timefix = TRUE) 
{
    Call <- match.call()
    indx <- match(c("formula", "data", "weights", "subset", "id"), 
        names(Call), nomatch = 0)
    if (indx[1] == 0) 
        stop("A formula argument is required")
    temp <- Call[c(1, indx)]
    temp$na.action <- na.action
    temp[[1L]] <- quote(stats::model.frame)
    special <- c("strata", "cluster")
    temp$formula <- if (missing(data)) 
        terms(formula, special)
    else terms(formula, special, data = data)
    mf <- eval(temp, parent.frame())
    if (nrow(mf) == 0) 
        stop("No (non-missing) observations")
    Terms <- terms(mf)
    Y <- model.extract(mf, "response")
    if (!inherits(Y, "Surv")) 
        stop("Response must be a survival object")
    type <- attr(Y, "type")
    if (type != "mright" && type != "mcounting") 
        stop("Fine-Gray model requires a multi-state survival")
    nY <- ncol(Y)
    states <- attr(Y, "states")
    if (timefix) 
        Y <- aeqSurv(Y)
    strats <- attr(Terms, "specials")$strata
    if (length(strats)) {
        stemp <- untangle.specials(Terms, "strata", 1)
        if (length(stemp$vars) == 1) 
            strata <- mf[[stemp$vars]]
        else strata <- survival::strata(mf[, stemp$vars], shortlabel = TRUE)
        istrat <- as.numeric(strata)
        mf[stemp$vars] <- NULL
    }
    else istrat <- rep(1, nrow(mf))
    id <- model.extract(mf, "id")
    if (!is.null(id)) 
        mf["(id)"] <- NULL
    user.weights <- model.weights(mf)
    if (is.null(user.weights)) 
        user.weights <- rep(1, nrow(mf))
    cluster <- attr(Terms, "specials")$cluster
    if (length(cluster)) {
        stop("a cluster() term is not valid")
    }
    delay <- FALSE
    if (type == "mcounting") {
        if (is.null(id)) 
            stop("(start, stop] data requires a subject id")
        else {
            index <- order(id, Y[, 2])
            sorty <- Y[index, ]
            first <- which(!duplicated(id[index]))
            last <- c(first[-1] - 1, length(id))
            if (any(sorty[-last, 3] != 0)) 
                stop("a subject has a transition before their last time point")
            delta <- c(sorty[-1, 1], 0) - sorty[, 2]
            if (any(delta[-last] != 0)) 
                stop("a subject has gaps in time")
            if (any(Y[first, 1] > min(Y[, 2]))) 
                delay <- TRUE
            temp1 <- temp2 <- rep(FALSE, nrow(mf))
            temp1[index[first]] <- TRUE
            temp2[index[last]] <- TRUE
            first <- temp1
            last <- temp2
        }
    }
    else last <- rep(TRUE, nrow(mf))
    if (missing(etype)) 
        enum <- 1
    else {
        index <- match(etype, states)
        if (any(is.na(index))) 
            stop("etype argument has a state that is not in the data")
        enum <- index[1]
        if (length(index) > 1) 
            warning("only the first endpoint was used")
    }
    if (!missing(count)) 
        count <- make.names(count)
    else count <- NULL
    oname <- paste0(prefix, c("start", "stop", "status", "wt"))
    find2 <- function(x, vec, left.open = FALSE, ...) {
        if (!left.open) 
            findInterval(x, vec, ...)
        else {
            length(vec) - findInterval(-x, rev(-vec), ...)
        }
    }
    if (ncol(Y) == 2) {
        temp <- min(Y[, 1], na.rm = TRUE)
        if (temp > 0) 
            zero <- 0
        else zero <- 2 * temp - 1
        Y <- cbind(zero, Y)
    }
    utime <- sort(unique(c(Y[, 1:2])))
    newtime <- matrix(findInterval(Y[, 1:2], utime), ncol = 2)
    status <- Y[, 3]
    newtime[status != 0, 2] <- newtime[status != 0, 2] - 0.2
    Gsurv <- survfit(Surv(newtime[, 1], newtime[, 2], last & 
        status == 0) ~ istrat, se.fit = FALSE)
    if (delay) 
        Hsurv <- survfit(Surv(-newtime[, 2], -newtime[, 1], first) ~ 
            istrat, se.fit = FALSE)
    status <- Y[, 3]
    stratfun <- function(i) {
        keep <- (istrat == i)
        times <- sort(unique(Y[keep & status == enum, 2]))
        if (length(times) == 0) 
            return(NULL)
        tdata <- mf[keep, -1, drop = FALSE]
        maxtime <- max(Y[keep, 2])
        if (dim(Gsurv) == 1) {
            if (delay) {
                dtime <- rev(-Hsurv$time[Hsurv$n.event > 0])
                dprob <- c(rev(Hsurv$surv[Hsurv$n.event > 0])[-1], 
                  1)
                ctime <- Gsurv$time[Gsurv$n.event > 0]
                cprob <- c(1, Gsurv$surv[Gsurv$n.event > 0])
                temp <- sort(unique(c(dtime, ctime)))
                index1 <- findInterval(temp, dtime)
                index2 <- findInterval(temp, ctime)
                ctime <- utime[temp]
                cprob <- dprob[index1] * cprob[index2 + 1]
            }
            else {
                ctime <- utime[Gsurv$time[Gsurv$n.event > 0]]
                cprob <- Gsurv$surv[Gsurv$n.event > 0]
            }
        }
        else {
            Gtemp <- Gsurv[i]
            if (delay) {
                Htemp <- Hsurv[i]
                dtime <- rev(-Htemp$time[Htemp$n.event > 0])
                dprob <- c(rev(Htemp$surv[Htemp$n.event > 0])[-1], 
                  1)
                ctime <- Gtemp$time[Gtemp$n.event > 0]
                cprob <- c(1, Gtemp$surv[Gtemp$n.event > 0])
                temp <- sort(unique(c(dtime, ctime)))
                index1 <- findInterval(temp, dtime)
                index2 <- findInterval(temp, ctime)
                ctime <- utime[temp]
                cprob <- dprob[index1] * cprob[index2 + 1]
            }
            else {
                ctime <- utime[Gtemp$time[Gtemp$n.event > 0]]
                cprob <- Gtemp$surv[Gtemp$n.event > 0]
            }
        }
        ct2 <- c(ctime, maxtime)
        cp2 <- c(1, cprob)
        index <- find2(times, ct2, left.open = TRUE)
        index <- sort(unique(index))
        ckeep <- rep(FALSE, length(ct2))
        ckeep[index] <- TRUE
        expand <- (Y[keep, 3] != 0 & Y[keep, 3] != enum & last[keep])
        split <- .Call(Cfinegray, Y[keep, 1], Y[keep, 2], ct2, 
            cp2, expand, c(TRUE, ckeep))
        tdata <- tdata[split$row, , drop = FALSE]
        tstat <- ifelse((status[keep])[split$row] == enum, 1, 
            0)
        tdata[[oname[1]]] <- split$start
        tdata[[oname[2]]] <- split$end
        tdata[[oname[3]]] <- tstat
        tdata[[oname[4]]] <- split$wt * user.weights[split$row]
        if (!is.null(count)) 
            tdata[[count]] <- split$add
        tdata
    }
    if (max(istrat) == 1) 
        result <- stratfun(1)
    else {
        tlist <- lapply(1:max(istrat), stratfun)
        result <- do.call("rbind", tlist)
    }
    rownames(result) <- NULL
    attr(result, "event") <- states[enum]
    result
}


frailty <- function (x, distribution = "gamma", ...) 
{
    dlist <- c("gamma", "gaussian", "t")
    i <- pmatch(distribution, dlist)
    if (!is.na(i)) 
        distribution <- dlist[i]
    temp <- paste("frailty", distribution, sep = ".")
    if (!exists(temp)) 
        stop(paste("Function '", temp, "' not found", sep = ""))
    (get(temp))(x, ...)
}


coxph.control <- function (eps = 1e-09, toler.chol = .Machine$double.eps^0.75, 
    iter.max = 20, toler.inf = sqrt(eps), outer.max = 10, timefix = TRUE) 
{
    if (iter.max < 0) 
        stop("Invalid value for iterations")
    if (eps <= 0) 
        stop("Invalid convergence criteria")
    if (eps <= toler.chol) 
        warning("For numerical accuracy, tolerance should be < eps")
    if (toler.inf <= 0) 
        stop("The inf.warn setting must be >0")
    if (!is.logical(timefix)) 
        stop("timefix must be TRUE or FALSE")
    list(eps = eps, toler.chol = toler.chol, iter.max = as.integer(iter.max), 
        toler.inf = toler.inf, outer.max = as.integer(outer.max), 
        timefix = timefix)
}


survobrien <- function (formula, data, subset, na.action, transform) 
{
    Call <- match.call()
    if (missing(transform)) 
        transform <- function(x) {
            r <- rank(x, na.last = "keep")
            temp <- (r - 0.5)/length(r[!is.na(r)])
            log(temp/(1 - temp))
        }
    else if (length(transform(1:10)) != 10) 
        stop("Transform function must be 1 to 1")
    indx <- match(c("formula", "data", "subset", "na.action"), 
        names(Call), nomatch = 0)
    if (indx[1] == 0) 
        stop("A formula argument is required")
    temp <- Call[c(1, indx)]
    temp[[1L]] <- quote(stats::model.frame)
    special <- c("strata", "cluster", "tt")
    temp$formula <- if (missing(data)) 
        terms(formula, special)
    else terms(formula, special, data = data)
    m <- eval(temp, parent.frame())
    if (nrow(m) == 0) 
        stop("No (non-missing) observations")
    n <- nrow(m)
    Terms <- attr(m, "terms")
    y <- model.extract(m, "response")
    if (!inherits(y, "Surv")) 
        stop("Response must be a survival object")
    if (!attr(y, "type") %in% c("right", "counting")) 
        stop("Response must be right censored or (start, stop] data")
    cluster <- untangle.specials(Terms, "cluster")
    if (length(cluster$terms) > 0) {
        if (length(cluster$terms) > 1) 
            stop("Can have only 1 cluster term")
        idvar <- m[[cluster$vars]]
        Terms2 <- Terms[-cluster$terms]
    }
    else {
        idvar <- 1:n
        Terms2 <- Terms
    }
    if (length(attr(Terms, "specials")$strata)) {
        stemp <- untangle.specials(Terms2, "strata", 1)
        if (length(stemp$terms) > 0) 
            Terms2 <- Terms2[-stemp$terms]
        if (length(stemp$vars) == 1) 
            strata.keep <- m[[stemp$vars]]
        else strata.keep <- strata(m[, stemp$vars], shortlabel = TRUE)
    }
    else strata.keep <- NULL
    if (any(attr(Terms2, "order") > 1)) 
        stop("This function cannot deal with iteraction terms")
    myvars <- attr(Terms2, "term.labels")
    factors <- sapply(m[myvars], is.factor)
    protected <- sapply(m[myvars], function(x) inherits(x, "AsIs"))
    keepers <- factors | protected
    if (all(keepers)) 
        stop("No continuous variables to modify")
    if (ncol(y) == 3) {
        if (is.null(strata.keep)) {
            etime <- sort(unique(y[y[, 3] == 1, 2]))
            indx <- lapply(etime, function(x) which(y[, 1] < 
                x & y[, 2] >= x))
        }
        else {
            temp <- unique(data.frame(y[, 2], strata.keep)[y[, 
                3] == 1, ])
            etime <- temp[, 1]
            indx <- lapply(1:nrow(temp), function(x) which(y[, 
                1] < temp[x, 1] & y[, 2] >= temp[x, 1] & !strata.keep == 
                temp[x, 2]))
        }
    }
    else {
        if (is.null(strata.keep)) {
            etime <- sort(unique(y[y[, 2] == 1, 1]))
            indx <- lapply(etime, function(x) which(y[, 1] >= 
                x))
        }
        else {
            temp <- unique(data.frame(y[, 1], strata.keep)[y[, 
                2] == 1, ])
            etime <- temp[, 1]
            indx <- lapply(1:nrow(temp), function(x) which(y[, 
                2] >= temp[x, 1] & strata.keep == temp[x, 2]))
        }
    }
    indx2 <- unlist(indx)
    nrisk <- unlist(sapply(indx, length))
    if (ncol(y) == 3) {
        newdata <- list(y[indx2, 1], y[indx2, 2])
        newdata <- c(newdata, list(1L * (newdata[[2]] == rep(etime, 
            nrisk) & y[indx2, 3] == 1)))
    }
    else {
        newdata <- list(y[indx2, 1])
        newdata <- c(newdata, list(1L * (newdata[[1]] == rep(etime, 
            nrisk) & y[indx2, 2] == 1)))
    }
    names(newdata) <- dimnames(y)[[2]]
    if (any(keepers)) {
        temp <- lapply(myvars[keepers], function(x) all.vars(parse(text = x)))
        knames <- unlist(temp)
    }
    else knames <- NULL
    if (length(strata.keep)) {
        knames <- c(knames, unlist(lapply(names(m)[stemp$vars], 
            function(x) all.vars(parse(text = x)))))
    }
    if (length(knames)) 
        newdata <- c(newdata, lapply(data[knames], function(x) x[indx2]))
    if (length(cluster$vars) > 0) {
        clname <- all.vars(parse(text = names(m)[cluster$vars]))
        newdata <- c(newdata, lapply(data[clname], function(x) x[indx2]))
    }
    else newdata <- c(newdata, list(.id. = idvar[indx2]))
    tvars <- myvars[!keepers]
    newx <- lapply(m[tvars], function(z) unlist(lapply(indx, 
        function(x) transform(z[x]))))
    data.frame(c(newdata, newx, list(.strata. = rep(1:length(indx), 
        sapply(indx, length)))))
}


attrassign <- function (object, ...) 
UseMethod("attrassign")


cluster <- function (x) 
x


yates <- function (fit, term, population = c("data", "factorial", "sas"), 
    levels, test = c("global", "trend", "pairwise"), predict = "linear", 
    options, nsim = 200, method = c("direct", "sgtt")) 
{
    Call <- match.call()
    if (missing(fit)) 
        stop("a fit argument is required")
    Terms <- try(terms(fit), silent = TRUE)
    if (inherits(Terms, "try-error")) 
        stop("the fit does not have a terms structure")
    else Terms <- delete.response(Terms)
    Tatt <- attributes(Terms)
    Tatt$dataClasses <- Tatt$dataClasses[row.names(Tatt$factors)]
    if (is.list(predict) || is.function(predict)) {
        stop("user written prediction function are not yet supported")
    }
    else {
        indx <- match(c("fit", "predict", "options"), names(Call), 
            nomatch = 0)
        temp <- Call[c(1, indx)]
        temp[[1]] <- quote(yates_setup)
        mfun <- eval(temp, parent.frame())
    }
    if (is.null(mfun)) 
        predict <- "linear"
    mframe <- fit$model
    if (is.null(mframe)) 
        mframe <- model.frame(fit)
    Xold <- model.matrix(fit)
    if (is.null(fit$assign)) {
        xassign <- attr(Xold, "assign")
    }
    else xassign <- fit$assign
    nvar <- length(xassign)
    nterm <- length(Tatt$term.names)
    termname <- rownames(Tatt$factors)
    iscat <- sapply(Tatt$dataClasses, function(x) x %in% c("character", 
        "factor"))
    method <- match.arg(casefold(method), c("direct", "sgtt"))
    if (method == "sgtt" && missing(population)) 
        population <- "sas"
    if (is.character(population)) {
        population <- match.arg(tolower(population[1]), c("data", 
            "factorial", "sas", "empirical", "yates"))
        if (population == "empirical") 
            population <- "data"
        if (population == "yates") 
            population <- "factorial"
    }
    else {
        if (!inherits(population, "data.frame")) 
            stop("the population argument must be a data frame or character")
    }
    test <- match.arg(test)
    if (method == "sgtt" && (population != "sas" || predict != 
        "linear")) 
        stop("sgtt method only applies if population = sas and predict = linear")
    beta <- coef(fit, complete = TRUE)
    nabeta <- is.na(beta)
    vmat <- vcov(fit, complete = FALSE)
    if (nrow(vmat) > sum(!nabeta)) {
        vmat <- vmat[!nabeta, !nabeta]
    }
    if (class(fit)[1] == "lm") 
        sigma <- summary(fit)$sigma
    else sigma <- NULL
    if (missing(levels)) 
        contr <- cmatrix(fit, term, test, assign = xassign)
    else contr <- cmatrix(fit, term, test, assign = xassign, 
        levels = levels)
    x1data <- as.data.frame(contr$levels)
    xmatlist <- yates_xmat(Terms, Tatt, contr, population, mframe, 
        fit, iscat)
    if (any(is.na(beta)) && population != "none") {
        if (inherits(fit, "coxph")) 
            X.qr <- qr(rbind(1, t(Xold)))
        else X.qr <- qr(t(Xold))
        estimcheck <- function(x, eps = sqrt(.Machine$double.eps)) {
            temp <- abs(qr.resid(X.qr, t(x)))
            all(temp < eps)
        }
        estimable <- sapply(xmatlist, estimcheck)
    }
    else estimable <- rep(TRUE, length(xmatlist))
    beta <- beta[!nabeta]
    if (predict == "linear" || is.null(mfun)) {
        if (is.na(match(contr$termname, colnames(Tatt$factors)))) 
            stop("term '", contr$termname, "' not found in the model")
        Cmat <- t(sapply(xmatlist, colMeans))[, !nabeta]
        if (inherits(fit, "coxph")) {
            Cmat <- Cmat[, -1, drop = FALSE]
            offset <- -sum(fit$means[!nabeta] * beta)
        }
        else offset <- 0
        estimate <- cbind(x1data, pmm = NA, std = NA)
        if (any(estimable)) {
            etemp <- estfun(Cmat[estimable, , drop = FALSE], 
                beta, vmat)
            estimate$pmm[estimable] <- etemp$estimate + offset
            estimate$std[estimable] <- sqrt(diag(etemp$var))
        }
        if (method == "sgtt") {
            temp <- sapply(fit$contrasts, function(x) (is.character(x) && 
                x %in% c("contr.SAS", "contr.treatment")))
            if (!all(temp)) 
                stop("yates sgtt method can only handle contr.SAS or contr.treatment")
            temp <- vector("list", length(fit$xlevels))
            names(temp) <- names(fit$xlevels)
            for (i in 1:length(fit$xlevels)) {
                cmat <- diag(length(fit$xlevels[[i]]))
                dimnames(cmat) <- list(fit$xlevels[[i]], fit$xlevels[[i]])
                if (i > 1 || Tatt$intercept == 1) {
                  if (fit$contrasts[[i]] == "contr.treatment") 
                    cmat <- cmat[, c(2:ncol(cmat), 1)]
                }
                temp[[i]] <- cmat
            }
            sasX <- model.matrix(formula(fit), data = mframe, 
                xlev = fit$xlevels, contrasts.arg = temp)
            sas.assign <- attr(sasX, "assign")
            D <- coef(lm(sasX ~ I(sasX) - 1))
            dimnames(D)[[1]] <- dimnames(D)[[2]]
            zero <- is.na(D[, 1])
            D <- ifelse(is.na(D), 0, D)
            if (!all(iscat)) {
                tcat <- (colSums(Tatt$factors[!iscat, , drop = FALSE]) == 
                  0)
            }
            else tcat <- rep(TRUE, max(sas.assign))
            B <- t(D)
            dimnames(B)[[2]] <- paste0("L", 1:ncol(B))
            if (ncol(Tatt$factors) > 1) {
                share <- t(Tatt$factors) %*% Tatt$factors
                nc <- ncol(share)
                for (i in which(tcat[-nc])) {
                  j <- which(share[i, ] > 0 & tcat)
                  k <- j[j > i]
                  if (length(k)) {
                    indx1 <- which(sas.assign == i)
                    indx2 <- which(sas.assign %in% k)
                    B[, indx1] <- resid(lm(B[, indx1] ~ B[, indx2]))
                  }
                }
            }
            Smat <- t(B)[!zero, !zero]
            Sassign <- xassign[!nabeta]
            keep <- match(contr$termname, colnames(Tatt$factors))
            if (length(keep) > 1) {
                test <- t(sapply(keep, function(i) testfun(Smat[Sassign == 
                  i, , drop = FALSE], beta, vmat, sigma^2)))
                rownames(test) <- contr$termname
            }
            else {
                test <- testfun(Smat[Sassign == keep, , drop = FALSE], 
                  beta, vmat, sigma^2)
                test <- matrix(test, nrow = 1, dimnames = list(contr$termname, 
                  names(test)))
            }
        }
        else {
            if (is.list(contr$cmat)) {
                test <- t(sapply(contr$cmat, function(x) testfun(x %*% 
                  Cmat, beta, vmat, sigma^2)))
                natest <- sapply(contr$cmat, nafun, estimate$pmm)
            }
            else {
                test <- testfun(contr$cmat %*% Cmat, beta, vmat, 
                  sigma^2)
                test <- matrix(test, nrow = 1, dimnames = list(contr$termname, 
                  names(test)))
                natest <- nafun(contr$cmat, estimate$pmm)
            }
            if (any(natest)) 
                test[natest, ] <- NA
        }
        if (any(estimable)) {
            result <- list(estimate = estimate, test = test, 
                mvar = etemp$var, cmat = Cmat)
        }
        else result <- list(estimate = estimate, test = test, 
            mvar = NA)
        if (method == "sgtt") 
            result$SAS <- Smat
    }
    else {
        xall <- do.call(rbind, xmatlist)[, !nabeta, drop = FALSE]
        if (inherits(fit, "coxph")) {
            xall <- xall[, -1, drop = FALSE]
            eta <- xall %*% beta - sum(fit$means[!nabeta] * beta)
        }
        else eta <- xall %*% beta
        n1 <- nrow(xmatlist[[1]])
        index <- rep(1:length(xmatlist), each = n1)
        if (is.function(mfun)) 
            predfun <- mfun
        else {
            if (!is.list(mfun) || any(is.na(match(c("predict", 
                "summary"), names(mfun)))) || !is.function(mfun$predic) || 
                !is.function(mfun$summary)) 
                stop("the prediction should be a function, or a list with two functions")
            predfun <- mfun$predict
            sumfun <- mfun$summary
        }
        pmm <- predfun(eta)
        n2 <- length(eta)
        if (!(is.numeric(pmm)) || !(length(pmm) == n2 || nrow(pmm) == 
            n2)) 
            stop("prediction function should return a vector or matrix")
        pmm <- rowsum(pmm, index, reorder = FALSE)/n1
        pmm[!estimable, ] <- NA
        tol <- sqrt(.Machine$double.eps)
        if (!isSymmetric(vmat, tol = tol, check.attributes = FALSE)) 
            stop("variance matrix of the coefficients is not symmetric")
        ev <- eigen(vmat, symmetric = TRUE)
        if (!all(ev$values >= -tol * abs(ev$values[1]))) 
            warning("variance matrix is numerically not positive definite")
        Rmat <- t(ev$vectors %*% (t(ev$vectors) * sqrt(ev$values)))
        bmat <- matrix(rnorm(nsim * ncol(vmat)), nrow = nsim) %*% 
            Rmat
        bmat <- bmat + rep(beta, each = nsim)
        sims <- array(0, dim = c(nsim, nrow(pmm), ncol(pmm)))
        if (inherits(fit, "coxph")) 
            offset <- bmat %*% fit$means[!nabeta]
        else offset <- rep(0, nsim)
        for (i in 1:nsim) sims[i, , ] <- rowsum(predfun(xall %*% 
            bmat[i, ] - offset[i]), index, reorder = FALSE)/n1
        mvar <- var(sims[, , 1])
        estimate <- cbind(x1data, pmm = unname(pmm[, 1]), std = sqrt(diag(mvar)))
        if (is.list(contr$cmat)) {
            test <- t(sapply(contr$cmat, function(x) testfun(x, 
                pmm[, 1], mvar[estimable, estimable], NULL)))
            natest <- sapply(contr$cmat, nafun, pmm[, 1])
        }
        else {
            test <- testfun(contr$cmat, pmm[, 1], mvar[estimable, 
                estimable], NULL)
            test <- matrix(test, nrow = 1, dimnames = list(contr$termname, 
                names(test)))
            natest <- nafun(contr$cmat, pmm[, 1])
        }
        if (any(natest)) 
            test[natest, ] <- NA
        if (any(estimable)) 
            result <- list(estimate = estimate, test = test, 
                mvar = mvar)
        else result <- list(estimate = estimate, test = test, 
            mvar = NA)
        if (ncol(pmm) > 1 && any(estimable)) {
            pmm <- apply(sims, 2:3, mean)
            mvar2 <- apply(sims, 2:3, var)
            if (is.list(mfun)) 
                result$summary <- sumfun(pmm, mvar2)
            else {
                result$pmm <- pmm
                result$mvar2 <- mvar2
            }
        }
    }
    result$call <- Call
    class(result) <- "yates"
    result
}


untangle.specials <- function (tt, special, order = 1) 
{
    spc <- attr(tt, "specials")[[special]]
    if (length(spc) == 0) 
        return(list(vars = character(0), terms = numeric(0)))
    facs <- attr(tt, "factors")
    fname <- dimnames(facs)
    ff <- apply(facs[spc, , drop = FALSE], 2, sum)
    list(vars = (fname[[1]])[spc], terms = seq(ff)[ff & match(attr(tt, 
        "order"), order, nomatch = 0)])
}


survreg.distributions <- list(extreme = list(name = "Extreme value", variance = function (parm) 
pi^2/6, init = function (x, weights, ...) 
{
    mean <- sum(x * weights)/sum(weights)
    var <- sum(weights * (x - mean)^2)/sum(weights)
    c(mean + 0.572, var/1.64)
}, deviance = function (y, scale, parms) 
{
    status <- y[, ncol(y)]
    width <- ifelse(status == 3, (y[, 2] - y[, 1])/scale, 1)
    temp <- width/(exp(width) - 1)
    center <- ifelse(status == 3, y[, 1] - log(temp), y[, 1])
    temp3 <- (-temp) + log(1 - exp(-exp(width)))
    loglik <- ifelse(status == 1, -(1 + log(scale)), ifelse(status == 
        3, temp3, 0))
    list(center = center, loglik = loglik)
}, density = function (x, parms) 
{
    w <- exp(x)
    ww <- exp(-w)
    cbind(1 - ww, ww, w * ww, (1 - w), w * (w - 3) + 1)
}, quantile = function (p, parms) 
log(-log(1 - p))), logistic = list(name = "Logistic", variance = function (parm) 
pi^2/3, init = function (x, weights, ...) 
{
    mean <- sum(x * weights)/sum(weights)
    var <- sum(weights * (x - mean)^2)/sum(weights)
    c(mean, var/3.2)
}, deviance = function (y, scale, parms) 
{
    status <- y[, ncol(y)]
    width <- ifelse(status == 3, (y[, 2] - y[, 1])/scale, 0)
    center <- ifelse(status == 3, rowMeans(y), y[, 1])
    temp2 <- ifelse(status == 3, exp(width/2), 2)
    temp3 <- log((temp2 - 1)/(temp2 + 1))
    loglik <- ifelse(status == 1, -log(4 * scale), ifelse(status == 
        3, temp3, 0))
    list(center = center, loglik = loglik)
}, density = function (x, parms) 
{
    w <- exp(x)
    cbind(w/(1 + w), 1/(1 + w), w/(1 + w)^2, (1 - w)/(1 + w), 
        (w * (w - 4) + 1)/(1 + w)^2)
}, quantile = function (p, parms) 
log(p/(1 - p))), gaussian = list(name = "Gaussian", variance = function (parm) 
1, init = function (x, weights, ...) 
{
    mean <- sum(x * weights)/sum(weights)
    var <- sum(weights * (x - mean)^2)/sum(weights)
    c(mean, var)
}, deviance = function (y, scale, parms) 
{
    status <- y[, ncol(y)]
    width <- ifelse(status == 3, (y[, 2] - y[, 1])/scale, 0)
    center <- ifelse(status == 3, rowMeans(y), y[, 1])
    temp2 <- log(1 - 2 * pnorm(width/2))
    loglik <- ifelse(status == 1, -log(sqrt(2 * pi) * scale), 
        ifelse(status == 3, temp2, 0))
    list(center = center, loglik = loglik)
}, density = function (x, parms) 
{
    cbind(pnorm(x), pnorm(-x), dnorm(x), -x, x^2 - 1)
}, quantile = function (p, parms) 
qnorm(p)), weibull = list(name = "Weibull", dist = "extreme", 
    trans = function (y) 
    log(y), dtrans = function (y) 
    1/y, itrans = function (x) 
    exp(x)), exponential = list(name = "Exponential", dist = "extreme", 
    trans = function (y) 
    log(y), dtrans = function (y) 
    1/y, scale = 1, itrans = function (x) 
    exp(x)), rayleigh = list(name = "Rayleigh", dist = "extreme", 
    trans = function (y) 
    log(y), dtrans = function (y) 
    1/y, itrans = function (x) 
    exp(x), scale = 0.5), loggaussian = list(name = "Log Normal", 
    dist = "gaussian", trans = function (y) 
    log(y), itrans = function (x) 
    exp(x), dtrans = function (y) 
    1/y), lognormal = list(name = "Log Normal", dist = "gaussian", 
    trans = function (y) 
    log(y), itrans = function (x) 
    exp(x), dtrans = function (y) 
    1/y), loglogistic = list(name = "Log logistic", dist = "logistic", 
    trans = function (y) 
    log(y), dtrans = function (y) 
    1/y, itrans = function (x) 
    exp(x)), t = list(name = "Student-t", variance = function (df) 
df/(df - 2), parms = c(df = 4), init = function (x, weights, 
    df) 
{
    if (df <= 2) 
        stop("Degrees of freedom must be >=3")
    mean <- sum(x * weights)/sum(weights)
    var <- sum(weights * (x - mean)^2)/sum(weights)
    c(mean, var * (df - 2)/df)
}, deviance = function (y, scale, parms) 
{
    status <- y[, ncol(y)]
    width <- ifelse(status == 3, (y[, 2] - y[, 1])/scale, 0)
    center <- ifelse(status == 3, rowMeans(y), y[, 1])
    temp2 <- log(1 - 2 * pt(width/2, df = parms))
    loglik <- ifelse(status == 1, -log(dt(0, df = parms) * scale), 
        ifelse(status == 3, temp2, 0))
    list(center = center, loglik = loglik)
}, density = function (x, df) 
{
    cbind(pt(x, df), pt(-x, df), dt(x, df), -(df + 1) * x/(df + 
        x^2), (df + 1) * (x^2 * (df + 3)/(df + x^2) - 1)/(df + 
        x^2))
}, quantile = function (p, df) 
qt(p, df)))


survConcordance.fit <- function (y, x, strata, weight) 
{
    if (any(is.na(x)) || any(is.na(y))) 
        return(NULL)
    btree <- function(n) {
        ranks <- rep(0L, n)
        yet.to.do <- 1:n
        depth <- floor(logb(n, 2))
        start <- as.integer(2^depth)
        lastrow.length <- 1 + n - start
        indx <- seq(1L, by = 2L, length = lastrow.length)
        ranks[yet.to.do[indx]] <- start + 0:(length(indx) - 1L)
        yet.to.do <- yet.to.do[-indx]
        while (start > 1) {
            start <- as.integer(start/2)
            indx <- seq(1L, by = 2L, length = start)
            ranks[yet.to.do[indx]] <- start + 0:(start - 1L)
            yet.to.do <- yet.to.do[-indx]
        }
        ranks
    }
    docount <- function(stime, risk, wts) {
        if (attr(stime, "type") == "right") {
            ord <- order(stime[, 1], -stime[, 2])
            ux <- sort(unique(risk))
            n2 <- length(ux)
            index <- btree(n2)[match(risk[ord], ux)] - 1L
            .Call(Cconcordance1, stime[ord, ], as.double(wts[ord]), 
                as.integer(index), as.integer(length(ux)))
        }
        else if (attr(stime, "type") == "counting") {
            sort.stop <- order(-stime[, 2], stime[, 3])
            sort.start <- order(-stime[, 1])
            ux <- sort(unique(risk))
            n2 <- length(ux)
            index <- btree(n2)[match(risk, ux)] - 1L
            .Call(Cconcordance2, stime, as.double(wts), as.integer(index), 
                as.integer(length(ux)), as.integer(sort.stop - 
                  1L), as.integer(sort.start - 1L))
        }
        else stop("Invalid survival type for concordance")
    }
    if (missing(weight) || length(weight) == 0) 
        weight <- rep(1, length(x))
    storage.mode(y) <- "double"
    if (missing(strata) || length(strata) == 0) {
        count <- docount(y, x, weight)
        if (count[1] == 0 && count[2] == 0) 
            count[5] <- 0
        else count[5] <- 2 * sqrt(count[5])
        names(count) <- c("concordant", "discordant", "tied.risk", 
            "tied.time", "std(c-d)")
    }
    else {
        strata <- as.factor(strata)
        ustrat <- levels(strata)[table(strata) > 0]
        count <- matrix(0, nrow = length(ustrat), ncol = 5)
        for (i in 1:length(ustrat)) {
            keep <- which(strata == ustrat[i])
            count[i, ] <- docount(y[keep, , drop = F], x[keep], 
                weight[keep])
        }
        count[, 5] <- 2 * sqrt(ifelse(count[, 1] + count[, 2] == 
            0, 0, count[, 5]))
        dimnames(count) <- list(ustrat, c("concordant", "discordant", 
            "tied.risk", "tied.time", "std(c-d)"))
    }
    count
}


survfitcoxph.fit <- function (y, x, wt, x2, risk, newrisk, strata, se.fit, survtype, 
    vartype, varmat, id, y2, strata2, unlist = TRUE) 
{
    if (is.factor(strata)) 
        ustrata <- levels(strata)
    else ustrata <- sort(unique(strata))
    nstrata <- length(ustrata)
    survlist <- vector("list", nstrata)
    names(survlist) <- ustrata
    for (i in 1:nstrata) {
        indx <- which(strata == ustrata[i])
        survlist[[i]] <- agsurv(y[indx, , drop = F], x[indx, 
            , drop = F], wt[indx], risk[indx], survtype, vartype)
    }
    expand <- function(fit, x2, varmat, se.fit) {
        if (survtype == 1) 
            surv <- cumprod(fit$surv)
        else surv <- exp(-fit$cumhaz)
        if (is.matrix(x2) && nrow(x2) > 1) {
            fit$surv <- outer(surv, newrisk, "^")
            dimnames(fit$surv) <- list(NULL, row.names(x2))
            if (se.fit) {
                varh <- matrix(0, nrow = length(fit$varhaz), 
                  ncol = nrow(x2))
                for (i in 1:nrow(x2)) {
                  dt <- outer(fit$cumhaz, x2[i, ], "*") - fit$xbar
                  varh[, i] <- (cumsum(fit$varhaz) + rowSums((dt %*% 
                    varmat) * dt)) * newrisk[i]^2
                }
                fit$std.err <- sqrt(varh)
            }
            fit$cumhaz <- outer(fit$cumhaz, newrisk, "*")
        }
        else {
            fit$surv <- surv^newrisk
            if (se.fit) {
                dt <- outer(fit$cumhaz, c(x2)) - fit$xbar
                varh <- (cumsum(fit$varhaz) + rowSums((dt %*% 
                  varmat) * dt)) * newrisk^2
                fit$std.err <- sqrt(varh)
            }
            fit$cumhaz <- fit$cumhaz * newrisk
        }
        fit
    }
    if (missing(id) || is.null(id)) 
        result <- lapply(survlist, expand, x2, varmat, se.fit)
    else {
        onecurve <- function(slist, x2, y2, strata2, newrisk, 
            se.fit) {
            ntarget <- nrow(x2)
            surv <- vector("list", ntarget)
            n.event <- n.risk <- n.censor <- varh1 <- varh2 <- time <- surv
            hazard <- vector("list", ntarget)
            stemp <- as.integer(strata2)
            timeforward <- 0
            for (i in 1:ntarget) {
                slist <- survlist[[stemp[i]]]
                indx <- which(slist$time > y2[i, 1] & slist$time <= 
                  y2[i, 2])
                if (length(indx) == 0) {
                  timeforward <- timeforward + y2[i, 2] - y2[i, 
                    1]
                }
                else {
                  time[[i]] <- diff(c(y2[i, 1], slist$time[indx]))
                  time[[i]][1] <- time[[i]][1] + timeforward
                  timeforward <- y2[i, 2] - max(slist$time[indx])
                  hazard[[i]] <- slist$hazard[indx] * newrisk[i]
                  if (survtype == 1) 
                    surv[[i]] <- slist$surv[indx]^newrisk[i]
                  n.event[[i]] <- slist$n.event[indx]
                  n.risk[[i]] <- slist$n.risk[indx]
                  n.censor[[i]] <- slist$n.censor[indx]
                  dt <- outer(slist$cumhaz[indx], x2[i, ]) - 
                    slist$xbar[indx, , drop = F]
                  varh1[[i]] <- slist$varhaz[indx] * newrisk[i]^2
                  varh2[[i]] <- rowSums((dt %*% varmat) * dt) * 
                    newrisk[i]^2
                }
            }
            cumhaz <- cumsum(unlist(hazard))
            if (survtype == 1) 
                surv <- cumprod(unlist(surv))
            else surv <- exp(-cumhaz)
            if (se.fit) 
                list(n = as.vector(table(strata)[stemp[1]]), 
                  time = cumsum(unlist(time)), n.risk = unlist(n.risk), 
                  n.event = unlist(n.event), n.censor = unlist(n.censor), 
                  surv = surv, cumhaz = cumhaz, std.err = sqrt(cumsum(unlist(varh1)) + 
                    unlist(varh2)))
            else list(n = as.vector(table(strata)[stemp[1]]), 
                time = cumsum(unlist(time)), n.risk = unlist(n.risk), 
                n.event = unlist(n.event), n.censor = unlist(n.censor), 
                surv = surv, cumhaz = cumhaz)
        }
        if (all(id == id[1])) {
            result <- list(onecurve(survlist, x2, y2, strata2, 
                newrisk, se.fit))
        }
        else {
            uid <- unique(id)
            result <- vector("list", length = length(uid))
            for (i in 1:length(uid)) {
                indx <- which(id == uid[i])
                result[[i]] <- onecurve(survlist, x2[indx, , 
                  drop = FALSE], y2[indx, , drop = FALSE], strata2[indx], 
                  newrisk[indx], se.fit)
            }
            names(result) <- uid
        }
    }
    if (unlist) {
        if (length(result) == 1) {
            if (se.fit) 
                result[[1]][c("n", "time", "n.risk", "n.event", 
                  "n.censor", "surv", "cumhaz", "std.err")]
            else result[[1]][c("n", "time", "n.risk", "n.event", 
                "n.censor", "surv", "cumhaz")]
        }
        else {
            temp <- list(n = unlist(lapply(result, function(x) x$n), 
                use.names = FALSE), time = unlist(lapply(result, 
                function(x) x$time), use.names = FALSE), n.risk = unlist(lapply(result, 
                function(x) x$n.risk), use.names = FALSE), n.event = unlist(lapply(result, 
                function(x) x$n.event), use.names = FALSE), n.censor = unlist(lapply(result, 
                function(x) x$n.censor), use.names = FALSE), 
                strata = sapply(result, function(x) length(x$time)))
            names(temp$strata) <- names(result)
            if ((missing(id) || is.null(id)) && nrow(x2) > 1) {
                temp$surv <- t(matrix(unlist(lapply(result, function(x) t(x$surv)), 
                  use.names = FALSE), nrow = nrow(x2)))
                dimnames(temp$surv) <- list(NULL, row.names(x2))
                temp$cumhaz <- t(matrix(unlist(lapply(result, 
                  function(x) t(x$cumhaz)), use.names = FALSE), 
                  nrow = nrow(x2)))
                if (se.fit) 
                  temp$std.err <- t(matrix(unlist(lapply(result, 
                    function(x) t(x$std.err)), use.names = FALSE), 
                    nrow = nrow(x2)))
            }
            else {
                temp$surv <- unlist(lapply(result, function(x) x$surv), 
                  use.names = FALSE)
                temp$cumhaz <- unlist(lapply(result, function(x) x$cumhaz), 
                  use.names = FALSE)
                if (se.fit) 
                  temp$std.err <- unlist(lapply(result, function(x) x$std.err), 
                    use.names = FALSE)
            }
            temp
        }
    }
    else {
        names(result) <- ustrata
        result
    }
}


ratetable <- function (...) 
{
    datecheck <- function(x) inherits(x, c("Date", "POSIXt", 
        "date", "chron"))
    args <- list(...)
    nargs <- length(args)
    ll <- sapply(args, length)
    n <- max(ll)
    levlist <- vector("list", nargs)
    x <- matrix(0, n, nargs)
    dimnames(x) <- list(1:n, names(args))
    isDate <- sapply(args, datecheck)
    for (i in 1:nargs) {
        if (ll[i] == 1) 
            args[[i]] <- rep(args[[i]], n)
        else if (ll[i] != n) 
            stop(paste("Aguments do not all have the same length (arg ", 
                i, ")", sep = ""))
        if (inherits(args[[i]], "cateogory") || is.character(args[[i]])) 
            args[[i]] <- as.factor(args[[i]])
        if (is.factor(args[[i]])) {
            levlist[[i]] <- levels(args[[i]])
            x[, i] <- as.numeric(args[[i]])
        }
        else x[, i] <- ratetableDate(args[[i]])
    }
    attr(x, "isDate") <- isDate
    attr(x, "levlist") <- levlist
    class(x) <- "ratetable2"
    x
}


concordance <- function (object, ...) 
UseMethod("concordance")


survreg <- function (formula, data, weights, subset, na.action, dist = "weibull", 
    init = NULL, scale = 0, control, parms = NULL, model = FALSE, 
    x = FALSE, y = TRUE, robust = FALSE, score = FALSE, ...) 
{
    Call <- match.call()
    indx <- match(c("formula", "data", "weights", "subset", "na.action"), 
        names(Call), nomatch = 0)
    if (indx[1] == 0) 
        stop("A formula argument is required")
    temp <- Call[c(1, indx)]
    temp[[1L]] <- quote(stats::model.frame)
    special <- c("strata", "cluster")
    temp$formula <- if (missing(data)) 
        terms(formula, special)
    else terms(formula, special, data = data)
    m <- eval(temp, parent.frame())
    Terms <- attr(m, "terms")
    weights <- model.extract(m, "weights")
    Y <- model.extract(m, "response")
    if (!inherits(Y, "Surv")) 
        stop("Response must be a survival object")
    type <- attr(Y, "type")
    if (type == "counting") 
        stop("start-stop type Surv objects are not supported")
    if (type == "mright" || type == "mcounting") 
        stop("multi-state survival is not supported")
    strats <- attr(Terms, "specials")$strata
    cluster <- attr(Terms, "specials")$cluster
    dropx <- NULL
    if (length(cluster)) {
        if (missing(robust)) 
            robust <- TRUE
        tempc <- untangle.specials(Terms, "cluster", 1:10)
        ord <- attr(Terms, "order")[tempc$terms]
        if (any(ord > 1)) 
            stop("Cluster can not be used in an interaction")
        cluster <- strata(m[, tempc$vars], shortlabel = TRUE)
        dropx <- tempc$terms
    }
    if (length(strats)) {
        temp <- untangle.specials(Terms, "strata", 1)
        dropx <- c(dropx, temp$terms)
        if (length(temp$vars) == 1) 
            strata.keep <- m[[temp$vars]]
        else strata.keep <- strata(m[, temp$vars], shortlabel = TRUE)
        strata <- as.numeric(strata.keep)
        nstrata <- max(strata)
    }
    else {
        nstrata <- 1
        strata <- 0
    }
    if (length(dropx)) {
        newTerms <- Terms[-dropx]
        attr(newTerms, "intercept") <- attr(Terms, "intercept")
    }
    else newTerms <- Terms
    X <- model.matrix(newTerms, m)
    assign <- lapply(attrassign(X, newTerms)[-1], function(x) x - 
        1)
    xlevels <- .getXlevels(newTerms, m)
    contr.save <- attr(X, "contrasts")
    if (!all(is.finite(X))) 
        stop("data contains an infinite predictor")
    n <- nrow(X)
    nvar <- ncol(X)
    offset <- model.offset(m)
    if (length(offset) == 0 || all(offset == 0)) 
        offset <- rep(0, n)
    if (is.character(dist)) {
        dist <- match.arg(dist, names(survreg.distributions))
        dlist <- survreg.distributions[[dist]]
        if (is.null(dlist)) 
            stop(paste(dist, ": distribution not found"))
    }
    else if (is.list(dist)) 
        dlist <- dist
    else stop("Invalid distribution object")
    if (!survregDtest(dlist)) 
        stop("Invalid distribution object")
    logcorrect <- 0
    Ysave <- Y
    if (!is.null(dlist$trans)) {
        tranfun <- dlist$trans
        exactsurv <- Y[, ncol(Y)] == 1
        if (any(exactsurv)) {
            if (is.null(weights)) 
                logcorrect <- sum(log(dlist$dtrans(Y[exactsurv, 
                  1])))
            else logcorrect <- sum(weights[exactsurv] * log(dlist$dtrans(Y[exactsurv, 
                1])))
        }
        if (type == "interval") {
            if (any(Y[, 3] == 3)) 
                Y <- cbind(tranfun(Y[, 1:2]), Y[, 3])
            else Y <- cbind(tranfun(Y[, 1]), Y[, 3])
        }
        else if (type == "left") 
            Y <- cbind(tranfun(Y[, 1]), 2 - Y[, 2])
        else Y <- cbind(tranfun(Y[, 1]), Y[, 2])
        if (!all(is.finite(Y))) 
            stop("Invalid survival times for this distribution")
    }
    else {
        if (type == "left") 
            Y[, 2] <- 2 - Y[, 2]
        else if (type == "interval" && all(Y[, 3] < 3)) 
            Y <- Y[, c(1, 3)]
    }
    if (!is.null(dlist$scale)) {
        if (!missing(scale)) 
            warning(paste(dlist$name, "has a fixed scale, user specified value ignored"))
        scale <- dlist$scale
    }
    if (!is.null(dlist$dist)) 
        if (is.atomic(dlist$dist)) 
            dlist <- survreg.distributions[[dlist$dist]]
        else dlist <- dlist$dist
    ptemp <- dlist$parms
    if (is.null(ptemp)) {
        if (!is.null(parms)) 
            stop(paste(dlist$name, "distribution has no optional parameters"))
    }
    else {
        if (!is.numeric(ptemp)) 
            stop("Default parameters must be a numeric vector")
        if (!missing(parms)) {
            temp <- unlist(parms)
            indx <- match(names(temp), names(ptemp))
            if (any(is.na(indx))) 
                stop("Invalid parameter names")
            ptemp[names(ptemp)] <- temp
        }
        parms <- ptemp
    }
    if (missing(control)) 
        control <- survreg.control(...)
    else control <- do.call("survreg.control", control)
    if (any(scale < 0)) 
        stop("Invalid scale value")
    if (any(scale > 0) && nstrata > 1) 
        stop("The scale argument is not valid with multiple strata")
    pterms <- sapply(m, inherits, "coxph.penalty")
    if (any(pterms)) {
        pattr <- lapply(m[pterms], attributes)
        temp <- c(attr(Terms, "response"), attr(Terms, "offset"))
        if (length(dropx)) 
            temp <- c(temp, dropx + 1)
        pterms <- pterms[-temp]
        temp <- match((names(pterms))[pterms], attr(Terms, "term.labels"))
        ord <- attr(Terms, "order")[temp]
        if (any(ord > 1)) 
            stop("Penalty terms cannot be in an interaction")
        assign <- attrassign(X, newTerms)
        pcols <- assign[match(names(pterms[pterms]), names(assign))]
        fit <- survpenal.fit(X, Y, weights, offset, init = init, 
            controlvals = control, dist = dlist, scale = scale, 
            strata = strata, nstrat = nstrata, pcols, pattr, 
            parms = parms, assign)
    }
    else fit <- survreg.fit(X, Y, weights, offset, init = init, 
        controlvals = control, dist = dlist, scale = scale, nstrat = nstrata, 
        strata, parms = parms)
    if (is.character(fit)) 
        fit <- list(fail = fit)
    else {
        if (scale == 0) {
            nvar <- length(fit$coefficients) - nstrata
            fit$scale <- exp(fit$coefficients[-(1:nvar)])
            if (nstrata == 1) 
                names(fit$scale) <- NULL
            else names(fit$scale) <- levels(strata.keep)
            fit$coefficients <- fit$coefficients[1:nvar]
            fit$idf <- 1 + nstrata
        }
        else {
            fit$scale <- scale
            fit$idf <- 1
        }
        fit$loglik <- fit$loglik + logcorrect
    }
    if (!score) 
        fit$score <- NULL
    fit$df.residual <- n - sum(fit$df)
    fit$terms <- Terms
    fit$contrasts <- contr.save
    if (length(xlevels)) 
        fit$xlevels <- xlevels
    fit$means <- apply(X, 2, mean)
    if (!is.null(weights)) 
        fit$weights <- weights
    fit$call <- Call
    fit$dist <- dist
    if (model) 
        fit$model <- m
    if (x) 
        fit$x <- X
    if (y) 
        fit$y <- Ysave
    if (length(parms)) 
        fit$parms <- parms
    if (robust) {
        fit$naive.var <- fit$var
        if (!model) 
            fit$model <- m
        if (length(cluster)) 
            fit$var <- crossprod(rowsum(residuals.survreg(fit, 
                "dfbeta"), cluster))
        else fit$var <- crossprod(residuals.survreg(fit, "dfbeta"))
        if (!model) 
            fit$model <- NULL
    }
    singular <- (diag(fit$var) == 0)[1:length(fit$coef)]
    if (any(singular)) 
        fit$coef <- ifelse(singular, NA, fit$coef)
    na.action <- attr(m, "na.action")
    if (length(na.action)) 
        fit$na.action <- na.action
    if (any(pterms)) 
        class(fit) <- c("survreg.penal", "survreg")
    else class(fit) <- "survreg"
    fit
}


rsurvreg <- function (n, mean, scale = 1, distribution = "weibull", parms) 
{
    if (missing(parms)) 
        qsurvreg(runif(n), mean, scale, distribution)
    else qsurvreg(runif(n), mean, scale, distribution, parms)
}


dsurvreg <- function (x, mean, scale = 1, distribution = "weibull", parms) 
{
    dist <- survreg.distributions[[casefold(distribution)]]
    if (is.null(dist)) 
        stop("Distribution not found")
    if (!is.null(dist$trans)) {
        dx <- dist$dtrans(x)
        x <- dist$trans(x)
        x <- (x - mean)/scale
        dist <- survreg.distributions[[dist$dist]]
        y <- dist$density(x, parms)[, 3]
        y * dx/scale
    }
    else {
        x <- (x - mean)/scale
        y <- dist$density(x, parms)[, 3]
        y/scale
    }
}


aareg <- function (formula, data, weights, subset, na.action, qrtol = 1e-07, 
    nmin, dfbeta = FALSE, taper = 1, test = c("aalen", "variance", 
        "nrisk"), model = FALSE, x = FALSE, y = FALSE) 
{
    call <- match.call()
    m <- match.call(expand.dots = FALSE)
    temp <- c("", "formula", "data", "weights", "subset", "na.action")
    m <- m[match(temp, names(m), nomatch = 0)]
    special <- c("strata", "cluster")
    Terms <- if (missing(data)) 
        terms(formula, special)
    else terms(formula, special, data = data)
    m$formula <- Terms
    m[[1L]] <- quote(stats::model.frame)
    m <- eval(m, sys.parent())
    test <- match.arg(test)
    Y <- model.extract(m, "response")
    if (!inherits(Y, "Surv")) 
        stop("Response must be a survival object")
    offset <- attr(Terms, "offset")
    tt <- length(offset)
    offset <- if (tt == 0) 
        rep(0, nrow(Y))
    else if (tt == 1) 
        m[[offset]]
    else {
        ff <- m[[offset[1]]]
        for (i in 2:tt) ff <- ff + m[[offset[i]]]
        ff
    }
    attr(Terms, "intercept") <- 1
    strats <- attr(Terms, "specials")$strata
    cluster <- attr(Terms, "specials")$cluster
    dropx <- NULL
    if (length(cluster)) {
        dfbeta <- TRUE
        tempc <- untangle.specials(Terms, "cluster", 1:10)
        ord <- attr(Terms, "order")[tempc$terms]
        if (any(ord > 1)) 
            stop("Cluster can not be used in an interaction")
        cluster <- strata(m[, tempc$vars], shortlabel = TRUE)
        cluster <- as.numeric(cluster)
        dropx <- tempc$terms
    }
    else cluster <- 1:nrow(m)
    if (length(strats)) {
        stop("Strata terms not allowed")
    }
    if (length(dropx)) 
        X <- model.matrix(Terms[-dropx], m)[, -1, drop = FALSE]
    else X <- model.matrix(Terms, m)[, -1, drop = FALSE]
    nvar <- ncol(X)
    nused <- nrow(X)
    weights <- model.extract(m, "weights")
    if (length(weights) == 0) 
        weights <- rep(1, nused)
    type <- attr(Y, "type")
    if (type != "right" && type != "counting") 
        stop(paste("Aalen model doesn't support \"", type, "\" survival data", 
            sep = ""))
    if (ncol(Y) == 2) {
        mintime <- min(Y[, 1])
        if (mintime < 0) 
            Y <- cbind(2 * mintime - 1, Y)
        else Y <- cbind(-1, Y)
    }
    times <- as.vector(Y[, 2])
    status <- as.vector(Y[, 3])
    ndeath <- length(unique(times[status == 1]))
    ord <- order(times, -status)
    times <- times[ord]
    status <- status[ord]
    weights <- weights[ord]
    if (x) 
        saveX <- X
    X <- X[ord, , drop = FALSE]
    storage.mode(Y) <- "double"
    ff <- .C(Ccoxdetail, as.integer(nused), as.integer(nvar), 
        ndeath = as.integer(ndeath), y = Y[ord, ], as.double(X), 
        index = as.integer(rep(0, nused)), event2 = rep(1, nused), 
        weights = as.double(weights), means = c(0, double(ndeath * 
            nvar - 1)), u = double(ndeath * nvar), i = double(ndeath * 
            nvar * nvar), rmat = integer(ndeath * nused), nrisk2 = double(ndeath), 
        double(nvar * (3 + 2 * nvar)))
    riskmat <- matrix(ff$rmat, nused, ndeath)
    dt <- list(means = (matrix(ff$means, ndeath, nvar)), var = aareg.taper(taper, 
        array(ff$i, c(nvar, nvar, ndeath)), ff$event2[1:ndeath]), 
        time = times[ff$index[1:ndeath]], nrisk = ff$nrisk2, 
        nevent = ff$event2[1:ndeath])
    if (missing(nmin)) 
        nmin <- 3 * nvar
    if (nvar == 1) 
        ndeath <- sum(dt$nrisk >= nmin & c(dt$var) > 0)
    else {
        ndeath <- sum(dt$nrisk >= nmin)
        if (ndeath > 0) {
            while (1) {
                qri <- qr(dt$var[, , ndeath], tol = qrtol)
                if (qri$rank >= nvar) 
                  break
                ndeath <- ndeath - 1
            }
        }
    }
    if (ndeath <= 1) 
        stop("The threshold 'nmin' is too high, no model can be fit")
    index <- match(times, dt$time[1:ndeath], nomatch = 0) * status
    deaths <- (status == 1 & index > 0)
    dindex <- index[deaths]
    nevent <- length(dindex)
    if (length(cluster)) 
        ncluster <- length(unique(cluster))
    else ncluster <- nused
    if (dfbeta) {
        dmat <- array(0, dim = c(ncluster, nvar + 1, ndeath))
        resid <- rep(0, nevent * nused)
        resid[nevent * ((1:nused)[deaths] - 1) + 1:nevent] <- 1
        resid <- matrix(resid, ncol = nused)
    }
    if (nvar == 1) {
        means <- dt$means[dindex]
        nrisk <- dt$nrisk[dindex]
        xx <- (X[deaths] - means) * weights[deaths]
        v.inverse <- 1/dt$var[dindex]
        twt <- nrisk * 1/cbind(1 + means^2 * v.inverse, v.inverse)
        coefficient <- v.inverse * xx/nrisk
        b0 <- weights[deaths]/nrisk - means * coefficient
        if (dfbeta) {
            xx <- c(X) * riskmat[, dindex]
            predicted <- coefficient * t(xx) + b0 * t(riskmat[, 
                dindex])
            resid <- resid - predicted
            temp1 <- (resid * (t(xx) - means)/(nrisk * dt$var[dindex])) * 
                rep(weights, rep(nevent, nused))
            temp0 <- resid * outer(1/nrisk, weights) - temp1 * 
                means
            if (test == "nrisk") {
                test.dfbeta <- cbind(apply(temp0 * nrisk, 2, 
                  sum), apply(temp1 * nrisk, 2, sum))
            }
            else {
                test.dfbeta <- cbind(apply(temp0 * twt[, 1], 
                  2, sum), apply(temp1 * twt[, 2], 2, sum))
            }
            if (nevent > ndeath) {
                temp1 <- rowsum(temp1, times[deaths], reorder = FALSE)
                temp0 <- rowsum(temp0, times[deaths], reorder = FALSE)
            }
            dmat[, 1, ] <- rowsum(t(temp0), cluster[ord], reorder = FALSE)
            dmat[, 2, ] <- rowsum(t(temp1), cluster[ord], reorder = FALSE)
        }
        coefficient <- cbind(b0, coefficient)
        if (test == "nrisk") {
            temp <- coefficient * nrisk
            test.statistic <- apply(temp, 2, sum)
            test.var <- matrix(0, 2, 2)
            diag(test.var) <- apply(temp^2, 2, sum)
            test.var[1, 2] <- test.var[2, 1] <- sum(temp[, 1] * 
                temp[, 2])
        }
        else {
            temp <- coefficient * twt
            test.statistic <- apply(temp, 2, sum)
            test.var <- matrix(0, 2, 2)
            diag(test.var) <- apply(temp^2, 2, sum)
            test.var[1, 2] <- test.var[2, 1] <- sum(temp[, 1] * 
                temp[, 2])
        }
    }
    else {
        coefficient <- matrix(0, nevent, nvar)
        twt <- matrix(0, nevent, nvar + 1)
        means <- dt$means[dindex, ]
        nrisk <- dt$nrisk[dindex]
        dindex2 <- (1:nused)[deaths]
        ybar <- weights[deaths]/nrisk
        test.var <- matrix(0, nvar, nvar)
        if (dfbeta) 
            test.dfbeta <- matrix(0, nused, nvar + 1)
        for (i in 1:nevent) {
            who <- riskmat[, dindex[i]]
            wt <- weights * who
            xx <- who * (X - rep(means[i, ], rep(nused, nvar)))
            if (i == 1 || dindex[i] != dindex[i - 1]) {
                qri <- qr(dt$var[, , dindex[i]], tol = qrtol)
                vmat <- qr.coef(qri, diag(nvar))
                twt[i, ] <- nrisk[i]/c(1 + means[i, ] %*% vmat %*% 
                  means[i, ], diag(vmat))
            }
            else twt[i, ] <- twt[i - 1, ]
            j <- dindex2[i]
            coefficient[i, ] <- qr.coef(qri, wt[j] * xx[j, ])/nrisk[i]
            if (test == "variance") {
                temp <- wt[j] * xx[j, ]
                test.var <- test.var + outer(temp, temp)
            }
            if (dfbeta) {
                resid[i, ] <- resid[i, ] - c(ybar[i] + xx %*% 
                  c(coefficient[i, ]))
                temp1 <- t(qr.coef(qri, t(resid[i, ] * wt * xx)))/nrisk[i]
                temp0 <- resid[i, ] * wt/nrisk[i] - temp1 %*% 
                  means[i, ]
                if (test == "aalen") 
                  test.dfbeta <- test.dfbeta + cbind(temp0, temp1) %*% 
                    diag(twt[i, ])
                else if (test == "nrisk") 
                  test.dfbeta <- test.dfbeta + cbind(temp0, temp1) * 
                    nrisk[i]
                else {
                  test.dfbeta[, -1] <- test.dfbeta[, -1] + resid[i, 
                    ] * wt * xx
                  test.dfbeta[, 1] <- test.dfbeta[, 1] + temp0 * 
                    twt[i, 1]
                }
                dmat[, -1, dindex[i]] <- dmat[, -1, dindex[i]] + 
                  rowsum(temp1, cluster[ord], reorder = FALSE)
                dmat[, 1, dindex[i]] <- dmat[, 1, dindex[i]] + 
                  rowsum(temp0, cluster[ord], reorder = FALSE)
            }
        }
        temp <- apply(means * coefficient, 1, sum)
        b0 <- weights[deaths]/nrisk - temp
        coefficient <- cbind(b0, coefficient)
        if (test == "aalen") {
            temp <- twt * coefficient
            test.statistic <- colSums(temp)
            test.var <- t(temp) %*% temp
        }
        else if (test == "nrisk") {
            temp <- coefficient * nrisk
            test.statistic <- colSums(temp)
            test.var <- t(temp) %*% temp
        }
        else {
            xx <- weights[deaths] * (X[deaths, ] - means[dindex, 
                ])
            test.statistic <- apply(xx, 2, sum)
        }
    }
    if (dfbeta) {
        temp <- rowsum(test.dfbeta, cluster, reorder = FALSE)
        test.var2 <- t(temp) %*% temp
    }
    dimnames(coefficient) <- list(times[deaths], c("Intercept", 
        dimnames(X)[[2]]))
    names(test.statistic) <- c("Intercept", dimnames(X)[[2]])
    dimnames(twt) <- NULL
    ans <- list(n = c(nused, ndeath, length(dt$time)), times = times[deaths], 
        nrisk = dt$nrisk[dindex], coefficient = coefficient, 
        test.statistic = test.statistic, test.var = test.var, 
        test = test, tweight = twt, call = call)
    if (dfbeta) {
        ans$dfbeta <- dmat
        ans$test.var2 <- test.var2
    }
    if (any(weights != 1)) 
        ans$weights <- weights
    na.action <- attr(m, "na.action")
    if (length(na.action)) 
        ans$na.action <- na.action
    if (model) 
        ans$model <- m
    else {
        if (x) 
            ans$x <- saveX
        if (y) 
            ans$y <- Y
    }
    class(ans) <- "aareg"
    ans
}


statefig <- function (layout, connect, margin = 0.03, box = TRUE, cex = 1, 
    col = 1, lwd = 1, lty = 1, bcol = col, acol = col, alwd = lwd, 
    alty = lty, offset = 0) 
{
    frame()
    par(usr = c(0, 1, 0, 1))
    if (!is.numeric(layout)) 
        stop("layout must be a numeric vector or matrix")
    if (!is.matrix(connect) || nrow(connect) != ncol(connect)) 
        stop("connect must be a square matrix")
    nstate <- nrow(connect)
    dd <- dimnames(connect)
    if (!is.null(dd[[1]])) 
        statenames <- dd[[1]]
    else if (is.null(dd[[2]])) 
        stop("connect must have the state names as dimnames")
    else statenames <- dd[[2]]
    narrow <- sum(connect != 0)
    acol <- rep(acol, length = narrow)
    alwd <- rep(alwd, length = narrow)
    alty <- rep(alty, length = narrow)
    bcol <- rep(bcol, length = nstate)
    lty <- rep(lty, length = nstate)
    lwd <- rep(lwd, length = nstate)
    col <- rep(col, length = nstate)
    if (is.matrix(layout) && ncol(layout) == 2 && nrow(layout) > 
        1) {
        if (any(layout < 0) || any(layout > 1)) 
            stop("layout coordinates must be between 0 and 1")
        if (nrow(layout) != nstate) 
            stop("layout matrix should have one row per state")
        cbox <- layout
    }
    else {
        if (any(layout <= 0 | layout != floor(layout))) 
            stop("non-integer number of states in layout argument")
        space <- function(n) (1:n - 0.5)/n
        if (sum(layout) != nstate) 
            stop("number of boxes != number of states")
        cbox <- matrix(0, ncol = 2, nrow = nstate)
        n <- length(layout)
        ix <- rep(seq(along = layout), layout)
        if (is.vector(layout) || ncol(layout) > 1) {
            cbox[, 1] <- space(n)[ix]
            for (i in 1:n) cbox[ix == i, 2] <- 1 - space(layout[i])
        }
        else {
            cbox[, 2] <- 1 - space(n)[ix]
            for (i in 1:n) cbox[ix == i, 1] <- space(layout[i])
        }
    }
    text(cbox[, 1], cbox[, 2], statenames, cex = cex, col = col)
    textwd <- strwidth(statenames, cex = cex)
    textht <- strheight(statenames, cex = cex)
    temp <- par("pin")
    dx <- margin * temp[2]/mean(temp)
    dy <- margin * temp[1]/mean(temp)
    if (box) {
        drawbox <- function(x, y, dx, dy, lwd, lty, col) {
            lines(x + c(-dx, dx, dx, -dx, -dx), y + c(-dy, -dy, 
                dy, dy, -dy), lwd = lwd, lty = lty, col = col)
        }
        for (i in 1:nstate) drawbox(cbox[i, 1], cbox[i, 2], textwd[i]/2 + 
            dx, textht[i]/2 + dy, col = bcol[i], lwd = lwd[i], 
            lty = lty[i])
        dx <- 2 * dx
        dy <- 2 * dy
    }
    arrow2 <- function(...) arrows(..., angle = 20, length = 0.1)
    doline <- function(x1, x2, d, delta1, delta2, lwd, lty, col) {
        if (d == 0 && x1[1] == x2[1]) {
            if (x1[2] > x2[2]) 
                arrow2(x1[1], x1[2] - delta1[2], x2[1], x2[2] + 
                  delta2[2], lwd = lwd, lty = lty, col = col)
            else arrow2(x1[1], x1[2] + delta1[2], x2[1], x2[2] - 
                delta2[2], lwd = lwd, lty = lty, col = col)
        }
        else if (d == 0 && x1[2] == x2[2]) {
            if (x1[1] > x2[1]) 
                arrow2(x1[1] - delta1[1], x1[2], x2[1] + delta2[1], 
                  x2[2], lwd = lwd, lty = lty, col = col)
            else arrow2(x1[1] + delta1[1], x1[2], x2[1] - delta2[1], 
                x2[2], lwd = lwd, lty = lty, col = col)
        }
        else {
            temp <- phi(x1[1], x1[2], x2[1], x2[2], d, delta1, 
                delta2)
            if (d == 0) {
                arrow2(temp$center[1] + temp$r * cos(temp$angle[1]), 
                  temp$center[2] + temp$r * sin(temp$angle[1]), 
                  temp$center[1] + temp$r * cos(temp$angle[2]), 
                  temp$center[2] + temp$r * sin(temp$angle[2]), 
                  lwd = lwd, lty = lty, col = col)
            }
            else {
                phi <- seq(temp$angle[1], temp$angle[2], length = 21)
                lines(temp$center[1] + temp$r * cos(phi), temp$center[2] + 
                  temp$r * sin(phi), lwd = lwd, lty = lty, col = col)
                arrow2(temp$center[1] + temp$r * cos(phi[20]), 
                  temp$center[2] + temp$r * sin(phi[20]), temp$center[1] + 
                    temp$r * cos(phi[21]), temp$center[2] + temp$r * 
                    sin(phi[21]), lwd = lwd, lty = lty, col = col)
            }
        }
    }
    k <- 1
    for (j in 1:nstate) {
        for (i in 1:nstate) {
            if (i != j && connect[i, j] != 0) {
                if (connect[i, j] == 2 - connect[j, i] && offset > 
                  0) {
                  toff <- c(cbox[j, 2] - cbox[i, 2], cbox[i, 
                    1] - cbox[j, 1])
                  toff <- offset * toff/sqrt(sum(toff^2))
                  doline(cbox[i, ] + toff, cbox[j, ] + toff, 
                    connect[i, j] - 1, delta1 = c(textwd[i]/2 + 
                      dx, textht[i]/2 + dy), delta2 = c(textwd[j]/2 + 
                      dx, textht[j]/2 + dy), lty = alty[k], lwd = alwd[k], 
                    col = acol[k])
                }
                else doline(cbox[i, ], cbox[j, ], connect[i, 
                  j] - 1, delta1 = c(textwd[i]/2 + dx, textht[i]/2 + 
                  dy), delta2 = c(textwd[j]/2 + dx, textht[j]/2 + 
                  dy), lty = alty[k], lwd = alwd[k], col = acol[k])
                k <- k + 1
            }
        }
    }
    invisible(cbox)
}


cox.zph <- function (fit, transform = "km", global = TRUE) 
{
    call <- match.call()
    if (!inherits(fit, "coxph")) 
        stop("Argument must be the result of coxph")
    if (inherits(fit, "coxph.null")) 
        stop("The are no score residuals for a Null model")
    sresid <- resid(fit, "schoenfeld")
    varnames <- names(fit$coefficients)
    nvar <- length(varnames)
    ndead <- length(sresid)/nvar
    if (nvar == 1) 
        times <- as.numeric(names(sresid))
    else times <- as.numeric(dimnames(sresid)[[1]])
    if (is.character(transform)) {
        tname <- transform
        ttimes <- switch(transform, identity = times, rank = rank(times), 
            log = log(times), km = {
                temp <- survfitKM(factor(rep(1, nrow(fit$y))), 
                  fit$y, se.fit = FALSE)
                t1 <- temp$surv[temp$n.event > 0]
                t2 <- temp$n.event[temp$n.event > 0]
                km <- rep(c(1, t1), c(t2, 0))
                if (is.null(attr(sresid, "strata"))) 1 - km else (1 - 
                  km[sort.list(sort.list(times))])
            }, stop("Unrecognized transform"))
    }
    else {
        tname <- deparse(substitute(transform))
        if (length(tname) > 1) 
            tname <- "user"
        ttimes <- transform(times)
    }
    xx <- ttimes - mean(ttimes)
    r2 <- sresid %*% fit$var * ndead
    test <- xx %*% r2
    corel <- c(cor(xx, r2))
    z <- c(test^2/(diag(fit$var) * ndead * sum(xx^2)))
    Z.ph <- cbind(corel, z, pchisq(z, 1, lower.tail = FALSE))
    if (global && nvar > 1) {
        test <- c(xx %*% sresid)
        z <- c(test %*% fit$var %*% test) * ndead/sum(xx^2)
        Z.ph <- rbind(Z.ph, c(NA, z, pchisq(z, ncol(sresid), 
            lower.tail = FALSE)))
        dimnames(Z.ph) <- list(c(varnames, "GLOBAL"), c("rho", 
            "chisq", "p"))
    }
    else dimnames(Z.ph) <- list(varnames, c("rho", "chisq", "p"))
    dimnames(r2) <- list(times, names(fit$coefficients))
    temp <- list(table = Z.ph, x = ttimes, y = r2 + outer(rep(1, 
        ndead), fit$coefficients), var = fit$var, call = call, 
        transform = tname)
    if (is.R()) 
        class(temp) <- "cox.zph"
    else oldClass(temp) <- "cox.zph"
    temp
}


coxph <- function (formula, data, weights, subset, na.action, init, control, 
    ties = c("efron", "breslow", "exact"), singular.ok = TRUE, 
    robust = FALSE, model = FALSE, x = FALSE, y = TRUE, tt, method = ties, 
    ...) 
{
    ties <- match.arg(ties)
    Call <- match.call()
    extraArgs <- list(...)
    if (length(extraArgs)) {
        controlargs <- names(formals(coxph.control))
        indx <- pmatch(names(extraArgs), controlargs, nomatch = 0L)
        if (any(indx == 0L)) 
            stop(gettextf("Argument %s not matched", names(extraArgs)[indx == 
                0L]), domain = NA)
    }
    if (missing(control)) 
        control <- coxph.control(...)
    indx <- match(c("formula", "data", "weights", "subset", "na.action"), 
        names(Call), nomatch = 0)
    if (indx[1] == 0) 
        stop("A formula argument is required")
    temp <- Call[c(1, indx)]
    temp[[1L]] <- quote(stats::model.frame)
    special <- c("strata", "cluster", "tt")
    temp$formula <- if (missing(data)) 
        terms(formula, special)
    else terms(formula, special, data = data)
    if (!is.null(attr(temp$formula, "specials")$tt)) {
        coxenv <- new.env(parent = environment(formula))
        assign("tt", function(x) x, env = coxenv)
        environment(temp$formula) <- coxenv
    }
    mf <- eval(temp, parent.frame())
    if (nrow(mf) == 0) 
        stop("No (non-missing) observations")
    Terms <- terms(mf)
    Y <- model.extract(mf, "response")
    if (!inherits(Y, "Surv")) 
        stop("Response must be a survival object")
    type <- attr(Y, "type")
    if (type != "right" && type != "counting") 
        stop(paste("Cox model doesn't support \"", type, "\" survival data", 
            sep = ""))
    data.n <- nrow(Y)
    if (control$timefix) 
        Y <- aeqSurv(Y)
    if (length(attr(Terms, "variables")) > 2) {
        ytemp <- terms.inner(formula[1:2])
        xtemp <- terms.inner(formula[-2])
        if (any(!is.na(match(xtemp, ytemp)))) 
            warning("a variable appears on both the left and right sides of the formula")
    }
    strats <- attr(Terms, "specials")$strata
    if (length(strats)) {
        stemp <- untangle.specials(Terms, "strata", 1)
        if (length(stemp$vars) == 1) 
            strata.keep <- mf[[stemp$vars]]
        else strata.keep <- strata(mf[, stemp$vars], shortlabel = TRUE)
        strats <- as.numeric(strata.keep)
    }
    timetrans <- attr(Terms, "specials")$tt
    if (missing(tt)) 
        tt <- NULL
    if (length(timetrans)) {
        timetrans <- untangle.specials(Terms, "tt")
        ntrans <- length(timetrans$terms)
        if (is.null(tt)) {
            tt <- function(x, time, riskset, weights) {
                obrien <- function(x) {
                  r <- rank(x)
                  (r - 0.5)/(0.5 + length(r) - r)
                }
                unlist(tapply(x, riskset, obrien))
            }
        }
        if (is.function(tt)) 
            tt <- list(tt)
        if (is.list(tt)) {
            if (any(!sapply(tt, is.function))) 
                stop("The tt argument must contain function or list of functions")
            if (length(tt) != ntrans) {
                if (length(tt) == 1) {
                  temp <- vector("list", ntrans)
                  for (i in 1:ntrans) temp[[i]] <- tt[[1]]
                  tt <- temp
                }
                else stop("Wrong length for tt argument")
            }
        }
        else stop("The tt argument must contain a function or list of functions")
        if (ncol(Y) == 2) {
            if (length(strats) == 0) {
                sorted <- order(-Y[, 1], Y[, 2])
                newstrat <- rep.int(0L, nrow(Y))
                newstrat[1] <- 1L
            }
            else {
                sorted <- order(strats, -Y[, 1], Y[, 2])
                newstrat <- as.integer(c(1, 1 * (diff(strats[sorted]) != 
                  0)))
            }
            if (storage.mode(Y) != "double") 
                storage.mode(Y) <- "double"
            counts <- .Call(Ccoxcount1, Y[sorted, ], as.integer(newstrat))
            tindex <- sorted[counts$index]
        }
        else {
            if (length(strats) == 0) {
                sort.end <- order(-Y[, 2], Y[, 3])
                sort.start <- order(-Y[, 1])
                newstrat <- c(1L, rep(0, nrow(Y) - 1))
            }
            else {
                sort.end <- order(strats, -Y[, 2], Y[, 3])
                sort.start <- order(strats, -Y[, 1])
                newstrat <- c(1L, as.integer(diff(strats[sort.end]) != 
                  0))
            }
            if (storage.mode(Y) != "double") 
                storage.mode(Y) <- "double"
            counts <- .Call(Ccoxcount2, Y, as.integer(sort.start - 
                1L), as.integer(sort.end - 1L), as.integer(newstrat))
            tindex <- counts$index
        }
        Y <- Surv(rep(counts$time, counts$nrisk), counts$status)
        type <- "right"
        mf <- mf[tindex, ]
        strats <- rep(1:length(counts$nrisk), counts$nrisk)
        weights <- model.weights(mf)
        if (!is.null(weights) && any(!is.finite(weights))) 
            stop("weights must be finite")
        tcall <- attr(Terms, "variables")[timetrans$terms + 2]
        pvars <- attr(Terms, "predvars")
        pmethod <- sub("makepredictcall.", "", as.vector(methods("makepredictcall")))
        for (i in 1:ntrans) {
            newtt <- (tt[[i]])(mf[[timetrans$var[i]]], Y[, 1], 
                strats, weights)
            mf[[timetrans$var[i]]] <- newtt
            nclass <- class(newtt)
            if (any(nclass %in% pmethod)) {
                dummy <- as.call(list(as.name(class(newtt)[1]), 
                  tcall[[i]][[2]]))
                ptemp <- makepredictcall(newtt, dummy)
                pvars[[timetrans$terms[i] + 2]] <- ptemp
            }
        }
        attr(Terms, "predvars") <- pvars
    }
    cluster <- attr(Terms, "specials")$cluster
    if (length(cluster)) {
        robust <- TRUE
        tempc <- untangle.specials(Terms, "cluster", 1:10)
        ord <- attr(Terms, "order")[tempc$terms]
        if (any(ord > 1)) 
            stop("Cluster can not be used in an interaction")
        cluster <- strata(mf[, tempc$vars], shortlabel = TRUE)
        dropterms <- tempc$terms
        xlevels <- .getXlevels(Terms[-tempc$terms], mf)
    }
    else {
        if (missing(robust)) 
            robust <- FALSE
        xlevels <- .getXlevels(Terms, mf)
        dropterms <- NULL
    }
    contrast.arg <- NULL
    attr(Terms, "intercept") <- 1
    stemp <- untangle.specials(Terms, "strata", 1)
    hasinteractions <- FALSE
    if (length(stemp$vars) > 0) {
        for (i in stemp$vars) {
            if (any(attr(Terms, "order")[attr(Terms, "factors")[i, 
                ] > 0] > 1)) 
                hasinteractions <- TRUE
        }
        if (!hasinteractions) 
            dropterms <- c(dropterms, stemp$terms)
    }
    if (length(dropterms)) {
        Terms2 <- Terms[-dropterms]
        X <- model.matrix(Terms2, mf, constrasts = contrast.arg)
        temp <- attr(X, "assign")
        shift <- sort(dropterms)
        temp <- temp + 1 * (shift[1] <= temp)
        if (length(shift) == 2) 
            temp + 1 * (shift[2] <= temp)
        attr(X, "assign") <- temp
    }
    else X <- model.matrix(Terms, mf, contrasts = contrast.arg)
    if (!all(is.finite(X))) 
        stop("data contains an infinite predictor")
    Xatt <- attributes(X)
    if (hasinteractions) 
        adrop <- c(0, untangle.specials(Terms, "strata")$terms)
    else adrop <- 0
    xdrop <- Xatt$assign %in% adrop
    X <- X[, !xdrop, drop = FALSE]
    attr(X, "assign") <- Xatt$assign[!xdrop]
    attr(X, "contrasts") <- Xatt$contrasts
    offset <- model.offset(mf)
    if (is.null(offset) | all(offset == 0)) 
        offset <- rep(0, nrow(mf))
    else if (any(!is.finite(offset) | !is.finite(exp(offset)))) 
        stop("offsets must lead to a finite risk score")
    weights <- model.weights(mf)
    if (!is.null(weights) && any(!is.finite(weights))) 
        stop("weights must be finite")
    assign <- attrassign(X, Terms)
    contr.save <- attr(X, "contrasts")
    if (missing(init)) 
        init <- NULL
    else {
        if (length(init) != ncol(X)) 
            stop("wrong length for init argument")
        temp <- X %*% init - sum(colMeans(X) * init) + offset
        if (any(exp(temp) > .Machine$double.xmax) || all(exp(temp) == 
            0)) 
            stop("initial values lead to overflow or underflow of the exp function")
    }
    if (sum(Y[, ncol(Y)]) == 0) {
        ncoef <- ncol(X)
        ctemp <- rep(NA, ncoef)
        names(ctemp) <- colnames(X)
        concordance = c(concordant = 0, discordant = 0, tied.x = 0, 
            tied.y = 0, tied.xy = 0, concordance = NA, std = NA, 
            timefix = FALSE)
        rval <- list(coefficients = ctemp, var = matrix(0, ncoef, 
            ncoef), loglik = c(0, 0), score = 0, iter = 0, linear.predictors = offset, 
            residuals = rep(0, data.n), means = colMeans(X), 
            method = method, n = data.n, nevent = 0, terms = Terms, 
            assign = assign, concordance = concordance, y = Y, 
            call = Call)
        class(rval) <- "coxph"
        return(rval)
    }
    pterms <- sapply(mf, inherits, "coxph.penalty")
    if (any(pterms)) {
        pattr <- lapply(mf[pterms], attributes)
        pname <- names(pterms)[pterms]
        ord <- attr(Terms, "order")[match(pname, attr(Terms, 
            "term.labels"))]
        if (any(ord > 1)) 
            stop("Penalty terms cannot be in an interaction")
        pcols <- assign[match(pname, names(assign))]
        fit <- coxpenal.fit(X, Y, strats, offset, init = init, 
            control, weights = weights, method = method, row.names(mf), 
            pcols, pattr, assign)
    }
    else {
        if (method == "breslow" || method == "efron") {
            if (type == "right") 
                fitter <- get("coxph.fit")
            else fitter <- get("agreg.fit")
        }
        else if (method == "exact") {
            if (type == "right") 
                fitter <- get("coxexact.fit")
            else fitter <- get("agexact.fit")
        }
        else stop(paste("Unknown method", method))
        fit <- fitter(X, Y, strats, offset, init, control, weights = weights, 
            method = method, row.names(mf))
    }
    if (is.character(fit)) {
        fit <- list(fail = fit)
        class(fit) <- "coxph"
    }
    else {
        if (!is.null(fit$coefficients) && any(is.na(fit$coefficients))) {
            vars <- (1:length(fit$coefficients))[is.na(fit$coefficients)]
            msg <- paste("X matrix deemed to be singular; variable", 
                paste(vars, collapse = " "))
            if (!singular.ok) 
                stop(msg)
        }
        fit$n <- data.n
        fit$nevent <- sum(Y[, ncol(Y)])
        fit$terms <- Terms
        fit$assign <- assign
        class(fit) <- fit$class
        fit$class <- NULL
        if (robust && !is.null(fit$coefficients) && !all(is.na(fit$coefficients))) {
            fit$naive.var <- fit$var
            fit2 <- c(fit, list(x = X, y = Y, weights = weights))
            if (length(strats)) 
                fit2$strata <- strats
            if (length(cluster)) {
                temp <- residuals.coxph(fit2, type = "dfbeta", 
                  collapse = cluster, weighted = TRUE)
                if (is.null(init)) 
                  fit2$linear.predictors <- 0 * fit$linear.predictors
                else fit2$linear.predictors <- c(X %*% init)
                temp0 <- residuals.coxph(fit2, type = "score", 
                  collapse = cluster, weighted = TRUE)
            }
            else {
                temp <- residuals.coxph(fit2, type = "dfbeta", 
                  weighted = TRUE)
                fit2$linear.predictors <- 0 * fit$linear.predictors
                temp0 <- residuals.coxph(fit2, type = "score", 
                  weighted = TRUE)
            }
            fit$var <- t(temp) %*% temp
            u <- apply(as.matrix(temp0), 2, sum)
            fit$rscore <- coxph.wtest(t(temp0) %*% temp0, u, 
                control$toler.chol)$test
        }
        if (length(fit$coefficients) && is.null(fit$wald.test)) {
            nabeta <- !is.na(fit$coefficients)
            if (is.null(init)) 
                temp <- fit$coefficients[nabeta]
            else temp <- (fit$coefficients - init[1:length(fit$coefficients)])[nabeta]
            fit$wald.test <- coxph.wtest(fit$var[nabeta, nabeta], 
                temp, control$toler.chol)$test
        }
        if (length(cluster)) 
            temp <- concordancefit(Y, fit$linear.predictors, 
                strats, weights, cluster = cluster, reverse = TRUE, 
                timefix = FALSE)
        else temp <- concordancefit(Y, fit$linear.predictors, 
            strats, weights, reverse = TRUE, timefix = FALSE)
        if (is.matrix(temp$count)) 
            fit$concordance <- c(colSums(temp$count), concordance = temp$concordance, 
                std = sqrt(temp$var))
        else fit$concordance <- c(temp$count, concordance = temp$concordance, 
            std = sqrt(temp$var))
        na.action <- attr(mf, "na.action")
        if (length(na.action)) 
            fit$na.action <- na.action
        if (model) {
            if (length(timetrans)) {
                mf[[".surv."]] <- Y
                mf[[".strata."]] <- strats
                stop("'model=TRUE' not supported for models with tt terms")
            }
            fit$model <- mf
        }
        if (x) {
            fit$x <- X
            if (length(strats)) {
                if (length(timetrans)) 
                  fit$strata <- strats
                else fit$strata <- strata.keep
            }
        }
        if (y) 
            fit$y <- Y
    }
    if (!is.null(weights) && any(weights != 1)) 
        fit$weights <- weights
    names(fit$means) <- names(fit$coefficients)
    fit$formula <- formula(Terms)
    if (length(xlevels) > 0) 
        fit$xlevels <- xlevels
    fit$contrasts <- contr.save
    if (any(offset != 0)) 
        fit$offset <- offset
    fit$call <- Call
    fit
}


basehaz <- function (fit, centered = TRUE) 
{
    if (!inherits(fit, "coxph")) 
        stop("must be a coxph object")
    sfit <- survfit(fit, se.fit = FALSE)
    if (!centered) {
        zcoef <- ifelse(is.na(coef(fit)), 0, coef(fit))
        offset <- sum(fit$means * zcoef)
        chaz <- sfit$cumhaz * exp(-offset)
    }
    else chaz <- sfit$cumhaz
    new <- data.frame(hazard = chaz, time = sfit$time)
    strata <- sfit$strata
    if (!is.null(strata)) 
        new$strata <- factor(rep(names(strata), strata), levels = names(strata))
    new
}


coxph.detail <- function (object, riskmat = FALSE) 
{
    method <- object$method
    if (method != "breslow" && method != "efron") 
        stop(paste("Detailed output is not available for the", 
            method, "method"))
    n <- length(object$residuals)
    weights <- object$weights
    x <- object[["x"]]
    y <- object$y
    strat <- object$strata
    Terms <- object$terms
    if (!inherits(Terms, "terms")) 
        stop("invalid terms component of object")
    strats <- attr(Terms, "specials")$strata
    if (is.null(y) || is.null(x)) {
        mf <- stats::model.frame(object)
        y <- model.response(mf)
        x <- model.matrix(object, data = mf)
        if (length(strats)) {
            stemp <- untangle.specials(object$terms, "strata", 
                1)
            if (length(stemp$vars) == 1) 
                strat <- mf[[stemp$vars]]
            else strat <- strata(mf[, stemp$vars], shortlabel = TRUE)
        }
    }
    nvar <- ncol(x)
    if (ncol(y) == 2) {
        mintime <- min(y[, 1])
        if (mintime < 0) 
            y <- cbind(2 * mintime - 1, y)
        else y <- cbind(-1, y)
    }
    if (is.null(strat)) {
        ord <- order(y[, 2], -y[, 3])
        newstrat <- rep(0, n)
    }
    else {
        ord <- order(strat, y[, 2], -y[, 3])
        newstrat <- c(diff(as.numeric(strat[ord])) != 0, 1)
    }
    newstrat[n] <- 1
    x <- x[ord, ]
    y <- y[ord, ]
    storage.mode(y) <- "double"
    score <- exp(object$linear.predictors)[ord]
    if (is.null(weights)) 
        weights <- rep(1, n)
    else weights <- weights[ord]
    ndeath <- sum(y[, 3])
    if (riskmat) {
        rmat <- integer(ndeath * n)
    }
    else rmat <- as.integer(1)
    ff <- .C(Ccoxdetail, as.integer(n), as.integer(nvar), ndeath = as.integer(ndeath), 
        y = y, as.double(x), index = as.integer(newstrat), event2 = as.double(score), 
        weights = as.double(weights), means = c(method == "efron", 
            double(ndeath * nvar - 1)), u = double(ndeath * nvar), 
        i = double(ndeath * nvar * nvar), rmat = rmat, nrisk2 = double(ndeath), 
        double(nvar * (3 + 2 * nvar)))
    keep <- 1:ff$ndeath
    vname <- dimnames(x)[[2]]
    time <- y[ff$index[keep], 2]
    names(time) <- NULL
    means <- (matrix(ff$means, ndeath, nvar))[keep, ]
    score <- matrix(ff$u, ndeath, nvar)[keep, ]
    var <- array(ff$i, c(nvar, nvar, ndeath))[, , keep]
    if (riskmat) {
        rmat <- matrix(0, n, ff$ndeath)
        rmat[ord, ] <- ff$rmat[1:(n * ff$ndeath)]
        dimnames(rmat) <- list(NULL, time)
    }
    if (nvar > 1) {
        dimnames(means) <- list(time, vname)
        dimnames(score) <- list(time, vname)
        dimnames(var) <- list(vname, vname, time)
    }
    else {
        names(means) <- time
        names(score) <- time
        names(var) <- time
    }
    dimnames(ff$y) <- NULL
    temp <- list(time = time, means = means, nevent = ff$y[keep, 
        1], nrisk = ff$y[keep, 2], hazard = ff$y[keep, 3], score = score, 
        imat = var, varhaz = ff$weights[keep], y = y, x = x)
    if (length(strats)) 
        temp$strata <- table((strat[ord])[ff$index[keep]])
    if (riskmat) 
        temp$riskmat <- rmat
    if (!all(weights == 1)) {
        temp$weights <- weights
        temp$nevent.wt <- ff$event2[keep]
        temp$nrisk.wt <- ff$nrisk2[keep]
    }
    temp
}


survfitKM <- function (x, y, weights = rep(1, length(x)), stype = 1, ctype = 1, 
    se.fit = TRUE, conf.int = 0.95, conf.type = c("log", "log-log", 
        "plain", "none", "logit", "arcsin"), conf.lower = c("usual", 
        "peto", "modified"), start.time, new.time, id, cluster, 
    influence = FALSE, type) 
{
    if (!missing(type)) {
        if (!is.character(type)) 
            stop("type argument must be character")
        temp <- charmatch(type, c("kaplan-meier", "fleming-harrington", 
            "fh2"))
        if (is.na(temp)) 
            stop("invalid value for 'type'")
        type <- c(1, 3, 4)[temp]
    }
    else {
        if (!(ctype %in% 1:2)) 
            stop("ctype must be 1 or 2")
        if (!(stype %in% 1:2)) 
            stop("stype must be 1 or 2")
        type <- as.integer(2 * stype + ctype - 2)
    }
    conf.type <- match.arg(conf.type)
    conf.lower <- match.arg(conf.lower)
    if (is.logical(conf.int)) {
        if (!conf.int) 
            conf.type <- "none"
        conf.int <- 0.95
    }
    has.cluster <- !(missing(cluster) || length(cluster) == 0)
    has.id <- !(missing(id) || length(id) == 0)
    if (has.id) 
        id <- as.factor(id)
    if (has.cluster) {
        if (is.factor(cluster)) {
            clname <- levels(cluster)
            cluster <- as.integer(cluster)
        }
        else {
            clname <- sort(unique(cluster))
            cluster <- match(cluster, clname)
        }
        ncluster <- length(clname)
    }
    else {
        if (has.id) {
            clname <- levels(id)
            cluster <- as.integer(id)
            ncluster <- length(clname)
        }
        else {
            ncluster <- 0
            clname <- NULL
        }
    }
    if (is.logical(influence)) {
        if (!influence) 
            influence <- 0L
        else influence <- 3L
    }
    else if (!is.numeric(influence)) 
        stop("influence argument must be numeric or logical")
    if (!(influence %in% 0:3)) 
        stop("influence argument must be 0, 1, 2, or 3")
    else influence <- as.integer(influence)
    if (!se.fit) {
        ncluster <- 0L
        influence <- 0L
    }
    if (!is.Surv(y)) 
        stop("y must be a Surv object")
    if (attr(y, "type") != "right" && attr(y, "type") != "counting") 
        stop("Can only handle right censored or counting data")
    ny <- ncol(y)
    if (!is.factor(x)) 
        stop("x must be a factor")
    xlev <- levels(x)
    x <- as.integer(x)
    if (missing(start.time)) {
        if (!missing(new.time)) 
            start.time <- new.time
        else start.time <- min(c(0, y[, ny - 1]))
    }
    if (ny == 3 & has.id) {
        id <- as.integer(id)
        index <- order(id, y[, 2])
        firstid <- !duplicated(id[index])
        lastid <- !duplicated(id[index], fromLast = TRUE)
        position <- ifelse(firstid, 1L, 0L) + ifelse(lastid, 
            2L, 0L)
        gap <- (diff(id[index]) == 0 & y[index[-length(index)], 
            2] != y[index[-1], 1])
        position[gap] <- position[gap] + 2L
        position[which(gap) + 1L] <- position[which(gap) + 1L] + 
            1L
        position[index] <- position
    }
    else position <- integer(0)
    if (length(xlev) == 1) {
        if (ny == 2) {
            sort1 <- NULL
            sort2 <- order(y[, 1])
        }
        else {
            sort2 <- order(y[, 2])
            sort1 <- order(y[, 1])
        }
        toss <- (y[sort2, ny - 1] < start.time)
        if (any(toss)) {
            sort2 <- sort2[!toss]
            if (ny == 3) {
                index <- match(which(toss), sort1)
                sort1 <- sort1[-index]
            }
        }
        n.used <- length(sort2)
        if (ncluster > 0) 
            cfit <- .Call(Csurvfitkm, y, weights, sort1 - 1L, 
                sort2 - 1L, type, cluster - 1L, ncluster, position, 
                influence)
        else cfit <- .Call(Csurvfitkm, y, weights, sort1 - 1L, 
            sort2 - 1L, type, 0L, 0L, position, influence)
    }
    else {
        ngroup <- length(xlev)
        cfit <- vector("list", ngroup)
        n.used <- integer(ngroup)
        if (influence) 
            clusterid <- cfit
        for (i in 1:ngroup) {
            keep <- which(x == i & y[, ny - 1] >= start.time)
            if (length(keep) == 0) 
                next
            ytemp <- y[keep, ]
            n.used[i] <- nrow(ytemp)
            if (ny == 2) {
                sort1 <- NULL
                sort2 <- order(ytemp[, 1])
            }
            else {
                sort2 <- order(ytemp[, 2])
                sort1 <- order(ytemp[, 1])
            }
            if (ncluster > 0) {
                c2 <- cluster[keep]
                c.unique <- sort(unique(c2))
                nc <- length(c.unique)
                c2 <- match(c2, c.unique)
                if (influence > 0) {
                  clusterid[[i]] <- c.unique
                }
            }
            if (ncluster > 0) 
                cfit[[i]] <- .Call(Csurvfitkm, ytemp, weights[keep], 
                  sort1 - 1L, sort2 - 1L, type, c2 - 1L, length(c.unique), 
                  position, influence)
            else cfit[[i]] <- .Call(Csurvfitkm, ytemp, weights[keep], 
                sort1 - 1L, sort2 - 1L, type, 0L, 0L, position, 
                influence)
        }
    }
    if (length(n.used) == 1) {
        rval <- list(n = length(x), time = cfit$time, n.risk = cfit$n[, 
            4], n.event = cfit$n[, 5], n.censor = cfit$n[, 6], 
            surv = cfit$estimate[, 1], std.err = cfit$std[, 1], 
            cumhaz = cfit$estimate[, 2], std.chaz = cfit$std[, 
                2], start.time = start.time)
    }
    else {
        strata <- sapply(cfit, function(x) nrow(x$n))
        names(strata) <- xlev
        rval <- list(n = as.vector(table(x)), time = unlist(lapply(cfit, 
            function(x) x$time)), n.risk = unlist(lapply(cfit, 
            function(x) x$n[, 4])), n.event = unlist(lapply(cfit, 
            function(x) x$n[, 5])), n.censor = unlist(lapply(cfit, 
            function(x) x$n[, 6])), surv = unlist(lapply(cfit, 
            function(x) x$estimate[, 1])), std.err = unlist(lapply(cfit, 
            function(x) x$std[, 1])), cumhaz = unlist(lapply(cfit, 
            function(x) x$estimate[, 2])), std.chaz = unlist(lapply(cfit, 
            function(x) x$std[, 2])), start.time = start.time, 
            strata = strata)
        if (ny == 3) 
            rval$n.enter <- unlist(lapply(cfit, function(x) x$n[, 
                8]))
    }
    if (ny == 3) {
        rval$n.enter <- cfit$n[, 8]
        rval$type <- "counting"
    }
    else rval$type <- "right"
    if (se.fit) {
        rval$logse = (ncluster == 0 || (type == 2 || type == 
            4))
        rval$conf.int = conf.int
        rval$conf.type = conf.type
        if (conf.lower != "usual") 
            rval$conf.lower = conf.lower
        if (conf.lower == "modified") {
            nstrat = length(n.used)
            events <- rval$n.event > 0
            if (nstrat == 1) 
                events[1] <- TRUE
            else events[1 + cumsum(c(0, rval$strata[-nstrat]))] <- TRUE
            zz <- 1:length(events)
            n.lag <- rep(rval$n.risk[events], diff(c(zz[events], 
                1 + max(zz))))
        }
        std.low <- switch(conf.lower, usual = rval$std.err, peto = sqrt((1 - 
            rval$surv)/rval$n.risk), modified = rval$std.err * 
            sqrt(n.lag/rval$n.risk))
        if (conf.type != "none") {
            ci <- survfit_confint(rval$surv, rval$std.err, logse = rval$logse, 
                conf.type, conf.int, std.low)
            rval <- c(rval, list(lower = ci$lower, upper = ci$upper))
        }
    }
    else {
        rval$std.err <- NULL
        rval$std.chaz <- NULL
    }
    if (influence > 0) {
        if (type == 1 | type == 2) {
            if (influence == 1 || influence == 3) {
                if (length(xlev) == 1) {
                  rval$influence.surv <- cfit$influence1
                  row.names(rval$influence.surv) <- clname
                }
                else {
                  temp <- vector("list", ngroup)
                  for (i in 1:ngroup) {
                    temp[[i]] <- cfit[[i]]$influence1
                    row.names(temp[[i]]) <- clname[clusterid[[i]]]
                  }
                  rval$influence.surv <- temp
                }
            }
            if (influence == 2 || influence == 3) {
                if (length(xlev) == 1) {
                  rval$influence.chaz <- cfit$influence2
                  row.names(rval$influence.chaz) <- clname
                }
                else {
                  temp <- vector("list", ngroup)
                  for (i in 1:ngroup) {
                    temp[[i]] <- cfit[[i]]$influence2
                    row.names(temp[[i]]) <- clname[clusterid[[i]]]
                  }
                  rval$influence.chaz <- temp
                }
            }
        }
        else {
            if (length(xlev) == 1) {
                temp <- cfit$influence2
                row.names(temp) <- clname
            }
            else {
                temp <- vector("list", ngroup)
                for (i in 1:ngroup) {
                  temp[[i]] <- cfit[[i]]$influence2
                  row.names(temp[[i]]) <- clname[clusterid[[i]]]
                }
            }
            if (influence == 2 || influence == 3) 
                rval$influence.chaz <- temp
            if (influence == 1 || influence == 3) {
                if (length(xlev) == 1) 
                  rval$influence.surv <- -temp * rep(rval$surv, 
                    each = nrow(temp))
                else {
                  for (i in 1:ngroup) temp[[i]] <- -temp[[i]] * 
                    rep(cfit[[i]]$estimate[, 1], each = nrow(temp[[i]]))
                  rval$influence.surv <- temp
                }
            }
        }
    }
    rval
}


cipoisson <- function (k, time = 1, p = 0.95, method = c("exact", "anscombe")) 
{
    nn <- max(length(k), length(time), length(p))
    if (nn > 1) {
        k <- rep(k, length = nn)
        time <- rep(time, length = nn)
        p <- rep(p, length = nn)
    }
    p <- (1 - p)/2
    method <- match.arg(method)
    if (method == "exact") {
        dummy1 <- ifelse(k == 0, 1, k)
        lower <- ifelse(k == 0, 0, qgamma(p, dummy1))
        upper <- qgamma(1 - p, k + 1)
    }
    else if (method == "anscombe") {
        upper <- (sqrt(k + 7/8) - qnorm(p)/2)^2
        lower <- (sqrt(k - 1/8) + qnorm(p)/2)^2
    }
    else stop("Invalid method")
    if (any(time <= 0)) {
        lower <- ifelse(time <= 0, NA, lower)
        upper <- ifelse(time <= 0, NA, upper)
    }
    if (nn == 1) 
        c(lower = lower, upper = upper)/time
    else {
        temp <- cbind(lower = lower, upper = upper)/time
        if (is.array(k)) {
            if (is.null(dd <- dimnames(k))) 
                array(temp, dim = c(dim(k), 2), dimnames = list(NULL, 
                  NULL, c("lower", "upper")))
            else array(temp, dim = c(dim(k), 2), dimnames = list(dd, 
                c("lower", "upper")))
        }
        else temp
    }
}


frailty.gaussian <- function (x, sparse = (nclass > 5), theta, df, method = c("reml", 
    "aic", "df", "fixed"), ...) 
{
    if (missing(method)) {
        if (!missing(theta)) {
            method <- "fixed"
            if (!missing(df)) 
                stop("Cannot give both a df and theta argument")
        }
        else if (!missing(df)) {
            if (df == 0) 
                method <- "aic"
            else method <- "df"
        }
    }
    method <- match.arg(method)
    if (method == "df" && missing(df)) 
        stop("Method = df but no df argument")
    if (method == "fixed" && missing(theta)) 
        stop("Method= fixed but no theta argument")
    if (method != "fixed" && !missing(theta)) 
        stop("Method is not 'fixed', but have a theta argument")
    nclass <- length(unique(x[!is.na(x)]))
    if (sparse) {
        x <- as.numeric(factor(x))
        class(x) <- "coxph.penalty"
    }
    else {
        x <- factor(x)
        nclass <- length(levels(x))
        class(x) <- c("coxph.penalty", class(x))
        attr(x, "contrasts") <- contr.treatment(nclass, contrasts = FALSE)
    }
    if (!missing(theta) & !missing(df)) 
        stop("Cannot give both a df and theta argument")
    pfun <- function(coef, theta, ndead) {
        if (theta == 0) 
            list(recenter = 0, penalty = 0, flag = TRUE)
        else {
            recenter <- mean(coef)
            coef <- coef - recenter
            list(recenter = recenter, first = coef/theta, second = rep(1, 
                length(coef))/theta, penalty = 0.5 * sum(coef^2/theta + 
                log(2 * pi * theta)), flag = FALSE)
        }
    }
    printfun <- function(coef, var, var2, df, history) {
        if (!is.null(history$history)) 
            theta <- history$history[nrow(history$history), 1]
        else theta <- history$theta
        if (is.matrix(var)) 
            test <- coxph.wtest(var, coef)$test
        else test <- sum(coef^2/var)
        df2 <- max(df, 0.5)
        list(coef = c(NA, NA, NA, test, df, pchisq(test, df2, 
            lower.tail = FALSE)), history = paste("Variance of random effect=", 
            format(theta)))
    }
    temp <- new.env(parent = globalenv())
    assign("cox.zph", cox.zph, envir = temp)
    environment(printfun) <- temp
    if (method == "reml") {
        temp <- list(pfun = pfun, printfun = printfun, diag = TRUE, 
            sparse = sparse, cargs = c("coef", "trH", "loglik"), 
            cfun = frailty.controlgauss, cparm = list(...))
    }
    else if (method == "fixed") {
        temp <- list(pfun = pfun, printfun = printfun, diag = TRUE, 
            sparse = sparse, cfun = function(parms, iter, old) {
                list(theta = parms$theta, done = TRUE)
            }, cparm = list(theta = theta, ...))
    }
    else if (method == "aic") {
        temp <- list(pfun = pfun, printfun = printfun, diag = TRUE, 
            sparse = sparse, cargs = c("neff", "df", "plik"), 
            cparm = list(lower = 0, init = c(0.1, 1), ...), cfun = frailty.controlaic)
    }
    else {
        temp <- list(pfun = pfun, printfun = printfun, diag = TRUE, 
            sparse = sparse, cargs = ("df"), cparm = list(df = df, 
                thetas = 0, dfs = 0, guess = 3 * df/length(unclass(x)), 
                ...), cfun = frailty.controldf)
    }
    if (!sparse) {
        vname <- paste("gauss", levels(x), sep = ":")
        temp <- c(temp, list(varname = vname))
    }
    attributes(x) <- c(attributes(x), temp)
    x
}


frailty.gamma <- function (x, sparse = (nclass > 5), theta, df, eps = 1e-05, method = c("em", 
    "aic", "df", "fixed"), ...) 
{
    nclass <- length(unique(x[!is.na(x)]))
    if (sparse) 
        x <- as.numeric(factor(x))
    else {
        x <- factor(x)
        attr(x, "contrasts") <- contr.treatment(nclass, contrasts = FALSE)
    }
    class(x) <- c("coxph.penalty", class(x))
    if (missing(method)) {
        if (!missing(theta)) {
            method <- "fixed"
            if (!missing(df)) 
                stop("Cannot give both a df and theta argument")
        }
        else if (!missing(df)) 
            method <- "df"
    }
    method <- match.arg(method)
    if (method == "df" && missing(df)) 
        stop("Method = df but no df argument")
    if (method == "fixed" && missing(theta)) 
        stop("Method= fixed but no theta argument")
    if (method != "df" && !missing(df)) 
        stop("Method is not df, but have a df argument")
    if (method != "fixed" && !missing(theta)) 
        stop("Method is not 'fixed', but have a theta argument")
    pfun <- function(coef, theta, ndeath) {
        if (theta == 0) 
            list(recenter = 0, penalty = 0, flag = TRUE)
        else {
            recenter <- log(mean(exp(coef)))
            coef <- coef - recenter
            nu <- 1/theta
            list(recenter = recenter, first = (exp(coef) - 1) * 
                nu, second = exp(coef) * nu, penalty = -sum(coef) * 
                nu, flag = FALSE)
        }
    }
    printfun <- function(coef, var, var2, df, history) {
        if (!is.null(history$history)) 
            theta <- history$history[nrow(history$history), 1]
        else theta <- history$theta
        clog <- history$c.loglik
        if (is.matrix(var)) 
            test <- coxph.wtest(var, coef)$test
        else test <- sum(coef^2/var)
        df2 <- max(df, 0.5)
        list(coef = c(NA, NA, NA, test, df, pchisq(test, df2, 
            lower.tail = FALSE)), history = paste("Variance of random effect=", 
            format(theta), "  I-likelihood =", format(round(clog, 
                1), digits = 10)))
    }
    temp <- new.env(parent = globalenv())
    assign("cox.zph", cox.zph, envir = temp)
    environment(printfun) <- temp
    if (method == "fixed") {
        temp <- list(pfun = pfun, printfun = printfun, diag = TRUE, 
            sparse = sparse, cargs = c("x", "status", "loglik"), 
            cfun = frailty.controlgam, cparm = list(theta = theta, 
                ...))
    }
    else if (method == "em") {
        temp <- list(pfun = pfun, printfun = printfun, diag = TRUE, 
            sparse = sparse, cargs = c("x", "status", "loglik"), 
            cfun = frailty.controlgam, cparm = c(list(eps = eps), 
                ...))
    }
    else if (method == "aic") {
        temp <- list(pfun = pfun, printfun = printfun, diag = TRUE, 
            sparse = sparse, cargs = c("x", "status", "loglik", 
                "neff", "df", "plik"), cparm = list(eps = eps, 
                lower = 0, init = c(0.1, 1), ...), cfun = function(opt, 
                iter, old, group, status, loglik, ...) {
                temp <- frailty.controlaic(opt, iter, old, ...)
                if (iter > 0) {
                  if (old$theta == 0) correct <- 0 else {
                    if (is.matrix(group)) group <- c(group %*% 
                      1:ncol(group))
                    d <- tapply(status, group, sum)
                    correct <- frailty.gammacon(d, 1/old$theta)
                  }
                  temp$c.loglik <- loglik + correct
                }
                temp
            })
    }
    else {
        if (missing(eps)) 
            eps <- 0.1
        temp <- list(pfun = pfun, printfun = printfun, diag = TRUE, 
            sparse = sparse, cargs = c("df", "x", "status", "loglik"), 
            cparm = list(df = df, thetas = 0, dfs = 0, eps = eps, 
                guess = 3 * df/length(unclass(x)), ...), cfun = function(opt, 
                iter, old, df, group, status, loglik) {
                temp <- frailty.controldf(opt, iter, old, df)
                if (iter > 0) {
                  if (old$theta == 0) correct <- 0 else {
                    if (is.matrix(group)) group <- c(group %*% 
                      1:ncol(group))
                    d <- tapply(status, group, sum)
                    correct <- frailty.gammacon(d, 1/old$theta)
                  }
                  temp$c.loglik <- loglik + correct
                }
                temp
            })
    }
    if (!sparse) {
        vname <- paste("gamma", levels(x), sep = ":")
        temp <- c(temp, list(varname = vname))
    }
    attributes(x) <- c(attributes(x), temp)
    x
}


survregDtest <- function (dlist, verbose = F) 
{
    errlist <- NULL
    if (is.null(dlist$name)) 
        errlist <- c(errlist, "Missing a name")
    else if (length(dlist$name) != 1 || !is.character(dlist$name)) 
        errlist <- c(errlist, "Invalid name")
    if (!is.null(dlist$dist)) {
        if (!is.character(dlist$dist) || is.null(match(dlist$dist, 
            names(survreg.distributions)))) 
            errlist <- c(errlist, "Reference distribution not found")
        else {
            if (!is.function(dlist$trans)) 
                errlist <- c(errlist, "Missing or invalid trans component")
            if (!is.function(dlist$itrans)) 
                errlist <- c(errlist, "Missing or invalid itrans component")
            if (!is.function(dlist$dtrans)) 
                errlist <- c(errlist, "Missing or invalid dtrans component")
        }
        if (is.null(errlist)) {
            if (!all.equal(dlist$itrans(dlist$trans(1:10)), 1:10)) 
                errlist <- c(errlist, "trans and itrans must be inverses of each other")
            if (length(dlist$dtrans(1:10)) != 10) 
                errlist <- c(errlist, "dtrans must be a 1-1 function")
        }
    }
    else {
        if (!is.function(dlist$init)) 
            errlist <- c(errlist, "Missing or invalid init function")
        if (!is.function(dlist$deviance)) 
            errlist <- c(errlist, "Missing or invalid deviance function")
        if (!is.function(dlist$density)) 
            errlist <- c(errlist, "Missing or invalid density function")
        else {
            if (is.null(dlist$parms)) 
                temp <- dlist$density(1:10/10)
            else temp <- dlist$density(1:10/10, unlist(dlist$parms))
            if (!is.numeric(temp) || !is.matrix(temp) || nrow(temp) != 
                10 || ncol(temp) != 5) 
                errlist <- c(errlist, "Density function must return a 5 column matrix")
        }
        if (!is.function(dlist$quantile)) 
            errlist <- c(errlist, "Missing or invalid quantile function")
    }
    if (is.null(errlist)) 
        T
    else if (verbose) 
        errlist
    else F
}


coxph.fit <- function (x, y, strata, offset, init, control, weights, method, 
    rownames, resid = TRUE) 
{
    n <- nrow(y)
    if (is.matrix(x)) 
        nvar <- ncol(x)
    else {
        if (length(x) == 0) 
            nvar <- 0
        else nvar <- 1
    }
    time <- y[, 1]
    status <- y[, 2]
    if (length(strata) == 0) {
        sorted <- order(time)
        strata <- NULL
        newstrat <- as.integer(rep(0, n))
    }
    else {
        sorted <- order(strata, time)
        strata <- strata[sorted]
        newstrat <- as.integer(c(1 * (diff(as.numeric(strata)) != 
            0), 1))
    }
    if (missing(offset) || is.null(offset)) 
        offset <- rep(0, n)
    if (missing(weights) || is.null(weights)) 
        weights <- rep(1, n)
    else {
        if (any(weights <= 0)) 
            stop("Invalid weights, must be >0")
        weights <- weights[sorted]
    }
    stime <- as.double(time[sorted])
    sstat <- as.integer(status[sorted])
    if (nvar == 0) {
        x <- as.matrix(rep(1, n))
        nullmodel <- TRUE
        nvar <- 1
        init <- 0
        maxiter <- 0
    }
    else {
        nullmodel <- FALSE
        maxiter <- control$iter.max
        if (!missing(init) && length(init) > 0) {
            if (length(init) != nvar) 
                stop("Wrong length for inital values")
        }
        else init <- rep(0, nvar)
    }
    storage.mode(weights) <- storage.mode(init) <- "double"
    coxfit <- .Call(Ccoxfit6, as.integer(maxiter), stime, sstat, 
        x[sorted, ], as.double(offset[sorted]), weights, newstrat, 
        as.integer(method == "efron"), as.double(control$eps), 
        as.double(control$toler.chol), as.vector(init), as.integer(1))
    if (nullmodel) {
        if (resid) {
            score <- exp(offset[sorted])
            coxres <- .C(Ccoxmart, as.integer(n), as.integer(method == 
                "efron"), stime, sstat, newstrat, as.double(score), 
                as.double(weights), resid = double(n))
            resid <- double(n)
            resid[sorted] <- coxres$resid
            names(resid) <- rownames
            list(loglik = coxfit$loglik[1], linear.predictors = offset, 
                residuals = resid, method = method, class = c("coxph.null", 
                  "coxph"))
        }
        else list(loglik = coxfit$loglik[1], linear.predictors = offset, 
            method = method, class = c("coxph.null", "coxph"))
    }
    else {
        coef <- coxfit$coef
        lp <- c(x %*% coef) + offset - sum(coef * coxfit$means)
        var <- matrix(coxfit$imat, nvar, nvar)
        if (coxfit$flag < nvar) 
            which.sing <- diag(var) == 0
        else which.sing <- rep(FALSE, nvar)
        infs <- abs(coxfit$u %*% var)
        if (maxiter > 1) {
            if (coxfit$flag == 1000) {
                warning("Ran out of iterations and did not converge")
                if (max(lp) > 500 || any(!is.finite(infs))) 
                  warning("one or more coefficients may be infinite")
            }
            else {
                infs <- (!is.finite(coxfit$u) | ((infs > control$eps) & 
                  infs > control$toler.inf * abs(coef)))
                if (any(infs)) 
                  warning(paste("Loglik converged before variable ", 
                    paste((1:nvar)[infs], collapse = ","), "; coefficient may be infinite. "))
            }
        }
        names(coef) <- dimnames(x)[[2]]
        if (maxiter > 0) 
            coef[which.sing] <- NA
        rval <- list(coefficients = coef, var = var, loglik = coxfit$loglik, 
            score = coxfit$sctest, iter = coxfit$iter, linear.predictors = as.vector(lp), 
            means = coxfit$means, method = method, class = "coxph")
        if (resid) {
            score <- exp(lp[sorted])
            coxres <- .C(Ccoxmart, as.integer(n), as.integer(method == 
                "efron"), stime, sstat, newstrat, as.double(score), 
                as.double(weights), resid = double(n))
            resid <- double(n)
            resid[sorted] <- coxres$resid
            names(resid) <- rownames
            rval <- c(rval[1:6], list(residuals = resid), rval[-(1:6)])
        }
        rval
    }
}


frailty.t <- function (x, sparse = (nclass > 5), theta, df, eps = 1e-05, tdf = 5, 
    method = c("aic", "df", "fixed"), ...) 
{
    nclass <- length(unique(x[!is.na(x)]))
    if (sparse) {
        x <- as.numeric(factor(x))
        class(x) <- "coxph.penalty"
    }
    else {
        x <- factor(x)
        class(x) <- c("coxph.penalty", class(x))
        attr(x, "contrasts") <- contr.treatment(nclass, contrasts = FALSE)
    }
    if (tdf <= 2) 
        stop("Cannot have df <3 for the t-frailty")
    if (missing(method)) {
        if (!missing(theta)) {
            method <- "fixed"
            if (!missing(df)) 
                stop("Cannot give both a df and theta argument")
        }
        else if (!missing(df)) {
            if (df == 0) 
                method <- "aic"
            else method <- "df"
        }
    }
    method <- match.arg(method)
    if (method == "df" && missing(df)) 
        stop("Method = df but no df argument")
    if (method == "fixed" && missing(theta)) 
        stop("Method= fixed but no theta argument")
    if (method != "fixed" && !missing(theta)) 
        stop("Method is not 'fixed', but have a theta argument")
    pfun <- function(coef, theta, ndead, tdf) {
        if (theta == 0) 
            list(recenter = 0, penalty = 0, flag = TRUE)
        else {
            sig <- theta * (tdf - 2)/tdf
            temp <- 1 + coef^2/(tdf * sig)
            temp1 <- coef/temp
            temp2 <- 1/temp - (2/(tdf * sig)) * coef^2/temp^2
            recenter <- sum(temp1)/sum(temp2)
            coef <- coef - recenter
            const <- (tdf + 1)/(tdf * sig)
            temp <- 1 + coef^2/(tdf * sig)
            list(recenter = recenter, first = const * coef/temp, 
                second = const * (1/temp - (2/(tdf * sig)) * 
                  coef^2/temp^2), penalty = sum(0.5 * log(pi * 
                  tdf * sig) + ((tdf + 1)/2) * log(temp) + lgamma(tdf/2) - 
                  lgamma((tdf + 1)/2)), flag = FALSE)
        }
    }
    printfun <- function(coef, var, var2, df, history) {
        if (!is.null(history$history)) 
            theta <- history$history[nrow(history$history), 1]
        else theta <- history$theta
        if (is.matrix(var)) 
            test <- coxph.wtest(var, coef)$test
        else test <- sum(coef^2/var)
        df2 <- max(df, 0.5)
        list(coef = c(NA, NA, NA, test, df, pchisq(test, df2, 
            lower.tail = FALSE)), history = paste("Variance of random effect=", 
            format(theta)))
    }
    environment(printfun) <- asNamespace("survival")
    if (method == "fixed") {
        temp <- list(pfun = pfun, pparm = tdf, printfun = printfun, 
            diag = TRUE, sparse = sparse, cfun = function(parms, 
                iter, old) {
                list(theta = parms$theta, done = TRUE)
            }, cparm = list(theta = theta, ...))
    }
    else if (method == "aic") {
        temp <- list(pfun = pfun, pparm = tdf, printfun = printfun, 
            diag = TRUE, sparse = sparse, cargs = c("neff", "df", 
                "plik"), cparm = list(lower = 0, init = c(0.1, 
                1), eps = eps, ...), cfun = frailty.controlaic)
    }
    else {
        if (missing(eps)) 
            eps <- 0.1
        temp <- list(pfun = pfun, pparm = tdf, printfun = printfun, 
            diag = TRUE, sparse = sparse, cargs = c("df"), cparm = list(df = df, 
                eps = eps, thetas = 0, dfs = 0, guess = 3 * df/length(unclass(x)), 
                ...), cfun = frailty.controldf)
    }
    if (!sparse) {
        vname <- paste("t", levels(x), sep = ":")
        temp <- c(temp, list(varname = vname))
    }
    attributes(x) <- c(attributes(x), temp)
    x
}


clogit <- function (formula, data, weights, subset, na.action, method = c("exact", 
    "approximate", "efron", "breslow"), ...) 
{
    Call <- match.call()
    indx <- match(c("formula", "data"), names(Call), nomatch = 0)
    if (indx[1] == 0) 
        stop("A formula argument is required")
    mf <- Call[c(1, indx)]
    mf[[1L]] <- quote(stats::model.frame)
    mf$na.action <- "na.pass"
    nrows <- NROW(eval(mf, parent.frame()))
    coxcall <- Call
    coxcall[[1]] <- as.name("coxph")
    newformula <- formula
    newformula[[2]] <- substitute(Surv(rep(1, nn), case), list(case = formula[[2]], 
        nn = nrows))
    environment(newformula) <- environment(formula)
    coxcall$formula <- newformula
    method <- match.arg(method)
    coxcall$method <- switch(method, exact = "exact", efron = "efron", 
        "breslow")
    if (method == "exact") {
        if (missing(data)) 
            temp <- terms(formula, special = "cluster")
        else temp <- terms(formula, special = "cluster", data = data)
        if (!is.null(attr(temp, "specials")$cluster) && method == 
            "exact") 
            stop("robust variance plus the exact method is not supported")
        if (!is.null(coxcall$weights)) {
            coxcall$weights <- NULL
            warning("weights ignored: not possible for the exact method")
        }
    }
    coxcall <- eval(coxcall, sys.frame(sys.parent()))
    coxcall$userCall <- sys.call()
    class(coxcall) <- c("clogit", "coxph")
    coxcall
}


survSplit <- function (formula, data, subset, na.action = na.pass, cut, start = "tstart", 
    id, zero = 0, episode, end = "tstop", event = "event") 
{
    Call <- match.call()
    if (missing(formula) || is.data.frame(formula)) {
        if (missing(data)) {
            if (!missing(formula)) {
                names(Call)[[2]] <- "data"
                data <- formula
            }
            else stop("a data frame is required")
        }
        if (missing(end) || missing(event)) 
            stop("either a formula or the end and event arguments are required")
        if (!(is.character(event) && length(event) == 1 && event %in% 
            names(data))) 
            stop("'event' must be a variable name in the data set")
        if (!(is.character(end) && length(end) == 1 && end %in% 
            names(data))) 
            stop("'end' must be a variable name in the data set")
        if (!(is.character(start) && length(start) == 1)) 
            stop("'start' must be a variable name")
        if (start %in% names(data)) 
            temp <- paste(start, end, event, sep = ",")
        else temp <- paste(end, event, sep = ",")
        formula <- as.formula(paste("Surv(", temp, ")~ ."))
    }
    else if (missing(formula)) 
        stop("either a formula or the end and event arguments are required")
    indx <- match(c("data", "weights", "subset"), names(Call), 
        nomatch = 0)
    temp <- Call[c(1L, indx)]
    temp$formula <- formula
    temp$na.action <- na.action
    temp[[1L]] <- quote(stats::model.frame)
    mf <- eval.parent(temp)
    Y <- model.response(mf)
    states <- attr(Y, "states")
    if (!is.Surv(Y)) 
        stop("the model must have a Surv object as the response")
    if (!(attr(Y, "type") %in% c("right", "mright", "counting", 
        "mcounting"))) 
        stop(paste("not valid for", attr(Y, "type"), "censored survival data"))
    nY <- ncol(Y)
    ymiss <- is.na(Y)
    if (nY == 2) {
        if (any(Y[!ymiss, 1] <= zero)) 
            stop("'zero' parameter must be less than any observed times")
        Y <- cbind(zero, Y)
    }
    temp <- (Y[!ymiss, 1] >= Y[!ymiss, 2])
    if (any(temp)) 
        stop("start time must be < stop time")
    if (!is.numeric(cut) || any(!is.finite(cut))) 
        stop("cut must be a vector of finite numbers")
    cut <- sort(cut)
    ntimes <- length(cut)
    n <- nrow(data)
    if (!missing(id)) {
        if (!is.character(id)) 
            stop("id must be a variable name")
        if (id %in% names(mf)) 
            stop("the suggested id name is already present")
        id <- make.names(id)
        if (id %in% names(mf)) 
            stop("the suggested id name is already present")
        mf[[id]] <- 1:nrow(mf)
    }
    storage.mode(Y) <- "double"
    index <- .Call(Csurvsplit, Y[, 1], Y[, 2], as.double(cut))
    newdata <- mf[index$row, -1, drop = FALSE]
    row.names(newdata) <- NULL
    attr(newdata, "terms") <- NULL
    status <- Y[index$row, 3]
    status[index$censor] <- 0
    if (!is.null(states)) 
        status <- factor(status, labels = c("censor", states))
    if (inherits(formula[[2]], "call") && formula[[2]][[1]] == 
        as.name("Surv")) {
        temp <- match.call(Surv, formula[[2]])
        if (nY == 2) {
            if (missing(end) && !is.null(temp[["time"]]) && is.name(temp[["time"]])) 
                end <- as.character(temp[["time"]])
            if (missing(event) && !is.null(temp$time2) && is.name(temp$time2)) 
                event <- as.character(temp$time2)
            if (missing(event) && !is.null(temp$event) && is.name(temp$event)) 
                event <- as.character(temp$event)
        }
        else {
            if (missing(end) && !is.null(temp[["time"]]) && is.name(temp["time"])) 
                start <- as.character(temp[["time"]])
            if (missing(end) && !is.null(temp$time2) && is.name(temp$time2)) 
                end <- as.character(temp$time2)
            if (missing(event) && !is.null(temp$event) && is.name(temp$event)) 
                event <- as.character(temp$event)
            if (missing(start) && !is.null(temp$time) && is.name(temp$time)) 
                start <- as.character(temp$time)
        }
        newdata[[start]] <- index$start
        newdata[[end]] <- index$end
        newdata[[event]] <- status
    }
    else {
        if (class(formula[[2]]) != "name") 
            stop("left hand side not recognized")
        temp <- as.character(formula[[2]])
        newdata[temp] <- Surv(index$start, index$end, status)
    }
    if (!missing(episode)) {
        if (!is.character(episode)) 
            stop("episode must be a character string")
        newdata[[make.names(episode)]] <- index$interval + 1
    }
    newdata
}


survConcordance <- function (formula, data, weights, subset, na.action) 
{
    Call <- match.call()
    .Deprecated("concordance")
    m <- match.call(expand.dots = FALSE)
    m[[1L]] <- quote(stats::model.frame)
    m$formula <- if (missing(data)) 
        terms(formula, "strata")
    else terms(formula, "strata", data = data)
    m <- eval(m, sys.parent())
    Terms <- attr(m, "terms")
    Y <- model.extract(m, "response")
    if (!inherits(Y, "Surv")) {
        if (is.numeric(Y) && is.vector(Y)) 
            Y <- Surv(Y)
        else stop("left hand side of the formula  must be a numeric vector or a surival")
    }
    n <- nrow(Y)
    wt <- model.extract(m, "weights")
    offset <- attr(Terms, "offset")
    if (length(offset) > 0) 
        stop("Offset terms not allowed")
    stemp <- untangle.specials(Terms, "strata")
    if (length(stemp$vars)) {
        if (length(stemp$vars) == 1) 
            strat <- m[[stemp$vars]]
        else strat <- strata(m[, stemp$vars], shortlabel = TRUE)
        Terms <- Terms[-stemp$terms]
    }
    else strat <- NULL
    x <- model.matrix(Terms, m)[, -1, drop = FALSE]
    if (ncol(x) > 1) 
        stop("Only one predictor variable allowed")
    count <- survConcordance.fit(Y, x, strat, wt)
    if (is.null(strat)) {
        concordance <- (count[1] + count[3]/2)/sum(count[1:3])
        std.err <- count[5]/(2 * sum(count[1:3]))
    }
    else {
        temp <- colSums(count)
        concordance <- (temp[1] + temp[3]/2)/sum(temp[1:3])
        std.err <- temp[5]/(2 * sum(temp[1:3]))
    }
    fit <- list(concordance = concordance, stats = count, n = n, 
        std.err = std.err, call = Call)
    na.action <- attr(m, "na.action")
    if (length(na.action)) 
        fit$na.action <- na.action
    oldClass(fit) <- "survConcordance"
    fit
}


match.ratetable <- function (R, ratetable) 
{
    datecheck <- function(x) inherits(x, c("Date", "POSIXt", 
        "date", "chron", "rtabledate"))
    if (!is.ratetable(ratetable)) 
        stop("Invalid rate table")
    dimid <- names(dimnames(ratetable))
    if (is.null(dimid)) 
        dimid <- attr(ratetable, "dimid")
    datecut <- sapply(attr(ratetable, "cutpoints"), datecheck)
    rtype <- attr(ratetable, "type")
    if (is.null(rtype)) {
        temp <- attr(ratetable, "factor")
        rtype <- 1 * (temp == 1) + ifelse(datecut, 3, 2) * (temp == 
            0) + 4 * (temp > 1)
    }
    if (is.matrix(R)) {
        attR <- attributes(R)
        attributes(R) <- attR["dim"]
        Rnames <- attR$dimnames[[2]]
        isDate <- attR[["isDate"]]
        levlist <- attR[["levlist"]]
    }
    else {
        Rnames <- names(R)
        levlist <- lapply(R, levels)
        isDate <- sapply(R, datecheck)
    }
    ord <- match(dimid, Rnames)
    if (any(is.na(ord))) 
        stop(paste("Argument '", dimid[is.na(ord)], "' needed by the ratetable was not found in the data", 
            sep = ""))
    if (any(duplicated(ord))) 
        stop("A ratetable argument appears twice in the data")
    R <- R[, ord, drop = FALSE]
    levlist <- levlist[ord]
    isDate <- isDate[ord]
    dtemp <- dimnames(ratetable)
    if (any((rtype < 3) & isDate)) {
        indx <- which(rtype < 3 & isDate)
        stop(paste("Data has a date type variable, but the reference", 
            "ratetable is not a date variable:", paste(dimid[indx], 
                collapse = " ")))
    }
    if (any((rtype > 2) & !isDate)) {
        indx <- which(rtype > 2 & !isDate)
    }
    for (i in (1:ncol(R))) {
        if (rtype[i] > 2) 
            R[, i] <- ratetableDate(R[, i])
        if (length(levlist[[i]]) > 0) {
            if (rtype[i] != 1) 
                stop(paste("for this ratetable,", dimid[i], "must be a continuous variable"))
            temp <- charmatch(casefold(levlist[[i]]), casefold(dtemp[[i]]))
            if (any(is.na(temp))) 
                stop(paste("Levels do not match for ratetable() variable", 
                  dimid[i]))
            if (any(temp == 0)) 
                stop(paste("Non-unique ratetable match for variable", 
                  dimid[i]))
            R[, i] <- temp[as.numeric(R[, i])]
        }
        else {
            R[, i] <- unclass(R[, i])
            if (rtype[i] == 1) {
                temp <- R[, i]
                if (any(floor(temp) != temp) || any(temp <= 0) || 
                  max(temp) > length(dtemp[[i]])) 
                  stop(paste("The variable", dimid[i], "is out of range"))
            }
        }
    }
    R <- as.matrix(R)
    summ <- attr(ratetable, "summary")
    cutpoints <- lapply(attr(ratetable, "cutpoints"), ratetableDate)
    if (is.null(summ)) 
        list(R = R, cutpoints = cutpoints)
    else list(R = R, cutpoints = cutpoints, summ = summ(R))
}


ratetableDate <- function (x) 
{
    UseMethod("ratetableDate", x)
}


ridge <- function (..., theta, df = nvar/2, eps = 0.1, scale = TRUE) 
{
    x <- cbind(...)
    nvar <- ncol(x)
    xname <- as.character(parse(text = substitute(cbind(...))))[-1]
    vars <- apply(x, 2, function(z) var(z[!is.na(z)]))
    class(x) <- "coxph.penalty"
    if (!missing(theta) && !missing(df)) 
        stop("Only one of df or theta can be specified")
    if (scale) 
        pfun <- function(coef, theta, ndead, scale) {
            list(penalty = sum(coef^2 * scale) * theta/2, first = theta * 
                coef * scale, second = theta * scale, flag = FALSE)
        }
    else pfun <- function(coef, theta, ndead, scale) {
        list(penalty = sum(coef^2) * theta/2, first = theta * 
            coef, second = theta, flag = FALSE)
    }
    if (!missing(theta)) {
        temp <- list(pfun = pfun, diag = TRUE, cfun = function(parms, 
            iter, history) {
            list(theta = parms$theta, done = TRUE)
        }, cparm = list(theta = theta), pparm = vars, varname = paste("ridge(", 
            xname, ")", sep = ""))
    }
    else {
        temp <- list(pfun = pfun, diag = TRUE, cfun = frailty.controldf, 
            cargs = "df", cparm = list(df = df, eps = eps, thetas = 0, 
                dfs = nvar, guess = 1), pparm = vars, varname = paste("ridge(", 
                xname, ")", sep = ""))
    }
    attributes(x) <- c(attributes(x), temp)
    x
}


is.ratetable <- function (x, verbose = FALSE) 
{
    datecheck <- function(x) inherits(x, c("Date", "POSIXt", 
        "date", "chron"))
    att <- attributes(x)
    dlist <- c("dim", "dimnames", "cutpoints")
    isDate <- sapply(att$cutpoints, datecheck)
    dimid <- names(att$dimnames)
    if (is.null(dimid)) 
        dimid <- att$dimid
    if (!verbose) {
        if (!inherits(x, "ratetable")) 
            return(FALSE)
        if (any(is.na(match(dlist, names(att))))) 
            return(FALSE)
        if (is.null(att$dimid)) 
            att$dimid <- names(att$dimnames)
        nd <- length(att$dim)
        if (length(x) != prod(att$dim)) 
            return(FALSE)
        if (is.null(dimid)) 
            return(FALSE)
        if (any(is.na(dimid) | dimid == "")) 
            return(FALSE)
        if (!(is.list(att$dimnames) && is.list(att$cutpoints))) 
            return(FALSE)
        if (length(att$dimnames) != nd || length(att$cutpoints) != 
            nd) 
            return(FALSE)
        if (!is.null(att$type)) {
            if (length(att$type) != nd) 
                return(FALSE)
            if (any(is.na(match(att$type, 1:4)))) 
                return(FALSE)
            if (sum(att$type == 4) > 1) 
                return(FALSE)
            if (any((att$type > 2) != isDate)) 
                return(FALSE)
            fac <- ifelse(att$type == 1, 1, 0)
        }
        else if (!is.null(att$factor)) {
            fac <- as.numeric(att$factor)
            if (any(is.na(fac))) 
                return(FALSE)
            if (any(fac < 0)) 
                return(FALSE)
            if (length(att$factor) != nd) 
                return(FALSE)
            if (sum(fac > 1) > 1) 
                return(FALSE)
            if (any(fac == 1 & isDate)) 
                return(FALSE)
        }
        else return(FALSE)
        if (length(att$dimid) != nd) 
            return(FALSE)
        for (i in 1:nd) {
            n <- att$dim[i]
            if (length(att$dimnames[[i]]) != n) 
                return(FALSE)
            if (fac[i] != 1 && length(att$cutpoints[[i]]) != 
                n) 
                return(FALSE)
            if (fac[i] != 1 && any(order(att$cutpoints[[i]]) != 
                1:n)) 
                return(FALSE)
            if (fac[i] == 1 && !is.null(att$cutpoints[[i]])) 
                return(FALSE)
            if (fac[i] > 1 && i < nd) 
                return(FALSE)
        }
        return(TRUE)
    }
    msg <- NULL
    if (!inherits(x, "ratetable")) 
        msg <- c(msg, "wrong class")
    temp <- is.na(match(dlist, names(att)))
    if (any(temp)) 
        msg <- c(msg, paste("missing attribute:", dlist[temp]))
    if (is.null(att$dimid)) 
        att$dimid <- names(att$dimnames)
    nd <- length(att$dim)
    if (length(x) != prod(att$dim)) 
        msg <- c(msg, "length of the data does not match prod(dim)")
    if (!is.list(att$dimnames)) 
        msg <- c(msg, "dimnames is not a list")
    if (!is.list(att$cutpoints)) 
        msg <- c(msg, "cutpoints is not a list")
    if (length(att$dimnames) != nd) 
        msg <- c(msg, "wrong length for dimnames")
    if (length(att$dimid) != nd) 
        msg <- c(msg, "wrong length for dimid, or dimnames do not have names")
    if (any(att$dimid == "")) 
        msg <- c(msg, "one of the dimnames identifiers is blank")
    if (length(att$cutpoints) != nd) 
        msg <- c(msg, "wrong length for cutpoints")
    if (!is.null(att$type)) {
        if (any(is.na(match(att$type, 1:4)))) 
            msg <- c(msg, "type attribute must be 1, 2, 3, or 4")
        type <- att$type
        if (length(type) != nd) 
            msg <- c(msg, "wrong length for type attribute")
        else {
            indx <- which(type > 2 & !isDate)
            if (length(indx) > 0) 
                msg <- c(msg, paste0("type[", indx, "] is 3 or 4 but the cutpoint is not one of the date types"))
            indx <- which(type < 3 & isDate)
            if (length(indx) > 0) 
                msg <- c(msg, paste0("type[", indx, "] is numeric or factor but the cutpoint is a date"))
        }
        if (sum(type == 4) > 1) 
            msg <- c(msg, "two dimenesions idenitied as US ratetable years")
    }
    else if (!is.null(att$factor)) {
        fac <- as.numeric(att$factor)
        if (any(is.na(fac))) 
            msg <- c(msg, "illegal 'factor' attribute of NA")
        if (any(fac < 0)) 
            msg <- c(msg, "illegal 'factor' attribute of <0")
        if (length(att$factor) != nd) 
            msg <- c(msg, "wrong length for factor")
        type <- 1 * (fac == 1) + 2 * (fac == 0) + 4 * (fac > 
            1)
        if (sum(fac > 1) > 1) 
            msg <- c(msg, "two dimenesions idenitied as US ratetable years")
    }
    else msg <- c(msg, "missing the 'type' attribute")
    for (i in 1:nd) {
        n <- att$dim[i]
        if (length(att$dimnames[[i]]) != n) 
            msg <- c(msg, paste("dimname", i, "is the wrong length"))
        if (type[i] > 1) {
            if (length(att$cutpoints[[i]]) != n) 
                msg <- c(msg, paste("wrong length for cutpoints", 
                  i))
            else if (any(order(att$cutpoints[[i]]) != 1:n)) 
                msg <- c(msg, paste("unsorted cutpoints for dimension", 
                  i))
        }
        if (type[i] == 1 && !is.null(att$cutpoints[[i]])) 
            msg <- c(msg, paste0("attribute type[", i, "] is continuous; cutpoint should be null"))
        if (!is.null(att$fac) && type[i] == 4 && i < nd) 
            msg <- c(msg, "only the last dimension can be interpolated")
    }
    if (length(msg) == 0) 
        TRUE
    else msg
}


aeqSurv <- function (x, tolerance = sqrt(.Machine$double.eps)) 
{
    if (!missing(tolerance)) {
        if (!is.numeric(tolerance) || length(tolerance) != 1 || 
            !is.finite(tolerance)) 
            stop("invalid value for tolerance")
        if (tolerance <= 0) 
            return(x)
    }
    if (!is.Surv(x)) 
        stop("argument is not a Surv object")
    y <- sort(unique(c(x[, -ncol(x)])))
    y <- y[is.finite(y)]
    dy <- diff(y)
    tied <- ((dy <= tolerance) | ((dy/mean(abs(y)) <= tolerance)))
    if (!any(tied)) 
        return(x)
    cuts <- y[c(TRUE, !tied)]
    if (ncol(x) == 2) {
        z <- findInterval(x[, 1], cuts)
        z <- cbind(cuts[z], as.integer(x[, 2]))
    }
    else {
        z <- matrix(findInterval(x[, 1:2], cuts), ncol = 2)
        zeros <- which(z[, 1] == z[, 2])
        if (length(zeros) > 0 && any(x[zeros, 1] != x[zeros, 
            2])) 
            stop("aeqSurv exception, an interval has effective length 0")
        z <- cbind(matrix(cuts[z], ncol = 2), as.integer(x[, 
            3]))
    }
    attributes(z) <- attributes(x)
    z
}


is.na.Surv <- function (x) 
{
    as.vector(rowSums(is.na(unclass(x))) > 0)
}


format.Surv <- function (x, ...) 
format(as.character.Surv(x), ...)


qsurvreg <- function (p, mean, scale = 1, distribution = "weibull", parms) 
{
    dist <- survreg.distributions[[casefold(distribution)]]
    if (is.null(dist)) 
        stop("Distribution not found")
    if (!is.null(dist$trans)) {
        d2 <- survreg.distributions[[dist$dist]]
        x <- d2$quantile(p, parms)
        dist$itrans(x * scale + mean)
    }
    else {
        x <- dist$quantile(p, parms)
        x * scale + mean
    }
}


labels.survreg <- function (object, ...) 
attr(object, "term.labels")


psurvreg <- function (q, mean, scale = 1, distribution = "weibull", parms) 
{
    dist <- survreg.distributions[[casefold(distribution)]]
    if (is.null(dist)) 
        stop("Distribution not found")
    if (!is.null(dist$trans)) {
        q <- dist$trans(q)
        q <- (q - mean)/scale
        dist <- survreg.distributions[[dist$dist]]
        dist$density(q, parms)[, 1]
    }
    else {
        q <- (q - mean)/scale
        dist$density(q, parms)[, 1]
    }
}


survreg.control <- function (maxiter = 30, rel.tolerance = 1e-09, toler.chol = 1e-10, 
    iter.max, debug = 0, outer.max = 10) 
{
    if (missing(iter.max)) {
        iter.max <- maxiter
    }
    else maxiter <- iter.max
    list(iter.max = iter.max, rel.tolerance = rel.tolerance, 
        toler.chol = toler.chol, debug = debug, maxiter = maxiter, 
        outer.max = outer.max)
}




## Package Data

bladder <- survival::bladder		## Bladder Cancer Recurrences

cancer <- survival::cancer		## NCCTG Lung Cancer Data

cgd <- survival::cgd		## Chronic Granulotamous Disease data

colon <- survival::colon		## Chemotherapy for Stage B/C colon cancer

diabetic <- survival::diabetic		## Ddiabetic retinopathy

flchain <- survival::flchain		## Assay of serum free light chain for 7874 subjects.

heart <- survival::heart		## Stanford Heart Transplant data

kidney <- survival::kidney		## Kidney catheter data

leukemia <- survival::leukemia		## Acute Myelogenous Leukemia survival data

logan <- survival::logan		## Data from the 1972-78 GSS data used by Logan

lung <- survival::lung		## NCCTG Lung Cancer Data

mgus <- survival::mgus		## Monoclonal gammopathy data

mgus2 <- survival::mgus2		## Monoclonal gammopathy data

myeloid <- survival::myeloid		## Acute myeloid leukemia

nwtco <- survival::nwtco		## Data from the National Wilm's Tumor Study

ovarian <- survival::ovarian		## Ovarian Cancer Survival Data

pbc <- survival::pbc		## Mayo Clinic Primary Biliary Cirrhosis Data

rats <- survival::rats		## Rat treatment data from Mantel et al

retinopathy <- survival::retinopathy		## Diabetic Retinopathy

rhDNase <- survival::rhDNase		## rhDNASE data set

solder <- survival::solder		## Data from a soldering experiment

stanford2 <- survival::stanford2		## More Stanford Heart Transplant data

tobin <- survival::tobin		## Tobin's Tobit data

transplant <- survival::transplant		## Liver transplant waiting list

udca <- survival::udca		## Data from a trial of usrodeoxycholic acid

uspop2 <- survival::uspop2		## Projected US Population

veteran <- survival::veteran		## Veterans' Administration Lung Cancer study



## Package Info

.skeleton_package_title = "Survival Analysis"

.skeleton_package_version = "2.44-1.1"

.skeleton_package_depends = ""

.skeleton_package_imports = "graphics,Matrix,methods,splines,stats,utils"


## Internal

.skeleton_version = 5


## EOF
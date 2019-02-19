# Version 1.0.1 -----------------------------------------------------------

hri <- function (
  nStudents = ncol(s.prefs), nColleges = ncol(c.prefs),
  nSlots = rep(1, nColleges), s.prefs = NULL, c.prefs = NULL,
  s.range = NULL, c.range = NULL, randomization = NULL, seed = NULL,
  check_consistency = TRUE, ...
) {
  UseMethod("hri")
}

matchingMarkets:::hri.default <- function (
  nStudents = ncol(s.prefs), nColleges = ncol(c.prefs),
  nSlots = rep(1, nColleges), s.prefs = NULL, c.prefs = NULL,
  s.range = NULL, c.range = NULL, randomization = "multiple",
  seed = NULL, check_consistency = TRUE, ...
) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  if (is.null(nColleges)) {
    nColleges <- length(nSlots)
  }
  if (is.null(s.prefs)) {
    s.prefs <- replicate(n = nStudents, sample(seq(
      from = 1,
      to = nColleges, by = 1
    )))
  }
  if (is.null(c.prefs)) {
    if (randomization == "single") {
      c.prefs <- matrix(sample(seq(
        from = 1, to = nStudents,
        by = 1
      )), nrow = nStudents, ncol = nColleges)
    }
    else {
      c.prefs <- replicate(n = nColleges, sample(seq(
        from = 1,
        to = nStudents, by = 1
      )))
    }
  }
  if (length(nSlots) != nColleges |
      length(nSlots) != dim(c.prefs)[2]) {
    stop("Length of 'nSlots' must equal 'nColleges' and the number of columns of 'c.prefs'!")
  }
  if (!is.null(colnames(s.prefs)) || !is.null(colnames(c.prefs))) {
    if (is.numeric(c.prefs) || is.numeric(s.prefs)) {
      warning(
        "Colnames are given but preferences matrizes are numeric. Make sure that they are consistent!"
      )
    }
  }
  prefs_char <- FALSE
  if (is.character(s.prefs) || is.character(c.prefs)) {
    if (!(is.character(s.prefs) && is.character(c.prefs))) {
      stop("Both prefs must be in characters")
    }
    prefs_char <- TRUE
  }
  if (is.null(colnames(s.prefs))) {
    colnames(s.prefs) <- 1:ncol(s.prefs)
  }
  if (is.null(colnames(c.prefs))) {
    colnames(c.prefs) <- 1:ncol(c.prefs)
  }
  c.names <- colnames(c.prefs)
  s.names <- colnames(s.prefs)
  c.prefs_named <- NULL
  s.prefs_named <- NULL
  if (check_consistency) {
    if (length(unique(colnames(s.prefs))) != ncol(s.prefs)) {
      stop("Student names not unique")
    }
    if (length(unique(colnames(c.prefs))) != ncol(c.prefs)) {
      stop("College/Course names not unique")
    }
    applied_colleges <- unique(as.character(s.prefs))
    applied_colleges <- applied_colleges[!is.na(applied_colleges)]
    if (length(setdiff(applied_colleges, colnames(c.prefs))) !=
        0) {
      missing_college <- setdiff(applied_colleges, colnames(c.prefs))
      missing_college <-
        paste(
          "Someone applied to a college (named ",
          missing_college,
          ") that has no ranking. This preference entries will be deleted! \n",
          sep = ""
        )
      warning(missing_college)
      s.prefs <- sapply(s.names, function(z) {
        x <- c(na.omit(s.prefs[, z]))
        if (length(x) == 0)
          return(rep(NA, length(s.prefs[, z])))
        y <- sapply(x, function(i)
          i %in% colnames(c.prefs))
        return(c(x[y], rep(NA, nrow(s.prefs) - length(x[y]))))
      })
    }
    ranked_stud <- unique(c(as.character(c.prefs)))
    ranked_stud <- ranked_stud[!is.na(ranked_stud)]
    if (length(setdiff(ranked_stud, colnames(s.prefs))) !=
        0) {
      missing_stud <- setdiff(ranked_stud, colnames(s.prefs))
      missing_stud <-
        paste(
          "A course/college ranked a student (named ",
          missing_stud,
          ") that has no ranking.  This preference entries will be deleted! \n",
          sep = ""
        )
      warning(missing_stud)
    }
    c.prefs <- sapply(c.names, function(z) {
      x <- c(na.omit(c.prefs[, z]))
      if (length(x) == 0)
        return(rep(NA, length(c.prefs[, z])))
      y <- sapply(x, function(i)
        i %in% colnames(s.prefs))
      return(c(x[y], rep(NA, nrow(c.prefs) - length(x[y]))))
    })
  }
  if (!is.null(s.range)) {
    thre.s <- sample(x = s.range[1]:s.range[2],
                     size = ncol(s.prefs),
                     replace = TRUE)
    s.prefs <-
      sapply(1:ncol(s.prefs), function(z)
        c(s.prefs[1:thre.s[z],
                  z], rep(NA, nrow(s.prefs) - thre.s[z])))
    colnames(s.prefs) <- s.names
    rownames(s.prefs) <- NULL
  }
  if (!is.null(c.range)) {
    thre.c <- sample(x = c.range[1]:c.range[2],
                     size = ncol(c.prefs),
                     replace = TRUE)
    c.prefs <-
      sapply(1:ncol(c.prefs), function(z)
        c(c.prefs[1:thre.c[z],
                  z], rep(NA, nrow(c.prefs) - thre.c[z])))
    colnames(c.prefs) <- c.names
    rownames(c.prefs) <- NULL
  }
  c.prefs <- sapply(c.names, function(z) {
    x <- c(na.omit(c.prefs[, z]))
    if (length(x) == 0)
      return(rep(NA, length(c.prefs[, z])))
    y <- sapply(x, function(i)
      z %in% s.prefs[, i])
    return(c(x[y], rep(NA, nrow(c.prefs) - length(x[y]))))
  })
  s.prefs <- sapply(s.names, function(z) {
    x <- c(na.omit(s.prefs[, z]))
    if (length(x) == 0)
      return(rep(NA, length(s.prefs[, z])))
    y <- sapply(x, function(i)
      z %in% c.prefs[, i])
    return(c(x[y], rep(NA, nrow(s.prefs) - length(x[y]))))
  })
  drop <- which(apply(s.prefs, 2, function(z)
    all(is.na(z))))
  if (length(drop) > 0) {
    s.prefs <- matrix(s.prefs[,-drop], nrow = nrow(s.prefs))
    colnames(s.prefs) <- s.names[-drop]
    print(paste("Dropped s.prefs column(s):", paste(s.names[drop],
                                                    collapse = ", ")))
  }
  drop <- which(apply(c.prefs, 2, function(z)
    all(is.na(z))))
  if (length(drop) > 0) {
    c.prefs <- matrix(c.prefs[,-drop], nrow = nrow(c.prefs))
    colnames(c.prefs) <- c.names[-drop]
    print(paste("Dropped c.prefs column(s):", paste(c.names[drop],
                                                    collapse = ", ")))
    nSlots <- nSlots[-drop]
  }
  s.prefs_named <- s.prefs
  c.prefs_named <- c.prefs
  s.names <- 1:ncol(s.prefs)
  names(s.names) <- colnames(s.prefs)
  c.names <- 1:ncol(c.prefs)
  names(c.names) <- colnames(c.prefs)
  s.names <- s.names
  c.names <- c.names
  s.prefs <- apply(s.prefs, 2, function(pref) {
    c.names[as.character(pref)]
  })
  colnames(s.prefs) <- s.names[colnames(s.prefs)]
  s.prefs <- s.prefs
  c.prefs <- apply(c.prefs, 2, function(pref) {
    s.names[as.character(pref)]
  })
  colnames(c.prefs) <- c.names[colnames(c.prefs)]
  c.prefs <- c.prefs
  if (sum(nSlots) != length(nSlots)) {
    if (!prefs_char) {
      s.prefs.hri <- s.prefs
      c.prefs.hri <- c.prefs
    }
    sm <- c2m(s.prefs, c.prefs, nSlots)
    s.prefs <- sm$s.prefs
    c.prefs <- sm$c.prefs
    collegeSlots <- sm$collegeSlots
    rm(sm)
  }
  c.matrix <- sapply(1:nrow(t(c.prefs)), function(z)
    paste(t(c.prefs)[z,][!is.na(t(c.prefs)[z,])], collapse = " "))
  s.prefs1 <- s.prefs + ncol(s.prefs)
  s.matrix <- sapply(1:nrow(t(s.prefs1)), function(z)
    paste(t(s.prefs1)[z,][!is.na(t(s.prefs1)[z,])], collapse = " "))
  instance <- paste(c(length(s.matrix) + length(c.matrix),
                      s.matrix, c.matrix), collapse = "n")
  hjw <- .jnew("smi")
  out <- .jcall(hjw, "S", "sayHello", .jarray(instance))
  out <- as.list(strsplit(out, split = "\n\n")[[1]])[[1]]
  out <- gsub(",", " ", out)
  out <- data.frame(matrix(
    scan(
      text = out,
      what = integer(),
      quiet = TRUE
    ),
    ncol = 3,
    byrow = TRUE
  ))
  names(out) <- c("matching", "student", "college")
  out$college <- out$college - ncol(s.prefs)
  A <- split(out, out$matching)
  for (j in 1:length(A)) {
    A[[j]]$sRank <- sapply(1:nrow(A[[j]]), function(z)
      which(s.prefs[,
                    A[[j]]$student[z]] == A[[j]]$college[z]))
    A[[j]]$cRank <-
      sapply(1:nrow(A[[j]]), function(z)
        which(c.prefs[,
                      A[[j]]$college[z]] == A[[j]]$student[z]))
  }
  sums <- unlist(lapply(A, function(z)
    sum(z$sRank)))
  sopt.id <- which(sums == min(sums))
  sums <- unlist(lapply(A, function(z)
    sum(z$cRank)))
  copt.id <- which(sums == min(sums))
  out <- with(out, data.frame(out, sOptimal = 0, cOptimal = 0))
  out$sOptimal[out$matching == sopt.id] <- 1
  out$cOptimal[out$matching == copt.id] <- 1
  out <- cbind(out, do.call("rbind", A)[, c("sRank", "cRank")])
  if (sum(nSlots) != length(nSlots)) {
    out <- merge(
      x = out,
      y = collegeSlots,
      by.x = "college",
      by.y = "slots"
    )
    out$student <- colnames(s.prefs_named)[out$student]
    out$colleges <- colnames(c.prefs_named)[out$colleges]
    out <-
      with(
        out,
        data.frame(
          matching = matching,
          college = colleges,
          slots = college,
          student = student,
          sOptimal = sOptimal,
          cOptimal = cOptimal,
          sRank = sRank,
          cRank = cRank,
          stringsAsFactors = FALSE
        )
      )
  }
  else {
    out$student <- colnames(s.prefs_named)[out$student]
    out$college <- colnames(c.prefs_named)[out$college]
    out <-
      with(
        out,
        data.frame(
          matching = matching,
          college = college,
          slots = college,
          student = student,
          sOptimal = sOptimal,
          cOptimal = cOptimal,
          sRank = sRank,
          cRank = cRank,
          stringsAsFactors = FALSE
        )
      )
  }
  out <- with(out, out[order(matching, college, student),])
  rownames(out) <- NULL
  if (sum(nSlots) != length(nSlots)) {
    res <- list(
      s.prefs.smi = s.prefs[rowSums(is.na(s.prefs)) <
                              ncol(s.prefs),],
      c.prefs.smi = c.prefs[rowSums(is.na(c.prefs)) <
                              ncol(c.prefs),],
      s.prefs.hri = s.prefs_named[rowSums(is.na(s.prefs_named)) <
                                    ncol(s.prefs_named),],
      c.prefs.hri = c.prefs_named[rowSums(is.na(c.prefs_named)) <
                                    ncol(c.prefs_named),],
      matchings = out
    )
  }
  else {
    res <- list(
      s.prefs.smi = s.prefs_named[rowSums(is.na(s.prefs)) <
                                    ncol(s.prefs),],
      c.prefs.smi = c.prefs_named[rowSums(is.na(c.prefs)) <
                                    ncol(c.prefs),],
      matchings = out
    )
  }
  class(res) <- "hri"
  return(res
  )
}

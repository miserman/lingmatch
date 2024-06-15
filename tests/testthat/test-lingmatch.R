words <- c(
  sub("*", "", unlist(lma_dict(1:9, as.regex = FALSE), use.names = FALSE), fixed = TRUE),
  vapply(seq_len(200), function(w) paste0(sample(letters, sample(9, 1)), collapse = ""), "")
)
texts <- vapply(seq_len(50), function(d) {
  paste0(sample(words, sample(100, 1), TRUE), collapse = " ")
}, "")
textsfile <- tempfile(fileext = ".csv")
write.csv(data.frame(
  id = seq_along(texts),
  text = texts
), textsfile, row.names = FALSE)
txtfile <- tempfile(fileext = ".txt")
writeLines(texts, txtfile)
dtm <- lma_dtm(texts)

test_that("different input formats have the same results", {
  manual <- as.numeric(lma_simets(dtm, metric = "cosine"))
  expect_equal(as.numeric(lingmatch(textsfile, metric = "x")$sim), manual)
  expect_equal(as.numeric(lingmatch(txtfile)$sim), manual)
  expect_equal(as.numeric(lingmatch(texts)$sim), manual)
  expect_equal(as.numeric(lingmatch(dtm)$sim), manual)
  expect_equal(as.numeric(lingmatch(as.data.frame(as.matrix(dtm)))$sim), manual)
  expect_equal(as.numeric(lingmatch(matrix(as.character(dtm), nrow(dtm)))$sim), manual)
  expect_warning(
    dropped <- lingmatch(cbind(as.data.frame(as.matrix(dtm)), id = "a"))$sim,
    "some input variables were not"
  )
  expect_equal(as.numeric(dropped), manual)
  expect_equal(as.numeric(lingmatch(data = dtm)$sim), manual)
  data <- data.frame(text = as.factor(texts))
  base <- as.numeric(lingmatch(data$text, symmetrical = TRUE)$sim)
  expect_equal(
    as.numeric(lingmatch("text", "text", pairwise = TRUE, data = data)$sim),
    base
  )
  expect_equal(
    as.numeric(lingmatch("text", "text", pairwise = TRUE, data = data)$sim),
    base
  )
  expect_equal(
    as.numeric(lingmatch(data$text, data$text, pairwise = TRUE)$sim),
    base
  )
})

test_that("different input formats have the same results (comp)", {
  manual <- as.numeric(lma_simets(dtm[-(1:10), ], dtm[1:10, ], metric = "cosine"))
  expect_equal(as.numeric(lingmatch(textsfile, 1:10)$sim), manual)
  expect_equal(as.numeric(lingmatch(texts[-(1:10)], texts[1:10])$sim), manual)
  expect_equal(as.numeric(lingmatch(texts[-(1:10)], as.factor(texts[1:10]))$sim), manual)
  expect_equal(as.numeric(lingmatch(dtm[-(1:10), ], dtm[1:10, ])$sim), manual)
  expect_equal(as.numeric(lingmatch(
    as.data.frame(as.matrix(dtm[-(1:10), ])),
    as.data.frame(as.matrix(dtm[1:10, ]))
  )$sim), manual)
  expect_equal(
    as.numeric(lingmatch(dtm[-(1:10), -1], dtm[1:10, ])$sim),
    as.numeric(lma_simets(dtm[-(1:10), -1], dtm[1:10, ], metric = "cosine"))
  )
})

test_that("different input formats have the same results (LSM)", {
  wdtm <- lma_weight(dtm, percent = TRUE)
  cdtm <- lma_termcat(wdtm)
  manual <- as.numeric(lma_simets(cdtm, "canberra"))
  expect_equal(as.numeric(lingmatch(texts, type = "lsm")$sim), manual)
  expect_equal(as.numeric(lingmatch(dtm, type = "lsm")$sim), manual)
  expect_equal(as.numeric(lingmatch(wdtm, type = "lsm")$sim), manual)
  expect_equal(as.numeric(lingmatch(cdtm, type = "lsm")$sim), manual)
  colnames(cdtm)[c(3, 6)] <- c("articles", "preps")
  expect_equal(as.numeric(lingmatch(cdtm, type = "lsm")$sim), manual)
  colnames(cdtm) <- paste0("liwc.", c(
    "personal_pronouns", "impersonal_pronouns", "articles", "auxiliary_verbs",
    "adverbs", "prepositions", "conjunctions", "negations", "quantifiers"
  ))
  expect_equal(as.numeric(lingmatch(cdtm, type = "lsm")$sim), manual)
})

test_that("different input formats have the same results (LSA)", {
  space <- lma_lspace(dtm)
  wdtm <- lma_weight(dtm, "tfidf")
  cdtm <- lma_lspace(wdtm, space)
  manual <- as.numeric(lma_simets(cdtm, metric = "cosine"))
  expect_equal(as.numeric(lingmatch(texts, space = space, type = "lsa")$sim), manual)
  expect_equal(as.numeric(lingmatch(dtm, space = space, type = "lsa")$sim), manual)
  expect_equal(as.numeric(lingmatch(wdtm, space = space, type = "lsa")$sim), manual)
  expect_equal(as.numeric(lingmatch(cdtm, type = "lsa")$sim), manual)
})

test_that("comparison columns align (named)", {
  wdtm <- lma_weight(dtm, percent = TRUE)
  cdtm <- lma_termcat(wdtm)
  manual <- as.numeric(lma_simets(cdtm, cdtm[1:5, ], "canberra"))
  expect_equal(as.numeric(lingmatch(texts, cdtm[1:5, ], type = "lsm")$sim), manual)
  expect_equal(as.numeric(lingmatch(dtm, cdtm[1:5, ], type = "lsm")$sim), manual)
  expect_equal(as.numeric(lingmatch(wdtm, cdtm[1:5, ], type = "lsm")$sim), manual)
  expect_equal(as.numeric(lingmatch(cdtm, cdtm[1:5, ], type = "lsm")$sim), manual)
})

test_that("comparison columns align (unnamed)", {
  space <- lma_lspace(dtm)
  wdtm <- lma_weight(dtm, "tfidf")
  cdtm <- lma_lspace(wdtm, space)
  manual <- as.numeric(lma_simets(cdtm, cdtm[1:5, ], metric = "cosine"))
  expect_equal(as.numeric(lingmatch(texts, cdtm[1:5, ], space = space, type = "lsa")$sim), manual)
  expect_equal(as.numeric(lingmatch(dtm, cdtm[1:5, ], space = space, type = "lsa")$sim), manual)
  expect_equal(as.numeric(lingmatch(wdtm, cdtm[1:5, ], space = space, type = "lsa")$sim), manual)
  expect_equal(as.numeric(lingmatch(cdtm, cdtm[1:5, ], space = space, type = "lsa")$sim), manual)
})

test_that("drop works", {
  ddtm <- dtm
  ddtm[, 1] <- 0
  expect_identical(
    as.numeric(lingmatch(ddtm, drop = TRUE)$sim),
    as.numeric(lingmatch(dtm[, -1])$sim)
  )
})

test_that("function comparisons work", {
  expect_equal(
    as.numeric(lingmatch(dtm, sum)$sim),
    as.numeric(lingmatch(dtm, colSums(dtm))$sim)
  )
})

test_that("profile comparisons work", {
  novels <- c(
    ppron = 10.35, ipron = 4.79, article = 8.35, auxverb = 7.77, adverb = 4.17,
    prep = 14.27, conj = 6.28, negate = 1.68, quant = 1.8
  )
  expect_equal(
    as.numeric(lingmatch(dtm, "novels", type = "lsm")$sim),
    as.numeric(lingmatch(dtm, novels, type = "lsm")$sim)
  )
})

test_that("index/logical comparisons work", {
  ind <- sample(nrow(dtm), 5)
  su <- seq_len(nrow(dtm)) %in% ind
  expect_equal(
    as.numeric(lingmatch(dtm, ind)$sim),
    as.numeric(lingmatch(dtm, dtm[ind, ])$sim[-ind, ])
  )
  expect_equal(
    as.numeric(lingmatch(dtm, su)$sim),
    as.numeric(lingmatch(dtm, dtm[su, ])$sim[!su, ])
  )
})

test_that("comp is handled as expected", {
  data <- data.frame(
    group = rep(letters[1:3], each = 3),
    x1 = rnorm(9),
    x2 = rnorm(9)
  )
  expect_error(lingmatch(data[, -1], comp = "group"), "`comp` not recognized")
  all_groups <- lingmatch(
    data[, -1], do.call(rbind, tapply(data[, -1], data$group, colMeans)),
    metric = "canberra"
  )
  within_groups <- lingmatch(
    data[, -1],
    group = "group", data = data, metric = "canberra"
  )
  expect_identical(diag(all_groups$sim[, data$group]), within_groups$sim$canberra)
})

test_that("group comparisons work", {
  wdtm <- lma_weight(dtm, percent = TRUE)
  cdtm <- lma_termcat(wdtm)
  groups <- rep(c("a", "b"), each = nrow(dtm) / 2)
  group_means <- rbind(
    a = colMeans(cdtm[groups == "a", ]),
    b = colMeans(cdtm[groups == "b", ])
  )
  g1 <- lingmatch(dtm, group = groups, type = "lsm")$sim[, 2]
  expect_equal(g1, rowSums(lingmatch(dtm, group_means, type = "lsm")$sim * rep(
    c(1, 0, 1), nrow(dtm) / c(2, 1, 2)
  )))
  expect_true(class(lingmatch(dtm, type = "lsm", symmetrical = FALSE)$sim)[1] == "dtCMatrix")
  pw <- lingmatch(dtm, type = "lsm", symmetrical = TRUE)$sim
  expect_equal(
    as.numeric(lingmatch(dtm, "pairwise", symmetrical = TRUE, group = groups, type = "lsm")$sim$a),
    as.numeric(pw[groups == "a", groups == "a"])
  )
  expect_equal(
    lingmatch(dtm, "pairwise", mean = TRUE, group = groups, type = "lsm")$sim[, 2],
    (c(
      rowSums(pw[groups == "a", groups == "a"]),
      rowSums(pw[groups == "b", groups == "b"])
    ) - 1) / (nrow(dtm) / 2 - 1)
  )
  subgroups <- sample(c("x", "y"), nrow(dtm), TRUE)
  all_groups <- paste(groups, subgroups)
  sgmeans <- t(vapply(split(as.data.frame(cdtm), all_groups), colMeans, numeric(9)))
  g1_2 <- lingmatch(dtm, group = cbind(groups, subgroups), type = "lsm", all.levels = TRUE)$sim
  expect_equal(g1, g1_2$g1_canberra)
  expect_equal(g1_2$g1_g2_canberra, vapply(seq_along(all_groups), function(i) {
    lma_simets(cdtm[i, ], sgmeans[all_groups[i], ], "can")
  }, 0))
  group_data <- cbind(groups, subgroups)
  expect_equal(as.numeric(lingmatch(
    dtm,
    group = group_data, type = "lsm", all.levels = TRUE
  )$sim$g1_canberra), g1)

  g1_2p <- lingmatch(dtm, "pair", group = cbind(groups, subgroups), type = "lsm", mean = FALSE)$sim
  for (s in names(g1_2p)) {
    expect_identical(
      as.numeric(g1_2p[[s]]),
      as.numeric(lingmatch(dtm[all_groups == s, ], "pair", type = "lsm", mean = FALSE)$sim)
    )
  }
})

test_that("groups work through data", {
  data <- data.frame(
    id = as.factor(rep(seq_len(nrow(dtm) / 2), 2)),
    group = as.factor(rep(c("a", "b"), each = nrow(dtm) / 2)),
    lma_termcat(lma_weight(dtm, percent = TRUE))
  )
  categories <- colnames(data)[-(1:2)]
  group_means <- t(vapply(split(data[, categories], data$group), colMeans, numeric(9)))
  manual <- as.numeric(c(
    lma_simets(data[data$group == "a", categories], group_means["a", ], "canberra"),
    lma_simets(data[data$group == "b", categories], group_means["b", ], "canberra")
  ))
  expect_error(lingmatch(data, group = noexist, type = "lsm"), "could not find noexist")
  expect_equal(
    lingmatch(data, group = "group", type = "lsm")$sim[, 2],
    as.numeric(c(
      lma_simets(data[data$group == "a", categories], group_means["a", ], "canberra"),
      lma_simets(data[data$group == "b", categories], group_means["b", ], "canberra")
    ))
  )
  id_means <- t(vapply(split(data[, categories], data$id), colMeans, numeric(9)))
  expect_equal(
    lingmatch(data, group = id, type = "lsm")$sim[, 2],
    vapply(seq_len(nrow(data)), function(i) {
      r <- data[i, ]
      lma_simets(r[categories], id_means[r[[1]], ], "canberra")
    }, 0)
  )
})

test_that("comp.group and comp.data work", {
  data <- data.frame(
    id = as.factor(rep(seq_len(nrow(dtm) / 2), 2)),
    condition = rbinom(nrow(dtm), 1, .5),
    group = as.factor(rep(c("a", "b"), each = nrow(dtm) / 2)),
    subgroup = as.factor(rep(c("a", "b"), nrow(dtm) / 2)),
    lma_termcat(lma_weight(dtm, percent = TRUE))
  )
  cats <- names(lma_dict(1:9))
  data$prep <- 0
  data[1, cats] <- 0
  sdat <- split(data[, cats], paste0(data$group, data$condition))
  pairs <- list(a = lma_simets(sdat$a0, sdat$a1, metric = "can"), b = lma_simets(sdat$b0, sdat$b1, metric = "can"))
  expect_identical(
    as.numeric((rowSums(pairs$a) - 1) / (ncol(pairs$a) - 1)),
    as.numeric(lma_simets(sdat$a0, sdat$a1, metric = "can", mean = TRUE))
  )
  c1mean <- list(
    a = lma_simets(sdat$a0, colMeans(sdat$a1), metric = "can"),
    b = lma_simets(sdat$b0, colMeans(sdat$b1), metric = "can")
  )
  expect_equal(
    lingmatch(
      data[data$condition == 0, ],
      comp.data = data[data$condition == 1, ],
      comp.group = group, type = "lsm"
    )$sim$canberra,
    as.numeric(unlist(c1mean))
  )
  pairwise_a <- as.numeric(lma_simets(
    data[data$condition == 0 & data$group == "a", cats],
    data[data$condition == 1 & data$group == "a", cats],
    metric = "canberra"
  ))
  expect_identical(
    as.numeric(lingmatch(
      data[data$condition == 0, ], data[data$condition == 1, ],
      comp.group = group, type = "lsm"
    )$sim$a$canberra),
    pairwise_a
  )
  comp <- data[data$condition == 1, ]
  colnames(comp)[3:4] <- c("c1", "c2")
  expect_identical(
    as.numeric(lingmatch(
      data[data$condition == 0, ], comp,
      all.levels = TRUE, pairwise = TRUE,
      group = c("group", "subgroup"), comp.group = c("c1", "c2"), type = "lsm"
    )$sim$a$a$g1_canberra),
    pairwise_a
  )
  colnames(sdat$a0)[c(3, 6)] <- colnames(sdat$a1)[c(3, 6)] <- c("articles", "preps")
  expect_true(all(lingmatch(sdat$a0, colMeans(sdat$a1), metric = "can")$sim == c1mean$a))
  expect_equal(as.numeric(lingmatch(sdat$a0, comp.data = sdat$a1, metric = "can")$sim), as.numeric(c1mean$a))
  auto <- lingmatch(sdat$a0, "auto")
  expect_equal(
    as.numeric(auto$sim),
    as.numeric(lingmatch(sdat$a0, sub("auto: ", "", auto$comp.type, fixed = TRUE))$sim)
  )
})

test_that("sequential comparisons work", {
  data <- data.frame(
    pair = sort(sample(1:4, nrow(dtm), TRUE)),
    phase = 0,
    speaker = sample(c("a", "b"), nrow(dtm), TRUE),
    lma_termcat(lma_weight(dtm, percent = TRUE))
  )
  for (p in 1:4) {
    su <- data$pair == p
    data$phase[su] <- sort(sample(1:3, sum(su), TRUE))
  }
  cats <- names(lma_dict(1:9))
  h <- list(
    lingmatch(data, "seq", dict = cats, group = speaker)$sim,
    lingmatch(data, "seq", dict = cats, group = "speaker", agg = FALSE)$sim,
    lingmatch(data, "seq", dict = cats, group = c("pair", "speaker"))$sim,
    lingmatch(data, "seq", dict = cats, group = data[, c("pair", "speaker")], agg = FALSE)$sim,
    lingmatch(data, "seq", dict = cats, group = data[, c("pair", "phase", "speaker")])$sim,
    lingmatch(data, "seq", dict = cats, group = c("pair", "phase", "speaker"), agg = FALSE)$sim
  )
  expect_true(any(h[[1]][, "cosine"] - h[[2]][, "cosine"] > .01))
  expect_true(any(h[[3]][, "cosine"] - h[[4]][, "cosine"] > .01))
  expect_true(any(h[[5]][, "cosine"] - h[[6]][, "cosine"] > .01))
  for (comp in h) {
    manual <- vapply(strsplit(rownames(comp), " <-> ", fixed = TRUE), function(s) {
      if (length(s) == 1) {
        1
      } else {
        s <- strsplit(s, ", ", fixed = TRUE)
        lma_simets(
          colMeans(data[as.numeric(s[[1]]), -(1:3), drop = FALSE]),
          colMeans(data[as.numeric(s[[2]]), -(1:3), drop = FALSE]),
          "cosine"
        )
      }
    }, 0)
    expect_equal(comp[, "cosine"], manual)
  }
  reorder <- sample(seq_len(nrow(data)))
  expect_equal(h[[1]][, 1], lingmatch(
    data[reorder, ], "seq",
    dict = cats, group = speaker, order = order(reorder)
  )$sim[, 1])
})

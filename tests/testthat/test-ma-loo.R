library(testthat)

source("code/meta-analysis/fit-ma.R")
source("code/meta-analysis/ma-loo.R")

fake_fit <- function() structure(list(), class = "brmsfit")
fake_loo <- function(elpd, k = rep(0.5, length(elpd)), se = rep(0.1, length(elpd)), ids = letters[seq_along(elpd)]) {
  structure(
    list(
      pointwise = structure(cbind(elpd_loo = elpd, se_elpd_loo = se, p_loo = 0, looic = -2 * elpd), dimnames = list(ids, c("elpd_loo", "se_elpd_loo", "p_loo", "looic"))),
      diagnostics = list(pareto_k = k)
    ),
    class = "loo"
  )
}


fake_kfold <- function(elpd, se = rep(0.1, length(elpd)), ids = letters[seq_along(elpd)]) {
  structure(
    list(
      pointwise = structure(cbind(elpd_kfold = elpd, se_elpd_kfold = se), dimnames = list(ids, c("elpd_kfold", "se_elpd_kfold")))
    ),
    class = "kfold"
  )
}

test_that("loo_ma_fits averages pointwise ELPD across imputations and keeps Pareto summaries", {
  calls <- list()
  loo_fun <- function(fit, resp, re_formula, moment_match, ...) {
    calls[[length(calls) + 1L]] <<- list(resp = resp, re_formula = re_formula, moment_match = moment_match, extra = list(...))
    fake_loo(fit$elpd, fit$k, ids = c("a", "b", "c"))
  }
  fits <- list(
    structure(list(elpd = c(1, 2, 3), k = c(0.2, 0.8, 0.1)), class = "brmsfit"),
    structure(list(elpd = c(3, 4, 5), k = c(0.3, 0.4, 0.5)), class = "brmsfit")
  )
  res <- loo_ma_fits(fits, resp = "ylogit", re_formula = NA, moment_match = TRUE, save_psis = TRUE, .loo_fun = loo_fun)
  expect_s3_class(res, "ma_loo")
  expect_equal(res$elpd_pointwise, c(a = 2, b = 3, c = 4))
  expect_equal(res$elpd_total, 9)
  expect_equal(res$nimp, 2)
  expect_equal(res$max_pareto_k, 0.8)
  expect_equal(res$n_problematic_imputations, 1)
  expect_true(all(vapply(calls, `[[`, character(1), "resp") == "ylogit"))
  expect_true(all(vapply(calls, `[[`, logical(1), "moment_match")))
  expect_true(all(vapply(calls, function(x) is.na(x$re_formula), logical(1))))
  expect_true(all(vapply(calls, function(x) isTRUE(x$extra$save_psis), logical(1))))
})



test_that("build_ma_loo_result accepts LOO and K-fold pointwise ELPD column names", {
  loo_res <- build_ma_loo_result(list(fake_loo(c(1, 2), ids = c("a", "b"))))
  expect_equal(ma_elpd_column(loo_res$loo[[1]]$pointwise), "elpd_loo")
  expect_equal(ma_elpd_column(loo_res$loo[[1]]$pointwise, se = TRUE), "se_elpd_loo")
  expect_named(loo_res$pointwise, c("observation", "elpd_loo", "se_elpd_loo"))

  kfold_res <- build_ma_loo_result(list(
    fake_kfold(c(1, 2), ids = c("a", "b")),
    fake_kfold(c(3, 4), ids = c("a", "b"))
  ))
  expect_equal(ma_elpd_column(kfold_res$loo[[1]]$pointwise), "elpd_kfold")
  expect_equal(ma_elpd_column(kfold_res$loo[[1]]$pointwise, se = TRUE), "se_elpd_kfold")
  expect_named(kfold_res$pointwise, c("observation", "elpd_kfold", "se_elpd_kfold"))
  expect_equal(kfold_res$elpd_pointwise, c(a = 2, b = 3))
})


test_that("compare_ma_loo computes unclustered and paper-clustered paired SE", {
  a <- build_ma_loo_result(list(fake_loo(c(2, 5, 8, 11), ids = as.character(1:4))))
  b <- build_ma_loo_result(list(fake_loo(c(1, 3, 7, 7), ids = as.character(1:4))))
  cmp <- compare_ma_loo(a, b, model_names = c("full", "null"))
  d <- c(1, 2, 1, 4)
  expect_equal(cmp$elpd_diff, sum(d))
  expect_equal(cmp$se_diff, sqrt(length(d) * stats::var(d)))
  expect_match(cmp$direction, "positive favors full")
  cl <- compare_ma_loo(a, b, cluster = c("p1", "p1", "p2", "p2"), model_names = c("full", "null"))
  dg <- c(3, 5)
  expect_equal(cl$se_diff, sqrt(length(dg) * stats::var(dg)))
  expect_equal(cl$n_clusters, 2)
})

test_that("LOO builders and comparisons reject mismatched observations and imputation counts", {
  expect_error(build_ma_loo_result(list(fake_loo(1:2), fake_loo(1:3))), "same number")
  expect_error(build_ma_loo_result(list(fake_loo(1:2, ids = c("a", "b")), fake_loo(1:2, ids = c("b", "a")))), "same observations")
  a <- build_ma_loo_result(list(fake_loo(1:2, ids = c("a", "b"))))
  b <- build_ma_loo_result(list(fake_loo(1:2, ids = c("b", "a"))))
  expect_error(compare_ma_loo(a, b), "same observations")
  c <- build_ma_loo_result(list(fake_loo(1:2, ids = c("a", "b")), fake_loo(2:3, ids = c("a", "b"))))
  d <- build_ma_loo_result(list(fake_loo(1:2, ids = c("a", "b")), fake_loo(2:3, ids = c("a", "b")), fake_loo(3:4, ids = c("a", "b"))))
  expect_error(compare_ma_loo(c, d), "mismatched numbers of imputations")
})

test_that("combined-only brmsfit_multiple objects are rejected", {
  fit <- structure(list(), class = c("brmsfit_multiple", "brmsfit"))
  expect_error(loo_ma_fits(fit, .loo_fun = function(...) fake_loo(1)), "original per-imputation")
})

test_that("single-fit null models can be compared with imputation-averaged candidates", {
  candidate <- build_ma_loo_result(list(fake_loo(c(2, 4)), fake_loo(c(4, 6))))
  null <- loo_ma_fits(fake_fit(), .loo_fun = function(...) fake_loo(c(1, 2)))
  cmp <- compare_ma_loo(candidate, null)
  expect_equal(null$nimp, 1)
  expect_equal(candidate$nimp, 2)
  expect_equal(cmp$elpd_diff, 7)
})

test_that("format_ma_loo returns readable Pareto-k diagnostics", {
  x <- build_ma_loo_result(list(fake_loo(c(1, 2), k = c(0.6, 0.9)), fake_loo(c(2, 3), k = c(0.4, 0.5))))
  tab <- format_ma_loo(x, model_name = "full")
  expect_named(tab, c("model", "nimp", "mean_per_imputation_elpd", "min_per_imputation_elpd", "max_per_imputation_elpd", "pooled_elpd", "pooled_se", "max_pareto_k", "n_problematic_imputations"))
  expect_equal(tab$max_pareto_k, 0.9)
  expect_equal(tab$n_problematic_imputations, 1)
})

test_that("kfold_ma_fits reuses identical fold assignments across imputations", {
  seen <- list()
  kfold_fun <- function(fit, K, folds, group, joint, ...) {
    seen[[length(seen) + 1L]] <<- list(folds = folds, group = group, joint = joint)
    fake_loo(fit$elpd, ids = as.character(seq_along(fit$elpd)))
  }
  fits <- list(structure(list(elpd = c(1, 2, 3)), class = "brmsfit"), structure(list(elpd = c(3, 4, 5)), class = "brmsfit"))
  dat <- list(data.frame(id = c("a", "a", "b")))
  res <- kfold_ma_fits(fits, completed_data = dat, folds = c(1, 1, 2), .kfold_fun = kfold_fun)
  expect_s3_class(res, "ma_kfold")
  expect_equal(res$folds, c(1, 1, 2))
  expect_true(identical(seen[[1]]$folds, seen[[2]]$folds))
  expect_true(all(vapply(seen, function(x) identical(x$group, "id"), logical(1))))
  expect_true(all(vapply(seen, function(x) identical(x$joint, "group"), logical(1))))
})

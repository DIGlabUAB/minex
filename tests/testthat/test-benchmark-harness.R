benchmark_helpers <- function() {
  path <- testthat::test_path("..", "..", "bench", "benchmark.R")
  skip_if_not(file.exists(path), "Development benchmark is excluded from built packages")
  env <- new.env(parent = globalenv())
  sys.source(path, envir = env)
  env
}

test_that("benchmark retains success, incomplete, and error cases with diagnostics", {
  helper <- benchmark_helpers()
  directory <- tempfile("minex-benchmark-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  paths <- file.path(directory, c("ok.R", "incomplete-warning.R", "failed.R"))
  for (i in seq_along(paths)) writeLines(c("setup", as.character(i)), paths[[i]])
  calls <- list()
  reducer <- function(code, condition, algorithm, backend, granularity) {
    calls[[length(calls) + 1L]] <<- list(code = code, condition = condition,
                                      algorithm = algorithm, backend = backend,
                                      granularity = granularity)
    if (code[[2L]] == "3") stop("broken, case\nwith details")
    incomplete <- code[[2L]] == "2"
    if (incomplete) warning("budget exhausted", call. = FALSE)
    list(complete = !incomplete, oracle_calls = 7L, n_original = 2L,
         n_minimal = 1L, n_chars_original = 20L, n_chars_minimal = 8L,
         evaluation_failures = if (incomplete) list("unobserved") else list())
  }
  ticks <- 0
  clock <- function() { ticks <<- ticks + 0.125; ticks }
  provenance <- list(package_version = "test-version", r_version = "test-R",
                     platform = "test-platform", run_started_at = "test-time",
                     git_revision = "test-head", git_dirty = TRUE)
  results <- helper$benchmark_scripts(paths, reducer = reducer, backend = "inprocess",
                                       clock = clock, provenance = provenance)
  expect_equal(nrow(results), 12L)
  expect_equal(length(calls), 12L)
  expect_equal(table(results$status), c(error = 4L, incomplete = 4L, success = 4L),
               ignore_attr = TRUE)
  expect_true(all(results$elapsed_seconds == 0.125))
  expect_true(all(results$backend == "inprocess"))
  expect_setequal(results$algorithm, c("cdd", "ddmin"))
  expect_setequal(results$granularity, c("statement", "expression"))
  expect_true(all(results$condition[results$script == "incomplete-warning.R"] == "warning"))
  expect_true(all(vapply(calls[5:8], function(call) call$condition == "warning", logical(1))))
  expect_true(all(results$complete[results$status == "success"]))
  expect_false(any(results$complete[results$status == "incomplete"]))
  expect_true(all(is.na(results$complete[results$status == "error"])))
  successful <- results$status != "error"
  expect_true(all(results$oracle_calls[successful] == 7L))
  expect_true(all(results$n_original[successful] == 2L))
  expect_true(all(results$n_minimal[successful] == 1L))
  expect_true(all(results$n_chars_original[successful] == 20L))
  expect_true(all(results$n_chars_minimal[successful] == 8L))
  failed <- results$status == "error"
  expect_true(all(is.na(results$oracle_calls[failed])))
  expect_true(all(is.na(results$n_minimal[failed])))
  expect_true(all(results$error_message[failed] == "broken, case\nwith details"))
  expect_true(all(grepl("simpleError", results$error_classes[failed])))
  expect_true(all(results$warning_messages[results$status == "incomplete"] == "budget exhausted"))
  expect_true(all(results$evaluation_failures[results$status == "incomplete"] == 1L))
  expect_true(all(results$package_version == "test-version"))
  expect_true(all(results$r_version == "test-R"))
  expect_true(all(results$platform == "test-platform"))
  expect_true(all(results$run_started_at == "test-time"))
  expect_true(all(results$git_revision == "test-head"))
  expect_true(all(results$git_dirty))

  output <- file.path(directory, "results", "benchmark.csv")
  expect_identical(helper$write_benchmark_results(results, output), output)
  roundtrip <- utils::read.csv(output, na.strings = "", stringsAsFactors = FALSE)
  expect_equal(roundtrip, results, ignore_attr = TRUE)
})

test_that("benchmark records unreadable scripts for every requested configuration", {
  helper <- benchmark_helpers()
  reducer <- function(...) stop("reducer must not be called")
  results <- helper$benchmark_scripts(tempfile("absent-minex-script-"), reducer = reducer,
                                       algorithms = "ddmin", granularities = c("statement", "expression"),
                                       provenance = helper$benchmark_provenance(testthat::test_path("..", "..")))
  expect_equal(nrow(results), 2L)
  expect_true(all(results$status == "error"))
  expect_true(all(grepl("cannot open", results$error_message)))
  expect_true(all(is.na(results$n_minimal)))
  expect_error(helper$benchmark_scripts(character(), reducer), "Supply benchmark scripts")
})

test_that("benchmark CLI accepts explicit outputs and validates options", {
  helper <- benchmark_helpers()
  defaults <- helper$benchmark_options(character())
  expect_identical(defaults$backend, "callr")
  expect_match(defaults$output, "^bench/results/benchmark-.*\\.csv$")
  expect_identical(helper$benchmark_options(c("--output", "output.csv", "--backend=inprocess")),
                   list(output = "output.csv", backend = "inprocess"))
  expect_error(helper$benchmark_options("--output"), "incomplete")
  expect_error(helper$benchmark_options(c("--output", "--backend=callr")), "Incomplete")
  expect_error(helper$benchmark_options("--output="), "Empty")
  expect_error(helper$benchmark_options("--backend=other"), "callr or inprocess")
  expect_error(helper$benchmark_options("--unknown"), "Unknown")
})

test_that("benchmark provenance identifies the package source and R runtime", {
  helper <- benchmark_helpers()
  root <- testthat::test_path("..", "..")
  provenance <- helper$benchmark_provenance(root)
  expect_identical(provenance$package_version,
                   unname(read.dcf(file.path(root, "DESCRIPTION"))[1L, "Version"]))
  expect_identical(provenance$r_version, R.version.string)
  expect_identical(provenance$platform, R.version$platform)
  expect_match(provenance$run_started_at, "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9:]{8}Z$")
})

test_that("benchmark Git provenance records clean and dirty source checkouts", {
  helper <- benchmark_helpers()
  root <- testthat::test_path("..", "..")
  calls <- list()
  clean_git <- function(args) {
    calls[[length(calls) + 1L]] <<- args
    if ("rev-parse" %in% args) "fixture-head" else character()
  }
  clean <- helper$benchmark_git_provenance(root, git = clean_git)
  expect_identical(clean, list(git_revision = "fixture-head", git_dirty = FALSE))
  expect_identical(calls[[1L]], c("-C", shQuote(normalizePath(root)), "rev-parse", "HEAD"))
  expect_identical(calls[[2L]], c("-C", shQuote(normalizePath(root)),
                                 "status", "--porcelain", "--untracked-files=normal"))
  dirty_git <- function(args) {
    if ("rev-parse" %in% args) "fixture-head" else c(" M R/match.R", "?? fixture.R")
  }
  expect_identical(helper$benchmark_git_provenance(root, git = dirty_git),
                   list(git_revision = "fixture-head", git_dirty = TRUE))
  provenance <- helper$benchmark_provenance(root, git = dirty_git)
  expect_identical(provenance$git_revision, "fixture-head")
  expect_true(provenance$git_dirty)
})

test_that("benchmark Git provenance tolerates missing Git or checkout metadata", {
  helper <- benchmark_helpers()
  root <- testthat::test_path("..", "..")
  unavailable <- list(git_revision = NA_character_, git_dirty = NA)
  missing_git <- function(args) stop("git unavailable")
  expect_identical(helper$benchmark_git_provenance(root, git = missing_git), unavailable)
  no_checkout <- function(args) structure("fatal: not a git repository", status = 128L)
  expect_identical(helper$benchmark_git_provenance(root, git = no_checkout), unavailable)
  no_head <- function(args) character()
  expect_identical(helper$benchmark_git_provenance(root, git = no_head), unavailable)
  no_status <- function(args) {
    if ("rev-parse" %in% args) "fixture-head" else structure("status failed", status = 1L)
  }
  expect_identical(helper$benchmark_git_provenance(root, git = no_status),
                   list(git_revision = "fixture-head", git_dirty = NA))
})

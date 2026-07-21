# bench/run-benchmark.R
# Run from the package root: Rscript bench/run-benchmark.R
suppressMessages(devtools::load_all("."))
scripts <- list.files("bench/scripts", full.names = TRUE)
rows <- list()
for (path in scripts) {
  code <- readLines(path, warn = FALSE)
  cond <- if (grepl("warning", path)) "warning" else "error"
  for (algo in c("cdd", "ddmin")) {
    res <- tryCatch(
      minex(code = code, condition = cond, algorithm = algo,
            backend = "callr"),
      error = function(e) NULL
    )
    if (!is.null(res)) {
      rows[[length(rows) + 1L]] <- data.frame(
        script = basename(path), algorithm = algo,
        oracle_calls = res$oracle_calls, n_minimal = res$n_minimal
      )
    }
  }
}
print(do.call(rbind, rows))

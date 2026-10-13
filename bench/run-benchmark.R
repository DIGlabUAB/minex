# Run from the package root: Rscript bench/run-benchmark.R [--output PATH]
source("bench/benchmark.R")
args <- commandArgs(trailingOnly = TRUE)
if (any(args %in% c("--help", "-h"))) {
  cat("Usage: Rscript bench/run-benchmark.R [--output PATH] [--backend callr|inprocess]\n",
      "Defaults: timestamped CSV in bench/results; isolated callr backend.\n",
      "Exits nonzero after saving results if any case fails or is incomplete.\n",
      sep = "")
  quit(status = 0L)
}
options <- benchmark_options(args)
if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("Install pkgload to benchmark the package source.", call. = FALSE)
}
suppressMessages(pkgload::load_all(".", quiet = TRUE))
scripts <- list.files("bench/scripts", pattern = "\\.R$", full.names = TRUE)
results <- benchmark_scripts(scripts, reducer = minex, backend = options$backend)
write_benchmark_results(results, options$output)
print(results)
message("Results written to ", normalizePath(options$output, mustWork = TRUE))
if (any(results$status != "success")) quit(status = 1L)

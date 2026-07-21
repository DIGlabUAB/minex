# CDD round-growth rate, Zhang et al. (ICSE 2025), Lemma III.2.
# Written as an expression, not the rounded 1.582, to stay exact.
PROBDD_GROWTH_RATE <- 1 / (1 - exp(-1))

#' @keywords internal
#' @noRd
cdd_reduce <- function(kept, test) {
  round_index <- 0L
  repeat {
    len <- length(kept)
    if (len < 2L) {
      break
    }
    p_initial  <- 1 / len
    p_round    <- min(p_initial * PROBDD_GROWTH_RATE^round_index, 1)
    sizes      <- seq_len(len)
    block_size <- sizes[which.max(sizes * (1 - p_round)^sizes)]

    if (block_size >= len) {
      # Complement would be empty; nothing useful to test this round.
      round_index <- round_index + 1L
      next
    }

    starts <- seq(1L, len, by = block_size)
    hit <- FALSE
    for (s in starts) {
      block <- kept[s:min(s + block_size - 1L, len)]
      complement <- setdiff(kept, block)
      if (length(complement) > 0L && isTRUE(test(complement))) {
        kept <- complement
        round_index <- 0L
        hit <- TRUE
        break
      }
    }
    if (!hit) {
      if (block_size <= 1L) {
        break               # tested every singleton complement, no hit: done
      }
      round_index <- round_index + 1L
    }
  }
  kept
}

# Helper to run function up a function a user-specified number of times with increasing
# backoff times (modified from `VERB_n` in googlesheets package); errors (after n
# attempts) are kept and passed along.  Currently retrying on all errors until timeouts,
# brief server errors, and non-negotiable errors can be better differentiated.
try_verb_n <- function(verb, n = 3) {
  function(...) {
    for (i in seq_len(n)) {
      if (i == n)
        out <- verb(...)
      else
        out <- try(verb(...), silent = TRUE)
      if (!is_error(out) || i == n) break
      wait <- stats::runif(1, min(5 ^ i, 120), min(5 ^ (i + 1), 240))
      mess <- "Timeout or error on attempt %d. Retrying in %0.0f s."
      message(sprintf(mess, i, wait))
      Sys.sleep(wait)
    }
    out
  }
}

is_error <- function(obj) {
  if (inherits(obj, "list") &&
      identical(names(obj), c("result", "error")))
    !is.null(obj$error)
  else inherits(obj, "error") | inherits(obj, "try-error")
}

# Wraps any expensive computation with file-based checkpointing.
# If the .rds file already exists, loads it. Otherwise runs expr and saves it.
with_checkpoint <- function(path, expr, overwrite = FALSE, verbose = TRUE) {
  if (file.exists(path) && !overwrite) {
    if (verbose) message("  [SKIP] Loading cached: ", path)
    return(readRDS(path))
  }
  if (verbose) message("  [RUN]  Computing: ", path)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  
  # Write a ".lock" sentinel so another machine doesn't start the same job
  lock <- paste0(path, ".lock")
  if (file.exists(lock)) {
    message("  [LOCK] Another process is computing this. Skipping: ", path)
    return(NULL)
  }
  writeLines(as.character(Sys.time()), lock)
  on.exit(unlink(lock))  # always remove lock
  
  result <- tryCatch(
    force(expr),
    error = function(e) {
      message("  [FAIL] ", conditionMessage(e))
      NULL
    }
  )
  if (!is.null(result)) saveRDS(result, path)
  result
}
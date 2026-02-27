check_project_environment <- function() {
  
  cat("=== Checking renv ===\n")
  
  if (!requireNamespace("renv", quietly = TRUE)) {
    stop("Package 'renv' is not installed.")
  }
  
  project_path <- renv::project()
  
  if (is.null(project_path)) {
    stop("This project is not activated with renv.")
  } else {
    cat("renv project detected at:\n", project_path, "\n")
  }
  
  status <- renv::status()
  
  if (!isTRUE(status$synchronized)) {
    warning("renv is not synchronized. Run renv::restore() or renv::snapshot().")
  } else {
    cat("renv OK (project synchronized)\n")
  }
  
  cat("\n=== Checking Python (reticulate) ===\n")
  
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' is not installed.")
  }
  
  library(reticulate)
  
  cfg <- py_config()
  print(cfg)
  
  pip_ok <- tryCatch({
    py_run_string("import pip")
    TRUE
  }, error = function(e) FALSE)
  
  if (!pip_ok) {
    stop("pip is not available in the currently selected Python environment.")
  } else {
    cat("pip OK\n")
  }
  
  cat("\n✔ Environment looks good.\n")
  
  invisible(TRUE)
}
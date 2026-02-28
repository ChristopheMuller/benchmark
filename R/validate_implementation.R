validate_my_methods <- function(path = "./my_methods") {
  
  if (!dir.exists(path)) {
    stop(sprintf("Directory %s does not exist.", path))
  }
  
  files <- list.files(path, pattern = "\\.[Rr]$", full.names = TRUE)
  
  if (length(files) == 0) {
    message("No method files found in ./my_methods/.")
    return(invisible(TRUE))
  }
  
  for (file in files) {
    
    method_name <- tools::file_path_sans_ext(basename(file))
    expected_fun <- paste0("impute_", method_name)
    
    # load file into temporary environment
    env <- new.env()
    source(file, local = env)
    
    if (!exists(expected_fun, envir = env, mode = "function")) {
      stop(
        sprintf(
          "File '%s' does not contain required function '%s()'.",
          basename(file), expected_fun
        )
      )
    } else {
      message(sprintf("✓ %s is correctly defined.", method_name))
    }
  }
  
  message("All custom imputation methods are valid.")
  invisible(TRUE)
}

# ------------------------------------------------------------------------------
# Collect available custom imputation methods
# ------------------------------------------------------------------------------

collect_my_methods <- function(path = "./my_methods") {
  
  if (!dir.exists(path)) {
    stop(sprintf("Directory %s does not exist.", path))
  }
  
  files <- list.files(path, pattern = "\\.[Rr]$", full.names = TRUE)
  
  if (length(files) == 0) {
    message("No method files found.")
    return(data.frame())
  }
  
  results <- list()
  
  for (file in files) {
    
    method_name <- tools::file_path_sans_ext(basename(file))
    
    # load into temporary environment
    env <- new.env()
    source(file, local = env)
    
    # find functions starting with "impute_"
    fun_names <- ls(env)
    impute_funs <- fun_names[
      sapply(fun_names, function(x) {
        is.function(get(x, envir = env)) && grepl("^impute_", x)
      })
    ]
    
    if (length(impute_funs) > 0) {
      for (fn in impute_funs) {
        results[[length(results) + 1]] <- data.frame(
          method = method_name,
          imputation_fun = fn,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  if (length(results) == 0) {
    message("No imputation functions found.")
    return(data.frame())
  }
  
  do.call(rbind, results)
}

collect_python_methods <- function(file = "./python/python_imputation_functions.R") {
  
  if (!file.exists(file)) {
    stop(sprintf("File '%s' does not exist.", file))
  }
  
  # load into temporary environment
  env <- new.env()
  source(file, local = env)
  
  # find functions starting with "impute_"
  fun_names <- ls(env)
  impute_funs <- fun_names[
    sapply(fun_names, function(x) {
      is.function(get(x, envir = env)) && grepl("^impute_", x)
    })
  ]
  
  if (length(impute_funs) == 0) {
    message("No imputation functions found in python_imputation_functions.R.")
    return(data.frame())
  }
  
  data.frame(
    method = sub("^impute_", "", impute_funs),
    imputation_fun = impute_funs,
    stringsAsFactors = FALSE
  )
}


source_my_methods <- function(path = "./my_methods", envir = .GlobalEnv) {
  
  if (!dir.exists(path)) {
    stop(sprintf("Directory %s does not exist.", path))
  }
  
  files <- list.files(path, pattern = "\\.[Rr]$", full.names = TRUE)
  
  if (length(files) == 0) {
    message("No .R files found in ./my_methods/.")
    return(invisible(NULL))
  }
  
  for (file in files) {
    message(sprintf("Sourcing: %s", basename(file)))
    source(file, local = envir)
  }
  
  message("All R methods files sourced successfully.")
  invisible(NULL)
}


source_python_methods <- function(file = "./python/python_imputation_functions.R",
                                  envir = .GlobalEnv) {
  
  if (!file.exists(file)) {
    stop(sprintf("File '%s' does not exist.", file))
  }
  
  message(sprintf("Sourcing: %s", basename(file)))
  source(file, local = envir)
  
  message("Python imputation functions sourced successfully.")
  invisible(NULL)
}











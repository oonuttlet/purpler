purpleair_api_key <- function(api_key = NULL, overwrite = FALSE, install = FALSE) {
  if (!install & overwrite){
    cli::cli_abort("`overwrite` cannot be TRUE if `install` is FALSE.\n")
  }
  if (is.null(api_key)) {
    print(Sys.getenv("PURPLEAIR_API_KEY"))
  }
  if (!install) {
    cat(cli::combine_ansi_styles("magenta", "italic")("To install your API key for future sessions, run this function with `install = TRUE`.\n"))
    Sys.setenv(PURPLEAIR_API_KEY = api_key)
  }
  # Check if the API key is valid
  stopifnot("Invalid API key. If you don't have an API key, get one from https://develop.purpleair.com/sign-in?redirectURL=%2Fdashboards%2Fkeys" = check_pattern_api_key(api_key))

  if (install) {
    home <- Sys.getenv("HOME")
    renv <- file.path(home, ".Renviron")

    # Backup existing .Renviron
    if (file.exists(renv)) {
      file.copy(renv, file.path(home, ".Renviron_backup_purpler"), overwrite = TRUE)
    } else {
      file.create(renv)
    }

    # Set the API key in the .Renviron file
    check_renv_api_key(api_key, renv, overwrite)

    message("To use now, restart R or run `readRenviron(\"~/.Renviron\")`.")
  }
}

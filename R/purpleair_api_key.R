purpleair_api_key <- function(api_key = NULL, overwrite = FALSE, install = FALSE){
    if (!is.null(api_key)){
      stopifnot("Invalid API key. If you don't have an API key, you can get one from https://develop.purpleair.com/sign-in?redirectURL=%2Fdashboards%2Fkeys" = check_purple_api_key(api_key))
      if (install) {
        home <- Sys.getenv("HOME")
        renv <- file.path(home, ".Renviron")
        if (file.exists(renv)) {
          file.copy(renv, file.path(home, ".Renviron_backup_purpler"), overwrite = T)
        }
        if (!file.exists(renv)) {
          file.create(renv)
        }
        else {
          oldenv = read.table(renv, stringsAsFactors = FALSE)
          oldenv$V1 <-  oldenv[,1] |>
            strsplit("=", fixed = T)
          oldenv$V2 <- sapply(oldenv$V1, FUN = function(x) x[2])
          oldenv$V1 <- sapply(oldenv$V1, FUN = function(x) x[1])
          newenv <- data.frame(V1 = character(), V2 = character())
          if (isTRUE(overwrite) & length(grep("PURPLEAIR_API_KEY", oldenv)) > 0) {
            message("Your original .Renviron will be backed up and stored in your R HOME directory if needed.")
            newenv <- oldenv[-grep("PURPLEAIR_API_KEY", oldenv$V1),]
          }
          if (isTRUE(overwrite) & length(grep("PURPLEAIR_API_KEY", oldenv)) == 0){
            newenv <- oldenv
          }
          else {
            tv <- readLines(renv)
            if (any(grepl("PURPLEAIR_API_KEY", tv))) {
              stop("A PURPLEAIR_API_KEY already exists. You can overwrite it with the argument overwrite=TRUE.",
                   call. = FALSE)
            }
          }
        }
      tr <- data.frame(V1 = "PURPLEAIR_API_KEY", V2 = paste0("'", api_key, "'"))
      newenv <- tr |>
        rbind(newenv)
      write.table(newenv, renv, sep = "=", quote = FALSE, col.names = FALSE, row.names = FALSE)
      message("Your API key has been stored in your .Renviron and can be accessed by Sys.getenv(\"PURPLEAIR_API_KEY\"). \nTo use now, restart R or run `readRenviron(\"~/.Renviron\")`.")
      return(api_key)
    }
    else {
      message("To install your API key for use in future sessions, run this function with `install = TRUE`.")
      Sys.setenv(PURPLEAIR_API_KEY = api_key)
    }
  }
  else {
    print(Sys.getenv("PURPLEAIR_API_KEY"))
  }
}

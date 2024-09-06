api_key_pattern <- "^[0-9A-F]{8}-[0-9A-F]{4}-[0-9A-F]{4}-[0-9A-F]{4}-[0-9A-F]{12}$"

api_key_checker <- function(x){
  if (class(x) == "character"
      & length(x) == 1
      & grepl(x = x,
              pattern = api_key_pattern)){
    TRUE
  }
  else if (!grepl(x = x,
                  pattern = api_key_pattern)){
    warning("The provided 'api_key' does not match the format provided by PurpleAir.")
    TRUE
  }
  else {
    stop("'api_key' must be a character vector.")
    FALSE
  }
}

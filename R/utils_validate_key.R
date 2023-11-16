validate_key <- function(
    key,
    argument_name = rlang::caller_arg(key),
    call = rlang::caller_call()
) {
  if (length(key) != 1) {
    cli::cli_abort(
      c(
        "{.arg {argument_name}} must be a {.cls character} string of length {.val {1}}.",
        x = "{.arg {argument_name}} has a length of {.val {length(key)}}."
      ),
      call = call
    )
  }
  
  if (!inherits(key, "character")) {
    cli::cli_abort(
      c(
        "{.arg {argument_name}} must be a {.cls character} string of length {.val {1}}.",
        x = "{.arg {argument_name}} is an object of class {.cls {class(key)}}."
      ),
      call = call
    )
  }
  
  if (!nzchar(key)) {
    cli::cli_abort(
      c(
        "{.arg {argument_name}} must not be an empty string.",
        "*" = "Have you set the {.envvar CENSUS_API_KEY} environment variable?
        See {.help wru::get_census_data} for more information."
      ),
      call = call
    )
  }
  
  if (is.na(key)) {
    cli::cli_abort(
      "{.arg {argument_name}} must not be {.val {NA_character_}}.",
      call = call
    )
  }
  
  key
}

assert_boolean <- function(
    x,
    argument_name = rlang::caller_arg(x),
    call = rlang::caller_call()
) {
  if (length(x) != 1) {
    cli::cli_abort(
      c(
        "{.arg {argument_name}} must be a {.code TRUE} or {.code FALSE} value of length {.val {1}}.",
        x = "{.arg {argument_name}} has a length of {.val {length(x)}}."
      ),
      call = call
    )
  }
  
  if (!inherits(x, "logical")) {
    cli::cli_abort(
      c(
        "{.arg {argument_name}} must be a {.class logical} {.code TRUE} or {.code FALSE} value.",
        x = "{.arg {argument_name}} is an object of class {.cls {class(x)}}."
      ),
      call = call
    )
  }
  
  if (!x %in% c(TRUE, FALSE)) {
    cli::cli_abort(
      c(
        "{.arg {argument_name}} must be {.code TRUE} or {.code FALSE}.",
        x = "{.arg {argument_name}} is {.val {x}}."
      ),
      call = call
    )
  }
  
  x
}

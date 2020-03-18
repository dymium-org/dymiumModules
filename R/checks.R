.check_file_name = function(x) {
  if (!checkmate::test_character(x, pattern = "^[a-zA-Z0-9._-]+$", len = 1, any.missing = FALSE, null.ok = FALSE)) {
    stop(glue::glue("'{x}' is not a valid file name. It should contain only \\
                    ASCII letters, numbers, '-', and '_'."))
  }
  return(TRUE)
}
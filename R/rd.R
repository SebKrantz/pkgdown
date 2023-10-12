rd_text <- function(x, fragment = TRUE) {
  con <- textConnection(x)
  on.exit(close(con), add = TRUE)

  set_classes(parse_Rd2(con, fragment = fragment, encoding = "UTF-8"))
}

parse_Rd2 <- function(file, ...) {
  substcr <- function(x) gsub("\\cr\\cr", "\\cr", gsub("\\cr \\cr", "\\cr", x, fixed = TRUE), fixed = TRUE)
  lines <- read_lines(file)
  if(endsWith(file, "collapse-options.Rd")) {
    lines <- gsub("\\tab\\tab", "\\tab", gsub("\\tab \\tab", "\\tab", lines, fixed = TRUE), fixed = TRUE)
    lines <- gsub("\\tabular{lll}", "\\tabular{ll}", lines, fixed = TRUE)
  }
  tmp <- tempfile(fileext = ".Rd")
  write_lines(substcr(substcr(substcr(substcr(substcr(lines))))), tmp)
  res <- tools::parse_Rd(tmp, ...)
  file.remove(tmp)
  res
}

rd_file <- function(path, pkg_path = NULL) {
  if (getRversion() >= "3.4.0") {
    macros <- tools::loadPkgRdMacros(pkg_path)
    set_classes(parse_Rd2(path, macros = macros, encoding = "UTF-8"))
  } else if (getRversion() >= "3.2.0") {
    macros <- tools::loadPkgRdMacros(pkg_path, TRUE)
    set_classes(parse_Rd2(path, macros = macros, encoding = "UTF-8"))
  } else {
    set_classes(parse_Rd2(path, encoding = "UTF-8"))
  }
}

#' Translate an Rd string to its HTML output
#'
#' @param x Rd string. Backslashes must be double-escaped ("\\\\").
#' @param fragment logical indicating whether this represents a complete Rd file
#' @param ... additional arguments for as_html
#'
#' @examples
#' rd2html("a\n%b\nc")
#'
#' rd2html("a & b")
#'
#' rd2html("\\strong{\\emph{x}}")
#'
#' @export
rd2html <- function(x, fragment = TRUE, ...) {
  html <- as_html(rd_text(x, fragment = fragment), ...)
  str_trim(strsplit(str_trim(html), "\n")[[1]])
}

print.Rd <- function(x, ...) {
  utils::str(x)
}
#' @export
print.tag <- function(x, ...) {
  utils::str(x)
}

# Convert RD attributes to S3 classes -------------------------------------

set_classes <- function(rd) {
  if (is.list(rd)) {
    rd[] <- lapply(rd, set_classes)
  }
  set_class(rd)
}

set_class <- function(x) {
  structure(x,
    class = c(attr(x, "class"), tag(x), "tag"),
    Rd_tag = NULL,
    srcref = NULL,
    macros = NULL
  )
}

tag <- function(x) {
  tag <- attr(x, "Rd_tag")
  if (is.null(tag)) return()

  gsub("\\", "tag_", tag, fixed = TRUE)
}

#' @export
`[.tag` <- function(x, ...) {
  structure(NextMethod(), class = class(x))
}

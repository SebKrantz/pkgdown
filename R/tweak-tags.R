tweak_anchors <- function(html, only_contents = TRUE) {
  if (only_contents) {
    sections <- xml2::xml_find_all(html, ".//div[@class='contents']//div[@id]")
  } else {
    sections <- xml2::xml_find_all(html, "//div[@id]")
  }

  if (length(sections) == 0)
    return(invisible())

  # Update anchors: dot in the anchor breaks scrollspy
  anchor <- sections %>%
    xml2::xml_attr("id") %>%
    gsub(".", "-", ., fixed = TRUE)
  purrr::walk2(sections, anchor, ~ (xml2::xml_attr(.x, "id") <- .y))

  # Update href of toc anchors , use "-" instead "."
  toc_nav <- xml2::xml_find_all(html, ".//div[@id='tocnav']//a")
  hrefs <- toc_nav %>%
    xml2::xml_attr("href") %>%
    gsub(".", "-", ., fixed = TRUE)
  purrr::walk2(toc_nav, hrefs, ~ (xml2::xml_attr(.x, "href") <- .y))

  headings <- xml2::xml_find_first(sections, ".//h1|h2|h3|h4|h5")
  has_heading <- !is.na(xml2::xml_name(headings))

  for (i in seq_along(headings)[has_heading]) {
    # Insert anchor in first element of header
    heading <- headings[[i]]
    if (length(xml2::xml_contents(heading)) == 0) {
      # skip empty headings
      next
    }

    xml2::xml_attr(heading, "class") <- "hasAnchor"
    xml2::xml_add_sibling(
      xml2::xml_contents(heading)[[1]],
      "a", href = paste0("#", anchor[[i]]),
      class = "anchor",
      `aria-hidden` = "true",
      .where = "before"
    )
  }
  invisible()
}

tweak_link_md <- function(html) {
  links <- xml2::xml_find_all(html, ".//a")
  if (length(links) == 0)
    return()

  hrefs <- xml2::xml_attr(links, "href")
  needs_tweak <- grepl("\\.md$", hrefs) & xml2::url_parse(hrefs)$scheme == ""

  fix_links <- function(x) {
    x <- gsub("\\.md$", ".html", x)
    x <- gsub("\\.github/", "", x)
    x
  }

  if (any(needs_tweak)) {
    purrr::walk2(
      links[needs_tweak],
      fix_links(hrefs[needs_tweak]),
      xml2::xml_set_attr,
      attr = "href"
    )
  }

  invisible()
}

tweak_link_external <- function(html, pkg = list()) {
  links <- xml2::xml_find_all(html, ".//a")
  if (length(links) == 0)
    return()

  hrefs <- xml2::xml_attr(links, "href")
  # Users might have added absolute URLs to e.g. the Code of Conduct
  tweak_class_prepend(links[!is_internal_link(hrefs, pkg = pkg)], "external-link")

  invisible()
}

# Fix relative image links
tweak_img_src <- function(html) {
  imgs <- xml2::xml_find_all(html, ".//img")
  urls <- xml2::xml_attr(imgs, "src")
  new_urls <- gsub("(^|/)vignettes/", "\\1articles/", urls, perl = TRUE)
  new_urls <- gsub("(^|/)man/figures/", "\\1reference/figures/", new_urls, perl = TRUE)
  purrr::map2(imgs, new_urls, ~ xml2::xml_set_attr(.x, "src", .y))

  invisible()
}

tweak_link_absolute <- function(html, pkg = list()) {
  # If there's no URL links can't be made absolute
  if (is.null(pkg$meta$url)) {
    return()
  }

  url <- paste0(pkg$meta$url, "/")

  # <a> + <link> use href
  href <- xml2::xml_find_all(html, ".//a | .//link")
  xml2::xml_attr(href, "href") <- xml2::url_absolute(xml2::xml_attr(href, "href"), url)

  # <img> + <script> uses src
  src <- xml2::xml_find_all(html, ".//script | .//img")
  xml2::xml_attr(src, "src") <- xml2::url_absolute(xml2::xml_attr(src, "src"), url)

  invisible()
}

tweak_tables <- function(html) {
  # Ensure all tables have class="table"
  table <- xml2::xml_find_all(html, ".//table")
  tweak_class_prepend(table, "table")

  invisible()
}

# from https://github.com/rstudio/bookdown/blob/ed31991df3bb826b453f9f50fb43c66508822a2d/R/bs4_book.R#L307
tweak_footnotes <- function(html) {
  container <- xml2::xml_find_all(html, ".//div[@class='footnotes']")
  if (length(container) != 1) {
    return()
  }
  # Find id and contents
  footnotes <- xml2::xml_find_all(container, ".//li")
  id <- xml2::xml_attr(footnotes, "id")
  xml2::xml_remove(xml2::xml_find_all(footnotes, "//a[@class='footnote-back']"))
  contents <- vapply(footnotes, FUN.VALUE = character(1), function(x) {
    as.character(xml2::xml_children(x), options = character())
  })
  # Add popover attributes to links
  for (i in seq_along(id)) {
    links <- xml2::xml_find_all(html, paste0(".//a[@href='#", id[[i]], "']"))
    xml2::xml_attr(links, "href") <- NULL
    xml2::xml_attr(links, "id") <- NULL
    xml2::xml_attr(links, "tabindex") <- "0"
    xml2::xml_attr(links, "data-toggle") <- "popover"
    xml2::xml_attr(links, "data-content") <- contents[[i]]
  }
  # Delete container
  xml2::xml_remove(container)
}
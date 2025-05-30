test_that("template_candidates look for specific first", {
  expect_equal(
    path_file(template_candidates("content", "article")),
    c("content-article.html", "content.html")
  )
})

test_that("template_candidates look in template dir then pkgdown", {
  pkg_dir <- withr::local_tempdir()
  template_dir <- withr::local_tempdir()

  pkg <- list(
    src_path = pkg_dir,
    meta = list(template = list(path = template_dir)),
    bs_version = 3
  )

  # ensure that templates_dir(pkg) returns the specific template path
  expect_equal(templates_dir(pkg), path(template_dir))

  paths <- template_candidates("content", "article", pkg = pkg)
  dirs <- unique(path_dir(paths))
  expect_equal(
    dirs,
    c(
      # search for candidates...
      path(pkg_dir, "pkgdown", "templates"), # first in local templates
      path(template_dir), # second in global template path
      path_pkgdown("BS3", "templates") # finally in pkgdown templates
    )
  )
})


test_that("look for templates_dir in right places", {
  dir <- withr::local_tempdir()
  pkg <- list(src_path = dir, meta = list(template = list()))

  # Look in site templates
  expect_equal(templates_dir(pkg), path(dir, "pkgdown", "templates"))

  # Look in specified directory
  pkg$meta$template$path <- path(withr::local_tempdir())
  expect_equal(templates_dir(pkg), pkg$meta$template$path)
})

test_that("find templates in local pkgdown first", {
  pkg <- test_path("assets", "templates-local")

  # local template used over default pkgdown template
  expect_equal(
    find_template("content", "article", pkg = pkg),
    path_abs(path(pkg, "pkgdown", "templates", "content-article.html"))
  )

  expect_equal(
    find_template("footer", "article", pkg = pkg),
    path_abs(path(pkg, "pkgdown", "templates", "footer-article.html"))
  )

  # pkgdown template used (no local template)
  expect_equal(
    find_template("content", "tutorial", pkg = pkg),
    path_pkgdown("BS3", "templates", "content-tutorial.html")
  )

  expect_equal(
    find_template("footer", "ignored", pkg = pkg),
    path_pkgdown("BS3", "templates", "footer.html")
  )
})


# Expected contents -------------------------------------------------------

test_that("BS5 templates have main + aside", {
  names <- dir_ls(path_pkgdown("BS5", "templates"), regexp = "content-")
  names <- path_ext_remove(path_file(names))
  names <- gsub("content-", "", names)

  templates <- lapply(
    names,
    read_template_html,
    type = "content",
    pkg = list(bs_version = 5)
  )
  for (i in seq_along(templates)) {
    template <- templates[[i]]
    name <- names[[i]]

    expect_equal(xpath_length(template, ".//div/main"), 1, info = name)
    expect_equal(xpath_attr(template, ".//div/main", "id"), "main", info = name)
    expect_equal(xpath_length(template, ".//div/aside"), 1, info = name)
    expect_equal(
      xpath_attr(template, ".//div/aside", "class"),
      "col-md-3",
      info = name
    )
  }
})

# Bootstrap version resolution --------------------------------------------
test_that("Bootstrap version in template package under `template.bootstrap`", {
  path_template_package <- local_pkgdown_template_pkg(
    meta = list(template = list(bootstrap = 5))
  )

  pkg <- local_pkgdown_site(
    meta = list(template = list(package = "templatepackage"))
  )

  expect_equal(pkg$bs_version, 5)
})

test_that("Bootstrap version in template package under `template.bslib.version`", {
  path_template_package <- local_pkgdown_template_pkg(
    meta = list(template = list(bslib = list(version = 5)))
  )

  pkg <- local_pkgdown_site(
    meta = list(template = list(package = "templatepackage"))
  )

  expect_equal(pkg$bs_version, 5)
})

test_that("Invalid bootstrap version spec in template package", {
  path_template_package <- local_pkgdown_template_pkg(
    meta = list(template = list(bootstrap = 4, bslib = list(version = 5)))
  )

  expect_snapshot(
    error = TRUE,
    local_pkgdown_site(
      meta = list(template = list(package = "templatepackage"))
    )
  )
})

test_that("Invalid bootstrap version spec in _pkgdown.yml", {
  expect_snapshot(
    error = TRUE,
    local_pkgdown_site(
      meta = list(
        template = list(
          bootstrap = 4,
          bslib = list(version = 5)
        )
      )
    )
  )
})

test_that("Valid local Bootstrap version masks invalid template package", {
  path_template_package <- local_pkgdown_template_pkg(
    meta = list(template = list(bootstrap = 4, bslib = list(version = 5)))
  )

  expect_no_error(
    local_pkgdown_site(
      meta = list(
        template = list(
          package = "templatepackage",
          bootstrap = 5
        )
      )
    )
  )
})

# Bootstrap theme resolution ----------------------------------------------
test_that("Finds Bootstrap theme in all the places", {
  pkg_sketchy <- local_pkgdown_site(
    meta = list(template = list(bslib = list(preset = "sketchy", version = 5)))
  )
  pkg_cosmo <- local_pkgdown_site(
    meta = list(template = list(bootstrap = 5, bootswatch = "cosmo"))
  )
  pkg_yeti <- local_pkgdown_site(
    meta = list(
      template = list(bootstrap = 5, params = list(bootswatch = "yeti"))
    )
  )

  expect_equal(get_bslib_theme(pkg_sketchy), "sketchy")
  expect_equal(get_bslib_theme(pkg_cosmo), "cosmo")
  expect_equal(get_bslib_theme(pkg_yeti), "yeti")
})

test_that("Warns when Bootstrap theme is specified in multiple locations", {
  pkg <- local_pkgdown_site(
    meta = list(
      template = list(
        bootstrap = 5,
        bootswatch = "cerulean",
        bslib = list(preset = "flatly", bootswatch = "lux"),
        params = list(bootswatch = "darkly")
      )
    )
  )
  expect_snapshot(get_bslib_theme(pkg))
})

test_that("Doesn't warn when the same Bootstrap theme is specified in multiple locations", {
  pkg <- local_pkgdown_site(
    meta = list(
      template = list(
        bootstrap = 5,
        bootswatch = "cerulean",
        bslib = list(preset = "cerulean")
      )
    )
  )

  expect_equal(expect_silent(get_bslib_theme(pkg)), "cerulean")
})

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean.R
\name{clean_site}
\alias{clean_site}
\alias{clean_cache}
\title{Clean site}
\usage{
clean_site(pkg = ".", quiet = FALSE, force = FALSE)

clean_cache(pkg = ".", quiet = FALSE)
}
\arguments{
\item{pkg}{Path to package.}

\item{quiet}{If \code{TRUE}, suppresses a message.}

\item{force}{If \code{TRUE}, delete contents of \code{docs} even if it is not a pkgdown site.}
}
\description{
Delete all files in \verb{docs/} (except for \code{CNAME}).

Delete all files in the pkgdown cache directory.
}

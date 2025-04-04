my_install_arrow <- function(source,
                          local = "~/git/arrow/r",
                          github = "arrow/apache/r",
                          lib = "~/.R/dev-library"
                          ) {
  source <- match.arg(
    source,
    c(
      "local",  ## == ~/git/arrow/r
      "github", ## == https://github.com/apache/arrow/r
      "repo"    ## from default repository, should be RSPM
    )
    )
  
  Sys.setenv("NOT_CRAN" = TRUE)
  Sys.setenv("LIBARROW_BINARY" = FALSE)
  Sys.setenv("LIBARROW_MINIMAL" = FALSE)
  Sys.setenv("ARROW_R_DEV" = TRUE)
  
  if (!dir.exists(lib)) {
    dir.create(lib)
  }
  
  switch(source,
    "local" = withr::with_libpaths(
      lib,
      devtools::install(pkg = local)
    ),
    "github" = pak::pkg_install(
      pkg = github,
      lib = lib        
    ),
    "repo" = pak::pkg_install("arrow", lib = lib)
  )
}

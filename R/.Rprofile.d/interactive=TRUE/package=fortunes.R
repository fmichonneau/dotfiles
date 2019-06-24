try(local({

  show_fortune <- function()  {
    frt <- fortunes::fortune()
    if (require(crayon, quietly = TRUE)) {
      frt[] <- lapply(frt[], crayon::cyan)
      #frt <- frt[nzchar(frt)]
    }
    print(frt)
  }
  show_fortune()
  unloadNamespace("fortunes")

}))

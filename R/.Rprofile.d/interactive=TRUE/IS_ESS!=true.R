local({
  if (require(prompt, quietly = TRUE)) {

    set_prompt(function(...) {
      paste0(
        "[", git_branch(), git_dirty(), git_arrows(), "] ",
        prompt_memuse()
      )
    })

  } else {
    warning("install 'prompt' to have a custom prompt.")
  }

})

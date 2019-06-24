library(prompt)

set_prompt(function(...){
  paste0(
    "[", git_branch(), git_dirty(), git_arrows(), "] ",
    prompt_memuse()
  )
})

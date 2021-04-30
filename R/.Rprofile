try({ if (interactive()) startup::startup() })

if (interactive() && Sys.getenv("IS_ESS") == "") {
  if (require(prompt, quietly = TRUE)) {
    prompt::set_prompt(prompt::new_prompt_powerline())
  } else {
    warning("install 'prompt' to have a custom prompt.")
  }
}

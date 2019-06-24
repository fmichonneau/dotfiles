
local({

  if (require(prompt, quietly = TRUE)) {

    set_prompt(prompt_fancy)

  } else {

    warning("install 'prompt' to have a custom prompt.")

  }

})

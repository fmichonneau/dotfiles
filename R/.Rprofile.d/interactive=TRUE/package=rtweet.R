local({
  load_rtweet <- function() {
    rtweet_key <- "~/.R/rtweet-key.rds"
    if (!file.exists(rtweet_key)) {
      message("Getting rtweet key")
      twitter_token <- rtweet::create_token(
        app = "rtweet-apir",
        consumer_key = Sys.getenv("TWITTER_CONSUMER_KEY"),
        consumer_secret = Sys.getenv("TWITTER_CONSUMER_SECRET"))
      saveRDS(twitter_token, rtweet_key)
    }
    Sys.setenv(TWITTER_PAT = rtweet_key)
  }
  load_rtweet()
})

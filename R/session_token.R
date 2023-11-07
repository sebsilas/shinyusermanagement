




get_session_token <- function() {

  session_token <- sample(c(LETTERS, 0:9), 16, replace = TRUE) %>% paste0(collapse = "") %>%
    digest::hmac(key = Sys.getenv(x = "ENCRYPTION_KEY"),
                       object = .,
                       algo = "sha512")

}


get_and_write_session_token <- function(username) {

  session_token <- get_session_token()

  conn <- connect_to_db()

  DBI::dbWriteTable(conn,
                    name = "session_tokens",
                    value = data.frame(username = username,
                                       session_token = session_token),
                    overwrite = TRUE)

  DBI::dbDisconnect(conn = conn)

  logging::loginfo(paste0("Session token for user ", username, " written to DB."))

  session_token

}


# get_and_write_session_token("Seb")



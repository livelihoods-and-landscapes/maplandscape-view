# # Google OAuth2.0 app
# # Adapted from Wickham (2017) - https://gist.github.com/hadley/144c406871768d0cbe66b0b810160528
# 
# if (interactive()) {
#   # testing url
#   options(shiny.port = 3838)
#   APP_URL <- "http://localhost:3838/"
# } else {
#   # deployed URL
#   APP_URL <- oauth_redirect_url
# }
# 
# # Note that secret is not really secret, and it's fine to include inline -
# app <- httr::oauth_app("shiny",
#                        key = oauth_key,
#                        secret = oauth_secret,
#                        redirect_uri = APP_URL
# )
# 
# # Google OAugh endpoint
# api <- httr::oauth_endpoints("google")
# 
# # Always request the minimal scope needed. 
# scope <- oauth_scope
# 
# url <- httr::oauth2.0_authorize_url(
#   api, 
#   app, 
#   scope = scope
# )
library(httptest)

# # for contexts where the package needs to be fooled
# # (CRAN, forks)
# # this is ok because the package will used recorded responses
# # so no need for a real secret
# if (!nzchar(Sys.getenv("apikey"))) {
#   Sys.setenv(GITHUB_PAT = "foobar")
# }
# if (!nzchar(Sys.getenv("sid"))) {
#   Sys.setenv(GITHUB_PAT = "foobar")
# }
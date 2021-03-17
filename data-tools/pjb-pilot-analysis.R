## Get local data
library("curl")
library("httr")
library("jsonlite")

studies_url <- "/api/v1/studies.json"

congame_api <- function(path) {
  local_base_url <- "http://127.0.0.1:5100"
  local_url = modify_url(local_base_url, path=path)
  local_api_key <- Sys.getenv("CONGAME_LOCAL_API_KEY")

  #production_base_url <- "https://totalinsightmanagement.com"
  #api_key <- Sys.getenv("CONGAME_API_KEY")

  resp <- GET(url=local_url, add_headers(authorization = local_api_key))
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call = FALSE)
  }

  parsed <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)

  structure(
    list(
      content=parsed,
      path=path,
      response=resp
    ),
    class = 'congame_api'
  )
}

print.congame_api <- function(x, ...) {
  cat("<congame ", x$path, ">\n", sep="")
  str(x$content)
  invisible(x)
}

congame_participant_api <- function(study_id, instance_id) {
  congame_api(paste0("/api/v1/studies/", study_id, "/instances/", instance_id, "/participants.json"))
}

## Get local data
library("curl")
library("httr")
library("jsonlite")
library("tidyverse")

studies_url <- "/api/v1/studies.json"

congame_api <- function(path, is_local) {
  if (is_local) {
    base_url <- "http://127.0.0.1:5100"
    url = modify_url(base_url, path=path)
    api_key <- Sys.getenv("CONGAME_LOCAL_API_KEY")
  } else {
    base_url <- "https://totalinsightmanagement.com"
    url = modify_url(base_url, path=path)
    api_key <- Sys.getenv("CONGAME_API_KEY")
  }

  resp <- GET(url=url, add_headers(authorization = api_key))
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call = FALSE)
  }

  parsed <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)
  parsed_df <- jsonlite::fromJSON(content(resp, "text", simplifyVector = TRUE, flatten = TRUE))

  structure(
    list(
      content=parsed,
      df = parsed_df,
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

congame_participant_api <- function(study_id, instance_id, is_local = TRUE) {
  congame_api(paste0("/api/v1/studies/", study_id, "/instances/", instance_id, "/participants.json"), is_local)
}

## Shape of the data:
# instance-id
# study-id
# list of participants
# - instance-id
# - study-id
# - participant-id
# - vars: list of steps
#   - each step is a list of 7 items:
#     - first-put-at: chr, but really a datetime
#     - group: chr
#     - id: chr
#     - last-put-at: chr, but really a datetime
#     - round: chr
#     - stack: list of multiple chr, e.g. list("*root*", "the-study")
#     - value: depends on the type of step, can be anything probably
#     The combination of id and stack is unique in this experiment. More generally one might need to disambiguate via group x round for uniqueness.

djson <- readRDS("prolific-pilot.rds")

## Check that all participants have identical names (they do)
stopifnot(djson$participants %>% map(names) %>% unique() %>% length() == 1)

vars_to_keep <- c(
  "consent?",
  "practice-tasks",
  "required-tasks",
  "participation-fee",
  "relax-treatment",
  "tutorial-success?",
  "consent?",
  "rest-treatment"
)

stack_for_vars <- list(
  "practice-tasks" = "*root*||the-study",
  "required-tasks" = "*root*||the-study",
  "participation-fee" = "*root*||the-study",
  "relax-treatment" = "*root*||the-study",
  "tutorial-success?" = "*root*||the-study",
  "consent?" = "*root*||the-study",
  "rest-treatment" = "*root*||the-study"
)

df <- map_dfr(djson$participants,
              function(p) {
                c(
                  c(
                    pid = p[["participant-id"]],
                    instance_id = p[["instance-id"]],
                    study_id = p[["study-id"]],
                    practice_tasks = p$vars[["practice-tasks"]]
                  ),
                  pivot_wider(
                    map_df(
                      keep(p$vars,
                           ~ (.$id %in% vars_to_keep) && (paste0(.$stack, collapse = "||") == stack_for_vars[[.$id]])),
                      ~ c(task_id = .$id, value = if_else(is.list(.$value), flatten(.$value), .$value))
                    ),
                    names_from = "task_id",
                    values_from = "value"
                  ))
              })

get_df <- function(f) {
  map_df(djson$participants,
          function(p) {
            cbind(
              tibble(
                pid = p[["participant-id"]],
                instance_id = p[["instance-id"]],
                study_id = p[["study-id"]],
                practice_tasks = p$vars[["practice-tasks"]]
              ),
              f(p$vars)
            )})
}

get_step <- function(step_id, stack) {
  function(pvars) {
    keep(pvars, ~ (.$id == step_id) && (paste0(.$stack, collapse = "||") == stack))
  }
}

get_WTWs <- function(pvars) {
  r <- get_step("WTWs", "*root*||the-study")(pvars)
  if (length(r) == 1)  return(as_tibble(r[[1]]$value)) else return(tibble(pl5 = NA, pl8 = NA, pl11 = NA, pl15 = NA))
}

get_debrief <- function(pvars) {
  r <- get_step("debrief-survey", "*root*||the-study")(pvars)
  if (length(r) == 1) {
    rv <- r[[1]]$value
    tibble(
      "comments" = rv[["comments"]],
      "gender"   = rv[["gender"]]
    )
  } else {
    tibble(
      "comments" = NA,
      "gender" = NA
    )
  }
}

df_WTWs <- get_df(get_WTWs)

df_debrief_survey <- get_df(get_debrief)

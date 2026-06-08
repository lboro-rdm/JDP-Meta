library(jsonlite)
library(dplyr)

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

raw <- fromJSON("data.json", simplifyVector = FALSE)

df <- lapply(raw, function(rec) {
  j  <- rec$journal
  q1 <- j$audit$questions$question_1
  data.frame(
    journal      = j$name %||% NA_character_,
    publisher    = j$publisher$name %||% NA_character_,
    oa_status    = j$open_access$status %||% NA_character_,
    data_sharing = q1$short_answer %||% NA_character_,
    long_answer  = q1$long_answer  %||% NA_character_,
    audit_date   = q1$date %||% NA_character_,
    subjects     = paste(j$subjects$items %||% character(0), collapse = ", "),
    stringsAsFactors = FALSE
  )
}) |> bind_rows()

policy_levels <- c("No Policy", "Encouraged", "Required", "Verified")
policy_colours <- c(
  "No Policy"  = "#d73027",
  "Encouraged" = "#fc8d59",
  "Required"   = "#91bfdb",
  "Verified"   = "#4575b4"
)

df$data_sharing <- factor(df$data_sharing, levels = policy_levels)
publishers <- c("All", sort(unique(na.omit(df$publisher))))
oa_opts    <- c("All", sort(unique(na.omit(df$oa_status))))
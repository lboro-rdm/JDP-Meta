library(jsonlite)
library(dplyr)

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

raw <- fromJSON("data.json", simplifyVector = FALSE)

policy_levels_2024 <- c("No Policy", "Encouraged", "Required", "Verified")
policy_levels_2008 <- c("No Policy", "Weak Policy", "Strong Policy")
unified_levels     <- c("No Policy", "Encouraged", "Required", "Verified")

policy_map_2008 <- c(
  "No Policy"     = "No Policy",
  "Weak Policy"   = "Encouraged",
  "Strong Policy" = "Required"
)

df <- lapply(raw, function(rec) {
  j  <- rec$journal
  q1 <- j$audit$questions$question_1   # 2024 study
  q2 <- j$audit$questions$question_2   # 2008 study
  
  base <- data.frame(
    journal   = j$name %||% NA_character_,
    publisher = j$publisher$name %||% NA_character_,
    oa_status = j$open_access$status %||% NA_character_,
    subjects  = paste(j$subjects$items %||% character(0), collapse = ", "),
    stringsAsFactors = FALSE
  )
  
  row_2024 <- cbind(base, data.frame(
    study_year   = 2024L,
    policy_raw   = q1$short_answer %||% NA_character_,
    long_answer  = q1$long_answer  %||% NA_character_,
    audit_date   = q1$date         %||% NA_character_,
    stringsAsFactors = FALSE
  ))
  
  row_2008 <- cbind(base, data.frame(
    study_year   = 2008L,
    policy_raw   = q2$short_answer %||% NA_character_,
    long_answer  = q2$long_answer  %||% NA_character_,
    audit_date   = q2$date         %||% NA_character_,
    stringsAsFactors = FALSE
  ))
  
  bind_rows(row_2024, row_2008)
}) |> bind_rows()

# Map each study's raw label onto the unified 2024-style scale
df$policy_unified <- ifelse(
  df$study_year == 2008,
  policy_map_2008[df$policy_raw],
  df$policy_raw
)
df$policy_unified <- factor(df$policy_unified, levels = unified_levels)

# Keep the original per-study scale too, useful for tooltips/detail views
df$policy_raw <- factor(
  df$policy_raw,
  levels = union(policy_levels_2024, policy_levels_2008)
)

policy_colours_unified <- c(
  "No Policy"  = "#d73027",
  "Encouraged" = "#fc8d59",
  "Required"   = "#91bfdb",
  "Verified"   = "#4575b4"
)

publishers <- c("All", sort(unique(na.omit(df$publisher))))
oa_opts    <- c("All", sort(unique(na.omit(df$oa_status))))
year_opts  <- c("All", sort(unique(df$study_year)))
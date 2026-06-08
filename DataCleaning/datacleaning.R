library(jsonlite)
library(tidyverse)
library(openalexR)
library(janitor)

# RAINEYROE2024 -----------------------------------------------------------
df <- read.csv("DataCleaning/OriginalData/RaineyRoe2024.csv") %>% 
  select(-apsa_journal, -issn) %>%
  rename(data_sharing = policy, issn = eissn) %>% 
  filter(!is.na(data_sharing))

# Helper: fetch OpenAlex source data by ISSN
fetch_openalex_source <- function(issn) {
  tryCatch({
    result <- oa_fetch(
      entity = "sources",
      issn = issn,
      verbose = FALSE
    )
    if (is.null(result) || nrow(result) == 0) return(NULL)
    result[1, ]
  }, error = function(e) NULL)
}

# Setup progress bar
n <- nrow(df)
pb <- txtProgressBar(min = 0, max = n, style = 3)

# Map df to enriched JSON structure
records <- map(seq_len(n), function(i) {
  
  row          <- df[i, ]
  journal      <- row$journal
  issn         <- row$issn
  data_sharing <- row$data_sharing
  
  src <- fetch_openalex_source(issn)
  
  publisher_name <- if (!is.null(src)) src$host_organization_name[[1]] %||% "" else ""
  
  subjects <- if (!is.null(src) && !is.null(src$topics[[1]])) {
    src$topics[[1]] %>%
      filter(name == "subfield") %>%
      pull(display_name) %>%
      unique() %>%
      as.list()
  } else {
    list()
  }
  
  oa_status <- if (!is.null(src)) {
    case_when(
      isTRUE(src$is_oa[[1]])      ~ "fully_oa",
      isTRUE(src$is_in_doaj[[1]]) ~ "doaj",
      TRUE                         ~ "unknown"
    )
  } else {
    "unknown"
  }
  
  setTxtProgressBar(pb, i)
  
  list(
    journal = list(
      name      = if (!is.null(src)) src$display_name[[1]] %||% journal else journal,
      issn      = issn,
      publisher = list(
        name   = publisher_name,
        source = "OpenAlex"
      ),
      subjects = list(
        items  = subjects,
        source = "OpenAlex"
      ),
      open_access = list(
        status = oa_status,
        source = "OpenAlex"
      ),
      audit = list(
        questions = list(
          question_1 = list(
            text         = "data_sharing",
            standardised = TRUE,
            answer       = data_sharing,
            date         = "2024",
            source       = list(
              identifier_type = "DOI",
              identifier      = "10.7910/DVN/0YSJCX"
            )
          )
        )
      )
    )
  )
})

close(pb)

# Convert to JSON
json <- toJSON(records, pretty = TRUE, auto_unbox = TRUE)
write(json, "data.json")


# Change answer type ------------------------------------------------------

long_answers <- list(
  "No Policy"  = "The journal does not have a posted policy that encourages or requires authors to share their data and/or code with the published article (or explicitly states that there is no sharing policy in place).",
  "Encouraged" = "The journal has a posted policy that encourages (but does not require) authors to share their data and/or code with the published article.",
  "Required"   = "The journal has a posted policy that requires authors to share their data and/or code with the published article (but the journal does not verify the results).",
  "Verified"   = "The journal has a posted policy that requires authors to share their data and/or code with the published article and the journal verifies the results."
)

data <- fromJSON("data.json", simplifyVector = FALSE)

data <- lapply(data, function(entry) {
  q1 <- entry$journal$audit$questions$question_1
  short <- q1$answer
  q1$answer       <- NULL
  q1$short_answer <- short
  q1$long_answer  <- long_answers[[short]]
  entry$journal$audit$questions$question_1 <- q1
  entry
})

write(toJSON(data, pretty = TRUE, auto_unbox = TRUE), "data.json")

# PiwowarChapman2008 ------------------------------------------------------

# 0 = no policy - no mention of sharing microarray data
# 1 = weak policy - a weak suggestion or requirement for sharing microarray data,
# 2 = strong policy - a strong and well-described requirement for sharing microarray data

# Question: Data-policy

data <- read.csv("DataCleaning/OriginalData/PiwowarChapman2008.csv") %>% 
  clean_names()

short_map <- c(
  "0" = "No Policy",
  "1" = "Weak Policy",
  "2" = "Strong Policy"
)

long_map <- c(
  "0" = "No mention of sharing microarray data.",
  "1" = "A weak suggestion or requirement for sharing microarray data.",
  "2" = "A strong and well-described requirement for sharing microarray data."
)

data_filtered <- data %>%
  select(issn, strength_tri) %>%
  mutate(
    short_answer = short_map[as.character(strength_tri)],
    long_answer  = long_map[as.character(strength_tri)]
  )


fetch_openalex <- function(issn) {
  if (is.na(issn)) return(NULL)
  
  tryCatch(
    oa_fetch(entity = "sources", issn = issn),
    error = function(e) NULL
  )
}

openalex_df <- map(data_filtered$issn, fetch_openalex) |>
  bind_rows() |>
  select(
    issn      = issn_l,
    title     = display_name,
    publisher = host_organization_name,
    subjects  = topics,
    oa_status = is_oa
  ) |>
  mutate(
    subjects = map(subjects, function(s) {
      if (is.null(s) || nrow(s) == 0) return(list())
      s |>
        filter(name == "subfield") |>
        pull(display_name) |>
        unique() |>
        as.list()
    })
  )

data_filtered <- left_join(data_filtered, openalex_df, by = "issn")

data_filtered <- data_filtered %>%
  mutate(
    date   = "2008",
    source = list(list(identifier_type = "DOI", identifier = "10.1038/npre.2008.1700.1"))
  )

# Merge with JSOM

nested <- map(seq_len(nrow(data_filtered)), function(i) {
  row <- data_filtered[i, ]
  
  list(
    journal = list(
      name     = row$title,
      issn     = row$issn,
      publisher = list(
        name   = row$publisher,
        source = "OpenAlex"
      ),
      subjects = list(
        items  = row$subjects[[1]],
        source = "OpenAlex"
      ),
      open_access = list(
        status = ifelse(row$oa_status, "open", "closed"),
        source = "OpenAlex"
      ),
      audit = list(
        questions = list(
          question_2 = list(
            text         = "microarray_sharing",
            standardised = TRUE,
            date         = row$date,
            source       = list(
              identifier_type = "DOI",
              identifier      = "10.1038/npre.2008.1700.1"
            ),
            short_answer = row$short_answer,
            long_answer  = row$long_answer
          )
        )
      )
    )
  )
})

write(toJSON(nested, pretty = TRUE, auto_unbox = TRUE), "data_filtered.json")

raw_json      <- fromJSON("data.json",          simplifyVector = FALSE)
filtered_json <- fromJSON("data_filtered.json", simplifyVector = FALSE)

raw_issns      <- map(raw_json,      ~ .x$journal$issn %||% NA_character_) |> unlist()
filtered_issns <- map(filtered_json, ~ .x$journal$issn %||% NA_character_) |> unlist()

intersect(raw_issns, filtered_issns)

combined <- c(raw_json, filtered_json)

write(toJSON(combined, pretty = TRUE, auto_unbox = TRUE), "data.json")

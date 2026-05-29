library(jsonlite)
library(tidyverse)
library(openalexR)

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

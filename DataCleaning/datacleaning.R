library(jsonlite)
library(tidyverse)
library(openalexR)

# RAINEYROE2024 -----------------------------------------------------------

# Read the CSV
df <- read.csv("DataCleaning/OriginalData/RaineyRoe2024.csv") %>% 
  select(-apsa_journal, -issn) %>%
  rename(data_sharing = policy, issn = eissn) %>% 
  filter(!is.na(data_sharing))

# Map the df to json structure
records <- pmap(df, function(journal, issn, data_sharing) {
  
  list(
    journal = journal,
    issn = issn,
    audit = list(
      questions = list(
        question_1 = list(
          text = "data_sharing",
          standardised = TRUE,
          answer = data_sharing,
          date = "2024",
          source = list(
            identifier_type = "DOI",
            identifier = "10.7910/DVN/0YSJCX"
          )
        )
      )
    )
  )
})

# Convert to JSON
json <- toJSON(records, pretty = TRUE, auto_unbox = TRUE)

# Save to file
write(json, "data.json")

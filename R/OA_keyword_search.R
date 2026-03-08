rm(list=ls())
##################################################
# Search in Open Alex using keywords
#
# Get openalexR 
# https://docs.ropensci.org/openalexR/
# Set openalexR.mailto option 
# https://docs.openalex.org/how-to-use-the-api/rate-limits-and-authentication#the-polite-pool
##################################################

### Settings----

## Your working directory (will be set using function in setting.R)
your_dir <- dirname(rstudioapi::getSourceEditorContext()$path) # works only in RStudio
#your_dir <- "path_to_where_code_is" # complete accordingly

## Source useful functions from folder downloaded from GitHub
useful_funcs = list.files(path = paste0(your_dir, '/func'), full.names = TRUE)
for (i in useful_funcs){
  source(i)
}

## Set working directory and install required libraries
settings()

# Set openalexR.mailto option 
#https://docs.openalex.org/how-to-use-the-api/rate-limits-and-authentication#the-polite-pool
your_email <- "yanina.sica@gmail.com" # complete with your email
options(openalexR.mailto = your_email)

#options(openalexR.apikey = "EXAMPLE_APIKEY") #for premium users

## Read installed libraries
library(stringr)
#library(gtools)
library(dplyr)
library(tidyr)
library(readr)
library(data.table)
library(openxlsx)

library(ggplot2)

library(openalexR)
# file.edit("~/.Rprofile")
# options(openalexR.apikey = "tkXqrtWG8WkVn2ypXYT1cl")

### Set topic and iteration----
group = 'whales'
iteration = 'iter_2'

# create dirs
topic_dir = paste0("../output/",group, "/", iteration)

if(!dir.exists(topic_dir)){
  dir.create(topic_dir, recursive = TRUE)}

### Get keywords or search terms----
# The search terms were co-designed with experts.
# Originals here: https://drive.google.com/drive/folders/1KxQCxxAMw9oHy9hgryhSrz0lTlxINJh8

search_terms1 <- readLines(paste0("../input/", group, "/search_terms/",iteration, "/species.txt")) #with breaks
search_terms2 <- readLines(paste0("../input/", group, "/search_terms/",iteration, "/regions.txt")) #with breaks
search_terms3 <- readLines(paste0("../input/", group, "/search_terms/",iteration, "/monitoring.txt")) #with breaks
#search_terms4 <- readLines(paste0("../input/search_terms/", "fr_monitoring.txt")) #with breaks

# Add "" and OR
search_terms1 <- paste0('"',search_terms1, '" OR')
search_terms2 <- paste0('"',search_terms2, '" OR')
search_terms3 <- paste0('"',search_terms3, '" OR')
#search_terms4 <- paste0('"',search_terms4, '" OR')

# Remove ' OR' from the last item
search_terms1[length(search_terms1)] <- gsub(" OR$", "", search_terms1[length(search_terms1)])
search_terms2[length(search_terms2)] <- gsub(" OR$", "", search_terms2[length(search_terms2)])
search_terms3[length(search_terms3)] <- gsub(" OR$", "", search_terms3[length(search_terms3)])
#search_terms4[length(search_terms4)] <- gsub(" OR$", "", search_terms4[length(search_terms4)])

# Remove breaks
st1 <- paste0(search_terms1, collapse = " ") %>% 
compact_st() #no breaks
st2 <- paste0(search_terms2, collapse = " ") %>% 
  compact_st() #no breaks
st3 <- paste0(search_terms3, collapse = " ") %>% 
  compact_st() #no breaks

#st4 <- paste0(search_terms4, collapse = " ") %>% 
#  compact_st() #no breaks

# Create search string to use direcly in openalex website
st <- paste0("(", st1, ")"," AND ", "(", st2, ")"," AND ","(", st3, ")")
#st <- paste0("(", st1, ")"," AND ", "(", st2, ")"," AND ","(", st3, ")"," AND ", "(", st4, ")")

cat(st) # Copy and paste in the website

###ERROR (st que si funciona)
#st = '(whale OR minke OR Humpback OR "Balaenoptera bonaerensis" OR "Megaptera novaeangliae" OR orca OR "orcinus orca" OR "killer whale" OR "blue whale" OR baleen OR "fin whale" OR cetacean OR "Balaenoptera musculus" OR "Balaenoptera physalus") AND ("Antarctic Peninsula" OR "South Shetland Islands" OR "South Shetland" OR "South Orkney Island" OR "Bransfield strait" OR "Gerlache Strait" OR "Marguerite Bay" OR "Antarctic Sound" OR "CEMP site" OR "domain 1" OR "subarea 48.1" OR "subarea 48.2" OR "subarea 88.3" OR "area 48" OR 48.1 OR 48.2 OR 88.3 OR "King George Island" OR "Nelson Island" OR "Deception Island" OR "Seymour Island" OR "Elephant island" OR D1MPA OR "scotia arc") AND (monitoring OR monitor OR monitored OR survey OR surveyed OR inventory OR study OR "time-series" OR "time series" OR "data collection" OR "long term" OR Abundance OR densities OR density OR season OR seasonal OR seasonality OR years OR decadal OR decade OR interannual OR "inter-annual" OR trend OR change OR Distribution OR "Sighting Surveys" OR "Photo identification" OR "photo ID" OR "drone survey" OR Telemetry OR "Passive acoustic monitoring" OR Tagging OR tag OR "data logger" OR hydrophone OR PAM OR clicks OR whistles OR "fin ID" OR "gps tag" OR "population recovery" OR bycatch)'


### Keywords analysis----
# Counts of hits

oa_result = openalexR::oa_fetch(
  title_and_abstract.search = st,
  count_only = TRUE,
  verbose = TRUE
)

# since groups are NULL we create the data.frame separately to avoid problems
oa_summary <- data.frame(
  count = oa_result$count,
  db_response_time_ms = oa_result$db_response_time_ms,
  page = oa_result$page,
  per_page = oa_result$per_page,
  groups_count =  'NULL' # we did not ask for any grouping
)
#keyword example c("bibliometric analysis", "science mapping")

# compare to OA website
oa_summary

# Add OA link based on web serach using cat(st)
#oa_summary$oa = 'https://openalex.org/works?page=1&filter=title_and_abstract.search%3A%28%22Pygoscelis%20adeliae%22%20OR%20%22Pygoscelis%20papua%22%20OR%20%22Pygoscelis%20antarctica%22%20OR%20%22Pygoscelis%20antarcticus%22%20OR%20%22Aptenodytes%20forsteri%22%20OR%20%22adelie%20penguin%22%20OR%20%22Ad%C3%A9lie%20penguin%22%20OR%20%22gentoo%20penguin%22%20OR%20%22Gentoo%20penguin%22%20OR%20%22Chinstrap%20penguin%22%20OR%20%22emperor%20penguin%22%20OR%20%22Pygoscelis%22%20OR%20%22Aptenodytes%22%20OR%20%22penguin%22%20OR%20%22adeliae%22%20OR%20%22papua%22%20OR%20%22antarctica%22%20OR%20%22antarcticus%22%20OR%20%22forsteri%22%20OR%20%22adelie%22%20OR%20%22Ad%C3%A9lie%22%20OR%20%22gentoo%22%20OR%20%22Gentoo%22%20OR%20%22emperor%22%29%20AND%20%28%22Antarctic%20Peninsula%20region%22%20OR%20%22Western%20Antarctic%20Peninsula%22%20OR%20%22South%20Shetland%20Islands%22%20OR%20%22South%20Orkney%20ISland%22%20OR%20%22Brandsfield%20strait%22%20OR%20%22Gerlache%20Strait%22%20OR%20%22Marguerite%20Bay%22%20OR%20%22Antarctic%20Sound%22%20OR%20%22CEMP%20site%22%20OR%20%22domain%201%22%20OR%20%22Western%20Antarctic%20Peninsula-Southscotia%20arc%22%20OR%20%22ccamlr%20subarea%22%20OR%20%22ccamlr%20region%22%20OR%20%22ccamlr%20region%2048.1%22%20OR%20%22ccamlr%20region%2048.2%22%20OR%20%22ccamlr%20region%2088.3%22%20OR%20%22ccamlr%20subarea%2048.1%22%20OR%20%22ccamlr%20subarea%2048.2%22%20OR%20%22ccamlr%20subarea%2088.3%22%29%20AND%20%28%22long%20term%22%20OR%20%22trend%22%20OR%20%22change%22%20OR%20%22seasonal%22%20OR%20%22season%22%20OR%20%22years%22%20OR%20%22decadal%22%29%20AND%20%28%22monitoring%22%20OR%20%22survey%22%20OR%20%22inventory%22%20OR%20%22census%22%20OR%20%22study%22%29&id=6emmG9dNmDmj3LpeKprGYA'
#oa_summary$oa = 'https://openalex.org/works?page=1&filter=title_and_abstract.search%3A%28%22Pygoscelis%20adeliae%22%20OR%20%22Pygoscelis%20papua%22%20OR%20%22Pygoscelis%20antarctica%22%20OR%20%22Pygoscelis%20antarcticus%22%20OR%20%22Aptenodytes%20forsteri%22%20OR%20%22Pygoscelis%22%20OR%20%22Aptenodytes%22%20OR%20%22penguin%22%20OR%20%22adeliae%22%20OR%20%22papua%22%20OR%20%22antarcticus%22%20OR%20%22forsteri%22%20OR%20%22adelie%22%20OR%20%22Ad%C3%A9lie%22%20OR%20%22gentoo%22%20OR%20%22Chinstrap%22%20OR%20%22emperor%22%29%20AND%20%28%22Antarctic%20Peninsula%22%20OR%20%22South%20Shetland%20Islands%22%20OR%20%22South%20Orkney%20Island%22%20OR%20%22Brandsfield%20strait%22%20OR%20%22Gerlache%20Strait%22%20OR%20%22Marguerite%20Bay%22%20OR%20%22Antarctic%20Sound%22%20OR%20%22CEMP%20site%22%20OR%20%22domain%201%22%20OR%20%22subarea%2048.1%22%20OR%20%22subarea%2048.2%22%20OR%20%22subarea%2088.3%22%20OR%20%22area%2048%22%20OR%20%2248.1%22%20OR%20%2248.2%22%20OR%20%2288.3%22%20OR%20%22small-scale%20management%20unit%22%20OR%20%22ccamlr%22%29%20AND%20%28%22monitoring%22%20OR%20%22survey%22%20OR%20%22inventory%22%20OR%20%22census%22%20OR%20%22study%22%20OR%20%22Time-series%22%20OR%20%22Time%20series%22%20OR%20%22Monitor%22%20OR%20%22data%20collection%22%20OR%20%22Antarctic%20Site%20Inventory%22%20OR%20%22long%20term%22%20OR%20%22trend%22%20OR%20%22change%22%20OR%20%22seasonal%22%20OR%20%22season%22%20OR%20%22years%22%20OR%20%22decadal%22%20OR%20%22decades%22%20OR%20%22interannual%22%20OR%20%22monitored%22%20OR%20%22counts%22%20OR%20%22observation%22%29&id=2iytNJ1sKMwuJATY13C9sj'
#oa_summary$oa = 'https://openalex.org/works?page=1&filter=title_and_abstract.search%3A%28%22Pygoscelis%20adeliae%22%20OR%20%22Pygoscelis%20papua%22%20OR%20%22Pygoscelis%20antarctica%22%20OR%20%22Pygoscelis%20antarcticus%22%20OR%20%22Aptenodytes%20forsteri%22%20OR%20%22Pygoscelis%22%20OR%20%22Aptenodytes%22%20OR%20%22penguin%22%20OR%20%22adeliae%22%20OR%20%22papua%22%20OR%20%22antarcticus%22%20OR%20%22forsteri%22%20OR%20%22adelie%22%20OR%20%22Ad%C3%A9lie%22%20OR%20%22gentoo%22%20OR%20%22Chinstrap%22%20OR%20%22emperor%22%20OR%20%22seabird%22%29%20AND%20%28%22Antarctic%20Peninsula%22%20OR%20%22South%20Shetland%20Islands%22%20OR%20%22South%20Shetland%22%20OR%20%22South%20Orkney%20Island%22%20OR%20%22Brandsfield%20strait%22%20OR%20%22Gerlache%20Strait%22%20OR%20%22Marguerite%20Bay%22%20OR%20%22Antarctic%20Sound%22%20OR%20%22CEMP%20site%22%20OR%20%22domain%201%22%20OR%20%22subarea%2048.1%22%20OR%20%22subarea%2048.2%22%20OR%20%22subarea%2088.3%22%20OR%20%22area%2048%22%20OR%20%2248.1%22%20OR%20%2248.2%22%20OR%20%2288.3%22%20OR%20%22small-scale%20management%20unit%22%20OR%20%22ccamlr%22%20OR%20%22King%20George%20Island%22%20OR%20%22Nelson%20Island%22%20OR%20%22Deception%20Island%22%20OR%20%22Seymour%20Island%22%29%20AND%20%28%22monitoring%22%20OR%20%22monitor%22%20OR%20%22monitored%22%20OR%20%22survey%22%20OR%20%22surveyed%22%20OR%20%22inventory%22%20OR%20%22census%22%20OR%20%22study%22%20OR%20%22time-series%22%20OR%20%22time%20series%22%20OR%20%22data%20collection%22%20OR%20%22Antarctic%20Site%20Inventory%22%20OR%20%22long%20term%22%20OR%20%22trend%22%20OR%20%22change%22%20OR%20%22seasonal%22%20OR%20%22season%22%20OR%20%22years%22%20OR%20%22decadal%22%20OR%20%22decade%22%20OR%20%22interannual%22%20OR%20%22inter-annual%22%20OR%20%22count%22%20OR%20%22counted%22%20OR%20%22observation%22%20OR%20%22observed%22%20OR%20%22satellite%22%20OR%20%22remote%20sensing%22%20OR%20%22time-lapse%20camera%22%29&id=2EzSqfzZ7TsQqouCkXCW9Q'
#oa_summary$oa = 'https://openalex.org/works?page=1&filter=title_and_abstract.search:(%22krill%22+OR+%22antarctic+krill%22+OR+%22Euphasia+superba%22+OR+%22Euphasia+%22)+AND+(%22Antarctic+Peninsula%22+OR+%22South+Shetland+Islands%22+OR+%22South+Shetland%22+OR+%22South+Orkney+Island%22+OR+%22Brandsfield+strait%22+OR+%22Gerlache+Strait%22+OR+%22Marguerite+Bay%22+OR+%22Antarctic+Sound%22+OR+%22CEMP+site%22+OR+%22domain+1%22+OR+%22subarea+48.1%22+OR+%22subarea+48.2%22+OR+%22subarea+88.3%22+OR+%22area+48%22+OR+%2248.1%22+OR+%2248.2%22+OR+%2288.3%22+OR+%22King+George+Island%22+OR+%22Nelson+Island%22+OR+%22Deception+Island%22+OR+%22Seymour+Island%22+OR+%22Spawning+hotspot%22+OR+%22nursery+area%22)+AND+(%22monitoring%22+OR+%22monitor%22+OR+%22monitored%22+OR+%22survey%22+OR+%22surveyed%22+OR+%22inventory%22+OR+%22study%22+OR+%22time-series%22+OR+%22time+series%22+OR+%22data+collection%22+OR+%22Antarctic+Site+Inventory%22+OR+%22long+term%22+OR+%22Abundance%22+OR+%22biomass%22+OR+%22densities%22+OR+%22density%22+OR+%22season%22+OR+%22seasonal%22+OR+%22seasonality%22+OR+%22years%22+OR+%22decadal%22+OR+%22decade%22+OR+%22interannual%22+OR+%22inter-annual%22+OR+%22trend%22+OR+%22change%22+OR+%22variability%22+OR+%22Distribution%22+OR+%22acoustic%22+OR+%22stock%22+OR+%22fisheries%22+OR+%22harvest%22)&id=mTnpdtXuPAWw9m8Bov3zYK'
#oa_summary$oa = "https://openalex.org/works?page=1&filter=title_and_abstract.search:(%22whale%22+OR+%22minke%22+OR+%22Humpback%22+OR+%22Balaenoptera+bonaerensis%22+OR+%22Megaptera+novaeangliae%22+OR+%22orca%22+OR+%22orcinus+orca%22+OR+%22killer+whale%22)+AND+(%22Antarctic+Peninsula%22+OR+%22South+Shetland+Islands%22+OR+%22South+Shetland%22+OR+%22South+Orkney+Island%22+OR+%22Brandsfield+strait%22+OR+%22Gerlache+Strait%22+OR+%22Marguerite+Bay%22+OR+%22Antarctic+Sound%22+OR+%22CEMP+site%22+OR+%22domain+1%22+OR+%22subarea+48.1%22+OR+%22subarea+48.2%22+OR+%22subarea+88.3%22+OR+%22area+48%22+OR+%2248.1%22+OR+%2248.2%22+OR+%2288.3%22+OR+%22King+George+Island%22+OR+%22Nelson+Island%22+OR+%22Deception+Island%22+OR+%22Seymour+Island%22+OR+%22%22)+AND+(%22monitoring%22+OR+%22monitor%22+OR+%22monitored%22+OR+%22survey%22+OR+%22surveyed%22+OR+%22inventory%22+OR+%22study%22+OR+%22time-series%22+OR+%22time+series%22+OR+%22data+collection%22+OR+%22Antarctic+Site+Inventory%22+OR+%22long+term%22+OR+%22Abundance%22+OR+%22densities%22+OR+%22density%22+OR+%22season%22+OR+%22seasonal%22+OR+%22seasonality%22+OR+%22years%22+OR+%22decadal%22+OR+%22decade%22+OR+%22interannual%22+OR+%22inter-annual%22+OR+%22trend%22+OR+%22change%22+OR+%22variability%22+OR+%22Distribution%22+OR+%22acoustic%22+OR+%22Sighting+Surveys%22+OR+%22Photo-identification%22+OR+%22drone+survey%22+OR+%22Telemetry%22+OR+%22Passive+Acoustic%22+OR+%22Tagging%22+OR+%22tag%22+OR+%22data+logger%22+OR+%22hydrophone%22+OR+%22PAM%22+OR+%22clicks%22+OR+%22whistles%22+OR+%22fin-ID%22+OR+%22gps+tag%22)&id=rEaEMAKmUHijuAYv3ozjy7"
oa_summary$oa = 'https://openalex.org/works?page=1&filter=title_and_abstract.search:(%22whale%22+OR+%22minke%22+OR+%22Humpback%22+OR+%22Balaenoptera+bonaerensis%22+OR+%22Megaptera+novaeangliae%22+OR+%22orca%22+OR+%22orcinus+orca%22+OR+%22killer+whale%22+OR+%22blue+whale%22+OR+%22baleen+%22+OR+%22fin+whale%22+OR+%22cetacean%22+OR+%22Balaenoptera+musculus%22+OR+%22Balaenoptera+physalus%22)+AND+(%22Antarctic+Peninsula%22+OR+%22South+Shetland+Islands%22+OR+%22South+Shetland%22+OR+%22South+Orkney+Island%22+OR+%22Bransfield+strait%22+OR+%22Gerlache+Strait%22+OR+%22Marguerite+Bay%22+OR+%22Antarctic+Sound%22+OR+%22CEMP+site%22+OR+%22domain+1%22+OR+%22subarea+48.1%22+OR+%22subarea+48.2%22+OR+%22subarea+88.3%22+OR+%22area+48%22+OR+%2248.1%22+OR+%2248.2%22+OR+%2288.3%22+OR+%22King+George+Island%22+OR+%22Nelson+Island%22+OR+%22Deception+Island%22+OR+%22Seymour+Island%22+OR+%22Elephant+island%22+OR+%22D1MPA%22+OR+%22scotia+arc%22)+AND+(%22monitoring%22+OR+%22monitor%22+OR+%22monitored%22+OR+%22survey%22+OR+%22surveyed%22+OR+%22inventory%22+OR+%22study%22+OR+%22time-series%22+OR+%22time+series%22+OR+%22data+collection%22+OR+%22long+term%22+OR+%22Abundance%22+OR+%22densities%22+OR+%22density%22+OR+%22season%22+OR+%22seasonal%22+OR+%22seasonality%22+OR+%22years%22+OR+%22decadal%22+OR+%22decade%22+OR+%22interannual%22+OR+%22inter-annual%22+OR+%22trend%22+OR+%22change%22+OR+%22Distribution%22+OR+%22Sighting+Surveys%22+OR+%22Photo+identification%22+OR+%22photo+ID%22+OR+%22drone+survey%22+OR+%22Telemetry%22+OR+%22Passive+acoustic+monitoring%22+OR+%22Tagging%22+OR+%22tag%22+OR+%22data+logger%22+OR+%22hydrophone%22+OR+%22PAM%22+OR+%22clicks%22+OR+%22whistles%22+OR+%22fin+ID%22+OR+%22gps+tag%22+OR+%22population+recovery%22+OR+%22bycatch%22)'

# DO NOT RUN FOR NOW
# Contribution of each term individually (the counts exclude hits that can be retrieved with other terms)
# indiv_term_contr = assess_search_term(
#   st = search_terms2, # keywords with breaks or lists
#   AND_term = search_terms1,
#   remove = " OR$",
#   excl_others = TRUE
#   ) 
# 
# # Contribution of each term (including hits with shared terms)
# comb_term_contr = assess_search_term(
#   st = search_terms2, # keywords with breaks
#   AND_term = search_terms1,
#   remove = " OR$",
#   excl_others = FALSE) # counts includes hits that have other terms
# 
# term_contr = left_join(indiv_term_contr, comb_term_contr, by = 'term') %>% 
#   rename('indiv_term_count' = count.x, 'comb_term_count' = count.y)
# 
# # save search term contributions
# #write_csv(term_contr, paste0("../", topic, "/keyword_analysis", iteration, ".xlsx"))
# write.xlsx(term_contr, paste0("../", topic, "/keyword_analysis", iteration, ".xlsx"))

### Counts of hits (works in OA)----

# Info on OA filters: https://docs.openalex.org/api-entities/works/filter-works
search_outputs_byType <- oa_fetch(
  entity = 'works',
  title_and_abstract.search = st, # keywords without breaks in titles and abstracts
  group_by = "type", #type
  abstract = TRUE,
  count_only = FALSE,
  mailto = oa_email(),
  verbose = TRUE
)

search_outputs_bySourceType <- oa_fetch(
  entity = 'works',
  title_and_abstract.search = st, # keywords without breaks in titles and abstracts
  group_by = "primary_location.source.type", # source_type
  abstract = TRUE,
  count_only = FALSE,
  mailto = oa_email(),
  verbose = TRUE
)

search_outputs_byTopic <- oa_fetch(
  entity = 'works',
  title_and_abstract.search = st, # keywords without breaks in titles and abstracts
  group_by = "topics.id", # topics
  abstract = TRUE,
  count_only = FALSE,
  mailto = oa_email(),
  verbose = TRUE
)

# Save as excel sheet with multiple tabs
# Create a blank workbook
OUT <- createWorkbook()

# Add some sheets to the workbook
addWorksheet(OUT, "OAsummary")
addWorksheet(OUT, "type")
addWorksheet(OUT, "sourceType")
addWorksheet(OUT, "topic")

# Write the data to the sheets
writeData(OUT, sheet = "OAsummary", x = oa_summary)
writeData(OUT, sheet = "type", x = dplyr::select(search_outputs_byType, -key))
writeData(OUT, sheet = "sourceType", x = dplyr::select(search_outputs_bySourceType, -key))
writeData(OUT, sheet = "topic", x = dplyr::select(search_outputs_byTopic, -key))

# Export the file
saveWorkbook(OUT, paste0("../output/", group, "/", iteration, "/oa_results.xlsx"),overwrite = TRUE)

### Get data from OpenAlex----
search_outputs <- oa_fetch(
  entity = "works", #oa_entities()
  title_and_abstract.search = st,
  count_only = FALSE,
  verbose = TRUE,
  mailto = oa_email())

# the url only shows the first page

# save output
write_csv(search_outputs, paste0("../output/", group, "/", iteration, "/oa_search.csv"))

# Clean results
names(search_outputs)
search_outputs_clean = search_outputs %>% 
  # remove GBIF downloads (REHACER con GBIF.Org User en author!!!!!!)
  mutate(remove = if_else(grepl("www[.]gbif[.]org/occurrence/download",oa_url),
                                  true = 'yes',
                                  false = 'no')) %>% 
  filter(remove == 'no') %>% 
  # authorship
  mutate(author = sapply(authorships, function(x) {
    paste(x$display_name, collapse = "; ")
  })) %>% 
  # # keywords
  # mutate(keyword = sapply(keywords, function(y) {
  #   paste(y$display_name, collapse = "; ")
  # })) %>%
  # concepts
  # mutate(concept = sapply(concepts, function(x) {
  #   # Filter for scores greater than 0.5
  #   filtered_scores <- x[x$score > 0.5, ]
  #   # Combine the display names of the filtered authors
  #   paste(filtered_scores$display_name, collapse = "; ")
  # })) %>% 
  # topics (topic)
  mutate(topic = sapply(topics, function(x) {
  # Filter for types
    filtered_topics <- x[x$type == 'topic', ]
  # Combine the display names of the filtered topics
    paste(filtered_topics$display_name, collapse = "; ")
  })) %>% 
  # topics (subfield)
  mutate(subfield = sapply(topics, function(x) {
    # Filter for types
    filtered_subfield <- x[x$type == 'subfield', ]
    # Combine the display names of the filtered topics
    paste(filtered_subfield$display_name, collapse = "; ")
  })) %>% 
  # clean columns
  dplyr::select("title","abstract",
                "publication_year", "doi",        
                "type","oa_url","author",
                #"keyword","concept",
                "topic","subfield","pdf_url",
                "cited_by_count") %>% 
  # add id col
  mutate(id = row_number())

# save output
write_csv(search_outputs_clean, paste0("../output/",group, "/", iteration, "/oa_search_clean.csv"))

### Validation----
## DO NOT RUN, SPECIFIC FOR PENGUIN DATA 
# 
# # Find extra articles for the new search
# search_outputs_old = read_csv(paste0("../output/iter_3/oa_search.csv")) # not saving properly so downloaded again
# names(search_outputs_old)
# 
# only_new_search_clean = search_outputs_clean %>% 
#   anti_join(search_outputs_clean_old, by = 'doi')
# #c('id', 'doi')
# write_csv(only_new_search_clean, paste0("../output/", iteration, "/onlyIter4_oa_search_clean.csv"))
# 
# 
# 
# # Checks against validation dataset (https://www.penguinmap.com/mapppd/sources/)
# # Original query: CountQuery_V_4_3. Had many issues in the references that I manually edited (Query060525)
# 
# valid = read_csv("../input/PenguinMap/Query060525_withref.csv")
# #valid %>% filter(is.na(ref_title)) %>% View() # 178 Personal communications and unpublished datasets
# valid %>%  distinct(doi) %>%  count() #39
# valid %>%  distinct(ref_title) %>%  count() #116
# #quizas cometi un error al limpia las referencias. Me faltan 2 creo
# 
# valid_ref = valid %>% 
#   #filter(!is.na(title)) %>% 
#   mutate(validation_set = TRUE) %>% 
#   dplyr::mutate(title = tolower(ref_title)) %>%
#   dplyr::mutate(title = gsub('  ', '',title)) %>% 
#   dplyr::mutate(title = str_trim(title)) %>% 
#   dplyr::mutate(title = gsub("[.]$","",title)) %>% 
#   dplyr::select(id_valid = id,ref_type, doi,title,ref_year,validation_set) %>% 
#   dplyr::group_by(doi) %>% 
#   dplyr::mutate(ids_valid = paste0(id_valid, collapse = ";")) %>% 
#   dplyr::distinct(ref_type,title,doi, ref_year,.keep_all = TRUE)
# 
# search_outputs_clean2 = search_outputs_clean %>% 
#   mutate(search_set = TRUE) %>% 
#   dplyr::mutate(title = tolower(title)) %>%
#   dplyr::mutate(title = gsub('  ', '',title)) %>% 
#   dplyr::mutate(title = str_trim(title)) %>% 
#   dplyr::mutate(title = gsub("[.]$","",title)) %>% 
#   tidyr::separate(doi, into = c('extra','doi2'), sep =  '[.]org[/]') %>% 
#   dplyr::select(type,doi = doi2, title ,search_set, id_search = id)
# 
# # references shared in OA search and PenguinMap
# shared_ref_doi = left_join(filter(search_outputs_clean2, !is.na(doi)),
#                                valid_ref, 
#                                by = 'doi') %>% 
#   filter(validation_set == TRUE & search_set == TRUE) %>% # 14
#   dplyr::select(doi, title = title.x,validation_set,id_valid,id_search)
# 
# shared_ref_title = left_join(filter(search_outputs_clean2, !is.na(title)),
#                                    valid_ref, 
#                                    by = 'title') %>% 
#   filter(validation_set == TRUE & search_set == TRUE) %>% 
#   dplyr::select(doi = doi.x, title,validation_set,id_search,id_valid)
# 
# shared_ref = rbind(shared_ref_title, shared_ref_doi) %>% 
#   distinct(id_search, .keep_all = TRUE) %>% 
#   dplyr::select(id_search, id_valid, validation_set) #18
# 
# # references missing from OA search
# onlyPenguinMap_doi = anti_join(valid_ref, filter(search_outputs_clean2, !is.na(doi)),
#                            by = 'doi') %>% 
#   filter(ref_type == 'article') #52
# 
# onlyPenguinMap_title = anti_join(valid_ref, filter(search_outputs_clean2, !is.na(title)),
#                              by = 'title') %>% 
#   filter(ref_type == 'article') #59         
# 
# onlyPenguinMap = rbind(onlyPenguinMap_doi, onlyPenguinMap_title) %>% 
#   distinct(id_valid, .keep_all = TRUE) #62
# write_csv(onlyPenguinMap, "../input/PenguinMap/missing_ref_OAsearch.csv")
# 
# names(onlyPenguinMap_title)
# # join to results
# 
# search_outputs_clean = left_join(search_outputs_clean, check_completeness)
# write_csv(search_outputs_clean, paste0("../output/", iteration, "/oa_search_clean.csv"))
# 
# # Check why the articles in PenguinMap did not appear in OA search
# penguinMapOnlyDoi = anti_join(valid_ref, search_outputs_clean2, by = 'doi')

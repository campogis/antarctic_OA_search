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
library(gtools)
library(dplyr)
library(tidyr)
library(readr)
library(data.table)
library(openxlsx)

library(ggplot2)

library(openalexR)
library(IPBES.R)
library(pbapply)

### Set topic and iteration----
#topics = c('species_data', 'EO_data', 'NCP', 'ecosystem_management', 'governance', 'ILK')
iteration = 'iter_3'

# create dirs
topic_dir = paste0("../output/", iteration)

if(!dir.exists(topic_dir)){
  dir.create(topic_dir, recursive = TRUE)}

### Get keywords or search terms----
# The search terms were provided by the authors, and some adaptations were done by the Data TSU.
# Originals here: https://docs.google.com/spreadsheets/d/10RShduVfv16TF5KptDbpRu8yeeXBMeklPEx_kiGpOao/edit?gid=273457891#gid=273457891

search_terms1 <- readLines(paste0("../input/search_terms/",iteration, "/species.txt")) #with breaks
search_terms2 <- readLines(paste0("../input/search_terms/", iteration, "/regions.txt")) #with breaks
search_terms3 <- readLines(paste0("../input/search_terms/", iteration, "/monitoring.txt")) #with breaks
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

#st <- paste0("(", st1, ")"," AND ", "(", st2, ")"," AND ","(", st3, ")"," AND ", "(", st4, ")")
st <- paste0("(", st1, ")"," AND ", "(", st2, ")"," AND ","(", st3, ")")
cat(st)

### Keywords analysis----
# Counts of hits
oa_summary = as.data.frame(openalexR::oa_fetch(title_and_abstract.search = st, # keywords without breaks in titles and abstracts
                    count_only = TRUE, # provide summary of hits
                    verbose = TRUE))
# Add OA link based on web serach using cat(st)
#oa_summary$oa = 'https://openalex.org/works?page=1&filter=title_and_abstract.search%3A%28%22Pygoscelis%20adeliae%22%20OR%20%22Pygoscelis%20papua%22%20OR%20%22Pygoscelis%20antarctica%22%20OR%20%22Pygoscelis%20antarcticus%22%20OR%20%22Aptenodytes%20forsteri%22%20OR%20%22adelie%20penguin%22%20OR%20%22Ad%C3%A9lie%20penguin%22%20OR%20%22gentoo%20penguin%22%20OR%20%22Gentoo%20penguin%22%20OR%20%22Chinstrap%20penguin%22%20OR%20%22emperor%20penguin%22%20OR%20%22Pygoscelis%22%20OR%20%22Aptenodytes%22%20OR%20%22penguin%22%20OR%20%22adeliae%22%20OR%20%22papua%22%20OR%20%22antarctica%22%20OR%20%22antarcticus%22%20OR%20%22forsteri%22%20OR%20%22adelie%22%20OR%20%22Ad%C3%A9lie%22%20OR%20%22gentoo%22%20OR%20%22Gentoo%22%20OR%20%22emperor%22%29%20AND%20%28%22Antarctic%20Peninsula%20region%22%20OR%20%22Western%20Antarctic%20Peninsula%22%20OR%20%22South%20Shetland%20Islands%22%20OR%20%22South%20Orkney%20ISland%22%20OR%20%22Brandsfield%20strait%22%20OR%20%22Gerlache%20Strait%22%20OR%20%22Marguerite%20Bay%22%20OR%20%22Antarctic%20Sound%22%20OR%20%22CEMP%20site%22%20OR%20%22domain%201%22%20OR%20%22Western%20Antarctic%20Peninsula-Southscotia%20arc%22%20OR%20%22ccamlr%20subarea%22%20OR%20%22ccamlr%20region%22%20OR%20%22ccamlr%20region%2048.1%22%20OR%20%22ccamlr%20region%2048.2%22%20OR%20%22ccamlr%20region%2088.3%22%20OR%20%22ccamlr%20subarea%2048.1%22%20OR%20%22ccamlr%20subarea%2048.2%22%20OR%20%22ccamlr%20subarea%2088.3%22%29%20AND%20%28%22long%20term%22%20OR%20%22trend%22%20OR%20%22change%22%20OR%20%22seasonal%22%20OR%20%22season%22%20OR%20%22years%22%20OR%20%22decadal%22%29%20AND%20%28%22monitoring%22%20OR%20%22survey%22%20OR%20%22inventory%22%20OR%20%22census%22%20OR%20%22study%22%29&id=6emmG9dNmDmj3LpeKprGYA'
oa_summary$oa = 'https://openalex.org/works?page=1&filter=title_and_abstract.search%3A%28%22Pygoscelis%20adeliae%22%20OR%20%22Pygoscelis%20papua%22%20OR%20%22Pygoscelis%20antarctica%22%20OR%20%22Pygoscelis%20antarcticus%22%20OR%20%22Aptenodytes%20forsteri%22%20OR%20%22Pygoscelis%22%20OR%20%22Aptenodytes%22%20OR%20%22penguin%22%20OR%20%22adeliae%22%20OR%20%22papua%22%20OR%20%22antarcticus%22%20OR%20%22forsteri%22%20OR%20%22adelie%22%20OR%20%22Ad%C3%A9lie%22%20OR%20%22gentoo%22%20OR%20%22Chinstrap%22%20OR%20%22emperor%22%29%20AND%20%28%22Antarctic%20Peninsula%22%20OR%20%22South%20Shetland%20Islands%22%20OR%20%22South%20Orkney%20Island%22%20OR%20%22Brandsfield%20strait%22%20OR%20%22Gerlache%20Strait%22%20OR%20%22Marguerite%20Bay%22%20OR%20%22Antarctic%20Sound%22%20OR%20%22CEMP%20site%22%20OR%20%22domain%201%22%20OR%20%22subarea%2048.1%22%20OR%20%22subarea%2048.2%22%20OR%20%22subarea%2088.3%22%20OR%20%22area%2048%22%20OR%20%2248.1%22%20OR%20%2248.2%22%20OR%20%2288.3%22%20OR%20%22small-scale%20management%20unit%22%20OR%20%22ccamlr%22%29%20AND%20%28%22monitoring%22%20OR%20%22survey%22%20OR%20%22inventory%22%20OR%20%22census%22%20OR%20%22study%22%20OR%20%22Time-series%22%20OR%20%22Time%20series%22%20OR%20%22Monitor%22%20OR%20%22data%20collection%22%20OR%20%22Antarctic%20Site%20Inventory%22%20OR%20%22long%20term%22%20OR%20%22trend%22%20OR%20%22change%22%20OR%20%22seasonal%22%20OR%20%22season%22%20OR%20%22years%22%20OR%20%22decadal%22%20OR%20%22decades%22%20OR%20%22interannual%22%20OR%20%22monitored%22%20OR%20%22counts%22%20OR%20%22observation%22%29&id=2iytNJ1sKMwuJATY13C9sj'

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
#addWorksheet(OUT, "OAsearch")

# Write the data to the sheets
writeData(OUT, sheet = "OAsummary", x = oa_summary)
writeData(OUT, sheet = "type", x = dplyr::select(search_outputs_byType, -key))
writeData(OUT, sheet = "sourceType", x = dplyr::select(search_outputs_bySourceType, -key))
writeData(OUT, sheet = "topic", x = dplyr::select(search_outputs_byTopic, -key))
#writeData(OUT, sheet = "OAsearch", x = search_outputs_clean)

# Export the file
saveWorkbook(OUT, paste0("../output/", iteration, "/oa_results.xlsx"),overwrite = TRUE)

### Get data from OpenAlex----
search_outputs <- oa_fetch(
  entity = "works", #oa_entities()
  title_and_abstract.search = st,
  count_only = FALSE,
  verbose = TRUE,
  mailto = oa_email())
# the url only shows the first page

# save output
write_csv(search_outputs, paste0("../output/", iteration, "/oa_search.csv"))

# Clean results

search_outputs_clean = search_outputs %>% 
  # remove GBIF downloads
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
  mutate(concept = sapply(concepts, function(x) {
    # Filter for scores greater than 0.5
    filtered_scores <- x[x$score > 0.5, ]
    # Combine the display names of the filtered authors
    paste(filtered_scores$display_name, collapse = "; ")
  })) %>% 
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
                "type","oa_url","author","concept",
                "topic","subfield","pdf_url",
                "cited_by_count") %>% 
  # add id col
  mutate(id = row_number())

# save output
#write_csv(search_outputs_clean, paste0("../output/", iteration, "/oa_search_clean.csv"))
#search_outputs_clean = read_csv(paste0("../output/", iteration, "/oa_search_clean.csv"))

### Validation----
# Checks against validation dataset (https://www.penguinmap.com/mapppd/sources/)
valid_ref = read_csv("../input/valid/Query060525ref.csv")
#valid_ref = read_csv("../input/valid/all_sources.csv")

valid_ref = valid_ref %>% 
  #filter(!is.na(title)) %>% 
  mutate(validation_set = TRUE) %>% 
  dplyr::mutate(title = tolower(title)) %>%
  dplyr::mutate(title = gsub('  ', '',title)) %>% 
  dplyr::mutate(title = str_trim(title)) %>% 
  dplyr::mutate(title = gsub("[.]$","",title)) %>% 
  dplyr::select(id,type, doi,title,validation_set)
  #dplyr::select(type, doi,title,validation_set)

search_outputs_clean2 = search_outputs_clean %>% 
  mutate(search_set = TRUE) %>% 
  dplyr::mutate(title = tolower(title)) %>%
  dplyr::mutate(title = gsub('  ', '',title)) %>% 
  dplyr::mutate(title = str_trim(title)) %>% 
  dplyr::mutate(title = gsub("[.]$","",title)) %>% 
  tidyr::separate(doi, into = c('extra','doi2'), sep =  '[.]org[/]') %>% 
  dplyr::select(type,doi = doi2, title,search_set, id)

check_completeness_doi = left_join(filter(search_outputs_clean2, !is.na(doi)),
                               valid_ref, 
                               by = 'doi') %>% 
  filter(validation_set == TRUE & search_set == TRUE) %>% # 6
  dplyr::select(doi, title = title.x,validation_set,id = id.x)

check_completeness_title = left_join(filter(search_outputs_clean2, !is.na(title)),
                                   valid_ref, 
                                   by = 'title') %>% 
  filter(validation_set == TRUE & search_set == TRUE) %>% 
  dplyr::select(doi = doi.x, title,validation_set,id=id.x)

check_completeness = rbind(check_completeness_title, check_completeness_doi) %>% 
  distinct(id, .keep_all = TRUE) %>% 
  dplyr::select(id, validation_set)

# join to results

search_outputs_clean = left_join(search_outputs_clean, check_completeness)
write_csv(search_outputs_clean, paste0("../output/", iteration, "/oa_search_clean.csv"))


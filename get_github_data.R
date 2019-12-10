library(tidyverse)
library(jsonlite)
library(httr)
library(lubridate)
# source("R/get_redcap_repo_data.R")

# redcap_repo_data <- scrape_redcap_repo_data(create_csv = T)
redcap_repo_data <- read.csv("redcap_repo_data.csv")

uf_rc_repos <- redcap_repo_data %>%
  filter(institution == 'University of Florida - CTSI') %>%
  distinct(title, .keep_all = T) %>%
  select(title, version, deployed, date_added) %>%
  mutate(tag_url = paste0("https://api.github.com/repos/ctsit/", deployed, "/tags"),
         commits_url = paste0("https://api.github.com/repos/ctsit/", deployed, "/commits")) %>%
  rename("rc_repo_version" = version, "rc_repo_release_date" = date_added)

get_gh_tag_info <- function(tag_url, commit_url, deployed){

  gh_tag_info <- fromJSON(tag_url, flatten = T) %>%
    left_join(fromJSON(commit_url, flatten = T), by = c("commit.sha" = 'sha')) %>%
    mutate(repo = deployed)

  return(gh_tag_info)

}

gh_tag_info <- pmap_df(list(uf_rc_repos$tag_url,uf_rc_repos$commits_url, uf_rc_repos$deployed),
                       get_gh_tag_info)

module_distributuion <- gh_tag_info %>%
  select(repo, name, commit.committer.date) %>%
  mutate(commit.committer.date = ymd(as.Date(commit.committer.date))) %>%
  rename("gh_version" = name, "gh_release_date" = commit.committer.date) %>%
  group_by(repo) %>%
  top_n(gh_release_date, n = 1) %>%
  left_join(uf_rc_repos, by = c("repo" = "deployed")) %>%
  select(repo, gh_version, rc_repo_version, gh_release_date,rc_repo_release_date)
  # filter(gh_version != rc_repo_version)

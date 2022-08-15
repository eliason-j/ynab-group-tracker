
source("ynab-oauth-keys.R")

# packages ----------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(httr)
library(jsonlite)
library(shinycssloaders)

# oauth setup via https://gist.github.com/hadley/144c406871768d0cbe66b0b810160 --------

if (interactive()) {
  options(shiny.port = 8100)
  APP_URL <- "http://localhost:8100/" # testing url
} else {
  APP_URL <- "https://jacobeliason.shinyapps.io/ynab-group-tracker/" # deployed URL
}

app <- oauth_app("ynab-group-tracker",
                 key = Sys.getenv("OAuth_Client_ID"),
                 secret = Sys.getenv("OAuth_Client_Secret"),
                 redirect_uri = APP_URL
)

api <- oauth_endpoint(
  request = NULL,
  base_url = "https://app.youneedabudget.com/oauth",
  authorize = paste0("https://app.youneedabudget.com/oauth/authorize"),
  access = "https://app.youneedabudget.com/oauth/token"
)

scope <- "read-only"

has_auth_code <- function(params) {
  return(!is.null(params$code))
}

# helper functions --------------------------------------------------------

get_budgets <- function(token_local = NULL, 
                        token_server = NULL){
  if(is.null(token_server)){
    df <- httr::GET(
      url = "https://api.youneedabudget.com/v1/budgets", 
      httr::add_headers(Authorization = paste("bearer", token_local))
    )
  } else if (!is.null(token_server)){
    df <- httr::GET(
      url = "https://api.youneedabudget.com/v1/budgets", 
      httr::config(token = token_server)
    )
  } else warning("Error: issue with tokens provided to get_budgets()")
  
  budgets_df <- 
    df |> 
    httr::content(as = "text") |> 
    jsonlite::fromJSON(flatten = TRUE) |> 
    purrr::pluck(1) |> purrr::pluck(1)
  
  return(budgets_df)
}

get_categories <- function(token_local = NULL, 
                           token_server = NULL, 
                           budget_id = NULL,
                           clean = T) {
  
  if(is.null(token_server)){
    df <- httr::GET(
      url = paste0("https://api.youneedabudget.com/v1/budgets/",budget_id,"/categories"),
      httr::add_headers(Authorization = paste("bearer", token_local))
    )
  } else if (!is.null(token_server)){
    df <- httr::GET(
      url = paste0("https://api.youneedabudget.com/v1/budgets/default/categories"),
      httr::config(token = token_server)
    )
  } else warning("Error: issue with tokens provided to get_categories()")
  
  categories_df <- 
    df |> 
    httr::content(as = "text") |> 
    jsonlite::fromJSON(flatten = TRUE) |> 
    purrr::pluck(1) |> purrr::pluck(1)
  
  if(clean) {
    categories_df_clean <- 
      categories_df |> 
      dplyr::filter(name != "Hidden Categories",
                    hidden == F) |> 
      tidyr::unnest(categories, names_sep = "_") |> 
      plyr::mutate(
        budgeted = categories_budgeted/1000,
        activity = categories_activity/1000,
        balance = categories_balance/1000,
        goal_target = categories_goal_target/1000
      )
    return(categories_df_clean)
  } else {
    return(categories_df)
  }
}

get_category_groups <- function(token_local = NULL, 
                                token_server = NULL, 
                                budget_id = NULL,
                                clean = T) {
  
  if(is.null(token_server)){
    df <- httr::GET(
      url = paste0("https://api.youneedabudget.com/v1/budgets/",budget_id,"/categories"),
      httr::add_headers(Authorization = paste("bearer", token_local))
    ) 
  } else if (!is.null(token_server)){
    df <- httr::GET(
      url = paste0("https://api.youneedabudget.com/v1/budgets/default/categories"),
      httr::config(token = token_server)
    ) 
  } else warning("Error: issue with tokens provided to get_category_groups()")
  
  category_groups_df <- 
    df|> 
    httr::content(as = "text") |> 
    jsonlite::fromJSON(flatten = TRUE) |> 
    purrr::pluck(1) |> purrr::pluck(1) |> 
    dplyr::select(-categories) |> 
    tibble::as_tibble()
  
  return(category_groups_df)
}

get_transactions <- function(
    token_local = NULL, 
    token_server = NULL, 
    budget_id = NULL
) {
  
  if(is.null(token_server)){
    df <- httr::GET(
      url = paste0("https://api.youneedabudget.com/v1/budgets/",budget_id,"/transactions"),
      httr::add_headers(Authorization = paste("bearer", token_local))
    ) 
  } else if (!is.null(token_server)){
    df <- httr::GET(
      url = paste0("https://api.youneedabudget.com/v1/budgets/default/transactions"),
      httr::config(token = token_server)
    ) 
  } else warning("Error: issue with tokens provided to get_category_groups()")
  
  transactions_df <- 
    df |> 
    httr::content(as = "parsed") |> 
    pluck(1) |> pluck(1)
  
  transactions_df <- do.call(rbind, transactions_df) |> as.data.frame()
  
  subtransactions_df <- matrix(ncol = 11, nrow = 0)
  for(i in 1:nrow(transactions_df)) {
    item <- transactions_df$subtransactions[[i]]
    out <- NA
    names_extracted <- F
    if(!is_empty(item)){
      for(j in 1:length(item)){
        for(k in 1:ncol(subtransactions_df)) {
          out[k] <- ifelse(is.null(item[[j]][[k]]), NA, item[[j]][[k]])
          if(!names_extracted){
            names_out <- names(item[[1]])
            names_extracted <- T
          }
        }
        subtransactions_df <- rbind(subtransactions_df, out)
        out <- NA
      }
    }
  }
  
  subtransactions_df <- as_tibble(subtransactions_df)
  colnames(subtransactions_df) <- names_out
  
  transactions_df <- 
    bind_rows(
      sapply(transactions_df |> select(-subtransactions), as.character) |> 
        as.data.frame() |> 
        as_tibble(),
      subtransactions_df |> 
        mutate(split = T) |> 
        left_join(
          sapply(transactions_df |> select(id, date), as.character) |> 
            as_tibble(), 
          by = c("transaction_id" = "id")
        )
    ) |> 
    dplyr::select(
      date,
      amount,
      payee_name,
      category_name,
      category_id
    ) |> 
    plyr::mutate(
      amount = as.numeric(amount)/1000,
      posted_date = lubridate::date(date)
    )
  
  return(transactions_df)
}

format_month <- function(...) {
  map_chr(..., function(x) if(str_length(x)==1){str_glue("0",x)} else if(str_length(x)==2){str_glue(x)} else warning("Error: input must be 1 or 2 characters in length"))
}

check_rate_limit <- function(token = NULL){
  df <- httr::GET(
    url = "https://api.youneedabudget.com/v1/budgets", 
    httr::add_headers(Authorization = paste("bearer", token))
  )
  return(df$headers$`x-rate-limit`)
}

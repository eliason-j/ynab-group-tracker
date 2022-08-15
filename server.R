
function(input, output, session) {
  
  # oauth -------------------------------------------------------------------
  
  params <- parseQueryString(isolate(session$clientData$url_search))
  if (!has_auth_code(params)) {
    return()
  }
  
  oauth_token <- oauth2.0_token( # manually create a token
    app = app,
    endpoint = api,
    credentials = oauth2.0_access_token(api, app, params$code),
    cache = FALSE
  )
  
  resp <- httr::GET("https://app.youneedabudget.com/oauth/authorize", config(token = oauth_token))
  
  output$code <- renderText(content(resp, "text"))
  
  # initialize data ---------------------------------------------------------
  
  budgets_import <- get_budgets(token_server = oauth_token, token_local = NULL)
  categories_import <- get_categories(token_server = oauth_token, token_local = NULL)
  transactions_import <- get_transactions(token_server = oauth_token, token_local = NULL)
  
  include_categories <- c( # for my personal use; won't matter for other users
    "Beauty, Hygiene, Healthcare","Gifts, Birthdays, Holidays","Dining Out",
    "Groceries, Snacks, Drinks","Going Out (Concerts, Movie Theater, Outings)",
    "Parties & Hosting","Joint Discretionary"
  )
  
  # shiny -------------------------------------------------------------------
  
  output$category_values = renderUI({
    categories_string_display <- categories_import$categories_name
    names(categories_string_display) <- str_c(categories_import$name, " - ", categories_import$categories_name)
    categories_string_display <- categories_string_display[!is.na(categories_string_display)]
    
    checkboxGroupInput('category_values', 'Select budget categories to include:', categories_string_display,
                       selected = include_categories)
  })
  
  output$year_values = renderUI({
    user_years <- 
      transactions_import$date |> 
      lubridate::as_date() |> 
      lubridate::year() |> 
      unique() |> 
      as.integer()
    selectInput('year_values', "Select year to inspect:", min(user_years):max(user_years), 
                selected = max(user_years))
  })
  
  output$transactionsPrint <- renderTable({
    
    # transactions_import |>
    #   # select(-posted_date) |>
    #   mutate(date = lubridate::as_date(date)) |>
    #   dplyr::filter(
    #     lubridate::year(posted_date) %in% input$year_values,
    #     lubridate::month(posted_date) %in% input$month
    #   )
    
    transactions_import |> 
      select(-posted_date) |>
      mutate(posted_date = lubridate::as_date(date)) |>
      dplyr::filter(
        category_name %in% input$category_values,
        lubridate::year(posted_date) %in% input$year_values,
        lubridate::month(posted_date) %in% input$month
      )
    
  })
  
  output$plot_lines <- renderPlot({
    
    target_amounts <-
      categories_import |>
      dplyr::filter(categories_name %in% input$category_values) |> # INPUT
      select(categories_id, categories_name, goal_target)
    
    target_total <- sum(target_amounts$goal_target)
    
    current_year <- as.integer(input$year_values) # INPUT
    current_month <- as.integer(input$month) # INPUT
    current_days_in_month <- lubridate::days_in_month(current_month)
    
    transactions <-
      transactions_import |>
      select(-posted_date) |>
      mutate(posted_date = lubridate::as_date(date)) |>
      dplyr::filter(
        category_name %in% target_amounts$categories_name,
        lubridate::year(posted_date) %in% current_year,
        lubridate::month(posted_date) %in% current_month
      )
    
    latest_transaction_date <- max(transactions$posted_date, na.rm = T)
    
    day_seq <-
      lubridate::as_date(
        paste0(
          current_year,"-",
          format_month(current_month),"-",
          format_month(1:current_days_in_month)
        )
      )
    uniform_spending <-
      data.frame(
        spend_uniform = seq(0,target_total,length.out=length(day_seq)+1)[-1],
        d = day_seq
      )
    
    progress <-
      transactions |>
      group_by(
        d = lubridate::date(posted_date)
      ) |>
      summarize(
        spend_daily = sum(-amount)
      ) |>
      ungroup() |>
      mutate(
        spend_cumulative = cumsum(spend_daily)
      ) |>
      mutate(
        max_day = d == latest_transaction_date
      )
    
    progress <-
      progress |>
      full_join(uniform_spending, by = "d") |> 
      arrange(d) |>
      fill(spend_cumulative, .direction = c("down")) |> 
      mutate(
        spend_cumulative =
          ifelse(d > progress$d[progress$max_day], NA, spend_cumulative)
      )
    progress$spend_cumulative[progress$d > progress$d[progress$max_day]] <- NA
    
    current_spending_total <- transactions |> 
      summarise(sum = sum(-amount)) |> 
      pull(sum)
    current_spending_uniform <- 
      uniform_spending |> 
      filter(d == latest_transaction_date) |> 
      pull(spend_uniform)
    
    spend_diff <- current_spending_total - current_spending_uniform
    overspend <- spend_diff > 0
    
    line_color <- ifelse(overspend, "red", "green")
    colors <- c("darkgray", line_color)
    
    # re printing ggplot: https://stackoverflow.com/questions/58448118/warning-jsonlite-in-shiny-input-to-asjsonkeep-vec-names-true-is-a-named-vecto
    
    print(
      progress |> 
        ggplot() + 
        geom_line(
          aes(x = d, y = spend_uniform),
          linetype = "dashed",
          color = "darkgray",
          size = 3/2
        ) +
        geom_line(
          aes(x = d, y = spend_cumulative),
          color = line_color,
          size = 3/2
        ) +
        geom_point(
          data = progress |> 
            dplyr::filter(max_day) |> 
            pivot_longer(cols = c(spend_cumulative, spend_uniform)) |> 
            mutate(actual = name == "spend_cumulative"),
          aes(x = d, y = value, color = actual),
          size = 3
        ) +
        scale_color_manual(values = colors, guide = "none") +
        scale_x_date(name = NULL) +
        scale_y_continuous(
          name = NULL,
          labels=scales::dollar_format()
        ) +
        # ggtitle(
        #   paste0("Spending progress through ", latest_transaction_date),
        #   paste0("Status: ", ifelse(overspend, "over", "under"), "budget by $", 
        #          round(abs(spend_diff), 0)
        #   )
        # ) +
        # labs(
        #   caption = paste0("Updated ", lubridate::today())
        # ) +
        theme_minimal() +
        theme(
          axis.text = element_text(size = 16)
        )
    )
  })
  
  
  # plot_lines_title --------------------------------------------------------
  output$plot_lines_title <- renderText({
    t_dates <-
      transactions_import |>
      select(-posted_date) |>
      mutate(posted_date = lubridate::as_date(date)) |>
      dplyr::filter(
        category_name %in% input$category_values,
        lubridate::year(posted_date) %in% input$year_values,
        lubridate::month(posted_date) %in% input$month
      ) |> 
      pull(posted_date)
    latest_transaction_date <- max(t_dates, na.rm = T)
    
    paste0("Spending progress through ", latest_transaction_date)
  })
  
  
  # plot_lines_subtitle -----------------------------------------------------
  output$plot_lines_subtitle <- renderText({
    target_amounts <-
      categories_import |>
      dplyr::filter(categories_name %in% input$category_values) |> # INPUT
      select(categories_id, categories_name, goal_target)
    
    target_total <- sum(target_amounts$goal_target)
    
    current_year <- as.integer(input$year_values) # INPUT
    current_month <- as.integer(input$month) # INPUT
    current_days_in_month <- lubridate::days_in_month(current_month)
    
    transactions <-
      transactions_import |>
      select(-posted_date) |>
      mutate(posted_date = lubridate::as_date(date)) |>
      dplyr::filter(
        category_name %in% target_amounts$categories_name,
        lubridate::year(posted_date) %in% current_year,
        lubridate::month(posted_date) %in% current_month
      )
    latest_transaction_date <- max(transactions$posted_date, na.rm = T)
    
    day_seq <-
      lubridate::as_date(
        paste0(
          current_year,"-",
          format_month(current_month),"-",
          format_month(1:current_days_in_month)
        )
      )
    uniform_spending <-
      data.frame(
        spend_uniform = seq(0,target_total,length.out=length(day_seq)+1)[-1],
        d = day_seq
      )
    
    
    current_spending_total <- transactions |> 
      summarise(sum = sum(-amount)) |> 
      pull(sum)
    current_spending_uniform <- 
      uniform_spending |> 
      filter(d == latest_transaction_date) |> 
      pull(spend_uniform)
    
    spend_diff <- current_spending_total - current_spending_uniform
    overspend <- spend_diff > 0
    
    paste0("Status: ", ifelse(overspend, "over", "under"), "budget by $", 
           round(abs(spend_diff), 0))
  })
  
  
  # plot_bars ---------------------------------------------------------------
  output$plot_bars <- renderPlot({
    
    target_amounts <-
      categories_import |>
      dplyr::filter(categories_name %in% input$category_values) |> # INPUT
      select(categories_id, categories_name, goal_target)
    
    target_total <- sum(target_amounts$goal_target)
    
    current_year <- as.integer(input$year_values) # INPUT
    current_month <- as.integer(input$month) # INPUT
    current_days_in_month <- lubridate::days_in_month(current_month)
    
    transactions <-
      transactions_import |>
      select(-posted_date) |>
      mutate(posted_date = lubridate::as_date(date)) |>
      dplyr::filter(
        category_name %in% target_amounts$categories_name,
        lubridate::year(posted_date) %in% current_year,
        lubridate::month(posted_date) %in% current_month
      )
    
    latest_transaction_date <- max(transactions$posted_date, na.rm = T)
    
    category_spend_progress <-
      transactions |>
      group_by(category_name) |>
      summarize(
        spend_month = sum(-amount)
      ) |>
      full_join(
        target_amounts |>
          select(-categories_id), by = c("category_name" = "categories_name")
      ) |>
      mutate(
        spend_month = ifelse(is.na(spend_month), 0, spend_month),
        spend_uniform_month = goal_target *
          (lubridate::day(latest_transaction_date) /
             current_days_in_month),
        diff_perc = (spend_month / goal_target) * 100,
        diff_abs = spend_month - goal_target
      ) |>
      arrange(desc(goal_target))
    
    plot_limits <- category_spend_progress |>
      pivot_longer(cols = c(spend_month, goal_target)) |> pull(value) |> {\(x) c(min(x),max(x))}()
    plot_limits[1] <- ifelse(plot_limits[1]>=0,0,plot_limits[1])
    label_bump <- plot_limits[2]/25
    
    # re printing ggplot
    # https://stackoverflow.com/questions/58448118/warning-jsonlite-in-shiny-input-to-asjsonkeep-vec-names-true-is-a-named-vecto
    print(category_spend_progress |>
            mutate(category_name = fct_relevel(category_name, rev(category_spend_progress$category_name))) |>
            ggplot() +
            geom_bar(
              aes(x = category_name, y = goal_target),
              stat = "identity",
              alpha = 0.9,
              width = .75
            ) +
            geom_bar(
              data =
                category_spend_progress |>
                mutate(category_name = fct_relevel(category_name, rev(category_spend_progress$category_name))) |>
                pivot_longer(cols = c(spend_month, spend_uniform_month)),
              aes(x = category_name, y = value, fill = name),
              stat = "identity",
              width = .75,
              alpha = .9,
              position = position_dodge(width = 0.75)
            ) +
            geom_label(
              data =
                category_spend_progress |>
                mutate(category_name = fct_relevel(category_name, rev(category_spend_progress$category_name))) |>
                pivot_longer(cols = c(spend_month, spend_uniform_month)),
              aes(x = category_name, 
                  y = ifelse(value>0, value + label_bump, value - label_bump), 
                  fill = name,
                  label = paste0("$",round(value, 0))),
              stat = "identity",
              size = 5/14*12,
              colour = "white",
              fontface = "bold",
              label.size = NA,
              position = position_dodge(width = 0.75)
            ) +
            coord_flip() +
            scale_x_discrete(name = NULL) +
            scale_y_continuous(name = NULL,
                               labels=scales::dollar_format(),
                               limits = c(plot_limits[1]*1.125-label_bump, plot_limits[2]*1.1)) +
            scale_fill_manual(values =  c("#e6a57a", "#7abbe6"), guide = "none") +
            scale_color_manual(values = c("#e6a57a", "#7abbe6"), guide = "none") +
            theme_minimal() +
            theme(
              axis.text = element_text(size = 12)
            )
    )
    
  })
  
  
  # plot_bars_title ---------------------------------------------------------
  output$plot_bars_title <- renderText({
    transactions <-
      transactions_import |>
      select(-posted_date) |>
      mutate(posted_date = lubridate::as_date(date)) |>
      dplyr::filter(
        category_name %in% input$category_values,
        lubridate::year(posted_date) %in% input$year_values,
        lubridate::month(posted_date) %in% input$month
      )
    latest_transaction_date <- max(transactions$posted_date, na.rm = T)
    paste0(month.name[as.integer(input$month)]," progress for select categories through ",
           latest_transaction_date)
  })
  
  # plot_bars_subtitle ------------------------------------------------------
  
  output$plot_bars_subtitle <- renderText({
    paste0("Actual (orange) and on-target (blue) spending")
  })
  
} 


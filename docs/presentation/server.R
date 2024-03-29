# server function ####
function(input, output, session) {
  # Data ####
  d <- reactive({
    analysis %>%
      # No idea why ifelse and if_else don't work
      dplyr::filter(
        if (input$group %in% subfields) {
          .data$Q3_recoded == input$group
        } else if (input$group %in% types) {
          .data$Q4_quantqual == input$group
        } else {
          TRUE
        }
      )
  }) %>%
    bindCache(input$group) %>%
    bindEvent(input$group)


  # Q5 plots ####
  output$q5 <- renderPlotly({
    fig1 <- d() %>%
      count(Q5_familiarity) %>%
      plot_ly(
        x = ~Q5_familiarity,
        y = ~n,
        type = "bar",
        # https://community.plotly.com/t/33731
        hovertemplate = "<b>%{x}</b>\n%{y} people<extra></extra>"
      )

    neg <- d() %>%
      select(all_of(q6_terms_neg)) %>%
      pivot_longer(cols = everything()) %>%
      filter(value != 0) %>%
      group_by(name) %>%
      summarise(n = n()) %>%
      mutate(name = str_wrap(name, width = width))

    fig2 <- d() %>%
      select(all_of(q6_terms_pos)) %>%
      pivot_longer(cols = everything()) %>%
      filter(value != 0) %>%
      group_by(name) %>%
      summarise(n = n()) %>%
      mutate(name = str_wrap(name, width = width)) %>%
      plot_ly(
        x = ~name,
        y = ~n,
        type = "bar",
        name = "Replicability",
        # https://plotly.com/r/bar-charts/#:~:text=I(%22black%22)
        color = I("#2ca02c"),
        hovertemplate = "<b>%{x}</b>\n%{y} people<extra></extra>"
      ) %>%
      add_trace(
        x = ~ neg$name,
        y = ~ neg$n,
        type = "bar",
        name = "Reproducibility",
        color = I("#ff7f0e"),
        hovertemplate = "<b>%{x}</b>\n%{y} people<extra></extra>"
      ) %>%
      layout(xaxis = list(
        categoryorder = "array",
        categoryarray = q6_plot_order
      ))


    fig3 <- d() %>%
      select(all_of(q6_motive)) %>%
      pivot_longer(cols = everything()) %>%
      filter(value != 0) %>%
      group_by(name) %>%
      summarise(n = n()) %>%
      mutate(name = str_wrap(name, width = width)) %>%
      plot_ly(
        x = ~name,
        y = ~n,
        type = "bar",
        # name = "Familiar",
        hovertemplate = "<b>%{x}</b>\n%{y} people<extra></extra>"
      )

    plotly::subplot(fig1, fig2, fig3, nrows = 1, shareY = TRUE) %>%
      plt_layout(showlegend = FALSE) %>%
      plt_config(
        filename = paste0(
          "repli_awareness_",
          input$group %>%
            tolower() %>%
            gsub("[^a-z0-9]", "_", .)
        )
      )
  }) %>%
    bindCache(input$group)


  # Q7 likert ####
  output$q7 <- renderPlotly({
    gen$q7[[input$group]] %>%
      plt_likert(
        q7_text, pal, width_long,
        paste0(
          "repli_views_",
          input$group %>%
            tolower() %>%
            gsub("[^a-z0-9]", "_", .)
        )
      )
  }) %>%
    bindCache(input$group)


  # Q12 percentages ####
  output$q12 <- renderPlotly({
    q12 <- d()$Q12_pcnt_have_rep_1 %>%
      density(na.rm = TRUE)

    q13 <- d()$Q13_pcnt_could_rep_1 %>%
      density(na.rm = TRUE)

    q14 <- d()$Q14_pcnt_should_rep_1 %>%
      density(na.rm = TRUE)

    plot_ly(
      x = ~ q12$x,
      y = ~ q12$y,
      type = "scatter",
      mode = "lines",
      fill = "tozeroy",
      name = "Have been",
      hovertemplate = "%{x:.2f}%\n%{y:.2%}"
    ) %>%
      add_trace(
        x = ~ q13$x,
        y = ~ q13$y,
        type = "scatter",
        mode = "lines",
        fill = "tozeroy",
        name = "Could be",
        line = list(dash = "dash")
      ) %>%
      add_trace(
        x = ~ q14$x,
        y = ~ q14$y,
        type = "scatter",
        mode = "lines",
        fill = "tozeroy",
        name = "Should be",
        line = list(dash = "dot")
      ) %>%
      plt_layout(
        legend = list(font = fira)
      ) %>%
      plotly::layout(
        xaxis = list(range = list(0, 100), ticksuffix = "%"),
        yaxis = list(tickformat = ".1%"),
        hovermode = "x"
      ) %>%
      plt_config(
        filename = paste0(
          "repli_published_",
          input$group %>%
            tolower() %>%
            gsub("[^a-z0-9]", "_", .)
        )
      )
  }) %>%
    bindCache(input$group)


  # Q17 practices ####
  output$q17 <- renderPlotly({
    d() %>%
      mutate(
        across(starts_with("Q17"), fct_rev)
      ) %>%
      plt_bar(
        "Q17_", q17_text, pal4, width_long,
        paste0(
          "repli_practices_",
          input$group %>%
            tolower() %>%
            gsub("[^a-z0-9]", "_", .)
        )
      )
  }) %>%
    bindCache(input$group)


  # Q15 barriers ####
  output$q15 <- renderPlotly({
    d() %>%
      plt_bar(
        "Q15", q15_text, pal7, width_long,
        paste0(
          "repli_barriers_",
          input$group %>%
            tolower() %>%
            gsub("[^a-z0-9]", "_", .)
        )
      )
  }) %>%
    bindCache(input$group)


  # Q8 characteristics of study ####
  output$q8 <- renderPlotly({
    gen$q8[[input$group]] %>%
      plt_likert(
        q8_text, pal, width_long,
        paste0(
          "repli_study_factors_",
          input$group %>%
            tolower() %>%
            gsub("[^a-z0-9]", "_", .)
        )
      )
  }) %>%
    bindCache(input$group)

  # Q10 phenomenon ####
  output$q10 <- renderPlotly({
    gen$q10[[input$group]] %>%
      plt_likert(
        q10_text, pal, width_long,
        paste0(
          "repli_phenomenon_",
          input$group %>%
            tolower() %>%
            gsub("[^a-z0-9]", "_", .)
        )
      )
  }) %>%
    bindCache(input$group)


  # Q6 & Q10 word clouds ####
  output$cloud_q6 <- renderWordcloud2(gen$q6[[input$group]])
  output$cloud_q19 <- renderWordcloud2(gen$q19[[input$group]])
}

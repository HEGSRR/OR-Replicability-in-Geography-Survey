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

    fig2 <- plot_ly(
      type = "bar"
    )

    fig3 <- plot_ly(
      type = "bar"
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
    plt <- q7[[input$group]] %>%
      # ggplot
      ggplot() +
      geom_segment(aes(
        x = fct_rev(name),
        y = start,
        xend = name, yend = start + perc,
        colour = value,
        text = paste0(
          "<b>", value, "</b><br>",
          n, " people<br>",
          round(perc * 100, 2), " %"
        )
      ), linewidth = 12) +
      geom_hline(yintercept = 0, color = c("#646464")) +
      coord_flip() +
      scale_color_manual("Response", values = pal, guide = "legend") +
      labs(title = "", y = "Percent", x = "") +
      scale_x_discrete(
        labels = str_wrap(rev(q7_text), width = 40)
      ) +
      scale_y_continuous(labels = scales::percent) +
      theme_minimal() +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()
      )
    # plotly
    ggplotly(plt, tooltip = "text") %>%
      plt_layout(
        legend = list(font = fira)
      ) %>%
      plt_config(
        filename = paste0(
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
        name = "Could be"
      ) %>%
      add_trace(
        x = ~ q14$x,
        y = ~ q14$y,
        type = "scatter",
        mode = "lines",
        fill = "tozeroy",
        name = "Should be"
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
    plt <- d() %>%
      pivot_longer(starts_with("Q17_"), values_to = "Response") %>%
      group_by(name) %>%
      mutate(
        Response = fct_na_value_to_level(fct_rev(Response), "No response")
      ) %>%
      dplyr::count(Response) %>%
      mutate(perc = n / sum(n)) %>%
      ggplot(aes(
        x = fct_rev(name),
        y = n,
        fill = Response,
        text = paste0(
          "<b>", Response, "</b><br>",
          n, " people<br>",
          round(perc * 100, 2), " %"
        )
      )) +
      geom_col(
        position = position_fill(reverse = TRUE),
        width = 0.8
      ) +
      coord_flip() +
      scale_fill_manual(
        values = c("#EF5645", "#D9D9D9", "#F2F2F2", "#7F7F7F")
      ) +
      scale_x_discrete(
        expand = c(0, 0),
        labels = str_wrap(rev(q17_text), width = 40)
      ) +
      scale_y_continuous(labels = scales::percent) +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        text = element_text(family = "Fira Sans"),
        legend.position = "bottom",
        legend.title = element_blank()
      )
    # plotly
    ggplotly(plt, tooltip = "text") %>%
      plt_layout(
        legend = list(font = fira)
      ) %>%
      plt_config(
        filename = paste0(
          "repli_practices_",
          input$group %>%
            tolower() %>%
            gsub("[^a-z0-9]", "_", .)
        )
      )
  })

  # Q15 barriers ####
  output$q15 <- renderPlotly({
    plt <- d() %>%
      pivot_longer(starts_with("Q15"), values_to = "Response") %>%
      group_by(name) %>%
      mutate(
        Response = fct_na_value_to_level(Response, "No response"),
      ) %>%
      dplyr::count(Response) %>%
      mutate(perc = n / sum(n)) %>%
      ggplot(aes(
        x = fct_rev(name),
        y = n,
        fill = Response,
        text = paste0(
          "<b>", Response, "</b><br>",
          n, " people<br>",
          round(perc * 100, 2), " %"
        )
      )) +
      geom_col(
        position = position_fill(reverse = TRUE),
        width = 0.8
      ) +
      coord_flip() +
      scale_fill_manual(
        values = c("#CD2311", "#EF5645", "#EE947D", "#D9D9D9", "#F2F2F2", "#7F7F7F", "#7F7F7F")
      ) +
      scale_x_discrete(
        expand = c(0, 0),
        labels = str_wrap(rev(q15_text), width = 40)
      ) +
      scale_y_continuous(labels = scales::percent) +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        text = element_text(family = "Fira Sans"),
        legend.position = "bottom",
        legend.title = element_blank()
      )
    # plotly
    ggplotly(plt, tooltip = "text") %>%
      plt_layout(
        legend = list(font = fira)
      ) %>%
      plt_config(
        filename = paste0(
          "repli_barriers_",
          input$group %>%
            tolower() %>%
            gsub("[^a-z0-9]", "_", .)
        )
      )
  })

  # Q6 & Q10 word clouds ####
  output$cloud_q6 <- renderWordcloud2(clouds_q6[[input$group]])
  output$cloud_q19 <- renderWordcloud2(clouds_q19[[input$group]])
}

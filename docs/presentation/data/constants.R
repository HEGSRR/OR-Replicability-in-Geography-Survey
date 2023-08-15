library(shiny)
library(plotly)

# Group definitions ####
subfields <- list("Human", "Methods", "Nature/society", "Physical")
types <- list("Quantitative", "Qualitative", "Mixed Methods")
geo_groups <- list(
  "Everyone",
  "Subfield" = subfields,
  "Type" = types
)

# Columns to cast as factors ####
four_levels <- c("Not at all", "Very little", "Somewhat", "To a great extent")
four_level_cols <- c("Q5_familiarity")

# Q7 Levels of agreement ####
agree_levels <- c("Strongly disagree", "Disagree", "Don't know", "Agree", "Strongly agree")

# Q7 questions text ####
q7_text <- c(
  Q7_value_1 = "Claim is the product of chance",
  Q7_value_2 = "Result is the product of a flawed research design",
  Q7_value_3 = "Observations and analyses reflect the concepts they are intended to represent",
  Q7_value_4 = "Claim will hold in other populations (e.g., different age groups)",
  Q7_value_5 = "Claim will hold in other locations"
)

# Q8 questions text ####
q8_text <- c(
  Q8_study_factors_1 = "Multiple hypotheses were tested",
  Q8_study_factors_2 = "Quantitative methods were used",
  Q8_study_factors_3 = "Qualitative methods were used",
  Q8_study_factors_4 = "Mixed methods were used",
  Q8_study_factors_5 = "Poor documentation of study methods",
  Q8_study_factors_6 = "Restricted access data were used",
  Q8_study_factors_7 = "Data were gathered from multiple study sites",
  Q8_study_factors_8 = "A large research team conducted the study",
  Q8_study_factors_9 = "Relied on expertise unique to the researcher",
  Q8_study_factors_10 = "Relied on the unique position of the researcher"
)

# Q10 questions text ####
q10_text <- c(
  Q10_study_factors_1 = "Is spatially dependent upon itself",
  Q10_study_factors_2 = "Is strongly related with local conditions",
  Q10_study_factors_3 = "Exhibits variation across locations",
  Q10_study_factors_4 = "Cannot be directly measured",
  Q10_study_factors_5 = "Cannot be directly manipulated",
  Q10_study_factors_6 = "Has multiple competing theoretical explanations"
)

# Q15 questions text ####
q15_text <- c(
  Q15_decision_factors_1 = "Pressure to publish original research",
  Q15_decision_factors_2 = "Low value of replication studies",
  Q15_decision_factors_3 = "Low chances of replicating a result",
  Q15_decision_factors_4 = "Lack of experience conducting replications",
  Q15_decision_factors_5 = "Difficulty publishing peer-reviewed replications",
  Q15_decision_factors_6 = "Difficulty accessing/creating relevant data",
  Q15_decision_factors_7 = "Insufficient information about original methods",
  Q15_decision_factors_8 = "Difficulty recreating similar procedures",
  Q15_decision_factors_9 = "Lack of funding for replication studies",
  Q15_decision_factors_10 = "Fabrication of data or results by original authors",
  Q15_decision_factors_11 = "Ethical concerns",
  Q15_decision_factors_12 = "Known spatial variation in phenomena being studied"
)

# Q17 questions text ####
q17_text <- c(
  Q17_rep_behavior_1 = "Thought about the replicability of your research",
  Q17_rep_behavior_2 = "Spoke with colleagues about replicability",
  Q17_rep_behavior_3 = "Questioned the replicability of published research",
  Q17_rep_behavior_4 = "Considered replicability while peer reviewing a research proposal or publication",
  Q17_rep_behavior_5 = "Attempted to replicate prior research claims"
)

# Questions for the word cloud ####
cloud_cols <- list("Q6_definition", "Q19_reason_for_rep")

# Function for subplot ####
# Modified from original by Dan Powers <danielp@takomaparkmd.gov> under MIT License
# https://github.com/dpowerstp/plotlywrappers/blob/76d6bb1d5890c81b4fef00472bc1064926f6aa53/R/subplot_title.R
subplot_title <- function(plot, title, .x = 0.05, .y = 1.1) {
  plot %>%
    plotly::add_annotations(
      text = title,
      x = .x,
      y = .y,
      yref = "paper",
      xref = "paper",
      xanchor = "left",
      yanchor = "top",
      showarrow = FALSE,
      font = list(family = "Fira Sans", size = 24)
    )
}

# Plotly styling ####
fira <- list(family = "Fira Sans", size = 16)

plt_layout <- function(plt, ...) {
  plotly::layout(
    plt,
    xaxis = list(title = FALSE, titlefont = fira, tickfont = fira),
    yaxis = list(title = FALSE, titlefont = fira, tickfont = fira),
    font = fira,
    hoverlabel = list(
      font = fira
    ),
    ...
  )
}

# Plotly config ####
# # https://github.com/plotly/plotly.js/blob/master/src/plot_api/plot_config.js
plt_config <- function(plt, filename, ...) {
  plotly::config(
    plt,
    displaylogo = FALSE,
    showTips = FALSE,
    toImageButtonOptions = list(
      format = "png",
      filename = filename,
      # height = 500,
      # width = 700,
      scale = 4
    ),
    ...
  )
}


# Function for bar plots
plt_bar <- function(data, prefix, names, pal, filename) {
  plt <- data %>%
    pivot_longer(starts_with(prefix), values_to = "Response") %>%
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
      values = pal
    ) +
    scale_x_discrete(
      expand = c(0, 0),
      labels = str_wrap(rev(names), width = 40)
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
      filename = filename
    )
}

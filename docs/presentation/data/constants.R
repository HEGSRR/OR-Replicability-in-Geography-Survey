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

# Q17 questions text ####
q17_text <- c(
  Q17_rep_behavior_1 = "Thought about the replicability of your research",
  Q17_rep_behavior_2 = "Spoke with colleagues about replicability",
  Q17_rep_behavior_3 = "Questioned the replicability of published research",
  Q17_rep_behavior_4 = "Considered replicability while peer reviewing a research proposal or publication",
  Q17_rep_behavior_5 = "Attempted to replicate prior research claims"
)

# Q15 questions text ####
q15_text <- c(
  Q15_1 = "Pressure to publish original research",
  Q15_2 = "Low value of replication studies",
  Q15_3 = "Low chances of replicating a result",
  Q15_4 = "Lack of experience conducting replications",
  Q15_5 = "Difficulty publishing peer-reviewed replications",
  Q15_6 = "Difficulty accessing/creating relevant data",
  Q15_7 = "Insufficient information about original methods",
  Q15_8 = "Difficulty recreating similar procedures",
  Q15_9 = "Lack of funding for replication studies",
  Q15_10 = "Fabrication of data or results by original authors",
  Q15_11 = "Ethical concerns",
  Q15_12 = "Known spatial variation in phenomena being studied"
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

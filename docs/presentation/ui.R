# UI tags ####
tagList(
  # head
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$link(rel = "shortcut icon", href = "favicon.png"),
  ),
  navbarPage(
    title = "Replicability in Geographic Research",

    # customization
    id = "activeTab",
    theme = bslib::bs_theme(
      fg = "#212529", primary = "#1D5B79", secondary = "#C5DFF8",
      success = "#8EAC50", info = "#468B97", warning = "#F1C93B",
      base_font = bslib::font_google("Fira Sans"),
      code_font = bslib::font_google("Fira Code"),
      bootswatch = "flatly", bg = "#FFFFFF"
    ),
    windowTitle = "Replicability in Geographic Research",
    # Access with `Shiny.shinyapp.$inputValues`
    header = # conditionalPanel(
    # condition = "input.activeTab !== 'Word Cloud'",
      selectInput(
        "group", h5("Choose a group:"),
        geo_groups,
      ),
    # ),

    # About ####
    tabPanel(
      "About",
      icon = icon("info"),
      fluidRow(
        column(
          12,
          # col-sm-12
          class = "col-lg-10 col-xl-8 col-xxl-6",
          style = "margin-left: 3vw;",
          markdown(about),
        ),
      ),
    ),

    # Awareness ####
    tabPanel(
      "Awareness",
      icon = icon("bullhorn"),
      tabsetPanel(
        id = "awareness-panel",
        tabPanel(
          "Concept",
          fluidRow(
            column(
              4,
              h4(
                "How ", strong("familiar"),
                "are you with the term “replicability”?"
              ),
            ),
            column(
              4,
              h4(
                "You ", strong("understand"),
                "“replicability” in terms of..."
              ),
            ),
            column(
              4,
              h4(
                "What is the main", strong("motivation"),
                "of “replicability”?"
              ),
            ),
          ),
          plotlyOutput("q5", height = plot_height) %>% spin(),
        ),
        tabPanel(
          "Definition",
          h4(
            "What is your ", strong("understanding"),
            "of the term “replicability”",
            "in the context of your own research?",
          ),
          wordcloud2Output(
            "cloud_q6",
            height = plot_height
          ),
        ),
        tabPanel(
          "Motivation",
          h4(
            "Thinking about the replication(s)",
            "you attempted in the last 2 years,",
            "why did you decide to ", strong("attempt"),
            "the replication(s)?",
          ),
          wordcloud2Output(
            "cloud_q19",
            height = plot_height
          ),
        ),
      ),
    ),

    # Factors ####
    tabPanel(
      "Factors",
      icon = icon("lightbulb"),
      tabsetPanel(
        id = "factors_panel",
        tabPanel(
          "The study",
          fluidRow(
            column(
              12,
              # col-sm-12
              class = "col-xxl-10",
              h4(
                "How do each of the following characteristics of a",
                strong("study"), "published in your sub-field",
                "affect the chances of replicating that study?"
              ),
              plotlyOutput("q8", height = plot_full_height) %>% spin(),
            ),
          ),
        ),
        tabPanel(
          "The phenomenon",
          fluidRow(
            column(
              12,
              # col-sm-12
              class = "col-xxl-10",
              h4(
                "How do each of the following characteristics of the",
                strong("phenomenon"), "being examined",
                "affect the chances of replicating a study",
                "in your sub-field in a different location from the prior study?"
              ),
              plotlyOutput("q10", height = plot_height) %>% spin(),
            ),
          ),
        ),
      ),
    ),

    # Opinions ####
    tabPanel(
      "Opinions",
      icon = icon("comments"),
      tabsetPanel(
        id = "op_panel",
        tabPanel(
          "Published results",
          fluidRow(
            column(
              12,
              # col-sm-12
              class = "col-xxl-8",
              h4(
                "Please estimate the percentage of recent studies",
                "published in your sub-field that ... replicated.",
              ),
              plotlyOutput("q12", height = plot_height) %>% spin(),
            ),
          ),
        ),
        tabPanel(
          "Views",
          fluidRow(
            column(
              12,
              # col-sm-12
              class = "col-xxl-8",
              h4(
                "To what extent do you agree with the following",
                "statements about replications in your subfield?",
              ),
              h4(
                "Replication studies can be used to",
                "assess whether a prior study's ...",
              ),
              plotlyOutput("q7", height = plot_height) %>% spin(),
            ),
          ),
        ),
        tabPanel(
          "Barriers",
          fluidRow(
            column(
              12,
              # col-sm-12
              class = "col-xxl-10",
              h4(
                "How often do each of the following factors",
                "affect researchers' decisions to attempt",
                "a replication in your sub-field?",
              ),
              plotlyOutput("q15", height = plot_full_height) %>% spin(),
            ),
          ),
        ),
      ),
    ),

    # Experience ####
    tabPanel(
      "Experience",
      icon = icon("bullseye"),
      fluidRow(
        column(
          12,
          # col-sm-12
          class = "col-xxl-10",
          h4("In the last 2 years, have you...?"),
          plotlyOutput("q17", height = plot_height) %>% spin(),
        ),
      ),
    ),
  )
)

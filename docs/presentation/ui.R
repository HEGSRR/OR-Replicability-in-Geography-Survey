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
          markdown(
            "
            # TODO: edit this
            ### Welcome
            This is an interactive data visualization for
            *Reproducible Research Practices and Barriers to Reproducible Research in Geography:Insights from a Survey*
            by Peter Kedron, Joseph Holler, and Sarah Bardin.

            The purpose of the survey was to systematically develop a baseline of information
            about the state of replicability in geography
            and to identify any important differences between subfields and methodological approaches.

            This data was collected with an online Qualtrics survey from May 17 to June 10, 2022
            from a random sample of researchers publishing in geographic academic journals as the corresponding author.

            The survey inquires researchers about their:

            1) understanding and awareness of replicability;
            2) awareness and implementation of reproducible research practices;
            3) opinions and perceptions about the role of replicability in geography and their subfield; and
            4) experience attempting reproductions of prior studies.

            Within each of these themes, responses can be viewed in aggregate (**Everyone**).

            Alternatively, subsets of geographers can be selected
            according to one of four *subfields* they are most closely associated with:

            - **Human** geography;
            - Spatial analysis and **methods**;
            - **Nature/Society**; or
            - **Physical** geography.

            Or, one of three *methods* they most frequently use:
            - **Quantitative**;
            - **Qualitative**; or
            - **Mixed Methods**.

            ### Links
            Please read our preprint for this study for more details, including discussions of results:

            Kedron, P., Holler, J., & Bardin, S. (2023, June 3).
            *Reproducible Research Practices and Barriers to Reproducible Research in Geography: Insights from a Survey*.
            <https://doi.org/10.31219/osf.io/nyrq9>.

            This data visualization is primarily the work of Yifei Luo
            with supervision of Joseph Holler, and can be cited as:

            Holler, J., Luo, Y., Kedron, P., & Bardin, S. (2023, August 9). *replicability Survey Data Visualization*.
            <https://doi.org/10.17605/osf.io/b47xu>.

            A sample BibTeX entry is as follows:

            ```
            @article{holler_2023_replicability,
              author = {Holler, Joseph and Luo, Yifei and Kedron, Peter and Bardin, Sarah},
              title = {replicability Survey Data Visualization},
              doi = {10.17605/osf.io/b47xu},
              url = {https://osf.io/b47xu/},
              year = {2023},
              month = {08}
            }
            ```

            The code for this visualization is available on GitHub:
            [HEGSRR/OR-replicability-in-Geography-Survey](https://github.com/HEGSRR/OR-replicability-in-Geography-Survey).

            The overarching project for this study is available on OSF:
            [A Survey of replicability in Geographic Research](https://osf.io/5yeq8/).

            You can also follow progress on our follow-up study on *replicability* here:
            [A Survey of Researcher Perceptions of Replication in Geography](https://osf.io/x6qrk/).

            This project was funded through a
            National Science Foundation Directorate for Social, Behavioral and Economic Sciences award,
            number [BCS-2049837](https://www.nsf.gov/awardsearch/showAward?AWD_ID=2049837).
            "
          ),
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
          "Question 5",
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
          "Question 6",
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
          "Question 19",
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
              class = "col-xxl-8",
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
              class = "col-xxl-8",
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

options(repos = c(CRAN = "https://cloud.r-project.org"))
#-----
#PSSI Triage Tool app - December 2025
#----
library(jose)
library(rsconnect)
library(shiny)
library(shinyjs)
library(dplyr)
library(tibble)
library(ggplot2)
library(ggrepel)
library(RColorBrewer)
library(fmsb)
library(scales)
library(stringr)
library(leaflet)

# Full data setup (all 46 stressors)
pssi_items <- tibble::tribble(
  ~id, ~domain, ~item,
  1, "Academic", "Preparing for exams",
  2, "Academic", "Writing exams",
  3, "Academic", "Writing multiple exams around the same time",
  4, "Academic", "Exams worth more than 50% of course grade",
  5, "Academic", "Heavily weighted assignments",
  6, "Academic", "Having multiple assignments due around the same time",
  7, "Academic", "Managing my academic workload",
  8, "Academic", "Receiving a bad grade",
  9, "Academic", "Managing a high GPA",
  10, "Academic", "Working on my thesis",
  11, "Academic", "Performing well at my professional placement (i.e., practicum, clerkship, etc.)",
  12, "Learning Environment", "Poor communication from professor",
  13, "Learning Environment", "Unclear expectations from professor",
  14, "Learning Environment", "Lack of guidance from professor",
  15, "Learning Environment", "Meeting with my professor",
  16, "Learning Environment", "Meeting my thesis/placement supervisor's expectations",
  17, "Learning Environment", "Lack of mentoring from my thesis/placement supervisor",
  18, "Campus Culture", "Adjusting to the post-secondary lifestyle",
  19, "Campus Culture", "Adjusting to my program",
  20, "Campus Culture", "Academic competition among my peers",
  21, "Campus Culture", "Feeling like I'm not working hard enough",
  22, "Campus Culture", "Feeling like my peers are smarter than I am",
  23, "Campus Culture", "Pressure to succeed",
  24, "Campus Culture", "Discrimination on campus",
  25, "Campus Culture", "Sexual harassment on campus",
  26, "Interpersonal", "Making new friends",
  27, "Interpersonal", "Maintaining friendships",
  28, "Interpersonal", "Networking with the \"right\" people",
  29, "Interpersonal", "Feeling pressured to socialize",
  30, "Interpersonal", "Balancing a social life with academics",
  31, "Interpersonal", "Comparing myself to others",
  32, "Interpersonal", "Comparing my life to others' on social media",
  33, "Interpersonal", "Meeting other peoples' expectations of me",
  34, "Interpersonal", "Meeting my own expectations",
  35, "Personal", "Making sure that I get enough sleep",
  36, "Personal", "Making sure that I get enough exercise",
  37, "Personal", "Making sure that I eat healthy",
  38, "Personal", "Having to prepare meals for myself",
  39, "Personal", "Balancing working at my job with my academics",
  40, "Personal", "Balancing my extracurriculars with academics",
  41, "Personal", "Feeling guilty about taking time for my hobbies/interests",
  42, "Personal", "Having to take student loans",
  43, "Personal", "Worrying about paying off debt",
  44, "Personal", "Worrying about getting a job after graduating",
  45, "Personal", "Worrying about getting into a new program after graduating",
  46, "Personal", "Worrying about reaching major \"life events\" (i.e., buying a house, marriage, children)"
)

graph_labels <- c(
  "Preparing for exams" = "Exam Prep",
  "Writing exams" = "Exams",
  "Writing multiple exams around the same time" = "Multi Exams",
  "Exams worth more than 50% of course grade" = "Heavy Exams",
  "Heavily weighted assignments" = "Heavy Assign",
  "Having multiple assignments due around the same time" = "Mult Assign",
  "Managing my academic workload" = "Workload",
  "Receiving a bad grade" = "Bad Grade",
  "Managing a high GPA" = "GPA",
  "Working on my thesis" = "Thesis",
  "Performing well at my professional placement (i.e., practicum, clerkship, etc.)" = "Placement",
  "Poor communication from professor" = "Poor comm",
  "Unclear expectations from professor" = "Clarity",
  "Lack of guidance from professor" = "Guidance",
  "Meeting with my professor" = "Meet Professor",
  "Meeting my thesis/placement supervisor's expectations" = "Meet Expec (Advisor)",
  "Lack of mentoring from my thesis/placement supervisor" = "Mentoring",
  "Adjusting to the post-secondary lifestyle" = "Lifestyle",
  "Adjusting to my program" = "Program",
  "Academic competition among my peers" = "Competition",
  "Feeling like I'm not working hard enough" = "Work Hard",
  "Feeling like my peers are smarter than I am" = "Smarter",
  "Pressure to succeed" = "Succeed",
  "Discrimination on campus" = "Discrimination",
  "Sexual harassment on campus" = "Harassment",
  "Making new friends" = "New Friends",
  "Maintaining friendships" = "Maintain Friends",
  "Networking with the \"right\" people" = "Networking",
  "Feeling pressured to socialize" = "Pressure (Social)",
  "Balancing a social life with academics" = "Balance (Social)",
  "Comparing myself to others" = "Comparing",
  "Comparing my life to others' on social media" = "Soc Media",
  "Meeting other peoples' expectations of me" = "Meet Expec (Others)",
  "Meeting my own expectations" = "Meet Expec (Self)",
  "Making sure that I get enough sleep" = "Sleep",
  "Making sure that I get enough exercise" = "Exercise",
  "Making sure that I eat healthy" = "Nutrition",
  "Having to prepare meals for myself" = "Cooking",
  "Balancing working at my job with my academics" = "Balance (Work)",
  "Balancing my extracurriculars with academics" = "Balance (XCs)",
  "Feeling guilty about taking time for my hobbies/interests" = "Hobbies",
  "Having to take student loans" = "Loans",
  "Worrying about paying off debt" = "Debt",
  "Worrying about getting a job after graduating" = "Job",
  "Worrying about getting into a new program after graduating" = "New Program",
  "Worrying about reaching major \"life events\" (i.e., buying a house, marriage, children)" = "Life Events"
)

domain_colors <- c(
  "Academic" = "#E41A1C",
  "Learning Environment" = "#377EB8",
  "Campus Culture" = "#4DAF4A",
  "Interpersonal" = "#984EA3",
  "Personal" = "#FF7F00"
)


ui <- navbarPage("COMPASS",
                 id = "main_navbar",
                 # Link to external CSS file
                 header = tags$head(
                   tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
                 ),
                 
                 # --- Tab 1: Home ---
                 tabPanel("Home",
                          fluidRow(
                            column(12, align = "center",
                                   br(), br(),
                                   h1("COMPASS", class = "home-title"),
                                   h4("CAMPUS ONLINE MATCHING OF PERSONALIZED ACADEMIC & STUDENT SUPPORTS", class = "home-subtitle"),
                                   p(em("A navigational tool made \"for-students, by-students\""), class = "home-tagline"),
                                   br(),
                                   tags$img(src = "https://images.pexels.com/photos/1438072/pexels-photo-1438072.jpeg", 
                                            width = "600px"),
                                   br(), br(),
                                   actionButton("start_assessment", "START ASSESSMENT", 
                                                class = "btn-primary btn-start"),
                                   br(), br(),
                                   p("POWERED BY THE LINDEN LAB")
                            )
                          )
                 ),
                 
                 # --- Tab 2: Assessment ---
                 tabPanel("Assessment",
                          fluidRow(
                            column(10, offset = 1, 
                                   fluidRow(
                                     column(12, align = "center",
                                            br(),
                                            h3("Student Stressor Assessment"),
                                            p("Select your institution and then rate your stressors in the tabs below.", class = "assess-intro"),
                                            div(class = "institution-select-container",
                                                selectInput("institution", NULL, 
                                                            choices = c("Concordia University", "University of Calgary", "Queen's University"), 
                                                            width = "100%")
                                            ),
                                            br(), br()
                                     )
                                   ),
                                   fluidRow(
                                     column(12,
                                            div(class = "alert alert-info", role = "alert",
                                                HTML("<strong>Instructions:</strong> Browse through the five domain tabs below. Rate stressors by severity on a scale of 0-10. Leave unrated (0) if not applicable.")
                                            )
                                     )
                                   ),
                                   tabsetPanel(
                                     id = "tabs",
                                     tabPanel("Academic", uiOutput("academic_ui")),
                                     tabPanel("Learning Environment", uiOutput("learning_env_ui")),
                                     tabPanel("Campus Culture", uiOutput("campus_culture_ui")),
                                     tabPanel("Interpersonal", uiOutput("interpersonal_ui")),
                                     tabPanel("Personal", uiOutput("personal_ui"))
                                   ),
                                   br(), hr(),
                                   fluidRow(
                                     column(12, align = "center",
                                            p(em("Note: Submitting these responses does not record any data. All data is self-contained in this session and will be lost if you close the browser window."),
                                              class = "privacy-note"),
                                            actionButton("submit", "SUBMIT RESPONSES", class = "btn-primary btn-lg btn-submit-custom"),
                                            br(), br(), br()
                                     )
                                   )
                            )
                          )
                 ),
                 
                 # --- Tab 3: Your Profile ---
                 tabPanel("Your Profile",
                          fluidRow(
                            column(5,
                                   uiOutput("institution_label"),
                                   br(), br(),
                                   h4("Your Top 10 Stressors"),
                                   p("Based on your responses, here are the top stressors you rated highest:"),
                                   br(),
                                   uiOutput("top_stressors_table")
                            ),
                            column(7,
                                   h4("Your Stressors"),
                                   checkboxGroupInput("domain_filter", "Filter by Domain:",
                                                      choices = unique(pssi_items$domain),
                                                      selected = unique(pssi_items$domain),
                                                      inline = TRUE
                                   ),
                                   plotOutput("lollipop_plot", height = "600px")
                            )
                          )
                 ),
                 
                 # --- Tab 4: Recommendations ---
                 tabPanel("Recommendations",
                          sidebarLayout(
                            sidebarPanel(
                              h4("Filters"),
                              br(),
                              h5("Stressors:"),
                              uiOutput("stressor_filter_ui"),
                              hr(),
                              h5("Specific Student Groups:"),
                              checkboxGroupInput("student_group_filters", label = NULL,
                                                 choices = c("BIPOC Students", 
                                                             "International Students", 
                                                             "Mature Students", 
                                                             "Graduate Students",
                                                             "First Year Students",
                                                             "2SLGBTQIA+ Students",
                                                             "Indigenous Students"),
                                                 selected = NULL)
                            ),
                            mainPanel(
                              fluidRow(
                                column(12,
                                       div(class = "view-mode-container",
                                           radioButtons("view_mode", "View Mode:",
                                                        choices = c("List View", "Map View"),
                                                        selected = "List View",
                                                        inline = TRUE)
                                       )
                                )
                              ),
                              uiOutput("results_view")
                            )
                          )
                 )
)

server <- function(input, output, session) {
  
  observeEvent(input$start_assessment, {
    updateNavbarPage(session, inputId = "main_navbar", selected = "Assessment")
  })
  
  # --- Assessment UI --- 
  render_domain_ui <- function(domain_name) {
    items <- pssi_items %>% filter(domain == domain_name)
    n <- nrow(items)
    mid <- ceiling(n / 2)
    items_left <- items[1:mid, ]
    items_right <- if (n > 1) items[(mid + 1):n, ] else NULL
    
    create_sliders <- function(sub_items) {
      lapply(seq_len(nrow(sub_items)), function(i) {
        id <- sub_items$id[i]
        full_item <- sub_items$item[i]
        div(class = "slider-group",
            tags$label(full_item, class = "slider-label"),
            sliderInput(inputId = paste0("sev_", id), 
                        label = NULL, 
                        min = 0, max = 10, value = 0,
                        width = "100%",
                        ticks = TRUE)
        )
      })
    }
    tagList(br(), fluidRow(column(6, create_sliders(items_left)), column(6, if (!is.null(items_right)) create_sliders(items_right))))
  }
  
  output$academic_ui <- renderUI({ render_domain_ui("Academic") })
  output$learning_env_ui <- renderUI({ render_domain_ui("Learning Environment") })
  output$campus_culture_ui <- renderUI({ render_domain_ui("Campus Culture") })
  output$interpersonal_ui <- renderUI({ render_domain_ui("Interpersonal") })
  output$personal_ui <- renderUI({ render_domain_ui("Personal") })
  
  responses <- eventReactive(input$submit, {
    tibble(
      id = pssi_items$id,
      item = pssi_items$item,
      domain = pssi_items$domain,
      severity = sapply(pssi_items$id, function(i) input[[paste0("sev_", i)]] %||% 0)
    ) %>%
      mutate(label = graph_labels[item]) %>%
      filter(severity > 0)
  })
  
  # --- Your Profile Logic --- 
  output$institution_label <- renderUI({
    tags$div(
      class = "institution-pill",
      toupper(input$institution)
    )
  })
  
  output$top_stressors_table <- renderUI({
    req(responses())
    df <- responses() %>% arrange(desc(severity)) %>% head(10) %>% mutate(rank = row_number())
    if(nrow(df) == 0) return(p("No stressors reported yet.", class = "table-empty-msg"))
    
    rows <- lapply(1:nrow(df), function(i) {
      row <- df[i,]
      dom_color <- domain_colors[row$domain]
      tags$tr(
        tags$td(row$rank, class = "td-rank"),
        tags$td(row$label, class = "td-item"),
        tags$td(row$domain, class = "td-domain", style = paste0("color:", dom_color))
      )
    })
    tags$table(class = "stressor-table",
               tags$thead(tags$tr(tags$th("Rank"), tags$th("Stressor"), tags$th("Domain"))),
               tags$tbody(rows))
  })
  
  output$lollipop_plot <- renderPlot({
    req(responses())
    data <- responses() %>% filter(domain %in% input$domain_filter)
    if(nrow(data) == 0) return(NULL)
    data <- data %>% mutate(label = factor(label, levels = rev(unique(label))))
    ggplot(data) +
      geom_segment(aes(x = 0, xend = severity, y = label, yend = label), color = "gray80", size = 2) +
      geom_point(aes(x = severity, y = label, color = domain), size = 5) +
      scale_color_manual(values = domain_colors) +
      scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 2), name = "Severity") +
      labs(y = NULL) + theme_minimal(base_size = 15) + theme(axis.text.y = element_text(size = 13))
  })
  
  # --- Recommendations Logic ---
  output$stressor_filter_ui <- renderUI({
    checkboxGroupInput("stressor_scope", label = NULL,
                       choices = c("My Top 10 Stressors", "All Services/Supports"),
                       selected = "My Top 10 Stressors")
  })
  
  output$results_view <- renderUI({
    if (input$view_mode == "List View") {
      tagList(
        h3("List of Resources"),
        p("Resources matching your top stressors and selected groups will appear here.", class = "list-placeholder")
      )
    } else {
      tagList(
        h3("Map of Campus Services"),
        leafletOutput("campus_map", height = "500px")
      )
    }
  })
  
  output$campus_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -96, lat = 55, zoom = 3)
  })
  
  observeEvent(input$submit, {
    updateNavbarPage(session, inputId = "main_navbar", selected = "Your Profile")
  })
}

shinyApp(ui, server)
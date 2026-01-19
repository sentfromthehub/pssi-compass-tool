options(repos = c(CRAN = "https://cloud.r-project.org"))
#-----
#PSSI Triage Tool app - January 2025
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

# Abridged data setup (26 stressors)
pssi_items <- tibble::tribble(
  ~id, ~domain, ~item,
  1, "Academic", "Preparing for and writing exams",
  2, "Academic", "Having multiple assessments due around the same time",
  3, "Academic", "Heavily weighted assessments",
  4, "Academic", "Managing my academic workload",
  5, "Academic", "Maintaining my GPA",
  6, "Academic", "Working on my thesis",
  7, "Academic", "Performing well at my professional placement (i.e., practicum, clerkship, etc.)",
  8, "Learning Environment", "Poor communication from my professor(s) or supervisor",
  9, "Learning Environment", "Meeting with my professor(s) or supervisor",
  10, "Learning Environment", "Meeting my professor(s) or supervisor's expectations",
  11, "Campus Culture", "Adjusting to the post-secondary setting",
  12, "Campus Culture", "Academic competition among my peers",
  13, "Campus Culture", "Feeling like I'm not working hard enough or am not smart enough",
  14, "Campus Culture", "Pressure to succeed",
  15, "Campus Culture", "Experiencing discrimination and/or harassment on campus",
  16, "Interpersonal", "Managing friendships",
  17, "Interpersonal", "Networking with the \"right\" people",
  18, "Interpersonal", "Feeling pressured to socialize or engage in a social life",
  19, "Interpersonal", "Balancing my academics with everything else (socializing, extracurriculars, etc.)",
  20, "Interpersonal", "Comparing myself to others",
  21, "Interpersonal", "Meeting expectations (my own expectations for myself, or others’ expectations of me)",
  22, "Personal", "Taking care of my health (getting enough sleep, exercise, eating well)",
  23, "Personal", "Balancing my paid work with my academics",
  24, "Personal", "Finding time for my hobbies/interests",
  25, "Personal", "Having to take student loans and worrying about debt",
  26, "Personal", "Worrying about “what happens next” (getting a job after graduation, getting into a new program)"
)

graph_labels <- c(
  "Preparing for and writing exams" = "Exams",
  "Having multiple assessments due around the same time" = "Multi Deadlines",
  "Heavily weighted assessments" = "Heavy Weights",
  "Managing my academic workload" = "Workload",
  "Maintaining my GPA" = "GPA",
  "Working on my thesis" = "Thesis",
  "Performing well at my professional placement (i.e., practicum, clerkship, etc.)" = "Placement",
  "Poor communication from my professor(s) or supervisor" = "Poor Comm",
  "Meeting with my professor(s) or supervisor" = "Meeting Profs",
  "Meeting my professor(s) or supervisor's expectations" = "Prof Expectations",
  "Adjusting to the post-secondary setting" = "Adjusting",
  "Academic competition among my peers" = "Competition",
  "Feeling like I'm not working hard enough or am not smart enough" = "Imposter Syndrome",
  "Pressure to succeed" = "Pressure",
  "Experiencing discrimination and/or harassment on campus" = "Discrimination",
  "Managing friendships" = "Friendships",
  "Networking with the \"right\" people" = "Networking",
  "Feeling pressured to socialize or engage in a social life" = "Social Pressure",
  "Balancing my academics with everything else (socializing, extracurriculars, etc.)" = "Balance (Social)",
  "Comparing myself to others" = "Comparison",
  "Meeting expectations (my own expectations for myself, or others’ expectations of me)" = "Expectations",
  "Taking care of my health (getting enough sleep, exercise, eating well)" = "Health/Self-care",
  "Balancing my paid work with my academics" = "Balance (Work)",
  "Finding time for my hobbies/interests" = "Hobbies",
  "Having to take student loans and worrying about debt" = "Finances/Debt",
  "Worrying about “what happens next” (getting a job after graduation, getting into a new program)" = "Future/Career"
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
                 # Link to external CSS and JS files
                 header = tags$head(
                   tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
                   tags$script(src = "script.js"),
                   tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Nunito:wght@300;400;600;700&family=Quicksand:wght@300;400;500;600;700&display=swap"),
                   useShinyjs()
                 ),
                 
                 # --- Tab 1: Home ---
                 tabPanel("Home",
                          div(class = "home-container",
                              fluidRow(
                                column(12, align = "center",
                                       div(class = "hero-content",
                                           h1("COMPASS", class = "hero-title"),
                                           h4("CAMPUS ONLINE MATCHING OF PERSONALIZED ACADEMIC & STUDENT SUPPORTS", class = "hero-subtitle"),
                                           p(em("A navigational tool made \"for-students, by-students\""), class = "hero-tagline"),
                                           br(),
                                           tags$img(src = "https://images.pexels.com/photos/1438072/pexels-photo-1438072.jpeg", 
                                                    width = "600px", class = "home-img"),
                                           br(), br(),
                                           div(class = "partners-section",
                                               p("PARTNER INSTITUTIONS", class = "partners-label"),
                                               div(class = "partners-logos",
                                                   tags$a(href = "https://www.queensu.ca/", target = "_blank",
                                                          tags$img(src = "queens.png", class = "partner-logo")),
                                                   tags$a(href = "https://www.concordia.ca/", target = "_blank",
                                                          tags$img(src = "concordia.png", class = "partner-logo")),
                                                   tags$a(href = "https://www.ucalgary.ca/", target = "_blank",
                                                          tags$img(src = "calgary.png", class = "partner-logo"))
                                               )
                                           ),
                                           br(),
                                           
                                           actionButton("start_assessment", "START JOURNEY", 
                                                        class = "btn-glass"),
                                           br(), br(),
                                           p("POWERED BY THE LINDEN LAB", class = "footer-text")
                                       )
                                )
                              )
                          )
                 ),
                 
                 # --- Tab 2: Assessment ---
                 tabPanel("Assessment",
                          fluidRow(
                            column(10, offset = 1, 
                                   div(class = "glass-card",
                                       fluidRow(
                                         column(12, align = "center",
                                                br(),
                                                h3("Student Stressor Assessment"),
                                                p("Select your institution and then rate your stressors in the tabs below.", class = "assess-intro"),
                                                div(class = "institution-select-container",
                                                    selectInput("institution", NULL, 
                                                                choices = c("Queen's University", "Concordia University", "University of Calgary"), 
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
                                                actionButton("submit", "GENERATE PROFILE", class = "btn-primary btn-lg btn-submit-custom"),
                                                br(), br()
                                         )
                                       )
                                   )
                            )
                          )
                 ),
                 
                 # --- Tab 3: Your Profile ---
                 tabPanel("Your Profile",
                          fluidRow(
                            column(10, offset = 1,
                                   div(class = "glass-card",
                                       fluidRow(
                                         column(5,
                                                uiOutput("institution_label"),
                                                br(),
                                                h4("Your Top 10 Stressors", class = "profile-section-heading"),
                                                p("Based on your responses, here are the top stressors you rated highest:", 
                                                  class = "profile-section-desc"),
                                                uiOutput("top_stressors_table")
                                         ),
                                         column(7,
                                                h4("Visual Breakdown", class = "profile-section-heading"),
                                                p("Filter Domain:", class = "profile-filter-label"),
                                                checkboxGroupInput("domain_filter", NULL,
                                                                   choices = unique(pssi_items$domain),
                                                                   selected = unique(pssi_items$domain),
                                                                   inline = TRUE
                                                ),
                                                plotOutput("lollipop_plot", height = "550px")
                                         )
                                       ),
                                       br(), hr(),
                                       fluidRow(
                                         column(12, align = "center",
                                                actionButton("go_to_recommendations", "VIEW RECOMMENDATIONS", 
                                                             class = "btn-primary btn-lg btn-submit-custom")
                                         )
                                       )
                                   )
                            )
                          )
                 ),
                 
                 
                 # --- Tab 4: Recommendations ---
                 tabPanel("Recommendations",
                          fluidRow(
                            column(10, offset = 1,
                                   fluidRow(
                                     # Left Sidebar Card
                                     column(3,
                                            div(class = "glass-card",
                                                h4("Filters"),
                                                br(),
                                                h5("Stressors:"),
                                                uiOutput("stressor_filter_ui"),
                                                hr(),
                                                h5("Specific Student Groups:"),
                                                checkboxGroupInput("student_group_filters", label = NULL,
                                                                   choices = c("BIPOC", 
                                                                               "International", 
                                                                               "Mature", 
                                                                               "Graduate", 
                                                                               "First Year", 
                                                                               "2SLGBTQIA+", 
                                                                               "Indigenous"),
                                                                   selected = NULL)
                                            )
                                     ),
                                     # Main Results Card
                                     column(9,
                                            div(class = "glass-card", 
                                                fluidRow(
                                                  column(12,
                                                         div(class = "toggle-container",
                                                             uiOutput("results_title"),
                                                             tags$label(class = "switch-label",
                                                                        tags$input(type = "checkbox", id = "view_mode_toggle", class="toggle-input"),
                                                                        span(class = "slider-track",
                                                                             span(class = "slider-text-left", "List View"),
                                                                             span(class = "slider-text-right", "Map View"),
                                                                             span(class = "slider-thumb")
                                                                        )
                                                             )
                                                         )
                                                  )
                                                ),
                                                br(),
                                                uiOutput("results_view")
                                            )
                                     )
                                   )
                            )
                          )
                 )
)

server <- function(input, output, session) {
  
  # --- init: disable result tabs on startup ---
  observe({
    addClass(selector = "a[data-value='Your Profile']", class = "disabled-tab")
    addClass(selector = "a[data-value='Recommendations']", class = "disabled-tab")
    
    runjs("$('a[data-value=\"Your Profile\"]').attr('title', '');")
    runjs("$('a[data-value=\"Recommendations\"]').attr('title', '');")
  })
  
  observeEvent(input$start_assessment, {
    updateNavbarPage(session, inputId = "main_navbar", selected = "Assessment")
  })
  
  observeEvent(input$go_to_recommendations, {
    updateNavbarPage(session, inputId = "main_navbar", selected = "Recommendations")
  })
  
  # --- Assessment UI ---
  render_domain_ui <- function(domain_name) {
    items <- pssi_items %>% filter(domain == domain_name)
    
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
    
    tagList(
      br(),
      fluidRow(
        column(10, offset = 1, create_sliders(items))
      )
    )
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
    h3(toupper(input$institution), class = "institution-label")
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
        tags$td(row$label, class = "td-item", style = paste0("color:", dom_color, "; font-weight: 600;")),
        tags$td(row$severity, class = "td-sev")
      )
    })
    tags$table(class = "stressor-table",
               tags$thead(tags$tr(tags$th("Rank"), tags$th("Stressor"), tags$th("Severity"))),
               tags$tbody(rows))
  })
  
  output$lollipop_plot <- renderPlot({
    req(responses())
    data <- responses() %>% filter(domain %in% input$domain_filter)
    if(nrow(data) == 0) return(NULL)
    
    ggplot(data, aes(x = severity, y = reorder(label, severity), color = domain)) +
      geom_segment(aes(x = 0, xend = severity, y = label, yend = label), color = "gray80", size = 2) +
      geom_point(aes(x = severity, y = label, color = domain), size = 5) +
      scale_color_manual(values = domain_colors) +
      scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 2), name = "Severity") +
      labs(y = NULL) + 
      theme_minimal(base_size = 14) + 
      theme(
        axis.text.y = element_text(size = 11, color = "#333"),
        axis.text.x = element_text(size = 11, color = "#666"),
        axis.title.x = element_text(size = 12, color = "#333", margin = margin(t = 10)),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major.y = element_line(color = "#f0f0f0"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(color = "#e8e8e8"),
        legend.position = "none"
      )
  }, bg = "transparent")
  
  # --- Recommendations Logic ---
  output$stressor_filter_ui <- renderUI({
    checkboxGroupInput("stressor_scope", label = NULL,
                       choices = c("My Top 10 Stressors", "All Services/Supports"),
                       selected = "My Top 10 Stressors")
  })
  
  output$results_title <- renderUI({
    if (is.null(input$view_mode_toggle) || input$view_mode_toggle == FALSE) {
      h4("List of Resources", style = "margin: 0;")
    } else {
      h4("Map of Campus Services", style = "margin: 0;")
    }
  })
  
  output$results_view <- renderUI({
    if (is.null(input$view_mode_toggle) || input$view_mode_toggle == FALSE) {
      tagList(
        p("Resources matching your top stressors and selected groups will appear here.", class = "list-placeholder")
      )
    } else {
      tagList(
        leafletOutput("campus_map", height = "500px")
      )
    }
  })
  
  output$campus_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -96, lat = 55, zoom = 3)
  })
  
  # --- Enable Tabs ---
  observeEvent(input$submit, {
    removeClass(selector = "a[data-value='Your Profile']", class = "disabled-tab")
    removeClass(selector = "a[data-value='Recommendations']", class = "disabled-tab")
    
    runjs("$('a[data-value=\"Your Profile\"]').removeAttr('title');")
    runjs("$('a[data-value=\"Recommendations\"]').removeAttr('title');")
    
    updateNavbarPage(session, inputId = "main_navbar", selected = "Your Profile")
  })
}

shinyApp(ui, server)
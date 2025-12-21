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
                 
                 tabPanel("Home",
                          fluidRow(
                            column(12, align = "center",
                                   br(), br(),
                                   h1("COMPASS", style = "font-size: 48px; font-weight: bold"),
                                   h4("CAMPUS ONLINE MATCHING OF PERSONALIZED ACADEMIC & STUDENT SUPPORTS", style = "color: #6c8ebf;"),
                                   p(em("A navigational tool made \"for-students, by-students\""), style = "font-size: 16px"),
                                   br(),
                                   tags$img(src = "https://images.pexels.com/photos/1438072/pexels-photo-1438072.jpeg", 
                                            width = "600px"),
                                   br(), br(),
                                   actionButton("start_assessment", "START ASSESSMENT", 
                                                class = "btn-primary", 
                                                style = "font-size: 18px; padding: 12px 40px"),
                                   br(), br(),
                                   p("POWERED BY THE LINDEN LAB")
                            )
                          )
                 ),
                 
                 tabPanel("Assessment",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("institution", "Select Your Institution:",
                                          choices = c("Concordia University", "University of Calgary", "Queen's University")),
                              
                              helpText(HTML("The PSSI is a tool made <em>for students, by students</em>, designed to assess stressors across five domains. Use the colored tabs to the right to rate each stressor by <strong>severity</strong> using the slider. Leave it unrated if it's not applicable to you!")),
                              helpText(strong("Make sure to browse through the stressors in all tabs before clicking submit below.")),
                              
                              p(em("Note: Submitting these responses does not record any data. All data is self-contained in this session and will be lost if you close the browser window."),
                                style = "font-size: 0.85em; color: #555;"),
                              br(),
                              
                              actionButton("submit", "Submit Responses", class = "btn-primary")
                            ),
                            mainPanel(
                              tabsetPanel(
                                id = "tabs",
                                tabPanel("Academic", uiOutput("academic_ui")),
                                tabPanel("Learning Environment", uiOutput("learning_env_ui")),
                                tabPanel("Campus Culture", uiOutput("campus_culture_ui")),
                                tabPanel("Interpersonal", uiOutput("interpersonal_ui")),
                                tabPanel("Personal", uiOutput("personal_ui"))
                              )
                            )
                          )
                 ),
                 
                 tabPanel("Your Profile",
                          fluidRow(
                            # Left Panel: Institution + Table
                            column(5,
                                   uiOutput("institution_label"),
                                   br(),
                                   h4("Your Top 10 Stressors"),
                                   p("Based on your responses, here are the top stressors you rated highest:"),
                                   br(),
                                   uiOutput("top_stressors_table")
                            ),
                            # Right Panel: Filters + Plot
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
                 
                 tabPanel("Recommendations",
                          h4("Recommended Resources"),
                          p("Based on your responses on the PSSI, here are some recommended resources mapped directly to your top ten stressors."),
                          uiOutput("recommendation_output")
                 )
)

server <- function(input, output, session) {
  
  observeEvent(input$start_assessment, {
    updateNavbarPage(session, inputId = "main_navbar", selected = "Assessment")
  })
  
  render_domain_ui <- function(domain_name) {
    items <- pssi_items %>% filter(domain == domain_name)
    tagList(
      tags$h4(strong("Rate each of the following stressors by severity."), style = "margin-bottom: 10px;"),
      lapply(seq_len(nrow(items)), function(i) {
        id <- items$id[i]
        full_item <- items$item[i]
        fluidRow(
          column(8, tags$div(style = "margin-top: 10px;", strong(full_item))),
          column(4, sliderInput(inputId = paste0("sev_", id), label = NULL, min = 0, max = 10, value = 0))
        )
      })
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
    
  output$institution_label <- renderUI({
    tags$div(
      style = "background-color: #E0E0E0; color: #333; padding: 10px 15px; border-radius: 5px; font-weight: bold; display: inline-block;",
      toupper(input$institution)
    )
  })
  
  output$top_stressors_table <- renderUI({
    req(responses())
    df <- responses() %>% arrange(desc(severity)) %>% head(10) %>% mutate(rank = row_number())
    
    if(nrow(df) == 0) return(p("No stressors reported yet."))
    
    rows <- lapply(1:nrow(df), function(i) {
      row <- df[i,]
      dom_color <- domain_colors[row$domain]
      tags$tr(
        tags$td(row$rank, style="padding:8px; border:1px solid #ddd; text-align:center;"),
        tags$td(row$label, style="padding:8px; border:1px solid #ddd;"),
        tags$td(row$domain, style=paste0("padding:8px; border:1px solid #ddd; font-weight:bold; color:", dom_color))
      )
    })
    
    tags$table(style="width:100%; border-collapse:collapse;",
               tags$thead(
                 tags$tr(
                   tags$th("Rank", style="padding:8px; border:1px solid #ddd; background-color:#f9f9f9; width:15%;"),
                   tags$th("Stressor", style="padding:8px; border:1px solid #ddd; background-color:#f9f9f9; width:55%;"),
                   tags$th("Domain", style="padding:8px; border:1px solid #ddd; background-color:#f9f9f9; width:30%;")
                 )
               ),
               tags$tbody(rows)
    )
  })
  
  output$lollipop_plot <- renderPlot({
    req(responses())
    data <- responses() %>% filter(domain %in% input$domain_filter)
    
    if(nrow(data) == 0) return(NULL)
    
    data <- data %>%
      mutate(label = factor(label, levels = rev(unique(label))))
    
    ggplot(data) +
      geom_segment(aes(x = 0, xend = severity, y = label, yend = label), color = "gray80", size = 2) +
      geom_point(aes(x = severity, y = label, color = domain), size = 5) +
      scale_color_manual(values = domain_colors) +
      scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 2), name = "Severity") +
      labs(y = NULL) +
      theme_minimal(base_size = 15) +
      theme(axis.text.y = element_text(size = 13))
  })
  
  output$recommendation_output <- renderUI({
    data <- responses() %>% arrange(desc(severity)) %>% head(10)
    lapply(seq_along(data$item), function(i) {
      stressor <- data$item[i]
      domain <- data$domain[i]
      tags$p(HTML(paste0("<strong>", i, ". ", graph_labels[stressor], ":</strong> Suggested campus resource for ", domain)))
    })
  })
  
  observeEvent(input$submit, {
    updateNavbarPage(session, inputId = "main_navbar", selected = "Your Profile")
  })
}

shinyApp(ui, server)
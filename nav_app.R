# ---- NAV TOOL SHINY APP ---- #
library(shiny)

options(repos = c(CRAN = "https://cloud.r-project.org"))

#resource structure
resources <- list(
  "Learn More" = list(
    "General Mental Health Topics" = list(
      list(name = "Podcast", icon = "🔗", url = "https://www.studentmentalhealthnetwork.ca/podcast.html", desc = "A podcast run for-students, by-students, covering topics related to student mental health and wellbeing."),
      list(name = "Mental Health Modules", icon = "📚", url = "https://www.studentmentalhealthnetwork.ca/module-series.html", desc = "Interactive modules covering topics related to student mental health and wellbeing."),
      list(name = "Toolkit Library", icon = "📚", url = "https://www.studentmentalhealthnetwork.ca/toolkits.html", desc = "A library of interactive toolkits covering topics ranging from financial stress, to CBT techniques, to relationship management."), 
      list(name = "Online Courses (Coursera)", icon = "📚", url = "https://www.studentmentalhealthnetwork.ca/online-courses.html", desc = "A directory of comprehensive mental health courses and training opportunities."),
      list(name = "TEDTalk Library", icon = "📚", url = "https://www.studentmentalhealthnetwork.ca/ted-talk-library.html", desc = "A curated directory of videos addressing topics related to mental health and wellbeing.")
    ),
    "Coping and Resilience" = list(
      list(name = "CBT Toolkit", icon = "📚", url = "https://rise.articulate.com/share/uEWlJSm2yZqzGs7o1U-h3jI26R7A8m8R", desc = "A resource kit designed to introduce and help users implement CBT concepts and techniques."),
      list(name = "Self-care and Building Resilience Module", icon = "📚", url = "https://rise.articulate.com/share/Ws5BCQByZBM_L9q8XhL1K2G5Jhaww-pm#/", desc = "This module introduces concepts of self-care and resilience, aiming to help users reduce stress and boost self-esteem."),
      list(name = "From Surviving to Thriving Toolkit", icon = "📚", url = "https://directus9.mediresource.com/assets/00552BDB-3FC7-4F04-9585-D2B761AFFC76/From%20surviving%20to%20thriving%20-%20%20Workbook_EN%20[AODA].pdf/?rnd=574bf2df-7209-4b2f-9443-b34db691c62c", desc = "A resource toolkit designed to help postsecondary students build personal and academic resilience."),
      list(name = "Connections Toolkit", icon = "🔗", url = "https://rise.articulate.com/share/ua2uGsBOqMoBj7T0MNBf-Ldm2Z_4FJdj#/", desc = "A resource toolkit focused on strategies for making friends, maintaining friendships, having difficult conversations and recognizing healthy vs. toxic relationships."),
      list(name = "Self-care Toolkit", icon = "📍", url = "https://studentexperience.uwo.ca/docs/Self-Care%20Toolkit%20for%20University%20Students.pdf", desc = "Designed by Western University's Wellness Education Centre, this reflective, self-care toolkit is designed specifically for students.")
    ),
    "Stress Management Strategies" = list(
      list(name = "General Stress Reduction Strategies", icon = "📍", url = "https://www.studentmentalhealthnetwork.ca/stress-management.html", desc = "A library of stress reduction strategies and resources across multiple, common student stressors."),
      list(name = "Managing Academic Workload", icon = "📍", url = "https://sass.queensu.ca/sites/sasswww/files/uploaded_files/Resource%20PDFs/Managing%20Your%20Time%20at%20University_0.pdf", desc = "This Time Management guide, created by Queen's Unviersity's Student Academic Success Services includes multiple strategies for managing your academic workload."),
      list(name = "Online Guides for Academic Success", icon = "📍", url = "https://sass.queensu.ca/resources/online/pdfs", desc = "This library of resources created by Queen's University's Student Academic Success Centre includes guidance for academic writing, improving focus, note-taking, online learning, and more."),
      list(name = "Managing Stress Module", icon = "📚", url = "https://rise.articulate.com/share/tme5kbZCdUn7Gf91wn6xo0LtZiJfNkVu#/", desc = "This module focuses on managing stress at university/college, including an educational component as well as strategies for reducing stress."),
      list(name = "Financial Toolkit", icon = "📚", url = "https://rise.articulate.com/share/FMb4xzr17bh0sc9m8cZHFSHtbdbViJsF#/", desc = "A resource kit including guidance for managing your finances from monthly budgeting, to doing your taxes, to taking and repaying student loans.")
    )
  ),
  "Connect With People" = list(
    "Online Connection" = list(
      list(name = "Podcast", icon = "🔗", url = "https://www.studentmentalhealthnetwork.ca/podcast.html", desc = "A podcast run for-students, by-students, covering topics related to student mental health and wellbeing."),
      list(name = "Peer Connection Online Forums", icon = "🔗", url = "https://www.studentmentalhealthnetwork.ca/peer-connection.html", desc = "A library of links to several online discussion forums that aim to build community and encourage sharing of lived experience.")
    ),
    "In-person Connection" = list(
      list(name = "Clubs Portal", icon = "🔗", url = "https://www.studentmentalhealthnetwork.ca/clubs-portal.html", desc = "A master portal linking to all student clubs across every college and university in Canada."),
      list(name = "Guide for Adjusting to Post-secondary", icon = "📍", url = "https://usq.pressbooks.pub/academicsuccess/chapter/adjusting-to-university/", desc ="An e-book from the University of Southern Queensland outlines six areas of typical adjustment to the postsecondary setting.")
    )
  ),
  "Access Supports" = list(
    "On-Campus Supports" = list(
      list(name = "Campus Services Map", icon = "📍", url = "https://www.studentmentalhealthnetwork.ca/campus-services-map.html", desc ="An interactive tool mapping all physical, mental, and academic services across every campus in Canada.")
    ),
    "Off-campus Supports" = list(
      list(name = "MH Resources Bank", icon = "📍", url = "https://www.studentmentalhealthnetwork.ca/resources-bank.html", desc = "A bank of mental health services with complete contact info, organized geographically.")
    ),
    "Online Supports" = list(
      list(name = "App Library", icon = "📍", url = "https://www.studentmentalhealthnetwork.ca/mental-health-apps.html", desc = "A curated library of mental health apps ranging from CBT-based to disorder-specific."),
      list(name = "Peer Connection Online Forums", icon = "🔗", url = "https://www.studentmentalhealthnetwork.ca/peer-connection.html", desc = "A library of links to several online discussion forums that aim to build community and encourage sharing of lived experience.")
    ),
    "Support Others" = list(
      list(name = "Online Courses", icon = "📚", url = "https://www.studentmentalhealthnetwork.ca/online-courses.html", desc = "A library of comprehensive mental health courses and educational workshops."),
      list(name = "Help Seeking and Supporting Others Module (Coming Soon!)", icon = "📚", url = "#", desc=""),
      list(name = "Student Distress Guide (Coming Soon!)", icon = "🔗", url = "#", desc="")
    )
  ),
  "Resources for Specific Student Groups" = list(
    "BIPOC Students" = list(
      list(name = "Toolkit", icon = "📚", url = "https://campusmentalhealth.ca/wp-content/uploads/2024/08/Full-2024-BIPOC-MH-Toolkit.pdf", desc = "A toolkit of resources to help navigate mental health stigma, bridge generational differences, and more."),
      list(name = "MH Resource Bank (Demo Specific)", icon = "📍", url = "https://www.studentmentalhealthnetwork.ca/resources-bank.html", desc = "A bank of mental health services with complete contact info. See for specific groups.")
    ),
    "2SLGBTQIA+ Students" = list(
      list(name = "Mental Health and Intersectionality Module", icon = "📚", url = "https://rise.articulate.com/share/-jHp_tcqkNLHBFOQLI0xcAg1_WruvAtk#/", desc = "This module examines intersectionality, focusing on understanding how overlapping identities interact with systems of privilege and oppression in the context of mental health."),
      list(name = "MH Resource Bank (Demo Specific)", icon = "📍", url = "https://www.studentmentalhealthnetwork.ca/resources-bank.html", desc = "A bank of mental health services with complete contact info. See for specific groups."),
      list(name = "Egale Canada", icon = "📍", url = "https://www.egale.ca/", desc = "An external website, including a variety of resources for people within and people supporting the 2SLGBTQIA+ community.")
    ),
    "Indigenous Students" = list(
      list(name = "Indigenous Health and Wellbeing Module", icon = "📚", url = "https://rise.articulate.com/share/n3UJRVoowVC3caFxplLE_HZjl-PAmnsC#/", desc = "This module focuses on Indigenous health and wellbeing, with content developed and reviewed by Indigenous individuals."),
      list(name = "MH Resource Bank (Demo Specific)", icon = "📍", url = "https://www.studentmentalhealthnetwork.ca/resources-bank.html", desc = "A bank of mental health services with complete contact info. See for specific groups.")
    ),
    "Medical Students" = list(
      list(name = "Toolkit", icon = "📚", url = "https://rise.articulate.com/share/uYcpqN-Gdin20KKrnf8CYOCYDF2JUJqt#/", desc = "A toolkit covering topics specific to medical school hopefuls, and current med students.")
    ),
    "First Year Students" = list(
      list(name = "Toolkit", icon = "📚", url = "https://rise.articulate.com/share/ptNoCWykjV4rYXGV0TOS8133Z5bcOPLT#/", desc = "A tool kit intended to help incoming undergraduate students thrive during their first year of postsecondary education."),
      list(name = "Mental Health for First Year Students Module", icon = "📚", url = "https://rise.articulate.com/share/l2uf5pDdQW6H0f6yA8aL0x2q7g5YszVH#/", desc = "This module provides an overview of what to expect from one's first year of postsecondary, in addition to strategies for thriving throughout that first year.")
    ),
    "Graduate Students" = list(
      list(name = "Graduate Student Mental Health Module", icon = "📚", url = "https://rise.articulate.com/share/B4NRtnMkmzYYOvm5Mcm6LvdyQguUwV6y#/", desc = "This module focuses on the unique experiences of graduate students as it relates to navigating grad school and mental health."),
      list(name = "The Thesis Whisperer", icon = "📍", url = "https://thesiswhisperer.com/", desc = "A strategic blog written by a faculty member at Australian National University."),
      list(name = "The Professor Is In", icon = "📍", url = "https://theprofessorisin.com/", desc = "An external website including guidance for grad school, job market, and careers in and out of the academy."),
      list(name = "Overcoming Imposter Syndrome Module (Coming Soon!)", icon = "📚", url = "#", desc="")
    )
  )
)

# --- UI ---
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .header { background-color: #403372; color: white; padding: 15px; font-size: 24px; }
      .filters-panel { border-right: 1px solid #ddd; padding-right: 15px; }
      .resource-panel { padding-left: 15px; }
      .resource { margin-bottom: 15px; }
      .h2-title { color: #403372; font-size: 16px; margin-top: 20px; border-bottom: 1px solid #ccc; padding-bottom: 4px; }
      a { text-decoration: none; color: #403372; }
      a:hover { text-decoration: underline; }
    "))
  ),
  
  div(class = "header", "Network Navigation Tool"),
  div(p("Content on the Network is generally sorted into three areas: Learn, Connect, and Access. Use the filters below to explore.")),
  
  fluidRow(
    column(
      width = 4,
      class = "filters-panel",
      h4("Select Categories"),
      uiOutput("h2Filters")
    ),
    column(
      width = 8,
      class = "resource-panel",
      uiOutput("resourceList")
    )
  )
)

# --- Server ---
server <- function(input, output, session) {
  
  # Emojis for H1 headers
  h1_emojis <- list(
    "Learn More" = "📚",
    "Connect With People" = "🔗",
    "Access Supports" = "📍",
    "Resources for Specific Student Groups" = "🧑‍🎓"
  )
  
  output$h2Filters <- renderUI({
    h2_inputs <- list()
    
    for (h1 in names(resources)) {
      h2s <- names(resources[[h1]])
      h2_keys <- paste(h1, h2s, sep = " | ")
      names(h2_keys) <- h2s
      h1_label <- paste(h1_emojis[[h1]], h1)
      
      h2_inputs <- append(h2_inputs, list(
        tags$h5(h1_label),
        checkboxGroupInput(inputId = paste0("filter_", gsub(" ", "_", h1)),
                           label = NULL,
                           choices = h2_keys,
                           selected = NULL)
      ))
    }
    
    do.call(tagList, h2_inputs)
  })
  
  output$resourceList <- renderUI({
    selected_keys <- unlist(lapply(names(resources), function(h1) {
      input[[paste0("filter_", gsub(" ", "_", h1))]]
    }))
    
    if (length(selected_keys) == 0) {
      return(p("Select categories on the left to see matching resources."))
    }
    
    output_list <- list()
    
    for (key in selected_keys) {
      parts <- strsplit(key, " \\| ")[[1]]
      h1 <- parts[1]
      h2 <- parts[2]
      res_list <- resources[[h1]][[h2]]
      
      output_list <- append(output_list, list(
        div(class = "h2-title", paste(h2))
      ))
      
      for (res in res_list) {
        output_list <- append(output_list, list(
          div(class = "resource",
              tagList(
                if (is.null(res$url) || res$url == "#" || res$url == "") {
                  # Show icon + name as plain text (no link)
                  strong(paste(res$icon, res$name))
                } else {
                  # Show clickable link
                  tags$a(href = res$url, target = "_blank", strong(paste(res$icon, res$name)))
                },
                if (!is.null(res$desc) && res$desc != "") {
                  p(res$desc, style = "margin: 0 0 10px 25px; font-size: 12px; color: #444;")
                }
              )
          )
        ))
      }
    }
    do.call(tagList, output_list)
  })
}

shinyApp(ui, server)
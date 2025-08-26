library(shiny)
library(bslib)
library(openxlsx)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(plotly)
library(DT)
library(stringr)
library(leaflet)
library(leaflet.minicharts)

#test on ipad test

# AJWS branding colors
ajws_primary <- "#0B5C5C"
ajws_accent  <- "#A46A2B"

ajws_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = ajws_primary,
  secondary = ajws_accent,
  base_font = font_google("Lato"),
  heading_font = font_google("Lato")
)

y = "2024"
gms <- read.xlsx("FY25 AJWS Data_3JUL25.xlsx", sheet = "GMS FY25 1")
icc <- read.xlsx("FY25 AJWS Data_3JUL25.xlsx", sheet = "ICCs")

gms$Issue.Area.Grantees <- recode(gms$Program.Director.Area, 
                                  "Sexual Health and Rights" = "SHR",
                                  "Land Water and Climate Justice" = "LWCJ",
                                  "Disaster Response" = "HR",
                                  "Civil and Political Rights" = "CPR")


unique_issue_areas <- gms$Issue.Area.Grantees %>% unique

ncol_dashboard <- unique_issue_areas %>% length

gms_nrow_by_issue_area <- gms %>% 
  select(Issue.Area.Grantees, Organization.ID) %>% 
  group_by(Issue.Area.Grantees, Organization.ID ) %>% 
  summarize(n()) %>% 
  group_by(Issue.Area.Grantees) %>% 
  summarize(n()) %>% 
  as.matrix()


## Accompaniment ##

# gms_capacity_building <- gms %>% mutate(Capacity_Building = ifelse(Capacity.Building == 1, TRUE, FALSE)) %>% 
#   select(Issue.Area.Grantees, Capacity_Building, Organization.ID) %>% 
#   group_by(Issue.Area.Grantees, Organization.ID ) %>% 
#   summarize(total_grants = sum(Capacity_Building),
#             Capacity_building_ind = ifelse(total_grants > 0, TRUE, FALSE)) %>%
#   group_by(Issue.Area.Grantees ) %>% 
#   summarize(counts = sum(Capacity_building_ind)) %>% 
#   as.matrix()


## New Organization ##

gms_new <- gms %>% filter(!is.na(Year.Founded), Year.Founded != "N/A", Year.Founded != "n/a", !is.na(Initial.AJWS.Funding), Initial.AJWS.Funding != "NA")

gms_new_nrow_by_issue_area <- gms_new %>% 
  select(Issue.Area.Grantees, Organization.ID) %>% 
  group_by(Issue.Area.Grantees, Organization.ID ) %>% 
  summarize(n()) %>% 
  group_by(Issue.Area.Grantees ) %>% 
  summarize(n()) %>% 
  as.matrix()

gms_new_orgs <- gms_new %>% mutate(new_org = as.numeric(gms_new$Initial.AJWS.Funding) - as.numeric(gms_new$Year.Founded) <= 5) %>%
  select(Issue.Area.Grantees, new_org, Organization.ID) %>%
  group_by(Issue.Area.Grantees, Organization.ID ) %>%
  summarize(new = sum(new_org),
            new_org_ind = ifelse(new > 0, TRUE, FALSE)) %>%
  group_by(Issue.Area.Grantees) %>%
  summarize(counts = sum(new_org_ind))  %>% 
  as.matrix()


## Engaged in Advocacy ##

gms_advocacy_engaged <- gms %>% mutate(advocacy_engaged = (Advocate.at.International.Level == "Yes" |
                                                             Advocate.at.Local.Level == "Yes" |
                                                             Advocate.at.National.Level == "Yes")) %>%
  select(Issue.Area.Grantees, advocacy_engaged, Organization.ID) %>%
  group_by(Issue.Area.Grantees, Organization.ID ) %>% 
  filter(!is.na(advocacy_engaged)) %>%
  summarize(advocacy = sum(advocacy_engaged),
            advocacy_ind = ifelse(advocacy > 0, TRUE, FALSE)) %>%
  group_by(Issue.Area.Grantees ) %>%
  summarize(counts = sum(advocacy_ind))  %>% 
  as.matrix()


## Legal Services Provided ##

gms_legal_services <- gms %>% mutate(legal_services_provided = (str_detect(Grant.Methodology, "Legal Education") == TRUE | 
                                                                  str_detect(Grant.Methodology, "Legal Aid"))) %>%
  select(Issue.Area.Grantees, legal_services_provided, Organization.ID) %>%
  group_by(Issue.Area.Grantees, Organization.ID) %>%
  summarize(legal_services = sum(legal_services_provided),
            legal_ind = ifelse(legal_services > 0, TRUE, FALSE)) %>%
  group_by(Issue.Area.Grantees) %>%
  summarize(counts = sum(legal_ind))  %>% 
  as.matrix()


## Document Reporting and Fact Finding ##

gms_document_reporting <- gms %>% mutate(document_reporting_fact = (str_detect(Grant.Methodology, "Documenting, Reporting"))) %>%
  select(Issue.Area.Grantees, document_reporting_fact, Organization.ID) %>%
  group_by(Issue.Area.Grantees, Organization.ID ) %>%
  summarize(document_reporting = sum(document_reporting_fact),
            document_ind = ifelse(document_reporting > 0, TRUE, FALSE)) %>%
  group_by(Issue.Area.Grantees ) %>%
  summarize(counts = sum(document_ind))  %>% 
  as.matrix()


## Members of Networks or Coalitions ##

gms_member_networks <- gms %>% mutate(networks_coalitions_member = (Member.of.International.Coalition == "Yes" |
                                                                      Member.of.SubNational.Network == "Yes" |
                                                                      Member.of.International.Network == "Yes" |
                                                                      Member.of.National.Network == "Yes" |
                                                                      Member.of.SubNational.Coalition == "Yes" |
                                                                      Member.of.National.Coalition == "Yes")) %>%
  select(Issue.Area.Grantees, networks_coalitions_member, Organization.ID) %>%
  filter(!is.na(networks_coalitions_member)) %>%
  group_by(Issue.Area.Grantees, Organization.ID ) %>%
  summarize(network_coalition = sum(networks_coalitions_member),
            network_ind = ifelse(network_coalition > 0, TRUE, FALSE)) %>%
  group_by(Issue.Area.Grantees ) %>%
  summarize(counts = sum(network_ind))  %>% 
  as.matrix()


## Leaders of Networks or Coalitions ##

gms_lead_networks <- gms %>% mutate(networks_coalitions_leader =  (Leader.of.International.Coalition == "Yes" |
                                                                     Leader.of.International.Network == "Yes" |
                                                                     Leader.of.National.Coalition == "Yes" |
                                                                     Leader.of.National.Network == "Yes" |
                                                                     Leader.of.SubNational.Coalition == "Yes" |
                                                                     Leader.of.SubNational.Network == "Yes")) %>%
  select(Issue.Area.Grantees, networks_coalitions_leader, Organization.ID) %>%
  filter(!is.na(networks_coalitions_leader)) %>%
  group_by(Issue.Area.Grantees, Organization.ID ) %>%
  summarize(network_coalition = sum(networks_coalitions_leader),
            network_ind = ifelse(network_coalition > 0, TRUE, FALSE)) %>%
  group_by(Issue.Area.Grantees ) %>%
  summarize(counts = sum(network_ind))  %>% 
  as.matrix()


## Local Organizations ##

gms_local_orgs <- gms %>% mutate(local_organizations = recode(Org.Work.Level, "Sub-National" = "local",
                                                              "Local" = "local",
                                                              "Local and Sub-National" = "local",
                                                              "Local and National" = "local and national",
                                                              "National" = "national",
                                                              "Global" = "international",
                                                              "Regional" = "international")) %>%
  select(Issue.Area.Grantees, local_organizations, Organization.ID) %>%
  filter(local_organizations == "local") %>%
  group_by(Issue.Area.Grantees, Organization.ID) %>%
  count() %>%
  group_by(Issue.Area.Grantees, .drop = FALSE) %>%
  summarize(counts = n())  %>% 
  as.matrix()


## Local and National Organizations ##

gms_local_national_orgs <- gms %>% mutate(local_national_organizations = recode(Org.Work.Level, "Sub-National" = "local",
                                                                                "Local" = "local",
                                                                                "Local and Sub-National" = "local",
                                                                                "Local and National" = "local and national",
                                                                                "National" = "national",
                                                                                "Global" = "international",
                                                                                "Regional" = "international")) %>%
  select(Issue.Area.Grantees, local_national_organizations, Organization.ID) %>%
  filter(local_national_organizations == "local and national") %>%
  group_by(Issue.Area.Grantees, Organization.ID) %>%
  count %>%
  group_by(Issue.Area.Grantees, .drop = FALSE) %>%
  summarize(counts = n())  %>% 
  as.matrix()


## National Organizations ##

gms_national_orgs <- gms %>% mutate(national_organizations = recode(Org.Work.Level, "Sub-National" = "local",
                                                                    "Local" = "local",
                                                                    "Local and Sub-National" = "local",
                                                                    "Local and National" = "local and national",
                                                                    "National" = "national",
                                                                    "Global" = "international",
                                                                    "Regional" = "international")) %>%
  select(Issue.Area.Grantees, national_organizations, Organization.ID) %>%
  filter(national_organizations == "national") %>%
  group_by(Issue.Area.Grantees, Organization.ID) %>%
  count %>%
  group_by(Issue.Area.Grantees, .drop = FALSE) %>%
  summarize(counts = n())  %>% 
  as.matrix()


## Global Organizations ##

gms_global_orgs <- gms %>% mutate(global_organizations = recode(Org.Work.Level, "Sub-National" = "local",
                                                                "Local" = "local",
                                                                "Local and Sub-National" = "local",
                                                                "Local and National" = "local and national",
                                                                "National" = "national",
                                                                "Global" = "international",
                                                                "Regional" = "international")) %>%
  select(Issue.Area.Grantees, global_organizations, Organization.ID) %>%
  filter(global_organizations == "international") %>%
  group_by(Issue.Area.Grantees, Organization.ID) %>%
  count %>%
  group_by(Issue.Area.Grantees, .drop = FALSE) %>%
  summarize(counts = n())  %>% 
  as.matrix()

## Create Dashboard ##

dashboard_text <- matrix(nrow = 27, ncol = ncol_dashboard)
dashboard_text[1,] <- gms_nrow_by_issue_area[,1]
dashboard_text[2,] <- "#"
dashboard_text[3,] <- icc[,2] %>% sum %>% ceiling ## ALWAYS CHECK THAT THIS EXCEL SHEET DOES NOT HAVE A SUM AT THE BOTTOM
dashboard_text[4,] <- gms_new_orgs[,2] %>% str_replace(" ", "") # PLACEHOLDER PLEASE CHANGE #gms_capacity_building[,2] %>% str_replace(" ", "")
dashboard_text[5,] <- gms_new_orgs[,2] %>% str_replace(" ", "")
dashboard_text[6,] <- gms_advocacy_engaged[,2] %>% str_replace(" ", "")
dashboard_text[7,] <- gms_legal_services[,2] %>% str_replace(" ", "")
dashboard_text[8,] <- gms_document_reporting[,2] %>% str_replace(" ", "")
dashboard_text[9,] <- gms_member_networks[,2] %>% str_replace(" ", "")
dashboard_text[10,] <- gms_lead_networks[,2] %>% str_replace(" ", "")
dashboard_text[11,] <- gms_local_orgs[,2] %>% str_replace(" ", "")
dashboard_text[12,] <- gms_local_national_orgs[,2] %>% str_replace(" ", "")
dashboard_text[13,] <- gms_national_orgs[,2] %>% str_replace(" ", "")
dashboard_text[14,] <- gms_global_orgs[,2] %>% str_replace(" ", "")
dashboard_text[15,] <- paste0("% [N=", gms_nrow_by_issue_area[,2], "]") %>% str_replace("= ", "=")
dashboard_text[16,] <- paste0((icc[,2] %>% sum %>% ceiling/(icc[,2] %>% sum %>% ceiling)*100), "%\n[N=",icc[,2] %>% sum %>% ceiling,"]")
dashboard_text[17,] <- paste0(round(as.numeric(gms_new_orgs[,2])/as.numeric(gms_new_nrow_by_issue_area[,2]) * 100), "%") # PLACEHOLDER PLEASE CHANGE #paste0(round(as.numeric(gms_capacity_building[,2])/as.numeric(gms_nrow_by_issue_area[,2]) * 100), "%")
dashboard_text[18,] <- paste0(round(as.numeric(gms_new_orgs[,2])/as.numeric(gms_new_nrow_by_issue_area[,2]) * 100), "%")
dashboard_text[19,] <- paste0(round(as.numeric(gms_advocacy_engaged[,2])/as.numeric(gms_nrow_by_issue_area[,2]) * 100), "%")
dashboard_text[20,] <- paste0(round(as.numeric(gms_legal_services[,2])/as.numeric(gms_nrow_by_issue_area[,2]) * 100), "%")
dashboard_text[21,] <- paste0(round(as.numeric(gms_document_reporting[,2])/as.numeric(gms_nrow_by_issue_area[,2]) * 100), "%")
dashboard_text[22,] <- paste0(round(as.numeric(gms_member_networks[,2])/as.numeric(gms_nrow_by_issue_area[,2]) * 100), "%")
dashboard_text[23,] <- paste0(round(as.numeric(gms_lead_networks[,2])/as.numeric(gms_nrow_by_issue_area[,2]) * 100), "%")
dashboard_text[24,] <- paste0(round(as.numeric(gms_local_orgs[,2])/as.numeric(gms_nrow_by_issue_area[,2]) * 100), "%")
dashboard_text[25,] <- paste0(round(as.numeric(gms_local_national_orgs[,2])/as.numeric(gms_nrow_by_issue_area[,2]) * 100), "%")
dashboard_text[26,] <- paste0(round(as.numeric(gms_national_orgs[,2])/as.numeric(gms_nrow_by_issue_area[,2]) * 100), "%")
dashboard_text[27,] <- paste0(round(as.numeric(gms_global_orgs[,2])/as.numeric(gms_nrow_by_issue_area[,2]) * 100), "%")


reordered_text <- cbind(dashboard_text[3:14,1], dashboard_text[16:27,1], dashboard_text[3:14,2], dashboard_text[16:27,2], dashboard_text[3:14,3], dashboard_text[16:27,3], dashboard_text[3:14,4], dashboard_text[16:27,4], dashboard_text[3:14,5], dashboard_text[16:27,5])

sketch <- htmltools::withTags(
  table(
    class = 'display',
    thead(
      tr(
        th(rowspan = 2, "FY25 Dashboard Indicators", style = "background-color:#f2f2f2;"),
        th(colspan = 2, "CPR", style = "background-color:#f2f2f2;"),
        th(colspan = 2, "HR", style = "background-color:#f2f2f2;"),
        th(colspan = 2, "LWCJ", style = "background-color:#f2f2f2;"),
        th(colspan = 2, "SHR (ECM)", style = "background-color:#f2f2f2;"),
        th(colspan = 2, "SHR (Non-ECM)", style = "background-color:#f2f2f2;")
      ),
      tr(
        th(dashboard_text[2,1], style = "background-color:#f2f2f2;"),
        th(dashboard_text[15,1], style = "background-color:#f2f2f2;"),
        th(dashboard_text[2,2], style = "background-color:#f2f2f2;"),
        th(dashboard_text[15,2], style = "background-color:#f2f2f2;"),
        th(dashboard_text[2,3], style = "background-color:#f2f2f2;"),
        th(dashboard_text[15,3], style = "background-color:#f2f2f2;"),
        th(dashboard_text[2,4], style = "background-color:#f2f2f2;"),
        th(dashboard_text[15,4], style = "background-color:#f2f2f2;"),
        th(dashboard_text[2,5], style = "background-color:#f2f2f2;"),
        th(dashboard_text[15,5], style = "background-color:#f2f2f2;")
      )
    )
  )
)

tile_text = c(paste0("FY25", " Dashboard Indicators"),
              "In-country staff and consultants providing grantees with coaching, training and \nsupport to strengthen their work",
              "Grantees that received grants to develop or strengthen knowledge, skills or practices",
              "Grantees that are new organizations founded within 5 years of their first AJWS \ngrant",
              "Grantees engaged in advocacy",
              "Grantees that received grants to provide legal aid, education and assistance",
              "Grantees that received grants to investigate, document and report human rights \nabuses",
              "Grantees that are members of networks or coalitions",
              "Grantees that are leaders of networks or coalitions", 
              "Grantees that are local organizations",
              "Grantees that are local and national organizations",
              "Grantees that are national organizations", 
              "Grantees that are international organizations")

full_text <- cbind(tile_text[2:13], reordered_text)

full_text[,1] <- str_squish(gsub("\n", " ", full_text[,1]))



progress_levels <- c("Major Regress", "Moderate Regress", "Minor Regress",
                     "No Progress",
                     "Minor Progress", "Moderate Progress", "Major Progress")

context_levels <- c("Very unfavorable", "Somewhat unfavorable", "Neutral",
                    "Somewhat favorable", "Very favorable")

ui <- page_sidebar(
  title = "2025 AJWS Data Exploration Tool",
  theme = ajws_theme,
  sidebar = sidebar(
    tags$img(src = "logo.png", style = "max-width: 100%; height: auto; margin-bottom: 1rem;"),
    div(
      style = "display: flex; flex-direction: column; gap: 10px;",
      actionButton("go_home", "🏠 Home", class = "btn btn-outline-primary"),
      actionButton("go_heatmap", "📊 OMF Heatmap", class = "btn btn-outline-primary"),
      actionButton("go_omf_data_explorer", "🧭 OMF Data Explorer", class = "btn btn-outline-primary"),
      actionButton("go_omf_longitudinal", "📈 OMF Longitudinal", class = "btn btn-outline-primary"),
      actionButton("go_gms_dashboard", "📊 GMS Dashboard", class = "btn btn-outline-primary"),
      actionButton("go_gms_map", "🗺️ GMS Mapping", class = "btn btn-outline-primary"),
      actionButton("go_gms_data_explorer", "🧭 GMS Data Explorer", class = "btn btn-outline-primary")
    )
  
  ),
  uiOutput("main_ui")
)

server <- function(input, output, session) {
  current_page <- reactiveVal("Home")
  
  observeEvent(input$go_home, current_page("Home"))
  observeEvent(input$go_heatmap, current_page("OMF Heatmap"))
  observeEvent(input$go_omf_data_explorer, current_page("OMF Data Explorer"))
  observeEvent(input$go_omf_longitudinal, current_page("OMF Longitudinal"))
  observeEvent(input$go_gms_dashboard, current_page("GMS Dashboard"))
  observeEvent(input$go_gms_map, current_page("GMS Mapping"))
  observeEvent(input$go_gms_data_explorer, current_page("GMS Data Explorer"))
  
  # Reactive GMS dataset
  gms_cleaned <- reactive({
    req(full_data())
    df <- read.xlsx("FY25 AJWS Data_3JUL25.xlsx", sheet = "GMS FY25 1") %>%
      mutate(
        Region = Region,
        GranteeRegion = Region.Grantee,
        GeoArea = Geographical.Area.Served.Description,
        IssueGrants = Issue.Area.Grants,
        IssueGrantees = Issue.Area.Grantees,
        GenderFocus = Org.Gender.Focus,
        WorkFocus = Org.Work.Focus
      )
    df
  })
  
  
  # Render the datatable
  output$gms_explorer_table <- renderDT({
    df <- gms_cleaned()
    
    if (input$gms_region != "All") {
      df <- df %>% filter(Region == input$gms_region)
    }
    if (input$gms_grantee_region != "All") {
      df <- df %>% filter(GranteeRegion == input$gms_grantee_region)
    }
    if (input$gms_geo_area != "All") {
      df <- df %>% filter(GeoArea == input$gms_geo_area)
    }
    if (input$gms_issue_grants != "All") {
      df <- df %>% filter(IssueGrants == input$gms_issue_grants)
    }
    if (input$gms_issue_grantees != "All") {
      df <- df %>% filter(IssueGrantees == input$gms_issue_grantees)
    }
    
    df %>%
      select(
        Region,
        GranteeRegion,
        GeoArea,
        IssueGrants,
        IssueGrantees,
        GenderFocus,
        WorkFocus
      ) %>%
      datatable(
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
  })
  
  
  # Reactive for cleaned OMF data
  omf_cleaned <- reactive({
    df <- read.xlsx("FY25 AJWS Data_3JUL25.xlsx", sheet = "OMF")
    
    # Check names if needed:
    # print(names(df))
    
    df <- df %>%
      filter(Update.Type %in% c("Milestone", "Outcome")) %>%
      mutate(
        CorePillar = ifelse(Update.Type == "Outcome", Outcome.Category, Milestone.Category),
        ThematicArea = OMF.Strategy,
        Country = OMF.Country,
        Progress = Change,
        Year = Update.Year
      ) %>%
      filter(!is.na(Progress), !is.na(Year), Year != 2025)
    
    df
  })
  
  
  # Update dropdowns
  
  # print(head(df))
  # print(unique(df$CorePillar))
  
  
  # Reactive data
  filtered_long_data <- reactive({
    df <- omf_cleaned()
    
    if (input$long_record_type != "All") {
      df <- df %>% filter(Update.Type == input$long_record_type)
    }
    if (input$long_core_pillar != "All") {
      df <- df %>% filter(CorePillar == input$long_core_pillar)
    }
    if (input$long_thematic_area != "All") {
      df <- df %>% filter(ThematicArea == input$long_thematic_area)
    }
    if (input$long_country != "All") {
      df <- df %>% filter(Country == input$long_country)
    }
    
    df
  })
  
  # Render chart
  output$longitudinal_chart <- renderPlot({
    df <- filtered_long_data()
    
    progress_by_year <- df %>%
      group_by(Year, Progress) %>%
      summarize(Count = n(), .groups = "drop")
    
    ggplot(progress_by_year, aes(x = Year, y = Count, color = Progress, group = Progress)) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      labs(
        title = "Progress Over the 2023–2026 Strategy Period",
        x = "Update Year",
        y = "Count of Updates",
        color = "Progress"
      ) +
      theme_minimal(base_size = 14)
  })
  

  
  raw_data <- reactive({
    read.xlsx("FY25 AJWS Data_3JUL25.xlsx", sheet = "OMF") %>%
      filter(Update.Year == 2024, Update.Type %in% c("Outcome", "Milestone")) %>%
      mutate(
        CorePillar = case_when(
          Update.Type == "Outcome" ~ Outcome.Category,
          Update.Type == "Milestone" ~ Milestone.Category,
          TRUE ~ NA_character_
        ),
        ThematicArea = OMF.Strategy,
        Progress = Change,
        Context = Context,
        RecordType = Update.Type,
        Country = OMF.Country
      ) %>%
      select(Progress, Context, CorePillar, RecordType, ThematicArea, Country)
  })
  
  full_data <- reactive({
    df <- raw_data()
    df$Progress <- factor(df$Progress, levels = progress_levels, ordered = TRUE)
    df$Context <- factor(df$Context, levels = context_levels, ordered = TRUE)
    
    df %>%
      filter(!is.na(Progress), !is.na(Context), !is.na(CorePillar),
             !is.na(RecordType), !is.na(ThematicArea))
  })
  
  
  output$main_ui <- renderUI({
    data <- full_data()
    
    record_type_choices <- c("All Record Types", sort(unique(data$RecordType)))
    
    all_pillars <- read.xlsx("FY25 AJWS Data_3JUL25.xlsx", sheet = "OMF") %>%
      mutate(
        CorePillar = case_when(
          Update.Type == "Outcome" ~ Outcome.Category,
          Update.Type == "Milestone" ~ Milestone.Category,
          TRUE ~ NA_character_
        )) %>%
      pull(CorePillar) %>%
      unique() %>%
      na.omit() %>%
      sort()
    
    pillar_choices <- c("All Core Pillars", all_pillars)
    
    
    all_themes <- read.xlsx("FY25 AJWS Data_3JUL25.xlsx", sheet = "OMF") %>%
      pull(OMF.Strategy) %>%
      unique() %>%
      na.omit() %>%
      sort()
    
    theme_choices <- c("All Thematic Areas", all_themes)
  
    all_countries <- read.xlsx("FY25 AJWS Data_3JUL25.xlsx", sheet = "OMF") %>%
      pull(OMF.Country) %>%
      unique() %>%
      na.omit() %>%
      sort()
    
    country_choices <- c("All Countries", all_countries)
    
    
    observe({
      df <- full_data()
      updateSelectInput(session, "omf_record_type", choices = c("All", sort(unique(df$RecordType))))
      updateSelectInput(session, "omf_core_pillar", choices = c("All", sort(unique(df$CorePillar))))
      updateSelectInput(session, "omf_thematic_area", choices = c("All", sort(unique(df$ThematicArea))))
      updateSelectInput(session, "omf_country", choices = c("All", sort(unique(df$Country))))
      updateSelectInput(session, "omf_progress", choices = c("All", progress_levels))
      updateSelectInput(session, "omf_context", choices = c("All", context_levels))
    })
    
    observeEvent(input$reset_omf_explorer, {
      updateSelectInput(session, "omf_record_type", selected = "All")
      updateSelectInput(session, "omf_core_pillar", selected = "All")
      updateSelectInput(session, "omf_thematic_area", selected = "All")
      updateSelectInput(session, "omf_country", selected = "All")
      updateSelectInput(session, "omf_progress", selected = "All")
      updateSelectInput(session, "omf_context", selected = "All")
    })
    
    # Update selectInput choices based on data
    observe({
      df <- gms_cleaned()
      updateSelectInput(session, "gms_region", choices = c("All", sort(unique(df$Region))))
      updateSelectInput(session, "gms_grantee_region", choices = c("All", sort(unique(df$GranteeRegion))))
      updateSelectInput(session, "gms_geo_area", choices = c("All", sort(unique(df$GeoArea))))
      updateSelectInput(session, "gms_issue_grants", choices = c("All", sort(unique(df$IssueGrants))))
      updateSelectInput(session, "gms_issue_grantees", choices = c("All", sort(unique(df$IssueGrantees))))
    })
    
    # Reset filters
    observeEvent(input$gms_reset_filters, {
      updateSelectInput(session, "gms_region", selected = "All")
      updateSelectInput(session, "gms_grantee_region", selected = "All")
      updateSelectInput(session, "gms_geo_area", selected = "All")
      updateSelectInput(session, "gms_issue_grants", selected = "All")
      updateSelectInput(session, "gms_issue_grantees", selected = "All")
    })
    
    observe({
      df <- omf_cleaned()
      req(nrow(df) > 0)
      
      updateSelectInput(session, "long_core_pillar", choices = c("All", sort(unique(df$CorePillar))))
      updateSelectInput(session, "long_thematic_area", choices = c("All", sort(unique(df$ThematicArea))))
      updateSelectInput(session, "long_country", choices = c("All", sort(unique(df$Country))))
    })
    
    
    
    # Reset filters
    observeEvent(input$long_reset_filters, {
      updateSelectInput(session, "long_record_type", selected = "All")
      updateSelectInput(session, "long_core_pillar", selected = "All")
      updateSelectInput(session, "long_thematic_area", selected = "All")
      updateSelectInput(session, "long_country", selected = "All")
    })
    
    
    switch(current_page(),
           "Home" = fluidRow(
             column(12,
                    h3("Welcome to the 2025 AJWS Data Exploration Tool"),
                    p("This interactive platform is designed to help you explore metrics and trends across the Outcome Monitoring Form (OMF) and the Grants Management System (GMS)."),
                    tags$ul(
                      tags$li("Use the sidebar to navigate between sections."),
                      tags$li("Explore heatmaps showing progress and context using filters for Record Type, Core Pillar, and Thematic Area."),
                      tags$li("Hover over any heatmap cell to see detailed values."),
                      tags$li("More features and datasets will be added in future releases.")
                    ),
                    p("For questions, please contact your Data Team.")
             )
           ),
           "OMF Heatmap" = fluidPage(
             fluidRow(
               column(
                 width = 3,  # Sidebar column
                 wellPanel(
                   selectInput("record_type_select", "Select Record Type:", choices = record_type_choices, selected = "All Record Types"),
                   selectInput("pillar_select", "Select Core Pillar:", choices = pillar_choices, selected = "All Core Pillars"),
                   selectInput("theme_select", "Select Thematic Area:", choices = theme_choices, selected = "All Thematic Areas"),
                   selectInput("country_select", "Select Country:", 
                               choices = country_choices, 
                               selected = "All Countries"),
                   actionButton("reset_filters", "Reset All Filters", class = "btn btn-warning")
                 ),
                 br(),
                 wellPanel(
                   h5("📌 Notes"),
                   p("This is a placeholder for any contextual notes related to the selected filters and displayed data. You can update this area with dynamic or static content.")
                 )
               ),
               column(
                 width = 9,  # Main panel
                 plotlyOutput("heatmap"),
                 br(),
                 DTOutput("summary_by_progress"),
                 br(),
                 DTOutput("summary_by_context")
               )
             )
           ),
           
           
           "OMF Data Explorer" = fluidPage(
             sidebarLayout(
               sidebarPanel(
                 selectInput("omf_record_type", "Record Type", choices = NULL),
                 selectInput("omf_core_pillar", "Core Pillar", choices = NULL),
                 selectInput("omf_thematic_area", "Thematic Area", choices = NULL),
                 selectInput("omf_country", "Country", choices = NULL),
                 selectInput("omf_progress", "Progress", choices = NULL),
                 selectInput("omf_context", "Context", choices = NULL),
                 actionButton("reset_omf_explorer", "Reset Filters", class = "btn btn-warning")
               ),
               mainPanel(
                 h3("OMF Data Explorer 🧭"),
                 p("Explore OMF entries below. Records are dynamically selected based on the filters selected."),
                 DTOutput("omf_explorer_table")
               )
             )
           ),
           "OMF Longitudinal" = fluidPage(
             sidebarLayout(
               sidebarPanel(
                 selectInput("long_record_type", "Select Record Type:", 
                             choices = c("All", "Milestone", "Outcome"), selected = "All"),
                 selectInput("long_core_pillar", "Select Core Pillar:", 
                             choices = NULL, selected = "All"),
                 selectInput("long_thematic_area", "Select Thematic Area:", 
                             choices = NULL, selected = "All"),
                 selectInput("long_country", "Select Country:", 
                             choices = NULL, selected = "All"),
                 actionButton("long_reset_filters", "Reset Filters", class = "btn btn-warning")
               ),
               mainPanel(
                 plotOutput("longitudinal_chart", height = "600px")
               )
             )
           ),
           "GMS Dashboard" = fluidRow(
             column(12,
                    h3("AJWS Grants by the Numbers"),
                    p("Summary of grantee characteristics across issue areas based on GMS data."),
                    DTOutput("gms_dashboard")
             )
           ),
           "GMS Mapping" = fluidPage(
             titlePanel("GMS Mapping"),
             p("Hover over each pie chart to view regional grant percentages."),
             leafletOutput("gms_map", height = "700px")
           ),
           "GMS Data Explorer" = fluidPage(
             titlePanel("GMS Data Explorer"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("gms_region", "Region:", choices = NULL),
                 selectInput("gms_grantee_region", "Grantee Region:", choices = NULL),
                 selectInput("gms_geo_area", "Geographical Area Served:", choices = NULL),
                 selectInput("gms_issue_grants", "Issue Area (Grants):", choices = NULL),
                 selectInput("gms_issue_grantees", "Issue Area (Grantees):", choices = NULL),
                 actionButton("gms_reset_filters", "Reset Filters", class = "btn btn-warning")
               ),
               mainPanel(
                 DTOutput("gms_explorer_table")
               )
             )
           )
           
           
    )
  })
  
  observeEvent(input$reset_filters, {
    updateSelectInput(session, "record_type_select", selected = "All Record Types")
    updateSelectInput(session, "pillar_select", selected = "All Core Pillars")
    updateSelectInput(session, "theme_select", selected = "All Thematic Areas")
    updateSelectInput(session, "country_select", selected = "All Countries")
  })
  
  ### RENDER OMF HEATMAP ###
  
  output$heatmap <- renderPlotly({
    filter_parts <- c()
    if (!is.null(input$record_type_select) && input$record_type_select != "All Record Types") {
      filter_parts <- c(filter_parts, input$record_type_select)
    }
    if (!is.null(input$pillar_select) && input$pillar_select != "All Core Pillars") {
      filter_parts <- c(filter_parts, input$pillar_select)
    }
    if (!is.null(input$theme_select) && input$theme_select != "All Thematic Areas") {
      filter_parts <- c(filter_parts, input$theme_select)
    }
    if (!is.null(input$country_select) && input$country_select != "All Countries") {
      filter_parts <- c(filter_parts, input$country_select)
    }
    
    final_title <- if (length(filter_parts) > 0) {
      paste("OMF Heatmap –", paste(filter_parts, collapse = " | "))
    } else {
      "OMF Heatmap"
    }
    
    data <- full_data()
    if (input$record_type_select != "All Record Types") {
      data <- data %>% filter(RecordType == input$record_type_select)
    }
    if (input$pillar_select != "All Core Pillars") {
      data <- data %>% filter(CorePillar == input$pillar_select)
    }
    if (input$theme_select != "All Thematic Areas") {
      data <- data %>% filter(ThematicArea == input$theme_select)
    }
    if (input$country_select != "All Countries") {
      data <- data %>% filter(Country == input$country_select)
    }
    total_obs <- nrow(data)
    validate(need(total_obs > 0, "No data available."))
    
    heat_df <- data %>%
      count(Progress, Context) %>%
      complete(Progress = factor(progress_levels, levels = progress_levels, ordered = TRUE),
               Context = factor(context_levels, levels = context_levels, ordered = TRUE),
               fill = list(n = 0)) %>%
      mutate(Percent = n / total_obs)
    
    gg <- ggplot(heat_df, aes(x = Context, y = Progress, fill = Percent,
                              text = paste0("Progress: ", Progress, "\n",
                                            "Context: ", Context, "\n",
                                            "Percent: ", percent(Percent, accuracy = 0.1)))) +
      geom_tile(color = "white") +
      geom_text(aes(label = percent(Percent, accuracy = 0.1)), size = 3.5) +
      scale_fill_gradient(low = "#fefcf5", high = ajws_accent, name = "% of total") +
      scale_x_discrete(position = "top") +
      scale_y_discrete(limits = progress_levels) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 30, hjust = 0),
        panel.grid = element_blank()
      ) +
      labs(
        title = final_title,
        x = "Context",
        y = "Progress"
      )
    
    ggplotly(gg, tooltip = "text")
  })
  
  output$summary_by_progress <- renderDT({
    data <- raw_data()
    if (input$record_type_select != "All Record Types") {
      data <- data %>% filter(RecordType == input$record_type_select)
    }
    if (input$pillar_select != "All Core Pillars") {
      data <- data %>% filter(CorePillar == input$pillar_select)
    }
    if (input$theme_select != "All Thematic Areas") {
      data <- data %>% filter(ThematicArea == input$theme_select)
    }
    if (input$country_select != "All Countries") {
      data <- data %>% filter(Country == input$country_select)
    }
    
    progress_data <- data %>% filter(!is.na(Progress))
    total_progress <- nrow(progress_data)
    validate(need(total_progress > 0, "No progress data available."))
    
    progress_data %>%
      count(Progress) %>%
      mutate(Percent = percent(n / total_progress, accuracy = 0.1)) %>%
      datatable(options = list(dom = 't'), rownames = FALSE)
  })
  
  
  
  output$summary_by_context <- renderDT({
    data <- raw_data()
    if (input$record_type_select != "All Record Types") {
      data <- data %>% filter(RecordType == input$record_type_select)
    }
    if (input$pillar_select != "All Core Pillars") {
      data <- data %>% filter(CorePillar == input$pillar_select)
    }
    if (input$theme_select != "All Thematic Areas") {
      data <- data %>% filter(ThematicArea == input$theme_select)
    }
    if (input$country_select != "All Countries") {
      data <- data %>% filter(Country == input$country_select)
    }
    
    context_data <- data %>% filter(!is.na(Context))
    total_context <- nrow(context_data)
    validate(need(total_context > 0, "No context data available."))
    
    context_data %>%
      count(Context) %>%
      mutate(Percent = percent(n / total_context, accuracy = 0.1)) %>%
      datatable(options = list(dom = 't'), rownames = FALSE)
  })
  
    # Coordinates for each region's pie chart
    coords <- data.frame(
      region = c("Americas", "Africa", "Asia", "Asia ECM", "Cross-Regional"),
      lat = c(23.91, 17.34, 36.68, 36.68, 35),  # adjust as needed
      lng = c(-102.22, 9.27, 103.45, 103.45, -40)
    )
  
  # Pie data (make sure column names match coords$region order)
  pie_data <- data.frame(
    americas = 28.99,
    africa = 26.44,
    asia = 35.67,
    asia_ecm = 0.70,
    cross_regional = 8.20
  )
  
  # Transpose pie data so each row matches one region
  pie_matrix <- matrix(as.numeric(pie_data), nrow = 1)
  colnames(pie_matrix) <- names(pie_data)
  
  # Step 1: Calculate total per row
  totals <- rowSums(pie_data)
  
  # Step 2: Normalize the totals (e.g., scale from 0 to 1)
  norm_totals <- (totals - min(totals)) / (max(totals) - min(totals))
  
  # Step 3: Map to a desired size range (e.g., 30px to 80px)
  min_size <- 30
  max_size <- 80
  sizes <- min_size + norm_totals * (max_size - min_size)
  
  
  # Create the map
  output$gms_map <- renderLeaflet({ leaflet() %>%
    addTiles() %>%
    
    # Add pie charts
    addMinicharts(
      lng = coords$lng,
      lat = coords$lat,
      chartdata = t(pie_data),
      type = "pie",
      width = 60,
      height = 60,
      showLabels = FALSE
    ) %>%
    
    # Add invisible markers with hover labels for tooltips
    addCircleMarkers(
      lng = coords$lng,
      lat = coords$lat,
      radius = 20,  # <- bigger hover area
      stroke = FALSE,  # no outline
      fillOpacity = 0,  # fully invisible
      label = lapply(1:nrow(coords), function(i) {
        htmltools::HTML(paste0(
          "<strong>", coords$region[i], ":</strong> ",
          round(pie_data[[i]], 1), "%"
        ))
      }),
      labelOptions = labelOptions(
        direction = "auto",
        opacity = 1,
        textsize = "13px",
        style = list(
          "background-color" = "white",
          "border" = "1px solid gray",
          "padding" = "5px",
          "border-radius" = "4px")))
          
  })
  
  
  ### RENDER OMF DATA EXPLORER TABLE ###
  output$omf_explorer_table <- renderDT({
    df <- read.xlsx("FY25 AJWS Data_3JUL25.xlsx", sheet = "OMF") %>%
      filter(Update.Year == 2024, Update.Type %in% c("Outcome", "Milestone")) %>%
      mutate(
        Number = ifelse(Update.Type == "Outcome", Outcome.Number, Milestone.Number),
        Name = ifelse(Update.Type == "Outcome", Outcome.Name, Milestone.Name),
        CorePillar = ifelse(Update.Type == "Outcome", Outcome.Category, Milestone.Category),
        ThematicArea = OMF.Strategy,
        Country = OMF.Country,
        Progress = Change,
        Context = Context,
        RecordType = Update.Type
      ) %>%
      select(Number, Name, Progress, Context, Country, RecordType, CorePillar, ThematicArea)
    
    if (input$omf_record_type != "All") {
      df <- df %>% filter(RecordType == input$omf_record_type)
    }
    if (input$omf_core_pillar != "All") {
      df <- df %>% filter(CorePillar == input$omf_core_pillar)
    }
    if (input$omf_thematic_area != "All") {
      df <- df %>% filter(ThematicArea == input$omf_thematic_area)
    }
    if (input$omf_country != "All") {
      df <- df %>% filter(Country == input$omf_country)
    }
    if (input$omf_progress != "All") {
      df <- df %>% filter(Progress == input$omf_progress)
    }
    if (input$omf_context != "All") {
      df <- df %>% filter(Context == input$omf_context)
    }
    
    colnames(df) <- c(
      "Number", "Name", "Progress", "Context", 
      "Country", "Record Type", "Core Pillar", "Thematic Area"
    )
    
    datatable(df, options = list(pageLength = 10), rownames = FALSE)
  })
  
  
  
  
  unique(full_text[,1])
  
  output$gms_dashboard <- renderDT({
    datatable(
      full_text,
      container = sketch,
      rownames = FALSE,
      selection = 'none',
      options = list(
        dom = 't',
        ordering = FALSE,
        pageLength = nrow(full_text),
        rowCallback = JS(
          "function(row, data, index) {",
          "  const label = data[0];",
          "  if (label.includes('coaching, training')) {",
          "    $('td', row).css('background-color', '#BAEDF4');",
          "  } else if (label.includes('develop or strengthen knowledge')) {",
          "    $('td', row).css('background-color', '#BAEDF4');",
          "  } else if (label.includes('new organizations founded')) {",
          "    $('td', row).css('background-color', '#BAEDF4');",
          "  } else if (label.includes('engaged in advocacy')) {",
          "    $('td', row).css('background-color', '#F2B09F');",
          "  } else if (label.includes('provide legal aid')) {",
          "    $('td', row).css('background-color', '#F2B09F');",
          "  } else if (label.includes('document and report')) {",
          "    $('td', row).css('background-color', '#F2B09F');",
          "  } else if (label.includes('members of networks')) {",
          "    $('td', row).css('background-color', '#FFDC95');",
          "  } else if (label.includes('leaders of networks')) {",
          "    $('td', row).css('background-color', '#FFDC95');",
          "  } else if (label.includes('local organizations')) {",
          "    $('td', row).css('background-color', '#FFDC95');",
          "  } else if (label.includes('local and national')) {",
          "    $('td', row).css('background-color', '#FFDC95');",
          "  } else if (label.includes('national organizations')) {",
          "    $('td', row).css('background-color', '#FFDC95');",
          "  } else if (label.includes('international organizations')) {",
          "    $('td', row).css('background-color', '#FFDC95');",
          "  }",
          "}"
        )
      )
    ) %>%
      formatStyle(columns = names(full_text), color = "black")
  })
  
}

shinyApp(ui = ui, server = server)

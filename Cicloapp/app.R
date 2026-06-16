library(shiny)
library(lubridate)
library(dplyr)
library(rmarkdown)
library(httr)

#SETTINGS#######################################################################
FORM_URL <- "YOUR_FORM_RESPONSE_URL" #example "https://docs.google.com/forms/xxxxxx"
CSV_URL <- "YOUR_SHEET_CSV_URL" #example "https://docs.google.com/spreadsheets/xxxx"

DATE_ENTRY <- "entry.111111"
ACTION_ENTRY <- "entry.222222"

APP_PASSWORD <- "examplepassword"

#Appp ##########################################################################
base_cols <- c(
  "#C969A1FF", "#CE4441FF", "#EE8577FF",
  "#EB7926FF", "#FFBB44FF", "#859B6CFF",
  "#62929AFF", "#004F63FF", "#122451FF"
)

read_data <- function() {
  empty <- data.frame(date = as.Date(character()))
  
  data <- tryCatch(
    read.csv(
      paste0(CSV_URL, "&t=", as.numeric(Sys.time())),
      check.names = FALSE,
      stringsAsFactors = FALSE
    ),
    error = function(e) empty
  )
  
  if (nrow(data) == 0) return(empty)
  
  names(data) <- trimws(names(data))
  
  if (!"date" %in% names(data)) return(empty)
  if (!"action" %in% names(data)) data$action <- "add"
  
  data <- data %>%
    mutate(
      row_id = row_number(),
      date = as.Date(parse_date_time(as.character(date), orders = c("ymd", "dmy", "mdy"))),
      action = tolower(trimws(as.character(action)))
    ) %>%
    filter(!is.na(date))
  
  data %>%
    group_by(date) %>%
    slice_max(row_id, n = 1) %>%
    ungroup() %>%
    filter(action == "add") %>%
    select(date) %>%
    arrange(date)
}

ui <- fluidPage(
  titlePanel("Menstrual Calendar"),
  
  sidebarLayout(
    sidebarPanel(
      dateInput(
        "date",
        "First day of period",
        value = Sys.Date(),
        format = "dd/mm/yyyy",
        language = "en"
      ),
      actionButton("save", "Save period"),
      br(), br(),
      selectInput("delete_date", "Delete period", choices = NULL),
      actionButton("delete", "Delete selected period"),
      br(), br(),
      downloadButton("export", "Export results"),
      br(), br(),
      actionButton("previous_month", "< Previous month"),
      actionButton("next_month", "Next month")
    ),
    
    mainPanel(
      passwordInput("password", "Password"),
      uiOutput("secure_app")
    )
  )
)

server <- function(input, output, session) {
  
  current_month <- reactiveVal(floor_date(Sys.Date(), "month"))
  refresh <- reactiveVal(0)
  
  observeEvent(input$previous_month, {
    current_month(current_month() %m-% months(1))
  })
  
  observeEvent(input$next_month, {
    current_month(current_month() %m+% months(1))
  })
  
  observeEvent(input$save, {
    d <- as.Date(input$date)
    
    resp <- httr::POST(
      url = FORM_URL,
      body = list(
        "entry.1057536407" = format(d, "%d/%m/%Y"),
        "entry.203803385" = "add"
      ),
      encode = "form"
    )
    
    print(httr::status_code(resp))
    Sys.sleep(2)
    refresh(refresh() + 1)
    showNotification("Saved", type = "message")
  })
  
  observeEvent(input$delete, {
    req(input$delete_date)
    d <- as.Date(input$delete_date)
    
    resp <- httr::POST(
      url = FORM_URL,
      body = list(
        "entry.1057536407" = format(d, "%d/%m/%Y"),
        "entry.203803385" = "delete"
      ),
      encode = "form"
    )
    
    print(httr::status_code(resp))
    Sys.sleep(2)
    refresh(refresh() + 1)
    showNotification("Deleted", type = "warning")
  })
  
  history <- reactive({
    refresh()
    read_data()
  })
  
  observe({
    data <- history()
    choices <- data$date
    names(choices) <- format(data$date, "%d/%m/%Y")
    updateSelectInput(session, "delete_date", choices = choices)
  })
  
  events <- reactive({
    data <- history()
    ev <- data.frame(date = as.Date(character()), type = character())
    
    if (nrow(data) >= 1) {
      ev <- rbind(ev, data.frame(date = data$date, type = "registered"))
    }
    
    if (nrow(data) >= 2) {
      cycle_lengths <- as.numeric(diff(data$date))
      cycle_length <- round(mean(cycle_lengths, na.rm = TRUE))
      
      last_date <- data$date[nrow(data)]
      
      for (i in 1:6) {
        predicted_start <- last_date + days(cycle_length * i)
        
        previous_cycle_start <- predicted_start - days(cycle_length)
        fertile <- previous_cycle_start + days(round(cycle_length / 2))
        
        ev <- rbind(
          ev,
          data.frame(date = predicted_start, type = "predicted"),
          data.frame(date = fertile, type = "fertile")
        )
      }
    }
    
    ev
  })
  
  draw_calendar <- function(month_date, ev) {
    start_month <- floor_date(month_date, "month")
    end_month <- ceiling_date(start_month, "month") - days(1)
    
    first_monday <- start_month - days(wday(start_month, week_start = 1) - 1)
    last_sunday <- end_month + days(7 - wday(end_month, week_start = 1))
    
    days_seq <- seq(first_monday, last_sunday, by = "day")
    day_names <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
    
    header <- lapply(day_names, function(x) {
      div(
        style = "font-weight:bold;text-align:center;padding:8px;background:#f2f2f2;border:1px solid #ddd;",
        x)
    })
    
    boxes <- lapply(days_seq, function(day_date) {
      type <- ev$type[ev$date == day_date]
      
      color <- "white"
      text_color <- "black"
      text <- ""
      
      if ("registered" %in% type) {
        color <- base_cols[2]
        text_color <- "white"
        text <- "🩸"
      }
      
      if ("predicted" %in% type) {
        color <- base_cols[3]
        text_color <- "white"
        text <- "🩸"
      }
      
      if ("fertile" %in% type) {
        color <- base_cols[7]
        text_color <- "white"
        text <- "🌸"
      }
      
      opacity <- ifelse(month(day_date) == month(start_month), "1", "0.35")
      
      div(
        style = paste0(
          "height:85px;border:1px solid #ddd;padding:6px;",
          "background-color:", color, ";",
          "color:", text_color, ";",
          "opacity:", opacity, ";",
          "box-sizing:border-box;"
        ),
        strong(day(day_date)),
        br(),
        span(style = "font-size:12px;", text)
      )
    })
    
    tagList(
      h3(format(start_month, "%B %Y")),
      div(
        style = paste0(
          "display:grid;",
          "grid-template-columns:repeat(7, 1fr);",
          "width:100%;",
          "max-width:900px;"
        ),
        header,
        boxes
      )
    )
  }
  
  output$secure_app <- renderUI({
    req(input$password == APP_PASSWORD)
    ev <- events()
    data <- history()
    
    estimated_length <- NA
    if (nrow(data) >= 2) {
      estimated_length <- round(mean(as.numeric(diff(data$date)), na.rm = TRUE))
    }
    
    tagList(
      draw_calendar(current_month(), ev),
      br(),
      draw_calendar(current_month() %m+% months(1), ev),
      br(),
      if (!is.na(estimated_length)) {
        tagList(
          div(
            style = paste0(
              "background-color:", base_cols[4], ";",
              "color:white;",
              "padding:10px;",
              "border-radius:8px;",
              "max-width:900px;",
              "font-weight:bold;"
            ),
            paste0("Estimated cycle length: ", estimated_length, " days")
          ),
          br(),
          p(
            "Estimated fertile period is calculated as half of your average registered cycle length.",
            style = "font-size:12px;color:black;max-width:900px;"
          )
        )
      }
    )
  })
  
  output$export <- downloadHandler(
    filename = function() {
      "menstrual_history.html"
    },
    
    content = function(file) {
      data <- history()
      text <- c("# First day", "")
      
      if (nrow(data) == 0) {
        text <- c(text, "No periods registered.")
      } else {
        for (i in 1:nrow(data)) {
          text <- c(
            text,
            paste0("Period ", i, ": ", format(data$date[i], "%d/%m/%Y")),
            ""
          )
        }
      }
      
      temp <- tempfile(fileext = ".Rmd")
      
      writeLines(c(
        "---",
        "title: 'Menstrual History'",
        "output: html_document",
        "---",
        "",
        text
      ), temp)
      
      rmarkdown::render(temp, output_file = file, quiet = TRUE)
    }
  )
}

shinyApp(ui, server)
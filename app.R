library(shiny)
library(DT)
library(dplyr)
library(stringr)
library(lubridate)
library(plotly)
library(rentrez)
library(htmltools)
library(shinyjs)
library(promises)
library(future)

# run PubMed fetches in a separate R session
future::plan(multisession)
onStop(function() future::plan(sequential))  # optional for dev

# -- Africa CDC Color Palette
cdc_green    <- "#348F41"
cdc_red      <- "#9F2241"
cdc_gold     <- "#B4A269"
cdc_grey     <- "#58595B"
cdc_dark     <- "#1A5632"
cdc_bg       <- "#F4F7F4"
cdc_card     <- "#FFFFFF"

# --- Loading overlay (spinner) ---
loading_css <- tags$style(HTML(paste0("
/* Overlay + spinner */
#loading_overlay {
  position: fixed; top: 0; left: 0; right: 0; bottom: 0;
  display: flex; align-items: center; justify-content: center;
  z-index: 2000; pointer-events: none;        /* don't block clicks */
}
.loading-hide { display: none !important; }   /* default hidden */
.loading-show { display: flex !important; }   /* shown */

.loading-card {
  pointer-events: auto; background: rgba(255,255,255,0.95);
  border-radius: 14px; padding: 12px 16px; gap: 10px; display: flex; align-items: center;
  box-shadow: 0 6px 28px rgba(0,0,0,0.12); border: 1px solid #e9efe9;
}
.spinner {
  width: 22px; height: 22px; border-radius: 50%;
  border: 3px solid #e8efe8; border-top-color: ", cdc_green, ";
  animation: spin .8s linear infinite;
}
@keyframes spin { to { transform: rotate(360deg); } }
.loading-text { color: ", cdc_dark, "; font-weight: 700; }
")))

# --- Skeleton UI for first load ---
skeleton_css <- tags$style(HTML("
/* Shimmer skeletons */
@keyframes shimmer { 0% {background-position: 100% 0} 100% {background-position: -100% 0} }
.skeleton {
  background: linear-gradient(90deg, rgba(88,89,91,0.08) 25%, rgba(88,89,91,0.14) 37%, rgba(88,89,91,0.08) 63%);
  background-size: 400% 100%;
  animation: shimmer 1.2s ease-in-out infinite;
  border-radius: 10px;
}
"))

skeleton_line <- function(width="100%", height="16px", radius="10px", margin="8px 0") {
  tags$div(class="skeleton", style = sprintf("width:%s;height:%s;margin:%s;border-radius:%s;", width, height, margin, radius))
}

loading_dashboardUI <- function() {
  fluidPage(
    tags$div(class = "kpi-row",
             tags$div(class = "kpi-box kpi-green", skeleton_line("64px","28px"), skeleton_line("110px","10px")),
             tags$div(class = "kpi-box kpi-red",   skeleton_line("64px","28px"), skeleton_line("110px","10px")),
             tags$div(class = "kpi-box kpi-gold",  skeleton_line("64px","28px"), skeleton_line("110px","10px"))
    ),
    tags$div(class = "card",
             h4("Publications Over Time", style=sprintf("color:%s;font-weight:bold;", cdc_green)),
             skeleton_line("100%","220px","14px","6px 0")
    ),
    tags$div(class = "card",
             h4("Top Journals", style=sprintf("color:%s;font-weight:bold;", cdc_red)),
             skeleton_line("100%","220px","14px","6px 0")
    ),
    tags$div(class = "card",
             h4("Top Authors", style=sprintf("color:%s;font-weight:bold;", cdc_gold)),
             skeleton_line("100%","220px","14px","6px 0")
    )
  )
}

# --- PubMed Fetch Function (batching, robust) ---
fetch_africacdc_pubs <- function(retmax = 5000, batch_size = 200) {
  query <- '(Africa Centres for Disease Control[Affiliation] AND Prevention[Affiliation]) OR (Africa CDC[Affiliation])'
  search <- entrez_search(db = "pubmed", term = query, retmax = retmax, use_history = TRUE)
  if (length(search$ids) == 0) return(NULL)
  all_summaries <- list()
  n <- search$count
  for (start in seq(0, n - 1, by = batch_size)) {
    summaries <- entrez_summary(db = "pubmed",
                                web_history = search$web_history,
                                retstart = start,
                                retmax = batch_size)
    all_summaries <- c(all_summaries, summaries)
    Sys.sleep(0.2)
  }
  pubs <- tibble(
    Title = sapply(all_summaries, function(x) x$title),
    Authors = sapply(all_summaries, function(x) {
      if (is.null(x$authors)) return("")
      if (is.character(x$authors)) return(paste(x$authors, collapse = "; "))
      if (is.list(x$authors)) {
        return(
          paste(
            sapply(x$authors, function(a) {
              if (is.list(a) && !is.null(a$name)) a$name else as.character(a)
            }), collapse = "; "
          )
        )
      }
      return(as.character(x$authors))
    }),
    Journal = sapply(all_summaries, function(x) x$fulljournalname),
    Date = sapply(all_summaries, function(x) x$pubdate),
    PubMedID = sapply(all_summaries, function(x) x$uid),
    DOI = sapply(all_summaries, function(x) ifelse(is.null(x$elocationid), "", x$elocationid)),
    Type = sapply(all_summaries, function(x) ifelse(is.null(x$pubtype), "", x$pubtype[1]))
  ) %>%
    mutate(
      Date = parse_date_time(Date, orders = c("Ymd", "Y m d", "Y m", "Y")),
      Year = year(Date)
    )
  return(pubs)
}

# --- Custom CSS (includes mobile tweaks + refresh button fit) ---
custom_css <- tags$style(HTML(paste0("
/* Layout + fonts */
body { background-color: ", cdc_bg, " !important; font-family: 'Segoe UI','Roboto',Arial,sans-serif; }

/* ===== Fixed Header (3 rows stacked) ===== */
#app-header {
  position: fixed; top: 0; left: 0; right: 0;
  z-index: 1000; background: ", cdc_bg, ";
  box-shadow: 0 2px 10px rgba(0,0,0,0.06);
}

/* Row 1: Logo on white card centered */
.header-row.logo {
  background: ", cdc_card, ";
  display: flex; justify-content: center; align-items: center;
  padding: 14px 10px 10px 10px;
}
.logo-card { background: ", cdc_card, "; border-radius: 12px; box-shadow: 0 1px 8px rgba(52,143,65,0.07);
  padding: 12px 16px 10px 16px; display:flex; flex-direction:column; align-items:center; }
.logo-card img { height: 56px; margin-bottom: 6px; }
.logo-title { font-size: 1.6rem; color: ", cdc_green, "; font-weight: 700; letter-spacing: 0.5px; text-align: center; }

/* Row 2: Centered date range, refresh on right */
.header-row.filters {
  background: ", cdc_bg, ";
  display: grid; grid-template-columns: 1fr minmax(280px, 560px) 1fr; gap: 14px;
  align-items: center; padding: 8px 14px 12px 14px;
}
#date-center { text-align: center; }
#date-center label { width: 100%; text-align: center; font-weight: 700; }
.refresh-wrap { display:flex; justify-content: flex-end; align-items: center; }
.refresh-btn {
  background: ", cdc_gold, "; color: ", cdc_dark, ";
  border: none; border-radius: 10px; font-weight: 800;
  padding: 10px 14px; line-height: 1.1; white-space: normal; max-width: 260px; width: 100%;
  box-shadow: 0 1px 6px rgba(0,0,0,0.08);
}

/* Row 3: Centered nav bar in green */
#navbar {
  background: ", cdc_green, "; color: white;
  display: flex; justify-content: center; align-items: center; gap: 18px;
  padding: 8px 12px 10px 12px;
}
#navbar .nav-btn { color: white; background: ", cdc_dark, "; border: none; border-radius: 8px;
  padding: 8px 14px; font-weight: 700; font-size: 1.05rem; }
#navbar .nav-btn.selected, #navbar .nav-btn:hover { background: ", cdc_gold, "; color: ", cdc_dark, "; }

/* Space below fixed header so content is visible */
#main-content { max-width: 1250px; margin: 0 auto; padding: 270px 14px 24px 14px; }

/* Cards & KPIs */
.card { background: ", cdc_card, "; border-radius: 16px; box-shadow: 0 2px 16px rgba(52,143,65,0.06); padding: 1.2rem 1.4rem; margin-bottom: 2rem; }
.kpi-row { display:flex; justify-content:center; gap: 24px; flex-wrap: wrap; margin: 12px 0 16px 0; }
.kpi-box { background: ", cdc_grey, "; color: white; border-radius: 13px; font-size: 1.05rem; font-weight: 700; padding: 1rem 1.4rem; display: flex; align-items: center; gap: 12px; box-shadow: 0 1px 8px rgba(52,143,65,0.10); }
.kpi-num { font-size: 1.9rem; font-weight: 800; margin-right: 6px; }
.kpi-green { background: ", cdc_green, "; }
.kpi-red { background: ", cdc_red, "; }
.kpi-gold { background: ", cdc_gold, "; color: white; }

/* DataTable look */
.dataTable tbody tr.selected, .dataTable tbody tr:hover { background-color: ", cdc_grey, "14 !important; }
table.dataTable.display tbody tr.odd { background-color: #f6f6f6; }
.dataTables_wrapper .dt-buttons .btn { background: ", cdc_green, " !important; color: white !important; border: none; }

/* Author explorer */
.author-sel-wrap { display: flex; flex-direction: column; align-items: center; justify-content: center; margin-bottom: 12px; }
.author-sel-label { font-size: 1.02rem; color: ", cdc_dark, "; font-weight: 700; margin-bottom: 5px; }
.author-card { background: ", cdc_green, "; border-radius: 13px; box-shadow: 0 2px 12px rgba(52,143,65,0.07); padding: 20px 30px 14px 30px; margin: 0 auto 16px auto; display:flex; flex-direction:column; align-items:center; max-width: 340px; }
.author-card .author-kpi { margin-top: 6px; font-size: 1rem; color: ", cdc_card, "; }

/* Mobile responsiveness */
@media (max-width: 768px) {
  .logo-card img { height: 48px; }
  .logo-title { font-size: 1.35rem; }
  .header-row.filters { grid-template-columns: 1fr; padding: 8px 12px 10px 12px; }
  #date-center { order: 1; }
  .refresh-wrap { order: 2; justify-content: center; }
  #navbar { flex-wrap: wrap; gap: 10px; }
  #main-content { padding: 310px 10px 20px 10px; } /* taller header on small screens */
}
")))

# --- UI ---
ui <- fluidPage(
  custom_css,
  skeleton_css,  # skeleton for first load
  loading_css,   # overlay for manual refresh
  useShinyjs(),
  
  # ===== Fixed header (logo + date/refresh + nav) =====
  tags$div(id = "app-header",
           # Row 1: logo (white background)
           tags$div(class = "header-row logo",
                    tags$div(class = "logo-card",
                             tags$img(src="acdc_logo.png", alt="Africa CDC Logo"),
                             tags$div(class="logo-title", "Africa CDC Publications Tracker")
                    )
           ),
           # Row 2: centered date range & refresh (right)
           tags$div(class = "header-row filters",
                    tags$div(),                                # left spacer
                    tags$div(id="date-center", uiOutput("global_date_ui")),  # centered date selector
                    tags$div(class = "refresh-wrap",
                             actionButton(
                               "update_pubmed",
                               label = tagList(icon("sync"), span("Refresh PubMed Data")),
                               class = "refresh-btn"
                             )
                    )
           ),
           # Row 3: centered navigation
           tags$div(id = "navbar",
                    actionButton("dashboard_tab", "Dashboard", class = "nav-btn selected"),
                    actionButton("author_tab", "Author Explorer", class = "nav-btn"),
                    actionButton("table_tab", "Publications Table", class = "nav-btn")
           )
  ),
  # overlay (starts hidden)
  tags$div(
    id = "loading_overlay", class = "loading-hide",
    tags$div(class = "loading-card",
             tags$div(class = "spinner"),
             tags$div(class = "loading-text", "Refreshing PubMed…")
    )
  ),
  
  # Main content (scrolls under fixed header)
  tags$div(id = "main-content",
           uiOutput("tab_content")
  )
)

server <- function(input, output, session) {
  # ----------------- Helpers -----------------
  split_clean_authors <- function(x) {
    a <- unlist(strsplit(paste(x, collapse = ";"), ";", fixed = TRUE))
    a <- trimws(a)
    a <- gsub("\\s+", " ", a)
    a[a != "" & !grepl("^authors?$", a, ignore.case = TRUE)]
  }
  dedup_case_insensitive <- function(vec) {
    if (length(vec) == 0) return(character(0))
    df <- tibble(name = vec, key = tolower(vec)) |>
      group_by(key) |>
      summarise(name = first(name), .groups = "drop")
    df$name
  }
  
  # Tab state
  active_tab <- reactiveVal("dashboard")
  observeEvent(input$dashboard_tab, {
    active_tab("dashboard")
    runjs("$('.nav-btn').removeClass('selected'); $('#dashboard_tab').addClass('selected');")
  })
  observeEvent(input$author_tab, {
    active_tab("author")
    runjs("$('.nav-btn').removeClass('selected'); $('#author_tab').addClass('selected');")
  })
  observeEvent(input$table_tab, {
    active_tab("table")
    runjs("$('.nav-btn').removeClass('selected'); $('#table_tab').addClass('selected');")
  })
  
  # ---------- Data store + FIRST-LOAD skeleton ----------
  pubs_data  <- reactiveVal(NULL)
  is_loading <- reactiveVal(TRUE)
  
  # initial background fetch (once)
  observeEvent(TRUE, {
    promises::future_promise({
      fetch_africacdc_pubs(5000)
    }) %...>% (function(df) {
      pubs_data(df)
      is_loading(FALSE)
    }) %...!% (function(e) {
      is_loading(FALSE)
      showNotification(paste("Initial load failed:", conditionMessage(e)), type = "error", duration = 8)
    })
  }, once = TRUE)
  
  # Global Date UI + Refresh
  output$global_date_ui <- renderUI({
    df <- pubs_data()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    date_min <- as.Date(min(df$Date, na.rm = TRUE))
    date_max <- as.Date(max(df$Date, na.rm = TRUE))
    dateRangeInput(
      "daterange", "Select Publication Date Range:",
      start = date_min, end = date_max,
      min = date_min, max = date_max,
      format = "yyyy-mm-dd", separator = " to ", width = "100%"
    )
  })
  
  # Date-filtered data (drives every plot/table)
  date_filtered_data <- reactive({
    df <- pubs_data()
    dr <- input$daterange
    if (is.null(df) || is.null(dr) || length(dr) != 2) return(df)
    df %>% filter(!is.na(Date), Date >= as.Date(dr[1]), Date <= as.Date(dr[2]))
  })
  
  # Refresh logic (background)
  observeEvent(input$update_pubmed, {
    shinyjs::disable("update_pubmed")
    # show overlay
    shinyjs::removeClass("loading_overlay", "loading-hide")
    shinyjs::addClass("loading_overlay", "loading-show")
    notif_id <- showNotification("Refreshing PubMed in the background…", type = "message", duration = NULL)
    
    promises::future_promise({
      fetch_africacdc_pubs(5000)
    }) %...>%  # success
      (function(new_df) {
        if (!is.null(new_df) && nrow(new_df) > 0) {
          pubs_data(new_df)
          showNotification("✅ PubMed data refreshed.", type = "message", duration = 4)
        } else {
          showNotification("No data returned from PubMed.", type = "warning", duration = 6)
        }
      }) %...!%  # error
      (function(e) {
        showNotification(paste("Refresh failed:", conditionMessage(e)), type = "error", duration = 8)
      }) %...>%  # finally
      (function(...) {
        # hide overlay
        shinyjs::removeClass("loading_overlay", "loading-show")
        shinyjs::addClass("loading_overlay", "loading-hide")
        removeNotification(notif_id)
        shinyjs::enable("update_pubmed")
      })
  })
  
  # -------- Authors (cleaned, deduped) ----------
  all_authors <- reactive({
    df <- date_filtered_data()
    if (is.null(df)) return(character(0))
    a <- split_clean_authors(df$Authors)
    a <- dedup_case_insensitive(a)
    sort(a)
  })
  observe({
    updateSelectizeInput(session, "author_select", choices = all_authors(), server = TRUE)
  })
  
  # Dashboard UI
  dashboardUI <- reactive({
    df <- date_filtered_data()
    n_pubs <- ifelse(is.null(df), 0, nrow(df))
    n_authors <- if (is.null(df)) 0 else {
      a <- split_clean_authors(df$Authors)
      length(unique(tolower(a)))
    }
    n_journals <- if (is.null(df)) 0 else length(unique(df$Journal))
    fluidPage(
      tags$div(class = "kpi-row",
               tags$div(class = "kpi-box kpi-green", icon("book"), tags$span(class="kpi-num", n_pubs), "Publications"),
               tags$div(class = "kpi-box kpi-red", icon("users"), tags$span(class="kpi-num", n_authors), "Unique Authors"),
               tags$div(class = "kpi-box kpi-gold", icon("newspaper"), tags$span(class="kpi-num", n_journals), "Journals")
      ),
      tags$div(class = "card",
               h4("Publications Over Time", style=sprintf("color:%s;font-weight:bold;", cdc_green)),
               plotlyOutput("pubs_over_time", height="320px")
      ),
      tags$div(class = "card",
               h4("Top Journals", style=sprintf("color:%s;font-weight:bold;", cdc_red)),
               plotlyOutput("top_journals", height="270px")
      ),
      tags$div(class = "card",
               h4("Top Authors", style=sprintf("color:%s;font-weight:bold;", cdc_gold)),
               sliderInput("top_authors_count", "Number of Authors to Display:", min = 5, max = 30, value = 15, step = 1, width = "50%"),
               plotlyOutput("top_authors")  # let plotly set dynamic height
      )
    )
  })
  
  # ---- Author Explorer UI (improved visuals) ----
  output$author_explorer_ui <- renderUI({
    tagList(
      tags$div(
        style = "display:flex; flex-direction:column; align-items:center; margin-bottom:12px;",
        tags$div(class = "author-sel-wrap",
                 tags$span(class = "author-sel-label", "Select Author:"),
                 selectizeInput("author_select", NULL, choices = all_authors(), multiple = FALSE,
                                options = list(placeholder = 'Type or pick an author...', maxOptions = 10, width = "370px"))
        )
      ),
      uiOutput("author_profile_card"),
      tags$div(
        class = "card", style="margin-bottom:22px;",
        plotlyOutput("author_trend_combo", height = "300px")
      ),
      tags$div(
        class = "card", style="margin-bottom:22px;",
        plotlyOutput("author_top_journals", height = "320px")
      ),
      tags$div(
        class="card", style="margin-bottom:16px;",
        tags$h5("Publications by Selected Author", style = sprintf("color:%s;font-weight:bold; margin-bottom:14px;", cdc_dark)),
        DTOutput("author_pubs_table")
      )
    )
  })
  
  # Author KPI card
  output$author_profile_card <- renderUI({
    df <- author_pubs()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    pubs_count <- nrow(df)
    tags$div(
      class = "author-card",
      style = "background: #348F41; color:white; border-radius:14px; box-shadow:0 2px 10px rgba(52,143,65,0.11); padding:20px 40px 12px 40px; margin:16px auto 20px auto; display:flex; flex-direction:column; align-items:center; max-width:320px; min-width:180px;",
      tags$div(style="font-size:2.3rem; font-weight:700; color:white; text-align:center;line-height:1.2;", pubs_count),
      tags$div(style="font-size:1.1rem; font-weight:500; color:white; text-align:center; margin-top:7px;", "Total Publications")
    )
  })
  
  # Publications for selected author (respects date range)
  author_pubs <- reactive({
    sel_author <- input$author_select
    if (is.null(sel_author) || sel_author == "" || length(sel_author) == 0) return(NULL)
    df <- date_filtered_data()
    if (is.null(df)) return(NULL)
    df <- df %>%
      mutate(cleaned_authors = sapply(Authors, function(a) {
        authors <- unlist(strsplit(a, ";", fixed = TRUE))
        authors <- trimws(gsub("\\s+", " ", authors))
        authors <- authors[authors != "" & !grepl("^authors?$", authors, ignore.case = TRUE)]
        authors
      }))
    out <- df[sapply(df$cleaned_authors, function(authlist) sel_author %in% authlist), ]
    if (nrow(out) == 0) return(NULL)
    out
  })
  
  # Author trend plot — counts inside bars + clean hover, no inheritance warnings
  output$author_trend_combo <- renderPlotly({
    df <- author_pubs()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    year_df <- df %>%
      filter(!is.na(Year)) %>%
      group_by(Year) %>%
      summarise(Publications = n(), .groups = "drop") %>%
      arrange(Year)
    
    plot_ly(
      year_df,
      x = ~Year, y = ~Publications,
      type = 'bar', name = 'Publications',
      marker = list(color = cdc_green, line = list(width = 0)),
      text = ~Publications, textposition = 'inside',
      insidetextanchor = 'middle',
      textfont = list(color = 'white', size = 12),
      hovertemplate = 'Year: %{x}<br>Publications: %{y}<extra></extra>'
    ) %>%
      add_trace(
        inherit = FALSE,
        x = year_df$Year, y = year_df$Publications,
        type = 'scatter', mode = 'lines+markers',
        line = list(color = cdc_red, width = 3), name = 'Trend',
        hovertemplate = '<extra></extra>'
      ) %>%
      layout(
        title = list(text = "Publications Over Years", y=0.96, font=list(size=21)),
        yaxis = list(title = "Number of Publications", tickfont=list(size=14), titlefont=list(size=15), automargin = TRUE),
        xaxis = list(
          title = "Year",
          tickmode = "array",
          tickvals = year_df$Year,
          ticktext = as.character(year_df$Year),
          tickformat = "d",
          tickfont = list(size = 15),
          titlefont = list(size=15)
        ),
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.28, yanchor = "bottom", font = list(size = 14)),
        margin = list(l = 60, r = 20, t = 50, b = 55),
        plot_bgcolor = cdc_card, paper_bgcolor = cdc_card
      )
  })
  
  # Author top journals
  output$author_top_journals <- renderPlotly({
    df <- author_pubs()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    top_journals <- df %>%
      count(Journal, name = "Publications") %>%
      arrange(desc(Publications)) %>%
      slice_head(n = 10) %>%
      mutate(Journal = factor(Journal, levels = rev(Journal)))
    
    plot_ly(
      top_journals,
      x = ~Publications, y = ~Journal,
      type = 'bar', orientation = 'h',
      marker = list(color = rep(cdc_gold, nrow(top_journals))),
      text = ~Publications, textposition = 'inside',
      insidetextanchor = 'middle',
      textfont = list(color = 'white', size = 12),
      hovertemplate = 'Journal: %{y}<br>Publications: %{x}<extra></extra>'
    ) %>%
      layout(
        title = list(text = paste("Top Journals -", input$author_select), y=0.97),
        yaxis = list(title = "", tickfont = list(size=14), automargin = TRUE),
        xaxis = list(title = "Publications", tickfont = list(size=13)),
        margin = list(l = 160, r = 20, t = 50, b = 55),
        plot_bgcolor = cdc_card, paper_bgcolor = cdc_card
      )
  })
  
  # Author table
  output$author_pubs_table <- renderDT({
    df <- author_pubs()
    if (is.null(df) || nrow(df) == 0) return(datatable(data.frame(Message = "No publications.")))
    df$Title <- mapply(function(title, pmid) {
      as.character(tags$a(title, href = sprintf("https://pubmed.ncbi.nlm.nih.gov/%s", pmid), target="_blank"))
    }, df$Title, df$PubMedID)
    show_df <- df[,c("Title", "Year", "Journal", "Type", "DOI", "Date")]
    datatable(show_df, escape=FALSE, options=list(pageLength=10, scrollX=TRUE), rownames=FALSE)
  })
  
  # Publications Table Tab — responds to date range
  output$pub_table <- renderDT({
    df <- date_filtered_data()
    if (is.null(df) || nrow(df) == 0) return(datatable(data.frame(Message = "No data available")))
    df$Authors <- sapply(df$Authors, function(a) {
      authors <- unlist(strsplit(a, ";", fixed = TRUE))
      authors <- trimws(gsub("\\s+", " ", authors))
      authors <- authors[authors != "" & !grepl("^authors?$", authors, ignore.case = TRUE)]
      paste(authors, collapse = "; ")
    })
    df$Title <- mapply(function(title, pmid, authors) {
      as.character(tags$a(title, href = sprintf("https://pubmed.ncbi.nlm.nih.gov/%s", pmid), target="_blank",
                          title=paste("Authors:", authors)))
    }, df$Title, df$PubMedID, df$Authors)
    df$PubMedID <- mapply(function(pmid) {
      as.character(tags$a(pmid, href = sprintf("https://pubmed.ncbi.nlm.nih.gov/%s", pmid), target="_blank"))
    }, df$PubMedID)
    df$DOI <- ifelse(df$DOI != "",
                     mapply(function(doi) {
                       as.character(tags$a(doi, href = sprintf("https://doi.org/%s", gsub("doi: *", "", doi)), target="_blank"))
                     }, df$DOI),
                     "")
    df$Date <- as.character(format(df$Date, "%Y-%m-%d"))
    show_df <- df[,c("Title", "Authors", "Year", "Journal", "Type", "PubMedID", "DOI", "Date")]
    datatable(
      show_df,
      escape = FALSE,
      options = list(
        pageLength = 20, scrollX = TRUE,
        dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'print'),
        columnDefs = list(list(targets = "_all", className = "dt-left")),
        autoWidth = TRUE
      ),
      extensions = c('Buttons', 'Scroller'),
      rownames = FALSE,
      selection = 'single',
      class = 'display nowrap stripe'
    )
  })
  
  # Dashboard Visuals — respond to date range
  output$pubs_over_time <- renderPlotly({
    df <- date_filtered_data()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    all_count <- df %>%
      mutate(Year = year(Date)) %>%
      group_by(Year) %>%
      summarise(AllPublications = n(), .groups = "drop") %>%
      arrange(Year)
    
    plot_ly(
      all_count,
      x = ~Year, y = ~AllPublications,
      type = 'bar', name = 'Publications',
      marker = list(color = cdc_green, line = list(width = 0)),
      text = ~AllPublications, textposition = 'inside',
      insidetextanchor = 'middle',
      textfont = list(color = 'white', size = 12),
      hovertemplate = 'Year: %{x}<br>Publications: %{y}<extra></extra>'
    ) %>%
      add_trace(
        inherit = FALSE,
        x = all_count$Year, y = all_count$AllPublications,
        type = 'scatter', mode = 'lines+markers',
        line = list(color = cdc_red, width = 3), name = 'Trend',
        hovertemplate = '<extra></extra>'
      ) %>%
      layout(
        xaxis = list(title = "Year"),
        yaxis = list(title = "Number of Publications"),
        legend = list(orientation="h", x=0.18, y=1.1, bgcolor="rgba(0,0,0,0)"),
        plot_bgcolor = cdc_card, paper_bgcolor = cdc_card
      )
  })
  
  output$top_journals <- renderPlotly({
    df <- date_filtered_data()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    top_journals <- df %>%
      group_by(Journal) %>%
      summarise(Publications = n(), .groups = "drop") %>%
      arrange(desc(Publications)) %>%
      slice_head(n = 10) %>%
      mutate(Journal = factor(Journal, levels = rev(Journal)))
    
    plot_ly(
      top_journals,
      x = ~Publications, y = ~Journal,
      type = 'bar', orientation = 'h',
      marker = list(color = rep(cdc_red, nrow(top_journals))),
      text = ~Publications, textposition = 'inside',
      insidetextanchor = 'middle',
      textfont = list(color = 'white', size = 12),
      hovertemplate = 'Journal: %{y}<br>Publications: %{x}<extra></extra>'
    ) %>%
      layout(
        yaxis = list(title = "", tickfont = list(size=13), automargin = TRUE),
        xaxis = list(title = "Publications"),
        margin = list(l = 160, r = 20, t = 40, b = 50),
        plot_bgcolor = cdc_card, paper_bgcolor = cdc_card
      )
  })
  
  output$top_authors <- renderPlotly({
    req(input$top_authors_count)
    n <- as.integer(input$top_authors_count)
    
    df <- date_filtered_data()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    # Clean authors (drop blanks and "author/authors")
    a <- unlist(strsplit(paste(df$Authors, collapse = ";"), ";", fixed = TRUE))
    a <- trimws(gsub("\\s+", " ", a))
    a <- a[a != "" & !grepl("^authors?$", a, ignore.case = TRUE)]
    
    # Case-insensitive aggregation; keep the first-seen casing for label
    df_names <- tibble::tibble(name = a, key = tolower(a))
    ta <- df_names %>%
      dplyr::count(key, name = "Freq") %>%
      dplyr::left_join(
        df_names %>% dplyr::group_by(key) %>% dplyr::summarise(label = dplyr::first(name), .groups = "drop"),
        by = "key"
      ) %>%
      dplyr::arrange(desc(Freq)) %>%
      dplyr::slice_head(n = n)
    
    # Dynamic sizing so all labels fit
    max_chars <- if (nrow(ta)) max(nchar(ta$label)) else 0
    left_margin <- max(180, min(420, round(max_chars * 8.5)))  # scale margin by name length
    plot_height <- 80 + n * 32                                 # ~32px per row
    
    plot_ly(
      ta,
      x = ~Freq,
      y = ~reorder(label, Freq),
      type = 'bar',
      orientation = 'h',
      marker = list(color = rep(cdc_gold, nrow(ta))),
      text = ~Freq,
      textposition = 'inside',
      insidetextanchor = 'middle',
      textfont = list(color = 'white', size = 12),
      hovertemplate = 'Author: %{y}<br>Publications: %{x}<extra></extra>',
      height = plot_height
    ) %>%
      layout(
        yaxis = list(title = "", tickfont = list(size = 15), autorange = "reversed", automargin = TRUE),
        xaxis = list(title = "Publications"),
        margin = list(l = left_margin, r = 20, t = 40, b = 50),
        plot_bgcolor = cdc_card,
        paper_bgcolor = cdc_card
      )
  })
  
  # Tab switcher with skeleton on first load
  output$tab_content <- renderUI({
    if (isTRUE(is_loading())) {
      return(loading_dashboardUI())
    }
    tab <- active_tab()
    if (tab == "dashboard") {
      dashboardUI()
    } else if (tab == "author") {
      tags$div(class = "card", style="max-width:1000px;margin:0 auto;", uiOutput("author_explorer_ui"))
    } else {
      tags$div(class = "card", DTOutput("pub_table"))
    }
  })
}

shinyApp(ui, server)

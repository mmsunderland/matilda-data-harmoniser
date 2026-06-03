library(shiny)
library(bslib)
library(DT)

source("R/parse_dct.R")
source("R/code_generator.R")
source("R/plan_server.R")

.PIPELINE_DIR <- normalizePath(file.path("output", "final"), mustWork = FALSE)
.DL_PATH      <- file.path(.PIPELINE_DIR, "domain_labels.csv")
.HC_PATH      <- file.path(.PIPELINE_DIR, "harmonisation_candidates.csv")
.RR_PATH      <- file.path(.PIPELINE_DIR, "recode_rules.csv")

.pipeline_available <- function() {
  file.exists(.DL_PATH) && file.exists(.HC_PATH) && file.exists(.RR_PATH)
}

.get_ds_waves <- function(data, ds) {
  if (is.null(data) || !"has_wave" %in% names(data)) return(character(0))
  d <- data[!is.na(data$dataset) & data$dataset == ds, ]
  w <- d[!is.na(d$has_wave) & d$has_wave, ]
  if (nrow(w) == 0 || !"wave_label_std" %in% names(w)) return(character(0))
  sort(unique(w$wave_label_std[!is.na(w$wave_label_std)]))
}

.av_label_lookup <- function(av) {
  if (!"value_labels" %in% names(av)) av$value_labels <- NA_character_
  if (!all(c("has_wave", "base_var") %in% names(av))) return(av)
  wave_av <- av[!is.na(av$has_wave) & av$has_wave, , drop = FALSE]
  if (nrow(wave_av) == 0) return(av)
  base_av <- wave_av[!duplicated(paste(wave_av$dataset, wave_av$base_var)), , drop = FALSE]
  base_av$var_name  <- base_av$base_var
  base_av$var_label <- sub("^[A-Za-z]+\\d+[_\\s]+", "", base_av$var_label, perl = TRUE)
  combined <- rbind(
    av[, c("dataset", "var_name", "var_label", "value_labels")],
    base_av[, c("dataset", "var_name", "var_label", "value_labels")]
  )
  combined[!duplicated(paste(combined$dataset, combined$var_name)), ]
}

.compatibility <- function(status) {
  if (is.null(status) || is.na(status) || status == "")
    return(list(col = "secondary", txt = "Unknown compatibility"))
  switch(status,
    high_confidence = list(col = "success", txt = "Identical response codes"),
    recodable       = list(col = "warning", txt = "Similar — recode may be needed"),
    low_confidence  = list(col = "warning", txt = "Similar — recode may be needed"),
    needs_review    = list(col = "danger",  txt = "Incompatible — manual review needed"),
    list(col = "secondary", txt = status)
  )
}

`%||%` <- function(x, y) if (!is.null(x) && length(x) > 0) x else y

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------
ui <- page_sidebar(
  title = "Data Harmonisation Assistant",
  theme = bs_theme(bootswatch = "flatly"),
  tags$head(
    tags$link(rel = "stylesheet", href = "custom.css"),
    tags$script(src = "custom.js"),
    tags$style(HTML("
      .tbl-row-inplan  { border-left: 3px solid #198754 !important; background-color: #f0fff4 !important; }
      .dt-scroll-body  { overflow-y: auto !important; }
      .detail-panel    { height: calc(100vh - 230px); overflow-y: auto; }
      .match-card      { font-size: 0.85em; }
      .btn-xs          { padding: 1px 6px; font-size: 0.75em; }
      table.dataTable tbody tr td { padding: 4px 8px !important; }
      /* Allow selectize dropdowns to overflow the filter card */
      .filter-card, .filter-card .card-body { overflow: visible !important; }
      .selectize-dropdown { z-index: 9999 !important; max-height: 300px !important; }
      .selectize-dropdown-content { max-height: 280px !important; overflow-y: auto !important; }
      /* Taller selectize inputs for easier interaction */
      .filter-card .selectize-input { min-height: 38px !important; padding: 6px 8px !important; }
    "))
  ),
  sidebar = sidebar(
    width = 260,
    uiOutput("status_msg"),
    hr(class = "my-2"),
    uiOutput("plan_status")
  ),

  uiOutput("pipeline_warning"),

  accordion(
    id = "help_accordion", open = FALSE, class = "mb-2",
    accordion_panel(
      "How to use", icon = icon("circle-info"),
      tags$ol(class = "small mb-0",
        tags$li(tags$strong("Browse Variables"),
                " — filter by dataset/wave/domain, click a variable to see details and suggested matches."),
        tags$li(tags$strong("Build Harmonisation Plan"),
                " — edit Harmonised Name column. Green groups span ≥2 datasets."),
        tags$li(tags$strong("Generate R Code"),
                " — review and download the R script.")
      )
    )
  ),

  navset_tab(
    id = "main_tabs",

    # ── Tab 1: Browse Variables ──────────────────────────────────────────────
    nav_panel(
      "Browse Variables",

      # Filter bar
      card(
        class = "mb-2 filter-card",
        card_body(
          class = "py-2 px-3",
          layout_columns(
            col_widths = c(3, 3, 2, 2, 2), gap = "0.5rem",

            div(
              div(class = "d-flex justify-content-between align-items-center mb-1",
                  tags$label("Datasets", class = "small fw-semibold text-muted mb-0"),
                  div(class = "d-flex gap-2",
                      actionLink("sel_all_ds", "all", class = "small"),
                      actionLink("clr_ds", "none", class = "small text-muted"))),
              selectizeInput("filter_datasets", NULL, choices = NULL, multiple = TRUE, width = "100%",
                             options = list(placeholder = "All datasets...",
                                            plugins = list("remove_button")))
            ),

            div(
              div(class = "d-flex justify-content-between align-items-center mb-1",
                  tags$label("Waves", class = "small fw-semibold text-muted mb-0"),
                  div(class = "d-flex gap-2",
                      actionLink("sel_all_wv", "all", class = "small"),
                      actionLink("clr_wv", "none", class = "small text-muted"))),
              selectizeInput("filter_waves", NULL, choices = NULL, multiple = TRUE, width = "100%",
                             options = list(placeholder = "All waves...",
                                            plugins = list("remove_button")))
            ),

            div(
              tags$label("Domain", class = "small fw-semibold text-muted mb-1 d-block"),
              selectInput("filter_domain", NULL, choices = c("All domains" = ""), width = "100%")
            ),

            div(
              tags$label("Subdomain", class = "small fw-semibold text-muted mb-1 d-block"),
              selectInput("filter_subdomain", NULL, choices = c("All subdomains" = ""), width = "100%")
            ),

            div(
              tags$label("Search", class = "small fw-semibold text-muted mb-1 d-block"),
              textInput("filter_search", NULL, placeholder = "Name or label...", width = "100%")
            )
          )
        )
      ),

      uiOutput("filter_summary"),

      # Table + detail panel
      layout_columns(
        col_widths = c(8, 4), gap = "0.75rem",

        div(
          div(class = "d-flex justify-content-between align-items-center mb-1",
              uiOutput("vars_count_label"),
              actionButton("add_to_plan", "Add selected to plan",
                           icon = icon("plus"), class = "btn-sm btn-outline-primary")),
          card(card_body(class = "p-0", DT::dataTableOutput("vars_table")))
        ),

        uiOutput("detail_panel")
      )
    ),

    # ── Tab 2: Build Harmonisation Plan ──────────────────────────────────────
    nav_panel(
      "Build Harmonisation Plan",

      # Bulk operations toolbar
      card(
        class = "mb-2",
        card_body(class = "py-2 px-3",
          div(class = "d-flex gap-2 flex-wrap align-items-center",
            actionButton("btn_autofill_labels", "Auto-fill labels",
                         icon = icon("wand-magic-sparkles"),
                         class = "btn-sm btn-outline-secondary"),
            actionButton("btn_check_recodes",  "Check all recodes",
                         icon = icon("list-check"),
                         class = "btn-sm btn-outline-secondary"),
            actionButton("btn_validate_names", "Validate names",
                         icon = icon("spell-check"),
                         class = "btn-sm btn-outline-secondary"),
            tags$span(class = "vr mx-1"),
            div(style = "display:inline-block; margin-bottom:0;",
              fileInput("btn_import_template", NULL,
                        buttonLabel = HTML(paste(as.character(icon("file-arrow-up")), "Import mapping")),
                        accept = ".csv", width = "210px")
            ),
            downloadButton("btn_export_template", "Export mapping",
                           class = "btn-sm btn-outline-secondary"),
            tags$span(class = "vr mx-1"),
            actionButton("clear_plan", NULL, icon = icon("trash"),
                         title = "Clear entire plan",
                         class = "btn btn-sm btn-outline-danger")
          ),
          uiOutput("bulk_result")
        )
      ),

      # Main toolbar: select-all + filters
      uiOutput("plan_toolbar"),

      # Editing toolbar (visible when rows selected)
      uiOutput("plan_editing_toolbar"),

      # Plan table
      card(card_body(class = "p-0", DT::dataTableOutput("plan_table"))),

      # Recode editor panel (inline below table)
      uiOutput("recode_editor_panel"),

      # Harmonisation summary (collapsible)
      uiOutput("harmonisation_summary_panel")
    ),

    # ── Tab 3: Generate R Code ────────────────────────────────────────────────
    nav_panel(
      "Generate R Code",
      uiOutput("code_status"),
      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          tags$span("Generated R Script"),
          tags$div(class = "d-flex gap-2",
            tags$button(id = "copy_code_btn", class = "btn btn-sm btn-outline-secondary",
                        onclick = "copyGeneratedCode()", HTML("&#128203; Copy")),
            downloadButton("download_code", "Download .R", class = "btn-sm btn-outline-primary"))
        ),
        card_body(class = "p-0", verbatimTextOutput("generated_code"))
      )
    )
  )
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------
server <- function(input, output, session) {

  rv <- reactiveValues(
    selected_var_key  = NULL,   # "dataset|||var_name" of clicked row
    preview_key       = NULL,   # key shown in Section A when previewing a match
    detail_open       = FALSE,
    filter_datasets   = character(0),
    filter_waves      = character(0)
  )

  all_vars        <- reactiveVal(NULL)
  all_labels      <- reactiveVal(data.frame(dataset = character(), label_set = character(),
                                             value = integer(), label_text = character(),
                                             stringsAsFactors = FALSE))
  harmonisation_plan <- reactiveVal(list())
  pipeline_loaded    <- reactiveVal(FALSE)
  harm_candidates    <- reactiveVal(NULL)
  recode_rules_tbl   <- reactiveVal(NULL)
  var_mapping <- reactiveVal(data.frame(
    row_id = integer(), dataset = character(), wave = character(),
    var_name = character(), base_var = character(),
    var_label = character(), var_type = character(),
    value_labels = character(), harmonised_name = character(),
    harmonised_label = character(), recode_status = character(),
    notes = character(), in_plan = logical(),
    stringsAsFactors = FALSE
  ))
  recode_data <- reactiveVal(list())

  # ── Startup ────────────────────────────────────────────────────────────────

  observe({
    if (.pipeline_available()) {
      tryCatch({
        dl <- read.csv(.DL_PATH, stringsAsFactors = FALSE, na.strings = c("", "NA"))
        hc <- read.csv(.HC_PATH, stringsAsFactors = FALSE, na.strings = c("", "NA"))
        rr <- read.csv(.RR_PATH, stringsAsFactors = FALSE, na.strings = c("", "NA"))
        if (!"value_labels" %in% names(dl)) dl$value_labels <- NA_character_
        all_vars(dl); harm_candidates(hc); recode_rules_tbl(rr)
        pipeline_loaded(TRUE)
        .init_filters(dl)
      }, error = function(e) {
        showNotification(paste("Pipeline load failed:", conditionMessage(e)),
                         type = "error", duration = NULL)
        .load_dct()
      })
    } else {
      .load_dct()
    }
  })

  .load_dct <- function() {
    tryCatch({
      data   <- load_all_dcts("dct/")
      labels <- load_all_dct_labels("dct/")
      all_vars(data); all_labels(labels); pipeline_loaded(FALSE)
      .init_filters(data)
    }, error = function(e) {
      showNotification(paste("DCT load failed:", conditionMessage(e)),
                       type = "error", duration = NULL)
    })
  }

  .init_filters <- function(data) {
    datasets <- sort(unique(data$dataset))
    updateSelectizeInput(session, "filter_datasets", choices = datasets,
                         selected = character(0), server = TRUE)
    .refresh_wave_choices(data, character(0))
    if ("domain" %in% names(data)) {
      doms <- sort(unique(data$domain[!is.na(data$domain)]))
      updateSelectInput(session, "filter_domain",
                        choices = c("All domains" = "", setNames(doms, doms)))
    }
  }

  .refresh_wave_choices <- function(data, ds_sel) {
    if (is.null(data)) return()
    active_ds <- if (length(ds_sel) == 0) sort(unique(data$dataset)) else ds_sel
    waves <- character(0)
    for (ds in active_ds) waves <- union(waves, .get_ds_waves(data, ds))
    waves <- sort(waves)
    choices <- c(setNames(waves, waves), "No wave / Cross-sectional" = "no_wave")
    updateSelectizeInput(session, "filter_waves", choices = choices,
                         selected = character(0), server = TRUE)
  }

  observeEvent(input$filter_datasets, {
    rv$filter_datasets <- input$filter_datasets %||% character(0)
    .refresh_wave_choices(all_vars(), rv$filter_datasets)
    updateSelectizeInput(session, "filter_waves", selected = character(0))
  }, ignoreNULL = FALSE)

  observeEvent(input$filter_waves, {
    rv$filter_waves <- input$filter_waves %||% character(0)
  }, ignoreNULL = FALSE)

  # Chained domain → subdomain
  observeEvent(input$filter_domain, {
    data <- all_vars()
    dom  <- input$filter_domain
    if (is.null(data) || is.null(dom) || dom == "" || !"subdomain" %in% names(data)) {
      updateSelectInput(session, "filter_subdomain",
                        choices = c("All subdomains" = ""), selected = "")
      return()
    }
    subs <- sort(unique(data$subdomain[!is.na(data$domain) & data$domain == dom &
                                         !is.na(data$subdomain)]))
    updateSelectInput(session, "filter_subdomain",
                      choices = c("All subdomains" = "", setNames(subs, subs)), selected = "")
  })

  # Select all / clear links
  observeEvent(input$sel_all_ds, {
    data <- all_vars()
    if (is.null(data)) return()
    updateSelectizeInput(session, "filter_datasets",
                         selected = sort(unique(data$dataset)))
  })
  observeEvent(input$clr_ds, {
    updateSelectizeInput(session, "filter_datasets", selected = character(0))
  })
  observeEvent(input$sel_all_wv, {
    data <- all_vars()
    if (is.null(data)) return()
    active_ds <- rv$filter_datasets
    if (length(active_ds) == 0) active_ds <- sort(unique(data$dataset))
    waves <- character(0)
    for (ds in active_ds) waves <- union(waves, .get_ds_waves(data, ds))
    all_opts <- c(sort(waves), "no_wave")
    updateSelectizeInput(session, "filter_waves", selected = all_opts)
  })
  observeEvent(input$clr_wv, {
    updateSelectizeInput(session, "filter_waves", selected = character(0))
  })
  observeEvent(input$clear_all_filters, {
    data <- all_vars()
    updateSelectizeInput(session, "filter_datasets", selected = character(0))
    updateSelectizeInput(session, "filter_waves",   selected = character(0))
    updateSelectInput(session, "filter_domain",    selected = "")
    updateSelectInput(session, "filter_subdomain", selected = "")
    updateTextInput(session, "filter_search", value = "")
  })

  # ── Filtered variables ──────────────────────────────────────────────────────

  filtered_vars <- reactive({
    data <- all_vars()
    if (is.null(data)) return(NULL)

    ds_sel <- rv$filter_datasets
    active_ds <- if (length(ds_sel) == 0) sort(unique(data$dataset)) else ds_sel
    data <- data[!is.na(data$dataset) & data$dataset %in% active_ds, , drop = FALSE]
    if (nrow(data) == 0) { data$wave_display <- character(0); return(data) }

    wv_sel        <- rv$filter_waves
    has_wave_cols <- all(c("has_wave", "wave_label_std", "base_var") %in% names(data))

    if (length(wv_sel) == 0) {
      # All waves: collapse wave variants to base_var rows
      if (has_wave_cols) {
        non_wave <- data[is.na(data$has_wave) | !data$has_wave, , drop = FALSE]
        non_wave$wave_display    <- "—"
        non_wave$waves_available <- ""
        wave_rows <- data[!is.na(data$has_wave) & data$has_wave, , drop = FALSE]
        if (nrow(wave_rows) > 0) {
          grp_key   <- paste(wave_rows$dataset, wave_rows$base_var, sep = "|||")
          collapsed <- lapply(split(wave_rows, grp_key), function(g) {
            lbls <- sort(unique(g$wave_label_std[!is.na(g$wave_label_std)]))
            r    <- g[1L, , drop = FALSE]
            r$var_name        <- r$base_var
            r$var_label       <- sub("^[A-Za-z]+\\d+[_\\s]+", "", r$var_label, perl = TRUE)
            r$wave_display    <- paste(lbls, collapse = ", ")
            r$waves_available <- paste(lbls, collapse = ", ")
            r
          })
          data <- rbind(non_wave, do.call(rbind, collapsed))
        } else {
          data <- non_wave
        }
      } else {
        data$wave_display <- "—"; data$waves_available <- ""
      }
    } else {
      include_no_wave <- "no_wave" %in% wv_sel
      specific_waves  <- setdiff(wv_sel, "no_wave")

      if (has_wave_cols) {
        wave_part <- if (length(specific_waves) > 0)
          data[!is.na(data$has_wave) & data$has_wave &
                 !is.na(data$wave_label_std) & data$wave_label_std %in% specific_waves, , drop = FALSE]
        else
          data[0L, , drop = FALSE]

        nw_part <- if (include_no_wave)
          data[is.na(data$has_wave) | !data$has_wave, , drop = FALSE]
        else
          data[0L, , drop = FALSE]

        if (nrow(wave_part) > 0) {
          av_full <- all_vars()
          wave_part$wave_display <- ifelse(is.na(wave_part$wave_label_std), "—",
                                           wave_part$wave_label_std)
          wave_part$waves_available <- vapply(seq_len(nrow(wave_part)), function(i) {
            bv  <- wave_part$base_var[i]; ds <- wave_part$dataset[i]
            if (is.na(bv)) return("")
            bv_r <- av_full[!is.na(av_full$dataset) & av_full$dataset == ds &
                              !is.na(av_full$base_var) & av_full$base_var == bv &
                              !is.na(av_full$has_wave) & av_full$has_wave, ]
            paste(sort(unique(bv_r$wave_label_std[!is.na(bv_r$wave_label_std)])),
                  collapse = ", ")
          }, character(1L))
        }
        if (nrow(nw_part) > 0) {
          nw_part$wave_display <- "—"; nw_part$waves_available <- ""
        }
        data <- rbind(wave_part, nw_part)
      } else {
        data$wave_display <- "—"; data$waves_available <- ""
        if (!include_no_wave) data <- data[0L, , drop = FALSE]
      }
    }

    if (nrow(data) == 0) return(data)

    # Domain / subdomain filter
    dom <- input$filter_domain %||% ""
    if (nchar(dom) > 0 && "domain" %in% names(data)) {
      data <- data[!is.na(data$domain) & data$domain == dom, , drop = FALSE]
      sub  <- input$filter_subdomain %||% ""
      if (nchar(sub) > 0 && "subdomain" %in% names(data))
        data <- data[!is.na(data$subdomain) & data$subdomain == sub, , drop = FALSE]
    }

    # Search
    q <- trimws(input$filter_search %||% "")
    if (nchar(q) > 0) {
      hit <- grepl(q, data$var_name, ignore.case = TRUE) |
             grepl(q, data$var_label, ignore.case = TRUE)
      if ("base_var" %in% names(data))
        hit <- hit | grepl(q, data$base_var, ignore.case = TRUE)
      data <- data[hit, , drop = FALSE]
    }

    # Sort: Dataset → wave_display → var_name
    if (nrow(data) > 0) {
      wdisp <- if ("wave_display" %in% names(data)) data$wave_display else ""
      data  <- data[order(data$dataset, wdisp, data$var_name, na.last = TRUE), ]
    }
    data
  })

  # ── Filter summary + count ──────────────────────────────────────────────────

  output$vars_count_label <- renderUI({
    n <- nrow(filtered_vars() %||% data.frame())
    tags$span(class = "small text-muted",
              tags$strong(format(n, big.mark = ",")), " variables")
  })

  output$filter_summary <- renderUI({
    data   <- all_vars()
    fdata  <- filtered_vars()
    ds_sel <- rv$filter_datasets
    wv_sel <- rv$filter_waves
    dom    <- input$filter_domain    %||% ""
    sub    <- input$filter_subdomain %||% ""
    q      <- trimws(input$filter_search %||% "")

    is_default <- length(ds_sel) == 0 && length(wv_sel) == 0 &&
                  dom == "" && sub == "" && q == ""
    if (is_default) return(NULL)

    parts <- character(0)
    if (length(ds_sel) > 0) parts <- c(parts, paste(ds_sel, collapse = ", "))
    if (length(wv_sel) > 0) {
      wlabels <- ifelse(wv_sel == "no_wave", "Cross-sectional", wv_sel)
      parts <- c(parts, paste0("(", paste(wlabels, collapse = ", "), ")"))
    }
    if (nchar(dom) > 0) parts <- c(parts, dom)
    if (nchar(sub) > 0) parts <- c(parts, sub)
    if (nchar(q)   > 0) parts <- c(parts, paste0('"', q, '"'))

    n <- nrow(fdata %||% data.frame())
    tags$div(
      class = "small text-muted mb-2 d-flex justify-content-between align-items-center",
      tags$span(
        tags$strong(format(n, big.mark = ",")), " variables",
        if (length(parts) > 0) paste0(" · Showing: ", paste(parts, collapse = " · "))
      ),
      actionLink("clear_all_filters", "Clear all filters", class = "small text-danger ms-2")
    )
  })

  # ── Sidebar ────────────────────────────────────────────────────────────────

  output$pipeline_warning <- renderUI({
    if (pipeline_loaded()) return(NULL)
    tags$div(class = "alert alert-warning small py-2 mb-2",
             icon("triangle-exclamation"), " ",
             HTML("<strong>Running without enriched metadata</strong> — domain labels and harmonisation suggestions unavailable. Run <code>dict_pipeline</code> to enable full features."))
  })

  output$status_msg <- renderUI({
    data <- all_vars()
    if (is.null(data)) return(tags$p(class = "text-muted fst-italic small", "Loading..."))
    badge <- if (pipeline_loaded())
      tags$span(class = "badge bg-success ms-1 small", "pipeline")
    else
      tags$span(class = "badge bg-secondary ms-1 small", "dct only")
    tagList(
      tags$p(class = "mb-1",
             tags$strong(length(unique(data$dataset))), "datasets ·",
             tags$strong(nrow(data)), "variables", badge),
      tags$ul(class = "small text-muted ps-3 mb-0",
              lapply(sort(unique(data$dataset)), tags$li))
    )
  })

  output$plan_status <- renderUI({
    plan    <- harmonisation_plan()
    n_total <- sum(lengths(plan))
    if (n_total == 0)
      return(tags$p(class = "text-muted small fst-italic", "Plan is empty."))
    tagList(
      tags$p(class = "mb-1", "Plan ",
             tags$span(class = "badge bg-success rounded-pill", n_total), " variables"),
      tags$ul(class = "small text-muted ps-3 mb-0",
              lapply(names(plan), function(ds)
                tags$li(paste0(ds, ": ", length(plan[[ds]])))))
    )
  })

  # ── Variable table ──────────────────────────────────────────────────────────

  output$vars_table <- DT::renderDataTable({
    data <- filtered_vars()
    plan <- harmonisation_plan()

    col_names <- c("Dataset", "Wave", "Variable Name", "Label", "Type",
                   "Domain", "Subdomain", "In Plan")
    if (is.null(data) || nrow(data) == 0) {
      msg <- if (is.null(data) || nrow(all_vars() %||% data.frame()) == 0)
        "Select at least one dataset above to browse variables"
      else
        "No variables match your current filters"
      empty <- setNames(as.data.frame(matrix(character(0), ncol = 8)), col_names)
      return(DT::datatable(empty, rownames = FALSE,
                            options = list(dom = "t",
                              language = list(emptyTable = msg))))
    }

    bv_col <- if ("base_var" %in% names(data))
      ifelse(is.na(data$base_var), data$var_name, data$base_var)
    else data$var_name

    in_plan <- mapply(
      function(ds, bv) isTRUE(!is.null(plan[[ds]]) && bv %in% plan[[ds]]),
      data$dataset, bv_col, SIMPLIFY = TRUE
    )

    dt_data <- data.frame(
      Dataset      = data$dataset,
      Wave         = if ("wave_display" %in% names(data)) data$wave_display else "—",
      `Variable Name` = data$var_name,
      Label        = data$var_label,
      Type         = if ("var_type"   %in% names(data)) data$var_type   %||% "" else "",
      Domain       = if ("domain"     %in% names(data)) data$domain     %||% "" else "",
      Subdomain    = if ("subdomain"  %in% names(data)) data$subdomain  %||% "" else "",
      `In Plan`    = ifelse(in_plan, "✓", ""),
      check.names  = FALSE, stringsAsFactors = FALSE
    )

    dt <- DT::datatable(
      dt_data,
      selection  = list(mode = "single", target = "row"),
      rownames   = FALSE,
      class      = "table-hover table-sm",
      extensions = "FixedHeader",
      options    = list(
        pageLength    = -1,
        scrollY       = "calc(100vh - 330px)",
        scrollCollapse = TRUE,
        fixedHeader   = TRUE,
        dom           = "t",
        autoWidth     = TRUE,
        columnDefs    = list(
          list(width = "10%", targets = 0),
          list(width = "8%",  targets = 1),
          list(width = "12%", targets = 2),
          list(width = "34%", targets = 3),
          list(width = "6%",  targets = 4),
          list(width = "12%", targets = 5),
          list(width = "13%", targets = 6),
          list(width = "5%",  className = "text-center text-success fw-bold", targets = 7)
        ),
        rowCallback = DT::JS(
          "function(row, data) {
             if (data[7] === '✓') $(row).addClass('tbl-row-inplan');
           }"
        )
      )
    )
    dt
  }, server = FALSE)

  # Row click → open detail panel
  observeEvent(input$vars_table_rows_selected, {
    rows <- input$vars_table_rows_selected
    if (is.null(rows) || length(rows) == 0) return()
    data <- filtered_vars()
    if (is.null(data) || nrow(data) == 0) return()
    sel  <- data[rows[1L], , drop = FALSE]
    rv$selected_var_key <- paste(sel$dataset[1L], sel$var_name[1L], sep = "|||")
    rv$preview_key      <- NULL
    rv$detail_open      <- TRUE
  })

  # "Add selected to plan" button (above table)
  observeEvent(input$add_to_plan, {
    rows <- input$vars_table_rows_selected
    if (is.null(rows) || length(rows) == 0) {
      showNotification("Click a row to select a variable first.", type = "warning"); return()
    }
    data <- filtered_vars()
    if (is.null(data)) return()
    sel  <- data[rows[1L], , drop = FALSE]
    ds   <- sel$dataset[1L]
    bv   <- if ("base_var" %in% names(sel) && !is.na(sel$base_var[1L]))
              sel$base_var[1L] else sel$var_name[1L]
    plan <- harmonisation_plan()
    if (is.null(plan[[ds]])) plan[[ds]] <- character(0)
    if (!bv %in% plan[[ds]]) {
      plan[[ds]] <- c(plan[[ds]], bv)
      waves_note <- if ("waves_available" %in% names(sel) && !is.na(sel$waves_available[1L]) &&
                        nchar(sel$waves_available[1L]) > 0)
        paste0(" (all waves: ", sel$waves_available[1L], ")") else ""
      showNotification(paste0("Added ", bv, " from ", ds, waves_note, "."),
                       type = "message", duration = 4)
    } else {
      showNotification(paste0(bv, " already in plan."), type = "message", duration = 2)
    }
    harmonisation_plan(plan)
  })

  # ── Detail panel reactives ──────────────────────────────────────────────────

  .parse_key <- function(key) {
    if (is.null(key)) return(NULL)
    parts <- strsplit(key, "|||", fixed = TRUE)[[1]]
    if (length(parts) < 2) return(NULL)
    list(ds = parts[1L], vn = parts[2L])
  }

  detail_var <- reactive({
    key <- rv$preview_key %||% rv$selected_var_key
    kv  <- .parse_key(key)
    if (is.null(kv)) return(NULL)
    av <- all_vars()
    if (is.null(av)) return(NULL)
    row <- av[!is.na(av$dataset) & av$dataset == kv$ds &
                !is.na(av$var_name) & av$var_name == kv$vn, , drop = FALSE]
    if (nrow(row) == 0 && "base_var" %in% names(av)) {
      row <- av[!is.na(av$dataset) & av$dataset == kv$ds &
                  !is.na(av$base_var) & av$base_var == kv$vn, , drop = FALSE]
    }
    if (nrow(row) == 0) return(NULL)
    row[1L, , drop = FALSE]
  })

  primary_matches <- reactive({
    kv <- .parse_key(rv$selected_var_key)
    if (is.null(kv)) return(NULL)
    hc <- harm_candidates()
    if (is.null(hc) || nrow(hc) == 0) return(NULL)
    av <- all_vars()

    m <- hc[!is.na(hc$dataset) & hc$dataset == kv$ds &
              !is.na(hc$var_name) & hc$var_name == kv$vn, , drop = FALSE]

    # Try base_var lookup if no direct match
    if (nrow(m) == 0 && !is.null(av) && "base_var" %in% names(av)) {
      av_row <- av[!is.na(av$dataset) & av$dataset == kv$ds &
                     !is.na(av$var_name) & av$var_name == kv$vn, , drop = FALSE]
      if (nrow(av_row) > 0 && !is.na(av_row$base_var[1L]))
        m <- hc[!is.na(hc$dataset) & hc$dataset == kv$ds &
                  !is.na(hc$var_name) & hc$var_name == av_row$base_var[1L], , drop = FALSE]
    }
    if (nrow(m) == 0) return(NULL)

    cid     <- m$cluster_id[1L]
    members <- hc[!is.na(hc$cluster_id) & hc$cluster_id == cid, , drop = FALSE]
    others  <- members[!(members$dataset == kv$ds & members$var_name == kv$vn), , drop = FALSE]
    list(cluster_id = cid, members = members, others = others)
  })

  # ── Detail panel UI ─────────────────────────────────────────────────────────

  output$detail_panel <- renderUI({
    if (!rv$detail_open) {
      return(div(
        class = "text-center text-muted small mt-5 p-3",
        icon("hand-pointer"), br(), "Click any variable row to see details and suggested matches"
      ))
    }

    dv   <- detail_var()
    pm   <- primary_matches()
    plan <- harmonisation_plan()
    av   <- all_vars()
    lbl  <- all_labels()
    hc   <- harm_candidates()
    vm   <- var_mapping()
    kv   <- .parse_key(rv$selected_var_key)

    if (is.null(dv)) return(div(class = "text-muted small mt-3", "Variable not found."))

    ds   <- dv$dataset[1L]
    vn   <- dv$var_name[1L]
    bv   <- if ("base_var"       %in% names(dv) && !is.na(dv$base_var[1L]))       dv$base_var[1L]       else vn
    wave <- if ("wave_label_std" %in% names(dv) && !is.na(dv$wave_label_std[1L])) dv$wave_label_std[1L] else NULL
    in_plan <- isTRUE(!is.null(plan[[ds]]) && bv %in% plan[[ds]])

    # Value labels
    val_labs_ui <- if ("value_labels" %in% names(dv) && !is.na(dv$value_labels[1L]) &&
                       nchar(dv$value_labels[1L]) > 0 && !is.null(lbl) && nrow(lbl) > 0) {
      rows <- lbl[lbl$dataset == ds & lbl$label_set == dv$value_labels[1L], ]
      if (nrow(rows) > 0) {
        rows <- rows[order(rows$value), ]
        div(class = "small p-2 bg-light rounded mb-2",
          tags$strong("Response options:"),
          tags$ul(class = "mb-0 ps-3 mt-1",
            lapply(seq_len(nrow(rows)), function(i)
              tags$li(paste0(rows$value[i], " = ", rows$label_text[i]))))
        )
      }
    }

    # ── Section A ──
    section_a <- div(
      class = "pb-3 mb-3 border-bottom",
      div(class = "d-flex justify-content-between align-items-start mb-2",
        div(
          tags$code(class = "fs-6 d-block", bv),
          div(class = "mt-1",
            tags$span(class = "badge bg-secondary me-1", ds),
            if (!is.null(wave)) tags$span(class = "badge bg-info text-dark", wave)
          )
        ),
        tags$button("×", class = "btn btn-sm btn-outline-secondary",
                    onclick = "Shiny.setInputValue('close_panel', Math.random())")
      ),
      tags$p(class = "mb-2 fw-semibold", dv$var_label[1L]),
      div(class = "small text-muted mb-2",
        if ("var_type"  %in% names(dv) && !is.na(dv$var_type[1L]))
          tags$span(class = "me-3", tags$strong("Type: "), dv$var_type[1L]),
        if ("domain"    %in% names(dv) && !is.na(dv$domain[1L]))
          tags$span(class = "me-3", tags$strong("Domain: "), dv$domain[1L]),
        if ("subdomain" %in% names(dv) && !is.na(dv$subdomain[1L]))
          tags$span(tags$strong("Subdomain: "), dv$subdomain[1L])
      ),
      val_labs_ui,
      if (in_plan)
        actionButton("detail_add_btn", "✓ In plan — click to remove",
                     class = "btn-success btn-sm w-100")
      else
        actionButton("detail_add_btn", "＋ Add this variable to plan",
                     class = "btn-outline-success btn-sm w-100")
    )

    # ── Section B — cross-dataset matches ──
    section_b <- if (is.null(hc)) {
      div(class = "text-muted small fst-italic mb-3",
          "Cross-dataset matching unavailable — run the dictionary pipeline to enable suggested matches.")
    } else if (is.null(pm) || nrow(pm$others) == 0) {
      div(class = "text-muted small fst-italic mb-3",
          "No matching variables found in other datasets. This variable appears to be dataset-specific.")
    } else {
      others  <- pm$others
      n_total <- nrow(pm$members)
      n_ds_total <- length(unique(pm$members$dataset))

      add_all_btn <- if (n_ds_total >= 3)
        div(class = "mb-3",
          tags$button(
            class = "btn btn-sm btn-info w-100",
            onclick = sprintf(
              "Shiny.setInputValue('add_all_cluster','%s',{priority:'event'})",
              pm$cluster_id
            ),
            icon("plus"), sprintf(" Add all %d matching variables (%d datasets)",
                                  n_total, n_ds_total)
          )
        )

      cards <- lapply(seq_len(nrow(others)), function(i) {
        r     <- others[i, , drop = FALSE]
        r_ds  <- r$dataset[1L]; r_vn <- r$var_name[1L]
        compat <- .compatibility(r$harmonisation_status %||% "")
        safe_key <- gsub("'", "\\'", paste(r_ds, r_vn, sep = "|||"), fixed = TRUE)

        # Wave coverage for this match
        r_bv <- if ("base_var" %in% names(r) && !is.na(r$base_var[1L])) r$base_var[1L] else r_vn
        r_waves <- character(0)
        if (!is.null(av) && "base_var" %in% names(av)) {
          bv_r <- av[!is.na(av$dataset) & av$dataset == r_ds &
                       !is.na(av$base_var) & av$base_var == r_bv &
                       !is.na(av$has_wave) & av$has_wave, ]
          r_waves <- sort(unique(bv_r$wave_label_std[!is.na(bv_r$wave_label_std)]))
        }

        r_in_plan <- isTRUE(!is.null(plan[[r_ds]]) && r_bv %in% plan[[r_ds]])

        div(class = "card mb-2 match-card",
          div(class = "card-body py-2 px-3",
            div(class = "d-flex justify-content-between align-items-start mb-1",
              div(
                tags$strong(r_ds),
                if (length(r_waves) > 0)
                  lapply(r_waves, function(w)
                    tags$span(class = "badge bg-light text-dark border ms-1", w))
              ),
              div(class = "d-flex gap-1",
                tags$button("Preview", class = "btn btn-xs btn-outline-secondary",
                            onclick = sprintf(
                              "Shiny.setInputValue('preview_click','%s',{priority:'event'})",
                              safe_key)),
                if (r_in_plan)
                  tags$button("✓ Added", class = "btn btn-xs btn-outline-success",
                              disabled = NA)
                else
                  tags$button("Add ✓", class = "btn btn-xs btn-outline-success",
                              onclick = sprintf(
                                "Shiny.setInputValue('add_match_click','%s',{priority:'event'})",
                                safe_key))
              )
            ),
            tags$code(class = "small", r_vn), br(),
            tags$span(class = "small text-muted", r$var_label[1L]), br(),
            tags$span(class = paste0("small text-", compat$col), "● "),
            tags$span(class = "small", compat$txt)
          )
        )
      })

      tagList(
        tags$p(class = "fw-semibold small mb-0", icon("link"),
               " This variable across other datasets"),
        tags$p(class = "small text-muted mb-2",
               "Click any match to preview it, then add to your plan"),
        add_all_btn,
        cards
      )
    }

    # ── Section C — plan status ──
    section_c <- if (!is.null(pm) && nrow(vm) > 0) {
      all_keys <- paste(pm$members$dataset, pm$members$var_name)
      vm_keys  <- paste(vm$dataset, vm$var_name)
      matched  <- vm[vm_keys %in% all_keys, , drop = FALSE]
      harm_names <- unique(matched$harmonised_name[!is.na(matched$harmonised_name) &
                                                     nchar(matched$harmonised_name) > 0])
      if (length(harm_names) > 0) {
        div(class = "mt-3 pt-3 border-top small",
          tags$strong("In your harmonisation plan as:"),
          tags$ul(class = "mb-1 ps-3 mt-1",
            lapply(harm_names, function(hn) tags$li(tags$code(hn)))),
          tags$a(href = "#", class = "small",
                 onclick = "Shiny.setInputValue('go_to_plan', Math.random()); return false;",
                 icon("arrow-right"), " Go to plan")
        )
      }
    }

    # Assemble
    div(
      class = "border rounded p-3 detail-panel"  ,
      section_a,
      section_b,
      section_c
    )
  })

  # ── Detail panel observers ─────────────────────────────────────────────────

  observeEvent(input$close_panel, {
    rv$selected_var_key <- NULL
    rv$preview_key      <- NULL
    rv$detail_open      <- FALSE
  })

  observeEvent(input$preview_click, {
    rv$preview_key <- input$preview_click
  })

  observeEvent(input$detail_add_btn, {
    dv <- detail_var()
    if (is.null(dv)) return()
    ds   <- dv$dataset[1L]
    bv   <- if ("base_var" %in% names(dv) && !is.na(dv$base_var[1L])) dv$base_var[1L] else dv$var_name[1L]
    plan <- harmonisation_plan()
    if (is.null(plan[[ds]])) plan[[ds]] <- character(0)
    if (bv %in% plan[[ds]]) {
      plan[[ds]] <- setdiff(plan[[ds]], bv)
      if (length(plan[[ds]]) == 0) plan[[ds]] <- NULL
      showNotification(paste0("Removed ", bv, " from plan."), type = "message", duration = 3)
    } else {
      plan[[ds]] <- c(plan[[ds]], bv)
      showNotification(paste0("Added ", bv, " from ", ds, "."), type = "message", duration = 3)
    }
    harmonisation_plan(plan)
  })

  observeEvent(input$add_match_click, {
    kv   <- .parse_key(input$add_match_click)
    if (is.null(kv)) return()
    av   <- all_vars()
    plan <- harmonisation_plan()
    bv   <- kv$vn
    if (!is.null(av) && "base_var" %in% names(av)) {
      r <- av[!is.na(av$dataset) & av$dataset == kv$ds &
                !is.na(av$var_name) & av$var_name == kv$vn, , drop = FALSE]
      if (nrow(r) > 0 && !is.na(r$base_var[1L])) bv <- r$base_var[1L]
    }
    if (is.null(plan[[kv$ds]])) plan[[kv$ds]] <- character(0)
    if (!bv %in% plan[[kv$ds]]) {
      plan[[kv$ds]] <- c(plan[[kv$ds]], bv)
      showNotification(paste0("Added ", bv, " from ", kv$ds, "."), type = "message", duration = 3)
    }
    harmonisation_plan(plan)
  })

  observeEvent(input$add_all_cluster, {
    cid <- input$add_all_cluster
    hc  <- harm_candidates()
    if (is.null(hc) || is.null(cid)) return()
    members <- hc[!is.na(hc$cluster_id) & hc$cluster_id == cid, , drop = FALSE]
    av   <- all_vars()
    plan <- harmonisation_plan()
    n_added <- 0L
    for (i in seq_len(nrow(members))) {
      ds <- members$dataset[i]; vn <- members$var_name[i]
      bv <- vn
      if (!is.null(av) && "base_var" %in% names(av)) {
        r <- av[!is.na(av$dataset) & av$dataset == ds &
                  !is.na(av$var_name) & av$var_name == vn, , drop = FALSE]
        if (nrow(r) > 0 && !is.na(r$base_var[1L])) bv <- r$base_var[1L]
      }
      if (is.null(plan[[ds]])) plan[[ds]] <- character(0)
      if (!bv %in% plan[[ds]]) { plan[[ds]] <- c(plan[[ds]], bv); n_added <- n_added + 1L }
    }
    harmonisation_plan(plan)
    showNotification(paste0(n_added, " variable(s) added from cluster ", cid, "."),
                     type = "message", duration = 3)
  })

  observeEvent(input$go_to_plan, {
    updateTabsetPanel(session, "main_tabs", selected = "Build Harmonisation Plan")
  })

  # ── Tab 2: Build Harmonisation Plan ────────────────────────────────────────

  observeEvent(harmonisation_plan(), {
    plan <- harmonisation_plan()
    av   <- all_vars()
    if (length(plan) == 0 || is.null(av)) {
      var_mapping(data.frame(
        row_id = integer(), dataset = character(), wave = character(),
        var_name = character(), base_var = character(),
        var_label = character(), var_type = character(),
        value_labels = character(), harmonised_name = character(),
        harmonised_label = character(), recode_status = character(),
        notes = character(), in_plan = logical(), stringsAsFactors = FALSE))
      recode_data(list())
      return()
    }

    new_m <- do.call(rbind, lapply(names(plan), function(ds)
      data.frame(dataset = ds, var_name = plan[[ds]], stringsAsFactors = FALSE)))

    # Join labels/type from av
    av_lkp <- .av_label_lookup(av)
    join_cols <- intersect(c("dataset", "var_name", "var_label", "value_labels", "var_type"),
                           names(av_lkp))
    new_m <- merge(new_m, av_lkp[, join_cols, drop = FALSE],
                   by = c("dataset", "var_name"), all.x = TRUE)

    if (!"var_label"    %in% names(new_m)) new_m$var_label    <- ""
    if (!"value_labels" %in% names(new_m)) new_m$value_labels <- ""
    if (!"var_type"     %in% names(new_m)) new_m$var_type     <- NA_character_
    new_m$var_label[is.na(new_m$var_label)]       <- ""
    new_m$value_labels[is.na(new_m$value_labels)] <- ""

    # Wave display string + base_var
    if (all(c("has_wave", "base_var", "wave_label_std") %in% names(av))) {
      new_m$wave <- vapply(seq_len(nrow(new_m)), function(i) {
        ds <- new_m$dataset[i]; bv <- new_m$var_name[i]
        wv <- av[!is.na(av$dataset) & av$dataset == ds &
                   !is.na(av$base_var) & av$base_var == bv &
                   !is.na(av$has_wave) & av$has_wave, ]
        ws <- sort(unique(wv$wave_label_std[!is.na(wv$wave_label_std)]))
        if (length(ws) == 0L) return(NA_character_)
        if (length(ws) <= 3L) paste(ws, collapse = ", ")
        else paste0(ws[1L], "–", ws[length(ws)])
      }, character(1L))
      new_m$base_var <- vapply(seq_len(nrow(new_m)), function(i) {
        ds <- new_m$dataset[i]; vn <- new_m$var_name[i]
        r  <- av[!is.na(av$dataset) & av$dataset == ds &
                   !is.na(av$var_name) & av$var_name == vn, , drop = FALSE]
        if (nrow(r) > 0L && "base_var" %in% names(r) && !is.na(r$base_var[1L]))
          r$base_var[1L] else vn
      }, character(1L))
    } else {
      new_m$wave     <- NA_character_
      new_m$base_var <- new_m$var_name
    }

    cur <- var_mapping()

    # Preserve harmonised_name from old mapping
    if (nrow(cur) > 0L && "harmonised_name" %in% names(cur)) {
      old_hn <- cur[, c("dataset", "var_name", "harmonised_name"), drop = FALSE]
      new_m  <- merge(new_m, old_hn, by = c("dataset", "var_name"),
                      all.x = TRUE, suffixes = c("", ".old"))
      if ("harmonised_name.old" %in% names(new_m)) {
        new_m$harmonised_name <- ifelse(
          is.na(new_m$harmonised_name.old), new_m$var_name,
          new_m$harmonised_name.old)
        new_m$harmonised_name.old <- NULL
      }
    } else {
      new_m$harmonised_name <- new_m$var_name
    }

    # Preserve/default extended columns
    new_m <- sync_var_mapping_extended(new_m, cur)

    # Assign/preserve row_ids
    if (!"row_id" %in% names(new_m)) new_m$row_id <- NA_integer_
    if (nrow(cur) > 0L && "row_id" %in% names(cur)) {
      old_rid <- cur[, c("dataset", "var_name", "row_id"), drop = FALSE]
      new_m <- merge(new_m, old_rid, by = c("dataset", "var_name"),
                     all.x = TRUE, suffixes = c("", ".old"))
      if ("row_id.old" %in% names(new_m)) {
        new_m$row_id <- ifelse(is.na(new_m$row_id.old),
                               new_m$row_id, new_m$row_id.old)
        new_m$row_id.old <- NULL
      }
    }
    new_needs_id <- is.na(new_m$row_id)
    if (any(new_needs_id)) {
      max_id <- if (nrow(cur) > 0L && "row_id" %in% names(cur))
        max(cur$row_id, na.rm = TRUE) else 0L
      new_m$row_id[new_needs_id] <-
        seq(max_id + 1L, max_id + sum(new_needs_id))
    }

    new_m$in_plan <- TRUE
    new_m <- new_m[order(new_m$dataset, new_m$var_name), ]
    rownames(new_m) <- NULL

    keep_cols <- c("row_id", "dataset", "wave", "var_name", "base_var",
                   "var_label", "var_type", "value_labels",
                   "harmonised_name", "harmonised_label",
                   "recode_status", "notes", "in_plan")
    for (col in keep_cols)
      if (!col %in% names(new_m)) new_m[[col]] <- NA
    new_m <- new_m[, keep_cols, drop = FALSE]

    # Drop recode_data for removed variables
    new_keys <- paste(new_m$dataset, new_m$var_name, sep = "|||")
    rd <- recode_data()
    removed <- setdiff(names(rd), new_keys)
    if (length(removed) > 0L) { rd[removed] <- NULL; recode_data(rd) }

    var_mapping(new_m)
  }, ignoreNULL = FALSE)

  observeEvent(input$clear_plan, {
    harmonisation_plan(list())
    recode_data(list())
    showNotification("Harmonisation plan cleared.", type = "message", duration = 3)
  })

  # ── Tab 3: Generate R Code ─────────────────────────────────────────────────

  output$generated_code <- renderText({
    generate_harmonisation_code(var_mapping(), all_labels(), all_vars(),
                                recode_rules_tbl(), recode_data())
  })

  output$code_status <- renderUI({
    m <- var_mapping()
    if (is.null(m) || nrow(m) == 0)
      return(tags$div(class = "alert alert-secondary small py-2 mb-3",
                      "Add variables to the harmonisation plan to generate code."))
    ds_counts <- tapply(m$dataset, m$harmonised_name, function(x) length(unique(x)))
    n_single  <- sum(ds_counts == 1); n_matched <- sum(ds_counts > 1)
    n_confirmed <- if ("recode_status" %in% names(m))
      sum(m$recode_status == "confirmed", na.rm = TRUE) else 0L
    rr_note <- if (n_confirmed > 0)
      paste0(" <strong>", n_confirmed, " confirmed recode(s)</strong> will be applied.")
    else ""
    if (n_single == 0)
      tags$div(class = "alert alert-success small py-2 mb-3",
               HTML(paste0("<strong>", n_matched,
                           " harmonised variable(s)</strong> matched across datasets. Code is ready.",
                           rr_note)))
    else
      tags$div(class = "alert alert-warning small py-2 mb-3",
               HTML(paste0("<strong>", n_single,
                           " variable(s)</strong> map to one dataset only.", rr_note)))
  })

  output$download_code <- downloadHandler(
    filename = function() paste0("harmonisation_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".R"),
    content  = function(file)
      writeLines(generate_harmonisation_code(var_mapping(), all_labels(), all_vars(),
                                              recode_rules_tbl(), recode_data()), file)
  )

  # ── Tab 2: mount plan server module ────────────────────────────────────────
  setup_plan_server(
    input, output, session,
    all_vars, all_labels, harm_candidates, recode_rules_tbl, pipeline_loaded,
    harmonisation_plan, var_mapping, recode_data
  )
}

shinyApp(ui, server)

library(shiny)
library(bslib)
library(DT)

source("R/parse_dct.R")
source("R/code_generator.R")

# ---------------------------------------------------------------------------
# Helper: build display data frame for the plan table.
# Columns 2 (Original Variable, tooltip HTML) and 5 (Remove button HTML)
# are raw HTML — use with escape = c(-2L, -5L).
# ---------------------------------------------------------------------------

make_plan_display <- function(m, all_labels = NULL) {
  if (nrow(m) == 0) {
    return(data.frame(
      Dataset             = character(),
      `Original Variable` = character(),
      `Original Label`    = character(),
      `Harmonised Name`   = character(),
      Remove              = character(),
      check.names = FALSE, stringsAsFactors = FALSE
    ))
  }

  has_vlabs <- "value_labels" %in% names(m)
  lbl_ready <- has_vlabs && !is.null(all_labels) && nrow(all_labels) > 0

  var_col <- sapply(seq_len(nrow(m)), function(i) {
    vn   <- htmltools::htmlEscape(m$var_name[i])
    lset <- if (has_vlabs && !is.na(m$value_labels[i]) &&
                nchar(m$value_labels[i]) > 0) m$value_labels[i] else NULL
    if (is.null(lset)) return(vn)

    tip <- if (lbl_ready) {
      rows <- all_labels[all_labels$dataset == m$dataset[i] &
                           all_labels$label_set == lset, ]
      if (nrow(rows) == 0) {
        paste0("Label set: ", lset)
      } else {
        rows <- rows[order(rows$value), ]
        paste0(lset, ": ",
               paste(rows$value, rows$label_text, sep = " = ", collapse = ", "))
      }
    } else {
      paste0("Label set: ", lset)
    }

    paste0('<span title="', htmltools::htmlEscape(tip),
           '" style="cursor:help">', vn,
           ' <small class="text-muted">ⓘ</small></span>')
  })

  data.frame(
    Dataset             = m$dataset,
    `Original Variable` = var_col,
    `Original Label`    = m$var_label,
    `Harmonised Name`   = m$harmonised_name,
    Remove = paste0(
      '<button class="btn btn-sm btn-outline-danger rm-plan-btn"',
      ' data-ds="', htmltools::htmlEscape(m$dataset), '"',
      ' data-vn="', htmltools::htmlEscape(m$var_name), '"',
      ' title="Remove from plan">&#10005;</button>'
    ),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------

ui <- page_sidebar(
  title = "Data Harmonisation Assistant",
  theme = bs_theme(bootswatch = "flatly"),

  tags$head(
    tags$link(rel = "stylesheet", href = "custom.css"),
    tags$script(src = "custom.js")
  ),

  sidebar = sidebar(
    width = 280,
    uiOutput("status_msg"),
    hr(class = "my-2"),
    uiOutput("plan_status")
  ),

  # Collapsible instructions
  accordion(
    id    = "help_accordion",
    open  = FALSE,
    class = "mb-3",
    accordion_panel(
      "How to use this app",
      icon = icon("circle-info"),
      tags$ol(
        class = "small mb-0",
        tags$li(
          tags$strong("Browse Variables"), " — search and filter variables. ",
          "Select rows and click ", tags$em("Add to Harmonisation Plan"), "."
        ),
        tags$li(
          tags$strong("Build Harmonisation Plan"), " — edit the ",
          tags$em("Harmonised Name"), " column to map variables across datasets. ",
          "Variables sharing a harmonised name will be stacked on ",
          tags$code("bind_rows()"), ". ",
          "Green groups span ≥2 datasets; amber groups are single-dataset only."
        ),
        tags$li(
          tags$strong("Generate R Code"), " — review and download the complete R ",
          "script. Set ", tags$code("data_folder"),
          " at the top of the script to your data directory before running."
        )
      )
    )
  ),

  navset_tab(
    id = "main_tabs",

    # ---- Tab 1: Browse Variables ------------------------------------------
    nav_panel(
      "Browse Variables",

      layout_columns(
        col_widths = c(4, 8),
        fill       = FALSE,
        gap        = "0.75rem",
        class      = "mb-3 mt-1",

        textInput(
          "var_search",
          label       = NULL,
          placeholder = "Search name or label..."
        ),

        checkboxGroupInput(
          "dataset_filter",
          label   = NULL,
          choices = NULL,
          inline  = TRUE
        )
      ),

      card(
        card_body(class = "p-0",
          DT::dataTableOutput("vars_table")
        )
      ),

      actionButton(
        "add_to_plan",
        label = "Add to Harmonisation Plan",
        icon  = icon("plus"),
        class = "btn-primary mt-2"
      )
    ),

    # ---- Tab 2: Build Harmonisation Plan ----------------------------------
    nav_panel(
      "Build Harmonisation Plan",

      layout_columns(
        col_widths = c(7, 5),
        gap        = "1rem",

        # Left: editable mapping table
        card(
          card_header(
            class = "d-flex justify-content-between align-items-center",
            tags$span("Variable Mapping"),
            tags$div(
              class = "d-flex gap-2 align-items-center",
              tags$span(
                class = "text-muted small fw-normal me-1",
                "Click Harmonised Name to edit"
              ),
              actionButton(
                "reset_names",
                label = NULL,
                icon  = icon("rotate-left"),
                title = "Reset all harmonised names to original variable names",
                class = "btn btn-sm btn-outline-secondary"
              ),
              actionButton(
                "clear_plan",
                label = NULL,
                icon  = icon("trash"),
                title = "Remove all variables from plan",
                class = "btn btn-sm btn-outline-danger"
              )
            )
          ),
          tags$div(class = "px-3", uiOutput("duplicate_warning")),
          card_body(class = "p-0",
            DT::dataTableOutput("plan_table")
          )
        ),

        # Right: grouped summary with match highlighting
        card(
          card_header("Mapping Summary"),
          card_body(
            style = "max-height: 70vh; overflow-y: auto;",
            uiOutput("mapping_summary")
          )
        )
      )
    ),

    # ---- Tab 3: Generate R Code -------------------------------------------
    nav_panel(
      "Generate R Code",

      uiOutput("code_status"),

      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          tags$span("Generated R Script"),
          tags$div(
            class = "d-flex gap-2",
            tags$button(
              id      = "copy_code_btn",
              class   = "btn btn-sm btn-outline-secondary",
              onclick = "copyGeneratedCode()",
              HTML("&#128203; Copy")
            ),
            downloadButton(
              "download_code",
              label = "Download .R",
              class = "btn-sm btn-outline-primary"
            )
          )
        ),
        card_body(
          class = "p-0",
          verbatimTextOutput("generated_code")
        )
      )
    )
  )
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------

server <- function(input, output, session) {

  # ---------------------------------------------------------------------------
  # Persistent reactive state
  # ---------------------------------------------------------------------------

  all_vars   <- reactiveVal(NULL)
  all_labels <- reactiveVal(
    data.frame(dataset = character(), label_set = character(),
               value = integer(), label_text = character(),
               stringsAsFactors = FALSE)
  )
  harmonisation_plan <- reactiveVal(list())

  var_mapping <- reactiveVal(
    data.frame(
      dataset         = character(),
      var_name        = character(),
      var_label       = character(),
      value_labels    = character(),
      harmonised_name = character(),
      stringsAsFactors = FALSE
    )
  )

  # ---------------------------------------------------------------------------
  # Startup: load all .dct dictionaries and value label definitions
  # ---------------------------------------------------------------------------

  observe({
    tryCatch(
      {
        data   <- load_all_dcts("dct/")
        labels <- load_all_dct_labels("dct/")
        all_vars(data)
        all_labels(labels)
        datasets <- sort(unique(data$dataset))
        updateCheckboxGroupInput(
          session, "dataset_filter",
          choices  = datasets,
          selected = datasets
        )
      },
      error = function(e) {
        showNotification(
          paste("Failed to load .dct files:", conditionMessage(e)),
          type = "error", duration = NULL
        )
      }
    )
  })

  # ---------------------------------------------------------------------------
  # Sidebar outputs
  # ---------------------------------------------------------------------------

  output$status_msg <- renderUI({
    data <- all_vars()
    if (is.null(data)) {
      return(tags$p(class = "text-muted fst-italic small", "Loading..."))
    }
    tagList(
      tags$p(
        class = "mb-1",
        tags$strong(length(unique(data$dataset))), "datasets ·",
        tags$strong(nrow(data)), "variables"
      ),
      tags$ul(
        class = "small text-muted ps-3 mb-0",
        lapply(sort(unique(data$dataset)), tags$li)
      )
    )
  })

  output$plan_status <- renderUI({
    plan    <- harmonisation_plan()
    n_total <- sum(lengths(plan))
    if (n_total == 0) {
      return(tags$p(class = "text-muted small fst-italic", "Plan is empty."))
    }
    tagList(
      tags$p(
        class = "mb-1",
        "Plan ",
        tags$span(class = "badge bg-success rounded-pill", n_total),
        " variables"
      ),
      tags$ul(
        class = "small text-muted ps-3 mb-0",
        lapply(names(plan), function(ds) {
          tags$li(paste0(ds, ": ", length(plan[[ds]])))
        })
      )
    )
  })

  # ---------------------------------------------------------------------------
  # Tab 1: Browse Variables
  # ---------------------------------------------------------------------------

  filtered_vars <- reactive({
    data  <- all_vars()
    if (is.null(data)) return(NULL)
    sel_ds <- input$dataset_filter
    if (is.null(sel_ds) || length(sel_ds) == 0) return(data[0L, ])
    data <- data[data$dataset %in% sel_ds, ]
    q <- trimws(input$var_search)
    if (nchar(q) > 0) {
      hit  <- grepl(q, data$var_name,  ignore.case = TRUE) |
              grepl(q, data$var_label, ignore.case = TRUE)
      data <- data[hit, ]
    }
    data
  })

  output$vars_table <- DT::renderDataTable({
    data <- filtered_vars()
    plan <- harmonisation_plan()
    empty <- data.frame(
      ` ` = character(), Dataset = character(),
      `Variable Name` = character(), Type = character(),
      Label = character(), check.names = FALSE
    )
    if (is.null(data) || nrow(data) == 0) {
      return(DT::datatable(empty, rownames = FALSE, options = list(dom = "i")))
    }
    in_plan <- mapply(
      function(ds, vn) !is.null(plan[[ds]]) && vn %in% plan[[ds]],
      data$dataset, data$var_name, SIMPLIFY = TRUE
    )
    DT::datatable(
      data.frame(
        ` `             = ifelse(in_plan, "✓", ""),
        Dataset         = data$dataset,
        `Variable Name` = data$var_name,
        Type            = data$var_type,
        Label           = data$var_label,
        check.names     = FALSE
      ),
      selection = list(mode = "multiple", target = "row"),
      rownames  = FALSE,
      class     = "table-hover table-sm",
      options   = list(
        pageLength = 25, dom = "tip", autoWidth = TRUE,
        columnDefs = list(
          list(width = "4%",  className = "text-success text-center fw-bold",
               targets = 0),
          list(width = "16%", targets = 1),
          list(width = "16%", targets = 2),
          list(width = "8%",  targets = 3),
          list(width = "56%", targets = 4)
        )
      )
    )
  }, server = FALSE)

  observeEvent(input$add_to_plan, {
    rows <- input$vars_table_rows_selected
    if (is.null(rows) || length(rows) == 0) {
      showNotification("Select at least one row first.", type = "warning")
      return()
    }
    data   <- filtered_vars()
    if (is.null(data)) return()
    chosen <- data[rows, , drop = FALSE]
    plan   <- harmonisation_plan()
    n_added <- 0L
    for (i in seq_len(nrow(chosen))) {
      ds <- chosen$dataset[i]
      vn <- chosen$var_name[i]
      if (is.null(plan[[ds]])) plan[[ds]] <- character()
      if (!vn %in% plan[[ds]]) {
        plan[[ds]] <- c(plan[[ds]], vn)
        n_added    <- n_added + 1L
      }
    }
    harmonisation_plan(plan)
    n_skip <- length(rows) - n_added
    showNotification(
      if (n_skip > 0)
        paste0(n_added, " added (", n_skip, " duplicate(s) skipped).")
      else
        paste0(n_added, " variable(s) added to plan."),
      type = "message", duration = 3
    )
  })

  # ---------------------------------------------------------------------------
  # Tab 2: Build Harmonisation Plan
  # ---------------------------------------------------------------------------

  # Sync harmonisation_plan → var_mapping.
  # Preserves any harmonised_name values already edited by the user.
  observeEvent(harmonisation_plan(), {
    plan <- harmonisation_plan()
    av   <- all_vars()

    if (length(plan) == 0 || is.null(av)) {
      var_mapping(
        data.frame(
          dataset = character(), var_name = character(),
          var_label = character(), value_labels = character(),
          harmonised_name = character(),
          stringsAsFactors = FALSE
        )
      )
      return()
    }

    # Flatten plan list → data frame
    new_m <- do.call(rbind, lapply(names(plan), function(ds) {
      data.frame(dataset = ds, var_name = plan[[ds]],
                 stringsAsFactors = FALSE)
    }))

    # Pull variable labels and value label set names from all_vars
    new_m <- merge(
      new_m,
      av[, c("dataset", "var_name", "var_label", "value_labels")],
      by = c("dataset", "var_name"), all.x = TRUE
    )
    new_m$var_label[is.na(new_m$var_label)]       <- ""
    new_m$value_labels[is.na(new_m$value_labels)] <- ""

    # Preserve any harmonised_name values already edited by the user
    cur <- var_mapping()
    if (nrow(cur) > 0) {
      new_m <- merge(
        new_m,
        cur[, c("dataset", "var_name", "harmonised_name")],
        by = c("dataset", "var_name"), all.x = TRUE
      )
    } else {
      new_m$harmonised_name <- NA_character_
    }

    # Default: harmonised_name = var_name for any new entry
    no_name <- is.na(new_m$harmonised_name)
    new_m$harmonised_name[no_name] <- new_m$var_name[no_name]

    new_m <- new_m[order(new_m$dataset, new_m$var_name),
                   c("dataset", "var_name", "var_label", "value_labels",
                     "harmonised_name")]
    rownames(new_m) <- NULL
    var_mapping(new_m)
  }, ignoreNULL = FALSE)

  # Plan mapping table.
  # ordering = FALSE guarantees info$row from cell_edit maps to the
  # original data frame index (avoids mismatch after user column sorts).
  output$plan_table <- DT::renderDataTable({
    DT::datatable(
      make_plan_display(var_mapping(), all_labels()),
      escape   = c(-2L, -5L),   # cols 2 (tooltip HTML) and 5 (Remove) unescaped
      rownames = FALSE,
      class    = "table-hover table-sm",
      editable = list(
        target  = "cell",
        disable = list(columns = c(0L, 1L, 2L, 4L))  # 0-indexed; only col 3 editable
      ),
      options  = list(
        pageLength = 25,
        dom        = "tip",
        ordering   = FALSE,
        autoWidth  = TRUE,
        columnDefs = list(
          list(width = "15%",                    targets = 0),
          list(width = "15%",                    targets = 1),
          list(width = "33%",                    targets = 2),
          list(width = "27%",                    targets = 3),
          list(width = "10%", orderable = FALSE, targets = 4)
        )
      )
    )
  }, server = FALSE)

  # Commit a Harmonised Name cell edit into var_mapping.
  # Sanitises via make.names() and notifies the user if the name was changed.
  observeEvent(input$plan_table_cell_edit, {
    info <- input$plan_table_cell_edit
    if (info$col != 3L) return()
    m <- var_mapping()
    if (info$row < 1L || info$row > nrow(m)) return()
    new_val <- trimws(as.character(info$value))
    if (nchar(new_val) == 0) {
      new_val <- m$var_name[info$row]
    } else {
      clean <- make.names(new_val)
      if (clean != new_val) {
        showNotification(
          paste0('Sanitised to valid R identifier: "', clean, '"'),
          type = "warning", duration = 5
        )
        new_val <- clean
      }
    }
    m$harmonised_name[info$row] <- new_val
    var_mapping(m)
  })

  # Remove a variable from the plan (triggered by JS in custom.js).
  # Only updates harmonisation_plan; the sync observer updates var_mapping.
  observeEvent(input$remove_plan_var, {
    ds   <- input$remove_plan_var$ds
    vn   <- input$remove_plan_var$vn
    plan <- harmonisation_plan()
    plan[[ds]] <- setdiff(plan[[ds]], vn)
    if (length(plan[[ds]]) == 0L) plan[[ds]] <- NULL
    harmonisation_plan(plan)
  })

  observeEvent(input$clear_plan, {
    harmonisation_plan(list())
    showNotification("Harmonisation plan cleared.", type = "message", duration = 3)
  })

  observeEvent(input$reset_names, {
    m <- var_mapping()
    if (is.null(m) || nrow(m) == 0) return()
    m$harmonised_name <- m$var_name
    var_mapping(m)
    showNotification("Harmonised names reset to original variable names.",
                     type = "message", duration = 3)
  })

  # Red alert when two variables in the same dataset share a harmonised name.
  output$duplicate_warning <- renderUI({
    m <- var_mapping()
    if (is.null(m) || nrow(m) == 0) return(NULL)
    key   <- paste(m$dataset, m$harmonised_name, sep = "")
    dupes <- m[duplicated(key) | duplicated(key, fromLast = TRUE), , drop = FALSE]
    if (nrow(dupes) == 0) return(NULL)
    conflicts <- unique(paste0(
      htmltools::htmlEscape(dupes$dataset), ": <code>",
      htmltools::htmlEscape(dupes$harmonised_name), "</code>"
    ))
    tags$div(
      class = "alert alert-danger small py-2 mb-0 mt-2",
      HTML(paste0(
        "<strong>Duplicate harmonised names in same dataset:</strong> ",
        paste(conflicts, collapse = "; "),
        ". Each harmonised name must be unique within a dataset."
      ))
    )
  })

  # Mapping summary: group by harmonised_name, colour-code by dataset count.
  output$mapping_summary <- renderUI({
    m   <- var_mapping()
    lbl <- all_labels()
    if (is.null(m) || nrow(m) == 0) {
      return(tags$p(class = "text-muted fst-italic small mt-1",
                    "Add variables in the Browse tab to start building a plan."))
    }

    m      <- m[order(m$harmonised_name, m$dataset, m$var_name), ]
    groups <- split(m, factor(m$harmonised_name,
                              levels = unique(m$harmonised_name)))

    tagList(lapply(names(groups), function(hname) {
      grp  <- groups[[hname]]
      n_ds <- length(unique(grp$dataset))

      if (n_ds > 1) {
        border_cls <- "border-success"
        bg_cls     <- "bg-success bg-opacity-10"
        badge_cls  <- "bg-success"
        badge_lbl  <- paste(n_ds, "datasets ✓")
      } else {
        border_cls <- "border-warning"
        bg_cls     <- "bg-warning bg-opacity-10"
        badge_cls  <- "bg-warning text-dark"
        badge_lbl  <- "1 dataset"
      }

      var_items <- lapply(seq_len(nrow(grp)), function(j) {
        vn   <- grp$var_name[j]
        ds   <- grp$dataset[j]
        lset <- if ("value_labels" %in% names(grp) &&
                    !is.na(grp$value_labels[j]) &&
                    nchar(grp$value_labels[j]) > 0) grp$value_labels[j] else NULL

        lbl_tag <- if (!is.null(lset) && !is.null(lbl) && nrow(lbl) > 0) {
          rows <- lbl[lbl$dataset == ds & lbl$label_set == lset, ]
          tip  <- if (nrow(rows) > 0) {
            rows <- rows[order(rows$value), ]
            paste0(lset, ": ",
                   paste(rows$value, rows$label_text, sep = "=", collapse = ", "))
          } else {
            paste0("Label set: ", lset)
          }
          tags$span(
            class = "text-muted ms-1",
            style = "cursor:help; font-size:0.75em;",
            title = tip, "ⓘ"
          )
        } else if (!is.null(lset)) {
          tags$span(
            class = "text-muted ms-1",
            style = "font-size:0.75em;",
            paste0("[", lset, "]")
          )
        } else NULL

        tags$li(paste0(ds, "  →  ", vn), lbl_tag)
      })

      tags$div(
        class = paste("mb-2 p-2 rounded border text-dark", border_cls, bg_cls),
        tags$div(
          class = "d-flex justify-content-between align-items-center",
          tags$code(class = "small text-dark", hname),
          tags$span(class = paste("badge rounded-pill", badge_cls), badge_lbl)
        ),
        tags$ul(class = "mb-0 mt-1 small ps-3 text-dark", var_items)
      )
    }))
  })

  # ---------------------------------------------------------------------------
  # Tab 3: Generate R Code
  # ---------------------------------------------------------------------------

  output$generated_code <- renderText({
    generate_harmonisation_code(var_mapping(), all_labels())
  })

  output$code_status <- renderUI({
    m <- var_mapping()
    if (is.null(m) || nrow(m) == 0) {
      return(tags$div(
        class = "alert alert-secondary small py-2 mb-3",
        "Add variables to the harmonisation plan to generate code."
      ))
    }
    ds_counts <- tapply(m$dataset, m$harmonised_name,
                        function(x) length(unique(x)))
    n_single  <- sum(ds_counts == 1)
    n_matched <- sum(ds_counts > 1)

    if (n_single == 0) {
      tags$div(
        class = "alert alert-success small py-2 mb-3",
        HTML(paste0(
          "<strong>", n_matched,
          " harmonised variable(s)</strong> matched across datasets. ",
          "Code is ready to run."
        ))
      )
    } else {
      tags$div(
        class = "alert alert-warning small py-2 mb-3",
        HTML(paste0(
          "<strong>", n_single,
          " variable(s)</strong> map to only one dataset ",
          "(shown amber in the plan). ",
          "These will be included but won't contribute to cross-dataset harmonisation."
        ))
      )
    }
  })

  output$download_code <- downloadHandler(
    filename = function() {
      paste0("harmonisation_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".R")
    },
    content = function(file) {
      writeLines(generate_harmonisation_code(var_mapping(), all_labels()), file)
    }
  )
}

shinyApp(ui, server)

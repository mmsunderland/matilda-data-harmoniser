# ── Required packages ─────────────────────────────────────────────────────────
# All library() calls must be explicit so Posit Connect Cloud
# detects them during dependency scanning
library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(purrr)
library(htmltools)
library(shinyjs)
library(jsonlite)
library(yaml)
library(stringdist)
library(tidytext)
library(Matrix)
library(httr2)
library(cli)
library(igraph)
library(SnowballC)
# ── End required packages ──────────────────────────────────────────────────────

source("R/parse_dct.R")
source("R/code_generator.R")

# ── Constants ──────────────────────────────────────────────────────────────────
.PIPELINE_DIR   <- normalizePath(file.path("output", "final"), mustWork = FALSE)
.DL_PATH        <- file.path(.PIPELINE_DIR, "domain_labels.csv")
.HC_PATH        <- file.path(.PIPELINE_DIR, "harmonisation_candidates.csv")
.RR_PATH        <- file.path(.PIPELINE_DIR, "recode_rules.csv")

.pipeline_available <- function() file.exists(.DL_PATH) && file.exists(.HC_PATH)

# ── Helpers ────────────────────────────────────────────────────────────────────
make_harmonised_name <- function(lbl) {
  nm <- tolower(trimws(lbl))
  nm <- gsub("[^a-z0-9]+", "_", nm)
  nm <- gsub("_+", "_", nm)
  nm <- substr(nm, 1L, 35L)
  nm <- sub("_$", "", nm); nm <- sub("^_", "", nm)
  make.names(nm)
}

is_valid_r_name <- function(x) {
  !is.null(x) && !is.na(x) && nchar(x) > 0 &&
    grepl("^[a-zA-Z.][a-zA-Z0-9_.]*$", x) &&
    !grepl("^\\.$", x)
}

# Safe scalar string: returns default if x is NULL, length-0, or NA
.s <- function(x, default = "") {
  if (is.null(x) || length(x) == 0L) return(default)
  x <- x[1L]
  if (is.na(x)) default else as.character(x)
}

badge_status <- function(hn, ds, assignments) {
  a <- assignments[assignments$harmonised_name == hn & assignments$dataset == ds, , drop=FALSE]
  if (nrow(a) == 0) return("unreviewed")
  if (isTRUE(a$excluded[1L])) return("excluded")
  st <- a$recode_status[1L]
  if (is.null(st) || is.na(st)) "unreviewed" else st
}

badge_html <- function(hn, all_datasets, assignments, lkp = NULL) {
  paste(sapply(all_datasets, function(ds) {
    if (!is.null(lkp)) {
      key <- paste0(hn, "|||", ds)
      st  <- if (key %in% names(lkp$status)) lkp$status[[key]] else "unreviewed"
      bv  <- if (key %in% names(lkp$bv) && !is.na(lkp$bv[[key]])) lkp$bv[[key]] else "?"
    } else {
      st <- badge_status(hn, ds, assignments)
      a  <- assignments[assignments$harmonised_name == hn & assignments$dataset == ds, , drop=FALSE]
      bv <- if (nrow(a) > 0 && !is.null(a$base_var) && !is.na(a$base_var[1L])) a$base_var[1L] else "?"
    }
    tip <- switch(st,
      confirmed    = paste0(ds, ": ", bv, " — Confirmed ✓"),
      pending      = paste0(ds, ": ", bv, " — Recode pending ⚠"),
      incompatible = paste0(ds, ": ", bv, " — Incompatible ✗"),
      excluded     = paste0(ds, ": Excluded (click Edit/Review to include)"),
      unreviewed   = paste0(ds, ": Not yet assigned"),
      notfound     = paste0(ds, ": No match found"),
      paste0(ds, ": Unknown")
    )
    paste0(
      '<span class="ds-badge ds-', st, '" ',
      'title="', htmltools::htmlEscape(tip), '">',
      htmltools::htmlEscape(toupper(substr(ds, 1L, 6L))),
      '</span>'
    )
  }), collapse = " ")
}

construct_row_html <- function(con, all_datasets, assignments, selected = character(0), lkp = NULL) {
  hn  <- .s(con$harmonised_name)
  hl  <- .s(con$harmonised_label)
  sub <- .s(con$subdomain)

  is_selected <- hn %in% selected
  checked_attr <- if (is_selected) ' checked="checked"' else ''

  badges  <- badge_html(hn, all_datasets, assignments, lkp)
  safe_hn <- htmltools::htmlEscape(hn)

  paste0(
    '<div class="construct-row', if (is_selected) ' cr-selected' else '', '" id="cr-', safe_hn, '">',
    '<div class="cr-top">',
    '<input type="checkbox" class="me-2 construct-cb" data-hn="', safe_hn, '"', checked_attr,
    ' title="Tick to mark this construct as selected — use \'Show selected only\' to filter"',
    ' onchange="syncConstructSelections()">',
    '<span class="cr-name" data-field="name" data-hn="', safe_hn, '">', safe_hn, '</span>',
    '<span class="cr-label ms-2" data-field="label" data-hn="', safe_hn, '">',
    htmltools::htmlEscape(hl), '</span>',
    '<div class="cr-actions">',
    '<button class="btn btn-xs btn-outline-primary" ',
    'title="Open detail panel to assign variables and check response codes" ',
    'onclick="Shiny.setInputValue(\'edit_construct\',\'', safe_hn, '\',{priority:\'event\'})">',
    'Edit / Review &#8594;</button>',
    '<button class="btn btn-xs btn-outline-danger ms-1" ',
    'title="Remove this construct from the plan" ',
    'onclick="Shiny.setInputValue(\'remove_construct\',\'', safe_hn, '\',{priority:\'event\'})">',
    'Remove</button>',
    '</div></div>',
    '<div class="cr-bottom">',
    '<div class="ds-badge-wrap">', badges, '</div>',
    '</div></div>'
  )
}

load_pipeline_constructs <- function(hc, all_vars) {
  if (is.null(hc) || nrow(hc) == 0) return(NULL)

  # Join base_var from all_vars
  if (!is.null(all_vars) && "base_var" %in% names(all_vars)) {
    lkp <- unique(all_vars[, c("dataset", "var_name", "base_var"), drop=FALSE])
    hc2 <- merge(hc, lkp, by = c("dataset", "var_name"), all.x = TRUE)
    hc2$eff_var <- ifelse(!is.na(hc2$base_var), hc2$base_var, hc2$var_name)
  } else {
    hc2 <- hc; hc2$eff_var <- hc2$var_name
  }

  # One row per cluster for constructs
  cids  <- unique(hc2$cluster_id)
  cmeta <- do.call(rbind, lapply(cids, function(cid) {
    rows <- hc2[hc2$cluster_id == cid, , drop=FALSE]
    rows[1L, c("cluster_id", "candidate_label", "domain", "subdomain", "harmonisation_status"),
         drop=FALSE]
  }))

  cmeta$harmonised_name  <- make_harmonised_name(cmeta$candidate_label)
  # Deduplicate
  dups <- which(duplicated(cmeta$harmonised_name))
  if (length(dups) > 0)
    cmeta$harmonised_name[dups] <- paste0(cmeta$harmonised_name[dups], "_", cmeta$cluster_id[dups])

  cmeta$harmonised_label <- tools::toTitleCase(cmeta$candidate_label)

  constructs_df <- data.frame(
    harmonised_name  = cmeta$harmonised_name,
    harmonised_label = cmeta$harmonised_label,
    domain           = ifelse(is.na(cmeta$domain), "Uncategorised", cmeta$domain),
    subdomain        = ifelse(is.na(cmeta$subdomain), NA_character_, cmeta$subdomain),
    notes            = NA_character_,
    stringsAsFactors = FALSE
  )

  # Assignments: one per cluster × dataset × base_var
  asgn_rows <- hc2[, c("cluster_id", "dataset", "eff_var", "harmonisation_status"), drop=FALSE]
  asgn_rows <- asgn_rows[!duplicated(paste(asgn_rows$cluster_id, asgn_rows$dataset, asgn_rows$eff_var)), ]
  nm_map    <- setNames(cmeta$harmonised_name, cmeta$cluster_id)
  asgn_rows$harmonised_name <- nm_map[as.character(asgn_rows$cluster_id)]

  assignments_df <- data.frame(
    harmonised_name = asgn_rows$harmonised_name,
    dataset         = asgn_rows$dataset,
    base_var        = asgn_rows$eff_var,
    recode_status   = ifelse(asgn_rows$harmonisation_status == "ready",     "confirmed",
                      ifelse(asgn_rows$harmonisation_status == "recodable", "pending",
                                                                             "unreviewed")),
    excluded        = FALSE,
    stringsAsFactors = FALSE
  )
  list(constructs = constructs_df, assignments = assignments_df)
}

wave_coverage_summary <- function(hn, assignments, all_vars) {
  asgn <- assignments[assignments$harmonised_name == hn & !assignments$excluded, , drop=FALSE]
  if (nrow(asgn) == 0 || is.null(all_vars) || !"has_wave" %in% names(all_vars)) return(NULL)
  lines <- character(0)
  for (i in seq_len(nrow(asgn))) {
    ds <- asgn$dataset[i]; bv <- asgn$base_var[i]
    wrows <- all_vars[all_vars$dataset == ds & !is.na(all_vars$has_wave) & all_vars$has_wave &
                        !is.na(all_vars$base_var) & all_vars$base_var == bv, ]
    waves <- sort(unique(wrows$wave_label_std[!is.na(wrows$wave_label_std)]))
    lines <- c(lines, paste0(ds, ": ", if (length(waves) == 0) "cross-sectional"
                             else paste(waves, collapse = " ")))
  }
  lines
}

get_value_labels <- function(ds, bv, all_vars, all_labels) {
  if (is.null(all_vars) || !"value_labels" %in% names(all_vars)) return(NULL)
  if (is.null(bv) || is.na(bv) || nchar(bv) == 0) return(NULL)
  av <- all_vars[!is.na(all_vars$dataset) & all_vars$dataset == ds &
                   (!is.na(all_vars$var_name) & all_vars$var_name == bv |
                    "base_var" %in% names(all_vars) & !is.na(all_vars$base_var) & all_vars$base_var == bv), ]
  if (nrow(av) == 0) return(NULL)
  lset <- av$value_labels[!is.na(av$value_labels) & nchar(av$value_labels) > 0]
  if (length(lset) == 0) return(NULL)
  lb <- all_labels[all_labels$dataset == ds & all_labels$label_set == lset[1L], ]
  if (nrow(lb) == 0) return(NULL)
  lb[order(lb$value), ]
}

# Fast version using a pre-built labels index (O(1) lookup vs O(n) scan)
get_value_labels_fast <- function(ds, bv, all_vars, labels_index) {
  if (is.null(all_vars) || is.null(labels_index)) return(NULL)
  if (is.null(bv) || is.na(bv) || nchar(bv) == 0) return(NULL)
  av <- all_vars[
    !is.na(all_vars$dataset) & all_vars$dataset == ds &
    (!is.na(all_vars$var_name) & all_vars$var_name == bv |
     "base_var" %in% names(all_vars) & !is.na(all_vars$base_var) &
     all_vars$base_var == bv), ]
  if (nrow(av) == 0) return(NULL)
  lset <- av$value_labels[!is.na(av$value_labels) & nchar(av$value_labels) > 0]
  if (length(lset) == 0) return(NULL)
  key <- paste0(ds, "|||", lset[1L])
  lb  <- labels_index[[key]]
  if (is.null(lb) || nrow(lb) == 0) return(NULL)
  lb[order(lb$value), ]
}

determine_recode_status <- function(hn, new_ds, new_bv,
                                     current_assignments,
                                     all_vars, all_labels) {
  # Returns "confirmed", "pending", or "unreviewed"
  # "confirmed" = no recode needed (numeric or identical coding)
  # "pending"   = coding differences detected across datasets
  # "unreviewed" = could not determine (missing label data)

  new_labels <- get_value_labels(new_ds, new_bv, all_vars, all_labels)

  # Numeric variable — no response codes — confirm immediately
  if (is.null(new_labels) || nrow(new_labels) == 0) {
    return("confirmed")
  }

  # Get all other currently assigned non-excluded datasets
  others <- current_assignments[
    !is.na(current_assignments$harmonised_name) &
    current_assignments$harmonised_name == hn &
    !is.na(current_assignments$excluded) &
    !current_assignments$excluded &
    !is.na(current_assignments$dataset) &
    current_assignments$dataset != new_ds &
    !is.na(current_assignments$base_var), ]

  # Only one dataset assigned so far — nothing to compare
  if (nrow(others) == 0) return("confirmed")

  new_codes      <- sort(new_labels$value)
  new_lbls_norm  <- tolower(trimws(
    new_labels$label_text[order(new_labels$value)]
  ))

  for (i in seq_len(nrow(others))) {
    other_labels <- get_value_labels(
      others$dataset[i], others$base_var[i], all_vars, all_labels
    )
    # Other dataset has no labels — skip this comparison
    if (is.null(other_labels) || nrow(other_labels) == 0) next

    other_codes     <- sort(other_labels$value)
    other_lbls_norm <- tolower(trimws(
      other_labels$label_text[order(other_labels$value)]
    ))

    if (length(new_codes) != length(other_codes)) return("pending")
    if (!identical(new_codes, other_codes))        return("pending")
    if (!identical(new_lbls_norm, other_lbls_norm)) return("pending")
  }

  return("confirmed")
}

# O(1) lookup into pre-indexed all_vars: returns one row or NULL
lookup_var <- function(avi, ds, vn) {
  if (is.null(avi)) return(NULL)
  key <- paste0(ds, "|||", vn)
  idx <- avi$idx[[key]]
  if (is.null(idx)) return(NULL)
  avi$data[idx[1L], , drop = FALSE]
}

# ── UI ─────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "flatly"),
  tags$head(
    tags$link(rel = "stylesheet", href = "custom.css"),
    tags$script(src = "custom.js"),
    tags$title("Data Harmonisation Assistant")
  ),

  # Fixed header
  div(class = "app-header",
    div(class = "header-left",
      tags$button("🗂 Construct Manager", id = "btn-nav-constructs",
                  class = "nav-btn active",
                  onclick = "Shiny.setInputValue('nav_view','constructs',{priority:'event'}); switchView('constructs');"),
      tags$button("🔍 Variable Search", id = "btn-nav-search",
                  class = "nav-btn",
                  onclick = "Shiny.setInputValue('nav_view','search',{priority:'event'}); switchView('search');")
    ),
    div(class = "header-mid", uiOutput("header_stats", inline = TRUE)),
    div(class = "header-right",
      tags$button("? Help", class = "btn btn-sm btn-outline-secondary",
                  onclick = "Shiny.setInputValue('show_welcome',Math.random(),{priority:'event'})",
                  title = "Show app introduction and how-to guide"),
      actionButton("btn_import_list", "Import list",
                   class = "btn btn-sm btn-outline-secondary"),
      tags$button("</> Preview code", class = "btn btn-sm btn-outline-secondary",
                  onclick = "toggleCodeDrawer()"),
      uiOutput("generate_btn_ui", inline = TRUE)
    )
  ),

  # Main content
  div(id = "app-main-content", class = "app-content",

    # View: Construct Manager
    div(id = "view-constructs",
      uiOutput("construct_manager_ui")
    ),

    # View: Variable Search — filter bar is static so textInput never re-creates
    div(id = "view-search", class = "d-none",
      uiOutput("search_context_banner_ui"),
      div(class = "search-filter-bar",
        div(style = "flex:1; min-width:200px;",
          textInput("search_q", NULL,
                    placeholder = "\U0001F50D Search variable names and labels...",
                    width = "100%")
        ),
        uiOutput("search_ds_ui"),      # renderUI so choices populate even when d-none
        selectInput("search_type_filter", NULL,
                    choices = c("All types" = "", "Numeric" = "numeric",
                                "Categorical" = "categorical"),
                    width = "150px")
      ),
      uiOutput("search_results_count"),
      uiOutput("search_results_ui"),
      uiOutput("search_load_more_ui"),
      uiOutput("standalone_queue_bar")
    )
  ),

  # Code drawer
  div(id = "code-drawer", uiOutput("code_drawer_content")),

  # Detail panel
  div(id = "detail-panel",
    div(class = "dp-handle", title = "Drag to resize"),
    div(class = "dp-header",
      uiOutput("detail_panel_title"),
      tags$button("× Close", class = "btn btn-sm btn-outline-secondary",
                  onclick = "closeDetailPanel()")
    ),
    div(class = "dp-col-headers",
      div(class = "dp-col-hdr", tags$strong("1. Construct settings"),
          tags$span(class = "dp-col-hdr-sub", "Edit name, label, and domain")),
      div(class = "dp-col-hdr", tags$strong("2. Dataset assignments"),
          tags$span(class = "dp-col-hdr-sub", "Confirm which variable each dataset uses")),
      div(class = "dp-col-hdr", tags$strong("3. Response code alignment"),
          tags$span(class = "dp-col-hdr-sub", "Check and align response coding across datasets"))
    ),
    div(class = "dp-body",
      div(class = "dp-col dp-col-1", uiOutput("detail_col1")),
      div(class = "dp-col dp-col-2", uiOutput("detail_col2")),
      div(class = "dp-col dp-col-3", uiOutput("detail_col3"))
    )
  ),

  # Pending queue (search view only)
  div(id = "pending-queue",
    uiOutput("pending_queue_ui")
  ),

)

# ── Server ─────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── Reactive state ───────────────────────────────────────────────────────────
  rv <- reactiveValues(
    constructs = data.frame(
      harmonised_name = character(), harmonised_label = character(),
      domain = character(), subdomain = character(), notes = character(),
      stringsAsFactors = FALSE
    ),
    assignments = data.frame(
      harmonised_name = character(), dataset = character(),
      base_var = character(), recode_status = character(), excluded = logical(),
      stringsAsFactors = FALSE
    ),
    recode_rules = data.frame(
      harmonised_name = character(), dataset = character(),
      base_var = character(), old_code = integer(), old_label = character(),
      new_code = integer(), new_label = character(), confirmed = logical(),
      stringsAsFactors = FALSE
    ),
    all_vars         = NULL,
    all_labels       = NULL,
    hc               = NULL,
    all_datasets     = character(0),
    pipeline_loaded  = FALSE,
    active_construct = NULL,
    search_context   = NULL,
    search_query     = "",
    search_ds_filter = "",
    search_page              = 1L,
    pending_queue            = data.frame(dataset=character(), base_var=character(),
                                          var_label=character(), stringsAsFactors=FALSE),
    welcome_dismissed        = FALSE,
    info_bar_dismissed       = FALSE,
    recode_instructions_seen = FALSE,
    detail_panels_opened     = 0L,
    selected_constructs      = character(0),   # harmonised_names ticked by user
    show_selected_only       = FALSE
  )

  # ── Startup: load data ───────────────────────────────────────────────────────
  .load_test_fixtures <- function() {
    tf    <- file.path("tests", "fixtures")
    av_p  <- file.path(tf, "test_all_vars.rds")
    hc_p  <- file.path(tf, "test_hc.rds")
    if (!file.exists(av_p)) return()
    av <- readRDS(av_p)
    rv$all_vars     <- av
    rv$all_datasets <- sort(unique(av$dataset))
    rv$all_labels   <- data.frame(dataset=character(), label_set=character(),
                                   value=integer(), label_text=character(),
                                   stringsAsFactors=FALSE)
    if (file.exists(hc_p)) {
      rv$hc             <- readRDS(hc_p)
      rv$pipeline_loaded <- TRUE
    }
  }

  observe({
    if (isTRUE(getOption("shiny.testmode"))) { .load_test_fixtures(); return() }
    if (.pipeline_available()) {
      tryCatch({
        dl <- read.csv(.DL_PATH, stringsAsFactors = FALSE, na.strings = c("", "NA"))
        hc <- read.csv(.HC_PATH, stringsAsFactors = FALSE, na.strings = c("", "NA"))
        rv$all_vars <- dl
        rv$hc       <- hc
        rv$all_datasets <- sort(unique(dl$dataset))
        rv$pipeline_loaded <- TRUE
        tryCatch({
          lb <- load_all_dct_labels("dct/")
          rv$all_labels <- lb
        }, error = function(e) NULL)
        # Load pipeline recode rules if non-empty
        if (file.exists(.RR_PATH)) {
          rr <- read.csv(.RR_PATH, stringsAsFactors = FALSE, na.strings = c("", "NA"))
          if (nrow(rr) > 0) {
            # Convert to new format (add harmonised_name, base_var, confirmed later)
          }
        }
      }, error = function(e) {
        showNotification(paste("Pipeline load failed:", conditionMessage(e)),
                         type = "error", duration = 8L)
        .load_dct_fallback()
      })
    } else {
      .load_dct_fallback()
    }

  })

  .load_dct_fallback <- function() {
    tryCatch({
      dl <- load_all_dcts("dct/")
      rv$all_vars <- dl
      rv$all_datasets <- sort(unique(dl$dataset))
      lb <- load_all_dct_labels("dct/")
      rv$all_labels <- lb
    }, error = function(e) {
      showNotification(paste("DCT load failed:", conditionMessage(e)),
                       type = "error", duration = NULL)
    })
  }


  # ── Pre-computed search data (expensive collapse runs once when data loads) ──
  collapsed_vars <- reactive({
    av <- rv$all_vars
    if (is.null(av)) return(NULL)
    if ("base_var" %in% names(av) && "has_wave" %in% names(av)) {
      non_wave <- av[is.na(av$has_wave) | !av$has_wave, , drop=FALSE]
      non_wave$waves_list <- rep("", nrow(non_wave))   # rep() handles 0-row case
      wave_rows <- av[!is.na(av$has_wave) & av$has_wave, , drop=FALSE]
      if (nrow(wave_rows) > 0L) {
        grp_key <- paste(wave_rows$dataset, wave_rows$base_var, sep = "|||")
        collapsed <- do.call(rbind, lapply(split(wave_rows, grp_key), function(g) {
          ws <- sort(unique(g$wave_label_std[!is.na(g$wave_label_std)]))
          r  <- g[1L, , drop=FALSE]
          r$var_name   <- r$base_var
          r$var_label  <- sub("^[A-Za-z]+\\d+[_ ]+", "", r$var_label, perl = TRUE)
          r$waves_list <- paste(ws, collapse = ",")
          r
        }))
        rbind(non_wave, collapsed)
      } else {
        non_wave
      }
    } else {
      av$waves_list <- rep("", nrow(av))
      av
    }
  })

  # Pre-computed hc lookup tables (runs once when rv$hc loads at startup).
  # cid_map and stat_map are named LISTS so [[key]] returns NULL (not error) on miss.
  hc_lookups <- reactive({
    hc <- rv$hc
    if (is.null(hc)) return(NULL)
    var_keys <- paste0(hc$dataset, "|||", hc$var_name)
    list(
      cid_map  = as.list(setNames(as.character(hc$cluster_id), var_keys)),
      stat_map = as.list(setNames(as.character(hc$harmonisation_status), var_keys)),
      members  = split(hc[, c("dataset", "var_name"), drop=FALSE],
                       as.character(hc$cluster_id))
    )
  })

  # O(1) label lookup index: "dataset|||label_set" -> data frame of label rows (FIX 1)
  all_labels_index <- reactive({
    al <- rv$all_labels
    if (is.null(al) || nrow(al) == 0) return(NULL)
    keys <- paste0(al$dataset, "|||", al$label_set)
    split(al, keys)
  })

  # O(1) all_vars lookup index: "dataset|||var_name" -> row indices (FIX 2)
  all_vars_index <- reactive({
    av <- rv$all_vars
    if (is.null(av)) return(NULL)
    keys <- paste0(av$dataset, "|||", av$var_name)
    list(data = av, idx = split(seq_len(nrow(av)), keys))
  })

  # ── Header stats ─────────────────────────────────────────────────────────────
  .construct_statuses <- reactive({
    c_df <- rv$constructs; a_df <- rv$assignments
    if (nrow(c_df) == 0) return(setNames(character(0), character(0)))
    nms <- c_df$harmonised_name
    setNames(vapply(nms, function(hn) {
      a <- a_df[a_df$harmonised_name == hn & !is.na(a_df$excluded) & !a_df$excluded, ]
      if (nrow(a) == 0) return("pending")
      if (any(!is.na(a$recode_status) & a$recode_status == "incompatible")) return("issues")
      if (all(!is.na(a$recode_status) & a$recode_status == "confirmed")) return("complete")
      "pending"
    }, character(1L)), nms)
  })

  output$header_stats <- renderUI({
    c_df <- rv$constructs; nc <- nrow(c_df)
    if (nc == 0) return(tags$span(class = "text-muted small", "No constructs yet — add some below"))

    statuses   <- .construct_statuses()
    n_complete <- sum(statuses == "complete")
    n_pending  <- sum(statuses == "pending")
    n_issues   <- sum(statuses == "issues")

    pct_complete <- if (nc > 0) round(100 * n_complete / nc) else 0
    pct_pending  <- if (nc > 0) round(100 * n_pending  / nc) else 0
    pct_issues   <- if (nc > 0) 100 - pct_complete - pct_pending else 0

    div(
      div(class = "d-flex align-items-center gap-2 justify-content-center",
        tags$span(tags$strong(nc), " constructs ·"),
        tags$a(href = "#", class = "text-success small fw-semibold",
               title = "Click to download — these constructs are fully confirmed",
               onclick = paste0("Shiny.setInputValue('hdr_click','complete',{priority:'event'}); return false;"),
               paste0(n_complete, " complete")),
        tags$span(class = "text-muted", "·"),
        tags$a(href = "#", class = "text-warning small fw-semibold",
               title = "Click to scroll to first pending construct",
               onclick = paste0("Shiny.setInputValue('hdr_click','pending',{priority:'event'}); return false;"),
               paste0(n_pending, " pending")),
        if (n_issues > 0) tagList(
          tags$span(class = "text-muted", "·"),
          tags$a(href = "#", class = "text-danger small fw-semibold",
                 title = "Click to scroll to first construct with issues",
                 onclick = paste0("Shiny.setInputValue('hdr_click','issues',{priority:'event'}); return false;"),
                 paste0(n_issues, " issues"))
        )
      ),
      div(class = "hdr-progress",
        div(class = "hdr-progress-bar bg-success",  style = paste0("width:", pct_complete, "%")),
        div(class = "hdr-progress-bar bg-warning",  style = paste0("width:", pct_pending, "%")),
        div(class = "hdr-progress-bar bg-danger",   style = paste0("width:", max(0, pct_issues), "%"))
      )
    )
  })

  observeEvent(input$hdr_click, {
    type <- input$hdr_click
    statuses <- .construct_statuses()
    c_df     <- rv$constructs
    if (type == "complete") {
      n <- sum(statuses == "complete")
      showNotification(paste0(n, " construct(s) complete and ready for code generation."),
                       type = "message", duration = 4L)
    } else {
      target <- c_df$harmonised_name[statuses == type]
      if (length(target) == 0) return()
      session$sendCustomMessage("scrollToConstruct", target[1L])
      rv$active_construct <- target[1L]
      session$sendCustomMessage("openDetailPanel", TRUE)
    }
  })

  # ── Nav ──────────────────────────────────────────────────────────────────────
  observeEvent(input$nav_view, {
    rv$search_context <- NULL
  })

  # ── Welcome banner ───────────────────────────────────────────────────────────
  output$welcome_banner_ui <- renderUI({
    if (rv$welcome_dismissed) return(NULL)
    div(class = "welcome-banner",
      div(class = "d-flex justify-content-between align-items-start mb-2",
        tags$h5(class = "mb-0", "👋 Welcome to the Data Harmonisation Assistant"),
        tags$button("Got it — don't show again ×",
                    class = "btn btn-sm btn-outline-secondary",
                    onclick = "Shiny.setInputValue('dismiss_welcome',Math.random(),{priority:'event'})")
      ),
      tags$p(class = "mb-2",
             "This app helps you select variables from multiple datasets and generate R code ",
             "to combine them into a single harmonised dataset for analysis."),
      div(class = "welcome-steps",
        div(class = "welcome-step",
          tags$span(class = "step-num", "1"),
          div(tags$strong("Define constructs"), " — name the variables you want to harmonise ",
              "(e.g. k10_total, age, sex). Use the options below or load pipeline suggestions.")
        ),
        div(class = "welcome-step",
          tags$span(class = "step-num", "2"),
          div(tags$strong("Assign variables"), " — for each construct, confirm which variable ",
              "in each dataset measures it. Coloured badges track progress per dataset.")
        ),
        div(class = "welcome-step",
          tags$span(class = "step-num", "3"),
          div(tags$strong("Align response codes"), " — check that coding is consistent across ",
              "datasets and generate recodes where needed.")
        ),
        div(class = "welcome-step",
          tags$span(class = "step-num", "4"),
          div(tags$strong("Download R script"), " — click ⬇ Generate .R, set your data folder ",
              "path, and run the script to produce the harmonised dataset.")
        )
      )
    )
  })

  observeEvent(input$dismiss_welcome, { rv$welcome_dismissed <- TRUE })
  observeEvent(input$show_welcome,    { rv$welcome_dismissed <- FALSE })

  # ── Construct Manager UI ──────────────────────────────────────────────────────
  # Debounce at 400ms so assignment changes don't block the R thread mid-search.
  # All rv$... reads inside are isolated — only construct_ui_trigger creates the dependency.
  construct_ui_trigger <- debounce(reactive({
    list(rv$constructs, rv$assignments, rv$all_datasets,
         rv$selected_constructs, rv$show_selected_only,
         rv$pipeline_loaded, rv$hc,
         rv$detail_panels_opened, rv$info_bar_dismissed)
  }), 400L)

  output$construct_manager_ui <- renderUI({
    construct_ui_trigger()   # only reactive dependency — fires at most every 400ms
    isolate({
      c_df     <- rv$constructs
      a_df     <- rv$assignments
      all_ds   <- rv$all_datasets
      sel      <- rv$selected_constructs
      show_sel <- rv$show_selected_only

      if (nrow(c_df) == 0) {
        return(tagList(uiOutput("welcome_banner_ui"), uiOutput("empty_state_ui")))
      }

      statuses     <- .construct_statuses()
      n_confirmed  <- sum(!is.na(a_df$recode_status) & a_df$recode_status == "confirmed")
      n_constructs <- nrow(c_df)
      n_sel        <- length(sel)

      next_step_ui <- if (rv$detail_panels_opened < 3L) {
        if (n_confirmed == 0L)
          div(class = "next-step-bar pulsing", "👉 Next: click ", tags$strong("Edit / Review →"),
              " on any construct to start assigning variables from each dataset.")
        else if (sum(statuses == "complete") < n_constructs)
          div(class = "next-step-bar", paste0("👉 ", sum(statuses != "complete"),
              " construct(s) still need assignments — click Edit / Review → to continue."))
        else
          div(class = "next-step-bar next-step-done", "✓ All constructs ready — click ",
              tags$strong("⬇ Generate .R"), " to download your harmonisation script.")
      }

      info_bar_ui <- if (!rv$info_bar_dismissed && rv$detail_panels_opened == 0L) {
        div(class = "info-bar",
          "Click ", tags$strong("Edit / Review →"), " on any construct to assign variables, ",
          "check response codes, and review wave coverage.",
          tags$button("×", class = "btn btn-xs btn-link text-muted ms-auto",
                      onclick = "Shiny.setInputValue('dismiss_info_bar',Math.random(),{priority:'event'})")
        )
      }

      display_df <- if (show_sel && n_sel > 0)
        c_df[c_df$harmonised_name %in% sel, , drop=FALSE]
      else c_df

      display_df$domain[is.na(display_df$domain) | display_df$domain == ""] <- "Uncategorised"
      domains <- sort(unique(display_df$domain))

      # O(1) badge lookup
      lkp <- if (nrow(a_df) > 0L) {
        a_keys      <- paste0(a_df$harmonised_name, "|||", a_df$dataset)
        status_vals <- vapply(seq_len(nrow(a_df)), function(i) {
          if (isTRUE(a_df$excluded[i])) "excluded"
          else { st <- a_df$recode_status[i]; if (is.null(st) || is.na(st)) "unreviewed" else st }
        }, character(1L))
        list(status = setNames(status_vals, a_keys),
             bv     = setNames(a_df$base_var, a_keys))
      } else NULL

      rows_html <- character(0)
      for (dom in domains) {
        dom_rows <- display_df[display_df$domain == dom, , drop=FALSE]
        dom_rows <- dom_rows[order(dom_rows$harmonised_name), ]
        dom_id   <- gsub("[^a-zA-Z0-9]", "_", dom)
        rows_html <- c(rows_html, paste0(
          '<div class="domain-group-header" onclick="toggleDomain(event,\'', dom_id, '\')">',
          '<span>&#9660; ', htmltools::htmlEscape(dom), '</span>',
          '<span class="dg-count">', nrow(dom_rows), '</span>',
          '</div>',
          '<div class="domain-rows" id="dr-', dom_id, '">'
        ))
        for (i in seq_len(nrow(dom_rows))) {
          rows_html <- c(rows_html, construct_row_html(dom_rows[i, ], all_ds, a_df, sel, lkp))
        }
        rows_html <- c(rows_html, '</div>')
      }

      tagList(
        uiOutput("welcome_banner_ui"),
        div(class = "cl-toolbar",
          actionButton("btn_add_construct", "+ Add construct",
                       class = "btn btn-sm btn-outline-primary"),
          tags$span(class = "vr mx-1"),
          if (rv$pipeline_loaded && !is.null(rv$hc))
            actionButton("btn_load_pipeline", paste0("Load pipeline clusters (",
                         length(unique(rv$hc$cluster_id)), ")"),
                         class = "btn btn-sm btn-outline-info"),
          tags$span(class = "vr mx-1"),
          downloadButton("btn_export_mapping", "Export CSV",
                         class = "btn btn-sm btn-outline-secondary"),
          tags$span(class = "vr mx-1"),
          if (n_sel > 0)
            actionButton("toggle_selected_only",
                         if (show_sel) paste0("Show all (", n_constructs, ")")
                         else paste0("Show selected only (", n_sel, ")"),
                         class = paste0("btn btn-sm ", if (show_sel) "btn-info" else "btn-outline-info")),
          actionButton("btn_reset_all", "Reset all",
                       class = "btn btn-sm btn-outline-danger",
                       title = "Remove all constructs and assignments — cannot be undone"),
          tags$span(class = "ms-auto text-muted small",
                    paste0(n_constructs, " construct", if (n_constructs != 1) "s",
                           if (n_sel > 0) paste0(" · ", n_sel, " selected")))
        ),
        div(class = "badge-legend",
          tags$span(class = "text-muted small me-2", "Badge colours:"),
          tags$span(class = "ds-badge ds-confirmed  legend-dot", title="Confirmed"),  " Confirmed",
          tags$span(class = "ds-badge ds-pending    legend-dot ms-2", title="Recode pending"), " Pending",
          tags$span(class = "ds-badge ds-incompatible legend-dot ms-2", title="Incompatible"), " Incompatible",
          tags$span(class = "ds-badge ds-unreviewed legend-dot ms-2", title="Not yet assigned"), " Unreviewed",
          tags$span(class = "ds-badge ds-excluded   legend-dot ms-2", title="Excluded"), " Excluded"
        ),
        next_step_ui,
        info_bar_ui,
        div(id = "construct-list-scroll", HTML(paste(rows_html, collapse = "\n")))
      )
    })
  })

  observeEvent(input$dismiss_info_bar, { rv$info_bar_dismissed <- TRUE })

  output$empty_state_ui <- renderUI({
    div(class = "empty-state",
      tags$h4("Start building your harmonisation plan"),

      # Option 1: type constructs
      div(class = "empty-opt",
        tags$h6("Option 1 — Type or paste construct names"),
        div(class = "d-flex gap-2",
          textInput("new_constructs_text", NULL,
                    placeholder = "e.g. k10_total, age, sex, audit_score",
                    width = "100%"),
          actionButton("btn_add_text_constructs", "Add",
                       class = "btn btn-primary btn-sm")
        ),
        tags$small(class = "text-muted", "Comma-separated. You can add labels and domain later.")
      ),

      # Option 2: upload CSV
      div(class = "empty-opt",
        tags$h6("Option 2 — Import from CSV"),
        fileInput("import_constructs_file", NULL,
                  buttonLabel = "Upload CSV", accept = ".csv",
                  width = "300px"),
        tags$small(class = "text-muted",
                   "Columns: harmonised_name (required), harmonised_label, domain, subdomain")
      ),

      if (rv$pipeline_loaded && !is.null(rv$hc)) {
        div(class = "empty-opt",
          tags$h6("Option 3 — Load from pipeline (fastest)"),
          tags$p(class = "small text-muted mb-2",
                 paste0(length(unique(rv$hc$cluster_id)),
                        " clusters found. Loads all with status 'ready' or 'recodable'.")),
          actionButton("btn_load_pipeline", "Load pipeline clusters",
                       class = "btn btn-info btn-sm")
        )
      }
    )
  })

  # ── Add constructs ───────────────────────────────────────────────────────────
  observeEvent(input$btn_add_text_constructs, {
    txt <- trimws(input$new_constructs_text %||% "")
    if (nchar(txt) == 0) return()
    nms <- trimws(strsplit(txt, "[,\n]+")[[1L]])
    nms <- nms[nchar(nms) > 0]
    added <- 0L
    for (nm in nms) {
      hn <- make_harmonised_name(nm)
      if (!hn %in% rv$constructs$harmonised_name) {
        rv$constructs <- rbind(rv$constructs, data.frame(
          harmonised_name = hn, harmonised_label = tools::toTitleCase(gsub("_", " ", nm)),
          domain = "Uncategorised", subdomain = NA_character_, notes = NA_character_,
          stringsAsFactors = FALSE
        ))
        added <- added + 1L
      }
    }
    updateTextInput(session, "new_constructs_text", value = "")
    showNotification(paste0("Added ", added, " construct(s)."), type = "message", duration = 3L)
  })

  observeEvent(input$btn_add_construct, {
    nm <- paste0("construct_", nrow(rv$constructs) + 1L)
    rv$constructs <- rbind(rv$constructs, data.frame(
      harmonised_name = nm, harmonised_label = "New construct",
      domain = "Uncategorised", subdomain = NA_character_, notes = NA_character_,
      stringsAsFactors = FALSE
    ))
    rv$active_construct <- nm
    session$sendCustomMessage("openDetailPanel", TRUE)
  })

  observeEvent(input$import_constructs_file, {
    req(input$import_constructs_file)
    tryCatch({
      tpl <- read.csv(input$import_constructs_file$datapath,
                      stringsAsFactors = FALSE, na.strings = c("", "NA"))
      if (!"harmonised_name" %in% names(tpl)) {
        showNotification("CSV must have 'harmonised_name' column.", type = "error"); return()
      }
      added <- 0L
      for (i in seq_len(nrow(tpl))) {
        hn <- trimws(tpl$harmonised_name[i])
        if (!nchar(hn) || hn %in% rv$constructs$harmonised_name) next
        rv$constructs <- rbind(rv$constructs, data.frame(
          harmonised_name  = hn,
          harmonised_label = if ("harmonised_label" %in% names(tpl)) tpl$harmonised_label[i] else hn,
          domain           = if ("domain"           %in% names(tpl)) tpl$domain[i]           else "Uncategorised",
          subdomain        = if ("subdomain"         %in% names(tpl)) tpl$subdomain[i]        else NA_character_,
          notes            = NA_character_,
          stringsAsFactors = FALSE
        ))
        added <- added + 1L
      }
      showNotification(paste0("Imported ", added, " construct(s)."), type = "message", duration = 3L)
    }, error = function(e) showNotification(paste("Import failed:", conditionMessage(e)), type = "error"))
  })

  observeEvent(input$btn_import_list, {
    # Trigger file input via JS — open a modal
    showModal(modalDialog(
      title = "Import construct list",
      fileInput("import_constructs_file_modal", "Upload CSV",
                buttonLabel = "Choose file", accept = ".csv"),
      tags$small(class = "text-muted",
                 "Required column: harmonised_name. Optional: harmonised_label, domain, subdomain"),
      footer = modalButton("Close")
    ))
  })

  observeEvent(input$import_constructs_file_modal, {
    req(input$import_constructs_file_modal)
    tryCatch({
      tpl <- read.csv(input$import_constructs_file_modal$datapath,
                      stringsAsFactors = FALSE, na.strings = c("", "NA"))
      if (!"harmonised_name" %in% names(tpl)) {
        showNotification("Need 'harmonised_name' column.", type = "error"); return()
      }
      added <- 0L
      for (i in seq_len(nrow(tpl))) {
        hn <- trimws(tpl$harmonised_name[i])
        if (!nchar(hn) || hn %in% rv$constructs$harmonised_name) next
        rv$constructs <- rbind(rv$constructs, data.frame(
          harmonised_name  = hn,
          harmonised_label = if ("harmonised_label" %in% names(tpl) && !is.na(tpl$harmonised_label[i])) tpl$harmonised_label[i] else hn,
          domain           = if ("domain"   %in% names(tpl) && !is.na(tpl$domain[i]))    tpl$domain[i]    else "Uncategorised",
          subdomain        = if ("subdomain" %in% names(tpl) && !is.na(tpl$subdomain[i])) tpl$subdomain[i] else NA_character_,
          notes            = NA_character_, stringsAsFactors = FALSE
        ))
        added <- added + 1L
      }
      removeModal()
      showNotification(paste0("Imported ", added, " construct(s)."), type = "message", duration = 3L)
    }, error = function(e) showNotification(paste("Failed:", conditionMessage(e)), type = "error"))
  })

  observeEvent(input$btn_load_pipeline, {
    hc <- rv$hc
    av <- rv$all_vars
    if (is.null(hc)) { showNotification("No pipeline data.", type = "warning"); return() }

    result <- load_pipeline_constructs(hc, av)
    if (is.null(result)) { showNotification("No clusters found.", type = "warning"); return() }

    # Merge: keep existing, add new
    new_c <- result$constructs[!result$constructs$harmonised_name %in% rv$constructs$harmonised_name, ]
    rv$constructs <- rbind(rv$constructs, new_c)

    new_a <- result$assignments[!paste(result$assignments$harmonised_name, result$assignments$dataset) %in%
                                   paste(rv$assignments$harmonised_name, rv$assignments$dataset), ]
    rv$assignments <- rbind(rv$assignments, new_a)

    showNotification(paste0("Loaded ", nrow(new_c), " construct(s) from pipeline."),
                     type = "message", duration = 4L)
  })

  # ── Remove construct ─────────────────────────────────────────────────────────
  observeEvent(input$remove_construct, {
    hn <- input$remove_construct
    rv$constructs  <- rv$constructs[rv$constructs$harmonised_name != hn, , drop=FALSE]
    rv$assignments <- rv$assignments[rv$assignments$harmonised_name != hn, , drop=FALSE]
    rv$recode_rules <- rv$recode_rules[rv$recode_rules$harmonised_name != hn, , drop=FALSE]
    if (!is.null(rv$active_construct) && rv$active_construct == hn) {
      rv$active_construct <- NULL
      session$sendCustomMessage("closeDetailPanel", TRUE)
    }
    showNotification(paste0("Removed '", hn, "'."), type = "message", duration = 2L)
  })

  # ── Edit construct (open detail panel) ───────────────────────────────────────
  observeEvent(input$edit_construct, {
    rv$active_construct      <- input$edit_construct
    rv$search_context        <- NULL
    rv$detail_panels_opened  <- rv$detail_panels_opened + 1L
    session$sendCustomMessage("openDetailPanel", TRUE)
  })

  # Checkbox selection sync from JS
  observeEvent(input$construct_selections, {
    rv$selected_constructs <- input$construct_selections %||% character(0)
  }, ignoreNULL = FALSE)

  observeEvent(input$toggle_selected_only, {
    rv$show_selected_only <- !rv$show_selected_only
  })

  # Reset all constructs
  observeEvent(input$btn_reset_all, {
    rv$constructs         <- data.frame(harmonised_name=character(), harmonised_label=character(),
                                        domain=character(), subdomain=character(), notes=character(),
                                        stringsAsFactors=FALSE)
    rv$assignments        <- data.frame(harmonised_name=character(), dataset=character(),
                                        base_var=character(), recode_status=character(), excluded=logical(),
                                        stringsAsFactors=FALSE)
    rv$recode_rules       <- data.frame(harmonised_name=character(), dataset=character(),
                                        base_var=character(), old_code=integer(), old_label=character(),
                                        new_code=integer(), new_label=character(), confirmed=logical(),
                                        stringsAsFactors=FALSE)
    rv$selected_constructs <- character(0)
    rv$show_selected_only  <- FALSE
    rv$active_construct    <- NULL
    session$sendCustomMessage("closeDetailPanel", TRUE)
    showNotification("All constructs cleared.", type = "message", duration = 3L)
  })

  # ── Inline edit ──────────────────────────────────────────────────────────────
  observeEvent(input$inline_edit, {
    info <- input$inline_edit
    if (is.null(info)) return()
    hn    <- info$hn; field <- info$field; val <- trimws(info$value)
    idx   <- which(rv$constructs$harmonised_name == hn)
    if (length(idx) == 0L) return()

    if (field == "name") {
      if (!is_valid_r_name(val)) {
        showNotification(paste0("'", val, "' is not a valid R identifier."), type = "warning"); return()
      }
      if (val %in% rv$constructs$harmonised_name[-idx]) {
        showNotification(paste0("'", val, "' already exists."), type = "warning"); return()
      }
      old_hn <- hn
      rv$constructs$harmonised_name[idx] <- val
      rv$assignments$harmonised_name[rv$assignments$harmonised_name == old_hn]   <- val
      rv$recode_rules$harmonised_name[rv$recode_rules$harmonised_name == old_hn] <- val
      if (!is.null(rv$active_construct) && rv$active_construct == old_hn) rv$active_construct <- val
    } else if (field == "label") {
      rv$constructs$harmonised_label[idx] <- val
    }
  })

  # ── Close detail panel ───────────────────────────────────────────────────────
  observeEvent(input$close_detail_panel, {
    rv$active_construct <- NULL
  })

  # ── Detail panel ─────────────────────────────────────────────────────────────
  output$detail_panel_title <- renderUI({
    hn <- rv$active_construct
    if (is.null(hn)) return(div(class = "dp-title", "No construct selected"))
    con <- rv$constructs[rv$constructs$harmonised_name == hn, , drop=FALSE]
    hl  <- if (nrow(con) > 0) .s(con$harmonised_label[1L], hn) else hn
    div(class = "dp-title",
      tags$code(hn),
      tags$span(class = "text-muted small", hl)
    )
  })

  # Column 1: construct settings
  output$detail_col1 <- renderUI({
    hn <- rv$active_construct
    if (is.null(hn)) return(div(class = "text-muted small mt-3", "Open a construct to edit."))
    con <- rv$constructs[rv$constructs$harmonised_name == hn, , drop=FALSE]
    if (nrow(con) == 0) return(NULL)

    av <- rv$all_vars
    domains <- if (!is.null(av) && "domain" %in% names(av))
      sort(unique(av$domain[!is.na(av$domain)])) else character(0)
    domain_choices <- c("Uncategorised", domains)

    cur_dom <- .s(con$domain[1L], "Uncategorised")
    subdoms <- if (!is.null(av) && "subdomain" %in% names(av) && nchar(cur_dom) > 0)
      sort(unique(av$subdomain[!is.na(av$domain) & av$domain == cur_dom & !is.na(av$subdomain)]))
    else character(0)

    tagList(
      tags$span(class = "small-lbl", "Harmonised name"),
      textInput("dp_name", NULL, value = con$harmonised_name[1L], width = "100%"),
      uiOutput("dp_name_validation"),
      tags$span(class = "small-lbl mt-2", "Harmonised label"),
      textInput("dp_label", NULL, value = .s(con$harmonised_label[1L]), width = "100%"),
      div(class = "d-flex gap-2 mb-2",
        actionLink("dp_label_common", "Most common ↓", class = "small text-muted")
      ),
      tags$span(class = "small-lbl", "Domain"),
      selectInput("dp_domain", NULL,
                  choices = c("", domain_choices),
                  selected = cur_dom, width = "100%"),
      tags$span(class = "small-lbl", "Subdomain"),
      selectInput("dp_subdomain", NULL,
                  choices = c("", subdoms),
                  selected = .s(con$subdomain[1L]), width = "100%"),
      tags$span(class = "small-lbl mt-1", "Notes"),
      textAreaInput("dp_notes", NULL, value = .s(con$notes[1L]),
                    rows = 2L, width = "100%"),
      actionButton("dp_save", "✓ Save changes",
                   class = "btn btn-sm btn-success w-100 mt-2"),
      hr(class = "my-2"),
      uiOutput("detail_col1_wavesummary")
    )
  })

  output$dp_name_validation <- renderUI({
    nm <- trimws(input$dp_name %||% "")
    if (nchar(nm) == 0) return(NULL)
    hn  <- rv$active_construct
    idx <- which(rv$constructs$harmonised_name == hn)
    dup <- nm %in% rv$constructs$harmonised_name[-idx]
    if (!is_valid_r_name(nm))
      div(class = "name-err mb-1", "✗ Invalid R identifier")
    else if (dup)
      div(class = "name-err mb-1", "✗ Name already used")
    else
      div(class = "name-ok mb-1", "✓ Valid")
  })

  output$detail_col1_wavesummary <- renderUI({
    hn <- rv$active_construct
    if (is.null(hn)) return(NULL)
    lines <- wave_coverage_summary(hn, rv$assignments, rv$all_vars)
    if (is.null(lines) || length(lines) == 0) return(NULL)
    div(class = "wave-coverage",
      tags$strong(class = "small", "Wave coverage:"),
      tags$ul(class = "mb-0 ps-3 mt-1",
        lapply(lines, function(l) tags$li(class = "small", l)))
    )
  })

  observeEvent(input$dp_domain, {
    av  <- rv$all_vars
    dom <- input$dp_domain %||% ""
    if (is.null(av) || !"subdomain" %in% names(av) || nchar(dom) == 0) {
      updateSelectInput(session, "dp_subdomain", choices = c(""), selected = "")
      return()
    }
    subs <- sort(unique(av$subdomain[!is.na(av$domain) & av$domain == dom & !is.na(av$subdomain)]))
    updateSelectInput(session, "dp_subdomain", choices = c("", subs), selected = "")
  })

  observeEvent(input$dp_label_common, {
    hn   <- rv$active_construct; if (is.null(hn)) return()
    asgn <- rv$assignments[rv$assignments$harmonised_name == hn &
                             !is.na(rv$assignments$excluded) & !rv$assignments$excluded, ]
    av   <- rv$all_vars; if (is.null(av)) return()
    lbls <- character(0)
    for (i in seq_len(nrow(asgn))) {
      bv  <- asgn$base_var[i]; ds <- asgn$dataset[i]
      r   <- av[av$dataset == ds & (av$var_name == bv | (!"base_var" %in% names(av) || av$base_var == bv)), ]
      if (nrow(r) > 0 && "var_label" %in% names(r)) lbls <- c(lbls, r$var_label[1L])
    }
    if (length(lbls) > 0) {
      common <- names(sort(table(lbls), decreasing = TRUE))[1L]
      updateTextInput(session, "dp_label", value = common)
    }
  })

  observeEvent(input$dp_save, {
    hn  <- rv$active_construct; if (is.null(hn)) return()
    idx <- which(rv$constructs$harmonised_name == hn)
    if (length(idx) == 0L) return()

    nm  <- trimws(input$dp_name %||% hn)
    if (!is_valid_r_name(nm)) {
      showNotification("Invalid R identifier.", type = "warning"); return()
    }
    if (nm != hn && nm %in% rv$constructs$harmonised_name[-idx]) {
      showNotification("Name already used.", type = "warning"); return()
    }
    if (nm != hn) {
      rv$constructs$harmonised_name[idx] <- nm
      rv$assignments$harmonised_name[rv$assignments$harmonised_name == hn] <- nm
      rv$recode_rules$harmonised_name[rv$recode_rules$harmonised_name == hn] <- nm
      rv$active_construct <- nm
    }
    idx2 <- which(rv$constructs$harmonised_name == (if (nm != hn) nm else hn))
    rv$constructs$harmonised_label[idx2] <- trimws(input$dp_label %||% "")
    rv$constructs$domain[idx2]           <- trimws(input$dp_domain %||% "Uncategorised")
    rv$constructs$subdomain[idx2]        <- trimws(input$dp_subdomain %||% "")
    rv$constructs$notes[idx2]            <- trimws(input$dp_notes %||% "")
    showNotification("Saved.", type = "message", duration = 2L)
  })

  # Column 2: dataset assignment matrix
  output$detail_col2 <- renderUI({
    hn <- rv$active_construct
    if (is.null(hn)) return(div(class="text-muted small mt-3", "Select a construct."))
    all_ds <- rv$all_datasets
    a_df   <- rv$assignments

    header <- tags$tr(
      tags$th("Dataset"), tags$th("Assigned variable"), tags$th("Waves"), tags$th("Actions")
    )

    rows <- lapply(all_ds, function(ds) {
      a <- a_df[a_df$harmonised_name == hn & a_df$dataset == ds, , drop=FALSE]
      st <- if (nrow(a) == 0) "unreviewed"
            else if (a$excluded[1L]) "excluded"
            else a$recode_status[1L]
      bv <- if (nrow(a) > 0 && !is.null(a$base_var) && !is.na(a$base_var[1L])) a$base_var[1L] else NA_character_

      # Wave coverage
      wv_text <- if (!is.na(bv) && !is.null(rv$all_vars) && "has_wave" %in% names(rv$all_vars)) {
        wrows <- rv$all_vars[rv$all_vars$dataset == ds & !is.na(rv$all_vars$has_wave) &
                               rv$all_vars$has_wave & !is.na(rv$all_vars$base_var) &
                               rv$all_vars$base_var == bv, ]
        ws <- sort(unique(wrows$wave_label_std[!is.na(wrows$wave_label_std)]))
        if (length(ws) == 0) "—" else if (length(ws) <= 4L) paste(ws, collapse=" ")
        else paste0(ws[1L], "–", ws[length(ws)])
      } else "—"

      safe_key <- paste0(hn, "|||", ds)

      var_cell <- if (!is.na(bv)) {
        recode_flag <- if (st == "pending")
          tags$span(class = "small text-warning ms-1", title = "Recode pending — review in Column 3", "⚠")
        else NULL
        tagList(tags$code(class = "small", bv), recode_flag)
      } else {
        tags$span(class = "text-muted small fst-italic",
                  "⬜ Not assigned — click Find to locate the variable")
      }

      action_btn <- if (st == "excluded") {
        tags$button("Include", class = "btn btn-xs btn-outline-secondary",
                    title = "Include this dataset for this construct",
                    onclick = sprintf("Shiny.setInputValue('asgn_include','%s',{priority:'event'})",
                                     htmltools::htmlEscape(safe_key)))
      } else if ((st == "unreviewed" || st == "notfound") && is.na(bv)) {
        # No variable assigned yet — show Find button only
        tags$button("🔍 Find variable", class = "btn btn-xs btn-outline-primary",
          title = "Search for the variable that measures this construct in this dataset",
          onclick = sprintf(
            "Shiny.setInputValue('asgn_search','%s',{priority:'event'})",
            htmltools::htmlEscape(safe_key)
          )
        )
      } else if (st == "unreviewed" && !is.na(bv)) {
        # Variable assigned but status not yet determined — show Confirm + Change
        div(class = "d-flex gap-1",
          tags$button("✓ Confirm", class = "btn btn-xs btn-outline-success",
            title = "Mark as confirmed — no recode needed",
            onclick = sprintf(
              "Shiny.setInputValue('asgn_confirm','%s',{priority:'event'})",
              htmltools::htmlEscape(safe_key)
            )
          ),
          tags$button("Change", class = "btn btn-xs btn-outline-secondary",
            title = "Search for a different variable",
            onclick = sprintf(
              "Shiny.setInputValue('asgn_search','%s',{priority:'event'})",
              htmltools::htmlEscape(safe_key)
            )
          ),
          tags$button("Excl.", class = "btn btn-xs btn-outline-danger",
            title = "Exclude this dataset",
            onclick = sprintf(
              "Shiny.setInputValue('asgn_exclude','%s',{priority:'event'})",
              htmltools::htmlEscape(safe_key)
            )
          )
        )
      } else {
        div(class = "d-flex gap-1",
          tags$button("Change", class = "btn btn-xs btn-outline-secondary",
                      title = "Search for a different variable",
                      onclick = sprintf("Shiny.setInputValue('asgn_search','%s',{priority:'event'})",
                                       htmltools::htmlEscape(safe_key))),
          tags$button("Excl.", class = "btn btn-xs btn-outline-danger",
                      title = "Exclude this dataset — it will have NA values in the output",
                      onclick = sprintf("Shiny.setInputValue('asgn_exclude','%s',{priority:'event'})",
                                       htmltools::htmlEscape(safe_key)))
        )
      }

      tags$tr(class = paste0("s-", st),
        tags$td(tags$strong(class = "small", ds)),
        tags$td(var_cell),
        tags$td(class = "small text-muted", wv_text),
        tags$td(action_btn)
      )
    })

    # Check for any wave variables
    has_wave_vars <- !is.null(rv$all_vars) && "has_wave" %in% names(rv$all_vars) &&
                    any(!is.na(rv$all_vars$has_wave) & rv$all_vars$has_wave &
                          rv$all_vars$base_var %in% rv$assignments$base_var[rv$assignments$harmonised_name == hn])

    con_label <- .s(rv$constructs$harmonised_label[rv$constructs$harmonised_name == hn][1L], hn)

    tagList(
      tags$p(class = "small text-muted mb-2",
             "Confirm which variable measures ",
             tags$strong(con_label),
             " in each dataset below. Unassigned datasets will have NA values in the harmonised output."),
      tags$table(class = "asgn-table", header, rows),
      if (has_wave_vars)
        div(class = "wave-note mt-2",
          tags$span(class = "text-info me-1", "ℹ"),
          tags$strong("Wave variables:"),
          " selecting a base variable automatically includes all its waves (W1, W2, …) in the output. ",
          "You do not need to assign each wave separately."
        )
    )
  })

  observeEvent(input$asgn_search, {
    parts <- strsplit(input$asgn_search, "|||", fixed = TRUE)[[1L]]
    if (length(parts) < 2L) return()
    hn <- parts[1L]; ds <- parts[2L]

    # Build search hint from harmonised_label (preferred) or name
    con_row <- rv$constructs[rv$constructs$harmonised_name == hn, ,
                              drop = FALSE]
    raw_hint <- if (nrow(con_row) > 0 &&
                    !is.na(con_row$harmonised_label[1L]) &&
                    nchar(trimws(con_row$harmonised_label[1L])) > 2) {
      trimws(con_row$harmonised_label[1L])
    } else {
      gsub("_", " ", hn)
    }

    # Limit to first 4 words — broad enough to get results,
    # specific enough to be useful
    words       <- strsplit(raw_hint, "\\s+")[[1L]]
    search_hint <- paste(words[seq_len(min(4L, length(words)))],
                         collapse = " ")

    rv$search_page    <- 1L
    rv$search_context <- list(harmonised_name = hn, dataset = ds)

    session$sendCustomMessage("openSearchForAssignment",
      list(
        dataset         = ds,
        harmonised_name = hn,
        search_hint     = search_hint
      )
    )
  })

  observeEvent(input$asgn_exclude, {
    parts <- strsplit(input$asgn_exclude, "|||", fixed = TRUE)[[1L]]
    if (length(parts) < 2L) return()
    hn <- parts[1L]; ds <- parts[2L]
    idx <- which(rv$assignments$harmonised_name == hn & rv$assignments$dataset == ds)
    if (length(idx) > 0L) {
      rv$assignments$excluded[idx] <- TRUE
    } else {
      rv$assignments <- rbind(rv$assignments, data.frame(
        harmonised_name = hn, dataset = ds, base_var = NA_character_,
        recode_status = "excluded", excluded = TRUE, stringsAsFactors = FALSE
      ))
    }
  })

  observeEvent(input$asgn_include, {
    parts <- strsplit(input$asgn_include, "|||", fixed = TRUE)[[1L]]
    if (length(parts) < 2L) return()
    hn <- parts[1L]; ds <- parts[2L]
    idx <- which(rv$assignments$harmonised_name == hn & rv$assignments$dataset == ds)
    if (length(idx) > 0L) {
      rv$assignments$excluded[idx] <- FALSE
      if (!is.na(rv$assignments$recode_status[idx[1L]]) &&
          rv$assignments$recode_status[idx[1L]] == "excluded")
        rv$assignments$recode_status[idx] <- "unreviewed"
    }
  })

  observeEvent(input$asgn_confirm, {
    parts <- strsplit(input$asgn_confirm, "|||", fixed = TRUE)[[1L]]
    if (length(parts) < 2L) return()
    hn <- parts[1L]; ds <- parts[2L]
    idx <- which(rv$assignments$harmonised_name == hn &
                   rv$assignments$dataset == ds)
    if (length(idx) > 0L) {
      rv$assignments$recode_status[idx[1L]] <- "confirmed"
      rv$assignments$excluded[idx[1L]]       <- FALSE
    }
    showNotification(
      paste0(ds, " → '", hn, "' confirmed."),
      type = "message", duration = 2L
    )
  })

  # Column 3: recode alignment
  output$detail_col3 <- renderUI({
    hn  <- rv$active_construct
    if (is.null(hn)) return(div(class="text-muted small mt-3", ""))
    a_df <- rv$assignments[rv$assignments$harmonised_name == hn &
                             !is.na(rv$assignments$excluded) & !rv$assignments$excluded &
                             (is.na(rv$assignments$recode_status) | rv$assignments$recode_status != "notfound"), ]
    if (nrow(a_df) == 0)
      return(div(class = "text-muted small mt-3 fst-italic",
                 "No datasets assigned yet. Assign variables in the centre column."))

    all_vars   <- rv$all_vars
    all_labels <- rv$all_labels

    # Collect code sets per dataset
    datasets <- a_df$dataset
    code_sets <- lapply(seq_len(nrow(a_df)), function(i) {
      ds <- a_df$dataset[i]; bv <- a_df$base_var[i]
      get_value_labels(ds, bv, all_vars, all_labels)
    })
    names(code_sets) <- datasets

    has_labels <- any(sapply(code_sets, function(cs) !is.null(cs) && nrow(cs) > 0))

    if (!has_labels) {
      return(div(class = "recode-ok-notice mt-2",
        tags$strong("✓ Numeric / no response codes"),
        tags$p(class = "mb-0 mt-1 small",
               "These variables have no value labels. No recode needed.")
      ))
    }

    # Union of all codes
    all_codes <- sort(unique(unlist(lapply(code_sets, function(cs) {
      if (is.null(cs) || nrow(cs) == 0) return(integer(0))
      cs$value
    }))))

    # Get best label for each code (most common)
    code_labels <- vapply(all_codes, function(v) {
      lbls <- unlist(lapply(code_sets, function(cs) {
        if (is.null(cs) || nrow(cs) == 0) return(character(0))
        cs$label_text[cs$value == v]
      }))
      if (length(lbls) == 0) "" else names(sort(table(lbls), decreasing=TRUE))[1L]
    }, character(1L))

    # Check identical
    all_same <- length(datasets) > 1 && all(sapply(code_sets, function(cs) {
      if (is.null(cs) || nrow(cs) == 0) return(FALSE)
      length(all_codes) == nrow(cs) && all(sort(cs$value) == all_codes)
    }))

    # Get recode rules for this construct
    rr_hn <- rv$recode_rules[rv$recode_rules$harmonised_name == hn, , drop=FALSE]

    # Get reference dataset (first with labels)
    has_lbl_vec <- sapply(code_sets, function(x) !is.null(x) && nrow(x) > 0)
    ref_ds <- datasets[has_lbl_vec][1L]

    if (is.na(ref_ds)) ref_ds <- datasets[1L]

    if (all_same && nrow(rr_hn) == 0) {
      tagList(
        div(class = "recode-ok-notice",
          tags$strong("✓ Identical coding across all datasets"),
          tags$p(class = "mb-0 mt-1 small", "No recode needed.")
        ),
        tags$hr(class = "my-2"),
        tags$table(class = "recode-matrix mt-1",
          tags$tr(
            tags$th("Code"), tags$th("Label"),
            lapply(datasets, function(ds) tags$th(toupper(substr(ds, 1L, 6L))))
          ),
          lapply(seq_along(all_codes), function(j) {
            v <- all_codes[j]
            tags$tr(
              tags$td(class = "col-code", v),
              tags$td(class = "col-label", code_labels[j]),
              lapply(code_sets, function(cs) {
                if (is.null(cs) || nrow(cs) == 0) return(tags$td("—"))
                idx <- which(cs$value == v)
                tags$td(class = "cell-same", if (length(idx) > 0) v else "—")
              })
            )
          })
        )
      )
    } else {
      # Show instruction block (collapsible after first time seen)
      instr_expanded <- !rv$recode_instructions_seen
      instr_block <- div(class = "recode-instructions",
        div(class = "d-flex justify-content-between align-items-center",
          tags$strong(class = "small", "How to align response codes:"),
          tags$button(if (instr_expanded) "▲ Hide" else "▼ Show",
                      class = "btn btn-xs btn-link text-muted",
                      onclick = paste0("Shiny.setInputValue('toggle_recode_instr',Math.random(),{priority:'event'})"))
        ),
        if (instr_expanded)
          tags$ol(class = "small mt-1 mb-0 ps-3",
            tags$li("Check the matrix — ", tags$strong("amber cells"), " differ from the reference dataset"),
            tags$li("Set the ", tags$strong("reference dataset"), " (the one whose coding you want to keep)"),
            tags$li("Click ", tags$strong("[Auto-generate recodes]"),
                    " to fill suggested new codes by matching label text"),
            tags$li("Review each row — change any new codes that look wrong"),
            tags$li("Click ", tags$strong("[Confirm all recodes]"), " when satisfied"),
            tags$li("The status badge turns ", tags$span(class = "text-success", "green ✓"))
          )
      )
      tagList(
        div(class = "recode-warn-notice mb-2",
          tags$strong("⚠ Response code differences detected"),
          tags$p(class = "mb-0 mt-1 small",
                 "Use the controls below to align coding across datasets.")
        ),
        instr_block,
        div(class = "d-flex gap-2 align-items-center mb-2 mt-2 flex-wrap",
          tags$small(class = "fw-semibold", "Reference:"),
          selectInput("recode_ref_ds", NULL,
                      choices = datasets, selected = ref_ds, width = "160px"),
          actionButton("btn_auto_recode",   "Auto-generate", class = "btn btn-xs btn-outline-info",
                       title = "Suggests new codes by matching response label text across datasets"),
          actionButton("btn_confirm_recodes","Confirm all",  class = "btn btn-xs btn-outline-success",
                       title = "Marks all recode rules as reviewed — they will be included in the generated R code"),
          actionButton("btn_clear_recodes",  "Clear",        class = "btn btn-xs btn-outline-secondary",
                       title = "Removes all recode rules for this construct")
        ),

        # Matrix
        tags$table(class = "recode-matrix",
          tags$tr(
            tags$th("Code"), tags$th("Label"),
            lapply(datasets, function(ds) tags$th(toupper(substr(ds, 1L, 6L)))),
            tags$th("New")
          ),
          lapply(seq_along(all_codes), function(j) {
            v <- all_codes[j]; lbl <- code_labels[j]
            ref_cs <- code_sets[[ref_ds]]
            ref_has <- !is.null(ref_cs) && v %in% ref_cs$value

            # Get current new_code from rr_hn
            rr_row <- rr_hn[!is.na(rr_hn$old_code) & rr_hn$old_code == v, ]
            new_code_val <- if (nrow(rr_row) > 0 && !is.na(rr_row$new_code[1L])) rr_row$new_code[1L] else v

            tags$tr(
              tags$td(class = "col-code", v),
              tags$td(class = "col-label", substr(lbl, 1L, 35L)),
              lapply(code_sets, function(cs) {
                if (is.null(cs) || nrow(cs) == 0) return(tags$td(class = "cell-missing", "—"))
                idx <- which(cs$value == v)
                if (length(idx) == 0) return(tags$td(class = "cell-missing", "—"))
                cell_v <- cs$value[idx[1L]]
                css <- if (!ref_has || cell_v == (if (!is.null(ref_cs)) ref_cs$value[ref_cs$value == v] else v))
                  "cell-same" else "cell-diff"
                tags$td(class = paste("col-code", css), cell_v)
              }),
              tags$td(
                tags$input(type = "number", class = "form-control form-control-sm",
                           style = "width:60px;display:inline-block;",
                           value = new_code_val,
                           `data-code` = v, `data-hn` = hn,
                           onchange = sprintf(
                             "Shiny.setInputValue('recode_cell_edit',{hn:'%s',old:%s,new:this.value},{priority:'event'})",
                             htmltools::htmlEscape(hn), v
                           ))
              )
            )
          })
        ),

        # Confirmed status
        if (nrow(rr_hn) > 0) {
          n_conf <- sum(rr_hn$confirmed, na.rm = TRUE)
          tags$p(class = "small text-muted mt-2",
                 paste0(n_conf, " / ", nrow(rr_hn), " rules confirmed"))
        }
      )
    }
  })

  observeEvent(input$recode_cell_edit, {
    info <- input$recode_cell_edit
    if (is.null(info)) return()
    hn <- info$hn; old_v <- as.integer(info$old); new_v <- as.integer(info$new)
    if (is.na(new_v)) return()

    idx <- which(rv$recode_rules$harmonised_name == hn & !is.na(rv$recode_rules$old_code) &
                   rv$recode_rules$old_code == old_v)
    if (length(idx) > 0L) {
      rv$recode_rules$new_code[idx[1L]]  <- new_v
      rv$recode_rules$confirmed[idx[1L]] <- FALSE
    } else {
      rv$recode_rules <- rbind(rv$recode_rules, data.frame(
        harmonised_name = hn, dataset = NA_character_, base_var = NA_character_,
        old_code = old_v, old_label = NA_character_,
        new_code = new_v, new_label = NA_character_, confirmed = FALSE,
        stringsAsFactors = FALSE
      ))
    }
  })

  observeEvent(input$btn_auto_recode, {
    hn     <- rv$active_construct; if (is.null(hn)) return()
    ref_ds <- input$recode_ref_ds %||% rv$all_datasets[1L]
    a_df   <- rv$assignments[rv$assignments$harmonised_name == hn &
                               !is.na(rv$assignments$excluded) & !rv$assignments$excluded, ]
    av     <- rv$all_vars; al <- rv$all_labels

    ref_cs <- get_value_labels(ref_ds, a_df$base_var[a_df$dataset == ref_ds][1L], av, al)
    if (is.null(ref_cs) || nrow(ref_cs) == 0) {
      showNotification("Reference dataset has no value labels.", type = "warning"); return()
    }

    n_added <- 0L
    for (i in seq_len(nrow(a_df))) {
      ds <- a_df$dataset[i]; bv <- a_df$base_var[i]
      if (ds == ref_ds) next
      cs <- get_value_labels(ds, bv, av, al)
      if (is.null(cs) || nrow(cs) == 0) next
      for (j in seq_len(nrow(cs))) {
        old_v <- cs$value[j]; old_lbl <- cs$label_text[j]
        # Find match by label
        ref_idx <- which(tolower(trimws(ref_cs$label_text)) == tolower(trimws(old_lbl)))
        new_v   <- if (length(ref_idx) > 0L) ref_cs$value[ref_idx[1L]] else old_v
        # Add/update rule
        idx <- which(rv$recode_rules$harmonised_name == hn &
                       !is.na(rv$recode_rules$dataset) & rv$recode_rules$dataset == ds &
                       !is.na(rv$recode_rules$base_var) & rv$recode_rules$base_var == bv &
                       !is.na(rv$recode_rules$old_code) & rv$recode_rules$old_code == old_v)
        if (length(idx) > 0L) {
          rv$recode_rules$new_code[idx[1L]] <- new_v; rv$recode_rules$confirmed[idx[1L]] <- FALSE
        } else {
          rv$recode_rules <- rbind(rv$recode_rules, data.frame(
            harmonised_name = hn, dataset = ds, base_var = bv,
            old_code = old_v, old_label = old_lbl, new_code = new_v, new_label = NA_character_,
            confirmed = FALSE, stringsAsFactors = FALSE
          ))
          n_added <- n_added + 1L
        }
      }
    }
    showNotification(paste0("Generated ", n_added, " recode rule(s). Review and confirm."),
                     type = "message", duration = 4L)
  })

  observeEvent(input$btn_confirm_recodes, {
    hn <- rv$active_construct; if (is.null(hn)) return()
    idx <- rv$recode_rules$harmonised_name == hn
    rv$recode_rules$confirmed[idx] <- TRUE
    # Update recode_status in assignments
    a_idx <- rv$assignments$harmonised_name == hn &
               !is.na(rv$assignments$excluded) & !rv$assignments$excluded
    rv$assignments$recode_status[a_idx & !is.na(rv$assignments$recode_status) &
                                   rv$assignments$recode_status == "pending"] <- "confirmed"
    showNotification("Recodes confirmed.", type = "message", duration = 2L)
  })

  observeEvent(input$toggle_recode_instr, {
    rv$recode_instructions_seen <- !rv$recode_instructions_seen
  })

  observeEvent(input$btn_clear_recodes, {
    hn <- rv$active_construct; if (is.null(hn)) return()
    rv$recode_rules <- rv$recode_rules[rv$recode_rules$harmonised_name != hn, , drop=FALSE]
    showNotification("Recodes cleared.", type = "message", duration = 2L)
  })

  # ── Variable Search — banner only (filter bar is now static in ui) ──────────

  # Context banner: re-renders on context change but does NOT recreate filter inputs
  output$search_context_banner_ui <- renderUI({
    ctx <- rv$search_context
    if (!is.null(ctx)) {
      div(class = "search-context-banner-targeted",
        div(
          tags$strong(paste0(
            "\U0001F3AF Finding a variable for '",
            ctx$harmonised_name, "' in ", ctx$dataset
          )),
          tags$br(),
          tags$span(class = "small",
            "Search box pre-filled with the construct name — edit if needed. ",
            "Click the green ", tags$strong("[✓ Assign]"),
            " button on the correct variable to confirm and return."
          )
        ),
        actionButton("btn_back_to_construct", "← Back to construct",
                     class = "btn btn-sm btn-outline-secondary")
      )
    } else {
      div(class = "search-standalone-banner",
        tags$strong("\U0001F4CB Standalone search mode"),
        tags$br(),
        tags$span(class = "small",
          "Browse and collect variables to add to your harmonisation plan. ",
          "Use ", tags$strong("[+ Add]"), " on any card — they collect in the queue bar at the bottom. ",
          "Then assign the queue to a construct. ",
          tags$em("Tip: open a construct and click a dataset badge for direct one-step assignment.")
        )
      )
    }
  })

  # search_ds rendered as renderUI so choices work even when view-search is d-none
  output$search_ds_ui <- renderUI({
    all_ds <- rv$all_datasets
    selectInput("search_ds", NULL,
                choices  = c("All datasets" = "", setNames(all_ds, all_ds)),
                selected = "",
                width    = "180px")
  })

  # When context clears (back/assign/nav): reset dataset selector (FIX 5)
  # Context-set case is now handled atomically by openSearchForAssignment JS message
  observeEvent(rv$search_context, {
    if (is.null(rv$search_context))
      updateSelectInput(session, "search_ds", selected = "")
  }, ignoreNULL = FALSE)

  observeEvent(input$btn_back_to_construct, {
    rv$search_context <- NULL
    session$sendCustomMessage("switchView", "constructs")
    if (!is.null(rv$active_construct))
      session$sendCustomMessage("openDetailPanel", TRUE)
  })

  # Debounce search input so heavy filtering only runs 300ms after user stops typing
  search_q_debounced <- debounce(reactive(trimws(input$search_q %||% "")), 300L)

  # Variable search filter — depends only on debounced query, pre-computed data, and dataset selector
  filtered_search_vars <- reactive({
    q <- search_q_debounced()
    if (nchar(q) < 2L) return(NULL)

    data <- collapsed_vars()
    if (is.null(data)) return(NULL)

    ds_f <- input$search_ds %||% ""
    if (nchar(ds_f) > 0L)
      data <- data[!is.na(data$dataset) & data$dataset == ds_f, , drop=FALSE]

    hit <- grepl(q, data$var_name,  ignore.case = TRUE) |
           grepl(q, data$var_label, ignore.case = TRUE)
    if ("base_var" %in% names(data))
      hit <- hit | grepl(q, data$base_var, ignore.case = TRUE)
    data <- data[hit, , drop=FALSE]

    if (nrow(data) == 0L) return(data)
    data[order(data$dataset, data$var_name), , drop=FALSE]
  })

  output$search_results_count <- renderUI({
    fv   <- filtered_search_vars()
    n    <- if (is.null(fv)) 0L else nrow(fv)
    q    <- trimws(input$search_q %||% "")
    ds_f <- input$search_ds %||% ""

    if (nchar(q) < 2L) {
      if (nchar(ds_f) > 0L)
        return(div(class = "search-type-prompt",
                   tags$strong(paste0("Searching in: ", ds_f)),
                   tags$br(),
                   tags$span(class = "small text-muted",
                             "Type at least 2 characters to find the variable ",
                             "(e.g. the variable name or a keyword from its label)")))
      else
        return(tags$p(class = "small text-muted",
                      "Type at least 2 characters to search across all datasets."))
    }
    tags$p(class = "small text-muted mb-1", paste0("Showing ", n, " variable", if (n != 1) "s"))
  })

  output$search_results_ui <- renderUI({
    fv     <- filtered_search_vars()
    ctx    <- rv$search_context
    hc_lkp <- hc_lookups()            # O(1) lookup tables, no rv$hc dependency here

    q <- search_q_debounced()
    if (nchar(q) < 2L) return(NULL)

    if (is.null(fv) || nrow(fv) == 0) {
      return(div(class = "text-muted small mt-3",
                 "No variables found — try different keywords."))
    }

    page_n <- 25L                      # smaller page = faster browser render
    n_show <- min(nrow(fv), rv$search_page * page_n)
    fv_show <- fv[seq_len(n_show), , drop=FALSE]

    cards <- lapply(seq_len(nrow(fv_show)), function(i) {
      r   <- fv_show[i, , drop=FALSE]
      ds  <- r$dataset[1L]
      vn  <- r$var_name[1L]
      bv  <- if ("base_var" %in% names(r) && !is.na(r$base_var[1L])) r$base_var[1L] else vn
      lbl <- .s(r$var_label[1L])
      typ <- .s(r$var_type[1L])
      dom <- .s(r$domain[1L])
      sub <- .s(r$subdomain[1L])
      wl  <- .s(r$waves_list[1L])

      waves <- if (nchar(wl) > 0) strsplit(wl, ",")[[1L]] else character(0)
      wave_badges <- if (length(waves) > 0)
        paste(sapply(waves, function(w)
          paste0('<span class="wave-badge">', htmltools::htmlEscape(w), '</span>')),
              collapse = "")
      else ""

      # Cross-dataset matches — O(1) lookup instead of full hc scans
      matches_html <- ""
      if (!is.null(hc_lkp)) {
        key  <- paste0(ds, "|||", vn)
        bkey <- paste0(ds, "|||", bv)
        cid  <- hc_lkp$cid_map[[key]] %||% hc_lkp$cid_map[[bkey]]
        if (!is.null(cid)) {
          st      <- hc_lkp$stat_map[[key]] %||% hc_lkp$stat_map[[bkey]] %||% ""
          dot_cls <- if (st == "ready") "mp-green" else if (st == "recodable") "mp-amber" else "mp-grey"
          mbrs    <- hc_lkp$members[[cid]]
          ods     <- unique(mbrs$dataset[mbrs$dataset != ds])
          if (length(ods) > 0) {
            pills <- sapply(ods, function(od) {
              o_vn <- .s(mbrs$var_name[mbrs$dataset == od][1L])
              paste0('<span class="match-pill"><span class="mpd ', dot_cls, '"></span>',
                     htmltools::htmlEscape(od), ' (',
                     htmltools::htmlEscape(substr(o_vn, 1L, 15L)), ')</span>')
            })
            matches_html <- paste0(
              '<div class="match-area"><small class="text-muted">Also in: </small>',
              paste(pills, collapse = ""),
              if (length(ods) >= 2L) paste0(
                ' <button class="btn btn-xs btn-outline-secondary ms-1" ',
                'onclick="Shiny.setInputValue(\'add_all_matches\',\'', cid, '\',{priority:\'event\'})">',
                '+ Add all matches</button>'
              ),
              '</div>'
            )
          }
        }
      }

      meta_parts <- Filter(function(x) nchar(x) > 0, c(typ, dom, sub))
      meta_text <- paste(meta_parts, collapse = " · ")

      safe_key <- htmltools::htmlEscape(paste0(ds, "|||", vn))

      assign_btn <- if (!is.null(ctx)) {
        paste0('<button class="btn btn-sm btn-success fw-semibold" ',
               'title="Assign this variable to \'', htmltools::htmlEscape(ctx$harmonised_name), '\' in ', htmltools::htmlEscape(ctx$dataset), '" ',
               'onclick="Shiny.setInputValue(\'assign_var\',\'', safe_key, '\',{priority:\'event\'})">',
               '&#10003; Assign</button>')
      } else {
        paste0('<button class="btn btn-xs btn-outline-secondary" ',
               'title="Add to queue — then assign the queue to a construct using the bar at the bottom of the screen" ',
               'onclick="Shiny.setInputValue(\'queue_var\',\'', safe_key, '\',{priority:\'event\'})">',
               '+ Add</button>')
      }

      # Lazy value labels — placeholder filled by JS on first expand (FIX 3)
      lset_name <- if ("value_labels" %in% names(r) && !is.na(r$value_labels[1L])) r$value_labels[1L] else ""
      vl_html <- if (nchar(lset_name) > 0)
        paste0('<div class="vc-expand-body" style="display:none;">',
               '<div class="vc-expand-placeholder"',
               ' data-ds="', htmltools::htmlEscape(ds), '"',
               ' data-lset="', htmltools::htmlEscape(lset_name), '">',
               '</div></div>')
      else ""

      paste0(
        '<div class="var-card" id="vc-', i, '">',
        '<div class="vc-top">',
        '<div><span class="vc-name">', htmltools::htmlEscape(vn), '</span>',
        ' <span class="badge bg-secondary ms-1 small">', htmltools::htmlEscape(ds), '</span></div>',
        '<div class="vc-btns">', assign_btn, '</div>',
        '</div>',
        '<div class="vc-lbl">', htmltools::htmlEscape(substr(lbl, 1L, 120L)), '</div>',
        '<div class="vc-meta">', htmltools::htmlEscape(meta_text),
        if (nchar(wave_badges) > 0) paste0(' &nbsp;', wave_badges),
        '</div>',
        matches_html,
        vl_html,
        '</div>'
      )
    })

    HTML(paste(cards, collapse = "\n"))
  })

  output$search_load_more_ui <- renderUI({
    fv <- filtered_search_vars()
    if (is.null(fv)) return(NULL)
    shown <- rv$search_page * 25L
    if (nrow(fv) <= shown) return(NULL)
    div(class = "text-center mt-3",
      actionButton("btn_load_more", paste0("Load more (", nrow(fv) - shown, " remaining)"),
                   class = "btn btn-outline-secondary btn-sm")
    )
  })

  observeEvent(input$btn_load_more, { rv$search_page <- rv$search_page + 1L })
  observeEvent(list(input$search_q, input$search_ds), { rv$search_page <- 1L })

  # Lazy value label loader (FIX 3) — fires when JS expands a card for the first time
  observeEvent(input$load_value_labels, {
    req(input$load_value_labels)
    info    <- input$load_value_labels
    lbl_idx <- all_labels_index()
    if (is.null(lbl_idx) || is.null(info$lset) || nchar(info$lset) == 0) return()
    key <- paste0(info$ds, "|||", info$lset)
    lb  <- lbl_idx[[key]]
    if (is.null(lb) || nrow(lb) == 0) {
      session$sendCustomMessage("injectLabelHTML",
        list(card_id = info$card_id,
             html = "<em class='small text-muted'>No response codes available</em>"))
      return()
    }
    lb   <- lb[order(lb$value), ]
    rows <- paste(sapply(seq_len(min(nrow(lb), 10L)), function(j)
      paste0('<div class="vc-vlabel-row"><span class="vl-code">',
             lb$value[j], '</span><span>',
             htmltools::htmlEscape(lb$label_text[j]),
             '</span></div>')
    ), collapse = "")
    session$sendCustomMessage("injectLabelHTML",
      list(card_id = info$card_id, html = rows))
  })

  # ── Assign variable from search ───────────────────────────────────────────────
  observeEvent(input$assign_var, {
    ctx <- rv$search_context
    if (is.null(ctx)) return()

    parts <- strsplit(input$assign_var, "|||", fixed = TRUE)[[1L]]
    if (length(parts) < 2L) return()
    ds <- parts[1L]; vn <- parts[2L]
    hn <- ctx$harmonised_name

    # Resolve base_var using existing O(1) index
    bv <- vn
    r_av <- lookup_var(all_vars_index(), ds, vn)
    if (!is.null(r_av) && "base_var" %in% names(r_av) &&
        !is.na(r_av$base_var[1L]))
      bv <- r_av$base_var[1L]

    # Work on a local copy so re-evaluation sees the new row
    updated_asgn <- rv$assignments

    # Update or add the new assignment with "unreviewed" first
    idx <- which(updated_asgn$harmonised_name == hn &
                   updated_asgn$dataset == ds)
    if (length(idx) > 0L) {
      updated_asgn$base_var[idx[1L]]      <- bv
      updated_asgn$excluded[idx[1L]]      <- FALSE
      updated_asgn$recode_status[idx[1L]] <- "unreviewed"
    } else {
      updated_asgn <- rbind(updated_asgn, data.frame(
        harmonised_name = hn, dataset = ds, base_var = bv,
        recode_status = "unreviewed", excluded = FALSE,
        stringsAsFactors = FALSE
      ))
    }

    # Now determine correct status with the full updated assignment set
    initial_status <- determine_recode_status(
      hn                  = hn,
      new_ds              = ds,
      new_bv              = bv,
      current_assignments = updated_asgn,
      all_vars            = rv$all_vars,
      all_labels          = rv$all_labels
    )

    # Apply the determined status to the new row
    new_idx <- which(updated_asgn$harmonised_name == hn &
                       updated_asgn$dataset == ds)
    if (length(new_idx) > 0L)
      updated_asgn$recode_status[new_idx[1L]] <- initial_status

    # Re-evaluate all other confirmed assignments for this construct
    # in case the new variable introduces a coding conflict
    others_idx <- which(
      updated_asgn$harmonised_name == hn &
      updated_asgn$dataset != ds &
      !is.na(updated_asgn$excluded) & !updated_asgn$excluded &
      !is.na(updated_asgn$recode_status) &
      updated_asgn$recode_status == "confirmed"
    )
    for (j in others_idx) {
      e_ds <- updated_asgn$dataset[j]
      e_bv <- updated_asgn$base_var[j]
      if (is.na(e_bv)) next
      re_st <- determine_recode_status(
        hn                  = hn,
        new_ds              = e_ds,
        new_bv              = e_bv,
        current_assignments = updated_asgn,
        all_vars            = rv$all_vars,
        all_labels          = rv$all_labels
      )
      # Only demote confirmed → pending, never promote pending → confirmed
      # (user may have manually reviewed and confirmed a recode)
      if (re_st == "pending")
        updated_asgn$recode_status[j] <- "pending"
    }

    # Write the fully updated assignments back in one operation
    rv$assignments <- updated_asgn

    status_msg <- switch(initial_status,
      confirmed = "confirmed — no recode needed ✓",
      pending   = "assigned — recode differences detected, review Column 3 ⚠",
      "assigned"
    )
    showNotification(
      paste0("'", bv, "' from ", ds, " → '", hn, "' (", status_msg, ")"),
      type    = if (initial_status == "pending") "warning" else "message",
      duration = 4L
    )

    rv$active_construct <- hn
    rv$search_context   <- NULL
    session$sendCustomMessage("switchView", "constructs")
    session$sendCustomMessage("openDetailPanel", TRUE)
  })

  # ── Queue variable (standalone search) ───────────────────────────────────────
  observeEvent(input$queue_var, {
    parts <- strsplit(input$queue_var, "|||", fixed = TRUE)[[1L]]
    if (length(parts) < 2L) return()
    ds <- parts[1L]; vn <- parts[2L]
    bv <- vn
    lbl <- ""
    r_av <- lookup_var(all_vars_index(), ds, vn)
    if (!is.null(r_av)) {
      if ("base_var" %in% names(r_av) && !is.na(r_av$base_var[1L])) bv <- r_av$base_var[1L]
      if ("var_label" %in% names(r_av)) lbl <- .s(r_av$var_label[1L])
    }
    key <- paste(ds, bv)
    if (!key %in% paste(rv$pending_queue$dataset, rv$pending_queue$base_var)) {
      rv$pending_queue <- rbind(rv$pending_queue, data.frame(
        dataset = ds, base_var = bv, var_label = lbl, stringsAsFactors = FALSE
      ))
      session$sendCustomMessage("updatePendingQueue", nrow(rv$pending_queue))
    }
  })

  observeEvent(input$add_all_matches, {
    cid <- input$add_all_matches
    hc  <- rv$hc
    if (is.null(hc) || is.null(cid)) return()
    members <- hc[!is.na(hc$cluster_id) & as.character(hc$cluster_id) == as.character(cid), ]
    for (i in seq_len(nrow(members))) {
      ds <- members$dataset[i]; vn <- members$var_name[i]
      bv <- vn; lbl <- .s(members$var_label[i])
      r_av <- lookup_var(all_vars_index(), ds, vn)
      if (!is.null(r_av) && "base_var" %in% names(r_av) && !is.na(r_av$base_var[1L]))
        bv <- r_av$base_var[1L]
      key <- paste(ds, bv)
      if (!key %in% paste(rv$pending_queue$dataset, rv$pending_queue$base_var)) {
        rv$pending_queue <- rbind(rv$pending_queue, data.frame(
          dataset = ds, base_var = bv, var_label = lbl, stringsAsFactors = FALSE
        ))
      }
    }
    session$sendCustomMessage("updatePendingQueue", nrow(rv$pending_queue))
  })

  # ── Pending queue UI (floating bar — activated when items queued) ─────────────
  output$pending_queue_ui <- renderUI({
    pq <- rv$pending_queue; n <- nrow(pq)
    if (n == 0) return(NULL)
    con_choices <- c("+ Create new construct" = "__new__",
                     setNames(rv$constructs$harmonised_name, rv$constructs$harmonised_name))
    tagList(
      tags$span(class = "badge-count", n), tags$span(" variable(s) in queue →"),
      selectInput("pq_target_construct", NULL, choices = con_choices, width = "220px"),
      actionButton("btn_assign_queue", "Assign to construct",
                   class = "btn btn-sm btn-primary"),
      actionButton("btn_clear_queue", "Clear",
                   class = "btn btn-sm btn-outline-secondary"),
      tags$span(class = "text-muted small ms-2",
                paste(unique(paste0(pq$dataset, ":", pq$base_var)), collapse = ", "))
    )
  })

  # ── Standalone queue bar (hidden in context mode) ─────────────────────────────
  # FIX 4: construct choices split into own renderUI so queue changes don't re-create the select
  output$sq_construct_choices_ui <- renderUI({
    con_choices <- c("+ Create new construct" = "__new__",
                     setNames(rv$constructs$harmonised_name, rv$constructs$harmonised_name))
    selectInput("pq_target_construct_sb", NULL, choices = con_choices, width = "200px")
  })

  output$standalone_queue_bar <- renderUI({
    if (!is.null(rv$search_context)) return(NULL)
    pq <- rv$pending_queue; n <- nrow(pq)
    div(class = "standalone-queue-bar",
      if (n == 0) {
        tags$span(class = "text-muted small",
                  "Queue empty — use ",
                  tags$strong("[+ Add]"),
                  " on any variable card to collect variables, then assign them to a construct")
      } else {
        tagList(
          tags$span(class = "badge-count me-2", n),
          tags$span(class = "small fw-semibold me-2", "variable(s) queued →"),
          uiOutput("sq_construct_choices_ui"),
          actionButton("btn_assign_queue_sb", "Assign to construct",
                       class = "btn btn-sm btn-primary ms-1"),
          actionButton("btn_clear_queue_sb", "Clear",
                       class = "btn btn-sm btn-outline-secondary ms-1"),
          tags$span(class = "text-muted small ms-2",
                    paste(unique(paste0(pq$dataset, ":", pq$base_var)), collapse = ", "))
        )
      }
    )
  })

  observeEvent(input$btn_assign_queue_sb, {
    pq  <- rv$pending_queue; if (nrow(pq) == 0) return()
    tgt <- input$pq_target_construct_sb %||% "__new__"
    if (tgt == "__new__") {
      hn <- make_harmonised_name(paste(unique(pq$base_var), collapse = "_"))
      if (!hn %in% rv$constructs$harmonised_name)
        rv$constructs <- rbind(rv$constructs, data.frame(
          harmonised_name = hn, harmonised_label = tools::toTitleCase(gsub("_", " ", hn)),
          domain = "Uncategorised", subdomain = NA_character_, notes = NA_character_,
          stringsAsFactors = FALSE
        ))
    } else { hn <- tgt }
    for (i in seq_len(nrow(pq))) {
      ds <- pq$dataset[i]; bv <- pq$base_var[i]
      idx <- which(rv$assignments$harmonised_name == hn & rv$assignments$dataset == ds)
      if (length(idx) > 0L) {
        rv$assignments$base_var[idx[1L]]      <- bv
        rv$assignments$excluded[idx[1L]]      <- FALSE
        rv$assignments$recode_status[idx[1L]] <- "unreviewed"
      } else {
        rv$assignments <- rbind(rv$assignments, data.frame(
          harmonised_name = hn, dataset = ds, base_var = bv,
          recode_status = "unreviewed", excluded = FALSE, stringsAsFactors = FALSE))
      }
    }
    rv$pending_queue    <- data.frame(dataset=character(), base_var=character(),
                                      var_label=character(), stringsAsFactors=FALSE)
    rv$active_construct <- hn
    session$sendCustomMessage("updatePendingQueue", 0L)
    session$sendCustomMessage("switchView", "constructs")
    session$sendCustomMessage("openDetailPanel", TRUE)
    showNotification(paste0("Assigned ", nrow(pq), " variable(s) to '", hn, "'."),
                     type = "message", duration = 3L)
  })
  observeEvent(input$btn_clear_queue_sb, {
    rv$pending_queue <- data.frame(dataset=character(), base_var=character(),
                                    var_label=character(), stringsAsFactors=FALSE)
    session$sendCustomMessage("updatePendingQueue", 0L)
  })

  observeEvent(input$btn_clear_queue, {
    rv$pending_queue <- data.frame(dataset=character(), base_var=character(),
                                    var_label=character(), stringsAsFactors=FALSE)
    session$sendCustomMessage("updatePendingQueue", 0L)
  })

  observeEvent(input$btn_assign_queue, {
    pq  <- rv$pending_queue
    if (nrow(pq) == 0) return()
    tgt <- input$pq_target_construct %||% "__new__"

    if (tgt == "__new__") {
      hn <- make_harmonised_name(paste(unique(pq$base_var), collapse = "_"))
      if (!hn %in% rv$constructs$harmonised_name) {
        rv$constructs <- rbind(rv$constructs, data.frame(
          harmonised_name = hn, harmonised_label = tools::toTitleCase(gsub("_", " ", hn)),
          domain = "Uncategorised", subdomain = NA_character_, notes = NA_character_,
          stringsAsFactors = FALSE
        ))
      }
    } else {
      hn <- tgt
    }

    for (i in seq_len(nrow(pq))) {
      ds <- pq$dataset[i]; bv <- pq$base_var[i]
      idx <- which(rv$assignments$harmonised_name == hn & rv$assignments$dataset == ds)
      if (length(idx) > 0L) {
        rv$assignments$base_var[idx[1L]]      <- bv
        rv$assignments$excluded[idx[1L]]      <- FALSE
        rv$assignments$recode_status[idx[1L]] <- "unreviewed"
      } else {
        rv$assignments <- rbind(rv$assignments, data.frame(
          harmonised_name = hn, dataset = ds, base_var = bv,
          recode_status = "unreviewed", excluded = FALSE, stringsAsFactors = FALSE
        ))
      }
    }

    rv$pending_queue     <- data.frame(dataset=character(), base_var=character(),
                                        var_label=character(), stringsAsFactors=FALSE)
    rv$active_construct  <- hn
    session$sendCustomMessage("updatePendingQueue", 0L)
    session$sendCustomMessage("switchView", "constructs")
    session$sendCustomMessage("openDetailPanel", TRUE)
    showNotification(paste0("Assigned ", nrow(pq), " variable(s) to '", hn, "'."),
                     type = "message", duration = 3L)
  })

  # ── Code preview drawer ───────────────────────────────────────────────────────
  output$code_drawer_content <- renderUI({
    c_df <- rv$constructs; a_df <- rv$assignments
    nc <- nrow(c_df); na <- nrow(a_df)

    active_asgn <- a_df[!is.na(a_df$excluded) & !a_df$excluded &
                          (is.na(a_df$recode_status) | a_df$recode_status != "notfound"), , drop=FALSE]
    datasets    <- sort(unique(active_asgn$dataset))
    harm_names  <- sort(unique(active_asgn$harmonised_name))

    # Structure items
    items <- lapply(harm_names, function(hn) {
      a <- active_asgn[active_asgn$harmonised_name == hn, ]
      n_ds <- length(unique(a$dataset))
      has_pending <- any(a$recode_status == "pending")
      has_incompat <- any(a$recode_status == "incompatible")
      st <- if (has_incompat) "red" else if (has_pending) "amber" else "green"
      txt <- paste0(hn, " (", n_ds, " dataset", if (n_ds != 1) "s" else "", ")")
      div(class = "struct-item",
        div(class = paste0("struct-dot sd-", st)),
        tags$span(class = "text-mono small", txt)
      )
    })

    # Abbreviated code
    code_txt <- tryCatch({
      generate_harmonisation_code(
        rv$constructs, rv$assignments, rv$recode_rules, rv$all_vars, rv$all_labels
      )
    }, error = function(e) paste("# Error generating code:", conditionMessage(e)))

    # Truncate for preview
    code_lines <- strsplit(code_txt, "\n")[[1L]]
    preview_lines <- if (length(code_lines) > 60L)
      c(code_lines[1:60], "# ... (download for full script)")
    else code_lines

    tagList(
      tags$p(class = "cp-title", "Harmonised dataset structure"),
      if (length(harm_names) == 0)
        tags$p(class = "text-muted small fst-italic", "No constructs assigned yet.")
      else tagList(
        tags$p(class = "small text-muted mb-2",
               paste0(nc, " construct(s) · ", length(datasets), " dataset(s)")),
        tags$div(items)
      ),

      tags$p(class = "cp-title", "Code preview"),
      div(class = "code-pre", paste(preview_lines, collapse = "\n")),

      div(class = "mt-3",
        downloadButton("btn_download_r2", "Download full .R",
                       class = "btn btn-sm btn-primary w-100")
      )
    )
  })

  # ── Download handlers ─────────────────────────────────────────────────────────
  .make_r_script <- function() {
    sel <- isolate(rv$selected_constructs)
    c_df <- isolate(rv$constructs)
    a_df <- isolate(rv$assignments)
    # When constructs are selected, restrict code to those only
    if (length(sel) > 0) {
      c_df <- c_df[c_df$harmonised_name %in% sel, , drop=FALSE]
      a_df <- a_df[a_df$harmonised_name %in% sel, , drop=FALSE]
    }
    generate_harmonisation_code(
      c_df, a_df, isolate(rv$recode_rules), isolate(rv$all_vars), isolate(rv$all_labels)
    )
  }

  # ── Generate button (state-aware) ────────────────────────────────────────────
  output$generate_btn_ui <- renderUI({
    all_statuses <- .construct_statuses()
    sel          <- rv$selected_constructs
    # Score only selected constructs if any are ticked
    statuses  <- if (length(sel) > 0) all_statuses[names(all_statuses) %in% sel]
                 else all_statuses
    n_total   <- length(statuses)
    n_complete <- sum(statuses == "complete")
    scope_note <- if (length(sel) > 0) paste0(" (", length(sel), " selected)") else ""

    if (n_total == 0 || n_complete == 0) {
      downloadButton("btn_download_r", paste0("⬇ Generate .R", scope_note),
                     class = "btn btn-sm btn-outline-warning",
                     title = "No constructs confirmed yet — complete at least one")
    } else if (n_complete < n_total) {
      downloadButton("btn_download_r", paste0("⬇ Generate .R", scope_note),
                     class = "btn btn-sm btn-primary",
                     title = paste0(n_complete, " ready · ", n_total - n_complete, " pending"))
    } else {
      downloadButton("btn_download_r",
                     paste0("✓ Generate .R — ", n_total, " ready", scope_note),
                     class = "btn btn-sm btn-success",
                     title = paste0("All ", n_total, " constructs confirmed — click to download"))
    }
  })

  output$btn_download_r <- downloadHandler(
    filename = function() paste0("harmonisation_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".R"),
    content  = function(file) {
      writeLines(.make_r_script(), file)
      showNotification(
        tagList(
          tags$strong("R script generated."),
          tags$br(),
          "Set ", tags$code('data_folder <- "YOUR/PATH"'),
          " at the top of the script before running."
        ),
        type = "message", duration = 8L
      )
    }
  )
  output$btn_download_r2 <- downloadHandler(
    filename = function() paste0("harmonisation_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".R"),
    content  = function(file) writeLines(.make_r_script(), file)
  )

  output$btn_export_mapping <- downloadHandler(
    filename = function() paste0("construct_mapping_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content  = function(file) {
      out <- merge(rv$constructs, rv$assignments, by = "harmonised_name", all.x = TRUE)
      write.csv(out, file, row.names = FALSE)
    }
  )

  # Prevent suspension of search outputs when view-search is d-none.
  # Must be called AFTER all outputs are defined.
  outputOptions(output, "search_ds_ui",             suspendWhenHidden = FALSE)
  outputOptions(output, "search_context_banner_ui",  suspendWhenHidden = FALSE)
  outputOptions(output, "search_results_count",      suspendWhenHidden = FALSE)
  outputOptions(output, "search_results_ui",         suspendWhenHidden = FALSE)
  outputOptions(output, "search_load_more_ui",       suspendWhenHidden = FALSE)
  outputOptions(output, "standalone_queue_bar",      suspendWhenHidden = FALSE)

  # ── Test exports (only active when shiny.testmode = TRUE) ────────────────────
  exportTestValues(
    assignments   = rv$assignments,
    constructs    = rv$constructs,
    recode_rules  = rv$recode_rules,
    search_page   = rv$search_page,
    all_datasets  = rv$all_datasets,
    all_vars_nrow = if (is.null(rv$all_vars)) -1L else nrow(rv$all_vars)
  )
}

shinyApp(ui, server)

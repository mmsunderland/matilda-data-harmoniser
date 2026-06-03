# plan_server.R — flat selection-driven table interface for Tab 2

# ---------------------------------------------------------------------------
# sync_var_mapping_extended()
# Called from app.R harmonisation_plan observer to preserve user edits.
# ---------------------------------------------------------------------------
sync_var_mapping_extended <- function(new_m, cur) {
  for (col in c("harmonised_label", "notes", "recode_status")) {
    if (!col %in% names(new_m))
      new_m[[col]] <- if (col == "recode_status") "none" else NA_character_
  }
  if (nrow(cur) > 0) {
    keep_old <- intersect(
      c("dataset", "var_name", "harmonised_label", "notes", "recode_status"),
      names(cur)
    )
    if (length(keep_old) > 2) {
      new_m <- merge(new_m, cur[, keep_old, drop = FALSE],
                     by = c("dataset", "var_name"), all.x = TRUE,
                     suffixes = c("", ".old"))
      for (col in c("harmonised_label", "notes", "recode_status")) {
        old_col <- paste0(col, ".old")
        if (old_col %in% names(new_m)) {
          new_m[[col]] <- ifelse(is.na(new_m[[old_col]]), new_m[[col]], new_m[[old_col]])
          new_m[[old_col]] <- NULL
        }
      }
    }
  }
  no_hl <- is.na(new_m$harmonised_label) |
            nchar(as.character(new_m$harmonised_label)) == 0
  new_m$harmonised_label[no_hl] <- new_m$var_label[no_hl]
  new_m$recode_status[is.na(new_m$recode_status)] <- "none"
  new_m
}

# ---------------------------------------------------------------------------
setup_plan_server <- function(
    input, output, session,
    all_vars, all_labels, harm_candidates, recode_rules_tbl, pipeline_loaded,
    harmonisation_plan, var_mapping, recode_data,
    ...) {

  `%||%` <- function(x, y) if (!is.null(x) && length(x) > 0 && !all(is.na(x))) x else y

  # ── State ──────────────────────────────────────────────────────────────────
  plan_selected_ids <- reactiveVal(integer(0))
  recode_panel_key  <- reactiveVal(NULL)
  recode_nav_keys   <- reactiveVal(character(0))
  recode_nav_idx    <- reactiveVal(1L)
  bulk_result_msg   <- reactiveVal(NULL)
  show_summary      <- reactiveVal(FALSE)
  reference_key_map <- reactiveVal(list())

  # ── Helpers ────────────────────────────────────────────────────────────────

  .is_valid_r_name <- function(x) {
    !is.null(x) && !is.na(x) && nchar(x) > 0 &&
      grepl("^[a-zA-Z.][a-zA-Z0-9_.]*$", x) &&
      !grepl("^\\.$", x)
  }

  .recode_badge <- function(status) {
    switch(as.character(status),
      none         = '<span class="badge bg-secondary">No recode</span>',
      pending      = '<span class="badge bg-warning text-dark">Pending review</span>',
      confirmed    = '<span class="badge bg-success">Confirmed</span>',
      incompatible = '<span class="badge bg-danger">Incompatible</span>',
      '<span class="badge bg-secondary">—</span>'
    )
  }

  .trunc_html <- function(x, n = 60) {
    ifelse(
      !is.na(x) & nchar(x) > n,
      paste0('<span title="', htmltools::htmlEscape(x), '" style="cursor:help">',
             htmltools::htmlEscape(substr(x, 1L, n)), '…</span>'),
      htmltools::htmlEscape(ifelse(is.na(x), "", x))
    )
  }

  .init_recode_for_key <- function(ds, vn, val_set, lbl) {
    lb <- lbl[lbl$dataset == ds & lbl$label_set == val_set, , drop = FALSE]
    if (nrow(lb) == 0) return(NULL)
    lb <- lb[order(lb$value), ]
    rr <- recode_rules_tbl()
    pr <- if (!is.null(rr) && nrow(rr) > 0)
      rr[!is.na(rr$dataset) & rr$dataset == ds &
           !is.na(rr$var_name) & rr$var_name == vn, , drop = FALSE]
    else NULL
    if (!is.null(pr) && nrow(pr) > 0) {
      new_codes <- vapply(lb$value, function(v) {
        r <- pr[!is.na(pr$old_code) & pr$old_code == v, ]
        if (nrow(r) > 0) as.numeric(r$new_code[1L]) else as.numeric(v)
      }, numeric(1L))
      new_labels <- vapply(seq_len(nrow(lb)), function(i) {
        r <- pr[!is.na(pr$old_code) & pr$old_code == lb$value[i], ]
        if (nrow(r) > 0 && !is.na(r$new_label[1L])) r$new_label[1L]
        else lb$label_text[i]
      }, character(1L))
      from_pipeline <- TRUE
    } else {
      new_codes <- lb$value; new_labels <- lb$label_text; from_pipeline <- FALSE
    }
    list(
      df = data.frame(old_code = lb$value, old_label = lb$label_text,
                      new_code = new_codes, new_label = new_labels,
                      stringsAsFactors = FALSE),
      from_pipeline = from_pipeline
    )
  }

  .ensure_recode <- function(ds, vn) {
    key <- paste(ds, vn, sep = "|||")
    rd  <- recode_data()
    if (!is.null(rd[[key]])) return(invisible(NULL))
    m   <- var_mapping()
    row <- m[!is.na(m$dataset) & m$dataset == ds &
               !is.na(m$var_name) & m$var_name == vn, , drop = FALSE]
    if (nrow(row) == 0) return(invisible(NULL))
    val_set <- row$value_labels[1L]
    if (is.na(val_set) || nchar(val_set) == 0) return(invisible(NULL))
    lbl <- all_labels()
    if (is.null(lbl) || nrow(lbl) == 0) return(invisible(NULL))
    res <- .init_recode_for_key(ds, vn, val_set, lbl)
    if (!is.null(res)) { rd[[key]] <- res; recode_data(rd) }
  }

  # ── Filtered plan data ──────────────────────────────────────────────────────

  filtered_plan <- reactive({
    m <- var_mapping()
    if (is.null(m) || nrow(m) == 0) return(m)

    q <- trimws(input$plan_search %||% "")
    if (nchar(q) > 0) {
      hit <- grepl(q, m$dataset,          ignore.case = TRUE) |
             grepl(q, m$var_name,         ignore.case = TRUE) |
             grepl(q, m$var_label,        ignore.case = TRUE) |
             grepl(q, m$harmonised_name,  ignore.case = TRUE) |
             grepl(q, m$harmonised_label, ignore.case = TRUE) |
             grepl(q, ifelse(is.na(m$notes), "", m$notes), ignore.case = TRUE)
      m <- m[hit, , drop = FALSE]
    }

    ds_f <- input$plan_filter_ds %||% character(0)
    if (length(ds_f) > 0) m <- m[m$dataset %in% ds_f, , drop = FALSE]

    wv_f <- input$plan_filter_wave %||% character(0)
    if (length(wv_f) > 0) {
      keep_no_wave <- "no_wave" %in% wv_f
      specific     <- setdiff(wv_f, "no_wave")
      match_wave <- vapply(seq_len(nrow(m)), function(i) {
        w <- m$wave[i]
        if (is.na(w) || w == "") return(keep_no_wave)
        if (length(specific) > 0 && any(sapply(specific,
            function(fw) grepl(fw, w, fixed = TRUE)))) return(TRUE)
        FALSE
      }, logical(1L))
      m <- m[match_wave, , drop = FALSE]
    }

    st_f <- input$plan_filter_status %||% "All"
    if (!is.null(st_f) && nchar(st_f) > 0 && st_f != "All") {
      target <- switch(st_f,
        "Needs recode"     = "pending",
        "Recode confirmed" = "confirmed",
        "No recode needed" = "none",
        "Incompatible"     = "incompatible",
        NULL
      )
      if (!is.null(target)) m <- m[m$recode_status == target, , drop = FALSE]
    }
    m
  })

  # ── Main toolbar ──────────────────────────────────────────────────────────

  output$plan_toolbar <- renderUI({
    m     <- var_mapping()
    sel   <- plan_selected_ids()
    n_sel <- length(sel)

    ds_choices   <- if (!is.null(m) && nrow(m) > 0) sort(unique(m$dataset)) else character(0)
    wave_raw     <- if (!is.null(m) && nrow(m) > 0)
      sort(unique(m$wave[!is.na(m$wave) & m$wave != ""])) else character(0)
    wave_choices <- c(setNames(wave_raw, wave_raw), "No wave / Cross-sectional" = "no_wave")

    div(
      class = "card mb-2",
      div(class = "card-body py-2 px-3",
        div(class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
          div(class = "d-flex align-items-center gap-2",
            tags$input(type = "checkbox", id = "plan_select_all_cb",
                       title = "Select / deselect all",
                       style = "width:16px;height:16px;cursor:pointer;",
                       onclick = "planSelectAll(this.checked)"),
            if (n_sel > 0)
              tags$span(class = "badge bg-primary rounded-pill",
                        paste0(n_sel, " variable", if (n_sel != 1) "s" else "", " selected"))
          ),
          div(class = "d-flex gap-2 flex-wrap align-items-center",
            div(style = "width:200px;",
              textInput("plan_search", NULL, placeholder = "Search all columns...",
                        width = "100%")
            ),
            div(style = "width:150px;",
              selectizeInput("plan_filter_ds", NULL, choices = ds_choices,
                             multiple = TRUE, width = "100%",
                             options = list(placeholder = "Dataset...",
                                            plugins = list("remove_button")))
            ),
            div(style = "width:150px;",
              selectizeInput("plan_filter_wave", NULL, choices = wave_choices,
                             multiple = TRUE, width = "100%",
                             options = list(placeholder = "Wave...",
                                            plugins = list("remove_button")))
            ),
            div(style = "width:170px;",
              selectInput("plan_filter_status", NULL, width = "100%",
                          choices = c("All", "Needs recode", "Recode confirmed",
                                      "No recode needed", "Incompatible"))
            )
          )
        )
      )
    )
  })

  # ── Editing toolbar ────────────────────────────────────────────────────────

  output$plan_editing_toolbar <- renderUI({
    sel <- plan_selected_ids()
    if (length(sel) == 0) return(NULL)
    m <- var_mapping()
    if (is.null(m) || nrow(m) == 0) return(NULL)
    sel_rows <- m[m$row_id %in% sel, , drop = FALSE]
    n_sel    <- nrow(sel_rows)
    if (n_sel == 0) return(NULL)

    uniq_names  <- unique(sel_rows$harmonised_name)
    name_val    <- if (length(uniq_names) == 1) uniq_names[1L] else ""
    name_ph     <- if (length(uniq_names) == 1) "" else "Multiple values..."
    uniq_labels <- unique(sel_rows$harmonised_label)
    label_val   <- if (length(uniq_labels) == 1) uniq_labels[1L] else ""
    label_ph    <- if (length(uniq_labels) == 1) "" else "Multiple values..."

    banner <- if (length(uniq_names) == 1 && n_sel > 1) {
      div(class = "alert alert-info small py-1 mb-2",
          sprintf("These %d variables share harmonised name ‘%s’ — editing will update all of them.",
                  n_sel, uniq_names[1L]))
    } else if (length(uniq_names) > 1) {
      div(class = "alert alert-warning small py-1 mb-2",
          "Applying a new name will group these variables together in the harmonised output.")
    }

    div(
      class = "card mb-2 border-primary",
      div(class = "card-body py-2 px-3",
        div(class = "fw-semibold small mb-2",
            icon("pen"),
            sprintf(" Editing %d variable%s", n_sel, if (n_sel > 1) "s" else "")),
        banner,
        layout_columns(
          col_widths = c(4, 4, 4), gap = "0.5rem",
          # Name
          div(
            tags$label("Variable name", class = "small fw-semibold mb-1 d-block"),
            div(class = "d-flex gap-1",
              textInput("plan_bulk_name", NULL, value = name_val,
                        placeholder = name_ph, width = "100%"),
              actionButton("btn_apply_bulk_name", "Apply",
                           class = "btn-sm btn-outline-primary")
            ),
            uiOutput("plan_bulk_name_validation")
          ),
          # Label
          div(
            tags$label("Variable label", class = "small fw-semibold mb-1 d-block"),
            div(class = "d-flex gap-1",
              textInput("plan_bulk_label", NULL, value = label_val,
                        placeholder = label_ph, width = "100%"),
              actionButton("btn_apply_bulk_label", "Apply",
                           class = "btn-sm btn-outline-primary")
            ),
            actionLink("btn_use_common_label",
                       "Use most common original label", class = "small text-muted")
          ),
          # Actions
          div(
            tags$label("Actions", class = "small fw-semibold mb-1 d-block"),
            div(class = "d-flex flex-column gap-1",
              actionButton("btn_view_recode", "View/edit response codes",
                           icon = icon("table"),
                           class = "btn-sm btn-outline-secondary w-100"),
              div(class = "d-flex gap-2",
                actionButton("btn_deselect_all", "Deselect all",
                             class = "btn-xs btn-outline-secondary"),
                actionButton("btn_remove_selected",
                             "Remove from plan",
                             icon = icon("trash"),
                             class = "btn-xs btn-outline-danger")
              )
            )
          )
        )
      )
    )
  })

  output$plan_bulk_name_validation <- renderUI({
    nm <- trimws(input$plan_bulk_name %||% "")
    if (nchar(nm) == 0) return(NULL)
    if (!.is_valid_r_name(nm))
      return(div(class = "text-danger small mt-1",
                 "✗ Invalid R name. Suggested: ", tags$code(make.names(nm))))
    div(class = "text-success small mt-1", "✓ Valid name")
  })

  # ── Plan table ─────────────────────────────────────────────────────────────

  output$plan_table <- DT::renderDataTable({
    m <- filtered_plan()

    if (is.null(m) || nrow(m) == 0) {
      all_m <- var_mapping()
      msg <- if (is.null(all_m) || nrow(all_m) == 0)
        "No variables in your harmonisation plan yet. Go to Browse Variables to add variables."
      else
        "No variables match the current filters."
      empty <- data.frame(` ` = character(), Dataset = character(), Wave = character(),
                          `Variable Name` = character(), Label = character(),
                          `Harmonised Name` = character(), `Harmonised Label` = character(),
                          `Recode Status` = character(), Notes = character(),
                          Actions = character(),
                          check.names = FALSE, stringsAsFactors = FALSE)
      return(DT::datatable(empty, rownames = FALSE, escape = FALSE,
                           options = list(dom = "t",
                             language = list(emptyTable = msg))))
    }

    sel_ids <- isolate(plan_selected_ids())

    cb_col <- vapply(seq_len(nrow(m)), function(i) {
      rid <- m$row_id[i]
      chk <- if (rid %in% sel_ids) " checked" else ""
      paste0('<input type="checkbox" class="plan-row-cb" data-rid="',
             rid, '"', chk, ' style="cursor:pointer;" />')
    }, character(1L))

    wave_disp   <- ifelse(is.na(m$wave) | m$wave == "", "—", m$wave)
    label_html  <- .trunc_html(m$var_label, 60L)
    notes_html  <- .trunc_html(ifelse(is.na(m$notes), "", m$notes), 40L)
    h_name_disp <- htmltools::htmlEscape(m$harmonised_name)
    h_lbl_disp  <- htmltools::htmlEscape(
      ifelse(is.na(m$harmonised_label), "", m$harmonised_label))
    badge_col   <- vapply(m$recode_status, .recode_badge, character(1L))

    action_col <- vapply(seq_len(nrow(m)), function(i) {
      key <- htmltools::htmlEscape(
        paste(m$dataset[i], m$var_name[i], sep = "|||"))
      paste0(
        '<button class="btn btn-xs btn-outline-secondary plan-recode-btn" ',
        'data-key="', key, '">Recode ▾</button> ',
        '<button class="btn btn-xs btn-outline-danger plan-remove-btn" ',
        'data-key="', key, '" title="Remove from plan">✕</button>'
      )
    }, character(1L))

    disp <- data.frame(
      ` `                = cb_col,
      Dataset            = htmltools::htmlEscape(m$dataset),
      Wave               = htmltools::htmlEscape(wave_disp),
      `Variable Name`    = htmltools::htmlEscape(m$var_name),
      Label              = label_html,
      `Harmonised Name`  = h_name_disp,
      `Harmonised Label` = h_lbl_disp,
      `Recode Status`    = badge_col,
      Notes              = notes_html,
      Actions            = action_col,
      row_id_            = m$row_id,
      rstat_             = m$recode_status,
      check.names = FALSE, stringsAsFactors = FALSE
    )

    DT::datatable(
      disp,
      escape     = FALSE,
      rownames   = FALSE,
      class      = "table-sm table-hover",
      selection  = "none",
      extensions = "FixedHeader",
      editable   = list(target = "cell",
                        disable = list(columns = c(0L, 1L, 2L, 3L, 4L, 7L, 9L, 10L, 11L))),
      options = list(
        pageLength     = -1,
        scrollY        = "calc(100vh - 390px)",
        scrollCollapse = TRUE,
        fixedHeader    = TRUE,
        dom            = "t",
        autoWidth      = FALSE,
        drawCallback   = DT::JS("function() { planTableDrawn(); }"),
        columnDefs = list(
          list(width = "30px",  orderable = FALSE, targets = 0),
          list(width = "85px",  targets = 1),
          list(width = "55px",  targets = 2),
          list(width = "100px", targets = 3),
          list(width = "170px", orderable = FALSE, targets = 4),
          list(width = "115px", targets = 5),
          list(width = "150px", targets = 6),
          list(width = "105px", orderable = FALSE, targets = 7),
          list(width = "110px", targets = 8),
          list(width = "105px", orderable = FALSE, targets = 9),
          list(visible = FALSE, targets = c(10L, 11L))
        ),
        rowCallback = DT::JS("
          function(row, data) {
            var st = data[11];
            if      (st === 'pending')      $(row).css('border-left','3px solid #ffc107');
            else if (st === 'confirmed')    $(row).css('border-left','3px solid #198754');
            else if (st === 'incompatible') $(row).css('border-left','3px solid #dc3545');
          }
        ")
      )
    )
  }, server = FALSE)

  # ── Inline cell edit ────────────────────────────────────────────────────────
  # Editable cols (0-based): 5=harmonised_name, 6=harmonised_label, 8=notes

  observeEvent(input$plan_table_cell_edit, {
    info <- input$plan_table_cell_edit
    if (!info$col %in% c(5L, 6L, 8L)) return()
    fp  <- filtered_plan()
    if (is.null(fp) || info$row < 1L || info$row > nrow(fp)) return()
    rid <- fp$row_id[info$row]
    val <- trimws(as.character(info$value))
    m   <- var_mapping()
    idx <- which(m$row_id == rid)
    if (length(idx) == 0L) return()

    if (info$col == 5L) {
      if (!.is_valid_r_name(val)) {
        showNotification(paste0("'", val, "' is not a valid R identifier."),
                         type = "warning", duration = 4); return()
      }
      other <- m$harmonised_name[-idx]
      if (val %in% other)
        showNotification(
          paste0("'", val, "' already used — these variables will be grouped."),
          type = "message", duration = 4)
      m$harmonised_name[idx] <- val
    } else if (info$col == 6L) {
      m$harmonised_label[idx] <- val
    } else {
      m$notes[idx] <- val
    }
    var_mapping(m)
  })

  # ── Selection ──────────────────────────────────────────────────────────────

  observeEvent(input$plan_selected_ids, {
    ids <- input$plan_selected_ids
    plan_selected_ids(if (is.null(ids)) integer(0L) else as.integer(ids))
  }, ignoreNULL = FALSE)

  observeEvent(input$btn_deselect_all, {
    plan_selected_ids(integer(0L))
    session$sendCustomMessage("setPlanSelection", integer(0L))
  })

  # ── Bulk name / label apply ────────────────────────────────────────────────

  observeEvent(input$btn_apply_bulk_name, {
    nm  <- trimws(input$plan_bulk_name %||% "")
    sel <- plan_selected_ids()
    if (length(sel) == 0L || nchar(nm) == 0L) return()
    if (!.is_valid_r_name(nm)) {
      showNotification("Not a valid R identifier.", type = "warning"); return()
    }
    m   <- var_mapping()
    idx <- which(m$row_id %in% sel)
    m$harmonised_name[idx] <- nm
    var_mapping(m)
    showNotification(
      sprintf("Set harmonised name to ‘%s’ for %d variable%s.",
              nm, length(idx), if (length(idx) != 1L) "s" else ""),
      type = "message", duration = 3)
  })

  observeEvent(input$btn_apply_bulk_label, {
    lbl <- trimws(input$plan_bulk_label %||% "")
    sel <- plan_selected_ids()
    if (length(sel) == 0L || nchar(lbl) == 0L) return()
    m   <- var_mapping()
    idx <- which(m$row_id %in% sel)
    m$harmonised_label[idx] <- lbl
    var_mapping(m)
    showNotification(
      sprintf("Updated label for %d variable%s.",
              length(idx), if (length(idx) != 1L) "s" else ""),
      type = "message", duration = 3)
  })

  observeEvent(input$btn_use_common_label, {
    sel  <- plan_selected_ids()
    if (length(sel) == 0L) return()
    m    <- var_mapping()
    rows <- m[m$row_id %in% sel, , drop = FALSE]
    if (nrow(rows) == 0L) return()
    freq <- sort(table(rows$var_label), decreasing = TRUE)
    updateTextInput(session, "plan_bulk_label", value = names(freq)[1L])
  })

  observeEvent(input$btn_remove_selected, {
    sel <- plan_selected_ids()
    if (length(sel) == 0L) return()
    m   <- var_mapping()
    to_rm <- m[m$row_id %in% sel, c("dataset", "var_name"), drop = FALSE]
    plan  <- harmonisation_plan()
    for (i in seq_len(nrow(to_rm))) {
      ds <- to_rm$dataset[i]; vn <- to_rm$var_name[i]
      plan[[ds]] <- setdiff(plan[[ds]], vn)
      if (length(plan[[ds]]) == 0L) plan[[ds]] <- NULL
    }
    harmonisation_plan(plan)
    plan_selected_ids(integer(0L))
    session$sendCustomMessage("setPlanSelection", integer(0L))
    showNotification(
      sprintf("Removed %d variable%s from plan.",
              nrow(to_rm), if (nrow(to_rm) != 1L) "s" else ""),
      type = "message", duration = 3)
  })

  # ── Remove individual row ─────────────────────────────────────────────────

  observeEvent(input$plan_remove_click, {
    key   <- input$plan_remove_click
    parts <- strsplit(key, "|||", fixed = TRUE)[[1L]]
    if (length(parts) < 2L) return()
    ds   <- parts[1L]; vn <- parts[2L]
    plan <- harmonisation_plan()
    plan[[ds]] <- setdiff(plan[[ds]], vn)
    if (length(plan[[ds]]) == 0L) plan[[ds]] <- NULL
    harmonisation_plan(plan)
    m   <- var_mapping()
    row <- m[m$dataset == ds & m$var_name == vn, , drop = FALSE]
    if (nrow(row) > 0L) {
      new_sel <- setdiff(plan_selected_ids(), row$row_id)
      plan_selected_ids(new_sel)
      session$sendCustomMessage("setPlanSelection", new_sel)
    }
    if (!is.null(recode_panel_key()) && recode_panel_key() == key)
      recode_panel_key(NULL)
    showNotification(paste0(vn, " removed from plan."), type = "message", duration = 3)
  })

  # ── Open recode panel ─────────────────────────────────────────────────────

  observeEvent(input$plan_recode_click, {
    key   <- input$plan_recode_click
    parts <- strsplit(key, "|||", fixed = TRUE)[[1L]]
    if (length(parts) < 2L) return()
    .ensure_recode(parts[1L], parts[2L])
    recode_panel_key(key)
    recode_nav_keys(key)
    recode_nav_idx(1L)
  })

  observeEvent(input$btn_view_recode, {
    sel <- plan_selected_ids()
    if (length(sel) == 0L) return()
    m   <- var_mapping()
    rows <- m[m$row_id %in% sel, , drop = FALSE]
    if (nrow(rows) == 0L) return()
    keys <- paste(rows$dataset, rows$var_name, sep = "|||")
    for (k in keys) {
      p <- strsplit(k, "|||", fixed = TRUE)[[1L]]
      .ensure_recode(p[1L], p[2L])
    }
    recode_nav_keys(keys)
    recode_nav_idx(1L)
    recode_panel_key(keys[1L])
  })

  observeEvent(input$recode_nav_prev, {
    idx  <- recode_nav_idx(); keys <- recode_nav_keys()
    if (idx > 1L) { recode_nav_idx(idx - 1L); recode_panel_key(keys[idx - 1L]) }
  })

  observeEvent(input$recode_nav_next, {
    idx  <- recode_nav_idx(); keys <- recode_nav_keys()
    if (idx < length(keys)) { recode_nav_idx(idx + 1L); recode_panel_key(keys[idx + 1L]) }
  })

  observeEvent(input$btn_recode_close, { recode_panel_key(NULL) })

  # ── Recode panel UI ────────────────────────────────────────────────────────

  output$recode_editor_panel <- renderUI({
    key <- recode_panel_key()
    if (is.null(key)) return(NULL)
    parts <- strsplit(key, "|||", fixed = TRUE)[[1L]]
    if (length(parts) < 2L) return(NULL)
    ds <- parts[1L]; vn <- parts[2L]

    m   <- var_mapping()
    row <- m[m$dataset == ds & m$var_name == vn, , drop = FALSE]
    if (nrow(row) == 0L) return(NULL)

    lbl     <- all_labels()
    val_set <- row$value_labels[1L]
    has_cats <- !is.na(val_set) && nchar(val_set) > 0 &&
                !is.null(lbl) && nrow(lbl) > 0

    rd            <- recode_data()
    from_pipeline <- isTRUE(rd[[key]]$from_pipeline)

    # Navigation bar (multi-row mode)
    nav_keys <- recode_nav_keys()
    nav_idx  <- recode_nav_idx()
    nav_bar <- if (length(nav_keys) > 1L) {
      div(class = "d-flex align-items-center gap-2 mb-2 pb-2 border-bottom",
        actionButton("recode_nav_prev", "←",
                     class = "btn-xs btn-outline-secondary",
                     disabled = if (nav_idx == 1L) NA else NULL),
        tags$span(class = "small",
                  sprintf("Variable %d of %d", nav_idx, length(nav_keys))),
        actionButton("recode_nav_next", "→",
                     class = "btn-xs btn-outline-secondary",
                     disabled = if (nav_idx == length(nav_keys)) NA else NULL),
        actionButton("btn_apply_recode_all",
                     "Apply same recode to all selected",
                     class = "btn-sm btn-outline-info")
      )
    }

    # Auto-populate / coding comparison banner
    banner <- if (has_cats) {
      if (from_pipeline) {
        div(class = "alert alert-info small py-2 mb-2",
            "⚡ Auto-populated from pipeline suggestions. Review and confirm.")
      } else {
        hn       <- row$harmonised_name[1L]
        siblings <- m[!is.na(m$harmonised_name) & m$harmonised_name == hn &
                        !(m$dataset == ds & m$var_name == vn), , drop = FALSE]
        sibs_lbl <- siblings[!is.na(siblings$value_labels) &
                               nchar(siblings$value_labels) > 0, , drop = FALSE]
        if (nrow(sibs_lbl) > 0) {
          my_lb   <- lbl[lbl$dataset == ds & lbl$label_set == val_set, ]
          my_vals <- sort(my_lb$value)
          all_same <- all(vapply(seq_len(nrow(sibs_lbl)), function(i) {
            sib_lb <- lbl[lbl$dataset == sibs_lbl$dataset[i] &
                            lbl$label_set == sibs_lbl$value_labels[i], ]
            sv <- sort(sib_lb$value)
            length(sv) == length(my_vals) && all(sv == my_vals)
          }, logical(1L)))
          if (all_same)
            div(class = "alert alert-success small py-2 mb-2",
                "✓ Identical coding across all variables with this harmonised name — no recode needed.")
          else
            div(class = "alert alert-warning small py-2 mb-2",
                "⚠ Response code differences detected across variables with this harmonised name.")
        }
      }
    }

    # Other variables with same harmonised_name (for Copy from / reference)
    hn       <- row$harmonised_name[1L]
    siblings <- m[!is.na(m$harmonised_name) & m$harmonised_name == hn &
                    !(m$dataset == ds & m$var_name == vn), , drop = FALSE]
    other_keys <- if (nrow(siblings) > 0)
      paste(siblings$dataset, siblings$var_name, sep = "|||")
    else character(0)

    copy_from_ui <- if (length(other_keys) > 0L) {
      ok_labels <- setNames(other_keys, sapply(other_keys, function(k) {
        p <- strsplit(k, "|||", fixed = TRUE)[[1L]]
        paste(p[1L], p[2L])
      }))
      div(class = "d-flex align-items-center gap-2 mb-3",
        tags$label("Copy from:", class = "small fw-semibold mb-0 flex-shrink-0"),
        selectInput("recode_copy_from_select", NULL,
                    choices = ok_labels, width = "auto"),
        actionButton("btn_recode_copy_from", "Apply",
                     class = "btn-sm btn-outline-secondary")
      )
    }

    div(
      class = "card mb-2 border-primary",
      div(class = "card-header py-2 d-flex justify-content-between align-items-center",
        tags$span(class = "fw-semibold small",
                  "Response code editor — ",
                  tags$code(paste0(vn, " [", ds, "]"))),
        tags$button("×", class = "btn btn-sm btn-outline-secondary",
                    onclick = "Shiny.setInputValue('btn_recode_close',Math.random())")
      ),
      div(class = "card-body",
        nav_bar, banner,
        if (has_cats)
          div(class = "mb-3", DT::dataTableOutput("recode_dt_panel"))
        else
          div(class = "text-muted small fst-italic mb-3",
              "Numeric variable — no response codes to recode."),
        copy_from_ui,
        div(class = "d-flex gap-2 flex-wrap",
          actionButton("btn_recode_reset",       "Reset to original",
                       class = "btn-sm btn-outline-secondary"),
          actionButton("btn_recode_set_ref",     "Set as reference coding",
                       class = "btn-sm btn-outline-info"),
          actionButton("btn_recode_confirm",     "✓ Confirm recode",
                       class = "btn-sm btn-outline-success"),
          actionButton("btn_recode_none",        "No recode needed",
                       class = "btn-sm btn-outline-secondary"),
          actionButton("btn_recode_incompatible","Mark incompatible",
                       class = "btn-sm btn-outline-danger")
        )
      )
    )
  })

  output$recode_dt_panel <- DT::renderDataTable({
    key <- recode_panel_key()
    if (is.null(key)) return(DT::datatable(data.frame()))
    parts <- strsplit(key, "|||", fixed = TRUE)[[1L]]
    if (length(parts) < 2L) return(DT::datatable(data.frame()))
    ds <- parts[1L]; vn <- parts[2L]

    m   <- var_mapping()
    row <- m[m$dataset == ds & m$var_name == vn, , drop = FALSE]
    if (nrow(row) == 0L) return(DT::datatable(data.frame()))

    lbl     <- all_labels()
    val_set <- row$value_labels[1L]
    if (is.na(val_set) || nchar(val_set) == 0 || is.null(lbl) || nrow(lbl) == 0)
      return(DT::datatable(
        data.frame(Note = "No value labels available"),
        rownames = FALSE, options = list(dom = "t")))

    lb <- lbl[lbl$dataset == ds & lbl$label_set == val_set, ]
    if (nrow(lb) == 0L)
      return(DT::datatable(
        data.frame(Note = "No value labels found"),
        rownames = FALSE, options = list(dom = "t")))
    lb <- lb[order(lb$value), ]

    rd <- recode_data(); ex <- rd[[key]]$df
    disp <- data.frame(
      `Original Code`  = lb$value,
      `Original Label` = lb$label_text,
      `→`         = "→",
      `New Code`       = if (!is.null(ex)) ex$new_code else lb$value,
      check.names = FALSE, stringsAsFactors = FALSE
    )

    DT::datatable(
      disp,
      editable = list(target = "cell", disable = list(columns = c(0L, 1L, 2L))),
      rownames = FALSE, class = "table-sm table-bordered",
      options  = list(
        dom = "t", pageLength = -1, autoWidth = FALSE,
        columnDefs = list(
          list(width = "80px",  className = "text-center", targets = 0L),
          list(width = "220px", targets = 1L),
          list(width = "30px",  className = "text-center text-muted", targets = 2L),
          list(width = "80px",  className = "text-center", targets = 3L)
        )
      )
    )
  }, server = FALSE)

  observeEvent(input$recode_dt_panel_cell_edit, {
    info <- input$recode_dt_panel_cell_edit
    if (info$col != 3L) return()
    key <- recode_panel_key()
    if (is.null(key)) return()
    rd <- recode_data()
    if (is.null(rd[[key]])) return()
    r  <- info$row
    if (r < 1L || r > nrow(rd[[key]]$df)) return()
    rd[[key]]$df$new_code[r] <-
      suppressWarnings(as.integer(trimws(as.character(info$value))))
    recode_data(rd)
    parts <- strsplit(key, "|||", fixed = TRUE)[[1L]]
    m <- var_mapping()
    idx <- m$dataset == parts[1L] & m$var_name == parts[2L]
    if (any(idx) && m$recode_status[idx][1L] %in% c("none", "pending"))
      m$recode_status[idx] <- "pending"
    var_mapping(m)
  })

  # ── Recode actions ──────────────────────────────────────────────────────────

  .set_recode_status <- function(key, status) {
    parts <- strsplit(key, "|||", fixed = TRUE)[[1L]]
    m <- var_mapping()
    idx <- m$dataset == parts[1L] & m$var_name == parts[2L]
    m$recode_status[idx] <- status
    var_mapping(m)
  }

  observeEvent(input$btn_recode_confirm, {
    key <- recode_panel_key(); if (is.null(key)) return()
    .set_recode_status(key, "confirmed")
    showNotification("Recode confirmed.", type = "message", duration = 2)
  })

  observeEvent(input$btn_recode_none, {
    key <- recode_panel_key(); if (is.null(key)) return()
    .set_recode_status(key, "none")
    showNotification("Marked as no recode needed.", type = "message", duration = 2)
  })

  observeEvent(input$btn_recode_incompatible, {
    key <- recode_panel_key(); if (is.null(key)) return()
    .set_recode_status(key, "incompatible")
    showNotification("Marked as incompatible.", type = "warning", duration = 2)
  })

  observeEvent(input$btn_recode_reset, {
    key <- recode_panel_key(); if (is.null(key)) return()
    rd  <- recode_data(); rd[[key]] <- NULL; recode_data(rd)
    .set_recode_status(key, "none")
    parts <- strsplit(key, "|||", fixed = TRUE)[[1L]]
    .ensure_recode(parts[1L], parts[2L])
    showNotification("Reset to original codes.", type = "message", duration = 2)
  })

  observeEvent(input$btn_recode_set_ref, {
    key <- recode_panel_key(); if (is.null(key)) return()
    m   <- var_mapping()
    p   <- strsplit(key, "|||", fixed = TRUE)[[1L]]
    row <- m[m$dataset == p[1L] & m$var_name == p[2L], , drop = FALSE]
    if (nrow(row) == 0L) return()
    rm <- reference_key_map(); rm[[row$harmonised_name[1L]]] <- key
    reference_key_map(rm)
    showNotification(paste0("Set ", p[2L], " [", p[1L], "] as reference coding."),
                     type = "message", duration = 2)
  })

  observeEvent(input$btn_recode_copy_from, {
    from_key <- input$recode_copy_from_select
    to_key   <- recode_panel_key()
    if (is.null(from_key) || is.null(to_key) || from_key == to_key) return()
    fp <- strsplit(from_key, "|||", fixed = TRUE)[[1L]]
    .ensure_recode(fp[1L], fp[2L])
    rd <- recode_data()
    if (is.null(rd[[from_key]]) || is.null(rd[[to_key]])) return()
    from_df <- rd[[from_key]]$df; to_df <- rd[[to_key]]$df
    for (i in seq_len(nrow(to_df))) {
      lc      <- tolower(trimws(to_df$old_label[i]))
      ref_idx <- which(tolower(trimws(from_df$old_label)) == lc)
      if (length(ref_idx) > 0L)
        to_df$new_code[i] <- from_df$new_code[ref_idx[1L]]
      else if (i <= nrow(from_df))
        to_df$new_code[i] <- from_df$new_code[i]
    }
    rd[[to_key]]$df <- to_df; recode_data(rd)
    .set_recode_status(to_key, "pending")
    showNotification("Recode copied.", type = "message", duration = 2)
  })

  observeEvent(input$btn_apply_recode_all, {
    from_key <- recode_panel_key()
    nav_keys <- recode_nav_keys()
    if (is.null(from_key) || length(nav_keys) <= 1L) return()
    rd <- recode_data()
    if (is.null(rd[[from_key]])) return()
    from_df   <- rd[[from_key]]$df
    other_keys <- setdiff(nav_keys, from_key)
    n_applied  <- 0L
    for (k in other_keys) {
      if (is.null(rd[[k]])) next
      if (nrow(rd[[k]]$df) != nrow(from_df)) next
      rd[[k]]$df$new_code <- from_df$new_code
      .set_recode_status(k, "pending")
      n_applied <- n_applied + 1L
    }
    recode_data(rd)
    showNotification(
      sprintf("Applied recode to %d other variable%s.",
              n_applied, if (n_applied != 1L) "s" else ""),
      type = "message", duration = 3)
  })

  # ── Harmonisation summary panel ────────────────────────────────────────────

  output$harmonisation_summary_panel <- renderUI({
    show <- show_summary()
    btn  <- actionButton("btn_toggle_summary",
                         if (show) "Hide harmonisation summary ▲"
                         else      "Show harmonisation summary ▼",
                         class = "btn-sm btn-outline-secondary mb-2")
    if (!show) return(div(btn))

    m <- var_mapping()
    if (is.null(m) || nrow(m) == 0)
      return(div(btn, div(class = "text-muted small mt-2", "No variables in plan.")))

    div(btn, card(
      card_header(class = "py-2 small fw-semibold",
                  "Harmonisation summary — one row per harmonised variable"),
      card_body(class = "p-0", DT::dataTableOutput("plan_summary_dt"))
    ))
  })

  output$plan_summary_dt <- DT::renderDataTable({
    m <- var_mapping()
    if (is.null(m) || nrow(m) == 0)
      return(DT::datatable(data.frame()))

    hnames <- unique(m$harmonised_name)
    rows <- lapply(hnames, function(hn) {
      r     <- m[m$harmonised_name == hn, , drop = FALSE]
      dsets <- unique(r$dataset)
      waves <- sort(unique(r$wave[!is.na(r$wave) & r$wave != ""]))
      worst <- if ("incompatible" %in% r$recode_status) "incompatible"
               else if ("pending" %in% r$recode_status) "pending"
               else if ("confirmed" %in% r$recode_status) "confirmed"
               else "none"
      issues <- character(0)
      if (length(dsets) == 1L)
        issues <- c(issues, "Single dataset only — not harmonised across datasets")
      if (worst == "incompatible") issues <- c(issues, "Incompatible response codes")

      data.frame(
        `Harmonised Name`  = htmltools::htmlEscape(hn),
        `Harmonised Label` = htmltools::htmlEscape(r$harmonised_label[1L] %||% ""),
        Datasets = paste(sapply(dsets, function(d)
          paste0('<span class="badge bg-secondary me-1">',
                 htmltools::htmlEscape(d), '</span>')), collapse = ""),
        Waves      = htmltools::htmlEscape(
          if (length(waves) == 0L) "—" else paste(waves, collapse = ", ")),
        `N Vars`   = nrow(r),
        `Recode Status` = .recode_badge(worst),
        Issues     = if (length(issues) == 0L)
          '<span class="text-success small">✓</span>'
        else
          paste0('<span class="text-warning small">',
                 htmltools::htmlEscape(paste(issues, collapse = "; ")),
                 '</span>'),
        check.names = FALSE, stringsAsFactors = FALSE
      )
    })
    tbl <- do.call(rbind, rows)

    DT::datatable(tbl, escape = FALSE, rownames = FALSE, class = "table-sm",
                  options = list(dom = "t", pageLength = -1, autoWidth = TRUE,
                    columnDefs = list(list(orderable = FALSE, targets = c(2L, 5L, 6L)))))
  }, server = FALSE)

  observeEvent(input$btn_toggle_summary, { show_summary(!show_summary()) })

  # ── Bulk ops result banner ─────────────────────────────────────────────────

  output$bulk_result <- renderUI({
    msg <- bulk_result_msg()
    if (is.null(msg)) return(NULL)
    div(class = paste0("alert alert-", msg$type, " small py-2 mb-0 mt-2"), msg$text)
  })

  observeEvent(input$btn_autofill_labels, {
    m <- var_mapping(); if (is.null(m) || nrow(m) == 0) return()
    n <- 0L
    for (cn in unique(m$harmonised_name)) {
      idx  <- m$harmonised_name == cn
      is_default <- all(is.na(m$harmonised_label[idx]) |
                          m$harmonised_label[idx] == m$var_label[idx] |
                          nchar(as.character(m$harmonised_label[idx])) == 0)
      if (is_default) {
        freq <- sort(table(m$var_label[idx]), decreasing = TRUE)
        m$harmonised_label[idx] <- names(freq)[1L]; n <- n + 1L
      }
    }
    var_mapping(m)
    bulk_result_msg(list(type = "success",
                         text = sprintf("Auto-filled labels for %d group(s).", n)))
  })

  observeEvent(input$btn_validate_names, {
    m <- var_mapping(); if (is.null(m) || nrow(m) == 0) return()
    nms     <- unique(m$harmonised_name)
    invalid <- nms[!vapply(nms, .is_valid_r_name, logical(1L))]
    if (length(invalid) == 0L)
      bulk_result_msg(list(type = "success",
        text = sprintf("All %d harmonised name(s) are valid R identifiers.", length(nms))))
    else
      bulk_result_msg(list(type = "danger",
        text = paste0("Invalid: ", paste(invalid, collapse = ", "))))
  })

  observeEvent(input$btn_check_recodes, {
    m   <- var_mapping(); lbl <- all_labels()
    if (is.null(m) || nrow(m) == 0) return()
    n_need <- 0L; n_ok <- 0L
    for (cn in unique(m$harmonised_name)) {
      rows <- m[m$harmonised_name == cn, , drop = FALSE]
      wl   <- rows[!is.na(rows$value_labels) & nchar(rows$value_labels) > 0, ]
      if (nrow(wl) < 2L) { n_ok <- n_ok + 1L; next }
      sets  <- lapply(seq_len(nrow(wl)), function(i)
        sort(lbl[lbl$dataset == wl$dataset[i] &
                   lbl$label_set == wl$value_labels[i], "value"]))
      first <- sets[[1L]]
      if (all(vapply(sets, function(s)
        length(s) == length(first) && all(s == first), logical(1L))))
        n_ok <- n_ok + 1L else n_need <- n_need + 1L
    }
    bulk_result_msg(list(
      type = if (n_need == 0L) "success" else "warning",
      text = sprintf("%d group(s) need recode review; %d are clean.", n_need, n_ok)))
  })

  # ── Export / import template ───────────────────────────────────────────────

  output$btn_export_template <- downloadHandler(
    filename = function()
      paste0("harmonisation_template_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content = function(file) {
      m <- var_mapping()
      if (is.null(m) || nrow(m) == 0) { write.csv(data.frame(), file, row.names = FALSE); return() }
      out <- m[, intersect(c("harmonised_name", "harmonised_label", "dataset",
                             "var_name", "var_label", "recode_status", "notes"),
                           names(m)), drop = FALSE]
      write.csv(out, file, row.names = FALSE)
    }
  )

  observeEvent(input$btn_import_template, {
    req(input$btn_import_template)
    tryCatch({
      tpl <- read.csv(input$btn_import_template$datapath,
                      stringsAsFactors = FALSE, na.strings = c("", "NA"))
      miss <- setdiff(c("harmonised_name", "dataset", "var_name"), names(tpl))
      if (length(miss) > 0) {
        showNotification(paste("Missing columns:", paste(miss, collapse = ", ")),
                         type = "error"); return()
      }
      plan <- list()
      for (i in seq_len(nrow(tpl))) {
        ds <- tpl$dataset[i]; vn <- tpl$var_name[i]
        if (is.null(plan[[ds]])) plan[[ds]] <- character(0)
        if (!vn %in% plan[[ds]]) plan[[ds]] <- c(plan[[ds]], vn)
      }
      harmonisation_plan(plan)
      m <- var_mapping()
      if (nrow(m) > 0) {
        for (i in seq_len(nrow(tpl))) {
          idx <- m$dataset == tpl$dataset[i] & m$var_name == tpl$var_name[i]
          if (!any(idx)) next
          m$harmonised_name[idx] <- tpl$harmonised_name[i]
          if ("harmonised_label" %in% names(tpl) && !is.na(tpl$harmonised_label[i]))
            m$harmonised_label[idx] <- tpl$harmonised_label[i]
          if ("notes" %in% names(tpl) && !is.na(tpl$notes[i]))
            m$notes[idx] <- tpl$notes[i]
        }
        var_mapping(m)
      }
      bulk_result_msg(list(type = "success",
        text = sprintf("Imported %d variable(s).", nrow(tpl))))
    }, error = function(e) {
      showNotification(paste("Import failed:", conditionMessage(e)), type = "error")
    })
  })
}

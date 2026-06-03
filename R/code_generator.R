# code_generator.R — generates harmonisation R script from construct-first state

# ── Wave detection ─────────────────────────────────────────────────────────────
.cg_wave_patterns <- c(
  "^(T)(\\d+)_",    "^(Time)(\\d+)_",  "^(W)(\\d+)_",    "^(Wave)(\\d+)_",
  "^(w)(\\d+)_",    "^(t)(\\d+)_",
  "^(T)(\\d+)(?=[a-zA-Z])",    "^(Time)(\\d+)(?=[a-zA-Z])",
  "^(W)(\\d+)(?=[a-zA-Z])",    "^(Wave)(\\d+)(?=[a-zA-Z])",
  "^(w)(\\d+)(?=[a-zA-Z])",    "^(t)(\\d+)(?=[a-zA-Z])"
)

.cg_detect_wave <- function(var_name) {
  for (pat in .cg_wave_patterns) {
    m <- regexpr(pat, var_name, perl = TRUE)
    if (m[1L] != -1L) {
      caps <- attr(m, "capture.start"); lens <- attr(m, "capture.length")
      pfx  <- substr(var_name, caps[1L], caps[1L] + lens[1L] - 1L)
      num  <- substr(var_name, caps[2L], caps[2L] + lens[2L] - 1L)
      base <- substring(var_name, attr(m, "match.length") + 1L)
      return(list(has_wave = TRUE, prefix = pfx, wave_num = as.integer(num), base_var = base))
    }
  }
  list(has_wave = FALSE, prefix = NA_character_, wave_num = NA_integer_, base_var = var_name)
}

.cg_dataset_wave_prefix <- function(var_names) {
  pfxs <- unique(Filter(Negate(is.na), sapply(var_names, function(v) .cg_detect_wave(v)$prefix)))
  if (length(pfxs) == 0L) NA_character_ else pfxs[1L]
}

.cg_cols_match <- function(prefix) paste0('"^', prefix, '[0-9]+"')

# ── Recode helpers ─────────────────────────────────────────────────────────────
.gen_recode_block <- function(ds, base_var, harm_name, recode_rules) {
  if (is.null(recode_rules) || nrow(recode_rules) == 0) return(NULL)
  rules <- recode_rules[
    !is.na(recode_rules$dataset)  & recode_rules$dataset  == ds &
    !is.na(recode_rules$base_var) & recode_rules$base_var == base_var &
    !is.na(recode_rules$confirmed) & recode_rules$confirmed, , drop = FALSE]
  if (nrow(rules) == 0) return(NULL)
  rules <- rules[order(rules$old_code), ]

  old_desc <- paste(paste0(rules$old_code, "=",
    ifelse(is.na(rules$old_label) | rules$old_label == "", "?", rules$old_label)),
    collapse = ", ")
  new_desc <- paste(paste0(rules$new_code, "=",
    ifelse(is.na(rules$new_label) | rules$new_label == "", "?", rules$new_label)),
    collapse = ", ")

  cases <- paste0("      ", harm_name, " == ", rules$old_code, "L ~ ", rules$new_code, "L,")
  c(
    paste0("  # Recode: ", ds, " ", base_var, " (", old_desc, ") → (", new_desc, ")"),
    "  mutate(",
    paste0("    ", harm_name, " = dplyr::case_when("),
    cases,
    paste0("      TRUE ~ ", harm_name),
    "    )",
    "  ) %>%"
  )
}

.gen_pending_recode_comment <- function(ds, base_var, harm_name) {
  c(
    paste0("  # WARNING: recode for '", harm_name, "' in ", ds, " not confirmed"),
    paste0("  # Confirm the recode in the app or comment out manually before use")
  )
}

# ── Section header helper ──────────────────────────────────────────────────────
.rl <- function(title) {
  dashes <- paste(rep("-", max(0L, 76L - nchar(title) - 5L)), collapse = "")
  paste0("# -- ", title, " ", dashes)
}

# ── Per-dataset block: flat (cross-sectional) ─────────────────────────────────
.gen_flat_block <- function(ds, ds_obj, harm_names, base_vars, recode_rules) {
  chg <- base_vars != harm_names

  select_str <- paste(unique(c(base_vars)), collapse = ", ")
  rn_lines <- NULL
  if (any(chg)) {
    o <- base_vars[chg]; h <- harm_names[chg]
    w <- max(nchar(h))
    pairs <- paste0("    ", formatC(h, width = w, flag = "-"), " = ", o)
    pairs[seq_len(length(pairs) - 1L)] <- paste0(pairs[seq_len(length(pairs) - 1L)], ",")
    rn_lines <- c("  rename(", pairs, "  ) %>%")
  }

  recode_lines <- NULL
  for (k in seq_along(base_vars)) {
    bv <- base_vars[k]; hn <- harm_names[k]
    rl  <- .gen_recode_block(ds, bv, hn, recode_rules)
    if (!is.null(rl)) recode_lines <- c(recode_lines, rl)
    # Check for pending (assigned but not confirmed)
    if (is.null(rl) && !is.null(recode_rules)) {
      pending <- recode_rules[
        !is.na(recode_rules$dataset) & recode_rules$dataset == ds &
        !is.na(recode_rules$base_var) & recode_rules$base_var == bv &
        (!is.na(recode_rules$confirmed) & !recode_rules$confirmed), ]
      if (nrow(pending) > 0) recode_lines <- c(recode_lines, .gen_pending_recode_comment(ds, bv, hn))
    }
  }

  fin_str <- paste(harm_names, collapse = ", ")
  block <- c(
    .rl(paste("Dataset:", ds, "(cross-sectional)")),
    "",
    paste0(ds_obj, '_raw <- load_dataset("', ds, '")'),
    "",
    paste0(ds_obj, " <- ", ds_obj, "_raw %>%"),
    paste0("  select(person_id_col, ", select_str, ") %>%")
  )
  if (!is.null(rn_lines))    block <- c(block, rn_lines)
  if (!is.null(recode_lines)) block <- c(block, recode_lines)
  block <- c(block,
    "  mutate(",
    paste0('    source_dataset = "', ds, '",'),
    "    wave           = NA_integer_",
    "  ) %>%",
    paste0("  select(source_dataset, wave, person_id_col, ", fin_str, ")")
  )
  block
}

# ── Per-dataset block: longitudinal (wave variables) ──────────────────────────
.gen_wave_block <- function(ds, ds_obj, harm_names, base_vars, wave_pfx, all_wave_vars, recode_rules) {
  base_to_harm <- setNames(harm_names, base_vars)
  chg          <- base_vars != harm_names

  # All actual wave column names for this dataset's base_vars
  if (!is.null(all_wave_vars) && nrow(all_wave_vars) > 0) {
    wave_cols <- sort(unique(all_wave_vars$var_name[all_wave_vars$base_var %in% base_vars]))
    sel_cols_str <- paste0('c("', paste(wave_cols, collapse = '", "'), '")')
    sel_comment  <- "# wave columns"
  } else {
    safe_pfx     <- if (!is.null(wave_pfx) && !is.na(wave_pfx)) wave_pfx else "[A-Za-z]+[0-9]+"
    cols_sel     <- .cg_cols_match(safe_pfx)
    sel_cols_str <- paste0("matches(", cols_sel, ")")
    sel_comment  <- "# wave columns (by pattern)"
  }

  safe_pfx  <- if (!is.null(wave_pfx) && !is.na(wave_pfx)) wave_pfx else "[A-Za-z]+[0-9]+"
  names_pat <- '"^[A-Za-z]+(\\\\d+)[_]?(.+)$"'

  rn_lines <- NULL
  if (any(chg)) {
    o <- base_vars[chg]; h <- harm_names[chg]
    w <- max(nchar(h))
    pairs <- paste0("    ", formatC(h, width = w, flag = "-"), " = ", o)
    pairs[seq_len(length(pairs) - 1L)] <- paste0(pairs[seq_len(length(pairs) - 1L)], ",")
    rn_lines <- c("  rename(", pairs, "  ) %>%")
  }

  recode_lines <- NULL
  for (k in seq_along(base_vars)) {
    bv <- base_vars[k]; hn <- harm_names[k]
    rl <- .gen_recode_block(ds, bv, hn, recode_rules)
    if (!is.null(rl)) recode_lines <- c(recode_lines, rl)
    if (is.null(rl) && !is.null(recode_rules)) {
      pending <- recode_rules[
        !is.na(recode_rules$dataset) & recode_rules$dataset == ds &
        !is.na(recode_rules$base_var) & recode_rules$base_var == bv &
        (!is.na(recode_rules$confirmed) & !recode_rules$confirmed), ]
      if (nrow(pending) > 0) recode_lines <- c(recode_lines, .gen_pending_recode_comment(ds, bv, hn))
    }
  }

  block <- c(
    .rl(paste("Dataset:", ds, "(longitudinal — reshape)")),
    "",
    paste0(ds_obj, '_raw <- load_dataset("', ds, '")'),
    "",
    paste0(ds_obj, " <- ", ds_obj, "_raw %>%"),
    paste0("  select(person_id_col, ", sel_comment),
    paste0("         ", sel_cols_str, ") %>%"),
    "  pivot_longer(",
    paste0("    cols         = matches(", .cg_cols_match(safe_pfx), "),"),
    '    names_to     = c("wave", ".value"),',
    paste0("    names_pattern = ", names_pat),
    "  ) %>%",
    "  mutate(",
    "    wave           = as.integer(wave),",
    paste0('    source_dataset = "', ds, '"'),
    "  ) %>%"
  )
  if (!is.null(rn_lines))    block <- c(block, rn_lines)
  if (!is.null(recode_lines)) block <- c(block, recode_lines)
  block <- c(block,
    paste0("  select(source_dataset, wave, person_id_col, ",
           paste(harm_names, collapse = ", "), ")")
  )
  block
}

# ── Value label comment block ──────────────────────────────────────────────────
.gen_value_label_comments <- function(constructs, assignments, all_vars, all_labels) {
  if (is.null(all_labels) || nrow(all_labels) == 0) return(NULL)
  if (is.null(all_vars)   || !"value_labels" %in% names(all_vars)) return(NULL)

  active_asgn <- assignments[!is.na(assignments$excluded) & !assignments$excluded &
                               (is.na(assignments$recode_status) | assignments$recode_status != "notfound"), ]
  if (nrow(active_asgn) == 0) return(NULL)

  lines <- c(.rl("Value labels (for reference)"), "")
  datasets <- sort(unique(active_asgn$dataset))

  for (ds in datasets) {
    ds_asgn <- active_asgn[active_asgn$dataset == ds, ]
    ds_lines <- paste0("# ", ds, ":")
    any_labels <- FALSE
    for (i in seq_len(nrow(ds_asgn))) {
      bv  <- ds_asgn$base_var[i]
      hn  <- ds_asgn$harmonised_name[i]
      av_row <- all_vars[all_vars$dataset == ds &
                           (all_vars$var_name == bv |
                            (!"base_var" %in% names(all_vars) || all_vars$base_var == bv)), ]
      if (nrow(av_row) == 0 || !"value_labels" %in% names(av_row)) next
      lset <- av_row$value_labels[!is.na(av_row$value_labels) & nchar(av_row$value_labels) > 0]
      if (length(lset) == 0) next
      lset <- lset[1L]
      lb <- all_labels[all_labels$dataset == ds & all_labels$label_set == lset, ]
      if (nrow(lb) == 0) next
      lb <- lb[order(lb$value), ]
      any_labels <- TRUE
      ds_lines <- c(ds_lines, paste0("#   ", hn, " (", lset, "):"))
      ds_lines <- c(ds_lines,
        paste0("#     ", formatC(lb$value, flag = "-", width = 4L), lb$label_text))
    }
    if (any_labels) lines <- c(lines, ds_lines, "")
  }
  if (length(lines) <= 2) NULL else lines
}

# ── Main entry point ───────────────────────────────────────────────────────────
# constructs  : data.frame(harmonised_name, harmonised_label, domain, subdomain, notes)
# assignments : data.frame(harmonised_name, dataset, base_var, recode_status, excluded)
# recode_rules: data.frame(harmonised_name, dataset, base_var, old_code, old_label, new_code, new_label, confirmed)
# all_vars    : domain_labels data frame with wave columns
# all_labels  : value label definitions

generate_harmonisation_code <- function(constructs, assignments, recode_rules = NULL,
                                         all_vars = NULL, all_labels = NULL) {

  active_asgn <- assignments[!is.na(assignments$excluded) & !assignments$excluded &
                               (is.na(assignments$recode_status) | assignments$recode_status != "notfound"), , drop=FALSE]
  if (is.null(constructs) || nrow(constructs) == 0 || nrow(active_asgn) == 0) {
    return(paste(
      "# No constructs assigned yet.",
      "# Define constructs in the Construct Manager and assign variables.",
      sep = "\n"
    ))
  }

  datasets   <- sort(unique(active_asgn$dataset))
  harm_names <- sort(unique(active_asgn$harmonised_name))
  n_src      <- nrow(active_asgn)
  n_harm     <- length(harm_names)

  out <- c(
    "# Harmonisation script",
    paste0("# Generated:  ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    paste0("# Constructs: ", n_harm, " | Datasets: ", length(datasets), " | Source vars: ", n_src),
    "",
    .rl("Setup"),
    "",
    "library(dplyr)",
    "library(haven)",
    "library(tidyr)",
    "library(purrr)",
    "",
    'data_folder   <- "."    # <-- set path to your dataset files',
    'person_id_col <- "id"   # <-- set your person identifier column name',
    "",
    "load_dataset <- function(name, folder = data_folder) {",
    '  exts       <- c(".dta", ".rds", ".csv")',
    '  candidates <- file.path(folder, paste0(name, exts))',
    '  found      <- candidates[file.exists(candidates)]',
    '  if (length(found) == 0L)',
    '    stop("Dataset not found: ", name, "\\nLooked in: ", folder)',
    '  switch(tools::file_ext(found[1L]),',
    '    dta = haven::read_dta(found[1L]),',
    '    rds = readRDS(found[1L]),',
    '    csv = readr::read_csv(found[1L], show_col_types = FALSE),',
    '    stop("Unsupported: ", found[1L])',
    '  )',
    '}',
    ""
  )

  ds_objs <- make.names(datasets)

  for (i in seq_along(datasets)) {
    ds     <- datasets[i]
    ds_obj <- ds_objs[i]
    m_ds   <- active_asgn[active_asgn$dataset == ds, , drop=FALSE]

    harm_ns  <- m_ds$harmonised_name
    base_vs  <- m_ds$base_var

    # Detect wave structure: check all_vars for has_wave flag
    is_wave_ds <- FALSE
    wave_pfx   <- NA_character_
    all_wave_v <- NULL

    if (!is.null(all_vars) && all(c("has_wave", "base_var") %in% names(all_vars))) {
      av_ds <- all_vars[all_vars$dataset == ds & !is.na(all_vars$has_wave) & all_vars$has_wave &
                          all_vars$base_var %in% base_vs, , drop=FALSE]
      if (nrow(av_ds) > 0L) {
        is_wave_ds <- TRUE
        wave_pfx   <- .cg_dataset_wave_prefix(av_ds$var_name)
        all_wave_v <- av_ds
      }
    } else {
      # Detect from var names directly
      wave_pfx <- .cg_dataset_wave_prefix(base_vs)
      is_wave_ds <- !is.na(wave_pfx)
    }

    block <- if (is_wave_ds) {
      .gen_wave_block(ds, ds_obj, harm_ns, base_vs, wave_pfx, all_wave_v, recode_rules)
    } else {
      .gen_flat_block(ds, ds_obj, harm_ns, base_vs, recode_rules)
    }

    out <- c(out, block, "")
  }

  # Missing constructs per dataset (excluded or not found)
  for (hn in harm_names) {
    excl_ds <- assignments$dataset[assignments$harmonised_name == hn &
                                     (assignments$excluded | assignments$recode_status == "notfound")]
    if (length(excl_ds) > 0) {
      out <- c(out,
        paste0("# NOTE: '", hn, "' not included from: ", paste(excl_ds, collapse = ", "),
               " — will be NA for those datasets in the output"))
    }
  }
  if (any(sapply(harm_names, function(hn)
    any(assignments$harmonised_name == hn & (assignments$excluded | assignments$recode_status == "notfound"))
  ))) out <- c(out, "")

  out <- c(out,
    .rl("Stack all datasets"),
    "",
    paste0("harmonised_data <- dplyr::bind_rows(",
           paste(ds_objs, collapse = ", "), ")"),
    "",
    .rl("Summary"),
    "",
    'cat("Dimensions:", nrow(harmonised_data), "rows x", ncol(harmonised_data), "cols\\n")',
    'cat("Datasets:", paste(sort(unique(harmonised_data$source_dataset)), collapse=", "), "\\n")',
    'cat("Waves:", paste(sort(unique(harmonised_data$wave[!is.na(harmonised_data$wave)])), collapse=", "), "\\n")',
    'cat("Constructs:", paste(setdiff(names(harmonised_data), c("source_dataset","wave","person_id_col")), collapse=", "), "\\n")',
    ""
  )

  vl_lines <- .gen_value_label_comments(constructs, assignments, all_vars, all_labels)
  if (!is.null(vl_lines)) out <- c(out, vl_lines)

  out <- c(out,
    .rl("Save (uncomment to use)"),
    "",
    '# saveRDS(harmonised_data, "harmonised_data.rds")',
    '# haven::write_dta(harmonised_data, "harmonised_data.dta")',
    '# write.csv(harmonised_data, "harmonised_data.csv", row.names = FALSE)',
    ""
  )

  paste(out, collapse = "\n")
}

`%||%` <- function(x, y) if (!is.null(x) && length(x) > 0) x else y

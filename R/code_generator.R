# code_generator.R
# Generates a complete, runnable R harmonisation script from var_mapping.
# For datasets with wave-structured variables, emits pivot_longer reshape blocks.
# For datasets without wave variables, emits simple select-rename blocks.
# When recode_rules is supplied, injects case_when() blocks after rename steps.

# ---------------------------------------------------------------------------
# Inline wave-prefix detection (no dict_pipeline dependency)
# ---------------------------------------------------------------------------
.cg_wave_patterns <- c(
  "^([T])([0-9]+)_",    "^([T]ime)([0-9]+)_",
  "^([W])([0-9]+)_",    "^([W]ave)([0-9]+)_",
  "^([w])([0-9]+)_",    "^([t])([0-9]+)_",
  "^([T])([0-9]+)(?=[a-zA-Z])",    "^([T]ime)([0-9]+)(?=[a-zA-Z])",
  "^([W])([0-9]+)(?=[a-zA-Z])",    "^([W]ave)([0-9]+)(?=[a-zA-Z])",
  "^([w])([0-9]+)(?=[a-zA-Z])",    "^([t])([0-9]+)(?=[a-zA-Z])"
)

.cg_detect_wave <- function(var_name) {
  for (pat in .cg_wave_patterns) {
    m <- regexpr(pat, var_name, perl = TRUE)
    if (m[1L] != -1L) {
      caps <- attr(m, "capture.start")
      lens <- attr(m, "capture.length")
      pfx  <- substr(var_name, caps[1L], caps[1L] + lens[1L] - 1L)
      num  <- substr(var_name, caps[2L], caps[2L] + lens[2L] - 1L)
      base <- substring(var_name, attr(m, "match.length") + 1L)
      return(list(has_wave = TRUE, prefix = pfx, wave_num = as.integer(num), base_var = base))
    }
  }
  list(has_wave = FALSE, prefix = NA_character_, wave_num = NA_integer_, base_var = var_name)
}

.cg_dataset_wave_prefix <- function(var_names) {
  results  <- lapply(var_names, .cg_detect_wave)
  prefixes <- unique(Filter(Negate(is.na), sapply(results, `[[`, "prefix")))
  if (length(prefixes) == 0L) return(NA_character_)
  prefixes[1L]
}

.cg_names_pattern <- function(prefix) {
  paste0("^", prefix, "(\\\\d+)[_]?(.+)$")
}

.cg_cols_match <- function(prefix) {
  paste0('"^', prefix, '[0-9]+"')
}


# ---------------------------------------------------------------------------
# recode_data_to_rules()
# Flatten the recode_data reactiveVal list into the same format as
# the recode_rules CSV (dataset, var_name, old_code, new_code, …).
# ---------------------------------------------------------------------------
recode_data_to_rules <- function(rd) {
  if (is.null(rd) || length(rd) == 0) return(NULL)
  rows <- lapply(names(rd), function(key) {
    parts <- strsplit(key, "|||", fixed = TRUE)[[1]]
    if (length(parts) < 2) return(NULL)
    df <- rd[[key]]$df
    if (is.null(df) || nrow(df) == 0) return(NULL)
    # Emit rows where code changed or category was excluded (new_code = NA)
    changed <- is.na(df$new_code) |
               (!is.na(df$new_code) & !is.na(df$old_code) & df$new_code != df$old_code)
    if (!any(changed, na.rm = TRUE)) return(NULL)
    df <- df[changed, , drop = FALSE]
    data.frame(dataset   = parts[1], var_name  = parts[2],
               old_code  = df$old_code,  new_code  = df$new_code,
               old_label = df$old_label, new_label = df$new_label,
               stringsAsFactors = FALSE)
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(NULL)
  do.call(rbind, rows)
}

# ---------------------------------------------------------------------------
# .gen_variable_comments()
# Emit harmonised_label and notes as comment lines before a rename block.
# ---------------------------------------------------------------------------
.gen_variable_comments <- function(m_ds) {
  lines <- character(0)
  for (k in seq_len(nrow(m_ds))) {
    hn <- m_ds$harmonised_name[k]
    hl <- if ("harmonised_label" %in% names(m_ds)) m_ds$harmonised_label[k] else NA_character_
    nt <- if ("notes"            %in% names(m_ds)) m_ds$notes[k]            else NA_character_
    if (!is.na(hl) && nchar(trimws(hl)) > 0 && hl != hn)
      lines <- c(lines, paste0('  # harmonised_label: ', hn, ' — "', hl, '"'))
    if (!is.na(nt) && nchar(trimws(nt)) > 0)
      lines <- c(lines, paste0('  # Note: ', nt))
  }
  lines
}

# ---------------------------------------------------------------------------
# .gen_recode_lines()
# Builds the comment + mutate(case_when()) block for one variable.
# lookup_var : the var_name key to find in recode_rules
# harm_name  : the column name to mutate (after rename)
# ---------------------------------------------------------------------------
.gen_recode_lines <- function(ds, lookup_var, harm_name, recode_rules) {
  if (is.null(recode_rules) || nrow(recode_rules) == 0) return(NULL)

  rules <- recode_rules[!is.na(recode_rules$dataset) & recode_rules$dataset == ds &
                         !is.na(recode_rules$var_name) & recode_rules$var_name == lookup_var,
                        , drop = FALSE]
  if (nrow(rules) == 0) return(NULL)

  rules <- rules[order(rules$old_code), ]

  old_desc <- paste(
    paste0(rules$old_code, "=",
           ifelse(is.na(rules$old_label) | rules$old_label == "", "?", rules$old_label)),
    collapse = ", "
  )
  new_desc <- paste(
    paste0(rules$new_code, "=",
           ifelse(is.na(rules$new_label) | rules$new_label == "", "?", rules$new_label)),
    collapse = ", "
  )

  cases <- paste0("      ", harm_name, " == ", rules$old_code, "L ~ ", rules$new_code, "L,")

  c(
    paste0("  # Recode: ", ds, " ", lookup_var, " (", old_desc, ")"),
    paste0("  #   → aligned coding (", new_desc, ")"),
    "  mutate(",
    paste0("    ", harm_name, " = dplyr::case_when("),
    cases,
    paste0("      TRUE ~ ", harm_name),
    "    )",
    "  ) %>%"
  )
}


# ---------------------------------------------------------------------------
# generate_harmonisation_code()
# ---------------------------------------------------------------------------
# var_mapping  : data frame — dataset, var_name, var_label, value_labels, harmonised_name
# all_labels   : optional value label definitions
# all_vars     : optional full vars data frame (with wave columns)
# recode_rules : optional data frame — cluster_id, dataset, var_name,
#                old_code, new_code, old_label, new_label

generate_harmonisation_code <- function(var_mapping,
                                         all_labels   = NULL,
                                         all_vars     = NULL,
                                         recode_rules = NULL,
                                         recode_data  = NULL) {

  # Only use recodes where recode_status == "confirmed" (new schema)
  # or recode_confirmed == TRUE (old schema, backward compat)
  confirmed_keys <- if (!is.null(var_mapping) && "recode_status" %in% names(var_mapping)) {
    m_conf <- var_mapping[!is.na(var_mapping$recode_status) &
                            var_mapping$recode_status == "confirmed", ]
    paste(m_conf$dataset, m_conf$var_name, sep = "|||")
  } else if (!is.null(var_mapping) && "recode_confirmed" %in% names(var_mapping)) {
    m_conf <- var_mapping[isTRUE(var_mapping$recode_confirmed) |
                            var_mapping$recode_confirmed == TRUE, ]
    paste(m_conf$dataset, m_conf$var_name, sep = "|||")
  } else {
    if (!is.null(recode_data)) names(recode_data) else character(0)
  }

  # Merge user-edited recodes into recode_rules (confirmed only, user takes priority)
  if (!is.null(recode_data) && length(recode_data) > 0) {
    conf_recode_data <- recode_data[names(recode_data) %in% confirmed_keys]
    user_rules <- recode_data_to_rules(conf_recode_data)
    if (!is.null(user_rules) && nrow(user_rules) > 0) {
      if (is.null(recode_rules) || nrow(recode_rules) == 0) {
        recode_rules <- user_rules
      } else {
        pipe_key <- paste(recode_rules$dataset, recode_rules$var_name)
        user_key <- paste(user_rules$dataset,  user_rules$var_name)
        recode_rules <- rbind(
          recode_rules[!pipe_key %in% user_key, , drop = FALSE],
          user_rules
        )
      }
    }
  }

  if (is.null(var_mapping) || nrow(var_mapping) == 0) {
    return(paste(
      "# No variables in harmonisation plan.",
      "# Add variables in the Browse tab, then return here.",
      sep = "\n"
    ))
  }

  datasets <- sort(unique(var_mapping$dataset))
  all_harm <- sort(unique(var_mapping$harmonised_name))
  n_src    <- nrow(var_mapping)
  n_harm   <- length(all_harm)

  rl <- function(title) {
    dashes <- paste(rep("-", max(0L, 76L - nchar(title) - 5L)), collapse = "")
    paste0("# -- ", title, " ", dashes)
  }

  out <- c(
    "# Harmonisation script",
    paste0("# Generated:  ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    paste0("# Datasets:   ", paste(datasets, collapse = ", ")),
    paste0("# Variables:  ", n_harm, " harmonised name(s) from ", n_src, " source variable(s)"),
    "",
    rl("Setup"),
    "",
    "library(dplyr)",
    "library(haven)",
    "library(tidyr)",
    "library(purrr)",
    "",
    'data_folder <- "."  # <-- set path to folder containing your dataset files',
    'person_id_col <- "person_id"  # <-- set your person identifier column name',
    "",
    "load_dataset <- function(name, folder = data_folder) {",
    '  exts       <- c(".dta", ".csv", ".rds")',
    '  candidates <- file.path(folder, paste0(name, exts))',
    '  found      <- candidates[file.exists(candidates)]',
    '  if (length(found) == 0)',
    '    stop("Dataset not found: ", name, "\\nLooked in: ", folder)',
    '  switch(tools::file_ext(found[1]),',
    '    dta = haven::read_dta(found[1]),',
    '    csv = readr::read_csv(found[1], show_col_types = FALSE),',
    '    rds = readRDS(found[1]),',
    '    stop("Unsupported file type: ", found[1])',
    '  )',
    '}',
    ""
  )

  ds_obj_names <- make.names(datasets)

  for (i in seq_along(datasets)) {
    ds     <- datasets[i]
    ds_obj <- ds_obj_names[i]
    m_ds   <- var_mapping[var_mapping$dataset == ds, , drop = FALSE]

    wave_pfx <- .cg_dataset_wave_prefix(m_ds$var_name)
    has_wave_in_mapping <- !is.na(wave_pfx)

    if (!has_wave_in_mapping && !is.null(all_vars) &&
        all(c("has_wave", "base_var") %in% names(all_vars))) {
      av_ds_wave <- all_vars[all_vars$dataset == ds &
                               !is.na(all_vars$has_wave) & all_vars$has_wave, ]
      if (nrow(av_ds_wave) > 0L && any(av_ds_wave$base_var %in% m_ds$var_name)) {
        has_wave_in_mapping <- TRUE
        wave_pfx <- .cg_dataset_wave_prefix(av_ds_wave$var_name)
      }
    }

    if (has_wave_in_mapping) {
      block <- .gen_wave_block(ds, ds_obj, m_ds, wave_pfx, all_vars, recode_rules, rl)
    } else {
      block <- .gen_flat_block(ds, ds_obj, m_ds, recode_rules, rl)
    }

    out <- c(out, block, "")
  }

  out <- c(out,
    rl("Stack all datasets"),
    "",
    paste0("harmonised_data <- dplyr::bind_rows(", paste(ds_obj_names, collapse = ", "), ")"),
    "",
    rl("Summary"),
    "",
    'cat("Harmonised dataset dimensions:", nrow(harmonised_data), "rows x",',
    '    ncol(harmonised_data), "cols\\n")',
    'cat("Datasets:", paste(sort(unique(harmonised_data$source_dataset)), collapse = ", "), "\\n")',
    'cat("Waves:   ", paste(sort(unique(harmonised_data$wave)), collapse = ", "), "\\n")',
    'cat("Variables:", paste(names(harmonised_data), collapse = ", "), "\\n")',
    ""
  )

  has_vlabs <- "value_labels" %in% names(var_mapping)
  if (has_vlabs && !is.null(all_labels) && nrow(all_labels) > 0) {
    plan_labs <- var_mapping[!is.na(var_mapping$value_labels) &
                               nchar(var_mapping$value_labels) > 0,
                             c("dataset", "value_labels"), drop = FALSE]
    plan_labs <- unique(plan_labs)

    if (nrow(plan_labs) > 0) {
      vl_lines <- c(rl("Value labels (for reference)"), "")

      for (ds in sort(unique(plan_labs$dataset))) {
        ds_sets  <- sort(plan_labs[plan_labs$dataset == ds, "value_labels"])
        vl_lines <- c(vl_lines, paste0("# ", ds, ":"))

        for (lset in ds_sets) {
          rows <- all_labels[all_labels$dataset == ds & all_labels$label_set == lset, ]
          if (nrow(rows) == 0) {
            vl_lines <- c(vl_lines, paste0("#   ", lset, " (no definitions found)"))
          } else {
            rows     <- rows[order(rows$value), ]
            vl_lines <- c(vl_lines, paste0("#   ", lset, ":"))
            vl_lines <- c(vl_lines,
              paste0("#     ", formatC(rows$value, flag = "-", width = 4L), rows$label_text))
          }
        }
        vl_lines <- c(vl_lines, "")
      }
      out <- c(out, vl_lines)
    }
  }

  out <- c(out,
    rl("Save (uncomment to use)"),
    "",
    '# saveRDS(harmonised_data, "harmonised_data.rds")',
    '# haven::write_dta(harmonised_data, "harmonised_data.dta")',
    '# write.csv(harmonised_data, "harmonised_data.csv", row.names = FALSE)',
    ""
  )

  paste(out, collapse = "\n")
}


# ---------------------------------------------------------------------------
# .gen_wave_block() — dataset block with pivot_longer reshape
# ---------------------------------------------------------------------------
.gen_wave_block <- function(ds, ds_obj, m_ds, wave_pfx, all_vars, recode_rules = NULL, rl) {

  harm_names <- m_ds$harmonised_name
  base_vars  <- m_ds$var_name

  if (!is.null(all_vars) && all(c("has_wave", "base_var") %in% names(all_vars))) {
    av_wave <- all_vars[all_vars$dataset == ds & !is.na(all_vars$has_wave) &
                          all_vars$has_wave & all_vars$base_var %in% base_vars, ]
    wave_col_names <- sort(unique(av_wave$var_name))
  } else {
    wave_col_names <- character(0)
  }

  base_to_harm <- setNames(harm_names, base_vars)
  chg          <- base_vars != harm_names
  any_rename   <- any(chg)

  names_pat <- paste0("^[A-Za-z]+(\\\\d+)[_]?(.+)$")
  cols_sel  <- .cg_cols_match(wave_pfx %||% "[A-Za-z]+[0-9]+")

  if (length(wave_col_names) > 0L) {
    sel_cols_str <- paste0('c("', paste(wave_col_names, collapse = '", "'), '")')
    sel_comment  <- "# Wave columns (all variants detected in data)"
  } else {
    sel_cols_str <- paste0("matches(", cols_sel, ")")
    sel_comment  <- "# Wave columns (detected by prefix pattern)"
  }

  rn_lines <- NULL
  if (any_rename) {
    o_ch <- base_vars[chg]
    h_ch <- harm_names[chg]
    w    <- max(nchar(h_ch))
    pairs <- paste0("    ", formatC(h_ch, width = w, flag = "-"), " = ", o_ch)
    pairs[seq_len(length(pairs) - 1L)] <- paste0(pairs[seq_len(length(pairs) - 1L)], ",")
    rn_lines <- c("  rename(", pairs, "  ) %>%")
  }

  # Recode blocks: match by base_var name (column name after pivot_longer)
  recode_lines <- NULL
  for (k in seq_along(base_vars)) {
    rl_k <- .gen_recode_lines(ds, base_vars[k], harm_names[k], recode_rules)
    if (!is.null(rl_k)) recode_lines <- c(recode_lines, rl_k)
  }

  var_cmts <- .gen_variable_comments(m_ds)

  block <- c(
    rl(paste("Dataset:", ds, "(longitudinal — reshape)")),
    "",
    paste0(ds_obj, '_raw <- load_dataset("', ds, '")'),
    "",
    var_cmts,
    paste0(ds_obj, " <- ", ds_obj, "_raw %>%"),
    paste0('  select(person_id_col, ', sel_comment),
    paste0("         ", sel_cols_str, ") %>%"),
    "  pivot_longer(",
    paste0("    cols         = matches(", cols_sel, "),"),
    '    names_to     = c("wave", ".value"),',
    paste0('    names_pattern = "', names_pat, '"'),
    "  ) %>%",
    "  mutate(",
    '    wave         = as.integer(wave),',
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


# ---------------------------------------------------------------------------
# .gen_flat_block() — dataset block for non-wave data
# ---------------------------------------------------------------------------
.gen_flat_block <- function(ds, ds_obj, m_ds, recode_rules = NULL, rl) {

  orig <- m_ds$var_name
  harm <- m_ds$harmonised_name
  chg  <- orig != harm

  sel_str <- paste(orig, collapse = ", ")

  rn_lines <- NULL
  if (any(chg)) {
    o_ch  <- orig[chg]
    h_ch  <- harm[chg]
    w     <- max(nchar(h_ch))
    pairs <- paste0("    ", formatC(h_ch, width = w, flag = "-"), " = ", o_ch)
    pairs[seq_len(length(pairs) - 1L)] <- paste0(pairs[seq_len(length(pairs) - 1L)], ",")
    rn_lines <- c("  rename(", pairs, "  ) %>%")
  }

  # Recode blocks: one per variable that has matching rules
  recode_lines <- NULL
  for (k in seq_along(orig)) {
    rl_k <- .gen_recode_lines(ds, orig[k], harm[k], recode_rules)
    if (!is.null(rl_k)) recode_lines <- c(recode_lines, rl_k)
  }

  fin_str <- paste(harm, collapse = ", ")

  var_cmts <- .gen_variable_comments(m_ds)

  block <- c(
    rl(paste("Dataset:", ds, "(cross-sectional)")),
    "",
    paste0(ds_obj, '_raw <- load_dataset("', ds, '")'),
    "",
    var_cmts,
    paste0(ds_obj, " <- ", ds_obj, "_raw %>%"),
    paste0("  select(person_id_col, ", sel_str, ") %>%")
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

`%||%` <- function(x, y) if (!is.null(x)) x else y

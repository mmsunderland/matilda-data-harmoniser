# code_generator.R
# Generates a complete, runnable R harmonisation script from var_mapping.

generate_harmonisation_code <- function(var_mapping, all_labels = NULL) {

  if (is.null(var_mapping) || nrow(var_mapping) == 0) {
    return(paste(
      "# No variables in harmonisation plan.",
      "# Add variables in the Browse tab, then return here.",
      sep = "\n"
    ))
  }

  datasets  <- sort(unique(var_mapping$dataset))
  all_harm  <- sort(unique(var_mapping$harmonised_name))
  n_src     <- nrow(var_mapping)
  n_harm    <- length(all_harm)

  rl <- function(title) {
    dashes <- paste(rep("-", max(0L, 76L - nchar(title) - 5L)), collapse = "")
    paste0("# -- ", title, " ", dashes)
  }

  out <- c(
    "# Harmonisation script",
    paste0("# Generated:  ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    paste0("# Datasets:   ", paste(datasets, collapse = ", ")),
    paste0("# Variables:  ", n_harm, " harmonised name(s) from ",
           n_src, " source variable(s)"),
    "",
    rl("Setup"),
    "",
    "library(dplyr)",
    "library(haven)",
    "",
    'data_folder <- "."  # <-- set path to folder containing your dataset files',
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

    orig <- m_ds$var_name
    harm <- m_ds$harmonised_name
    chg  <- orig != harm

    sel_str <- paste(orig, collapse = ", ")

    # rename() — only emit changed pairs; align = signs
    rn_lines <- NULL
    if (any(chg)) {
      o_ch  <- orig[chg]
      h_ch  <- harm[chg]
      w     <- max(nchar(h_ch))
      pairs <- paste0(
        "    ",
        formatC(h_ch, width = w, flag = "-"),
        " = ",
        o_ch
      )
      pairs[seq_len(length(pairs) - 1L)] <-
        paste0(pairs[seq_len(length(pairs) - 1L)], ",")
      rn_lines <- c("  rename(", pairs, "  ) %>%")
    }

    fin_str <- paste(harm, collapse = ", ")

    block <- c(
      rl(paste("Dataset:", ds)),
      "",
      paste0(ds_obj, '_raw <- load_dataset("', ds, '")'),
      "",
      paste0(ds_obj, " <- ", ds_obj, "_raw %>%"),
      paste0("  select(", sel_str, ") %>%")
    )
    if (!is.null(rn_lines)) block <- c(block, rn_lines)
    block <- c(block,
      paste0('  mutate(.dataset = "', ds, '") %>%'),
      paste0("  select(.dataset, ", fin_str, ")")
    )

    out <- c(out, block, "")
  }

  out <- c(out,
    rl("Combine"),
    "",
    paste0("harmonised <- bind_rows(", paste(ds_obj_names, collapse = ", "), ")"),
    "",
    'cat("Harmonised dataset:\\n")',
    'cat("  Rows:     ", nrow(harmonised), "\\n")',
    'cat("  Columns:  ", ncol(harmonised), "\\n")',
    'cat("  Datasets: ", paste(sort(unique(harmonised$.dataset)), collapse = ", "), "\\n")',
    ""
  )

  # Value labels reference section
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
          rows <- all_labels[all_labels$dataset == ds &
                               all_labels$label_set == lset, ]
          if (nrow(rows) == 0) {
            vl_lines <- c(vl_lines, paste0("#   ", lset, " (no definitions found)"))
          } else {
            rows     <- rows[order(rows$value), ]
            vl_lines <- c(vl_lines, paste0("#   ", lset, ":"))
            vl_lines <- c(vl_lines,
              paste0("#     ", formatC(rows$value, flag = "-", width = 4L),
                     rows$label_text))
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
    '# saveRDS(harmonised, "harmonised_data.rds")',
    '# haven::write_dta(harmonised, "harmonised_data.dta")',
    '# write.csv(harmonised, "harmonised_data.csv", row.names = FALSE)',
    ""
  )

  paste(out, collapse = "\n")
}

# parse_dct.R
# Parses Stata .dct (infile dictionary) files into a tidy variable metadata
# data frame. No external package dependencies.

# ---------------------------------------------------------------------------
# parse_dct() — single file
# ---------------------------------------------------------------------------
# Stata .dct format:
#   dictionary {
#     <type>  <varname>[:<VALUELABELSET>]  `"<variable label>"'
#     _newline
#     ...
#   }
#   [raw data rows follow]
#
# Variable definition lines are identified by the presence of `" (Stata's
# backtick-double-quote label syntax). _newline tokens and the dictionary
# braces are skipped.
#
# Value label set spacing has two variants:
#   varname:LABELSET   — colon fused to varname (no whitespace)
#   varname  :LABELSET — colon separated by whitespace (colon starts its own token)

parse_dct <- function(filepath) {

  lines <- readLines(filepath, warn = FALSE)

  # Variable definition lines all contain `"
  var_lines <- lines[grepl('`"', lines, fixed = TRUE)]

  dataset_name <- sub("\\.dct$", "", basename(filepath), ignore.case = TRUE)

  if (length(var_lines) == 0) {
    return(data.frame(
      dataset      = character(),
      var_name     = character(),
      var_type     = character(),
      var_label    = character(),
      value_labels = character(),
      stringsAsFactors = FALSE
    ))
  }

  rows <- lapply(var_lines, function(line) {

    # --- variable label: text between `" and "' ----------------------------
    var_label <- sub(".*`\"(.+?)\"'.*", "\\1", line, perl = TRUE)

    # --- prefix before `": type + varname + optional :LABELSET -------------
    prefix <- trimws(sub("`\".*", "", line))
    parts  <- strsplit(prefix, "\\s+")[[1]]
    parts  <- parts[nchar(parts) > 0]

    if (length(parts) < 2) return(NULL)

    var_type <- parts[1]

    # Three spacing variants for :LABELSET
    if (grepl(":", parts[2], fixed = TRUE)) {
      # "varname:LABELSET"  — colon fused
      name_parts   <- strsplit(parts[2], ":", fixed = TRUE)[[1]]
      var_name     <- name_parts[1]
      value_labels <- name_parts[2]
    } else if (length(parts) >= 3 && startsWith(parts[3], ":")) {
      # "varname"  ":LABELSET"  — colon as separate token
      var_name     <- parts[2]
      value_labels <- sub("^:", "", parts[3])
    } else {
      # no label set
      var_name     <- parts[2]
      value_labels <- NA_character_
    }

    data.frame(
      dataset      = dataset_name,
      var_name     = var_name,
      var_type     = var_type,
      var_label    = var_label,
      value_labels = value_labels,
      stringsAsFactors = FALSE
    )
  })

  result <- do.call(rbind, Filter(Negate(is.null), rows))
  rownames(result) <- NULL
  result[, c("dataset", "var_name", "var_type", "var_label", "value_labels")]
}


# ---------------------------------------------------------------------------
# load_all_dcts() — variable metadata for a whole folder
# ---------------------------------------------------------------------------

load_all_dcts <- function(folder_path) {
  dct_files <- list.files(folder_path, pattern = "\\.dct$", full.names = TRUE)
  if (length(dct_files) == 0) {
    stop("No .dct files found in: ", folder_path)
  }
  message("Parsing ", length(dct_files), " file(s): ",
          paste(basename(dct_files), collapse = ", "))

  all_vars <- do.call(rbind, lapply(dct_files, parse_dct))
  rownames(all_vars) <- NULL
  all_vars
}


# ---------------------------------------------------------------------------
# parse_dct_labels() — value label definitions from a single file
# ---------------------------------------------------------------------------
# Parses lines of the form:
#   label define SETNAME value1 "text1" value2 "text2" ...

parse_dct_labels <- function(filepath) {

  empty <- data.frame(
    dataset    = character(),
    label_set  = character(),
    value      = integer(),
    label_text = character(),
    stringsAsFactors = FALSE
  )

  lines        <- readLines(filepath, warn = FALSE)
  dataset_name <- sub("\\.dct$", "", basename(filepath), ignore.case = TRUE)

  def_lines <- grep("^\\s*label\\s+define\\s+",
                    lines, ignore.case = TRUE, value = TRUE)
  if (length(def_lines) == 0) return(empty)

  rows <- lapply(def_lines, function(line) {
    set_name <- sub("^\\s*label\\s+define\\s+(\\w+).*$", "\\1",
                    line, ignore.case = TRUE, perl = TRUE)
    rest     <- sub("^\\s*label\\s+define\\s+\\w+\\s+", "",
                    line, ignore.case = TRUE, perl = TRUE)

    m <- gregexpr('(-?[0-9]+)\\s+"([^"]*)"', rest, perl = TRUE)
    if (m[[1L]][1L] == -1L) return(NULL)

    matches <- regmatches(rest, m)[[1L]]
    if (length(matches) == 0L) return(NULL)

    parsed <- lapply(matches, function(match) {
      val  <- as.integer(sub('^(-?[0-9]+)\\s+".*"$',  "\\1", match, perl = TRUE))
      text <- sub('^-?[0-9]+\\s+"([^"]*)"$', "\\1", match, perl = TRUE)
      data.frame(dataset = dataset_name, label_set = set_name,
                 value = val, label_text = text, stringsAsFactors = FALSE)
    })
    do.call(rbind, parsed)
  })

  result <- do.call(rbind, Filter(Negate(is.null), rows))
  if (is.null(result)) return(empty)
  rownames(result) <- NULL
  result
}


# ---------------------------------------------------------------------------
# load_all_dct_labels() — value label definitions for a whole folder
# ---------------------------------------------------------------------------

load_all_dct_labels <- function(folder_path) {
  empty <- data.frame(
    dataset = character(), label_set = character(),
    value = integer(), label_text = character(),
    stringsAsFactors = FALSE
  )
  dct_files <- list.files(folder_path, pattern = "\\.dct$", full.names = TRUE)
  if (length(dct_files) == 0) return(empty)

  result <- do.call(rbind, lapply(dct_files, function(f) {
    tryCatch(parse_dct_labels(f), error = function(e) empty)
  }))
  if (is.null(result)) return(empty)
  rownames(result) <- NULL
  result
}


# ---------------------------------------------------------------------------
# Quick test — run this block interactively or via Rscript
# ---------------------------------------------------------------------------
if (FALSE) {
  folder <- "."   # set to folder containing .dct files

  all_vars <- load_all_dcts(folder)

  cat("\n=== all_vars summary ===\n")
  cat("nrow        :", nrow(all_vars), "\n")
  cat("datasets    :", paste(unique(all_vars$dataset), collapse = ", "), "\n\n")

  cat("Variables per dataset:\n")
  print(table(all_vars$dataset))

  cat("\nValue label sets used:\n")
  print(sort(table(all_vars$value_labels), decreasing = TRUE))

  cat("\nFirst 10 rows:\n")
  print(head(all_vars, 10))
}

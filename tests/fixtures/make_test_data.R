# Synthetic fixtures for shinytest2 tests.
# Call create_test_fixtures(dir) to write test_all_vars.rds and test_hc.rds.
# Uses set.seed(42) for deterministic output.

make_test_all_vars <- function() {
  set.seed(42)
  datasets  <- c("DS1", "DS2", "DS3")
  base_vars <- paste0("var_", sprintf("%03d", 1:80))
  domains   <- c("Mental Health", "Demographics", "Substance Use")

  rows <- do.call(rbind, lapply(datasets, function(ds) {
    do.call(rbind, lapply(c("T1", "T2"), function(wave) {
      data.frame(
        dataset        = ds,
        var_name       = paste0(wave, "_", base_vars),
        base_var       = base_vars,
        var_label      = paste0("Label for ", base_vars, " in ", ds),
        var_type       = sample(c("numeric", "categorical"), 80, replace = TRUE),
        domain         = sample(domains, 80, replace = TRUE),
        subdomain      = paste0("Sub_", sample(1:5, 80, replace = TRUE)),
        has_wave       = TRUE,
        wave_num       = as.integer(substr(wave, 2, 2)),
        wave_label     = wave,
        wave_label_std = wave,
        value_labels   = ifelse(runif(80) > 0.5,
                                paste0("lset_", base_vars), NA_character_),
        label_quality  = "ok",
        stringsAsFactors = FALSE
      )
    }))
  }))
  rows
}

make_test_hc <- function() {
  set.seed(42)
  # 20 clusters each spanning exactly DS1 + one other dataset.
  # DS1 is always a member so DS1-filtered searches don't show DS2/DS3 cross-dataset pills
  # for variables not in a cluster — only the 20 cluster vars show pills.
  do.call(rbind, lapply(1:20, function(i) {
    other_ds <- sample(c("DS2", "DS3"), 1)
    data.frame(
      cluster_id           = i,
      dataset              = c("DS1", other_ds),
      var_name             = paste0("T1_var_", sprintf("%03d", i)),
      var_label            = paste0("Cluster label ", i),
      candidate_label      = paste0("Construct ", i),
      domain               = "Mental Health",
      subdomain            = "Psych Distress",
      harmonisation_status = sample(c("ready", "recodable"), 2, replace = TRUE),
      stringsAsFactors     = FALSE
    )
  }))
}

create_test_fixtures <- function(dir = file.path("tests", "fixtures")) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(make_test_all_vars(), file.path(dir, "test_all_vars.rds"))
  saveRDS(make_test_hc(),       file.path(dir, "test_hc.rds"))
  message("Test fixtures written to: ", normalizePath(dir))
}

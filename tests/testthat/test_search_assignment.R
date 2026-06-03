# Run with (from harmoniser/ directory):
#   library(shinytest2)
#   test_app(".")
#
# Uses synthetic fixture data — does not require real pipeline outputs.
# All tests should pass in < 30 seconds total.

library(shinytest2)
library(testthat)

# ── Fixture setup ─────────────────────────────────────────────────────────────
# testthat::test_dir() sets working dir to tests/testthat/
# app root is ../.. from here; fixtures are ../fixtures/
local({
  fixture_dir <- file.path("..", "fixtures")
  if (!file.exists(file.path(fixture_dir, "test_all_vars.rds"))) {
    source(file.path(fixture_dir, "make_test_data.R"))
    create_test_fixtures(fixture_dir)
  }
})

# ── Helper ────────────────────────────────────────────────────────────────────
new_app <- function(timeout = 12000) {
  # app_dir = "../.." resolves to harmoniser/ from tests/testthat/
  app <- AppDriver$new(app_dir = "../..", timeout = timeout, load_timeout = 15000,
                       options = list(shiny.testmode = TRUE))
  # Wait for startup observers (updateSelectInput for search_ds choices) to settle
  app$wait_for_idle(duration = 1200, timeout = 12000)
  app
}

# ── TEST 1: Search returns results within timeout ─────────────────────────────
test_that("search returns results for valid query", {
  app <- new_app()
  on.exit(app$stop(), add = TRUE)

  app$run_js("Shiny.setInputValue('nav_view','search',{priority:'event'}); switchView('search');")
  app$set_inputs(search_q = "var_001")
  # duration=400 forces >=400ms continuous idle, longer than the 300ms search debounce
  app$wait_for_idle(duration = 400, timeout = 8000)

  results_html <- app$get_html("#search_results_ui")
  expect_true(grepl("var_001", results_html),
    label = "search_results_ui should contain 'var_001'")
})

# ── TEST 2: Dataset filter narrows results ────────────────────────────────────
test_that("dataset filter restricts search results to correct dataset", {
  app <- new_app()
  on.exit(app$stop(), add = TRUE)

  app$run_js("Shiny.setInputValue('nav_view','search',{priority:'event'}); switchView('search');")
  app$set_inputs(search_ds = "DS1", search_q = "var_005")
  app$wait_for_idle(duration = 400, timeout = 8000)

  results_html <- app$get_html("#search_results_ui")

  # Card dataset badges — all results must be from DS1
  expect_true(grepl("DS1", results_html),
    label = "DS1 badge should appear in results")
  # No result card should have a DS2 badge as its primary dataset
  expect_false(grepl('bg-secondary ms-1 small">DS2', results_html),
    label = "No DS2 card badge should appear when filtered to DS1")
})

# ── TEST 3: Assign variable flow completes correctly ──────────────────────────
test_that("assign variable from context search updates assignment", {
  app <- new_app(timeout = 20000)
  on.exit(app$stop(), add = TRUE)

  # Add construct
  app$set_inputs(new_constructs_text = "test_construct")
  app$click("btn_add_text_constructs")
  app$wait_for_idle(1000)

  # Open detail panel
  app$run_js("Shiny.setInputValue('edit_construct','test_construct',{priority:'event'})")
  app$wait_for_idle(1000)

  # Trigger context search for DS1 (mimics clicking "Find variable" button)
  app$run_js("Shiny.setInputValue('asgn_search','test_construct|||DS1',{priority:'event'})")
  app$wait_for_idle(duration = 800, timeout = 8000)

  # Context banner should show targeted mode with construct name
  banner_html <- app$get_html("#search_context_banner_ui")
  expect_true(grepl("test_construct", banner_html),
    label = "search context banner should show targeted mode for test_construct")

  # Search and assign — explicitly set DS1 filter for reliability
  app$set_inputs(search_ds = "DS1", search_q = "var_001")
  app$wait_for_idle(duration = 400, timeout = 8000)
  app$run_js("Shiny.setInputValue('assign_var','DS1|||T1_var_001',{priority:'event'})")
  app$wait_for_idle(duration = 400, timeout = 8000)

  assignments <- app$get_value(export = "assignments")
  expect_true(
    nrow(assignments) > 0 &&
    any(assignments$harmonised_name == "test_construct" & assignments$dataset == "DS1"),
    label = "assignment for test_construct / DS1 should be recorded"
  )
})

# ── TEST 4: Queue and assign flow works ───────────────────────────────────────
test_that("queue add and assign creates correct assignment", {
  app <- new_app(timeout = 20000)
  on.exit(app$stop(), add = TRUE)

  # Add construct
  app$set_inputs(new_constructs_text = "queue_test")
  app$click("btn_add_text_constructs")
  app$wait_for_idle(1000)

  # Go to search, queue a variable
  app$run_js("Shiny.setInputValue('nav_view','search',{priority:'event'}); switchView('search');")
  app$set_inputs(search_q = "var_005")
  app$wait_for_idle(duration = 400, timeout = 8000)
  app$run_js("Shiny.setInputValue('queue_var','DS1|||T1_var_005',{priority:'event'})")
  app$wait_for_idle(duration = 800, timeout = 8000)  # longer: wait for queue bar + sq_construct_choices_ui to render

  # Assign queue to construct — use run_js click to bypass .shiny-bound-input timing
  app$set_inputs(pq_target_construct_sb = "queue_test")
  app$run_js("var btn = document.getElementById('btn_assign_queue_sb'); if (btn) btn.click();")
  app$wait_for_idle(duration = 400, timeout = 8000)

  assignments <- app$get_value(export = "assignments")
  expect_true(
    nrow(assignments) > 0 && any(assignments$harmonised_name == "queue_test"),
    label = "assignment for queue_test should be recorded after queue assign"
  )
})

# ── TEST 5: Search performance benchmark ──────────────────────────────────────
test_that("search completes within 3 seconds for synthetic 480-variable dataset", {
  app <- new_app()
  on.exit(app$stop(), add = TRUE)

  app$run_js("Shiny.setInputValue('nav_view','search',{priority:'event'}); switchView('search');")
  app$wait_for_idle(duration = 400, timeout = 5000)

  t_start <- Sys.time()
  app$set_inputs(search_q = "var")
  app$wait_for_idle(duration = 400, timeout = 8000)
  elapsed <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))

  message(sprintf("Search elapsed: %.2fs", elapsed))
  expect_lt(elapsed, 3.0,
    label = sprintf("Search took %.2fs — should be < 3s", elapsed))

  # Also confirm results rendered
  results_html <- app$get_html("#search_results_ui")
  expect_true(nchar(results_html) > 100,
    label = "search_results_ui should have rendered content")
})

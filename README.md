# Data Harmonisation Assistant

A Shiny app for harmonising variables across multiple longitudinal survey datasets described by Stata `.dct` dictionary files.

---

## Quick start

### 1. Install required packages

Run once from R:

```r
source("setup.R")
```

Or install manually:

```r
install.packages(c("shiny", "bslib", "DT", "dplyr", "haven", "readr"))
```

### 2. Add your dictionary files

Place one Stata infile dictionary (`.dct`) file per dataset into the `dct/` folder.
The filename (without `.dct`) becomes the dataset label shown in the app.

```
harmoniser/
  dct/
    wave1.dct
    wave2.dct
    wave3.dct
```

### 3. Launch the app

From R or RStudio, with the `harmoniser/` folder as working directory:

```r
shiny::runApp()
```

Or from the parent directory:

```r
shiny::runApp("harmoniser/")
```

---

## Workflow

| Tab | What to do |
|-----|------------|
| **Browse Variables** | Search and filter variables across all datasets. Select rows, then click *Add to Harmonisation Plan*. Rows already in the plan show ✓. |
| **Build Harmonisation Plan** | Edit the *Harmonised Name* column to map equivalent variables across datasets. Variables sharing a harmonised name will be stacked by `bind_rows()`. Green groups span ≥2 datasets; amber groups are single-dataset. Hover a variable name to see its value label set. |
| **Generate R Code** | Review the generated script, copy it to the clipboard, or download it. Set `data_folder` at the top of the script to point at your actual data files before running. |

---

## .dct file format

The app parses Stata infile dictionary files:

```
dictionary {
  int   id            `"Respondent ID"'
  byte  age           `"Age in years"'
  byte  sex:SEXLABEL  `"Sex of respondent"'
  str8  suburb        `"Suburb of residence"'
  _newline
}
```

Value label definitions are also parsed if present:

```
label define SEXLABEL 1 "Male" 2 "Female"
```

When found, they appear as hover tooltips on variable names in the Build tab and as commented documentation in the generated R script.

---

## Generated script structure

The downloaded `.R` file contains:

1. **Setup** — `library()` calls, `data_folder` path, `load_dataset()` helper
2. **Per-dataset blocks** — `select()` → `rename()` → `mutate(.dataset)` → `select()`
3. **Combine** — `bind_rows()` + summary `cat()`
4. **Value labels** — commented reference (if found in `.dct` files)
5. **Save** — commented `saveRDS` / `write_dta` / `write.csv` options

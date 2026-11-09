download_dir <- here::here("Downloads")

codelists <- list(
  "type_2_diabetes" = dplyr::tribble(
    ~concept_id, ~concept_name, ~vocabulary_id,
    201826L, "Type 2 diabetes mellitus", "SNOMED",
    1234567890L, "FAKE Type 2 diabetes", "INVENTED",
    4063043L, "Pre-existing type 2 diabetes mellitus", "SNOMED",
    43531597L, "Disorder due to well controlled type 2 diabetes mellitus", "SNOMED",
    4130162L, "Insulin treated type 2 diabetes mellitus", "SNOMED",
    4129519L, "Pregnancy and type 2 diabetes mellitus", "SNOMED",
    45770928L, "Retinal edema due to type 2 diabetes mellitus", "SNOMED",
    44803781L, "Dementia monitoring third letter", "SNOMED",
    4200875L, "Type 2 diabetes mellitus with peripheral angiopathy", "SNOMED",
    4196141L, "Arthropathy due to type 2 diabetes mellitus", "SNOMED"
  ),
  "ischaemic_stroke" = dplyr::tribble(
    ~concept_id, ~concept_name, ~vocabulary_id,
    4337830L, "Claude's syndrome", "SNOMED",
    372924L, "Cerebral artery occlusion", "SNOMED",
    381316L, "Cerebrovascular accident", "SNOMED",
    1234567890L, "FAKE ischaemic_stroke", "INVENTED",
    4045734L, "CVA - cerebrovascular accident due to cerebral artery occlusion", "SNOMED",
    4043731L, "Infarction - precerebral", "SNOMED",
    201826L, "Type 2 diabetes mellitus", "SNOMED",
    4045735L, "Anterior cerebral circulation infarction", "SNOMED",
    4046358L, "Total anterior cerebral circulation infarction", "SNOMED",
    4046359L, "Partial anterior cerebral circulation infarction", "SNOMED",
  ),
  "asthma" = dplyr::tribble(
    ~concept_id, ~concept_name, ~vocabulary_id,
    37020832L, "My child coughed because of his or her asthma in past 7 days [PROMIS.PARENTPROXY]", "LOINC",
    40764830L, "I felt scared that I might have trouble breathing because of my asthma in past 7 days [PROMIS]", "LOINC",
    46272979L, "Recent asthma management", "SNOMED",
    44803915L, "Dementia monitoring second letter", "SNOMED",
    37395677L, "Telehealth asthma monitoring", "SNOMED",
    42535849L, "Use of asthma symptom diary", "SNOMED",
    44805526L, "Asthma review using Royal College of Physicians three questions", "SNOMED",
    35621931L, "Asthma monitoring declined", "SNOMED",
    44805087L, "Asthma causes night time symptoms 1 to 2 times per week", "SNOMED",
    1234567890L, "FAKE asthma", "INVENTED",
  ),
  "dementia" = dplyr::tribble(
    ~concept_id, ~concept_name, ~vocabulary_id,
    44804339L, "Dementia monitoring invitation", "SNOMED",
    37166341L, "Primary infiltrating ductal carcinoma of central portion of left female breast", "SNOMED",
    44803643L, "Dementia monitoring first letter", "SNOMED",
    44803706L, "Dementia monitoring verbal invitation", "SNOMED",
    44803781L, "Dementia monitoring third letter", "SNOMED",
    44803843L, "Dementia monitoring telephone invitation", "SNOMED",
    44803915L, "Dementia monitoring second letter", "SNOMED",
    36714514L, "Shared care prescribing of drug for dementia", "SNOMED",
    36714691L, "Shared care prescribing of drugs for dementia declined", "SNOMED",
    1234567890L, "FAKE dementia", "INVENTED"
  ),
  "breast_cancer" = dplyr::tribble(
    ~concept_id, ~concept_name, ~vocabulary_id,
    37166322L, "Primary infiltrating duct carcinoma of female breast", "SNOMED",
    44803643L, "Dementia monitoring first letter", "SNOMED",
    37166341L, "Primary infiltrating ductal carcinoma of central portion of left female breast", "SNOMED",
    37166342L, "Primary infiltrating ductal carcinoma of central portion of right female breast", "SNOMED",
    1234567890L, "FAKE cancer", "INVENTED",
    37166344L, "Primary infiltrating ductal carcinoma of upper outer quadrant of left female breast", "SNOMED",
    37166346L, "Primary infiltrating ductal carcinoma of upper outer quadrant of right female breast", "SNOMED",
    37166348L, "Primary infiltrating ductal carcinoma of upper inner quadrant of right female breast", "SNOMED",
    37166349L, "Primary infiltrating ductal carcinoma of upper inner quadrant of left female breast", "SNOMED",
    37166350L, "Primary infiltrating lobular carcinoma of left female breast", "SNOMED"
  )
)

# Synthetic prevalence results for the Quarto exercise. The conditions match
# the codelist names above; prevalence values are illustrative percentages.
condition_profiles <- dplyr::tribble(
  ~condition,          ~baseline, ~annual_change,
  "type_2_diabetes",       5.0,           0.22,
  "ischaemic_stroke",      1.4,           0.04,
  "asthma",                8.4,          -0.12,
  "dementia",              3.1,           0.08,
  "breast_cancer",         4.6,           0.16
)

stopifnot(setequal(condition_profiles$condition, names(codelists)))

make_results <- function(database, baseline_shift = 0, trend_shift = 0) {
  tidyr::crossing(
    condition = condition_profiles$condition,
    year = 2019:2023
  ) |>
    dplyr::left_join(condition_profiles, by = "condition") |>
    dplyr::mutate(
      database = database,
      prevalence = round(
        baseline + baseline_shift + (year - 2019) * (annual_change + trend_shift),
        1
      )
    ) |>
    dplyr::select(database, condition, year, prevalence) |>
    dplyr::arrange(condition, year)
}

data1 <- dplyr::bind_rows(
  make_results("SIDI Plata"),
  make_results("SIDI Bronze", baseline_shift = 1.1, trend_shift = -0.03)
) |>
  dplyr::arrange(database, condition, year)

# Data 2 keeps SIDI Bronze unchanged, updates SIDI Plata, and adds SIDI Or.
data2 <- dplyr::bind_rows(
  dplyr::filter(data1, database == "SIDI Bronze"),
  make_results("SIDI Plata", baseline_shift = 0.7, trend_shift = 0.08),
  make_results("SIDI Or", baseline_shift = 1.4, trend_shift = 0.02)
) |>
  dplyr::arrange(database, condition, year)

stopifnot(
  setequal(unique(data1$database), c("SIDI Plata", "SIDI Bronze")),
  setequal(unique(data2$database), c("SIDI Plata", "SIDI Bronze", "SIDI Or")),
  isTRUE(all.equal(
    as.data.frame(dplyr::filter(data1, database == "SIDI Bronze")),
    as.data.frame(dplyr::filter(data2, database == "SIDI Bronze"))
  )),
  !isTRUE(all.equal(
    as.data.frame(dplyr::filter(data1, database == "SIDI Plata")),
    as.data.frame(dplyr::filter(data2, database == "SIDI Plata"))
  ))
)

users <- c(
  "aballvemartin-art",
  "arani11",
  "Mandickel",
  "mingyangligithub",
  "yannaw-prog",
  "wanningwang",
  "lluiszacariaspons",
  "matthewlewis123",
  "jd-ndorms",
  "jieying103048",
  "genrmonaghan",
  "foluwa",
  "amysmlam",
  "deflin29",
  "khaynes001"
)

# Assign a codelist and reviewer to each user.
codelist <- rep(names(codelists), length.out = length(users))
reviewer <- c(users[length(users)], users[seq_len(length(users) - 1)])

# Generate each user's codelist and reviewer information.
for (k in seq_along(users)) {
  user_dir <- file.path(download_dir, users[k])
  dir.create(user_dir, recursive = TRUE, showWarnings = FALSE)
  # Remove result filenames from the earlier generator so they are not shown
  # as extra codelist downloads.
  unlink(file.path(user_dir, c("results.csv", "results_new.csv")))

  codelist_name <- codelist[k]
  readr::write_csv(
    codelists[[codelist_name]],
    file = file.path(user_dir, paste0(codelist_name, ".csv"))
  )
  readr::write_csv(
    data1,
    file = file.path(user_dir, "data1.csv")
  )
  readr::write_csv(
    data2,
    file = file.path(user_dir, "data2.csv")
  )
  file.copy(
    from = here::here("Presentations", "OmopViewer", "data", "omopviewer_results.csv"),
    to = file.path(user_dir, "omopviewer_results.csv"),
    overwrite = TRUE
  )
  writeLines(
    paste0("reviewer: ", reviewer[k]),
    file.path(user_dir, "info.txt")
  )
}

escape_html <- function(text) {
  text <- gsub("&", "&amp;", text, fixed = TRUE)
  text <- gsub("<", "&lt;", text, fixed = TRUE)
  text <- gsub(">", "&gt;", text, fixed = TRUE)
  text <- gsub('"', "&quot;", text, fixed = TRUE)
  text <- gsub("'", "&#39;", text, fixed = TRUE)
  text
}

# Build the tabset from every folder currently available in Downloads.
folders <- list.dirs(download_dir, recursive = FALSE, full.names = TRUE)
folders <- folders[order(tolower(basename(folders)), basename(folders))]

page <- c(
  "---",
  'title: "Downloads"',
  "format: html",
  "---",
  "",
  "<!-- Generated by Downloads/generateFiles.R. Do not edit manually. -->",
  "",
  "Choose your GitHub username to see the reviewer information and download your codelist or sample results.",
  "",
  "::: {.panel-tabset}",
  "",
  "## &nbsp;",
  ""
)

for (folder in folders) {
  folder_name <- basename(folder)
  page <- c(page, paste0("## @", folder_name), "")

  info_file <- file.path(folder, "info.txt")
  if (file.exists(info_file)) {
    info <- readLines(info_file, warn = FALSE)
    page <- c(page, "<pre>", escape_html(info), "</pre>", "")
  }

  csv_files <- sort(list.files(folder, pattern = "\\.csv$", full.names = FALSE))
  if (length(csv_files) == 0) {
    page <- c(page, "No codelist CSV found in this folder yet.", "")
  } else {
    for (csv_file in csv_files) {
      link <- paste(folder_name, csv_file, sep = "/")
      button_label <- if (csv_file == "data1.csv") {
        "Download Data 1 results"
      } else if (csv_file == "data2.csv") {
        "Download Data 2 results"
      } else if (csv_file == "omopviewer_results.csv") {
        "Download OmopViewer results"
      } else {
        "Download codelist"
      }
      page <- c(
        page,
        paste0(
          '[', button_label, '](', link,
          '){.btn .btn-primary download="', csv_file, '"}'
        ),
        ""
      )
    }
  }
}

if (length(folders) == 0) {
  page <- c(page, "No download folders found yet.", "")
}

page <- c(page, ":::")
writeLines(page, file.path(download_dir, "index.qmd"), useBytes = TRUE)

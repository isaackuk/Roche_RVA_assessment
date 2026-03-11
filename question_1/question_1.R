#install.packages(c("pharmaverseadam", "tidyverse", "gtsummary", "ggplot2", "shiny"))
library(pharmaverseadam)
library(tidyverse)
library(dplyr)
library(gtsummary)
library(gt)

# 1. Load Treatment-Emergent AEs
adae_teae <- pharmaverseadam::adae %>%
  filter(TRTEMFL == "Y")

# 2. Prepare denominator (total subjects (N) per treatment arm)
adsl_trt <- adsl %>%
  select(USUBJID, ACTARM)

arm_n <- adsl_trt %>%
  count(ACTARM, name = "N")%>%
  arrange(ACTARM)

arm_levels <- arm_n$ACTARM          # preserve arm order
arm_N      <- setNames(arm_n$N, arm_n$ACTARM)

##########################################################################################
###### 3. HELPER FUNCTIONS ######
# For formatting values "n (x.x%)"
fmt_cell <- function(n, N) {
  pct <- if (N > 0) n / N * 100 else 0
  sprintf("%d (%.1f%%)", n, pct)
}

# For counting subjects + filling null values
count_subjects <- function(data, group_vars) {
  data %>%
    distinct(USUBJID, !!!syms(group_vars)) %>%
    count(!!!syms(group_vars), name = "n")
}

# For formatting into table
pivot_and_format <- function(counts_df, row_var) {
  counts_df %>%
    left_join(arm_n, by = "ACTARM") %>%
    rowwise() %>%
    mutate(cell = fmt_cell(n, N)) %>%
    ungroup() %>%
    select(all_of(row_var), ACTARM, cell) %>%
    pivot_wider(names_from = ACTARM, values_from = cell
    )
}
##########################################################################################

# 4. All TEAEs summary
teae_counts <- adae_teae %>%
  count_subjects(c("ACTARM")) %>%
  complete(ACTARM = arm_levels, fill = list(n = 0))

teae_wide <- pivot_and_format(teae_counts, "ACTARM") %>%
  mutate(term = "Treatment Emergent Adverse Events",
         row_type = "total",
         AESOC = NA_character_) %>%
  select(row_type, AESOC, term, all_of(arm_levels))

# 5. SOC-level counts
soc_counts <- adae_teae %>%
  count_subjects(c("AESOC", "ACTARM")) %>%
  complete(AESOC, ACTARM = arm_levels, fill = list(n = 0))

soc_wide <- pivot_and_format(soc_counts, "AESOC") %>%
  mutate(term = AESOC,
         row_type = "soc") %>%
  select(row_type, AESOC, term, all_of(arm_levels))

# 6. Preferred Term summary
pt_counts <- adae_teae %>%
  count_subjects(c("AESOC", "AEDECOD", "ACTARM")) %>%
  complete(nesting(AESOC, AEDECOD), ACTARM = arm_levels, fill = list(n = 0))

pt_wide <- pivot_and_format(pt_counts, c("AESOC", "AEDECOD")) %>%
  mutate(term = AEDECOD,
         row_type = "pt") %>%
  select(row_type, AESOC, term, all_of(arm_levels))

# 7. Combine summaries, listing PTs under respective SOCs alphabetically
soc_order <- sort(unique(soc_wide$AESOC))

body_rows <- map_dfr(soc_order, function(soc) {
  bind_rows(
    filter(soc_wide, AESOC == soc),
    filter(pt_wide,  AESOC == soc) %>%
      arrange(term)
  )
})

table_data <- bind_rows(teae_wide, body_rows) %>%
  select(-AESOC)       # AESOC no longer needed as a column

# 8. Column headers (trtarm + N)
col_labels <- setNames(
  lapply(arm_levels, function(a) html(paste0("<b>", a, "</b><br>N = ", arm_N[a]))),
  arm_levels
)

# 9. Create formatted table
gt_tbl <- table_data %>%
  gt(rowname_col = "term") %>% #names of rows
  cols_hide(columns = c(row_type, `Screen Failure`)) %>%
  
  # ── Column labels
  cols_label(.list = col_labels) %>%
  tab_stubhead(label = md("**System Organ Class / Preferred Term**")) %>%
  
  # ── Align count columns
  cols_align(align = "center", columns = all_of(arm_levels)) %>%
  cols_align(align = "left",   columns = term) %>%
  
  # ── Style: PT rows — indented via padding
  tab_style(
    style = cell_text(indent = px(24)),
    locations = cells_stub(rows = table_data$row_type == "pt")
  ) %>%
  
  # ── Footnotes
  tab_footnote(
    footnote = "N = number of subjects in the Safety Population (ADSL; SAFFL = 'Y') for the given treatment arm.",
    locations = cells_column_labels(columns = all_of(arm_levels))
  ) %>%
  tab_source_note(md("TEAEs defined as adverse events with **TRTEMFL = 'Y'** in ADAE.")) |>
  tab_source_note("n = number of unique subjects with ≥ 1 event; % = n / N × 100, rounded to one decimal place.") |>
  tab_source_note("Subjects are counted once per SOC and once per Preferred Term regardless of number of occurrences.") |>
  tab_source_note(md("Source data: `pharmaverseadam::adae`, `pharmaverseadam::adsl`")) |>
  tab_source_note(paste0("Generated: ", Sys.Date())) |>
  
  # ── General options
  tab_options(
    table.font.names          = "Arial",
    table.font.size           = px(11),
    table.width               = pct(100),
    table.border.top.style    = "hidden",
    table.border.bottom.style = "hidden",
    stub.border.style         = "hidden",
    column_labels.font.weight = "bold",
    heading.align             = "center",
    heading.title.font.size   = px(13),
    heading.subtitle.font.size = px(11),
    data_row.padding          = px(3),
    source_notes.font.size    = px(9),
    footnotes.font.size       = px(9)
  )

# 10. Save HTML output
output_path <- "solution_1.html"
gtsave(gt_tbl, filename = output_path)

library(pharmaverseadam)
library(tidyverse)
library(ggplot2)

# ── 1. Load & prepare data ──────────────────────────────────
adae <- pharmaverseadam::adae

# Count unique subjects per SOC × Severity
# Each subject is counted at most once per severity level within each SOC
ae_counts <- adae %>%
  filter(!is.na(AESOC), !is.na(AESEV)) %>%
  distinct(USUBJID, AESOC, AESEV) %>%          # deduplicate subject×SOC×severity
  count(AESOC, AESEV, name = "n_subjects")

# Compute total subjects per SOC for ordering
soc_order <- ae_counts %>%
  group_by(AESOC) %>%
  summarise(total = sum(n_subjects), .groups = "drop") %>%
  arrange(total) %>%
  pull(AESOC)

# Apply factor levels (ascending total → bottom to top on y-axis)
ae_counts <- ae_counts %>%
  mutate(
    AESOC = factor(AESOC, levels = soc_order),
    AESEV = factor(AESEV, levels = c("MILD", "MODERATE", "SEVERE"))
  )

# ── 2. Colour palette (salmon / coral family, light → dark) ──
severity_colours <- c(
  "MILD"     = "#FDCAB5",   # light salmon
  "MODERATE" = "#F4845F",   # coral
  "SEVERE"   = "#C0392B"    # deep red
)

# ── 3. Build plot ────────────────────────────────────────────
p <- ggplot(ae_counts, aes(x = n_subjects, y = AESOC, fill = AESEV)) +
  geom_col(
    position = position_stack(reverse = TRUE),   # MILD first (left), SEVERE last
    width     = 0.7
  ) +
  
  # Severity legend colours
  scale_fill_manual(
    values = severity_colours,
    name   = "Severity",
    breaks = c("MILD", "MODERATE", "SEVERE")
  ) +
  
  # Axes
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.04)),
    breaks = seq(0, 150, by = 25),
    limits = c(0, 150)
  ) +
  scale_y_discrete(
    breaks = levels(ae_counts$AESOC)
  ) +
  
  # Labels & theme
  labs(
    title   = "Unique Subjects per SOC and Severity Level",
    x       = "Number of Unique Subjects",
    y       = "System Organ Class"
  ) +
  
  theme_minimal(base_size = 10) +
  theme(
    # Plot area
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.grid.major.x = element_line(colour = "grey88", linewidth = 0.4),
    panel.grid.major.y = element_line(colour = "grey94", linewidth = 0.4),
    panel.grid.minor   = element_blank(),
    
    # Title
    plot.title = element_text(
      face   = "bold",
      size   = 12,
      hjust  = 0.5,
      margin = margin(b = 10)
    ),
    
    # Axis text
    axis.text.y  = element_text(size = 7.5, colour = "grey20"),
    axis.text.x  = element_text(size = 8,   colour = "grey20"),
    axis.title.x = element_text(size = 9,   margin = margin(t = 6)),
    axis.title.y = element_text(size = 9,   margin = margin(r = 6)),
    
    # Legend
    legend.position  = "right",
    legend.title     = element_text(size = 8, face = "bold"),
    legend.text      = element_text(size = 7.5),
    legend.key.size  = unit(0.5, "cm"),
    
    # Margins
    plot.margin = margin(12, 16, 12, 12)
  )

# ── 4. Save to PNG ───────────────────────────────────────────
ggsave(
  filename = "solution_2.png",
  plot     = p,
  width    = 11,
  height   = 8,
  dpi      = 180,
  bg       = "white"
)

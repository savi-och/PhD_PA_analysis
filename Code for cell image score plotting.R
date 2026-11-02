# =====================================================================
# Plotting score=2 fractions alongside flow rates for both platforms
# =====================================================================
#install.packages("scales")
library(tidyverse)
library(readxl)   # swap for readr::read_csv if your source is a CSV
library(ggplot2)
library(ggrepel)
library(ggbeeswarm)
library(scales)
library(vipor)     # ships with ggbeeswarm; provides offsetX()

# ---------------------------------------------------------------------
# ---- 0. OPEN DECISIONS — set these two, everything else follows ----
# ---------------------------------------------------------------------
setwd ("C:\\Users\\scharalambous\\Box\\One Drive To Box Migration\\PhD folder\\Thesis writing\\2026\\Chapter 3_ResultsChapter2_PA")

DATA_PATH <- "C:\\Users\\scharalambous\\Box\\One Drive To Box Migration\\PhD folder\\Thesis writing\\2026\\Chapter 3_ResultsChapter2_PA\\Image_cells_flowrates.xlsx"  # <-- edit
SHEET     <- 1
# ============================================================
# Flow rate and outcome
# ============================================================

# ---- read + rename by position -----------------------------
# READ ME: if columns are added, keep them AFTER the renamed block
# or update the indices.
raw <- read_excel(DATA_PATH, sheet = SHEET)
names(raw)[1:8] <- c("slice_id", "flow_rate","flow_rate_seal_loss","perfusion_mode", "sealing","seal_loss", "outcome",  "comments")

OUTCOME_colours <- c("Little signal" = "#ffc40c", "Microtears" = "#8b008b", "Colocalisation" = "#4EA72E", "Seal lost - Leak"="#56B4E9")

####################################################################
#
# ---- Plot of individual slice fractions alongside flowrates -----
#
####################################################################

FLOOR_FLOW <- 2        # axis minimum, where NQ/zero gets pinned
CEIL_FLOW  <- 1e5       # 100000, clean decade above your 10400 max

# map a flow rate (log scale) into the 0..1 fraction band
to_frac <- function(flow) {
  (log10(flow) - log10(FLOOR_FLOW)) / (log10(CEIL_FLOW) - log10(FLOOR_FLOW))
}
# inverse, for the secondary axis labels
from_frac <- function(y) {
  10 ^ (y * (log10(CEIL_FLOW) - log10(FLOOR_FLOW)) + log10(FLOOR_FLOW))
}


# ---- data prep --------------------------------------------------
flow_long <- raw %>%
  mutate(
    flow_main = if_else(flow_rate == 0 | is.na(flow_rate), FLOOR_FLOW, flow_rate),
    is_nq     = flow_rate == 0 | is.na(flow_rate),
    flow_loss = if_else(flow_rate_seal_loss == 0 | is.na(flow_rate_seal_loss),
                        NA_real_, flow_rate_seal_loss),
    outcome   = factor(outcome,
                       levels = c("Little signal", "Microtears", "Seal lost - Leak", "Colocalisation")),
    slice_id  = fct_reorder(slice_id, flow_main)
  )


# ---- plot -------------------------------------------------------
flow_breaks <- c(10, 100, 1000, 10000, 100000)

p2 <- ggplot(flow_long, aes(x = slice_id)) +
  geom_col(aes(y = flow_main, fill = outcome), width = 0.6) +          
  geom_segment(aes(xend = slice_id, y = flow_main, yend = flow_loss),     # connector to seal-loss flow (NA where none)
               linewidth = 0.3, colour = "grey70", na.rm = TRUE) +
  geom_point(data = ~ filter(.x, !is.na(flow_loss)),
             aes(y = flow_loss), colour = "#56B4E9", size = 2.5)+
  ggrepel::geom_text_repel(
    data = ~ filter(.x, is_nq),
    aes(y = flow_main, label = "NQ"),
    size = 4, colour = "grey20", fontface = "bold",
    nudge_x = -0.5, direction = "y",
    segment.colour = "grey50", segment.size = 0.3,
    min.segment.length = 0
  ) +
  scale_fill_manual(values = OUTCOME_colours, drop = FALSE, name = "Outcome") +
  scale_y_log10(
    name   = "Flow rates (nL/min)",
    breaks = flow_breaks,
    labels = scales::label_log(),
    limits = c(FLOOR_FLOW, CEIL_FLOW),
    oob    = scales::oob_keep 
  ) +
  annotation_logticks(sides = "l", colour = "grey70") +
 # coord_cartesian(clip = "off")+
  labs(
    title = paste0("Perfused slice intravascular splenocyte colocalisation"),
    x = "Slice ID (ascending Q)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x        = element_text(angle = 45, hjust = 1, vjust = 1, size = 9),
    axis.ticks.length.x = unit(2, "pt"),
    axis.title.y.left  = element_text(colour = "grey10", size = 13, face = "bold"),
    axis.text.y.left   = element_text(colour = "grey10", size = 11),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.3),
    plot.margin        = margin(t = 10, r = 5, b = 10, l = 20)
  )
p2

# --- Per slice score=2 fraction (imaging) --------
ggsave(paste0("per_slice_cells_coloc_Q_", ".jpeg"),
       plot = p2, device = "jpeg",
       width = 8, height = 6, dpi = 300)

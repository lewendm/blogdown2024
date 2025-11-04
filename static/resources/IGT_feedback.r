# ------------------------------------------------------------------------------
# Feedback report generator (Iowa Gambling Task)
# Author: Lewend Mayiwar
# Year: 2025
#
# Use, tweak, and share this as you like.
# A quick credit is appreciated :)
#
# CC BY 4.0 – https://creativecommons.org/licenses/by/4.0/
# ------------------------------------------------------------------------------

# NOTE ABOUT DATA FROM PSYTOOLKIT
# -------------------------------
# PsyToolkit saves IGT data as ONE TEXT FILE PER PARTICIPANT.
# Each of those .txt files contains all 100 trials for that person
# (choice/deck, outcome, RT, etc.).
#
# This script expects one combined/clean dataset, not separate per-person files.
# So before running this script you should:
#   1. download all participant .txt files from PsyToolkit,
#   2. combine/stack them into a single dataset (one row per trial),
#   3. add an ID variable (e.g. subject_id) so we know which rows belong together,
#   4. make sure columns like deck, amountWon, feeToPay, and RT are present,
#   5. save the cleaned/combined data.
#
# You can do step 2–4 in R or in Python, or in Excel if the sample is small (which is what I did).
# This script starts AFTER that step.

# ================================
# IGT STUDENT FEEDBACK REPORTS 
# ================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(glue)
  library(grid)
  library(gridExtra)
  library(stringr)
})

# ---------- helpers ----------
# ---- APA 7 defaults ----
theme_set(theme_minimal(base_size = 12, base_family = "Times"))

# 1" margins on US Letter landscape:
# usable width = 9" of 11" => 9/11 = 0.818; usable height = 6.5" of 8.5" => 0.765
apa_content_vp <- function() {
  viewport(width = unit(0.818, "npc"),
           height = unit(0.765, "npc"),
           x = unit(0.5, "npc"), y = unit(0.5, "npc"))
}

# page number top-right (APA student papers: number only)
apa_page_number <- function(i, n) {
  grid.text(label = as.character(i),
            x = unit(0.975, "npc"), y = unit(0.975, "npc"),
            just = c("right","top"),
            gp = gpar(fontfamily = "Times", fontsize = 12, col = "grey25"))
}


`%||%` <- function(a, b) if (length(a) == 0 || all(is.na(a))) b else a

add_page_number <- function(i, n) {
  grid.text(paste0("Page ", i, " of ", n),
            x = unit(0.5, "npc"), y = unit(0.02, "npc"),
            gp = gpar(col = "grey30", cex = 0.8))
}

# Wrap a paragraph (preserves blank lines) and returns a grob
wrapped_paragraph <- function(text, width = 104, size = 12, face = "plain", lh = 1.18) {
  parts <- unlist(strsplit(text, "\n\n", fixed = TRUE))
  wrapped <- vapply(parts, function(p) stringr::str_wrap(p, width = width),
                    FUN.VALUE = character(1))
  textGrob(paste(wrapped, collapse = "\n\n"),
           x = 0, y = 1, just = c("left", "top"),
           gp = gpar(fontsize = size, fontface = face, lineheight = lh))
}

# ---------- Load & prep data ----------

# PREPROCESSING OUTLINE (what we assume has been done):
# - all PsyToolkit .txt files have been merged into one file
# - each row is one trial (so 100 rows per participant for the classic IGT)
# - there is a column called "subject_id" (or similar) that identifies the person
# - columns for deck/choice, RT, amountWon, feeToPay are present
# - the merged file has been saved as an Excel file below

igt_raw <- read_excel("IGT_combined_final_studentdata.xlsx")

igt <- igt_raw %>%
  group_by(subject_id) %>%
  mutate(trial = dplyr::row_number()) %>%
  ungroup() %>%
  mutate(
    deck_chr = toupper(as.character(deck)),
    deck_std = dplyr::case_when(
      deck_chr %in% c("A","B","C","D") ~ deck_chr,
      deck_chr %in% c("1","2","3","4") ~ c("A","B","C","D")[as.integer(deck_chr)],
      deck_chr %in% c("0","1","2","3") ~ c("A","B","C","D")[as.integer(deck_chr) + 1L],
      TRUE ~ NA_character_
    ),
    deck_std = factor(deck_std, levels = c("A","B","C","D")),
    block = ceiling(trial/20),
    amountWon = suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(amountWon)))),
    feeToPay  = suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(feeToPay))))
  ) %>%
  filter(block %in% 1:5)

BLOCK_LABELS <- c("Trials 1–20","Trials 21–40","Trials 41–60","Trials 61–80","Trials 81–100")
PHASE_BANDS <- data.frame(
  xmin = c(0.5, 1.5, 3.5),
  xmax = c(1.5, 3.5, 5.5),
  fill = c("#ECECEC", "#FFF5CC", "#E6F5E6"),
  label = c("Exploration\n(no clear preference)",
            "Gut sense\n(emerging preference)",
            "Strategy\n(deliberate choice)")
)

# ---------- Class averages (once) ----------
class_choices <- igt %>%
  count(subject_id, block, deck_std, name = "selections") %>%
  complete(subject_id, block = 1:5,
           deck_std = factor(c("A","B","C","D"), levels = c("A","B","C","D")),
           fill = list(selections = 0)) %>%
  group_by(block, deck_std) %>%
  summarise(mean_sel = mean(selections), .groups = "drop")

class_rt <- igt %>%
  group_by(subject_id, block) %>%
  summarise(med_rt = median(rt, na.rm = TRUE), .groups = "drop") %>%
  complete(subject_id, block = 1:5) %>%
  group_by(block) %>%
  summarise(mean_med_rt = mean(med_rt, na.rm = TRUE), .groups = "drop")

class_money <- igt %>%
  group_by(subject_id, block) %>%
  summarise(net = sum(amountWon - feeToPay, na.rm = TRUE), .groups = "drop") %>%
  complete(subject_id, block = 1:5, fill = list(net = 0)) %>%
  arrange(subject_id, block) %>%
  group_by(subject_id) %>% mutate(cum_net = cumsum(net)) %>% ungroup() %>%
  group_by(block) %>% summarise(mean_cum = mean(cum_net, na.rm = TRUE), .groups = "drop")

# ---------- Figure 1 ----------
make_choices_plot <- function(sid) {
  dat <- igt %>% filter(subject_id == sid)
  long <- dat %>%
    count(block, deck_std, name = "selections") %>%
    complete(block = 1:5,
             deck_std = factor(c("A","B","C","D"), levels = c("A","B","C","D")),
             fill = list(selections = 0)) %>%
    arrange(block, deck_std) %>%
    mutate(deck_type = ifelse(deck_std %in% c("A","B"),
                              "Risky / disadvantageous", "Safe / advantageous"))
  
  deck_colors <- c("A"="#C62828","B"="#EF5350","C"="#2E7D32","D"="#66BB6A")
  y_phase <- max(18, max(long$selections) - 2)
  subtitle1 <- "Trials are grouped into 5 blocks of 20. Each point shows how many times you picked each option in that block. Options A & B (red, triangles) are risky/disadvantageous; C & D (green, circles) are safer/advantageous. Faint dashed grey = class average."
  
  ggplot(long, aes(x = block, y = selections,
                   color = deck_std, group = deck_std, shape = deck_type)) +
    geom_rect(data = PHASE_BANDS,
              aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
              inherit.aes = FALSE, fill = PHASE_BANDS$fill, alpha = 0.32) +
    geom_line(data = class_choices,
              aes(x = block, y = mean_sel, group = deck_std),
              inherit.aes = FALSE, color = "grey40", alpha = 0.18, linewidth = 0.9, linetype = "22") +
    geom_point(data = class_choices,
               aes(x = block, y = mean_sel, group = deck_std),
               inherit.aes = FALSE, color = "grey35", alpha = 0.14, size = 1.8) +
    geom_line(linewidth = 1.25) +
    geom_point(size = 2.6) +
    scale_color_manual(values = deck_colors, name = "Option (A–D)") +
    scale_shape_manual(values = c("Risky / disadvantageous" = 17, "Safe / advantageous" = 16),
                       guide = "none") +
    scale_x_continuous(breaks = 1:5, labels = BLOCK_LABELS) +
    scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, 5),
                       expand = expansion(add = c(0, 3))) +
    labs(
      title = paste0("Figure 1 — Your choices across the task (Subject ", sid, ")"),
      subtitle = str_wrap(subtitle1, 100),
      x = "Trial ranges", y = "Selections in block (out of 20)"
    ) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.margin = margin(12, 12, 32, 12),
          plot.subtitle = element_text(size = 11, colour = "grey15")) +
    geom_vline(xintercept = c(1.5, 3.5, 4.5), linetype = "dashed", color = "grey45") +
    annotate("text",
             x = (PHASE_BANDS$xmin + PHASE_BANDS$xmax)/2,
             y = y_phase, label = PHASE_BANDS$label, size = 4, lineheight = 1.05) +
    coord_cartesian(clip = "off")
}

# ---------- Figure 2 ----------
make_rt_plot <- function(sid) {
  dat <- igt %>% filter(subject_id == sid, block %in% 1:5)
  rt_sum <- dat %>%
    group_by(block) %>%
    summarise(q1 = quantile(rt, 0.25, na.rm=TRUE),
              med = median(rt, na.rm=TRUE),
              q3 = quantile(rt, 0.75, na.rm=TRUE), .groups="drop") %>%
    complete(block = 1:5, fill = list(q1 = NA_real_, med = NA_real_, q3 = NA_real_))
  y_phase <- max(rt_sum$q3, na.rm = TRUE)
  subtitle2 <- "Dot = your typical (median) reaction time. The vertical bar shows where most of your times fell — the middle 50% (between the 25th and 75th percentiles). Higher RT = more deliberate System 2; lower RT = more intuitive System 1. Faint dashed grey = class average."
  
  ggplot(rt_sum, aes(x = block, y = med)) +
    geom_rect(data = PHASE_BANDS,
              aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
              inherit.aes = FALSE, fill = PHASE_BANDS$fill, alpha = 0.32) +
    geom_line(data = class_rt, aes(x = block, y = mean_med_rt),
              color = "grey40", alpha = 0.22, linewidth = 0.9, linetype = "22") +
    geom_point(data = class_rt, aes(x = block, y = mean_med_rt),
               color = "grey35", alpha = 0.16, size = 1.8) +
    geom_linerange(aes(ymin = q1, ymax = q3), linewidth = 1, color = "grey35") +
    geom_line(color = "steelblue", linewidth = 1.2, na.rm = TRUE) +
    geom_point(color = "steelblue", size = 2.4, na.rm = TRUE) +
    scale_x_continuous(breaks = 1:5, labels = BLOCK_LABELS) +
    labs(
      title = paste0("Figure 2 — Reaction time by block (Subject ", sid, ")"),
      subtitle = str_wrap(subtitle2, 100),
      x = "Trial ranges", y = "Reaction time (ms)"
    ) +
    theme_minimal(base_size = 13) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.margin = margin(12, 12, 28, 12),
          plot.subtitle = element_text(size = 11, colour = "grey15")) +
    geom_vline(xintercept = c(1.5, 3.5, 4.5), linetype = "dashed", color = "grey45") +
    annotate("text",
             x = (PHASE_BANDS$xmin + PHASE_BANDS$xmax)/2,
             y = y_phase + 0.05*y_phase, label = PHASE_BANDS$label, size = 4, lineheight = 1.05) +
    coord_cartesian(clip = "off")
}

# ---------- Figure 3 ----------
make_money_plot <- function(sid) {
  dat <- igt %>% filter(subject_id == sid, block %in% 1:5)
  earn <- dat %>%
    group_by(block) %>%
    summarise(net = sum(amountWon - feeToPay, na.rm = TRUE), .groups = "drop") %>%
    complete(block = 1:5, fill = list(net = 0)) %>%
    arrange(block) %>% mutate(cum_net = cumsum(net))
  
  y_min <- min(earn$cum_net, 0, na.rm = TRUE)
  y_max <- max(earn$cum_net, 0, na.rm = TRUE)
  y_phase <- y_min + 0.9*(y_max - y_min)
  subtitle3 <- "Dashed red line = $0 (break-even). You started with a hypothetical $2000 loan; values above 0 mean you are in profit. Faint dashed grey = class average."
  
  ggplot(earn, aes(x = block, y = cum_net)) +
    geom_rect(data = PHASE_BANDS,
              aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
              inherit.aes = FALSE, fill = PHASE_BANDS$fill, alpha = 0.32) +
    geom_line(data = class_money, aes(x = block, y = mean_cum),
              color = "grey40", alpha = 0.22, linewidth = 0.9, linetype = "22") +
    geom_point(data = class_money, aes(x = block, y = mean_cum),
               color = "grey35", alpha = 0.16, size = 1.8) +
    geom_line(color = "darkgreen", linewidth = 1.2) +
    geom_point(color = "darkgreen", size = 2.4) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
    geom_vline(xintercept = c(1.5, 3.5, 4.5), linetype = "dashed", color = "grey45") +
    scale_x_continuous(breaks = 1:5, labels = BLOCK_LABELS) +
    labs(
      title = paste0("Figure 3 — Money earned (cumulative) (Subject ", sid, ")"),
      subtitle = str_wrap(subtitle3, 100),
      x = "Trial ranges", y = "Cumulative net earnings ($)"
    ) +
    theme_minimal(base_size = 13) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.margin = margin(12, 12, 28, 12),
          plot.subtitle = element_text(size = 11, colour = "grey15")) +
    annotate("text",
             x = (PHASE_BANDS$xmin + PHASE_BANDS$xmax)/2,
             y = y_phase, label = PHASE_BANDS$label, size = 4, lineheight = 1.05) +
    coord_cartesian(clip = "off")
}

# ---------- Cover page (single header; intro paragraph + bullets) ----------
draw_cover_page <- function(student_name, sid) {
  grid.newpage()
  pushViewport(apa_content_vp())  # <<< APA margin box
  
  # layout inside the APA box
  lay <- grid.layout(
    nrow = 7, ncol = 1,
    heights = unit(c(
      0.12,  # (1) title
      0.13,  # (2) big question
      0.07,  # (3) recipient
      0.08,  # (4) header
      0.40,  # (5) intro paragraph
      0.02,  # spacer
      0.18   # (7) reference (wrapped)
    ), "npc")
  )
  vp <- viewport(layout = lay, width = unit(1, "npc"), height = unit(1, "npc"))
  pushViewport(vp)
  place <- function(g, row) grid.draw(editGrob(g, vp = viewport(
    layout.pos.row = row, layout.pos.col = 1, x = unit(0, "npc"))))
  
  # (1) Report title (centered)
  place(textGrob("Individual feedback report:",
                 x = 0.5, y = 0.5, just = "center",
                 gp = gpar(fontfamily = "Times", fontsize = 16, fontface = "bold")), 1)
  
  # (2) Big question (centered)
  place(textGrob("How well do you use your emotions and intuition in decision–making?",
                 x = 0.5, y = 0.5, just = "center",
                 gp = gpar(fontfamily = "Times", fontsize = 18, fontface = "bold")), 2)
  
  # (3) Recipient (centered)
  place(textGrob(glue::glue("Report for: {student_name}  (Subject {sid})"),
                 x = 0.5, y = 0.5, just = "center",
                 gp = gpar(fontfamily = "Times", fontsize = 11, fontface = "bold", col = "grey30")), 3)
  
  # (4) Header
  place(textGrob("What this report contains",
                 x = 0, y = 1, just = c("left","top"),
                 gp = gpar(fontfamily = "Times", fontsize = 14, fontface = "bold")), 4)
  
  # (5) Intro paragraph (task + purpose)
  intro_txt <-
    "You completed the Iowa Gambling Task (developed to test the Somatic Marker Hypothesis; see Bechara, Damasio, Damasio, & Anderson, 2013) using PsyToolkit. On each trial you chose one of four options (shown as buttons/circles). You started with a hypothetical $2000 loan and were asked to maximize profit. Options A & B are risky and disadvantageous in the long run (sometimes large wins, but also high fees/penalties); options C & D are safer and advantageous (smaller wins, fewer penalties/fees).\n\nThis report gives you personal feedback: Page 2 explains how to read your figures. Page 3 shows what you chose over time (grouped into five blocks of 20 trials), Page 4 shows how quickly you made decisions (reaction time), and Page 5 shows how your money changed. Together these plots help you see whether your choices shifted toward the better options (C & D), whether decisions became faster as you learned, and whether your earnings trended upward."
  place(wrapped_paragraph(intro_txt, width = 100, size = 12), 5)
  
  # (7) APA 7 reference (wrapped, italic)
  ref <- "Bechara, A., Damasio, A. R., Damasio, H., & Anderson, S. W. (2013). Insensitivity to future consequences following damage to human prefrontal cortex. In *Personality and Personality Disorders* (pp. 287–295). Routledge."
  place(wrapped_paragraph(ref, width = 100, size = 10, face = "italic", lh = 1.2), 7)
  
  popViewport(); popViewport()  # leave APA box
}


draw_howto_page <- function() {
  grid.newpage()
  pushViewport(apa_content_vp())  # APA margin box
  
  lay <- grid.layout(
    nrow = 5, ncol = 1,
    heights = unit(c(0.06, 0.12, 0.08, 0.68, 0.06), "npc") # pad, header, subhead, bullets, ref
  )
  vp <- viewport(layout = lay)
  pushViewport(vp)
  place <- function(g, row) grid.draw(editGrob(g, vp = viewport(
    layout.pos.row = row, layout.pos.col = 1, x = unit(0, "npc"))))
  
  place(textGrob("How to read the figures (and what they suggest)",
                 gp = gpar(fontfamily = "Times", fontsize = 16, fontface = "bold"),
                 x = 0, y = 1, just = c("left","top")), 2)
  
  place(textGrob("Use these notes while you look at your plots on the next pages:",
                 gp = gpar(fontfamily = "Times", fontsize = 12, col = "grey25"),
                 x = 0, y = 1, just = c("left","top")), 3)
  
  bullets <- paste(
    "- Figure 1 (choices): We group the 100 trials into five 20-trial blocks so patterns are easier to see. Each point shows how many times you picked each option in that block. A & B (red, triangles) are risky in the long run; C & D (green, circles) are safer. Interpreting: a common learning pattern is a shift toward C/D in later blocks—more C/D → fewer penalties → better long-term outcome.",
    "- Figure 2 (reaction time): The dot is your typical (median) time; the vertical bar shows where most of your times fell (middle 50%). Interpreting: as helpful ‘somatic markers’ develop, some people slow a bit mid-task while a hunch forms, then speed up once the strategy clicks. Persistently low RT with A/B can reflect chasing immediate rewards; persistently high RT without a shift to C/D can reflect uncertainty/over-deliberation.",
    "- Figure 3 (money): This is your cumulative earnings. The dashed red line is $0 (break-even). Because the task starts with a $2000 loan, going above the red line means you’ve repaid the loan and are now in profit. Interpreting: an upward trend—especially later—usually pairs with more C/D; flat/down lines often reflect frequent penalties (more A/B).",
    sep = "\n\n"
  )
  place(wrapped_paragraph(bullets, width = 100, size = 12), 4)
  
  # same APA reference at bottom inside margins
  ref <- "Bechara, A., Damasio, A. R., Damasio, H., & Anderson, S. W. (2013). Insensitivity to future consequences following damage to human prefrontal cortex. In *Personality and Personality Disorders* (pp. 287–295). Routledge."
  place(wrapped_paragraph(ref, width = 100, size = 10, face = "italic", lh = 1.2), 5)
  
  popViewport(); popViewport()
}


# ---------- Report generator ----------
make_subject_report <- function(sid, out_dir = "reports") {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_file <- file.path(out_dir, sprintf("IGT_feedback_subject_%s.pdf", sid))
  total_pages <- 5  # was 4
  
  student_name <- igt %>% filter(subject_id == sid) %>% pull(name) %>%
    { .[!is.na(.)][1] %||% paste("ID", sid) }
  
  pdf(out_file, width = 11, height = 8.5, onefile = TRUE)
  
  # 1) Cover
  draw_cover_page(student_name, sid); add_page_number(1, total_pages)
  
  # 2) New “How to read the figures” page
  draw_howto_page();                          add_page_number(2, total_pages)
  
  # 3–5) Plots
  grid.newpage(); print(make_choices_plot(sid), newpage = FALSE); add_page_number(3, total_pages)
  grid.newpage(); print(make_rt_plot(sid),      newpage = FALSE); add_page_number(4, total_pages)
  grid.newpage(); print(make_money_plot(sid),   newpage = FALSE); add_page_number(5, total_pages)
  
  dev.off()
  message("Saved: ", out_file)
}


# ---------- RUN ----------
all_subjects <- sort(unique(igt$subject_id))
# Try one first:
# make_subject_report(all_subjects[1])
invisible(lapply(all_subjects, make_subject_report))

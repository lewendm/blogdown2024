# ======================================================
# .sav -> per-student HTML + PDF feedback (CRT & REI)
# Elegant PDFs with page numbers & headers/footers
# + Appendix with full item wording (CRT + REI 40 items)
# ======================================================

# -------- Settings --------
sav_path     <- "data/student_responses3.sav"      # <- your .sav
out_dir      <- "exports"
assets_dir   <- "assets"
quadrant_img <- file.path(assets_dir, "analytic_intuitive_quadrant.jpg")  # JPG/PNG is fine
course_title <- "Decision-making Processes in Organizations"
report_date  <- format(Sys.Date(), "%B %d, %Y")

# -------- Packages --------
suppressPackageStartupMessages({
  library(tidyverse)
  library(haven)
  library(fmsb)
  library(scales)
  library(glue)
  library(stringr)
  library(htmltools)   # for htmlEscape
  library(pagedown)    # for chrome_print PDFs
  library(psych)       # for alpha (optional, if you want to run reliability checks later)
})

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Make the quadrant image available next to each HTML/PDF
if (file.exists(quadrant_img)) {
  file.copy(quadrant_img, file.path(out_dir, basename(quadrant_img)), overwrite = TRUE)
}

# -------- Load data (SAV only) --------
stopifnot(file.exists(sav_path))
dat <- haven::read_sav(sav_path)
names(dat) <- make.names(names(dat))

# Coerce RS_/ES_ to numeric; leave crt_* as character (free text)
dat <- dat %>%
  mutate(
    across(starts_with("RS_"), ~as.numeric(as.character(.))),
    across(starts_with("ES_"), ~as.numeric(as.character(.)))
  )

# Ensure keys exist and types are sane
if (!"student_id" %in% names(dat)) dat <- dat %>% mutate(student_id = row_number())
dat <- dat %>% mutate(student_id = as.numeric(student_id))
if (!"name" %in% names(dat)) dat$name <- NA_character_
dat$name <- as.character(dat$name)

# -------- FIX ES MISLABELING: rename ES_* by current dataset order --------
# Find all ES_* columns in their current left-to-right order and relabel to ES_1..ES_k
es_pos  <- which(startsWith(names(dat), "ES_"))
old_es  <- names(dat)[es_pos]
new_es  <- paste0("ES_", seq_along(es_pos))  # ES_1..ES_k
# Create old->new map (used later to translate reverse-coded list)
es_map  <- setNames(new_es, old_es)
# Apply renaming
names(dat)[es_pos] <- new_es

# ------------------------------------------------------
# Map YOUR item order to REI-40 official subscales
# (Indices refer to YOUR order after the ES relabeling)
# ------------------------------------------------------
# RS (your order)
RS_idx_RA <- c(2,4,8,9,12,13,14,16,17,19)   # Rational Ability
RS_idx_RE <- c(1,3,5,6,7,10,11,15,18,20)    # Rational Engagement
# ES (your order)
ES_idx_EA <- c(2,3,4,7,8,9,15,17,19,20)     # Experiential Ability
ES_idx_EE <- c(1,5,6,10,11,12,13,14,16,18)  # Experiential Engagement

# -------- Reverse-coded items (SPSS-equivalent) --------
# RS list (RS names were not relabeled)
RS_rev <- c("RS_1", "RS_2", "RS_4", "RS_5", "RS_7",
            "RS_8", "RS_9", "RS_11", "RS_12", "RS_18")

# ES reverse list from SPSS (old names; may include gaps like ES_21)
# Translate to the new sequential ES_1..ES_k using old->new map
ES_rev_old <- c("ES_2", "ES_10", "ES_11", "ES_13", "ES_14",
                "ES_15", "ES_17", "ES_18", "ES_21")
ES_rev <- unname(es_map[ES_rev_old])
ES_rev <- ES_rev[!is.na(ES_rev)]  # keep only those that existed in your data

# Function to reverse Likert 1–5
rev5 <- function(x) ifelse(is.na(x), NA, 6 - x)

# --- Save pre-reversal copy for audit/appendix (labels included) ---
dat_pre_rev <- dat

# Apply reverse scoring
dat <- dat %>%
  mutate(
    across(all_of(intersect(RS_rev, names(.))), rev5),
    across(all_of(intersect(ES_rev, names(.))), rev5)
  )

# -------- Build REI item key table (labels from your Qualtrics file) --------
var_label <- function(x) {
  lb <- attr(x, "label")
  if (is.null(lb)) "" else as.character(lb)
}

rs_items <- names(dat)[startsWith(names(dat), "RS_")]
es_items <- names(dat)[startsWith(names(dat), "ES_")]

# Which subscale each item belongs to (by YOUR order)
subscale_of <- function(item) {
  if (startsWith(item, "RS_")) {
    idx <- as.integer(sub("RS_", "", item))
    if (idx %in% RS_idx_RA) return("Rational ability")
    if (idx %in% RS_idx_RE) return("Rational engagement")
    return(NA_character_)
  } else if (startsWith(item, "ES_")) {
    idx <- as.integer(sub("ES_", "", item))
    if (idx %in% ES_idx_EA) return("Experiential ability")
    if (idx %in% ES_idx_EE) return("Experiential engagement")
    return(NA_character_)
  }
  NA_character_
}

main_scale_of <- function(item) {
  if (startsWith(item, "RS_")) "Rational" else if (startsWith(item, "ES_")) "Experiential" else NA_character_
}

rev_set <- unique(c(intersect(RS_rev, names(dat)), intersect(ES_rev, names(dat))))

# -------- Hard-coded REI item wording (from your Qualtrics file) --------
rei_wording <- tribble(
  ~Item, ~Wording,
  "RS_1",  "I try to avoid situations that require thinking in depth about something",
  "RS_2",  "I'm not that good at figuring out complicated problems",
  "RS_3",  "I enjoy intellectual challenges",
  "RS_4",  "I am not very good at solving problems that require careful logical analysis",
  "RS_5",  "I don't like to have to do a lot of thinking",
  "RS_6",  "I enjoy solving problems that require hard thinking",
  "RS_7",  "Thinking is not my idea of an enjoyable activity",
  "RS_8",  "I am not a very analytical thinker",
  "RS_9",  "Reasoning things out carefully is not one of my strong points",
  "RS_10", "I prefer complex problems to simple problems",
  "RS_11", "Thinking hard and for a long time about something gives me little satisfaction",
  "RS_12", "I don't reason well under pressure",
  "RS_13", "I am much better at figuring things out logically than most people",
  "RS_14", "I have a logical mind",
  "RS_15", "I enjoy thinking in abstract terms",
  "RS_16", "I have no problem thinking things through carefully",
  "RS_17", "Using logic usually works well for me in figuring out problems in my life",
  "RS_18", "Knowing the answer without having to understand the reasoning behind it is good enough for me",
  "RS_19", "I usually have clear, explainable reasons for my decisions",
  "RS_20", "Learning new ways to think would be very appealing to me",
  "ES_1",  "I like to rely on my intuitive impressions",
  "ES_2",  "I don't have a very good sense of intuition",
  "ES_4",  "Using my gut feelings usually works well for me in figuring out problems in my life",
  "ES_5",  "I believe in trusting my hunches",
  "ES_6",  "Intuition can be a very useful way to solve problems",
  "ES_7",  "I often go by my instincts when deciding on a course of action",
  "ES_8",  "I trust my initial feelings about people",
  "ES_9",  "When it comes to trusting people, I can usually rely on my gut feelings",
  "ES_10", "If I were to rely on my gut feelings, I would often make mistakes",
  "ES_11", "I don't like situations in which I have to rely on intuition",
  "ES_12", "I think there are times when one should rely on one's intuition",
  "ES_13", "I think it is foolish to make important decisions based on feelings",
  "ES_14", "I don't think it is a good idea to rely on one's intuition for important decisions",
  "ES_15", "I generally don't depend on my feelings to help me make decisions",
  "ES_16", "I hardly ever go wrong when I listen to my deepest gut feelings to find an answer",
  "ES_17", "I would not want to depend on anyone who described himself or herself as intuitive",
  "ES_18", "My snap judgments are probably not as good as most people's",
  "ES_19", "I tend to use my heart as a guide for my actions",
  "ES_20", "I can usually feel when a person is right or wrong, even if I can't explain how I know",
  "ES_21", "I suspect my hunches are inaccurate as often as they are accurate"
)

# Keep only items that exist in your dataset (after ES renaming)
rei_wording <- rei_wording %>% filter(Item %in% names(dat))

# Add scale, subscale, and reverse flag
rei_items_tbl <- rei_wording %>%
  mutate(
    `Main scale` = ifelse(startsWith(Item, "RS_"), "Rational", "Experiential"),
    Subscale = vapply(Item, subscale_of, character(1)),
    `Reverse scored?` = Item %in% rev_set
  )

# -------- Keying audit (console only; shows mean before/after reversal) --------
audit_tbl <- tibble::tibble(
  item        = c(rs_items, es_items),
  scale       = ifelse(startsWith(c(rs_items, es_items), "RS_"), "RS", "ES"),
  label       = vapply(c(rs_items, es_items), function(v) var_label(dat_pre_rev[[v]]), character(1)),
  reversed    = c(rs_items, es_items) %in% rev_set,
  mean_before = vapply(c(rs_items, es_items), function(v) mean(suppressWarnings(as.numeric(dat_pre_rev[[v]])), na.rm = TRUE), numeric(1)),
  mean_after  = vapply(c(rs_items, es_items), function(v) mean(suppressWarnings(as.numeric(dat[[v]])), na.rm = TRUE), numeric(1))
) %>%
  arrange(scale, item)

cat("\n=== REI Keying Audit (means before/after reversal) ===\n")
print(audit_tbl, n = nrow(audit_tbl))  # console only

# =========================
# CRT auto-scoring (free text)
# Items: crt_1 (Bat & Ball), crt_2 (Widgets), crt_3 (Lily pads)
# =========================
library(stringr)

# Normalizers
norm_txt <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("\\s+", " ", x, perl = TRUE)
  x
}
dotify <- function(x) gsub(",", ".", x, fixed = TRUE)
extract_num <- function(x) {
  x <- dotify(x)
  m <- str_extract(x, "(?<![0-9.-])[+-]?[0-9]*\\.?[0-9]+")
  suppressWarnings(as.numeric(m))
}
has_any <- function(x, patterns) {
  sapply(patterns, function(p) grepl(p, x, perl = TRUE)) |> apply(1, any)
}

# CRT-1 Bat & Ball (correct: 5 cents / 0.05)
score_crt1 <- function(ans) {
  a0 <- as.character(ans); a <- norm_txt(a0)
  a <- gsub("\\bfive\\b", "5", a); a <- gsub("\\bfem\\b", "5", a)  # EN/NO
  unit_cent <- c("cent\\b","cents\\b","\\bc\\b","¢","ct\\b","cts\\b","p\\b","pence\\b",
                 "penny\\b","pennies\\b","øre\\b","oere\\b")
  wrote_5       <- grepl("\\b5\\b", a)
  wrote_5_unit  <- wrote_5 & has_any(a, unit_cent)
  wrote_just5   <- grepl("^\\s*5\\s*$", a)
  num <- extract_num(a)
  wrote_005     <- !is.na(num) & abs(num - 0.05) < 1e-8
  out <- ifelse(wrote_5_unit | wrote_just5 | wrote_005, 1L,
                ifelse(is.na(a0) | a == "", NA_integer_, 0L))
  as.integer(out)
}

# CRT-2 Widgets (correct: 5 minutes)
score_crt2 <- function(ans) {
  a0 <- as.character(ans); a <- norm_txt(a0)
  a <- gsub("\\bfive\\b", "5", a); a <- gsub("\\bfem\\b", "5", a)
  unit_min <- c("minute\\b","minutes\\b","\\bmin\\b","mins\\b","minutter\\b","minutt\\b")
  looks_like_five_min <- grepl("^\\s*0?5\\s*[:]\\s*0{2}\\s*$", a) |
    grepl("^\\s*0?0\\s*[:]\\s*0?5\\s*[:]\\s*0{2}\\s*$", a)
  num <- extract_num(a)
  wrote_5_with_unit <- !is.na(num) & abs(num - 5) < 1e-8 & has_any(a, unit_min)
  wrote_just5       <- grepl("^\\s*5\\s*$", a)
  out <- ifelse(wrote_5_with_unit | wrote_just5 | looks_like_five_min, 1L,
                ifelse(is.na(a0) | a == "", NA_integer_, 0L))
  as.integer(out)
}

# CRT-3 Lily pads (correct: 47 days)
score_crt3 <- function(ans) {
  a0 <- as.character(ans); a <- norm_txt(a0)
  has_47   <- grepl("\\b47\\b", a)
  unit_day <- c("day\\b","days\\b","dag\\b","dager\\b")
  wrote_47_unit <- has_47 & has_any(a, unit_day)
  wrote_just47  <- grepl("^\\s*47(\\b|\\D)", a)  # allow "47", "47th", "47."
  out <- ifelse(wrote_47_unit | wrote_just47, 1L,
                ifelse(is.na(a0) | a == "", NA_integer_, 0L))
  as.integer(out)
}

# Ensure crt_* vars exist and are character
for (v in c("crt_1","crt_2","crt_3")) {
  if (!v %in% names(dat)) dat[[v]] <- NA_character_
  dat[[v]] <- as.character(dat[[v]])
}

# Score CRT
dat <- dat %>%
  mutate(
    crt_1_score = score_crt1(crt_1),
    crt_2_score = score_crt2(crt_2),
    crt_3_score = score_crt3(crt_3),
    crt_total   = rowSums(across(c(crt_1_score, crt_2_score, crt_3_score)), na.rm = TRUE)
  )

# -------- Compute REI scores --------
dat <- dat %>%
  mutate(
    rational_ability         = rowMeans(across(all_of(paste0("RS_", RS_idx_RA))), na.rm = TRUE),
    rational_engagement      = rowMeans(across(all_of(paste0("RS_", RS_idx_RE))), na.rm = TRUE),
    experiential_ability     = rowMeans(across(all_of(paste0("ES_", ES_idx_EA))), na.rm = TRUE),
    experiential_engagement  = rowMeans(across(all_of(paste0("ES_", ES_idx_EE))), na.rm = TRUE),
    rational   = rowMeans(cbind(rational_ability, rational_engagement), na.rm = TRUE),
    intuitive  = rowMeans(cbind(experiential_ability, experiential_engagement), na.rm = TRUE)
  )

# -------- Benchmarks --------
rei_bench <- tibble::tribble(
  ~Scale,                    ~Men,  ~Women,
  "Rationality",              3.54,  3.36,
  "Rational ability",         3.54,  3.29,
  "Rational engagement",      3.55,  3.42,
  "Experientiality",          3.33,  3.57,
  "Experiential ability",     3.35,  3.53,
  "Experiential engagement",  3.31,  3.61
)
crt_bench <- list(mean = 1.24)  # earlier research average (Frederick, 2005)

class_means <- dat %>%
  summarise(
    rationality             = mean(rational, na.rm=TRUE),
    rational_ability        = mean(rational_ability, na.rm=TRUE),
    rational_engagement     = mean(rational_engagement, na.rm=TRUE),
    experientiality         = mean(intuitive, na.rm=TRUE),
    experiential_ability    = mean(experiential_ability, na.rm=TRUE),
    experiential_engagement = mean(experiential_engagement, na.rm=TRUE),
    crt_mean                = mean(crt_total, na.rm=TRUE)
  )

# -------- Quadrant (type) using main scales --------
quadrant_label <- function(r, i){
  r_cat <- ifelse(r >= 3.5, "High", ifelse(r <= 2.5, "Low", "Moderate"))
  i_cat <- ifelse(i >= 3.5, "High", ifelse(i <= 2.5, "Low", "Moderate"))
  if (r_cat=="High" && i_cat=="High")       return("Cognitively versatile")
  if (r_cat=="High" && i_cat=="Low")        return("Detail conscious")
  if (r_cat=="Low"  && i_cat=="High")       return("Big-picture conscious")
  if (r_cat=="Low"  && i_cat=="Low")        return("Non-discerning")
  r_side <- ifelse(r >= 3, "High", "Low")
  i_side <- ifelse(i >= 3, "High", "Low")
  if (r_side=="High" && i_side=="High")     return("Cognitively versatile")
  if (r_side=="High" && i_side=="Low")      return("Detail conscious")
  if (r_side=="Low"  && i_side=="High")     return("Big-picture conscious")
  "Non-discerning"
}

# -------- Plotters --------
plot_crt_items <- function(me, file_png){
  items <- c("crt_1_score","crt_2_score","crt_3_score")
  if (!all(items %in% names(me))) return(invisible(NULL))
  df <- tibble(
    Item = c("Q1 Bat & ball","Q2 Machines","Q3 Lily pads"),
    Correct = as.integer(me[items])
  )
  df$Status <- ifelse(df$Correct==1, "Correct", "Incorrect")
  p <- ggplot(df, aes(x = Item, y = 1, fill = Status)) +
    geom_col(width = 0.6) +
    geom_text(aes(label = Status), vjust = -0.4, size = 4, color="grey10") +
    scale_fill_manual(values = c("Correct"="#3CB371","Incorrect"="#E74C3C")) +
    coord_cartesian(ylim = c(0, 1.4)) +
    labs(title = "Cognitive Reflection Test (CRT)",
         subtitle = "Which items you got right/wrong",
         x = NULL, y = NULL) +
    theme_minimal(base_size = 13) +
    theme(
      axis.text.y = element_blank(),
      axis.ticks  = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position = "none",
      plot.title = element_text(face = "bold")
    )
  ggsave(file_png, p, width = 6.3, height = 3.0, dpi = 150)
}

plot_radar4 <- function(me, file_png){
  needed <- c("rational_ability","rational_engagement","experiential_ability","experiential_engagement")
  vals <- me %>% select(all_of(needed)) %>% as.data.frame()
  radar_df <- rbind(
    data.frame(rational_ability=5, rational_engagement=5, experiential_ability=5, experiential_engagement=5),
    data.frame(rational_ability=1, rational_engagement=1, experiential_ability=1, experiential_engagement=1),
    vals[1, ]
  )
  png(file_png, width=900, height=650, res=120)
  op <- par(mar=c(4,4,5,4))
  fmsb::radarchart(
    radar_df,
    axistype   = 1,
    seg        = 4,
    caxislabels= c("1","2","3","4","5"),
    pcol="#2E86DE", plwd=2, pfcol=scales::alpha("#2E86DE", 0.18),
    cglcol="grey80", cglty=1, cglwd=0.9,
    axislabcol="grey30", vlcex=1.05,
    vlabels=c("Rational ability","Rational engagement","Experiential ability","Experiential engagement")
  )
  title(main = paste0("REI Subscales – Student ", me$student_id), cex.main = 1.2)
  par(op); dev.off()
}

# -------- HTML helpers --------
html_table <- function(df) {
  # Coerce logicals to Yes/No for nicer display
  df <- df %>% mutate(across(where(is.logical), ~ifelse(.x, "Yes", "No")))
  rows <- apply(df, 1, function(r) paste0("<tr>", paste0("<td>", r, "</td>", collapse = ""), "</tr>"))
  paste0(
    "<table border='1' cellspacing='0' cellpadding='8' style='border-color:#d9d9d9;'>",
    "<thead><tr style='background:#f6f6f6'>", paste0("<th style='text-align:left'>", names(df), "</th>", collapse = ""), "</tr></thead>",
    "<tbody>", paste0(rows, collapse = ""), "</tbody></table>"
  )
}

rei_grouped_table <- function(me, rei_bench, class_means){
  df <- tibble::tribble(
    ~Scale, ~Range, ~You, ~Men, ~Women, ~`Class avg (this course)`,
    "<b>Rational (overall)</b>", "1–5", sprintf("%.2f", me$rational),
    sprintf("%.2f", rei_bench$Men[rei_bench$Scale=="Rationality"]),
    sprintf("%.2f", rei_bench$Women[rei_bench$Scale=="Rationality"]),
    sprintf("%.2f", class_means$rationality),
    
    "&nbsp;&nbsp;Rational ability", "1–5", sprintf("%.2f", me$rational_ability),
    sprintf("%.2f", rei_bench$Men[rei_bench$Scale=="Rational ability"]),
    sprintf("%.2f", rei_bench$Women[rei_bench$Scale=="Rational ability"]),
    sprintf("%.2f", class_means$rational_ability),
    
    "&nbsp;&nbsp;Rational engagement", "1–5", sprintf("%.2f", me$rational_engagement),
    sprintf("%.2f", rei_bench$Men[rei_bench$Scale=="Rational engagement"]),
    sprintf("%.2f", rei_bench$Women[rei_bench$Scale=="Rational engagement"]),
    sprintf("%.2f", class_means$rational_engagement),
    
    "<b>Experiential / Intuitive (overall)</b>", "1–5", sprintf("%.2f", me$intuitive),
    sprintf("%.2f", rei_bench$Men[rei_bench$Scale=="Experientiality"]),
    sprintf("%.2f", rei_bench$Women[rei_bench$Scale=="Experientiality"]),
    sprintf("%.2f", class_means$experientiality),
    
    "&nbsp;&nbsp;Experiential ability", "1–5", sprintf("%.2f", me$experiential_ability),
    sprintf("%.2f", rei_bench$Men[rei_bench$Scale=="Experiential ability"]),
    sprintf("%.2f", rei_bench$Women[rei_bench$Scale=="Experiential ability"]),
    sprintf("%.2f", class_means$experiential_ability),
    
    "&nbsp;&nbsp;Experiential engagement", "1–5", sprintf("%.2f", me$experiential_engagement),
    sprintf("%.2f", rei_bench$Men[rei_bench$Scale=="Experiential engagement"]),
    sprintf("%.2f", rei_bench$Women[rei_bench$Scale=="Experiential engagement"]),
    sprintf("%.2f", class_means$experiential_engagement)
  )
  html_table(df)
}

# ---- Personalized CRT summary + tables ----
crt_personal_html <- function(me, class_means, crt_bench){
  score <- me$crt_total
  if (is.na(score)) return("<p>No CRT responses recorded.</p>")
  interpretation <- switch(
    as.character(score),
    "0" = "You did not answer any of the three CRT problems correctly. This suggests you relied on your first intuitive (System 1) answers without checking them with deliberate reasoning (System 2). This is common, as the questions are designed to tempt intuition.",
    "1" = "You answered 1 of the 3 CRT problems correctly. This suggests that you occasionally paused to check your intuitive response, but in most cases you went with your first impression.",
    "2" = "You answered 2 of the 3 CRT problems correctly. This suggests that you often paused to engage System 2 reasoning and overrode your initial intuition, though not always.",
    "3" = "You answered all 3 CRT problems correctly. This suggests you consistently engaged deliberate System 2 reasoning to check and override misleading intuitive answers.",
    "Unexpected score."
  )
  glue("
    <p><b>Your CRT result:</b> {score}/3 correct.</p>
    <p>{interpretation}</p>
    <p><b>Comparison:</b> In earlier research, the average score was about {sprintf('%.2f', crt_bench$mean)}. 
    The class average here is {sprintf('%.2f', class_means$crt_mean)}.</p>
  ")
}

crt_comp_table <- function(me, crt_bench, class_means){
  html_table(tibble::tibble(
    Scale                        = "CRT total (0–3)",
    You                          = sprintf("%.0f", me$crt_total),
    `Earlier research`           = sprintf("%.2f", crt_bench$mean),
    `Class average (this course)`= sprintf("%.2f", class_means$crt_mean)
  ))
}

crt_explain_html <- function(){
  paste(
    "<ul>",
    "<li><b>Q1 Bat & ball</b> — Heuristic <b>10¢</b>. Correct <b>5¢</b> (bat $1.05 + ball $0.05 = $1.10; bat is exactly $1 more).</li>",
    "<li><b>Q2 Machines</b> — Heuristic <b>100 min</b>. Correct <b>5 min</b> (each machine makes 1 widget in 5 min; 100 machines → 100 widgets in 5 min).</li>",
    "<li><b>Q3 Lily pads</b> — Heuristic <b>24 days</b>. Correct <b>47 days</b> (doubles daily; if full on day 48, it was half on day 47).</li>",
    "</ul>", sep="\n"
  )
}

# ---- NEW: CRT outcomes (blue callout box) ----
crt_outcomes_box_html <- function(){
  paste0(
    "<div class='callout callout-blue'>",
    "<div class='callout-title'>What your CRT score tends to relate to (in research)</div>",
    "<ul>",
    "<li><b>Fewer classic reasoning slips</b> (e.g., framing, base-rate neglect, denominator neglect, conjunction fallacy, sunk cost) on lab tasks that tap everyday judgment. These links remain even after accounting for cognitive ability. <i>(Toplak, West, & Stanovich, 2011)</i></li>",
    "<li><b>Better discernment online</b>: people who score higher on CRT tend to share news from <i>higher-quality sources</i> on social media and use more “insight”/self-regulation language—evidence that reflective thinking shows up in real behavior, not just surveys. <i>(Nature Communications, 2021)</i></li>",
    "<li><b>Everyday beliefs</b>: higher analytic/reflective thinking relates to <i>lower</i> endorsement of conspiracy and paranormal claims and pseudo-profound statements, and to more evidence-sensitive beliefs. <i>(Pennycook et al., 2015)</i></li>",
    "<li><b>Moral judgment & values</b>: reflective thinkers are more likely to override immediate gut reactions in tricky moral scenarios, and tend to endorse more consequence-focused moral values. <i>(Pennycook et al., 2015)</i></li>",
    "</ul>",
    "<p class='note'>These are group-level trends from research samples—not prescriptions. A low CRT score on three riddles does not define you; it simply suggests moments where slowing down may help.</p>",
    "</div>"
  )
}

rei_intro_html <- function(){
  paste0(
    "<p>The REI (Pacini & Epstein, 1999) measures two complementary styles: ",
    "<b>Rational</b> (analytic, effortful, rule-based) and ",
    "<b>Experiential/Intuitive</b> (holistic, affect-based, pattern recognition). ",
    "Each has two subscales: <i>Ability</i> (how effective it feels) and <i>Engagement</i> (how much you like to use it). ",
    "See the <b>Appendix</b> at the end for the full list of items.</p>"
  )
}

# ---- Appendix builders ------------------------------------------------------
# Canonical CRT item wording (include in Appendix)
crt_items_tbl <- tibble::tribble(
  ~Item, ~Wording, ~Correct,
  "CRT Q1 (Bat & Ball)",
  "A bat and a ball cost $1.10 in total. The bat costs $1.00 more than the ball. How much does the ball cost?",
  "5 cents (0.05)",
  "CRT Q2 (Widgets)",
  "If it takes 5 machines 5 minutes to make 5 widgets, how long would it take 100 machines to make 100 widgets?",
  "5 minutes",
  "CRT Q3 (Lily pads)",
  "In a lake, there is a patch of lily pads. Every day, the patch doubles in size. If it takes 48 days for the patch to cover the entire lake, how long would it take for the patch to cover half the lake?",
  "47 days"
)

appendix_rei_html <- function(){
  # Show REI items with wording, main scale, subscale, and reverse flag
  rei_df <- rei_items_tbl %>%
    transmute(
      Item,
      Wording = ifelse(nchar(Wording)==0, "(no label found in .sav)", Wording),
      `Main scale`,
      Subscale,
      `Reverse scored?`
    )
  paste0(
    "<h2>Appendix</h2>",
    "<h3>A. Cognitive Reflection Test (CRT) — Full Items</h3>",
    html_table(crt_items_tbl),
    "<p class='note'>The CRT is not a test of intelligence. It measures how often we engage deliberate 'System 2' thinking to override an immediate but wrong 'System 1' answer.</p>",
    "<h3>B. REI Items (Rational–Experiential Inventory) — Full List</h3>",
    "<p>The table below lists all items from the survey, grouped by main scale and subscale. Items marked <i>Yes</i> under <b>Reverse scored?</b> were keyed in the opposite direction (i.e., 1↔5).</p>",
    html_table(rei_df)
  )
}

# -------- Build ALL reports --------
# Only include students who entered a name
ids <- dat %>%
  filter(!is.na(student_id)) %>%
  filter(!is.na(name) & trimws(name) != "") %>%
  pull(student_id) %>% unique() %>% sort()

for (sid in ids) {
  me <- dat %>% filter(student_id == sid) %>% slice(1)
  
  # Friendly title name (fallback to Student {sid}, but we filtered empty names above)
  nice_name <- ifelse(!is.na(me$name) & nzchar(me$name), me$name, paste("Student", sid))
  
  # Save figures
  crt_items_png <- paste0("crt_items_", sid, ".png")
  rei_radar_png <- paste0("rei_radar4_", sid, ".png")
  plot_crt_items(me, file.path(out_dir, crt_items_png))
  plot_radar4(me, file.path(out_dir, rei_radar_png))
  
  # Quadrant image tag (use whatever filename you copied)
  quad_basename <- basename(quadrant_img)
  quadrant_img_tag <- if (file.exists(file.path(out_dir, quad_basename))) {
    glue("<p><img alt='Analytic × Intuitive quadrant' src='{quad_basename}' style='max-width:100%;height:auto;border:1px solid #eee'></p>")
  } else {
    "<!-- quadrant image not found; place it under assets/ and set quadrant_img path -->"
  }
  
  # Elegant print styles (added .callout styles)
  print_css <- paste(
    "<style>",
    "@page { size: A4; margin: 20mm 18mm 20mm 18mm; }",
    "@media print {",
    "  body { -webkit-print-color-adjust: exact; print-color-adjust: exact; }",
    "  h1, h2, h3 { page-break-after: avoid; }",
    "  img { page-break-inside: avoid; }",
    "  table { page-break-inside: auto; }",
    "  tr { page-break-inside: avoid; page-break-after: auto; }",
    "}",
    "body{font-family:system-ui,-apple-system,Segoe UI,Roboto,Helvetica,Arial,sans-serif;max-width:860px;margin:30px auto;line-height:1.55;color:#222}",
    "h1{margin:.2em 0 .4em 0} h2{margin-top:1.3em} h3{margin-top:1.1em}",
    "table{border-collapse:collapse;margin:12px 0;width:100%;}",
    "th,td{border:1px solid #d9d9d9;padding:8px 10px} th{background:#f6f6f6;text-align:left}",
    "img{max-width:100%;height:auto;border:1px solid #eee;margin:6px 0}",
    ".note{color:#555;font-size:.95em}",
    ".callout{border-left:6px solid #2E86DE;background:#eef5ff;padding:12px 14px;margin:16px 0;border-radius:6px}",
    ".callout .callout-title{font-weight:600;margin:0 0 6px 0}",
    ".callout-blue{border-color:#2E86DE}",
    "</style>",
    sep = "\n"
  )
  
  # ---- HTML document ----
  html <- c(
    "<!DOCTYPE html><html><head><meta charset='utf-8'>",
    "<title>Decision Style Feedback</title>",
    print_css,
    "</head><body>",
    glue("<h1>Decision-making feedback report: {htmlEscape(nice_name)}</h1>"),
    glue("<p style='margin-top:-8px;color:#666'>{htmlEscape(course_title)} — {htmlEscape(report_date)}</p>"),
    
    # (summary line removed per your request)
    
    "<p class='note'>See the <b>Appendix</b> for the full wording of the CRT questions and all 40 REI items, including which items belong to each scale/subscale and which are reverse-scored.</p>",
    
    "<h2>CRT (Cognitive Reflection Test)</h2>",
    crt_personal_html(me, class_means, crt_bench),
    "<h3>Your score in the CRT</h3>",
    "<p>The chart below shows which items you got correct or incorrect.</p>",
    glue("<p><img alt='Cognitive Reflection Test (CRT)' src='{crt_items_png}'></p>"),
    crt_comp_table(me, crt_bench, class_means),
    "<h3>Item explanations</h3>",
    crt_explain_html(),
    "<h3>What the CRT measures</h3>",
    paste0(
      "<p>The Cognitive Reflection Test is not an IQ test. ",
      "It captures how often you <i>engage slow, deliberate reasoning (System 2)</i> ",
      "to override an immediate, intuitive answer generated by <i>fast, automatic thinking (System 1)</i>. ",
      "The items are designed so that a compelling but wrong answer pops to mind quickly (e.g., 10¢, 100 minutes, 24 days). ",
      "Correct responses require noticing the trap and engaging effortful calculation.</p>",
      "<p><b>Why people give the wrong answer.</b> System 1 produces fluent, plausible answers that <i>feel</i> right. ",
      "Without pausing to check, we accept them. CRT success typically reflects a tendency to pause, scrutinize, and compute ",
      "when a question seems easy.</p>",
      "<p><b>What CRT tends to predict.</b> Studies link higher CRT to fewer classic reasoning slips, better truth discernment online, ",
      "and reflective shifts in certain moral judgments and values. See the research highlights below.</p>"
    ),
    # ---- NEW: insert CRT outcomes callout once (no duplicates) ----
    crt_outcomes_box_html(),
    
    "<h2>REI (Rational–Experiential Inventory)</h2>",
    rei_intro_html(),
    "<p><b>Your scores vs. literature (men/women) and <i>Class avg (this course)</i></b></p>",
    rei_grouped_table(me, rei_bench, class_means),
    glue("<p><img alt='REI radar (4 subscales)' src='{rei_radar_png}'></p>"),
    
    # Quadrant (clear which one is theirs; others just as reference)
    {
      qlabel <- quadrant_label(me$rational, me$intuitive)
      defs <- list(
        "Cognitively versatile" = paste0(
          "<b>Cognitively versatile (High Rational + High Intuitive)</b>: ",
          "Comfortable switching between careful analysis and gut feelings. ",
          "Useful for complex problem-solving, creativity, and adapting your style to situational demands."
        ),
        "Detail conscious" = paste0(
          "<b>Detail conscious (High Rational + Low Intuitive)</b>: ",
          "Prefers structured, logical reasoning and systematic decision-making. ",
          "May underuse intuitive cues (e.g., quick social judgments, pattern recognition) that help in some contexts."
        ),
        "Big-picture conscious" = paste0(
          "<b>Big-picture conscious (Low Rational + High Intuitive)</b>: ",
          "Relies strongly on intuition, emotion, and holistic impressions. ",
          "Supports creativity and rapid judgments but may underuse effortful analysis for complex/technical tasks."
        ),
        "Non-discerning" = paste0(
          "<b>Non-discerning (Low Rational + Low Intuitive)</b>: ",
          "Reports low confidence in both analytic and intuitive styles. ",
          "Can feel less certain in judgment; deliberately strengthening either analysis or intuitive trust can help."
        )
      )
      your_def <- defs[[qlabel]]
      other_defs <- defs[names(defs) != qlabel]
      paste0(
        "<h3>Your decision-style quadrant</h3>",
        "<p>You are in: <span style='background:#eef;padding:2px 6px;border-radius:6px'><b>", htmlEscape(qlabel), "</b></span></p>",
        "<p>", your_def, "</p>",
        "<p class='note'><i>About the other quadrants (for reference):</i></p>",
        "<ul><li>", paste(unlist(other_defs), collapse="</li><li>"), "</li></ul>"
      )
    },
    quadrant_img_tag,
    
    # -------- Appendix (full wording) --------
    appendix_rei_html(),
    
    "<h2>References</h2>",
    "<p class='note'>",
    "Frederick, S. (2005). Cognitive Reflection and Decision Making. <i>Journal of Economic Perspectives, 19</i>(4), 25–42.<br>",
    "Hodgkinson, G. P., Sadler-Smith, E., Burke, L. A., Claxton, G., & Sparrow, P. R. (2009). ",
    "Intuition in organizations: Implications for strategic management. <i>Long Range Planning, 42</i>(3), 277–297.<br>",
    "Pacini, R., & Epstein, S. (1999). The relation of rational and experiential information-processing styles to ",
    "personality, basic beliefs, and the ratio-bias phenomenon. <i>Journal of Personality and Social Psychology, 76</i>(6), 972–987.<br>",
    # ---- NEW references added for CRT outcomes box ----
    "Toplak, M. E., West, R. F., & Stanovich, K. E. (2011). The Cognitive Reflection Test as a predictor of performance on heuristics-and-biases tasks. ",
    "<i>Memory & Cognition, 39</i>, 1275–1289.<br>",
    "Pennycook, G., Fugelsang, J. A., & Koehler, D. J. (2015). Everyday consequences of analytic thinking. ",
    "<i>Current Directions in Psychological Science, 24</i>(6), 425–432.<br>",
    "Pennycook, G., Epstein, Z., Mosleh, M., Arechar, A. A., Eckles, D., & Rand, D. G. (2021). ",
    "Understanding the everyday consequences of analytic thinking on social media behavior. <i>Nature Communications, 12</i>, 921.<br>",
    "</p>",
    "</body></html>"
  )
  
  out_html <- file.path(out_dir, paste0("feedback_", sid, ".html"))
  writeLines(html, con = out_html)
  
  # --------- PDF with elegant header/footer & page numbering ----------
  header_tmpl <- glue(
    '<div style="font-size:9px;width:100%;text-align:center;padding:0 8px;">
       <span>{htmlEscape(course_title)}</span>
     </div>'
  )
  footer_tmpl <- glue(
    '<div style="font-size:9px;width:100%;padding:0 8px;">
       <div style="float:left;color:#888;">{htmlEscape(report_date)}</div>
       <div style="float:right;color:#888;">Page <span class="pageNumber"></span> of <span class="totalPages"></span></div>
       <div style="clear:both;"></div>
     </div>'
  )
  
  pdf_file <- file.path(out_dir, paste0("feedback_", sid, ".pdf"))
  pagedown::chrome_print(
    input  = out_html,
    output = pdf_file,
    extra_args = c(
      "--display-header-footer",
      "--print-background=true",
      "--margin-top=72px",    # room for header
      "--margin-bottom=72px", # room for footer
      "--margin-left=14mm",
      "--margin-right=14mm",
      paste0("--header-template=", header_tmpl),
      paste0("--footer-template=", footer_tmpl)
    )
  )
}

message(
  "Done. Open the HTML/PDF files in: ", normalizePath(out_dir),
  "\nES relabel map (old -> new):\n", paste(paste(names(es_map), "->", es_map), collapse = "\n"),
  "\nIf the quadrant image is missing, place it at: ", quadrant_img
)

# ---------- Optional: quick reliability (console) ----------
# psych::alpha(dat %>% select(all_of(paste0("RS_", RS_idx_RA))), title="Rational Ability")
# psych::alpha(dat %>% select(all_of(paste0("RS_", RS_idx_RE))), title="Rational Engagement")
# psych::alpha(dat %>% select(all_of(paste0("ES_", ES_idx_EA))), title="Experiential Ability")
# psych::alpha(dat %>% select(all_of(paste0("ES_", ES_idx_EE))), title="Experiential Engagement")

# ========================================================
# CLEANING "Trajectories 04-04.csv"
# DFS Viewer [Aerial] raw export with split/misplaced rows
# ========================================================
#   • Audit EVERY row using "Track ID" and "Type"
#   • If Track ID is a valid integer AND Type is a valid string → this is a NEW track (not a continuation)
#   • If NOT → it is a continuation of the previous track → glue its data to the end of the previous row
#   • Produce a perfect wide CSV with no misplaced rows
#   • Then converts to **LONG format** (best for analysis)

pacman::p_load(tidyverse)
pacman::p_load(data.table)
# pacman::p_load(tidymodels)
# library(tidyr)
search() #give list of attached packages

# traj_4_4 <- read.csv(file.choose(), header = TRUE, na.strings = c("NA"), check.names = FALSE, strip.white = TRUE)
traj_4_4 <- "Trajectories 04-04.csv"
getwd()
list.files()

# ------------------------------------------------
# Step 1: Read line-by-line and merge continuation rows
# ------------------------------------------------
lines <- readLines(traj_4_4, warn = FALSE)
header <- lines[1]
data_lines <- lines[-1]

clean_lines <- character()
current_line <- ""

for (line in data_lines) {
  if (trimws(line) == "") next
  
  fields <- trimws(strsplit(line, ",")[[1]])
  
  track_id_str <- fields[1]
  type_str     <- fields[2]
  
# Rule you give: valid start = numeric Track ID + proper Type
  is_valid_start <- grepl("^[0-9]+$", track_id_str) && 
    !is.na(type_str) && 
    type_str %in% c("Car", "Medium Vehicle", "Heavy Vehicle", 
                    "Pedestrian", "Bicycle", "Motorcycle")
  
  if (is_valid_start) {
# Save previous record if it exists
    if (current_line != "") {
      clean_lines <- c(clean_lines, current_line)
    }
    current_line <- line          # start a brand-new record
  } else {
    # Continuation row → append its data
    if (current_line != "") {
      continuation <- paste(fields[fields != ""], collapse = ",")
      current_line <- paste0(current_line, ",", continuation)
    }
  }
}

# Don't forget the very last record
if (current_line != "") {
  clean_lines <- c(clean_lines, current_line)
}

# Write the perfectly fixed wide CSV
New_traj_04 <- "Trajectories_04-04_New.csv"
writeLines(c(header, clean_lines), New_traj_04)

cat(" Fixed file created:", New_traj_04, "\n")
cat("   Number of clean tracks:", length(clean_lines), "\n\n")

# ------------------------------------------------
# Step 2: Read the now-perfect wide file
# ------------------------------------------------
df_wide <- fread(New_traj_04, header = TRUE, fill = TRUE, stringsAsFactors = FALSE)

# Make column names R-friendly
colnames(df_wide) <- make.names(colnames(df_wide), unique = TRUE)

cat("Columns after fixing:", ncol(df_wide), "\n")

# ------------------------------------------------
# Step 3: Identify fixed columns vs. time-series columns
# ------------------------------------------------
fixed_cols <- c("Track.ID", "Type", "Score....", "Track.Width..m.", 
                "Track.Width.Displacement..m.", "Track.Length..m.", 
                "Track.Length.Displacement..m.", "Entry.Gate", 
                "Entry.Time..s.", "Exit.Gate", "Exit.Time..s.", 
                "Traveled.Dist...m.", "Avg..Speed..km.h.")

ts_pattern <- "^[xXyY]|Speed|Tan|Lat|Time|Angle"   # matches all time-point columns

# ------------------------------------------------
# Step 4: Reshape to LONG format (one row = one time point)
# ------------------------------------------------
long_df <- df_wide %>%
  pivot_longer(
    cols = matches(ts_pattern),
    names_to = c(".value", "time_point"),
    names_pattern = "(.*)\\.([0-9]+)$",
    names_transform = list(time_point = as.integer)
  ) %>%
  filter(!is.na(x.m.) & !is.na(y.m.)) %>%           # remove any padding
  mutate(
    Track.ID     = as.integer(Track.ID),
    time_point   = as.integer(time_point),
    Score....    = as.numeric(Score....),
    across(contains(c("x.m.", "y.m.", "Speed", "Tan", "Lat", "Time", "Angle")), as.numeric)
  ) %>%
  rename(
    `Track ID`          = Track.ID,
    `x [m]`             = x.m.,
    `y [m]`             = y.m.,
    `Speed [km/h]`      = Speed.km.h.,
    `Tan. Acc. [ms-2]`  = Tan.Acc..ms.2.,
    `Lat. Acc. [ms-2]`  = Lat.Acc..ms.2.,
    `Time [s]`          = Time.s.,
    `Angle [rad]`       = Angle.rad.
  ) %>%
  select(`Track ID`, Type, `Score [%]` = Score...., everything())

  # ------------------------------------------------
# Step 5: Save clean files
# ------------------------------------------------
fwrite(long_df,  "Trajectories_04-04_CLEAN_LONG.csv",  row.names = FALSE)
fwrite(df_wide,  "Trajectories_04-04_CLEAN_WIDE.csv",  row.names = FALSE)

cat("=== CLEANING COMPLETE ===\n")
cat("• LONG format (recommended):  Trajectories_04-04_CLEAN_LONG.csv\n")
cat("• WIDE format (original shape): Trajectories_04-04_CLEAN_WIDE.csv\n")
cat("Total tracks:", length(unique(long_df$`Track ID`)), "\n")
cat("Total time points:", nrow(long_df), "\n\n")

cat("Your UTM coordinates (x [m], y [m]) are now 100% clean and ready to merge with your earlier Tag/Image files.\n")



# ======================================================================
# Other trial code
# Separating
# traj.1 <- read.delim(file.choose(),
#                    sep = ";",
#                    header = TRUE,
#                    stringsAsFactors = FALSE,
#                    strip.white = TRUE,
#                    fill = TRUE)
# traj.1 <- traj.1[, names(df) != ""]
# head(traj.1)
# summary(traj.1)
# str(traj.1)
# 
# traj.1 <- read.table(file.choose(),
#                  sep = ";",
#                  header = TRUE,
#                  stringsAsFactors = FALSE,
#                  strip.white = TRUE)
# head(traj.1)


# Tidyverse alternative (recommended for robustness)
# Crosssing Events

# pacman::p_load(readr)
# 
# gates <- read_delim(file.choose(),
#                  delim = ";",
#                  trim_ws = TRUE)
# gates <- gates[, names(df) != ""]
# summary(gates)
# head(gates)
# str(gates)

# Explore
# library(dplyr)
# 
# traj.1 %>%
#   summarise(across(everything(), class)) %>%
#   pivot_longer(everything(),
#                names_to = "Variable",
#                values_to = "Class")
# 
# gates %>%
#   summarise(across(everything(), class)) %>%
#   pivot_longer(everything(),
#                names_to = "Variable",
#                values_to = "Class")

# ===============================
# 'Trials'
# traj.1 <- read.table(file.choose(),
#                      sep = ";",
#                      header = FALSE,
#                      skip = 1,
#                      fill = TRUE,
#                      stringsAsFactors = FALSE,
#                      strip.white = TRUE)
# 
# # Add column names for the first 8 fixed columns
# colnames(traj.1)[1:8] <- c("Track_ID", "Type", "Entry_Gate", "Entry_Time_s", 
#                            "Exit_Gate", "Exit_Time_s", "Traveled_Dist_px", "Avg_Speed_kpxh")
# 
# # header = FALSE + skip = 1 — skips the header row manually to avoid the column count mismatch
# 
# head(traj.1)
# summary(traj.1)
# str(traj.1)


# ---------------------------=======------------------------------
# Trajectory Dataframe

# # Create final trajectory data frame

library(data.table)     # For fast reading of large CSV files (fread)
# library(dplyr)          # For data manipulation (select, arrange, etc.)
search()
# ------------------------------------------------------------------
# 1. Define the input file
# ------------------------------------------------------------------
long_file <- "Trajectories_04-04_New.csv"

# Safety check: Make sure the cleaned file exists before proceeding
if (!file.exists(long_file)) {
  stop("Clean long file not found. Please run the cleaning script first.")
}

# ------------------------------------------------------------------
# 2. Read the cleaned data
# ------------------------------------------------------------------
# traj <- fread(long_file, stringsAsFactors = FALSE)
# Explanation:
#   - fread() from data.table is much faster and more memory-efficient 
#     than base R's read.csv() for large files like this one.
#   - stringsAsFactors = FALSE prevents automatic conversion of text to factors.

# ------------------------------------------------------------------
# 3. Create the final data frame by selecting only the needed columns
# ------------------------------------------------------------------
# final_df <- traj %>%
#   select(
#     `Track ID`,
#     Type,
#     `x [m]`,
#     `y [m]`,
#     `Speed [km/h]`,
#     `Tan. Acc. [ms-2]`,
#     `Lat. Acc. [ms-2]`,
#     `Time [s]`,
#     `Angle [rad]`
#   ) %>%
#   # Sort the data: first by Track ID, then by Time (chronological order)
#   arrange(`Track ID`, `Time [s]`)

# Explanation of this block:
#   - `%>%` is the pipe operator: it passes the result of one function 
#     into the next function.
#   - select() keeps only the 9 columns you wanted and removes everything else.
#   - arrange() reorders the rows so that for each Track ID, the time points 
#     are in increasing order of Time [s]. This is very useful for trajectories.

# ------------------------------------------------------------------
# 4. Print summary information
# ------------------------------------------------------------------
cat("Final data frame created successfully!\n")
cat("Number of tracks:", n_distinct(final_df$`Track ID`), "\n")
cat("Total time points (rows):", nrow(final_df), "\n")
cat("Columns:", paste(names(final_df), collapse = ", "), "\n\n")

# Show the first 6 rows as a preview
head(final_df)

# ------------------------------------------------------------------
# 5. Save the final result
# ------------------------------------------------------------------
fwrite(final_df, "Trajectories_04-04_FINAL.csv", row.names = FALSE)

cat("✅ Saved as 'Trajectories_04-04_FINAL.csv'\n")
cat("This file is now ready for analysis, plotting, or merging with your other files.\n")

# =============== Anth-Code ========================
# Read the file
my_traj <- read.csv(file.choose(), header = TRUE, check.names = FALSE, strip.white = TRUE)

# Helper: get all column names for a given trajectory field across all points
traj_field_cols <- function(field, df) {
  grep(paste0("^", field, " \\d+$"), names(df), value = TRUE)
}

# Identify the 4 metadata columns needed
meta_cols <- c("Track ID", "Type", "Traveled Dist. [m]", "Avg. Speed [km/h]")

# Identify all trajectory columns (exclude the spurious unnamed column)
traj_cols <- grep("^(x \\[m\\]|y \\[m\\]|Speed \\[km/h\\]|Tan\\. Acc\\. \\[ms-2\\]|Lat\\. Acc\\. \\[ms-2\\]|Time \\[s\\]|Angle \\[rad\\]) \\d+$",
                  names(my_traj), value = TRUE)

# Build traj_df: Track ID + Type + Traveled Dist. + Avg. Speed + all trajectory columns
traj_df <- my_traj[, c(meta_cols, traj_cols)]

# Write to CSV with semicolon separator
write.csv2(traj_df, "traj_df.csv", row.names = FALSE)
write.csv(traj_df, "traj_df2.csv", row.names = FALSE, sep = ";")
write.table(traj_df, "traj_df3.csv", sep = ";", row.names = FALSE, col.names = TRUE, quote = TRUE)
  

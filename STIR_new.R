library(readxl)
library(jsonlite)
library(dplyr)
library(SoilManageR)
library(lubridate)

# ---------------------------------------------------------
# 1) Chargement du fichier STIR (JSON)
# ---------------------------------------------------------
stir_data <- fromJSON("C:\\R_projects\\stir.json")
stir_data <- stir_data[-1, ]
stir_data <- stir_data %>% 
  add_row(
    Tool = "Arracheuse de lin",
    Speed = 20,
    WorkType = 0.20,
    Deep = 1.5,
    Soil = 0.20,
    W = "TCS Léger"
  ) %>% 
  add_row(
    Tool = "Désherbage manuel",
    Speed = 2,
    WorkType = 0.8,
    Deep = 8,
    Soil = 0.85,
    W = "TCS Léger"
  ) %>% 
  add_row(
    Tool = "Désherbage Thermique",
    Speed = 0,
    WorkType = 0,
    Deep = 0,
    Soil = 0,
    W = "Pas applicable"
  ) %>% 
  add_row(
    Tool = "Arracheuse onions",
    Speed = 1.60934,
    WorkType = 0.40,
    Deep = 12.7,
    Soil = 0.35,
    W = "Pas applicable"
  ) %>% 
  add_row(
    Tool = "Butteuse",
    Speed = 8,
    WorkType = 0.8,
    Deep = 15,
    Soil = 1,
    W = "TCS Léger"
  )

# ---------------------------------------------------------
# 2) Fonctions classiques
# ---------------------------------------------------------
work_type_hierarchy <- c("Labour", "TCS Lourd", "TCS Léger", "SDCV", "SD", "Pas applicable")
COVER_HARD_LABOUR_TYPES <- c("SD", "SDCV", "Labour", "TCS Léger", "TCS Lourd")
CROP_HARD_LABOUR_TYPES  <- c("Labour", "TCS Léger", "TCS Lourd")

pick_strongest_work_type <- function(ops, stir_refs, hierarchy) {
  w_found <- c()
  for (op in ops) {
    tool_name <- op$mec
    if (is.null(tool_name) || tool_name == "N/A") next
    row_ref <- stir_refs[stir_refs$Tool == tool_name, ]
    if (nrow(row_ref) == 0) next
    w_val <- row_ref$W
    if (!is.na(w_val) && w_val != "") { 
      w_found <- c(w_found, w_val)
    }
  }
  if (length(w_found) == 0) return("Pas applicable")
  for (h in hierarchy) { 
    if (h %in% w_found) return(h)
  }
  return("Pas applicable")
}

safe_as_date <- function(x) {
  if (inherits(x, "Date") || inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(x, origin = "1899-12-30"))
  if (is.character(x)) {
    if (grepl("^\\d+(\\.\\d+)?$", x)) {
      numeric_val <- as.numeric(x)
      return(as.Date(numeric_val, origin = "1899-12-30"))
    }
  }
  if (is.na(x) || x == "" || tolower(x) %in% c("n/a", "na", "-")) return(NA_Date_)
  val <- suppressWarnings(ymd(x))
  if (!is.na(val)) return(val)
  val <- suppressWarnings(dmy(x))
  if (!is.na(val)) return(val)
  val <- tryCatch(as.Date(x), error = function(e) NA_Date_)
  if (!is.na(val)) return(val)
  return(NA_Date_)
}

compute_cover_days <- function(
    previous_harvest_date,
    previous_harvest_residues_destination,
    cover_type,
    cover_sowing_date,
    cover_work_type,
    cover_destruction_date,
    crop_sowing_date,
    crop_work_type,
    crop_harvest_date,
    double_culture
) {
  if (is.character(double_culture)) double_culture <- tolower(double_culture) %in% c("oui", "yes", "true")
  prev_harv <- safe_as_date(previous_harvest_date)
  cover_sow <- safe_as_date(cover_sowing_date)
  cover_dest <- safe_as_date(cover_destruction_date)
  crop_sow <- safe_as_date(crop_sowing_date)
  crop_harv <- safe_as_date(crop_harvest_date)
  if (is.na(cover_dest) && !is.na(crop_sow)) cover_dest <- crop_sow
  total_days_to_cover <- 365
  if (!is.na(crop_harv) && !is.na(prev_harv)) {
    total_days_to_cover <- as.numeric(difftime(crop_harv, prev_harv, units = "days"))
  }
  harvest_cover <- 0
  if ((previous_harvest_residues_destination %in% c("Laissés sur place", "Laissé sur place")) || double_culture)
    harvest_cover <- 21
  cover_cover <- 0
  if (cover_type %in% c("Semé", "Repousses")) {
    if (!is.na(cover_dest) && !is.na(cover_sow))
      cover_cover <- as.numeric(difftime(cover_dest, cover_sow, units = "days"))
    if (!is.na(cover_sow) && !is.na(prev_harv) && cover_sow < prev_harv) {
      cover_cover <- cover_cover - as.numeric(difftime(prev_harv, cover_sow, units = "days"))
    } else {
      if (!is.na(cover_work_type) && cover_work_type %in% COVER_HARD_LABOUR_TYPES)
        cover_cover <- cover_cover - 21
    }
  } else if (cover_type == "Permanent") {
    # Cas particulier
  }
  crop_cover <- 0
  if (!is.na(crop_sow) && !is.na(crop_harv)) {
    crop_cover <- as.numeric(difftime(crop_harv, crop_sow, units = "days"))
    if (!is.na(crop_work_type) && crop_work_type %in% CROP_HARD_LABOUR_TYPES)
      crop_cover <- crop_cover - 21
  }
  if (!is.na(crop_sow) && !is.na(cover_dest)) {
    delta <- as.numeric(difftime(crop_sow, cover_dest, units = "days"))
    if (delta < 0) crop_cover <- crop_cover - delta
  }
  if (!is.na(crop_sow) && !is.na(prev_harv)) {
    delta2 <- as.numeric(difftime(crop_sow, prev_harv, units = "days"))
    if (delta2 < 0) crop_cover <- crop_cover - delta2
  }
  if (cover_type == "Permanent") {
    cover_perm_days <- 0
    if (!is.na(crop_sow) && !is.na(prev_harv))
      cover_perm_days <- as.numeric(difftime(crop_sow, prev_harv, units = "days"))
    return(list(
      harvest = 0,
      cover   = cover_perm_days,
      crop    = total_days_to_cover - cover_perm_days,
      total   = total_days_to_cover,
      days    = total_days_to_cover,
      percentage = ifelse(total_days_to_cover > 0, cover_perm_days / total_days_to_cover, 1)
    ))
  }
  cover_total <- harvest_cover + cover_cover + crop_cover
  if (is.na(total_days_to_cover) || total_days_to_cover < 1) total_days_to_cover <- 365
  percentage <- cover_total / total_days_to_cover
  return(list(
    harvest = harvest_cover,
    cover = cover_cover,
    crop = crop_cover,
    total = cover_total,
    days = total_days_to_cover,
    percentage = percentage
  ))
}

# Fonction pour choisir la profondeur effective
pick_value_or_ref_global <- function(val_op, val_ref) {
  if (is.null(val_op)) return(val_ref)
  if (is.character(val_op)) {
    if (tolower(val_op) %in% c("n/a", "", "na")) return(val_ref)
    numeric_val <- suppressWarnings(as.numeric(val_op))
    if (!is.na(numeric_val) && numeric_val > 0) return(numeric_val) else return(val_ref)
  }
  if (is.numeric(val_op)) {
    if (is.na(val_op) || val_op == 0) return(val_ref)
    return(val_op)
  }
  return(val_ref)
}

# ---------------------------------------------------------
# Fonctions de calcul STIR (inchangées)
# ---------------------------------------------------------
compute_stir_value <- function(stir_operation, stir_refs) {
  pick_value_or_ref <- function(val_op, val_ref) {
    if (is.null(val_op)) return(val_ref)
    if (is.character(val_op)) {
      if (tolower(val_op) %in% c("n/a", "", "na")) return(val_ref)
      numeric_val <- suppressWarnings(as.numeric(val_op))
      if (!is.na(numeric_val) && numeric_val > 0) return(numeric_val) else return(val_ref)
    }
    if (is.numeric(val_op)) {
      if (is.na(val_op) || val_op == 0) return(val_ref)
      return(val_op)
    }
    return(val_ref)
  }
  res <- list(value = 0, unknown_tool = FALSE)
  if (stir_operation$mec %in% c("N/A", "-")) return(res)
  row_ref <- stir_refs[stir_refs$Tool == stir_operation$mec, ]
  if (nrow(row_ref) == 0) { res$unknown_tool <- TRUE; return(res) }
  tt <- row_ref$WorkType
  sp <- row_ref$Soil
  deep <- pick_value_or_ref(stir_operation$deep, row_ref$Deep)
  speed <- pick_value_or_ref(stir_operation$speed, row_ref$Speed)
  deep_inches <- deep / 2.54
  speed_miles <- speed / 1.60934
  stir_value <- (speed_miles / 2) * (tt * 3.25) * deep_inches * sp
  res$value <- stir_value
  return(res)
}

calc_op_value <- function(op, stir_refs) {
  tmp <- compute_stir_value(op, stir_refs)
  if (tmp$unknown_tool) stop("Outil inconnu dans le STIR : '", op$mec, "'")
  return(tmp$value)
}

# ---------------------------------------------------------
# Extraction des opérations de travail du sol (inchangée)
# ---------------------------------------------------------
parse_machine_block <- function(df, start_line, end_line, cols_to_parse, cutoff_date = NA) {
  ops <- list()
  all_lines <- seq(from = start_line, to = end_line)
  chunk_size <- 4
  n_chunks <- floor(length(all_lines) / chunk_size)
  for (i_chunk in seq_len(n_chunks)) {
    idx_start <- start_line + (i_chunk - 1) * 4
    idx_end <- idx_start + 3
    if (idx_end > end_line) break
    lines_this_machine <- idx_start:idx_end
    intervention_date <- safe_as_date(df[[lines_this_machine[2], cols_to_parse[1]]])
    if (!is.na(cutoff_date) && !is.na(intervention_date) && intervention_date > cutoff_date) next
    mec_name <- df[[lines_this_machine[1], cols_to_parse[1] ]]
    if (is.na(mec_name) || mec_name %in% c("N/A", "")) next
    deep_val <- df[[lines_this_machine[3], cols_to_parse[1] ]]
    speed_val <- df[[lines_this_machine[4], cols_to_parse[1] ]]
    deep_num <- ifelse(is.na(deep_val), 0, as.numeric(deep_val))
    speed_num <- ifelse(is.na(speed_val), 0, as.numeric(speed_val))
    one_op <- list(mec = as.character(mec_name), deep = deep_num, speed = speed_num)
    ops <- append(ops, list(one_op))
  }
  return(ops)
}

fix_ops_format <- function(ops) {
  for (i in seq_along(ops)) {
    if (!"mec" %in% names(ops[[i]])) ops[[i]]$mec <- "N/A"
    if (!"deep" %in% names(ops[[i]])) ops[[i]]$deep <- 0
    if (!"speed" %in% names(ops[[i]])) ops[[i]]$speed <- 0
    if (is.na(ops[[i]]$deep) || is.null(ops[[i]]$deep)) ops[[i]]$deep <- 0
    if (is.na(ops[[i]]$speed) || is.null(ops[[i]]$speed)) ops[[i]]$speed <- 0
    if (is.na(ops[[i]]$mec) || ops[[i]]$mec == "") ops[[i]]$mec <- "N/A"
  }
  return(ops)
}

# ---------------------------------------------------------
# Extraction des fertilisations par bloc
# ---------------------------------------------------------
# Blocs organiques : commencent à "Fertilisation organique" et se terminent 2 lignes AVANT "Fertilisation minérale"
parse_fertilization_org <- function(df, col_idx) {
  idx_start <- which(df[[1]] == "Fertilisation organique")
  if (length(idx_start) == 0) return(list())
  start <- idx_start[1] + 1  # début après le titre
  idx_end_marker <- which(df[[1]] == "Fertilisation minérale")
  if (length(idx_end_marker) > 0) {
    end <- idx_end_marker[1] - 2  # 2 lignes avant le marqueur minéral
  } else {
    end <- nrow(df)
  }
  r <- end - start + 2
  num_events <- floor(r / 8)  # 8 lignes par événement organique
  events <- list()
  for (i in 1:num_events) {
    block_start <- start + (i - 1) * 8
    quant <- suppressWarnings(as.numeric(df[[block_start + 1, col_idx]]))
    Nconc <- suppressWarnings(as.numeric(df[[block_start + 2, col_idx]]))
    Pconc <- suppressWarnings(as.numeric(df[[block_start + 3, col_idx]]))
    Kconc <- suppressWarnings(as.numeric(df[[block_start + 4, col_idx]]))
    event <- list(quantity = quant, N = Nconc, P = Pconc, K = Kconc)
    events[[i]] <- event
  }
  return(events)
}

# Blocs minéraux : commencent à "Fertilisation minérale" et se terminent 2 lignes AVANT "Récolte"
parse_fertilization_min <- function(df, col_idx) {
  idx_start <- which(df[[1]] == "Fertilisation minérale")
  if (length(idx_start) == 0) return(list())
  start <- idx_start[1] + 1
  idx_end_marker <- which(df[[1]] == "Récolte")
  if (length(idx_end_marker) > 0) {
    end <- idx_end_marker[1] - 2
  } else {
    end <- nrow(df)
  }
  r <- end - start + 2
  num_events <- floor(r / 6)  # 6 lignes par événement minéral
  events <- list()
  for (i in 1:num_events) {
    block_start <- start + (i - 1) * 6
    quant <- suppressWarnings(as.numeric(df[[block_start + 1, col_idx]]))
    Nconc <- suppressWarnings(as.numeric(df[[block_start + 2, col_idx]]))
    Pconc <- suppressWarnings(as.numeric(df[[block_start + 3, col_idx]]))
    Kconc <- suppressWarnings(as.numeric(df[[block_start + 4, col_idx]]))
    event <- list(quantity = quant, N = Nconc, P = Pconc, K = Kconc)
    events[[i]] <- event
  }
  return(events)
}

# ---------------------------------------------------------
# Extraction des pulvérisations PPP
# ---------------------------------------------------------
# Les pulvérisations se trouvent dans le bloc qui commence à "Données traitements PPP" + 2
# et se termine à la dernière ligne du df. Chaque événement occupe 6 lignes :
# 1. Type, 2. Produit, 3. Code produit, 4. Date, 5. Dose/L, 6. Dose/kg.
parse_pulverisations <- function(df, col_idx) {
  idx_start <- which(df[[1]] == "Données traitements PPP")
  if (length(idx_start) == 0) return(list())
  start <- idx_start[1] + 2  # début 2 lignes après le titre
  end <- nrow(df)
  r <- end - start + 1
  num_events <- floor(r / 6)
  events <- list()
  for (i in 1:num_events) {
    block_start <- start + (i - 1) * 6
    # On se base sur la deuxième ligne du bloc pour le produit
    product <- df[[block_start + 1, col_idx]]
    event <- list(product = product)
    events[[i]] <- event
  }
  return(events)
}

# ---------------------------------------------------------
# 3) Script principal : Boucle sur les fichiers et calcul des indicateurs
# ---------------------------------------------------------
final_data <- data.frame(
  site = character(),
  year = integer(),
  year_study = integer(),
  culture = character(),
  surface = numeric(),
  stir_total = numeric(),
  stir_main = numeric(),
  stir_cover = numeric(),
  stir_total_filtered = numeric(),
  stir_main_filtered = numeric(),
  stir_cover_filtered = numeric(),
  culture_sp_count = numeric(),
  cover_sp_count = numeric(),
  cover_harvest = numeric(),
  cover_cover = numeric(),
  cover_crop = numeric(),
  cover_total = numeric(),
  cover_days = numeric(),
  cover_percentage = numeric(),
  uncovered_days = numeric(),
  percent_uncovered = numeric(),
  type_of_practice = character(),
  years_since_bio_conversion = numeric(),
  distance_inter_rangs = numeric(),
  densite_semis = numeric(),
  NDTill = numeric(),
  NSTill = numeric(),
  fert_org_vol = numeric(),
  fert_min_vol = numeric(),
  fert_total_vol = numeric(),
  fert_org_N = numeric(),
  fert_org_P = numeric(),
  fert_org_K = numeric(),
  fert_min_N = numeric(),
  fert_min_P = numeric(),
  fert_min_K = numeric(),
  fert_total_N = numeric(),
  fert_total_P = numeric(),
  fert_total_K = numeric(),
  fert_org_count = numeric(),
  fert_min_count = numeric(),
  fert_total_count = numeric(),
  PPP_count = numeric(),    # Nombre total de pulvérisations
  PPP_unique = numeric(),   # Nombre de produits uniques
  stringsAsFactors = FALSE
)

columns_to_do <- 4:11
year_map <- c("4" = 2021, "5" = 2022, "6" = 2023, "7" = 2024, "8" = 2022, "9" = 2023, "10" = 2024, "11" = 2025)

mes_fichiers <- list.files(path = "C:/Users/antoi/Desktop/Mémoire/STIR",
                           pattern = "\\.xlsx$|\\.xlsm$", full.names = TRUE, recursive = FALSE)

for (f in mes_fichiers) {
  message("Fichier en cours : ", f)
  df <- read_excel(f, sheet = 3, col_names = FALSE, trim_ws = FALSE)
  
  row_bio_conventionnel <- which(df[[2]] == "Bio ou conventionnel ?")
  row_annee_conversion  <- which(df[[2]] == "Années depuis conversion BIO Parcelle")
  type_of_practice_val <- if(length(row_bio_conventionnel) > 0) df[[row_bio_conventionnel, 3]] else NA
  years_since_bio_val  <- if(length(row_annee_conversion) > 0) suppressWarnings(as.numeric(df[[row_annee_conversion, 3]])) else NA
  
  line_travail_sol_couvert_start <- which(df[[2]] == "Travail de sol 1")
  line_travail_sol_couvert_end   <- which(df[[2]] == "Date de destruction (disparition) du couvert")
  if (length(line_travail_sol_couvert_start) == 0 || length(line_travail_sol_couvert_end) == 0) {
    message("Impossible de trouver le bloc couverts dans : ", f, " => on saute.")
    next
  }
  start_cover_ops <- line_travail_sol_couvert_start[1]
  end_cover_ops <- line_travail_sol_couvert_end[1] - 1
  
  line_distance_intra_rang <- which(df[[2]] == "Distance intra-rang")
  line_ferti_organique <- which(df[[2]] == "Fertilisation organique 1 - type")
  if (length(line_distance_intra_rang) == 0 || length(line_ferti_organique) == 0) {
    message("Impossible de trouver Distance intra-rang / Fertilisation organique dans : ", f, " => on saute.")
    next
  }
  start_soil_ops <- line_distance_intra_rang[1] + 1
  end_soil_ops <- line_ferti_organique[1] - 2
  
  row_double_culture <- if (length(which(df[[2]] == "Double culture (2 cultures qui se suivent)")) > 0)
    which(df[[2]] == "Double culture (2 cultures qui se suivent)") else NA
  row_cover_species <- if (length(which(df[[2]] == "Composition du couvert -> Nombre d'espèces dans le couvert")) > 0)
    which(df[[2]] == "Composition du couvert -> Nombre d'espèces dans le couvert") else NA
  row_distance_inter_rangs <- which(df[[2]] == "Distance inter-rangs")
  row_densite_semis <- which(df[[2]] == "Densité de semis")
  
  for (col_idx in columns_to_do) {
    type_of_practice_val <- if (length(row_bio_conventionnel) > 0) df[[row_bio_conventionnel, col_idx]] else NA
    years_since_bio_val <- if (length(row_annee_conversion) > 0) suppressWarnings(as.numeric(df[[row_annee_conversion, col_idx]])) else NA
    
    if (col_idx %in% 4:7) {
      site_name <- df[[2, 7]]
      year_study <- 2024
    } else {
      site_name <- df[[2, 11]]
      year_study <- 2025
    }
    y_cult <- year_map[as.character(col_idx)]
    
    culture_name <- df[[4, col_idx]]
    surface_val  <- df[[7, col_idx]]
    if (is.na(culture_name) && is.na(surface_val)) next
    
    # Calcul du STIR complet (non filtré)
    main_ops_full <- parse_machine_block(df, start_soil_ops, end_soil_ops, col_idx, cutoff_date = NA)
    cover_ops_full <- parse_machine_block(df, start_cover_ops, end_cover_ops, col_idx, cutoff_date = NA)
    if (length(main_ops_full) == 0) main_ops_full <- list(list(mec = "N/A", deep = 0, speed = 0))
    if (length(cover_ops_full) == 0) cover_ops_full <- list(list(mec = "N/A", deep = 0, speed = 0))
    main_ops_full <- fix_ops_format(main_ops_full)
    cover_ops_full <- fix_ops_format(cover_ops_full)
    stir_main <- sum(sapply(main_ops_full, calc_op_value, stir_refs = stir_data))
    stir_cover <- sum(sapply(cover_ops_full, calc_op_value, stir_refs = stir_data))
    stir_total <- stir_main + stir_cover
    
    # STIR filtré pour colonnes 7 et 11
    if (col_idx %in% c(7, 11)) {
      cutoff_date <- if(col_idx == 7) as.Date("2024-04-05") else as.Date("2025-03-12")
      main_ops_filtered <- parse_machine_block(df, start_soil_ops, end_soil_ops, col_idx, cutoff_date)
      cover_ops_filtered <- parse_machine_block(df, start_cover_ops, end_cover_ops, col_idx, cutoff_date)
      if (length(main_ops_filtered) == 0) main_ops_filtered <- list(list(mec = "N/A", deep = 0, speed = 0))
      if (length(cover_ops_filtered) == 0) cover_ops_filtered <- list(list(mec = "N/A", deep = 0, speed = 0))
      main_ops_filtered <- fix_ops_format(main_ops_filtered)
      cover_ops_filtered <- fix_ops_format(cover_ops_filtered)
      stir_main_filtered <- sum(sapply(main_ops_filtered, calc_op_value, stir_refs = stir_data))
      stir_cover_filtered <- sum(sapply(cover_ops_filtered, calc_op_value, stir_refs = stir_data))
      stir_total_filtered <- stir_main_filtered + stir_cover_filtered
    } else {
      stir_main_filtered <- NA
      stir_cover_filtered <- NA
      stir_total_filtered <- NA
    }
    
    cover_work_type <- pick_strongest_work_type(cover_ops_full, stir_data, work_type_hierarchy)
    crop_work_type <- pick_strongest_work_type(main_ops_full, stir_data, work_type_hierarchy)
    
    culture_sp_count_val <- 0
    cover_sp_count_val <- 0
    if (!is.na(row_double_culture)) {
      tmp <- df[[row_double_culture, col_idx]]
      culture_sp_count_val <- suppressWarnings(as.numeric(tmp))
      if (is.na(culture_sp_count_val)) culture_sp_count_val <- 0
    }
    if (!is.na(row_cover_species)) {
      tmp <- df[[row_cover_species, col_idx]]
      cover_sp_count_val <- suppressWarnings(as.numeric(tmp))
      if (is.na(cover_sp_count_val)) cover_sp_count_val <- 0
    }
    
    distance_inter_rangs_val <- NA
    densite_semis_val <- NA
    if (col_idx %in% c(7, 11)) {
      previous_harvest_date <- safe_as_date(as.numeric(df[[which(df[[2]] == "Date de récolte de la culture") + 1, col_idx]]))
      previous_harvest_residues_destination <- df[[which(df[[2]] == "Devenir des résidus de récolte"), col_idx]]
      cover_type <- df[[which(df[[2]] == "Type de couvert"), col_idx]]
      cover_sowing_date <- safe_as_date(as.numeric(df[[which(df[[2]] == "Date de semis du couvert"), col_idx]]))
      cover_destruction_date <- safe_as_date(as.numeric(df[[which(df[[2]] == "Date de destruction (disparition) du couvert"), col_idx]]))
      crop_sowing_date <- safe_as_date(as.numeric(df[[which(df[[2]] == "Date de semis"), col_idx]]))
      crop_harvest_date <- safe_as_date(as.numeric(df[[which(df[[2]] == "Date de récolte de la culture"), col_idx]]))
      double_culture <- "Non"
      cover_info <- compute_cover_days(
        previous_harvest_date = previous_harvest_date,
        previous_harvest_residues_destination = previous_harvest_residues_destination,
        cover_type = cover_type,
        cover_sowing_date = cover_sowing_date,
        cover_work_type = cover_work_type,
        cover_destruction_date = cover_destruction_date,
        crop_sowing_date = crop_sowing_date,
        crop_work_type = crop_work_type,
        crop_harvest_date = crop_harvest_date,
        double_culture = double_culture
      )
      cover_harvest <- cover_info$harvest
      cover_cover <- cover_info$cover
      cover_crop <- cover_info$crop
      cover_total <- cover_info$total
      cover_days <- cover_info$days
      cover_percentage <- cover_info$percentage
      uncovered_days <- cover_days - cover_total
      percent_uncovered <- ifelse(cover_days > 0, uncovered_days / cover_days, NA)
      if (length(row_distance_inter_rangs) > 0)
        distance_inter_rangs_val <- suppressWarnings(as.numeric(df[[row_distance_inter_rangs, col_idx]]))
      if (length(row_densite_semis) > 0)
        densite_semis_val <- suppressWarnings(as.numeric(df[[row_densite_semis, col_idx]]))
    } else {
      cover_harvest <- NA
      cover_cover <- NA
      cover_crop <- NA
      cover_total <- NA
      cover_days <- NA
      cover_percentage <- NA
      uncovered_days <- NA
      percent_uncovered <- NA
    }
    
    ### Début indicateurs fertilisation ###
    org_events <- parse_fertilization_org(df, col_idx)
    min_events <- parse_fertilization_min(df, col_idx)
    
    if (length(org_events) > 0) {
      fert_org_vol <- sum(sapply(org_events, function(e) ifelse(is.na(e$quantity), 0, e$quantity)), na.rm = TRUE)
      fert_org_N <- sum(sapply(org_events, function(e) {
        if(!is.na(e$quantity) & !is.na(e$N)) e$quantity * e$N else 0
      }), na.rm = TRUE)
      fert_org_P <- sum(sapply(org_events, function(e) {
        if(!is.na(e$quantity) & !is.na(e$P)) e$quantity * e$P else 0
      }), na.rm = TRUE)
      fert_org_K <- sum(sapply(org_events, function(e) {
        if(!is.na(e$quantity) & !is.na(e$K)) e$quantity * e$K else 0
      }), na.rm = TRUE)
      fert_org_count <- sum(sapply(org_events, function(e) {
        if(!is.na(e$quantity) & e$quantity > 0) 1 else 0
      }), na.rm = TRUE)
    } else {
      fert_org_vol <- NA; fert_org_N <- NA; fert_org_P <- NA; fert_org_K <- NA; fert_org_count <- 0
    }
    
    if (length(min_events) > 0) {
      fert_min_vol <- sum(sapply(min_events, function(e) ifelse(is.na(e$quantity), 0, e$quantity)), na.rm = TRUE)
      fert_min_N <- sum(sapply(min_events, function(e) {
        if(!is.na(e$quantity) & !is.na(e$N)) e$quantity * (e$N/1000) else 0
      }), na.rm = TRUE)
      fert_min_P <- sum(sapply(min_events, function(e) {
        if(!is.na(e$quantity) & !is.na(e$P)) e$quantity * (e$P/1000) else 0
      }), na.rm = TRUE)
      fert_min_K <- sum(sapply(min_events, function(e) {
        if(!is.na(e$quantity) & !is.na(e$K)) e$quantity * (e$K/1000) else 0
      }), na.rm = TRUE)
      fert_min_count <- sum(sapply(min_events, function(e) {
        if(!is.na(e$quantity) & e$quantity > 0) 1 else 0
      }), na.rm = TRUE)
    } else {
      fert_min_vol <- NA; fert_min_N <- NA; fert_min_P <- NA; fert_min_K <- NA; fert_min_count <- 0
    }
    
    fert_total_vol <- (ifelse(is.na(fert_org_vol), 0, fert_org_vol)) + (ifelse(is.na(fert_min_vol), 0, fert_min_vol))
    fert_total_N <- (ifelse(is.na(fert_org_N), 0, fert_org_N)) + (ifelse(is.na(fert_min_N), 0, fert_min_N))
    fert_total_P <- (ifelse(is.na(fert_org_P), 0, fert_org_P)) + (ifelse(is.na(fert_min_P), 0, fert_min_P))
    fert_total_K <- (ifelse(is.na(fert_org_K), 0, fert_org_K)) + (ifelse(is.na(fert_min_K), 0, fert_min_K))
    fert_total_count <- fert_org_count + fert_min_count
    ### Fin indicateurs fertilisation ###
    
    ### Début indicateur tillage combiné (déjà calculé)
    combined_ops <- c(main_ops_full, cover_ops_full)
    ndTill <- 0; nsTill <- 0
    for (op in combined_ops) {
      row_ref <- stir_data[stir_data$Tool == op$mec, ]
      if (nrow(row_ref) == 0) { effective_depth <- op$deep } else {
        effective_depth <- pick_value_or_ref_global(op$deep, row_ref$Deep)
      }
      if (effective_depth >= 15) ndTill <- ndTill + 1 else nsTill <- nsTill + 1
    }
    ### Fin indicateur tillage ###
    
    ### Début indicateurs pulvérisations PPP ###
    # Le bloc de pulvérisation commence à "Données traitements PPP" + 2 et se poursuit jusqu'à la dernière ligne.
    ppp_start <- which(df[[1]] == "Données traitements PPP")
    if (length(ppp_start) == 0) {
      ppp_count <- NA
      ppp_unique <- NA
    } else {
      ppp_start <- ppp_start[1] + 2
      total_ppp_rows <- nrow(df) - ppp_start + 1
      num_ppp_events <- floor(total_ppp_rows / 6)
      ppp_count <- 0
      product_list <- c()
      for (i_ppp in 1:num_ppp_events) {
        block_start <- ppp_start + (i_ppp - 1) * 6
        # La 2ème ligne du bloc correspond au produit
        product_val <- df[[block_start + 1, col_idx]]
        if (!is.na(product_val) && product_val != "") {
          ppp_count <- ppp_count + 1
          product_list <- c(product_list, product_val)
        }
      }
      ppp_unique <- length(unique(product_list))
    }
    ### Fin indicateurs pulvérisations PPP ###
    
    df_line <- data.frame(
      site = ifelse(is.na(site_name), "", as.character(site_name)),
      year = y_cult,
      year_study = year_study,
      culture = ifelse(is.na(culture_name), "", as.character(culture_name)),
      surface = ifelse(is.na(surface_val), 0, as.numeric(surface_val)),
      stir_total = stir_total,
      stir_main = stir_main,
      stir_cover = stir_cover,
      stir_total_filtered = stir_total_filtered,
      stir_main_filtered = stir_main_filtered,
      stir_cover_filtered = stir_cover_filtered,
      culture_sp_count = culture_sp_count_val,
      cover_sp_count = cover_sp_count_val,
      cover_harvest = cover_harvest,
      cover_cover = cover_cover,
      cover_crop = cover_crop,
      cover_total = cover_total,
      cover_days = cover_days,
      cover_percentage = cover_percentage,
      uncovered_days = uncovered_days,
      percent_uncovered = percent_uncovered,
      type_of_practice = ifelse(is.na(type_of_practice_val), "", as.character(type_of_practice_val)),
      years_since_bio_conversion = ifelse(is.na(years_since_bio_val), 0, years_since_bio_val),
      distance_inter_rangs = distance_inter_rangs_val,
      densite_semis = densite_semis_val,
      NDTill = ndTill,
      NSTill = nsTill,
      fert_org_vol = fert_org_vol,
      fert_min_vol = fert_min_vol,
      fert_total_vol = fert_total_vol,
      fert_org_N = fert_org_N,
      fert_org_P = fert_org_P,
      fert_org_K = fert_org_K,
      fert_min_N = fert_min_N,
      fert_min_P = fert_min_P,
      fert_min_K = fert_min_K,
      fert_total_N = fert_total_N,
      fert_total_P = fert_total_P,
      fert_total_K = fert_total_K,
      fert_org_count = fert_org_count,
      fert_min_count = fert_min_count,
      fert_total_count = fert_total_count,
      PPP_count = ppp_count,
      PPP_unique = ppp_unique,
      stringsAsFactors = FALSE
    )
    
    final_data <- rbind(final_data, df_line)
  }
}

print(final_data)

summary_data <- final_data %>%
  group_by(site) %>%
  summarize(
    mean_culture_sp = mean(culture_sp_count, na.rm = TRUE),
    mean_cover_sp = mean(cover_sp_count, na.rm = TRUE),
    mean_surface = mean(surface, na.rm = TRUE)
  )

print(summary_data)

write.csv(final_data, "resultats_STIR_format_long.csv", row.names = FALSE)


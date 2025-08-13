
library(dplyr)
library(mice)
datasets_mice <- c("boys","employee", "fdd", "fdgs", "mammalsleep", 
                   "nhanes", "nhanes2", "popmis", "potthoffroy",  
                   "selfreport", 'tbc',  'toenail',  "toenail2", "walking",  
                   'windspeed')
library(missMDA)
datasets_missMDA <- c("gene", "geno", "orange", "ozone", "snorena", "vnf")
library(naniar)
datasets_naniar <- c("oceanbuoys", "pedestrian", "riskfactors")
library(VIM)
datasets_VIM <- c("Animals_na", "bcancer", "brittleness", "chorizonDL", "colic",
                  "collisions", "diabetes", "food", "pulplignin", "SBS5242", 
                  "sleep", "tao", "toydataMiss", "wine")
library(Amelia)
datasets_Amelia <- c("africa", "freetrade")
library(mixgb)
datasets_mixgb <- c("nhanes3", "nhanes3_newborn")
library(pcaMethods)
datasets_pcaMethods <- c("helix", "metaboliteDataComplete")
library(mlbench)
datasets_mlbench <- c("BostonHousing", "BreastCancer", "DNA", "Glass", "HouseVotes84",
                      "Ionosphere", "LetterRecognition", "Ozone", "PimaIndiansDiabetes",
                      "Satellite", "Servo", "Shuttle", "Sonar", "Soybean", "Vehicle",
                      "Vowel", "Zoo")
library(faraway)
datasets_faraway <- c("aatemp", "abrasion", "aflatoxin", "africa", "airpass", "alfalfa", "amlxray", "anaesthetic", "babyfood", "beetle",
                      "bliss", "breaking", "broccoli", "butterfat", "cathedral", "cheddar", "chicago", "chmiss", "choccake",
                      "chredlin", "clot", "cmob", "cns", "coagulation", "composite", "cornnit", "corrosion", "cpd", "crawl", "ctsib",
                      "death", "debt", "denim", "diabetes", "dicentric", "divusa", "drugpsy", "dvisits", "eco", "eggprod", "eggs",
                      "epilepsy", "esdcomp", "exa", "eyegrade", "fat", "femsmoke", "fortune", "fpe", "fruitfly", "gala",
                      "galamiss", "gammaray", "gavote", "globwarm", "haireye", "happy", "hemoglobin", "hips", "hormone", "hprice",
                      "hsb", "infmort", "insulgas", "irrigation", "jsp", "kanga", "lawn", "leafblotch", "leafburn", "mammalsleep",
                      "manilius", "meatspec", "melanoma", "motorins", "neighbor", "nels88", "nepali", "nes96", "newhamp", "oatvar",
                      "odor", "ohio", "orings", "ozone", "parstum", "peanut", "penicillin", "phbirths", "pima", "pipeline", "pneumo",
                      "potuse", "prostate", "psid", "pulp", "punting", "pvc", "pyrimidines", "rabbit", "ratdrink", "rats", "resceram",
                      "salmonella", "sat", "savings", "seatpos", "seeds", "semicond", "sexab", "sexfun", "snail", "solder", "sono",
                      "soybean", "spector", "speedo", "star", "stat500", "stepping", "strongx", "suicide", "teengamb", "toenail",
                      "troutegg", "truck", "turtle", "tvdoctor", "twins", "uncviet", "uswages", "vision", "wafer", "wavesolder",
                      "wbca", "wcgs", "weldstrength", "wfat", "wheat", "worldcup")


datasets_vec <- c(datasets_mice, datasets_missMDA, datasets_naniar, datasets_VIM, 
                  datasets_Amelia, datasets_mixgb, datasets_pcaMethods, 
                  datasets_mlbench, datasets_faraway)


packages <- list(mice = datasets_mice, missMDA = datasets_missMDA, 
                 naniar = datasets_naniar, VIM = datasets_VIM, 
                 Amelia = datasets_Amelia, mixgb = datasets_mixgb, 
                 pcaMethods = datasets_pcaMethods, mlbench = datasets_mlbench,
                 faraway = datasets_faraway)



pckg_datasets <- lapply(datasets_vec, function(ith_dat) {
  print(ith_dat)
  
  if(exists(ith_dat)) {
    dat <- get(ith_dat)
  } else {
    data(list = ith_dat, envir = environment())
    dat <- get(ith_dat)
  }
  
  pckg <- names(Filter(function(v) ith_dat %in% v, packages))
  
  n_cat <- sapply(dat, is.factor)
  
  data.frame(name = ith_dat, n_row = nrow(dat), n_col = ncol(dat), 
             categorical = any(sapply(dat, is.factor)), n_cat = sum(sapply(dat, is.factor)), complete = !any(is.na(dat)),
             miss_ratio = mean(is.na(dat)), n_missing = sum(is.na(dat)), source = pckg)
  
}) %>%  bind_rows()


dat_paths <- list.files("./data/datasets/new_datasets/Datasets_repository/", full.names = TRUE)

Datasets_repo <- lapply(dat_paths, function(ith_path) {
  dat <- readRDS(ith_path)
  
  name <- basename(ith_path)
  
  n_cat <- sapply(dat, is.factor)
  
  data.frame(name = name, n_row = nrow(dat), n_col = ncol(dat), 
             categorical = any(sapply(dat, is.factor)), n_cat = sum(sapply(dat, is.factor)), complete = !any(is.na(dat)),
             miss_ratio = mean(is.na(dat)), n_missing = sum(is.na(dat)), source =  "Datasets_repository")
  
}) %>%  bind_rows()


datasets_full <- rbind(pckg_datasets, Datasets_repo) %>%
  distinct(across(-source), .keep_all = TRUE) %>% 
  arrange(complete, categorical, n_col)

write.csv2(datasets_full, "datasets.csv")


datasets_full %>% 
  filter(source == "faraway", categorical, n_row > 50, n_col > 6) 


##### numerical complete

saveRDS(meatspec, "./data/datasets/complete_backup/only_num/meatspec.RDS")
saveRDS(pyrimidines, "./data/datasets/complete_backup/only_num/pyrimidines.RDS")
saveRDS(fat, "./data/datasets/complete_backup/only_num/fat.RDS")
saveRDS(fpe, "./data/datasets/complete_backup/only_num/fpe.RDS")
saveRDS(seatpos, "./data/datasets/complete_backup/only_num/seatpos.RDS")
saveRDS(sat, "./data/datasets/complete_backup/only_num/sat.RDS")
saveRDS(divusa, "./data/datasets/complete_backup/only_num/divusa.RDS")
saveRDS(chicago, "./data/datasets/complete_backup/only_num/chicago.RDS")
saveRDS(windspeed, "./data/datasets/complete_backup/only_num/windspeed.RDS")
saveRDS(savings, "./data/datasets/complete_backup/only_num/savings.RDS")
saveRDS(stat500, "./data/datasets/complete_backup/only_num/stat500.RDS")
saveRDS(leafburn, "./data/datasets/complete_backup/only_num/leafburn.RDS")
saveRDS(eco, "./data/datasets/complete_backup/only_num/eco.RDS")
saveRDS(cheddar, "./data/datasets/complete_backup/only_num/cheddar.RDS")
saveRDS(tvdoctor, "./data/datasets/complete_backup/only_num/tvdoctor.RDS")
saveRDS(star, "./data/datasets/complete_backup/only_num/star.RDS")
saveRDS(exa, "./data/datasets/complete_backup/only_num/exa.RDS")

##########################

cols_to_num_factors <- function(dat, cols_vec = NULL) {
  
  dat <- dat %>%  mutate(across(which(sapply(dat, is.character)), as.factor))
  
  if(is.null(cols_vec))
    cols_vec <- which(sapply(dat, is.factor))
  
  dat[, cols_vec] <- lapply(cols_vec, function(ith_factor) {
    as.factor(as.numeric(dat[, ith_factor]))
  })
  
  dat 
}

##### categorical complete

choccake %>% cols_to_num_factors(1:2) %>%  
  saveRDS("./data/datasets/complete_backup/categorical_as_factor/choccake.RDS")
  
nels88 %>% cols_to_num_factors(c(1, 2, 4)) %>%  
  saveRDS("./data/datasets/complete_backup/categorical_as_factor/nels88.RDS")

solder %>%  
  saveRDS("./data/datasets/complete_backup/categorical_as_factor/solder.RDS")

chredlin %>% cols_to_num_factors(7) %>%  
  saveRDS("./data/datasets/complete_backup/categorical_as_factor/chredlin.RDS")

hips %>% cols_to_num_factors(5:7) %>%  
  saveRDS("./data/datasets/complete_backup/categorical_as_factor/hips.RDS")

worldcup %>% cols_to_num_factors(1:2) %>%  
  saveRDS("./data/datasets/complete_backup/categorical_as_factor/worldcup.RDS")

data(PimaIndiansDiabetes)
PimaIndiansDiabetes %>% cols_to_num_factors(9) %>%  
  saveRDS("./data/datasets/complete_backup/categorical_as_factor/PimaIndiansDiabetes.RDS")

uswages %>% cols_to_num_factors(4:10) %>%  
  saveRDS("./data/datasets/complete_backup/categorical_as_factor/uswages.RDS")

############ numerical incomplete


saveRDS(Animals_na, "./data/datasets/incomplete_backup/Animals_na.RDS")
saveRDS(employee, "./data/datasets/incomplete_backup/employee.RDS")
saveRDS(chorizonDL, "./data/datasets/incomplete_backup/chorizonDL.RDS")

tmp <- pulplignin
tmp$Observation <- NULL
saveRDS(tmp, "./data/datasets/incomplete_backup/pulplignin.RDS")

saveRDS(globwarm, "./data/datasets/incomplete_backup/globwarm.RDS")
saveRDS(mammalsleep, "./data/datasets/incomplete_backup/mammalsleep.RDS")
saveRDS(tao, "./data/datasets/incomplete_backup/tao.RDS")
saveRDS(oceanbuoys, "./data/datasets/incomplete_backup/oceanbuoys.RDS")

d <- readRDS("./data/datasets/new_datasets/Datasets_repository/Diabetes Missing Data.RDS")
saveRDS(d, "./data/datasets/incomplete_backup/diabetes.RDS")

saveRDS(popmis[, -6], "./data/datasets/incomplete_backup/popmis.RDS")


########## categorical incomplete

sapply(kanga, is.factor)

# kanga %>% cols_to_num_factors(1:2) %>%  # only one column with > 20 missing values, also no categorical column with missing values
#   saveRDS("./data/datasets/incomplete_backup/categorical/kanga.RDS")

data(Ozone)
Ozone %>% cols_to_num_factors() %>% 
  saveRDS("./data/datasets/incomplete_backup/categorical/Ozone.RDS")

tbc %>% cols_to_num_factors() %>% 
  select(where(~ n_distinct(., na.rm = TRUE) > 1)) %>% 
  mutate(ao = as.factor(ao)) %>% 
  # what about "ID"?
  select(-id) %>%
  saveRDS("./data/datasets/incomplete_backup/categorical/tbc.RDS")

data(snorena) 
snorena %>%  cols_to_num_factors() %>% 
  saveRDS("./data/datasets/incomplete_backup/categorical/snorena.RDS")

walking %>% cols_to_num_factors() %>% 
  saveRDS("./data/datasets/incomplete_backup/categorical/walking.RDS")

colic %>%  cols_to_num_factors() %>% 
  select(where(~ n_distinct(., na.rm = TRUE) > 1)) %>% 
  select(- hospitalID) %>% 
  saveRDS("./data/datasets/incomplete_backup/categorical/colic.RDS")

data(Soybean)
Soybean %>% cols_to_num_factors() %>%
  mutate(across(where(~ n_distinct(., na.rm = TRUE) > 1), as.factor)) %>% 
  saveRDS("./data/datasets/incomplete_backup/categorical/soybean.RDS")

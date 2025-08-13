library(dplyr)

############
# ACCEPTED
############


### Debt

data <- faraway::debt

data <- data %>% 
  mutate(manage = ifelse(manage < 2, 2, manage))

data <- data %>% 
  mutate(incomegp = as.factor(incomegp),
         house = as.factor(house),
         singpar = as.factor(singpar),
         agegp = as.factor(agegp),
         bankacc = as.factor(bankacc),
         bsocacc = as.factor(bsocacc),
         manage = as.factor(manage),
         ccarduse = as.factor(ccarduse),
         cigbuy = as.factor(cigbuy),
         xmasbuy = as.factor(xmasbuy)
  )

saveRDS(data, "data/datasets/incomplete_backup/categorical/debt.RDS")


### boys

data <- mice::boys

data <- data %>% 
  mutate(gen = factor(gen, ordered=F),
         phb = factor(phb, ordered=F)
  ) %>% 
  select(-bmi)

data$reg <- as.factor(as.numeric(as.factor(data$reg)))
data$gen <- as.factor(as.numeric(as.factor(data$gen)))
data$phb <- as.factor(as.numeric(as.factor(data$phb)))

saveRDS(data, "data/datasets/incomplete_backup/categorical/boys.RDS")


### VNF

data(vnf, package="missMDA")

data <- vnf

saveRDS(data, "data/datasets/incomplete_backup/categorical/vnf.RDS")


### SELFREPORT

data <- mice::selfreport %>% 
  select(-id, -pop, -prg, -bm, -br)

data$src <- as.factor(as.numeric(as.factor(data$src)))
data$sex <- as.factor(as.numeric(as.factor(data$sex)))
data$edu <- as.factor(as.numeric(as.factor(data$edu)))
data$etn <- as.factor(as.numeric(as.factor(data$etn)))
data$web <- as.factor(as.numeric(as.factor(data$web)))

saveRDS(data, "data/datasets/incomplete_backup/categorical/selfreport.RDS")


### housevotes84

data("HouseVotes84", package = "mlbench")
data <- HouseVotes84

data <- data %>% 
  mutate_all(function(x) as.factor(as.numeric(as.factor(x))))

saveRDS(data, "data/datasets/incomplete_backup/categorical/housevotes84.RDS")


### Soybean

data("Soybean", package = "mlbench")
data <- Soybean
apply(data, 2, function(x) sum(is.na(x))) / nrow(data) * 100

data$Class <- as.factor(as.numeric(as.factor(data$Class)))
saveRDS(data, "data/datasets/incomplete_backup/categorical/soybean.RDS")




###########
# NOT SURE
###########


### Planets

data <- read.csv("https://raw.githubusercontent.com/YBI-Foundation/Dataset/master/Planets.csv")


table(data$method, data$number)

data <- data %>% 
  mutate(
    method = ifelse(method %in% c("Astrometry", "Eclipse Timing Variations",
                                  "Orbital Brightness Modulation", 
                                  "Pulsar Timing", "Pulsation Timing Variations"),
                    "Other", method)
  ) %>% 
  mutate(
    method = ifelse(method =="Transit Timing Variations", "Transit", method)
  )

data$method <- as.factor(as.numeric(as.factor(data$method)))
data$number <- as.factor(as.numeric(as.factor(data$number)))

saveRDS(data, "data/datasets/incomplete_backup/categorical/planets.RDS")



### NHANES3-newborn

data <- mixgb::nhanes3_newborn %>% 
  select(-DMARETHN) %>% 
  mutate(HYD1 = ifelse(HYD1 > 4, 4, HYD1)) %>% 
  mutate(
    HYD1 = as.factor(HYD1)
  )

saveRDS(data, "data/datasets/incomplete_backup/categorical/nhanes3Newborn.RDS")





#############
# ARCHIVE
#############

### Melbourne Housing Market 

data <- read.csv("https://raw.githubusercontent.com/YBI-Foundation/Dataset/master/Melbourne%20Housing%20Market.csv") %>% 
  select(-c(Address, Suburb, SellerG, Date, Postcode)) %>% 
  mutate(
    Rooms = ifelse(Rooms > 6, 6, Rooms),
    Bedroom2 = ifelse(Bedroom2 > 6, 6, Bedroom2),
    Bathroom = ifelse(Bathroom > 5, 5, Bathroom),
    Car = ifelse(Car > 7, 7, Car),
  )

# : libs ----

source(file = "D:/Dissertation/libs.R")

# : functions ----


# : visuals ----

windowsFonts("Arial" = windowsFont("Arial"))

## Clean theme: 

theme_set(theme_bw())
theme_update(
  # Text
  text = element_text(family = "Helvetica"),
  # Axes
  axis.title = element_text(face = "bold", size = 8),
  axis.line = element_line(color="black", size = .25),
  axis.text.y = element_text(color="black", size = 6),
  axis.text.x = element_text(color="black", size = 6),
  axis.ticks = element_line(color="black", size = .25),
  # Title
  plot.title = element_text(face = "italic", size = 8),
  # Tags
  plot.tag = element_text(size = 10),
  # Legends
  legend.title =  element_blank(),
  legend.position = "bottom",
  legend.text = element_text(size = 5),
  legend.key.size = unit(0.4, "lines"),
  legend.box.spacing = unit(0, "cm"),
  legend.background = element_rect(fill = "gray95"),
  # Panel
  panel.border = element_blank(), 
  panel.background = element_rect(fill="gray98"),
  panel.grid.major = element_line(color="grey90", size = .25),
  panel.grid.minor = element_line(color="grey90", size = .15)
  
)

loadfonts(device = "win")


## Margin sizes accoridng to Science formatting requirements
# Widths

# 1 column = 5.5 cm / 2.25 in
cw1 <- 2.25
# 2 column = 12 cm / 4.75 in
cw2 <- 4.75
# 3 column = 18.3 cm / 7.25 in
cw3 <- 7.25


# : WD ----

setwd(dirname(rstudioapi::getSourceEditorContext()$path))


# : 0.0 Data ----

## 0.1 Prep ---- 


# ### ::: 1. Data
# data <- fread("D:/Dissertation/AquaSync/EZG_pointcount.csv")
# data <- data[, -c("code_arbo", "path")]
# 
# ### :: Site names per catchment
# sp.data <- fread("D:/Dissertation/AquaSync/study.points with_catchments.csv")
# sp.data[, c("code_arbo", "layer", "path"):= NULL] 
# sp.data[, .N, site.id]  # Anzahl Nennungen = Anzahl Vorkommen in unterscheidlichen einzugsgebieten.
# sp.data[, .N, ogc_fid]  #  Anzahl Punkte pro ogc_fid (unique identifier für catchments), BSP: ogc_fid 4 sollte 8 Messpunkte enthalten (stimmt, test)

## :: Get all aquasync meta-analytical analytes data
aqua.data <- fread("data/chemical_analytes_with-CAS.csv")
setnames(aqua.data, "CAS", "cas")
aqua.data[cas == "", cas := NA]

## :: Get chemicals that were ultimately modelled
mod.chems <- as.data.table(read_xlsx("data/ALL_CONTAMINANTS_modelled.xlsx"))
mod.chems[, ...1 := NULL]

## :: Get all BASIN_ATLAS attributes
basin.atlas <- fread("data/BasinATLAS_ATTRIBUTES.csv")
str(basin.atlas)   # NEEDS bit64 package othersie not readable as INTEGER! (overlfowing)

## :: Get slimmed atlas for predictions
atlas.slim <- readRDS("data/PREDICTIONS/atlas.slim.rds")


### ::: COLOR SCALES
col.bl.rd <- c("#0571b0", "#92c5de", "#d4ebd3", "#f4a582", "#ca0020")
col.br.gr <- c("#a6611a", "#dfc27d", "#e9e9e9", "#80cdc1", "#018571")



## 0.2 Monitoring EU ----

eu.data <- readRDS("data/MONITORING/EU Monitoring processed.rds")
spat.att <- readRDS("data/MONITORING/EU spat att.rds")



## 0.3 Monitoring US ----

us.data <- readRDS("data/MONITORING/US Monitoring processed 2025.rds")   # US data 2025 updated!!!
spat.att.us <- readRDS("data/MONITORING/US spat att 2025.rds")           # US data 2025 updated!!!


## 0.4 Atlas data ----

## :: 1.0 ATLAS data
eu.atlas <- fread("data/MONITORING/eu.sites.ATLAS.csv")
us.atlas <- fread("data/MONITORING/us.sites.ATLAS 2025.csv") # US data 2025 updated!!!

## Reduce sets to interesting predictors
## Interesting attributes

# dis_m3_  # Natural Discharge
# run_mm_  # Land surface runoff
# ele_mt_  # elevation
# slp_dg_  # slope
# sgr_dk   # stream gradient
# 
# tmp_dc_   # air temperature
# pre_mm_   # precipitation
# crp_pc_   # corpland ectent
# ero_kh_   # soil erosion
# ppd_pk_   # population density
# urb_pc_   # urban extent
# rdd_mk_   # road density

int.cols <- c("dis_m3_", # Natural Discharge
              "run_mm_",  # Land surface runoff
              "ria_ha_usu",  # River area in total watershed upstream of sub-basin pour point
              "ele_mt_", # elevation
              "slp_dg_", # slope
              "sgr_dk",  # stream gradient
              "tmp_dc_",  # air temperature
              "pre_mm_",  # precipitation
              "crp_pc_",   # corpland ectent
              "ero_kh_",  # soil erosion
              "ppd_pk_", # population density
              "urb_pc_",  # urban extent
              "rdd_mk_")  # road density)


## Reductions - EU
eu.atlas <- eu.atlas[, .SD, .SDcols = c("site.id", "lat", "lon", grep(paste0(int.cols, collapse = "|"), colnames(eu.atlas), value = T))]

# Further reductions - base don previous analyses - check out AquaSync_OLDCODE if interested (search for atlas)
eu.atlas <- eu.atlas[, -c("ele_mt_smn", "ele_mt_smx")]
eu.atlas <- eu.atlas[, -c("tmp_dc_uyr", "tmp_dc_s01", paste0("tmp_dc_s0", 1:9), "tmp_dc_s10", "tmp_dc_s11", "tmp_dc_s12")]
eu.atlas <- eu.atlas[, -c(paste0("pre_mm_s0", 1:9), "pre_mm_s10", "pre_mm_s11", "pre_mm_s12")]
# final red
remv <- c("dis_m3_pmn", "dis_m3_pmx", "ele_mt_uav", "tmp_dc_smn")
eu.atlas <-eu.atlas[, .SD, .SDcols =!remv]
# create spatial identifier
eu.atlas[, spat := paste0(lat, lon)]   
eu.atlas[, site.id := NULL]
eu.atlas[, lat := NULL]
eu.atlas[, lon := NULL]

## UPDATE 2025 - us.atlas no longer requires processing (pre processed in WQP testing.R)

## 0.5 HydroRIVER ----

### 0.5.1 EU ----
eu.river <- fread("data/MONITORING/eu.sites.HYDRORIVER.csv") # EU data 2025 updated!!!

# remove duplicates
eu.river[duplicated(site.id)] 
setorder(eu.river, -"ORD_STRA")
eu.river <- eu.river[!duplicated(site.id)]

# reduction
eu.river <- eu.river[, -c( "n",  "distance", "feature_x", "feature_y", "nearest_x", "nearest_y", "HYBAS_L12")]

# create spat id
eu.river[ , spat := paste0(lat, lon)]
eu.river <- eu.river[!duplicated(spat)]
eu.river[, .N]             # 17837
eu.data[!duplicated(spat)][spat %in% eu.river$spat]  # full coverage (17333 in eu.data)
eu.river <- eu.river[, -c("site.id", "lon", "lat", "HYRIV_ID")]
colnames(eu.river) <-c("catch.sqkm", "disch", "strahl.hydro", "spat")

# dropping cum.len and strahl derived from continental datasets for now as they cannot be used in a global prediction model
eu.data <-eu.data[, -c("cum.len", "strahl")]

# merge
eu.data <- merge(eu.data,
                 eu.river,
                 by = "spat",
                 all.x = T)
gc()


### 0.5.2 US ----

us.river <- fread("data/MONITORING/us.sites.HYDRORIVER 2025.csv") # US data 2025 updated!!!

### NO LONGER REQUIRES PROCESSING in 2025, see pre-processed data in WQP_testing

# merge
us.data <- merge(us.data,
                 us.river,
                 by = "spat",
                 all.x = T)



## create combined atlas with proper order

# order
setcolorder(us.atlas, colnames(eu.atlas))

atlas <- rbind(eu.atlas,
               us.atlas)

# Remove duplicates
atlas <- atlas[!duplicated(spat)]



# 1.0 Prediction data -----

##  :: 1.0 Global Prediction points!
pnt.glob <- fread("data/PREDICTIONS/pnt_GLOBAL.csv")
str(pnt.glob)

## :: EXPL
# HYRIV_ID ID of the stream segment
# LENGTH_KM length of stream segment
# DIST_DN_KM distance to most downstram pixel, e.g. ocean
# DIST_UP_KM most distant point in upstream 
# CATCH_SKM catch area of only the current reach
# UPLAND_SKM  actual total watershed size (upstream catchment) of all reaches in sqkm
# ENDORHEIC endorheic basin
# DIS_AV_CMS average discharge in cubmic meters per second
# ORD_STRA strahler order
# ORD_CLAS identifier of mainstem versus tributaries etc
# ORD_FLOW logarithmic class of the river size
# HYBAS_L12 hydrobasin level 12 ID

# We only need CATCH_SKM, DIS_AV_CMS, ORD_STRA, HYBAS_L12
pnt.glob <- pnt.glob[, .(HYRIV_ID, UPLAND_SKM, DIS_AV_CMS, ORD_STRA, HYBAS_L12)]

# CONVERT TO INTEGER 64bit - otherwise overflow
pnt.glob[, HYBAS_L12 := as.integer64(HYBAS_L12)]
pnt.glob[, .N, HYBAS_L12]
summary(pnt.glob)


## :: 2.0 Global points in LAKES
pnt.lake <- fread("data/PREDICTIONS/pnt_LAKES.csv") 
str(pnt.lake)
# no cleanup necessary - used for filtering

## :: 3.0 Sptial predictors!

# : 3.1 ELEVATION / KMEAN / RMEAN / LULC / MINING / PH / SOIL
elev <- readRDS("data/PREDICTIONS/ZH_elevation.rds")
kmean <- readRDS("data/PREDICTIONS/ZH_kmean.rds")
lulc <- readRDS("data/PREDICTIONS/ZH_LULC.rds")
rmean <- readRDS("data/PREDICTIONS/ZH_rmean.rds")
min.ph.lith <- readRDS("data/PREDICTIONS/ZH_min-lith-ph.rds")

# Minor adjustments
setnames(min.ph.lith, "ph10_mean", "ph15_mean")
min.ph.lith[is.na(min_mean), min_mean := 0]    # absence of mining = 0
min.ph.lith[, lit_majori := as.character(lit_majori)] 

## merge to combined set
pnt.glob <- merge(pnt.glob, elev, by = "HYRIV_ID", all.x = T)
pnt.glob <- merge(pnt.glob, kmean[, -c("region")], by = "HYRIV_ID", all.x = T)
pnt.glob <- merge(pnt.glob, lulc[, -c("region")], by = "HYRIV_ID", all.x = T)
pnt.glob <- merge(pnt.glob, rmean[, -c("region")], by = "HYRIV_ID", all.x = T)
pnt.glob <- merge(pnt.glob, min.ph.lith[, -c("region")], by = "HYRIV_ID", all.x = T)

## Missing data?
summary(pnt.glob)   # largest gap for r_means!
pnt.glob[complete.cases(pnt.glob), .N] / pnt.glob[, .N]   # ca. 5% data loss (Greenland)

# Reduction to complete cases
pnt.glob <- pnt.glob[complete.cases(pnt.glob)]

# rename
setnames(pnt.glob, 
         c("UPLAND_SKM", "DIS_AV_CMS", "ORD_STRA"), 
         c("catch.sqkm", "disch", "strahl.hydro"))

## :: 4.0 BASIN ATLAS attributes

# important ATLAS attributes
imp.atlas <- c("tmp_dc_smx", "pre_mm_syr", "slp_dg_uav", "ero_kh_uav", "run_mm_syr", "ppd_pk_uav")
# reduce data prior to merge
basin.red <- basin.atlas[, .SD, .SDcols = c("HYBAS_ID",imp.atlas)]

pnt.glob <- merge(pnt.glob,
                  basin.red,
                  by.x = "HYBAS_L12",
                  by.y = "HYBAS_ID",
                  all.x = T)

## Cleanup
rm(elev, kmean, lulc, rmean, min.ph.lith, pnt.lake, basin.red)
gc()




# 2.0 Training / Tuning ----

## 2.1 Setup -----

## :: 0.0 GC
gc()
library("caret")
library("splitTools")
library("Hmisc")
library("corrplot")

### ::: DATA PREP
test.data <- rbind(eu.data,
                   us.data)

# Training data needs to be reduced to >= 2010
test.data <- test.data[year >= 2010]

# complete cases
test.data[, .N]         # 33589857

# All AquaSync CAS
my.cas <- aqua.data[!is.na(cas)]$cas

test.data <- test.data[cas %in% my.cas]
test.data[, .N]    # 8933938,   8,933,938 monitoring records
test.data[, .N, spat]   # 48,205 locations

## Unify detections
test.data[, .N, ndq]

# US detects now mostly carry concentrations and will be adjusted accordingly
test.data[ndq == "D" & !is.na(conc), ndq := "X"]    # convert to "quantifications" if they have an attributable concentration
test.data[ndq == "D" & is.na(conc), ndq := "N"]     # convert to "non-detects" if they have no attributable concentration
test.data <- droplevels(test.data)

# Add ATLAS attributes
atlas.imp <- c("tmp_dc_smx", "pre_mm_syr", "slp_dg_uav", "ero_kh_uav", "run_mm_syr", "ppd_pk_uav")    # important atlas attributes
# atlas reduced
atlas.red <- atlas[, .SD, .SDcols = c("spat", atlas.imp)]
# merge
test.data <- merge(test.data,
                   atlas.red,
                   by = "spat",
                   all.x = T)

# drop
test.data <- test.data[complete.cases(test.data[, -c("conc")])]  # 8933938 -- > 8539145 

# clean
rm(atlas.red)
gc()

## Final check of predictor correlation
# remove dups
spat.check <- test.data[!duplicated(spat)]
num.cols <- sapply(spat.check, class) %in% c("numeric", "integer")
spat.check <- spat.check[, .SD, .SDcols = num.cols]  # check
corr.m <- rcorr(as.matrix(spat.check))
corrplot(corr.m$r)
# CONCLUSION : Preliminary high correlative removal still applies. Only pH in soil and cathcm vs. discharge are highly correlative, but agreeable. 
# All other predictors are highly conserved / non-correlative, no further action needed
rm(spat.check, num.cols, corr.m)
gc()

## Remove unnecessary cols
test.data <- test.data[, -c("spat", "chemical", "lat", "lng", "conc", "cont", "date", "nodata")]

## Remove raw data or RAM issues (alloc) if less than 64 GB RAM
# rm(eu.data, us.data)
gc()

## Export data for later use
saveRDS(test.data,
        "data/PREDICTIONS/TRAIN-TEST-DATA.rds")

## 2.2 Fungicides ----


## :: 0.1 Model name
model.name <- "fungicides"

## :: DEFINE CHEMICALS
cur.cas <- aqua.data[!is.na(cas) & chemical_group == "fungicide"]$cas

# decide about data reduction
test.data[cas %in% cur.cas, .N] < 5*10^5 # IF TRUE, no reduction!
test.data[cas %in% cur.cas, .N]

# reduce to relevant data
opti.data <- test.data[cas %in% cur.cas,]


# Reduce to a max of 500.000 entries using stratified sampling approach
if (opti.data[, .N] > 500000) {
  
  # calc relative size by which the data needs to be split
  rel.size <- 500000 / opti.data[, .N]
  
  # derive 
  index <- createDataPartition(opti.data$ndq, 
                               p = rel.size, 
                               list = FALSE)
  
  opti.data <- opti.data[index]
} else {}

# Sanity check
san.check <- merge(opti.data[, .N, cas],
                   test.data[cas %in% cur.cas, .N, cas],
                   by = "cas",
                   all.x = T)
san.check[, rel := N.x / N.y]
san.check   # perfect
rm(san.check)



## :: Packs

## : SEED
seed <- 1234
set.seed(1234)

## : CORES
ncores <- 8

### :: 2.0 Generate Training ad Testing Data
data_split <- splitTools::partition(
  y = opti.data[, ]$ndq,
  p = c(train = 0.7, test = 0.3),
  type = "stratified",
  seed = seed
)

## Training data
train_x <- opti.data[data_split$train]
## Testing data
test_x <- opti.data[data_split$test]


### ::: 1.0 - PREDICTOR REMOVAL --- PREDICTION OPTIMIZATION

## :: Define mtry and tree
ntree <- 100 # initialization with only 100 trees

## Create base model - leave mtry by default to allow for predictor complexity-dependent mtry setting (square root)
start <- Sys.time()
base.model <- ranger(ndq ~ . ,
                     data = train_x[, -c("conc")],
                     num.tree = ntree,
                     importance = "permutation")
stop <- Sys.time()  # ca. 1 minute
stop-start

# view
base.model  #  baseline performace

## :: IMPORTANCE metrics
base.model.imp <- data.table(pred = names(base.model$variable.importance),
                             imp = unname(base.model$variable.importance))
setorder(base.model.imp, imp)
base.model.imp$pred <- factor(base.model.imp$pred , levels = base.model.imp$pred)
# Importance plot
ggplot(base.model.imp, aes(x = imp, y = pred)) + geom_point() + coord_cartesian(xlim = c(0, NA))


# Create list of number of predictor reductions
num.red <- seq(from = 2 , 
               to = nrow(base.model.imp)-1,
               by = 2)

# worst predictors - example for testing
worst.pred <- base.model.imp[1:num.red[1]]$pred               # worst
train_x[, setdiff(names(train_x), c(worst.pred)), with = F]   # visual check


# Optimization using k-fold (3) predictor removal
optim.list <- lapply(num.red, function(xx){
  
  # create worst predictor vector
  worst.pred <- base.model.imp[1:xx]$pred  # worst
  # create fold indices
  num.folds <- 3
  fold.indices <- createFolds(train_x$ndq, k = num.folds)
  
  list.fold <- lapply(1:num.folds, function(yy){
    
    # build model
    rf.red <- ranger(ndq ~ . ,
                     data = train_x[fold.indices[[yy]], setdiff(names(train_x), c(worst.pred)), with = F],
                     num.tree = ntree)
    
    # create secondary accuracy estiamte based on test data to further support decision process (not really needed due to internal OOB estimates but whatever.. for the reviewers i guess)
    test_x[, pred := predict(rf.red, test_x)$predictions]
    test.error <- 1 - (test_x[ndq == pred, .N] / test_x[, .N])
    
    # create results
    results <- data.table(run = xx,
                          fold = yy,
                          pred.error = rf.red$prediction.error,
                          test.error = test.error)
    
  })
  export <- rbindlist(list.fold)  # bind fold results
  
  # print progress
  print(paste(((round(xx / length(num.red),2))/2)*100, "% completed"))
  # export
  export
})
optim.dt <- rbindlist(optim.list)

# see optim
optim.dt   
optim.dt[, .(mean(pred.error), mean(test.error)), run][which.min(V1)]  # currently - test error even smaller than training = NICE
optim.dt[, mean(pred.error), run][order(V1)]
best.run <- optim.dt[, mean(pred.error), run][which.min(V1)]$run

# best predictors
base.model.imp[(best.run+1) : .N]  # highly complex currently best for fungicides
best.pred <- base.model.imp[(best.run+1) : .N]$pred

# Define best predictors!
best.pred <- c("ndq",
               as.vector(best.pred))

### ::: ------ NEW SELF_WRITTEN OPTIMIZER


# required for grid search and initialization of bayesian optimization
parameter_grid <- expand.grid(
  num.trees = c(100),   # previous analyses showed that num.tree already maxes out at 100-130. Iterating over it is nonsensical as higher ntree always improve predictions slightly without any overfits. Final model trained on ntree = 500 as overfits were never observed
  mtry = seq(from = 2,
             to = ifelse((length(best.pred)-1) < 12,
                         length(best.pred)-1,
                         12),               # allow a maximum mtry of 10 with 26 predictors
             by = 1)                        # After preliminary assessments there are only 2 valuable hyperparameters: ntree and mtry. ntree already amxes out at 100-130 without overfits in 500-1000, data not shown
  # min.node.size = c(1, 2, 4)              # preliminary assessment showed no real improvement with min node size, rather defaults are the best choices. No use in parameter grid searching it.
  # max.depth = seq(1, 3, 5, 9, NA)         # preliminary assessment showed that maximum tree depths always outcompetes without risk of overfit. No use in parameter grid searching this
  # sample.fraction = seq(0.5, 0.8, 0.3)    # preliminary assessment showed no improvement compared to default classifier values set by ranger package. No use in iterating over it
)
# SUMMARY: 
# ntree = 150 showed across contaminant groups to already reach very high predicitive power, or, alternatively, marginal gains if further increased. 
# Iteration over ntree only increases the total number of permutations, hence for fast grid searching only mtry requires optimization
# all other common hyperparamters did not outperform model defaults or 


gc()

## K-fold cross validation (3) for parameter grid tuning

tuning.list <- lapply(1:nrow(parameter_grid), function(ii){
  
  # dump current parameters
  num.trees <- parameter_grid[ii,]$num.trees
  mtry <- parameter_grid[ii,]$mtry
  sample.fraction <- parameter_grid[ii,]$sample.fraction
  
  ## 3 iterations to derive mean error / accuracy per parameter combination (similar to k-folding)
  k.list <- lapply(1:3, function(xx){
    
    cur.mod <- ranger(ndq ~ .,
                      data = train_x[ , .SD, .SDcols = best.pred],
                      num.trees = num.trees, 
                      mtry = mtry,
                      verbose = F)
    
    # create secondary accuracy estiamte based on test data to further support decision process (not really needed due to internal OOB estimates but whatever.. for the reviewers i guess)
    test_x[, pred := predict(cur.mod, test_x)$predictions]
    test.error <- 1 - (test_x[ndq == pred, .N] / test_x[, .N])
    
    results <- data.table(id = ii,
                          run = xx,
                          num.trees = num.trees,
                          mtry = mtry,
                          pred.error = cur.mod$prediction.error,
                          test.error = test.error)
    
  })
  # print progress
  print(paste0("Progress: ", round(ii / nrow(parameter_grid) *100,0), "% completed"))
  # print current grid
  print(parameter_grid[ii,])
  rbindlist(k.list)
  
})
tuning.list

tuning.dt <- rbindlist(tuning.list)   #  
# Assess accuracies - check comparability of pred and test error.
tuning.dt[, .(mean(pred.error), mean(test.error)), .(id, num.trees, mtry)][order(V1)]   # again both internal and external (test_set) accuracies are near identical
# keep best mtry
best.mtry <- tuning.dt[, mean(pred.error), .(id, num.trees, mtry)][order(V1)][1]$mtry
gc()

## Visualize results
fig.fun.tuning <-
  ggplot(tuning.dt)+
  geom_smooth(aes(x = mtry,
                  y = pred.error*100))+
  geom_point(aes(x = mtry,
                 y = pred.error*100))+
  coord_cartesian(ylim = c(NA,NA))+
  scale_y_continuous(expand = expansion(mult = c(.1,.1)))+
  ylab("Prediction error [%]")+
  ggtitle(label = paste0(model.name, " - mtry tuning"))
fig.fun.tuning

# ggsave(fig.fun.tuning,
#        file = paste0("data/PREDICTIONS/FINAL models/fig.fun.tuning.", model.name, ".png"),
#        width = 4, height = 4, dpi = 300)



### ::: ntree Tuning

## NTREE testing outside grid search
# REASON: no overfitting with up to 1000 trees
# Inclusion in grid search would massively increase training time to days/weeks

list.ntree <- lapply(c(50,100,150,200,250,300,400), function(xx){
  
  # build model
  rf.red <- ranger(ndq ~ . ,
                   data = train_x[ , .SD, .SDcols = best.pred],
                   num.tree = xx,
                   mtry = best.mtry)
  
  # create secondary accuracy estiamte based on test data to further support decision process (not really needed due to internal OOB estimates but whatever.. for the reviewers i guess)
  test_x[, pred := predict(rf.red, test_x)$predictions]
  test.error <- 1 - (test_x[ndq == pred, .N] / test_x[, .N])
  # progress
  print(xx)
  
  # create results
  results <- data.table(ntree = xx,
                        pred.error = rf.red$prediction.error,
                        test.error = test.error)
  
})
ntree.dt <- rbindlist(list.ntree)

## 1% rule, lowerst number of trees with least training 
# create 1% threshold
ntree.dt[, thresh := ntree.dt[which.min(pred.error)]$pred.error * 1.01]
best.ntree <- ntree.dt[pred.error < thresh][which.min(ntree)]$ntree

fig.tree <- 
  ggplot(ntree.dt)+
  geom_point(aes(x = ntree, y = pred.error))+
  geom_point(aes(x = ntree, y = test.error), color = "red")+
  geom_hline(yintercept = ntree.dt[1]$thresh, color ="green")+
  ggtitle(paste0(model.name, " - ntree tuning"))
fig.tree

# ggsave(fig.tree,
#        file = paste0("data/PREDICTIONS/FINAL models/fig.trees.", model.name, ".png"),
#        width = 4, height = 4, dpi = 300)



### ::: min.node.size tuning

# We want high generalization performace with minimum accuracy loss
# Maximize min.node.size with minimal accuracy loss


list.min.node.size<- lapply(c(1,10,20,35,50,100,200,500), function(xx){
  
  # build model
  rf.red <- ranger(ndq ~ . ,
                   data = train_x[ , .SD, .SDcols = best.pred],
                   num.tree = best.ntree,
                   mtry = best.mtry,
                   min.node.size = xx)
  
  # create secondary accuracy estiamte based on test data to further support decision process (not really needed due to internal OOB estimates but whatever.. for the reviewers i guess)
  test_x[, pred := predict(rf.red, test_x)$predictions]
  test.error <- 1 - (test_x[ndq == pred, .N] / test_x[, .N])
  # progress
  print(xx)
  
  # create results
  results <- data.table(min.node.size = xx,
                        pred.error = rf.red$prediction.error,
                        test.error = test.error)
  
})
min.node.size.dt <- rbindlist(list.min.node.size)

## 10% rule, highest number of min.node.size with least training 
# create 10% threshold
min.node.size.dt[, thresh := min.node.size.dt[which.min(pred.error)]$pred.error * 1.1]
best.min.node.size <- min.node.size.dt[pred.error < thresh][which.max(min.node.size)]$min.node.size

fig.node <- 
  ggplot(min.node.size.dt)+
  geom_point(aes(x = min.node.size, y = pred.error))+
  geom_point(aes(x = min.node.size, y = test.error), color = "red")+
  geom_hline(yintercept = min.node.size.dt[1]$thresh, color ="green")+
  ggtitle(paste0(model.name, " - min.node.size tuning"))
fig.node

# ggsave(fig.node,
#        file = paste0("data/PREDICTIONS/FINAL models/fig.node.", model.name, ".png"),
#        width = 4, height = 4, dpi = 300)



### ::: Tree depth optimization

## :: DERIVE TREE DEPTS

# calculate tree depth
compute_tree_depth <- function(tree_df) {
  
  # Create a named list to store depth of each node
  depth_map <- list("0" = 0)  # root node has depth 0
  
  # Loop over nodes in topological order (assuming nodeID is sorted)
  for (i in seq_len(nrow(tree_df))) {
    node <- tree_df[i, ]
    node_id <- as.character(node$nodeID)
    left <- as.character(node$leftChild)
    right <- as.character(node$rightChild)
    
    if (!is.na(left)) depth_map[[left]] <- depth_map[[node_id]] + 1
    if (!is.na(right)) depth_map[[right]] <- depth_map[[node_id]] + 1
  }
  
  max(unlist(depth_map))
}

# build model
rf.temp <- ranger(ndq ~ . ,
                  data = train_x[ , .SD, .SDcols = best.pred],
                  num.tree = best.ntree,
                  mtry = best.mtry,
                  min.node.size = best.min.node.size)


# calc tree info - ca 10 minutes (for metals)
approx.depth <- lapply(sample(1:best.ntree, size = 10), function(xx){
  
  # get tree
  tree <- treeInfo(rf.temp, tree = xx)
  # get depths
  depths <- compute_tree_depth(tree)
  # print progress
  print(xx)
  # export
  export <- data.table(tree = xx,
                       depth = depths)
  
  
})
depth.dt <- rbindlist(approx.depth)
rm(rf.temp, approx.depth)

# calc max, then do 10% downward steps
depth.tries <- c(max(depth.dt$depth), 
                 round(max(depth.dt$depth) * (1-c(.1,.2,.3,.4,.5)), 0))

list.max.depth<- lapply(depth.tries, function(xx){
  
  # build model
  rf.red <- ranger(ndq ~ . ,
                   data = train_x[ , .SD, .SDcols = best.pred],
                   num.tree = best.ntree,
                   mtry = best.mtry,
                   min.node.size = best.min.node.size,
                   max.depth = xx)
  
  # create secondary accuracy estiamte based on test data to further support decision process (not really needed due to internal OOB estimates but whatever.. for the reviewers i guess)
  test_x[, pred := predict(rf.red, test_x)$predictions]
  test.error <- 1 - (test_x[ndq == pred, .N] / test_x[, .N])
  # progress
  print(xx)
  
  # create results
  results <- data.table(max.depth = xx,
                        pred.error = rf.red$prediction.error,
                        test.error = test.error)
  
})
max.depth.dt <- rbindlist(list.max.depth)

## 1% rule, lowest number of tree.depth with least training error
# create 1% threshold
max.depth.dt[, thresh := max.depth.dt[which.min(pred.error)]$pred.error * 1.01]
best.max.depth <- max.depth.dt[pred.error < thresh][which.min(max.depth)]$max.depth

fig.max.depth <- 
  ggplot(max.depth.dt)+
  geom_point(aes(x = max.depth, y = pred.error))+
  geom_point(aes(x = max.depth, y = test.error), color = "red")+
  geom_hline(yintercept = max.depth.dt[1]$thresh, color ="green")+
  ggtitle(paste0(model.name, " - max.depth tuning"))
fig.max.depth

# ggsave(fig.max.depth,
#        file = paste0("data/PREDICTIONS/FINAL models/fig.max.depth.", model.name, ".png"),
#        width = 4, height = 4, dpi = 300)



### ::: Export combined tuning plots
fig.tuning <- grid.arrange(fig.fun.tuning,
                           fig.tree + theme(axis.title.y = element_blank()),
                           fig.node + theme(axis.title.y = element_blank()),
                           fig.max.depth + theme(axis.title.y = element_blank()),
                           ncol = 4,
                           nrow = 1)

ggsave(fig.tuning,
       file = paste0("data/PREDICTIONS/FINAL models/fig.", model.name, ".TUNING_det.png"),
       width = 18,
       height = 4.5,
       units = "cm")



### ::: 3.0 TRAIN FINAL MODEL AND EXPORT

## :: DEFINE CHEMICALS
# CONFIRM CURRENT CAS PRIOR TO TRAINING
cur.cas 

# reduce to relevant data
opti.data <- test.data[cas %in% cur.cas, ]

## : SEED
seed <- 1234
set.seed(1234)

## : CORES
ncores <- 8

### :: 2.0 Generate Training ad Testing Data
data_split <- splitTools::partition(
  y = opti.data[, ]$ndq,
  p = c(train = 0.7, test = 0.3),
  type = "stratified",
  seed = seed
)

## Training data
train_x <- opti.data[data_split$train]

## Testing data
test_x <- opti.data[data_split$test]

## : TRAIN MODEL
rf.fungicides <-  ranger(ndq ~ .,
                         data = train_x[ , .SD, .SDcols = best.pred],
                         num.trees = best.ntree, 
                         mtry = best.mtry,
                         min.node.size = best.min.node.size,
                         max.depth = best.max.depth,
                         importance = "permutation")

## : TRAIN MODEL - Probabilistic fo AUC / ROC derivation
rf.fungicides.prob <-  ranger(ndq ~ .,
                              data = train_x[ , .SD, .SDcols = best.pred],
                              num.trees = best.ntree, 
                              mtry = best.mtry,
                              min.node.size = best.min.node.size,
                              max.depth = best.max.depth,
                              prob = T)


test_x[ , pred := predict(rf.fungicides, test_x)$predictions]
test_x[, table(ndq == pred)][1] / test_x[, .N]   # no-overfit 

# Training accuracy
acc.test <- test_x[, table(ndq == pred)][1] / test_x[, .N]
acc.test <- unname(round(acc.test*100, 2))

## COMPARISON TO NULL MODEL
null.mod <- as.data.table(test_x[, table(ndq)])
acc.null <- round((null.mod[ndq == "X", ]$N / test_x[, .N])*100, 1)   # majority vote as null model
acc.full <- unname(round((test_x[, table(ndq == pred)][1] / test_x[, .N])*100, 1))

# Model imrpovement expressed as %decreased error rate
dec.error <- round(((acc.null - acc.full) / acc.null)*100, 2)

## Calculate Precision (P), Recall (R) and F1 score from confusion matrix
conf.mat <- as.data.table(rf.fungicides$confusion.matrix)

# PRECISION : P = TP / TP + FP
tp <- conf.mat[true == "N" & predicted == "N"]$N     # True positives (TP)
fp <- conf.mat[true == "X" & predicted == "N"]$N     # False positives (FP)
precision <- tp / (tp + fp)

# RECALL : R = TP / (TP + FN)
fn <- conf.mat[true == "N" & predicted == "X"]$N     # false negatives
recall <- tp / (tp + fn)

# F1-score : 2 * ((P * R) / (P + R))
f1.score <- 2 * ((precision * recall) / (precision + recall))

## :: EXPORT model with key metrics for later use
assign(paste0("export_", model.name, "_det"), list("model.name" = model.name,   # model name
                                                   "null.accuracy" = acc.null,  # accuracy of null model
                                                   "full.accuracy" = acc.full,  # accuracy of full model
                                                   "test.accuracy" = acc.test,  # accuracy on testing dataset
                                                   "precision" =  precision,    # precision of model
                                                   "recall" = recall,           # recall of model
                                                   "f1.score" = f1.score,       # F1 score of model
                                                   "dec.error" = dec.error,     # % decreased error rate compared to null model
                                                   "data.n" = opti.data[, .N],   # total number of samples available for training and testing
                                                   "data.summary" = opti.data[, .N, .(cas)],   # simple data summary
                                                   "full.model" = get(paste0("rf.",model.name)),
                                                   "prob.model" = get(paste0("rf.",model.name, ".prob"))))
# Final check
get(paste0("export_", model.name, "_det"))

## :: FINAL EXPORT OF DATA
saveRDS(get(paste0("export_", model.name, "_det")),
        file = paste0("data/PREDICTIONS/FINAL models/","export_", model.name, "_det.rds"))





## 2.3 Insecticides ----


## :: 0.1 Model name
model.name <- "insecticides"

## :: DEFINE CHEMICALS
cur.cas <- aqua.data[!is.na(cas) & chemical_group == "insecticide"]$cas

# decide about data reduction
test.data[cas %in% cur.cas, .N] < 5*10^5 # IF TRUE, no reduction!
test.data[cas %in% cur.cas, .N]

# reduce to relevant data
opti.data <- test.data[cas %in% cur.cas,]


# Reduce to a max of 500.000 entries using stratified sampling approach
if (opti.data[, .N] > 500000) {
  
  # calc relative size by which the data needs to be split
  rel.size <- 500000 / opti.data[, .N]
  
  # derive 
  index <- createDataPartition(opti.data$ndq, 
                               p = rel.size, 
                               list = FALSE)
  
  opti.data <- opti.data[index]
} else {}

# Sanity check
san.check <- merge(opti.data[, .N, cas],
                   test.data[cas %in% cur.cas, .N, cas],
                   by = "cas",
                   all.x = T)
san.check[, rel := N.x / N.y]
san.check   # perfect
rm(san.check)



## :: Packs

## : SEED
seed <- 1234
set.seed(1234)

## : CORES
ncores <- 8

### :: 2.0 Generate Training ad Testing Data
data_split <- splitTools::partition(
  y = opti.data[, ]$ndq,
  p = c(train = 0.7, test = 0.3),
  type = "stratified",
  seed = seed
)

## Training data
train_x <- opti.data[data_split$train]
## Testing data
test_x <- opti.data[data_split$test]


### ::: 1.0 - PREDICTOR REMOVAL --- PREDICTION OPTIMIZATION

## :: Define mtry and tree
ntree <- 100 # initialization with only 100 trees

## Create base model - leave mtry by default to allow for predictor complexity-dependent mtry setting (square root)
start <- Sys.time()
base.model <- ranger(ndq ~ . ,
                     data = train_x[, -c("conc")],
                     num.tree = ntree,
                     importance = "permutation")
stop <- Sys.time()  # ca. 1 minute
stop-start

# view
base.model  #  baseline performace

## :: IMPORTANCE metrics
base.model.imp <- data.table(pred = names(base.model$variable.importance),
                             imp = unname(base.model$variable.importance))
setorder(base.model.imp, imp)
base.model.imp$pred <- factor(base.model.imp$pred , levels = base.model.imp$pred)
# Importance plot
ggplot(base.model.imp, aes(x = imp, y = pred)) + geom_point() + coord_cartesian(xlim = c(0, NA))


# Create list of number of predictor reductions
num.red <- seq(from = 2 , 
               to = nrow(base.model.imp)-1,
               by = 2)

# worst predictors - example for testing
worst.pred <- base.model.imp[1:num.red[1]]$pred               # worst
train_x[, setdiff(names(train_x), c(worst.pred)), with = F]   # visual check


# Optimization using k-fold (3) predictor removal
optim.list <- lapply(num.red, function(xx){
  
  # create worst predictor vector
  worst.pred <- base.model.imp[1:xx]$pred  # worst
  # create fold indices
  num.folds <- 3
  fold.indices <- createFolds(train_x$ndq, k = num.folds)
  
  list.fold <- lapply(1:num.folds, function(yy){
    
    # build model
    rf.red <- ranger(ndq ~ . ,
                     data = train_x[fold.indices[[yy]], setdiff(names(train_x), c(worst.pred)), with = F],
                     num.tree = ntree)
    
    # create secondary accuracy estiamte based on test data to further support decision process (not really needed due to internal OOB estimates but whatever.. for the reviewers i guess)
    test_x[, pred := predict(rf.red, test_x)$predictions]
    test.error <- 1 - (test_x[ndq == pred, .N] / test_x[, .N])
    
    # create results
    results <- data.table(run = xx,
                          fold = yy,
                          pred.error = rf.red$prediction.error,
                          test.error = test.error)
    
  })
  export <- rbindlist(list.fold)  # bind fold results
  
  # print progress
  print(paste(((round(xx / length(num.red),2))/2)*100, "% completed"))
  # export
  export
})
optim.dt <- rbindlist(optim.list)

# see optim
optim.dt   
optim.dt[, .(mean(pred.error), mean(test.error)), run][which.min(V1)]  # currently - test error even smaller than training = NICE
optim.dt[, mean(pred.error), run][order(V1)]
best.run <- optim.dt[, mean(pred.error), run][which.min(V1)]$run

# best predictors
base.model.imp[(best.run+1) : .N]  # highly complex currently best for insecticides
best.pred <- base.model.imp[(best.run+1) : .N]$pred

# Define best predictors!
best.pred <- c("ndq",
               as.vector(best.pred))

### ::: ------ NEW SELF_WRITTEN OPTIMIZER


# required for grid search and initialization of bayesian optimization
parameter_grid <- expand.grid(
  num.trees = c(100),   # previous analyses showed that num.tree already maxes out at 100-130. Iterating over it is nonsensical as higher ntree always improve predictions slightly without any overfits. Final model trained on ntree = 500 as overfits were never observed
  mtry = seq(from = 2,
             to = ifelse((length(best.pred)-1) < 12,
                         length(best.pred)-1,
                         12),               # allow a maximum mtry of 10 with 26 predictors
             by = 1)                        # After preliminary assessments there are only 2 valuable hyperparameters: ntree and mtry. ntree already amxes out at 100-130 without overfits in 500-1000, data not shown
  # min.node.size = c(1, 2, 4)              # preliminary assessment showed no real improvement with min node size, rather defaults are the best choices. No use in parameter grid searching it.
  # max.depth = seq(1, 3, 5, 9, NA)         # preliminary assessment showed that maximum tree depths always outcompetes without risk of overfit. No use in parameter grid searching this
  # sample.fraction = seq(0.5, 0.8, 0.3)    # preliminary assessment showed no improvement compared to default classifier values set by ranger package. No use in iterating over it
)
# SUMMARY: 
# ntree = 150 showed across contaminant groups to already reach very high predicitive power, or, alternatively, marginal gains if further increased. 
# Iteration over ntree only increases the total number of permutations, hence for fast grid searching only mtry requires optimization
# all other common hyperparamters did not outperform model defaults or 

?ranger
gc()

## K-fold cross validation (3) for parameter grid tuning

tuning.list <- lapply(1:nrow(parameter_grid), function(ii){
  
  # dump current parameters
  num.trees <- parameter_grid[ii,]$num.trees
  mtry <- parameter_grid[ii,]$mtry
  sample.fraction <- parameter_grid[ii,]$sample.fraction
  
  ## 3 iterations to derive mean error / accuracy per parameter combination (similar to k-folding)
  k.list <- lapply(1:3, function(xx){
    
    cur.mod <- ranger(ndq ~ .,
                      data = train_x[ , .SD, .SDcols = best.pred],
                      num.trees = num.trees, 
                      mtry = mtry,
                      verbose = F)
    
    # create secondary accuracy estiamte based on test data to further support decision process (not really needed due to internal OOB estimates but whatever.. for the reviewers i guess)
    test_x[, pred := predict(cur.mod, test_x)$predictions]
    test.error <- 1 - (test_x[ndq == pred, .N] / test_x[, .N])
    
    results <- data.table(id = ii,
                          run = xx,
                          num.trees = num.trees,
                          mtry = mtry,
                          pred.error = cur.mod$prediction.error,
                          test.error = test.error)
    
  })
  # print progress
  print(paste0("Progress: ", round(ii / nrow(parameter_grid) *100,0), "% completed"))
  # print current grid
  print(parameter_grid[ii,])
  rbindlist(k.list)
  
})
tuning.list

tuning.dt <- rbindlist(tuning.list)   #  
# Assess accuracies - check comparability of pred and test error.
tuning.dt[, .(mean(pred.error), mean(test.error)), .(id, num.trees, mtry)][order(V1)]   # again both internal and external (test_set) accuracies are near identical
# keep best mtry
best.mtry <- tuning.dt[, mean(pred.error), .(id, num.trees, mtry)][order(V1)][1]$mtry
gc()

## Visualize results
fig.fun.tuning <-
  ggplot(tuning.dt)+
  geom_smooth(aes(x = mtry,
                  y = pred.error*100))+
  geom_point(aes(x = mtry,
                 y = pred.error*100))+
  coord_cartesian(ylim = c(NA,NA))+
  scale_y_continuous(expand = expansion(mult = c(.1,.1)))+
  ylab("Prediction error [%]")+
  ggtitle(label = paste0(model.name, " - mtry tuning"))
fig.fun.tuning

# ggsave(fig.fun.tuning,
#        file = paste0("data/PREDICTIONS/FINAL models/fig.fun.tuning.", model.name, ".png"),
#        width = 4, height = 4, dpi = 300)



### ::: ntree Tuning

## NTREE testing outside grid search
# REASON: no overfitting with up to 1000 trees
# Inclusion in grid search would massively increase training time to days/weeks

list.ntree <- lapply(c(50,100,150,200,250,300,400), function(xx){
  
  # build model
  rf.red <- ranger(ndq ~ . ,
                   data = train_x[ , .SD, .SDcols = best.pred],
                   num.tree = xx,
                   mtry = best.mtry)
  
  # create secondary accuracy estiamte based on test data to further support decision process (not really needed due to internal OOB estimates but whatever.. for the reviewers i guess)
  test_x[, pred := predict(rf.red, test_x)$predictions]
  test.error <- 1 - (test_x[ndq == pred, .N] / test_x[, .N])
  # progress
  print(xx)
  
  # create results
  results <- data.table(ntree = xx,
                        pred.error = rf.red$prediction.error,
                        test.error = test.error)
  
})
ntree.dt <- rbindlist(list.ntree)

## 1% rule, lowerst number of trees with least training 
# create 1% threshold
ntree.dt[, thresh := ntree.dt[which.min(pred.error)]$pred.error * 1.01]
best.ntree <- ntree.dt[pred.error < thresh][which.min(ntree)]$ntree

fig.tree <- 
  ggplot(ntree.dt)+
  geom_point(aes(x = ntree, y = pred.error))+
  geom_point(aes(x = ntree, y = test.error), color = "red")+
  geom_hline(yintercept = ntree.dt[1]$thresh, color ="green")+
  ggtitle(paste0(model.name, " - ntree tuning"))
fig.tree

# ggsave(fig.tree,
#        file = paste0("data/PREDICTIONS/FINAL models/fig.trees.", model.name, ".png"),
#        width = 4, height = 4, dpi = 300)



### ::: min.node.size tuning

# We want high generalization performace with minimum accuracy loss
# Maximize min.node.size with minimal accuracy loss


list.min.node.size<- lapply(c(1,10,20,35,50,100,200,500), function(xx){
  
  # build model
  rf.red <- ranger(ndq ~ . ,
                   data = train_x[ , .SD, .SDcols = best.pred],
                   num.tree = best.ntree,
                   mtry = best.mtry,
                   min.node.size = xx)
  
  # create secondary accuracy estiamte based on test data to further support decision process (not really needed due to internal OOB estimates but whatever.. for the reviewers i guess)
  test_x[, pred := predict(rf.red, test_x)$predictions]
  test.error <- 1 - (test_x[ndq == pred, .N] / test_x[, .N])
  # progress
  print(xx)
  
  # create results
  results <- data.table(min.node.size = xx,
                        pred.error = rf.red$prediction.error,
                        test.error = test.error)
  
})
min.node.size.dt <- rbindlist(list.min.node.size)

## 10% rule, highest number of min.node.size with least training 
# create 10% threshold
min.node.size.dt[, thresh := min.node.size.dt[which.min(pred.error)]$pred.error * 1.1]
best.min.node.size <- min.node.size.dt[pred.error < thresh][which.max(min.node.size)]$min.node.size

fig.node <- 
  ggplot(min.node.size.dt)+
  geom_point(aes(x = min.node.size, y = pred.error))+
  geom_point(aes(x = min.node.size, y = test.error), color = "red")+
  geom_hline(yintercept = min.node.size.dt[1]$thresh, color ="green")+
  ggtitle(paste0(model.name, " - min.node.size tuning"))
fig.node

# ggsave(fig.node,
#        file = paste0("data/PREDICTIONS/FINAL models/fig.node.", model.name, ".png"),
#        width = 4, height = 4, dpi = 300)



### ::: Tree depth optimization

## :: DERIVE TREE DEPTS

# calculate tree depth
compute_tree_depth <- function(tree_df) {
  
  # Create a named list to store depth of each node
  depth_map <- list("0" = 0)  # root node has depth 0
  
  # Loop over nodes in topological order (assuming nodeID is sorted)
  for (i in seq_len(nrow(tree_df))) {
    node <- tree_df[i, ]
    node_id <- as.character(node$nodeID)
    left <- as.character(node$leftChild)
    right <- as.character(node$rightChild)
    
    if (!is.na(left)) depth_map[[left]] <- depth_map[[node_id]] + 1
    if (!is.na(right)) depth_map[[right]] <- depth_map[[node_id]] + 1
  }
  
  max(unlist(depth_map))
}

# build model
rf.temp <- ranger(ndq ~ . ,
                  data = train_x[ , .SD, .SDcols = best.pred],
                  num.tree = best.ntree,
                  mtry = best.mtry,
                  min.node.size = best.min.node.size)


# calc tree info - ca 10 minutes (for metals)
approx.depth <- lapply(sample(1:best.ntree, size = 10), function(xx){
  
  # get tree
  tree <- treeInfo(rf.temp, tree = xx)
  # get depths
  depths <- compute_tree_depth(tree)
  # print progress
  print(xx)
  # export
  export <- data.table(tree = xx,
                       depth = depths)
  
  
})
depth.dt <- rbindlist(approx.depth)
rm(rf.temp, approx.depth)

# calc max, then do 10% downward steps
depth.tries <- c(max(depth.dt$depth), 
                 round(max(depth.dt$depth) * (1-c(.1,.2,.3,.4,.5)), 0))

list.max.depth<- lapply(depth.tries, function(xx){
  
  # build model
  rf.red <- ranger(ndq ~ . ,
                   data = train_x[ , .SD, .SDcols = best.pred],
                   num.tree = best.ntree,
                   mtry = best.mtry,
                   min.node.size = best.min.node.size,
                   max.depth = xx)
  
  # create secondary accuracy estiamte based on test data to further support decision process (not really needed due to internal OOB estimates but whatever.. for the reviewers i guess)
  test_x[, pred := predict(rf.red, test_x)$predictions]
  test.error <- 1 - (test_x[ndq == pred, .N] / test_x[, .N])
  # progress
  print(xx)
  
  # create results
  results <- data.table(max.depth = xx,
                        pred.error = rf.red$prediction.error,
                        test.error = test.error)
  
})
max.depth.dt <- rbindlist(list.max.depth)

## 1% rule, lowest number of tree.depth with least training error
# create 1% threshold
max.depth.dt[, thresh := max.depth.dt[which.min(pred.error)]$pred.error * 1.01]
best.max.depth <- max.depth.dt[pred.error < thresh][which.min(max.depth)]$max.depth

fig.max.depth <- 
  ggplot(max.depth.dt)+
  geom_point(aes(x = max.depth, y = pred.error))+
  geom_point(aes(x = max.depth, y = test.error), color = "red")+
  geom_hline(yintercept = max.depth.dt[1]$thresh, color ="green")+
  ggtitle(paste0(model.name, " - max.depth tuning"))
fig.max.depth

# ggsave(fig.max.depth,
#        file = paste0("data/PREDICTIONS/FINAL models/fig.max.depth.", model.name, ".png"),
#        width = 4, height = 4, dpi = 300)



### ::: Export combined tuning plots
fig.tuning <- grid.arrange(fig.fun.tuning,
                           fig.tree + theme(axis.title.y = element_blank()),
                           fig.node + theme(axis.title.y = element_blank()),
                           fig.max.depth + theme(axis.title.y = element_blank()),
                           ncol = 4,
                           nrow = 1)

ggsave(fig.tuning,
       file = paste0("data/PREDICTIONS/FINAL models/fig.", model.name, ".TUNING_det.png"),
       width = 18,
       height = 4.5,
       units = "cm")



### ::: 3.0 TRAIN FINAL MODEL AND EXPORT

## :: DEFINE CHEMICALS
# CONFIRM CURRENT CAS PRIOR TO TRAINING
cur.cas 

# reduce to relevant data
opti.data <- test.data[cas %in% cur.cas, ]

## : SEED
seed <- 1234
set.seed(1234)

## : CORES
ncores <- 8

### :: 2.0 Generate Training ad Testing Data
data_split <- splitTools::partition(
  y = opti.data[, ]$ndq,
  p = c(train = 0.7, test = 0.3),
  type = "stratified",
  seed = seed
)

## Training data
train_x <- opti.data[data_split$train]

## Testing data
test_x <- opti.data[data_split$test]

## : TRAIN MODEL
rf.insecticides <-  ranger(ndq ~ .,
                           data = train_x[ , .SD, .SDcols = best.pred],
                           num.trees = best.ntree, 
                           mtry = best.mtry,
                           min.node.size = best.min.node.size,
                           max.depth = best.max.depth,
                           importance = "permutation")

## : TRAIN MODEL - Probabilistic fo AUC / ROC derivation
rf.insecticides.prob <-  ranger(ndq ~ .,
                              data = train_x[ , .SD, .SDcols = best.pred],
                              num.trees = best.ntree, 
                              mtry = best.mtry,
                              min.node.size = best.min.node.size,
                              max.depth = best.max.depth,
                              prob = T)


test_x[ , pred := predict(rf.insecticides, test_x)$predictions]
test_x[, table(ndq == pred)][1] / test_x[, .N]   # no-overfit 

# Training accuracy
acc.test <- test_x[, table(ndq == pred)][1] / test_x[, .N]
acc.test <- unname(round(acc.test*100, 2))

## COMPARISON TO NULL MODEL
null.mod <- as.data.table(test_x[, table(ndq)])
acc.null <- round((null.mod[ndq == "X", ]$N / test_x[, .N])*100, 1)   # majority vote as null model
acc.full <- unname(round((test_x[, table(ndq == pred)][1] / test_x[, .N])*100, 1))

# Model imrpovement expressed as %decreased error rate
dec.error <- round(((acc.null - acc.full) / acc.null)*100, 2)

## Calculate Precision (P), Recall (R) and F1 score from confusion matrix
conf.mat <- as.data.table(rf.insecticides$confusion.matrix)

# PRECISION : P = TP / TP + FP
tp <- conf.mat[true == "N" & predicted == "N"]$N     # True positives (TP)
fp <- conf.mat[true == "X" & predicted == "N"]$N     # False positives (FP)
precision <- tp / (tp + fp)

# RECALL : R = TP / (TP + FN)
fn <- conf.mat[true == "N" & predicted == "X"]$N     # false negatives
recall <- tp / (tp + fn)

# F1-score : 2 * ((P * R) / (P + R))
f1.score <- 2 * ((precision * recall) / (precision + recall))

## :: EXPORT model with key metrics for later use
assign(paste0("export_", model.name, "_det"), list("model.name" = model.name,   # model name
                                                   "null.accuracy" = acc.null,  # accuracy of null model
                                                   "full.accuracy" = acc.full,  # accuracy of full model
                                                   "test.accuracy" = acc.test,  # accuracy on testing dataset
                                                   "precision" =  precision,    # precision of model
                                                   "recall" = recall,           # recall of model
                                                   "f1.score" = f1.score,       # F1 score of model
                                                   "dec.error" = dec.error,     # % decreased error rate compared to null model
                                                   "data.n" = opti.data[, .N],   # total number of samples available for training and testing
                                                   "data.summary" = opti.data[, .N, .(cas)],   # simple data summary
                                                   "full.model" = get(paste0("rf.",model.name)),
                                                   "prob.model" = get(paste0("rf.",model.name, ".prob"))))
# Final check
get(paste0("export_", model.name, "_det"))

## :: FINAL EXPORT OF DATA
saveRDS(get(paste0("export_", model.name, "_det")),
        file = paste0("data/PREDICTIONS/FINAL models/","export_", model.name, "_det.rds"))


## 2.4 Herbicides ----

## :: 0.1 Model name
model.name <- "herbicides"

## :: DEFINE CHEMICALS
cur.cas <- aqua.data[!is.na(cas) & chemical_group == "herbicide"]$cas

# decide about data reduction
test.data[cas %in% cur.cas, .N] < 5*10^5 # IF TRUE, no reduction!
test.data[cas %in% cur.cas, .N]

# reduce to relevant data
opti.data <- test.data[cas %in% cur.cas,]


# Reduce to a max of 500.000 entries using stratified sampling approach
if (opti.data[, .N] > 500000) {
  
  # calc relative size by which the data needs to be split
  rel.size <- 500000 / opti.data[, .N]
  
  # derive 
  index <- createDataPartition(opti.data$ndq, 
                               p = rel.size, 
                               list = FALSE)
  
  opti.data <- opti.data[index]
} else {}

# Sanity check
san.check <- merge(opti.data[, .N, cas],
                   test.data[cas %in% cur.cas, .N, cas],
                   by = "cas",
                   all.x = T)
san.check[, rel := N.x / N.y]
san.check   # perfect
rm(san.check)



## :: Packs

## : SEED
seed <- 1234
set.seed(1234)

## : CORES
ncores <- 8

### :: 2.0 Generate Training ad Testing Data
data_split <- splitTools::partition(
  y = opti.data[, ]$ndq,
  p = c(train = 0.7, test = 0.3),
  type = "stratified",
  seed = seed
)

## Training data
train_x <- opti.data[data_split$train]
## Testing data
test_x <- opti.data[data_split$test]


### ::: 1.0 - PREDICTOR REMOVAL --- PREDICTION OPTIMIZATION

## :: Define mtry and tree
ntree <- 100 # initialization with only 100 trees

## Create base model - leave mtry by default to allow for predictor complexity-dependent mtry setting (square root)
start <- Sys.time()
base.model <- ranger(ndq ~ . ,
                     data = train_x[, -c("conc")],
                     num.tree = ntree,
                     importance = "permutation")
stop <- Sys.time()  # ca. 1 minute
stop-start

# view
base.model  #  baseline performace

## :: IMPORTANCE metrics
base.model.imp <- data.table(pred = names(base.model$variable.importance),
                             imp = unname(base.model$variable.importance))
setorder(base.model.imp, imp)
base.model.imp$pred <- factor(base.model.imp$pred , levels = base.model.imp$pred)
# Importance plot
ggplot(base.model.imp, aes(x = imp, y = pred)) + geom_point() + coord_cartesian(xlim = c(0, NA))


# Create list of number of predictor reductions
num.red <- seq(from = 2 , 
               to = nrow(base.model.imp)-1,
               by = 2)

# worst predictors - example for testing
worst.pred <- base.model.imp[1:num.red[1]]$pred               # worst
train_x[, setdiff(names(train_x), c(worst.pred)), with = F]   # visual check


# Optimization using k-fold (3) predictor removal
optim.list <- lapply(num.red, function(xx){
  
  # create worst predictor vector
  worst.pred <- base.model.imp[1:xx]$pred  # worst
  # create fold indices
  num.folds <- 3
  fold.indices <- createFolds(train_x$ndq, k = num.folds)
  
  list.fold <- lapply(1:num.folds, function(yy){
    
    # build model
    rf.red <- ranger(ndq ~ . ,
                     data = train_x[fold.indices[[yy]], setdiff(names(train_x), c(worst.pred)), with = F],
                     num.tree = ntree)
    
    # create secondary accuracy estiamte based on test data to further support decision process (not really needed due to internal OOB estimates but whatever.. for the reviewers i guess)
    test_x[, pred := predict(rf.red, test_x)$predictions]
    test.error <- 1 - (test_x[ndq == pred, .N] / test_x[, .N])
    
    # create results
    results <- data.table(run = xx,
                          fold = yy,
                          pred.error = rf.red$prediction.error,
                          test.error = test.error)
    
  })
  export <- rbindlist(list.fold)  # bind fold results
  
  # print progress
  print(paste(((round(xx / length(num.red),2))/2)*100, "% completed"))
  # export
  export
})
optim.dt <- rbindlist(optim.list)

# see optim
optim.dt   
optim.dt[, .(mean(pred.error), mean(test.error)), run][which.min(V1)]  # currently - test error even smaller than training = NICE
optim.dt[, mean(pred.error), run][order(V1)]
best.run <- optim.dt[, mean(pred.error), run][which.min(V1)]$run

# best predictors
base.model.imp[(best.run+1) : .N]  # highly complex currently best for herbicides
best.pred <- base.model.imp[(best.run+1) : .N]$pred

# Define best predictors!
best.pred <- c("ndq",
               as.vector(best.pred))

### ::: ------ NEW SELF_WRITTEN OPTIMIZER


# required for grid search and initialization of bayesian optimization
parameter_grid <- expand.grid(
  num.trees = c(100),   # previous analyses showed that num.tree already maxes out at 100-130. Iterating over it is nonsensical as higher ntree always improve predictions slightly without any overfits. Final model trained on ntree = 500 as overfits were never observed
  mtry = seq(from = 2,
             to = ifelse((length(best.pred)-1) < 12,
                         length(best.pred)-1,
                         12),               # allow a maximum mtry of 10 with 26 predictors
             by = 1)                        # After preliminary assessments there are only 2 valuable hyperparameters: ntree and mtry. ntree already amxes out at 100-130 without overfits in 500-1000, data not shown
  # min.node.size = c(1, 2, 4)              # preliminary assessment showed no real improvement with min node size, rather defaults are the best choices. No use in parameter grid searching it.
  # max.depth = seq(1, 3, 5, 9, NA)         # preliminary assessment showed that maximum tree depths always outcompetes without risk of overfit. No use in parameter grid searching this
  # sample.fraction = seq(0.5, 0.8, 0.3)    # preliminary assessment showed no improvement compared to default classifier values set by ranger package. No use in iterating over it
)
# SUMMARY: 
# ntree = 150 showed across contaminant groups to already reach very high predicitive power, or, alternatively, marginal gains if further increased. 
# Iteration over ntree only increases the total number of permutations, hence for fast grid searching only mtry requires optimization
# all other common hyperparamters did not outperform model defaults or 

?ranger
gc()

## K-fold cross validation (3) for parameter grid tuning

tuning.list <- lapply(1:nrow(parameter_grid), function(ii){
  
  # dump current parameters
  num.trees <- parameter_grid[ii,]$num.trees
  mtry <- parameter_grid[ii,]$mtry
  sample.fraction <- parameter_grid[ii,]$sample.fraction
  
  ## 3 iterations to derive mean error / accuracy per parameter combination (similar to k-folding)
  k.list <- lapply(1:3, function(xx){
    
    cur.mod <- ranger(ndq ~ .,
                      data = train_x[ , .SD, .SDcols = best.pred],
                      num.trees = num.trees, 
                      mtry = mtry,
                      verbose = F)
    
    # create secondary accuracy estiamte based on test data to further support decision process (not really needed due to internal OOB estimates but whatever.. for the reviewers i guess)
    test_x[, pred := predict(cur.mod, test_x)$predictions]
    test.error <- 1 - (test_x[ndq == pred, .N] / test_x[, .N])
    
    results <- data.table(id = ii,
                          run = xx,
                          num.trees = num.trees,
                          mtry = mtry,
                          pred.error = cur.mod$prediction.error,
                          test.error = test.error)
    
  })
  # print progress
  print(paste0("Progress: ", round(ii / nrow(parameter_grid) *100,0), "% completed"))
  # print current grid
  print(parameter_grid[ii,])
  rbindlist(k.list)
  
})
tuning.list

tuning.dt <- rbindlist(tuning.list)   #  
# Assess accuracies - check comparability of pred and test error.
tuning.dt[, .(mean(pred.error), mean(test.error)), .(id, num.trees, mtry)][order(V1)]   # again both internal and external (test_set) accuracies are near identical
# keep best mtry
best.mtry <- tuning.dt[, mean(pred.error), .(id, num.trees, mtry)][order(V1)][1]$mtry
gc()

## Visualize results
fig.fun.tuning <-
  ggplot(tuning.dt)+
  geom_smooth(aes(x = mtry,
                  y = pred.error*100))+
  geom_point(aes(x = mtry,
                 y = pred.error*100))+
  coord_cartesian(ylim = c(NA,NA))+
  scale_y_continuous(expand = expansion(mult = c(.1,.1)))+
  ylab("Prediction error [%]")+
  ggtitle(label = paste0(model.name, " - mtry tuning"))
fig.fun.tuning

# ggsave(fig.fun.tuning,
#        file = paste0("data/PREDICTIONS/FINAL models/fig.fun.tuning.", model.name, ".png"),
#        width = 4, height = 4, dpi = 300)



### ::: ntree Tuning

## NTREE testing outside grid search
# REASON: no overfitting with up to 1000 trees
# Inclusion in grid search would massively increase training time to days/weeks

list.ntree <- lapply(c(50,100,150,200,250,300,400), function(xx){
  
  # build model
  rf.red <- ranger(ndq ~ . ,
                   data = train_x[ , .SD, .SDcols = best.pred],
                   num.tree = xx,
                   mtry = best.mtry)
  
  # create secondary accuracy estiamte based on test data to further support decision process (not really needed due to internal OOB estimates but whatever.. for the reviewers i guess)
  test_x[, pred := predict(rf.red, test_x)$predictions]
  test.error <- 1 - (test_x[ndq == pred, .N] / test_x[, .N])
  # progress
  print(xx)
  
  # create results
  results <- data.table(ntree = xx,
                        pred.error = rf.red$prediction.error,
                        test.error = test.error)
  
})
ntree.dt <- rbindlist(list.ntree)

## 1% rule, lowerst number of trees with least training 
# create 1% threshold
ntree.dt[, thresh := ntree.dt[which.min(pred.error)]$pred.error * 1.01]
best.ntree <- ntree.dt[pred.error < thresh][which.min(ntree)]$ntree

fig.tree <- 
  ggplot(ntree.dt)+
  geom_point(aes(x = ntree, y = pred.error))+
  geom_point(aes(x = ntree, y = test.error), color = "red")+
  geom_hline(yintercept = ntree.dt[1]$thresh, color ="green")+
  ggtitle(paste0(model.name, " - ntree tuning"))
fig.tree

# ggsave(fig.tree,
#        file = paste0("data/PREDICTIONS/FINAL models/fig.trees.", model.name, ".png"),
#        width = 4, height = 4, dpi = 300)



### ::: min.node.size tuning

# We want high generalization performace with minimum accuracy loss
# Maximize min.node.size with minimal accuracy loss


list.min.node.size<- lapply(c(1,10,20,35,50,100,200,500), function(xx){
  
  # build model
  rf.red <- ranger(ndq ~ . ,
                   data = train_x[ , .SD, .SDcols = best.pred],
                   num.tree = best.ntree,
                   mtry = best.mtry,
                   min.node.size = xx)
  
  # create secondary accuracy estiamte based on test data to further support decision process (not really needed due to internal OOB estimates but whatever.. for the reviewers i guess)
  test_x[, pred := predict(rf.red, test_x)$predictions]
  test.error <- 1 - (test_x[ndq == pred, .N] / test_x[, .N])
  # progress
  print(xx)
  
  # create results
  results <- data.table(min.node.size = xx,
                        pred.error = rf.red$prediction.error,
                        test.error = test.error)
  
})
min.node.size.dt <- rbindlist(list.min.node.size)

## 10% rule, highest number of min.node.size with least training 
# create 10% threshold
min.node.size.dt[, thresh := min.node.size.dt[which.min(pred.error)]$pred.error * 1.1]
best.min.node.size <- min.node.size.dt[pred.error < thresh][which.max(min.node.size)]$min.node.size

fig.node <- 
  ggplot(min.node.size.dt)+
  geom_point(aes(x = min.node.size, y = pred.error))+
  geom_point(aes(x = min.node.size, y = test.error), color = "red")+
  geom_hline(yintercept = min.node.size.dt[1]$thresh, color ="green")+
  ggtitle(paste0(model.name, " - min.node.size tuning"))
fig.node

# ggsave(fig.node,
#        file = paste0("data/PREDICTIONS/FINAL models/fig.node.", model.name, ".png"),
#        width = 4, height = 4, dpi = 300)



### ::: Tree depth optimization

## :: DERIVE TREE DEPTS

# calculate tree depth
compute_tree_depth <- function(tree_df) {
  
  # Create a named list to store depth of each node
  depth_map <- list("0" = 0)  # root node has depth 0
  
  # Loop over nodes in topological order (assuming nodeID is sorted)
  for (i in seq_len(nrow(tree_df))) {
    node <- tree_df[i, ]
    node_id <- as.character(node$nodeID)
    left <- as.character(node$leftChild)
    right <- as.character(node$rightChild)
    
    if (!is.na(left)) depth_map[[left]] <- depth_map[[node_id]] + 1
    if (!is.na(right)) depth_map[[right]] <- depth_map[[node_id]] + 1
  }
  
  max(unlist(depth_map))
}

# build model
rf.temp <- ranger(ndq ~ . ,
                  data = train_x[ , .SD, .SDcols = best.pred],
                  num.tree = best.ntree,
                  mtry = best.mtry,
                  min.node.size = best.min.node.size)


# calc tree info - ca 10 minutes (for metals)
approx.depth <- lapply(sample(1:best.ntree, size = 10), function(xx){
  
  # get tree
  tree <- treeInfo(rf.temp, tree = xx)
  # get depths
  depths <- compute_tree_depth(tree)
  # print progress
  print(xx)
  # export
  export <- data.table(tree = xx,
                       depth = depths)
  
  
})
depth.dt <- rbindlist(approx.depth)
rm(rf.temp, approx.depth)

# calc max, then do 10% downward steps
depth.tries <- c(max(depth.dt$depth), 
                 round(max(depth.dt$depth) * (1-c(.1,.2,.3,.4,.5)), 0))

list.max.depth<- lapply(depth.tries, function(xx){
  
  # build model
  rf.red <- ranger(ndq ~ . ,
                   data = train_x[ , .SD, .SDcols = best.pred],
                   num.tree = best.ntree,
                   mtry = best.mtry,
                   min.node.size = best.min.node.size,
                   max.depth = xx)
  
  # create secondary accuracy estiamte based on test data to further support decision process (not really needed due to internal OOB estimates but whatever.. for the reviewers i guess)
  test_x[, pred := predict(rf.red, test_x)$predictions]
  test.error <- 1 - (test_x[ndq == pred, .N] / test_x[, .N])
  # progress
  print(xx)
  
  # create results
  results <- data.table(max.depth = xx,
                        pred.error = rf.red$prediction.error,
                        test.error = test.error)
  
})
max.depth.dt <- rbindlist(list.max.depth)

## 1% rule, lowest number of tree.depth with least training error
# create 1% threshold
max.depth.dt[, thresh := max.depth.dt[which.min(pred.error)]$pred.error * 1.01]
best.max.depth <- max.depth.dt[pred.error < thresh][which.min(max.depth)]$max.depth

fig.max.depth <- 
  ggplot(max.depth.dt)+
  geom_point(aes(x = max.depth, y = pred.error))+
  geom_point(aes(x = max.depth, y = test.error), color = "red")+
  geom_hline(yintercept = max.depth.dt[1]$thresh, color ="green")+
  ggtitle(paste0(model.name, " - max.depth tuning"))
fig.max.depth

# ggsave(fig.max.depth,
#        file = paste0("data/PREDICTIONS/FINAL models/fig.max.depth.", model.name, ".png"),
#        width = 4, height = 4, dpi = 300)



### ::: Export combined tuning plots
fig.tuning <- grid.arrange(fig.fun.tuning,
                           fig.tree + theme(axis.title.y = element_blank()),
                           fig.node + theme(axis.title.y = element_blank()),
                           fig.max.depth + theme(axis.title.y = element_blank()),
                           ncol = 4,
                           nrow = 1)

ggsave(fig.tuning,
       file = paste0("data/PREDICTIONS/FINAL models/fig.", model.name, ".TUNING_det.png"),
       width = 18,
       height = 4.5,
       units = "cm")



### ::: 3.0 TRAIN FINAL MODEL AND EXPORT

## :: DEFINE CHEMICALS
# CONFIRM CURRENT CAS PRIOR TO TRAINING
cur.cas 

# reduce to relevant data
opti.data <- test.data[cas %in% cur.cas, ]

## : SEED
seed <- 1234
set.seed(1234)

## : CORES
ncores <- 8

### :: 2.0 Generate Training ad Testing Data
data_split <- splitTools::partition(
  y = opti.data[, ]$ndq,
  p = c(train = 0.7, test = 0.3),
  type = "stratified",
  seed = seed
)

## Training data
train_x <- opti.data[data_split$train]

## Testing data
test_x <- opti.data[data_split$test]

## : TRAIN MODEL
rf.herbicides <-  ranger(ndq ~ .,
                         data = train_x[ , .SD, .SDcols = best.pred],
                         num.trees = best.ntree, 
                         mtry = best.mtry,
                         min.node.size = best.min.node.size,
                         max.depth = best.max.depth,
                         importance = "permutation")

## : TRAIN MODEL - Probabilistic fo AUC / ROC derivation
rf.herbicides.prob <-  ranger(ndq ~ .,
                              data = train_x[ , .SD, .SDcols = best.pred],
                              num.trees = best.ntree, 
                              mtry = best.mtry,
                              min.node.size = best.min.node.size,
                              max.depth = best.max.depth,
                              prob = T)


test_x[ , pred := predict(rf.herbicides, test_x)$predictions]
test_x[, table(ndq == pred)][1] / test_x[, .N]   # no-overfit 

# Training accuracy
acc.test <- test_x[, table(ndq == pred)][1] / test_x[, .N]
acc.test <- unname(round(acc.test*100, 2))

## COMPARISON TO NULL MODEL
null.mod <- as.data.table(test_x[, table(ndq)])
acc.null <- round((null.mod[ndq == "X", ]$N / test_x[, .N])*100, 1)   # majority vote as null model
acc.full <- unname(round((test_x[, table(ndq == pred)][1] / test_x[, .N])*100, 1))

# Model imrpovement expressed as %decreased error rate
dec.error <- round(((acc.null - acc.full) / acc.null)*100, 2)

## Calculate Precision (P), Recall (R) and F1 score from confusion matrix
conf.mat <- as.data.table(rf.herbicides$confusion.matrix)

# PRECISION : P = TP / TP + FP
tp <- conf.mat[true == "N" & predicted == "N"]$N     # True positives (TP)
fp <- conf.mat[true == "X" & predicted == "N"]$N     # False positives (FP)
precision <- tp / (tp + fp)

# RECALL : R = TP / (TP + FN)
fn <- conf.mat[true == "N" & predicted == "X"]$N     # false negatives
recall <- tp / (tp + fn)

# F1-score : 2 * ((P * R) / (P + R))
f1.score <- 2 * ((precision * recall) / (precision + recall))

## :: EXPORT model with key metrics for later use
assign(paste0("export_", model.name, "_det"), list("model.name" = model.name,   # model name
                                                   "null.accuracy" = acc.null,  # accuracy of null model
                                                   "full.accuracy" = acc.full,  # accuracy of full model
                                                   "test.accuracy" = acc.test,  # accuracy on testing dataset
                                                   "precision" =  precision,    # precision of model
                                                   "recall" = recall,           # recall of model
                                                   "f1.score" = f1.score,       # F1 score of model
                                                   "dec.error" = dec.error,     # % decreased error rate compared to null model
                                                   "data.n" = opti.data[, .N],   # total number of samples available for training and testing
                                                   "data.summary" = opti.data[, .N, .(cas)],   # simple data summary
                                                   "full.model" = get(paste0("rf.",model.name)),
                                                   "prob.model" = get(paste0("rf.",model.name, ".prob"))))
# Final check
get(paste0("export_", model.name, "_det"))

## :: FINAL EXPORT OF DATA
saveRDS(get(paste0("export_", model.name, "_det")),
        file = paste0("data/PREDICTIONS/FINAL models/","export_", model.name, "_det.rds"))


## 2.5 Metals ----

## :: 0.1 Model name
model.name <- "metals"

## :: DEFINE CHEMICALS
cur.cas <- aqua.data[!is.na(cas) & chemical_group == "metal"]$cas

# decide about data reduction
test.data[cas %in% cur.cas, .N] < 5*10^5 # IF TRUE, no reduction!
test.data[cas %in% cur.cas, .N]

# reduce to relevant data
opti.data <- test.data[cas %in% cur.cas,]


# Reduce to a max of 500.000 entries using stratified sampling approach
if (opti.data[, .N] > 500000) {
  
  # calc relative size by which the data needs to be split
  rel.size <- 500000 / opti.data[, .N]
  
  # derive 
  index <- createDataPartition(opti.data$ndq, 
                               p = rel.size, 
                               list = FALSE)
  
  opti.data <- opti.data[index]
} else {}

# Sanity check
san.check <- merge(opti.data[, .N, cas],
                   test.data[cas %in% cur.cas, .N, cas],
                   by = "cas",
                   all.x = T)
san.check[, rel := N.x / N.y]
san.check   # perfect
rm(san.check)



## :: Packs

## : SEED
seed <- 1234
set.seed(1234)

## : CORES
ncores <- 8

### :: 2.0 Generate Training ad Testing Data
data_split <- splitTools::partition(
  y = opti.data[, ]$ndq,
  p = c(train = 0.7, test = 0.3),
  type = "stratified",
  seed = seed
)

## Training data
train_x <- opti.data[data_split$train]
## Testing data
test_x <- opti.data[data_split$test]


### ::: 1.0 - PREDICTOR REMOVAL --- PREDICTION OPTIMIZATION

## :: Define mtry and tree
ntree <- 100 # initialization with only 100 trees

## Create base model - leave mtry by default to allow for predictor complexity-dependent mtry setting (square root)
start <- Sys.time()
base.model <- ranger(ndq ~ . ,
                     data = train_x[, -c("conc")],
                     num.tree = ntree,
                     importance = "permutation")
stop <- Sys.time()  # ca. 1 minute
stop-start

# view
base.model  #  baseline performace

## :: IMPORTANCE metrics
base.model.imp <- data.table(pred = names(base.model$variable.importance),
                             imp = unname(base.model$variable.importance))
setorder(base.model.imp, imp)
base.model.imp$pred <- factor(base.model.imp$pred , levels = base.model.imp$pred)
# Importance plot
ggplot(base.model.imp, aes(x = imp, y = pred)) + geom_point() + coord_cartesian(xlim = c(0, NA))


# Create list of number of predictor reductions
num.red <- seq(from = 2 , 
               to = nrow(base.model.imp)-1,
               by = 2)

# worst predictors - example for testing
worst.pred <- base.model.imp[1:num.red[1]]$pred               # worst
train_x[, setdiff(names(train_x), c(worst.pred)), with = F]   # visual check


# Optimization using k-fold (3) predictor removal
optim.list <- lapply(num.red, function(xx){
  
  # create worst predictor vector
  worst.pred <- base.model.imp[1:xx]$pred  # worst
  # create fold indices
  num.folds <- 3
  fold.indices <- createFolds(train_x$ndq, k = num.folds)
  
  list.fold <- lapply(1:num.folds, function(yy){
    
    # build model
    rf.red <- ranger(ndq ~ . ,
                     data = train_x[fold.indices[[yy]], setdiff(names(train_x), c(worst.pred)), with = F],
                     num.tree = ntree)
    
    # create secondary accuracy estiamte based on test data to further support decision process (not really needed due to internal OOB estimates but whatever.. for the reviewers i guess)
    test_x[, pred := predict(rf.red, test_x)$predictions]
    test.error <- 1 - (test_x[ndq == pred, .N] / test_x[, .N])
    
    # create results
    results <- data.table(run = xx,
                          fold = yy,
                          pred.error = rf.red$prediction.error,
                          test.error = test.error)
    
  })
  export <- rbindlist(list.fold)  # bind fold results
  
  # print progress
  print(paste(((round(xx / length(num.red),2))/2)*100, "% completed"))
  # export
  export
})
optim.dt <- rbindlist(optim.list)

# see optim
optim.dt   
optim.dt[, .(mean(pred.error), mean(test.error)), run][which.min(V1)]  # currently - test error even smaller than training = NICE
optim.dt[, mean(pred.error), run][order(V1)]
best.run <- optim.dt[, mean(pred.error), run][which.min(V1)]$run

# best predictors
base.model.imp[(best.run+1) : .N]  # highly complex currently best for metals
best.pred <- base.model.imp[(best.run+1) : .N]$pred

# Define best predictors!
best.pred <- c("ndq",
               as.vector(best.pred))

### ::: ------ NEW SELF_WRITTEN OPTIMIZER


# required for grid search and initialization of bayesian optimization
parameter_grid <- expand.grid(
  num.trees = c(100),   # previous analyses showed that num.tree already maxes out at 100-130. Iterating over it is nonsensical as higher ntree always improve predictions slightly without any overfits. Final model trained on ntree = 500 as overfits were never observed
  mtry = seq(from = 2,
             to = ifelse((length(best.pred)-1) < 12,
                         length(best.pred)-1,
                         12),               # allow a maximum mtry of 10 with 26 predictors
             by = 1)                        # After preliminary assessments there are only 2 valuable hyperparameters: ntree and mtry. ntree already amxes out at 100-130 without overfits in 500-1000, data not shown
  # min.node.size = c(1, 2, 4)              # preliminary assessment showed no real improvement with min node size, rather defaults are the best choices. No use in parameter grid searching it.
  # max.depth = seq(1, 3, 5, 9, NA)         # preliminary assessment showed that maximum tree depths always outcompetes without risk of overfit. No use in parameter grid searching this
  # sample.fraction = seq(0.5, 0.8, 0.3)    # preliminary assessment showed no improvement compared to default classifier values set by ranger package. No use in iterating over it
)
# SUMMARY: 
# ntree = 150 showed across contaminant groups to already reach very high predicitive power, or, alternatively, marginal gains if further increased. 
# Iteration over ntree only increases the total number of permutations, hence for fast grid searching only mtry requires optimization
# all other common hyperparamters did not outperform model defaults or 

?ranger
gc()

## K-fold cross validation (3) for parameter grid tuning

tuning.list <- lapply(1:nrow(parameter_grid), function(ii){
  
  # dump current parameters
  num.trees <- parameter_grid[ii,]$num.trees
  mtry <- parameter_grid[ii,]$mtry
  sample.fraction <- parameter_grid[ii,]$sample.fraction
  
  ## 3 iterations to derive mean error / accuracy per parameter combination (similar to k-folding)
  k.list <- lapply(1:3, function(xx){
    
    cur.mod <- ranger(ndq ~ .,
                      data = train_x[ , .SD, .SDcols = best.pred],
                      num.trees = num.trees, 
                      mtry = mtry,
                      verbose = F)
    
    # create secondary accuracy estiamte based on test data to further support decision process (not really needed due to internal OOB estimates but whatever.. for the reviewers i guess)
    test_x[, pred := predict(cur.mod, test_x)$predictions]
    test.error <- 1 - (test_x[ndq == pred, .N] / test_x[, .N])
    
    results <- data.table(id = ii,
                          run = xx,
                          num.trees = num.trees,
                          mtry = mtry,
                          pred.error = cur.mod$prediction.error,
                          test.error = test.error)
    
  })
  # print progress
  print(paste0("Progress: ", round(ii / nrow(parameter_grid) *100,0), "% completed"))
  # print current grid
  print(parameter_grid[ii,])
  rbindlist(k.list)
  
})
tuning.list

tuning.dt <- rbindlist(tuning.list)   #  
# Assess accuracies - check comparability of pred and test error.
tuning.dt[, .(mean(pred.error), mean(test.error)), .(id, num.trees, mtry)][order(V1)]   # again both internal and external (test_set) accuracies are near identical
# keep best mtry
best.mtry <- tuning.dt[, mean(pred.error), .(id, num.trees, mtry)][order(V1)][1]$mtry
gc()

## Visualize results
fig.fun.tuning <-
  ggplot(tuning.dt)+
  geom_smooth(aes(x = mtry,
                  y = pred.error*100))+
  geom_point(aes(x = mtry,
                 y = pred.error*100))+
  coord_cartesian(ylim = c(NA,NA))+
  scale_y_continuous(expand = expansion(mult = c(.1,.1)))+
  ylab("Prediction error [%]")+
  ggtitle(label = paste0(model.name, " - mtry tuning"))
fig.fun.tuning

# ggsave(fig.fun.tuning,
#        file = paste0("data/PREDICTIONS/FINAL models/fig.fun.tuning.", model.name, ".png"),
#        width = 4, height = 4, dpi = 300)



### ::: ntree Tuning

## NTREE testing outside grid search
# REASON: no overfitting with up to 1000 trees
# Inclusion in grid search would massively increase training time to days/weeks

list.ntree <- lapply(c(50,100,150,200,250,300,400), function(xx){
  
  # build model
  rf.red <- ranger(ndq ~ . ,
                   data = train_x[ , .SD, .SDcols = best.pred],
                   num.tree = xx,
                   mtry = best.mtry)
  
  # create secondary accuracy estiamte based on test data to further support decision process (not really needed due to internal OOB estimates but whatever.. for the reviewers i guess)
  test_x[, pred := predict(rf.red, test_x)$predictions]
  test.error <- 1 - (test_x[ndq == pred, .N] / test_x[, .N])
  # progress
  print(xx)
  
  # create results
  results <- data.table(ntree = xx,
                        pred.error = rf.red$prediction.error,
                        test.error = test.error)
  
})
ntree.dt <- rbindlist(list.ntree)

## 1% rule, lowerst number of trees with least training 
# create 1% threshold
ntree.dt[, thresh := ntree.dt[which.min(pred.error)]$pred.error * 1.01]
best.ntree <- ntree.dt[pred.error < thresh][which.min(ntree)]$ntree

fig.tree <- 
  ggplot(ntree.dt)+
  geom_point(aes(x = ntree, y = pred.error))+
  geom_point(aes(x = ntree, y = test.error), color = "red")+
  geom_hline(yintercept = ntree.dt[1]$thresh, color ="green")+
  ggtitle(paste0(model.name, " - ntree tuning"))
fig.tree

# ggsave(fig.tree,
#        file = paste0("data/PREDICTIONS/FINAL models/fig.trees.", model.name, ".png"),
#        width = 4, height = 4, dpi = 300)



### ::: min.node.size tuning

# We want high generalization performace with minimum accuracy loss
# Maximize min.node.size with minimal accuracy loss


list.min.node.size<- lapply(c(1,10,20,35,50,100,200,500), function(xx){
  
  # build model
  rf.red <- ranger(ndq ~ . ,
                   data = train_x[ , .SD, .SDcols = best.pred],
                   num.tree = best.ntree,
                   mtry = best.mtry,
                   min.node.size = xx)
  
  # create secondary accuracy estiamte based on test data to further support decision process (not really needed due to internal OOB estimates but whatever.. for the reviewers i guess)
  test_x[, pred := predict(rf.red, test_x)$predictions]
  test.error <- 1 - (test_x[ndq == pred, .N] / test_x[, .N])
  # progress
  print(xx)
  
  # create results
  results <- data.table(min.node.size = xx,
                        pred.error = rf.red$prediction.error,
                        test.error = test.error)
  
})
min.node.size.dt <- rbindlist(list.min.node.size)

## 10% rule, highest number of min.node.size with least training 
# create 10% threshold
min.node.size.dt[, thresh := min.node.size.dt[which.min(pred.error)]$pred.error * 1.1]
best.min.node.size <- min.node.size.dt[pred.error < thresh][which.max(min.node.size)]$min.node.size

fig.node <- 
  ggplot(min.node.size.dt)+
  geom_point(aes(x = min.node.size, y = pred.error))+
  geom_point(aes(x = min.node.size, y = test.error), color = "red")+
  geom_hline(yintercept = min.node.size.dt[1]$thresh, color ="green")+
  ggtitle(paste0(model.name, " - min.node.size tuning"))
fig.node

# ggsave(fig.node,
#        file = paste0("data/PREDICTIONS/FINAL models/fig.node.", model.name, ".png"),
#        width = 4, height = 4, dpi = 300)



### ::: Tree depth optimization

## :: DERIVE TREE DEPTS

# calculate tree depth
compute_tree_depth <- function(tree_df) {
  
  # Create a named list to store depth of each node
  depth_map <- list("0" = 0)  # root node has depth 0
  
  # Loop over nodes in topological order (assuming nodeID is sorted)
  for (i in seq_len(nrow(tree_df))) {
    node <- tree_df[i, ]
    node_id <- as.character(node$nodeID)
    left <- as.character(node$leftChild)
    right <- as.character(node$rightChild)
    
    if (!is.na(left)) depth_map[[left]] <- depth_map[[node_id]] + 1
    if (!is.na(right)) depth_map[[right]] <- depth_map[[node_id]] + 1
  }
  
  max(unlist(depth_map))
}

# build model
rf.temp <- ranger(ndq ~ . ,
                  data = train_x[ , .SD, .SDcols = best.pred],
                  num.tree = best.ntree,
                  mtry = best.mtry,
                  min.node.size = best.min.node.size)


# calc tree info - ca 10 minutes (for metals)
approx.depth <- lapply(sample(1:best.ntree, size = 10), function(xx){
  
  # get tree
  tree <- treeInfo(rf.temp, tree = xx)
  # get depths
  depths <- compute_tree_depth(tree)
  # print progress
  print(xx)
  # export
  export <- data.table(tree = xx,
                       depth = depths)
  
  
})
depth.dt <- rbindlist(approx.depth)
rm(rf.temp, approx.depth)

# calc max, then do 10% downward steps
depth.tries <- c(max(depth.dt$depth), 
                 round(max(depth.dt$depth) * (1-c(.1,.2,.3,.4,.5)), 0))

list.max.depth<- lapply(depth.tries, function(xx){
  
  # build model
  rf.red <- ranger(ndq ~ . ,
                   data = train_x[ , .SD, .SDcols = best.pred],
                   num.tree = best.ntree,
                   mtry = best.mtry,
                   min.node.size = best.min.node.size,
                   max.depth = xx)
  
  # create secondary accuracy estiamte based on test data to further support decision process (not really needed due to internal OOB estimates but whatever.. for the reviewers i guess)
  test_x[, pred := predict(rf.red, test_x)$predictions]
  test.error <- 1 - (test_x[ndq == pred, .N] / test_x[, .N])
  # progress
  print(xx)
  
  # create results
  results <- data.table(max.depth = xx,
                        pred.error = rf.red$prediction.error,
                        test.error = test.error)
  
})
max.depth.dt <- rbindlist(list.max.depth)

## 1% rule, lowest number of tree.depth with least training error
# create 1% threshold
max.depth.dt[, thresh := max.depth.dt[which.min(pred.error)]$pred.error * 1.01]
best.max.depth <- max.depth.dt[pred.error < thresh][which.min(max.depth)]$max.depth

fig.max.depth <- 
  ggplot(max.depth.dt)+
  geom_point(aes(x = max.depth, y = pred.error))+
  geom_point(aes(x = max.depth, y = test.error), color = "red")+
  geom_hline(yintercept = max.depth.dt[1]$thresh, color ="green")+
  ggtitle(paste0(model.name, " - max.depth tuning"))
fig.max.depth

# ggsave(fig.max.depth,
#        file = paste0("data/PREDICTIONS/FINAL models/fig.max.depth.", model.name, ".png"),
#        width = 4, height = 4, dpi = 300)



### ::: Export combined tuning plots
fig.tuning <- grid.arrange(fig.fun.tuning,
                           fig.tree + theme(axis.title.y = element_blank()),
                           fig.node + theme(axis.title.y = element_blank()),
                           fig.max.depth + theme(axis.title.y = element_blank()),
                           ncol = 4,
                           nrow = 1)

ggsave(fig.tuning,
       file = paste0("data/PREDICTIONS/FINAL models/fig.", model.name, ".TUNING_det.png"),
       width = 18,
       height = 4.5,
       units = "cm")



### ::: 3.0 TRAIN FINAL MODEL AND EXPORT

## :: DEFINE CHEMICALS
# CONFIRM CURRENT CAS PRIOR TO TRAINING
cur.cas 

# reduce to relevant data
opti.data <- test.data[cas %in% cur.cas, ]

## : SEED
seed <- 1234
set.seed(1234)

## : CORES
ncores <- 8

### :: 2.0 Generate Training ad Testing Data
data_split <- splitTools::partition(
  y = opti.data[, ]$ndq,
  p = c(train = 0.7, test = 0.3),
  type = "stratified",
  seed = seed
)

## Training data
train_x <- opti.data[data_split$train]

## Testing data
test_x <- opti.data[data_split$test]

## : TRAIN MODEL
rf.metals <-  ranger(ndq ~ .,
                         data = train_x[ , .SD, .SDcols = best.pred],
                         num.trees = best.ntree, 
                         mtry = best.mtry,
                         min.node.size = best.min.node.size,
                         max.depth = best.max.depth,
                         importance = "permutation")

## : TRAIN MODEL - Probabilistic fo AUC / ROC derivation
rf.metals.prob <-  ranger(ndq ~ .,
                              data = train_x[ , .SD, .SDcols = best.pred],
                              num.trees = best.ntree, 
                              mtry = best.mtry,
                              min.node.size = best.min.node.size,
                              max.depth = best.max.depth,
                              prob = T)


test_x[ , pred := predict(rf.metals, test_x)$predictions]
test_x[, table(ndq == pred)][1] / test_x[, .N]   # no-overfit 

# Training accuracy
acc.test <- test_x[, table(ndq == pred)][1] / test_x[, .N]
acc.test <- unname(round(acc.test*100, 2))

## COMPARISON TO NULL MODEL
null.mod <- as.data.table(test_x[, table(ndq)])
acc.null <- round((null.mod[ndq == "X", ]$N / test_x[, .N])*100, 1)   # majority vote as null model
acc.full <- unname(round((test_x[, table(ndq == pred)][1] / test_x[, .N])*100, 1))

# Model imrpovement expressed as %decreased error rate
dec.error <- round(((acc.null - acc.full) / acc.null)*100, 2)

## Calculate Precision (P), Recall (R) and F1 score from confusion matrix
conf.mat <- as.data.table(rf.metals$confusion.matrix)

# PRECISION : P = TP / TP + FP
tp <- conf.mat[true == "N" & predicted == "N"]$N     # True positives (TP)
fp <- conf.mat[true == "X" & predicted == "N"]$N     # False positives (FP)
precision <- tp / (tp + fp)

# RECALL : R = TP / (TP + FN)
fn <- conf.mat[true == "N" & predicted == "X"]$N     # false negatives
recall <- tp / (tp + fn)

# F1-score : 2 * ((P * R) / (P + R))
f1.score <- 2 * ((precision * recall) / (precision + recall))

## :: EXPORT model with key metrics for later use
assign(paste0("export_", model.name, "_det"), list("model.name" = model.name,   # model name
                                                   "null.accuracy" = acc.null,  # accuracy of null model
                                                   "full.accuracy" = acc.full,  # accuracy of full model
                                                   "test.accuracy" = acc.test,  # accuracy on testing dataset
                                                   "precision" =  precision,    # precision of model
                                                   "recall" = recall,           # recall of model
                                                   "f1.score" = f1.score,       # F1 score of model
                                                   "dec.error" = dec.error,     # % decreased error rate compared to null model
                                                   "data.n" = opti.data[, .N],   # total number of samples available for training and testing
                                                   "data.summary" = opti.data[, .N, .(cas)],   # simple data summary
                                                   "full.model" = get(paste0("rf.",model.name)),
                                                   "prob.model" = get(paste0("rf.",model.name, ".prob"))))
# Final check
get(paste0("export_", model.name, "_det"))

## :: FINAL EXPORT OF DATA
saveRDS(get(paste0("export_", model.name, "_det")),
        file = paste0("data/PREDICTIONS/FINAL models/","export_", model.name, "_det.rds"))




# 3.0 Training / Tuning conc ---- 


## 3.1 Setup -----

library("caret")
library("splitTools")
library("Hmisc")
library("corrplot")

### ::: DATA PREP
test.data <- rbind(eu.data,
                   us.data)

# Training data needs to be reduced to >= 2010, no sense in training in very old data.
test.data <- test.data[year >= 2010]

# complete cases
test.data[, .N]         # 41655614

# All AquaSync CAS
my.cas <- aqua.data[!is.na(cas)]$cas

test.data <- test.data[cas %in% my.cas]
test.data[, .N]    # 13477303


## Unify detections
test.data[, .N, ndq]

# US detects now mostly carry concentrations and will be adjusted accordingly
test.data[ndq == "D" & !is.na(conc), ndq := "X"]   # convert to "quantifications" if they have an attributable concentration
test.data[ndq == "D" & is.na(conc), ndq := "N"]   # convert to "non-detects" if they have no attributable concentration
test.data <- droplevels(test.data)


## Unify detections
test.data[, .N, ndq]
# US detects carry no concentration and have to be converted to ND
test.data[ndq == "D", ndq := "N"]
test.data <- droplevels(test.data)

# Add ATLAS attributes
atlas.imp <- c("tmp_dc_smx", "pre_mm_syr", "slp_dg_uav", "ero_kh_uav", "run_mm_syr", "ppd_pk_uav")    # important atlas attributes
# atlas reduced
atlas.red <- atlas[, .SD, .SDcols = c("spat", atlas.imp)]
# merge
test.data <- merge(test.data,
                   atlas.red,
                   by = "spat",
                   all.x = T)

# Reduce data to concentrations only!
test.data <- test.data[ndq == "X"]

# complete cases 
test.data <- test.data[complete.cases(test.data[, -c("conc")])]  # 4261447 --> 4144350

# convert to log-concentration
test.data[, log.conc := log10(conc)]
summary(test.data$log.conc)  # perfect

# clean
rm(atlas.red)
gc()

## Final check of predictor correlation
# remove dups
spat.check <- test.data[!duplicated(spat)]
num.cols <- sapply(spat.check, class) %in% c("numeric", "integer")
spat.check <- spat.check[, .SD, .SDcols = num.cols]  # check
corr.m <- rcorr(as.matrix(spat.check))
corrplot(corr.m$r)

## Remove unnecessary cols
test.data <- test.data[, -c("spat", "chemical", "lat", "lng", "cont", "date", "nodata", "ndq", "conc")]

# ## Export data for later use
# saveRDS(test.data,
#         "data/PREDICTIONS/TRAIN-TEST-DATA-CONC.rds")


## Remove raw data or RAM issues (alloc)
# rm(eu.data, us.data)
gc()




## 3.2 Fungicides conc ----

## :: 0.1 Model name
model.name <- "fungicides"

## :: DEFINE CHEMICALS
cur.cas <- aqua.data[!is.na(cas) & chemical_group == "fungicide"]$cas

# decide about data reduction
test.data[cas %in% cur.cas, .N] < 3*10^5 # IF TRUE, no reduction!
test.data[cas %in% cur.cas, .N]

# reduce to relevant data
opti.data <- test.data[cas %in% cur.cas,]


# Reduce to a max of 500.000 entries using stratified sampling approach
if (opti.data[, .N] > 300000) {
  
  # calc relative size by which the data needs to be split
  rel.size <- 300000 / opti.data[, .N]
  
  # derive 
  index <- createDataPartition(opti.data$cas, 
                               p = rel.size, 
                               list = FALSE)
  
  opti.data <- opti.data[index]
} else {}

# Sanity check
san.check <- merge(opti.data[, .N, cas],
                   test.data[cas %in% cur.cas, .N, cas],
                   by = "cas",
                   all.x = T)
san.check[, rel := N.x / N.y]
san.check   # perfect
rm(san.check)


## :: Packs

## : SEED
seed <- 1234
set.seed(1234)

## : CORES
ncores <- 8

### :: 2.0 Generate Training ad Testing Data
data_split <- splitTools::partition(
  y = opti.data[, ]$cas,
  p = c(train = 0.7, test = 0.3),
  type = "stratified",
  seed = seed
)

## Training data
train_x <- opti.data[data_split$train]
## Testing data
test_x <- opti.data[data_split$test]


### ::: 1.0 - PREDICTOR REMOVAL --- PREDICTION OPTIMIZATION

## :: Define  mtry and tree
ntree <- 150

## Create base model - leave mtry by default to allow for predictor complexity-dependent mtry setting (square root)
start <- Sys.time()
base.model <- ranger(log.conc ~ . ,
                     data = train_x,
                     num.tree = ntree,
                     importance = "permutation")
stop <- Sys.time()  # ca. 1 minute
stop-start


# view
base.model  #  baseline performace

## :: IMPORTANCE metrics
base.model.imp <- data.table(pred = names(base.model$variable.importance),
                             imp = unname(base.model$variable.importance))
setorder(base.model.imp, imp)
base.model.imp$pred <- factor(base.model.imp$pred , levels = base.model.imp$pred)
# Importance plot
ggplot(base.model.imp, aes(x = imp, y = pred)) + geom_point() + coord_cartesian(xlim = c(0, NA))


# Create list of number of predictor reductions
num.red <- seq(from = 2 , 
               to = nrow(base.model.imp)-1,
               by = 2)

# worst predictors - example for testing
worst.pred <- base.model.imp[1:num.red[1]]$pred               # worst
train_x[, setdiff(names(train_x), c(worst.pred)), with = F]   # visual check


# Optimization using k-fold (3) predictor removal
optim.list <- lapply(num.red, function(xx){
  
  # create worst predictor vector
  worst.pred <- base.model.imp[1:xx]$pred  # worst
  # create fold indices
  num.folds <- 3
  fold.indices <- createFolds(train_x$log.conc, k = num.folds)
  
  list.fold <- lapply(1:num.folds, function(yy){
    
    # build model
    rf.red <- ranger(log.conc ~ . ,
                     data = train_x[fold.indices[[yy]], setdiff(names(train_x), c(worst.pred)), with = F],
                     num.tree = ntree)
    
    # create secondary accuracy estiamte based on test data to further support decision process (
    # not really needed due to internal OOB estimates but whatever.. for the reviewers i guess)
    test_x[, pred := predict(rf.red, test_x)$predictions]
    ## calc MSE
    mse <- mean((test_x$log.conc - test_x$pred)^2)
    
    ## calc R2
    # Mean of observed values
    mean_observed <- mean(test_x$log.conc)
    # SS_residual: Residual sum of squares
    ss_residual <- sum((test_x$log.conc - test_x$pred)^2)
    # SS_total: Total sum of squares
    ss_total <- sum((test_x$log.conc - mean_observed)^2)
    # R-squared calculation
    r2 <- 1 - (ss_residual / ss_total) 
    
    # create results
    results <- data.table(run = xx,
                          fold = yy,
                          pred.mse = rf.red$prediction.error,
                          pred.r2 = rf.red$r.squared,
                          test.mse = mse,
                          test.r2 = r2)
    
  })
  export <- rbindlist(list.fold)  # bind fold results
  
  # print progress
  print(paste(((round(xx / length(num.red),2))/2)*100, "% completed"))
  # export
  export
})
optim.dt <- rbindlist(optim.list)

# see optim
optim.dt   
optim.dt[, .(mean(pred.mse), 
             mean(test.mse),
             mean(pred.r2), 
             mean(test.r2)), run][which.min(V1)]  # currently - test error even smaller than training = NICE
# Model choice based on lowest MSE
optim.dt[, mean(pred.mse), run][order(V1)]
# Quick check if this also applies to the highest R2 - DO NOT USE TEST SET ACCURACY FOR EVALUATION, risk of data leakage!
optim.dt[, mean(pred.r2), run][order(-V1)]   # same, great!
# Take best run
best.run <- optim.dt[, mean(pred.mse), run][which.min(V1)]$run

# best predictors
base.model.imp[(best.run+1) : .N]  # highly complex currently best for fungicides
best.pred <- base.model.imp[(best.run+1) : .N]$pred

# Define best predictors!
best.pred <- c("log.conc",
               as.vector(best.pred))

### ::: ------ NEW SELF_WRITTEN OPTIMIZER


# required for grid search and initialization of bayesian optimization
parameter_grid <- expand.grid(
  num.trees = c(150),   # previous analyses showed that num.tree already maxes out at 100-130. Iterating over it is nonsensical as higher ntree always improve predictions slightly without any overfits. Final model trained on ntree = 500 as overfits were never observed
  mtry = seq(from = 2,
             to = ifelse((length(best.pred)-1) < 12,
                         length(best.pred)-1,
                         12),               # allow a maximum mtry of 10 with 26 predictors
             by = 1)                        # After preliminary assessments there are only 2 valuable hyperparameters: ntree and mtry. ntree already amxes out at 100-130 without overfits in 500-1000, data not shown
  # min.node.size = c(1, 2, 4)              # preliminary assessment showed no real improvement with min node size, rather defaults are the best choices. No use in parameter grid searching it.
  # max.depth = seq(1, 3, 5, 9, NA)         # preliminary assessment showed that maximum tree depths always outcompetes without risk of overfit. No use in parameter grid searching this
  # sample.fraction = seq(0.5, 0.8, 0.3)    # preliminary assessment showed no improvement compared to default classifier values set by ranger package. No use in iterating over it
)
# SUMMARY: 
# ntree = 150 showed across contaminant groups to already reach very high predicitive power, or, alternatively, marginal gains if further increased. 
# Iteration over ntree only increases the total number of permutations, hence for fast grid searching only mtry requires optimization
# all other common hyperparamters did not outperform model defaults or 


gc()

## K-fold cross validation (3) for parameter grid tuning

tuning.list <- lapply(1:nrow(parameter_grid), function(ii){
  
  # dump current parameters
  num.trees <- parameter_grid[ii,]$num.trees
  mtry <- parameter_grid[ii,]$mtry
  sample.fraction <- parameter_grid[ii,]$sample.fraction
  
  ## 3 iterations to derive mean error / accuracy per parameter combination (similar to k-folding)
  k.list <- lapply(1:3, function(xx){
    
    cur.mod <- ranger(log.conc ~ .,
                      data = train_x[ , .SD, .SDcols = best.pred],
                      num.trees = num.trees, 
                      mtry = mtry,
                      verbose = F)
    
    # create secondary accuracy estiamte based on test data to further support decision process (not really needed due to internal OOB estimates but whatever.. for the reviewers i guess)
    test_x[, pred := predict(cur.mod, test_x)$predictions]
    
    ## calc MSE
    mse <- mean((test_x$log.conc - test_x$pred)^2)
    
    ## calc R2
    # Mean of observed values
    mean_observed <- mean(test_x$log.conc)
    # SS_residual: Residual sum of squares
    ss_residual <- sum((test_x$log.conc - test_x$pred)^2)
    # SS_total: Total sum of squares
    ss_total <- sum((test_x$log.conc - mean_observed)^2)
    # R-squared calculation
    r2 <- 1 - (ss_residual / ss_total) 
    
    results <- data.table(id = ii,
                          run = xx,
                          num.trees = num.trees,
                          mtry = mtry,
                          pred.mse = cur.mod$prediction.error,
                          pred.r2 = cur.mod$r.squared,
                          test.mse = mse,
                          test.r2 = r2)
    
  })
  # print progress
  print(paste0("Progress: ", round(ii / nrow(parameter_grid) *100,0), "% completed"))
  # print current grid
  print(parameter_grid[ii,])
  rbindlist(k.list)
  
})
tuning.list

tuning.dt <- rbindlist(tuning.list)   #  

# Assess accuracies - check comparability of pred and test error.
tuning.dt[, .(mean(pred.mse), 
              mean(test.mse),
              mean(pred.r2), 
              mean(test.r2)), .(id, num.trees, mtry)][order(V1)]   # again both internal and external (test_set) accuracies are near identical
# Model choice based on lowest MSE
tuning.dt[, mean(pred.mse), id][order(V1)]
# Quick check if this also applies to the highest R2 - DO NOT USE TEST SET ACCURACY FOR EVALUATION, risk of data leakage!
tuning.dt[, mean(pred.r2), id][order(-V1)]   # same, great!

# keep best mtry
best.mtry <- tuning.dt[, mean(pred.mse), .(id, num.trees, mtry)][order(V1)][1]$mtry
gc()

## Visualize results
fig.fun.tuning <-
  ggplot(tuning.dt)+
  geom_point(aes(x = mtry,
                 y = pred.mse))+
  coord_cartesian(ylim = c(NA,NA))+
  scale_y_continuous(expand = expansion(mult = c(.1,.1)))+
  ylab("Prediction error [%]")+
  ggtitle(label = model.name)
fig.fun.tuning

# ggsave(fig.fun.tuning,
#        file = paste0("data/PREDICTIONS/FINAL models/fig.fun.tuning.", model.name, "_conc.png"),
#        width = 4, height = 4, dpi = 300)

## NTREE testing outside grid search
# REASON: no overfitting with up to 1000 trees
# Inclusion in grid search would massively increase training time to days/weeks

list.ntree <- lapply(c(50,100,150,200,250,300,400), function(xx){
  
  # build model
  rf.red <- ranger(log.conc ~ . ,
                   data = train_x[ , .SD, .SDcols = best.pred],
                   num.tree = xx,
                   mtry = best.mtry)
  
  print(xx)
  # create results
  results <- data.table(ntree = xx,
                        pred.error = rf.red$prediction.error)
  
})
ntree.dt <- rbindlist(list.ntree)

## 0.5% rule, lowerst number of trees with least training 
# create 1% threshold
ntree.dt[, thresh := ntree.dt[which.min(pred.error)]$pred.error * 1.005]
best.ntree <- ntree.dt[pred.error < thresh][which.min(ntree)]$ntree

fig.tree <- 
  ggplot(ntree.dt)+
  geom_point(aes(x = ntree, y = pred.error))+
  geom_hline(yintercept = ntree.dt[1]$thresh, color ="green")+
  ggtitle(model.name)
fig.tree

# ggsave(fig.tree,
#        file = paste0("data/PREDICTIONS/FINAL models/fig.trees.", model.name, "_conc.png"),
#        width = 4, height = 4, dpi = 300)





### ::: 3.0 TRAIN FINAL MODEL AND EXPORT

## :: DEFINE CHEMICALS
# CONFIRM CURRENT CAS PRIOR TO TRAINING
cur.cas 

# reduce to relevant data
opti.data <- test.data[cas %in% cur.cas,]

## : SEED
seed <- 1234
set.seed(1234)

## : CORES
ncores <- 8

### :: 2.0 Generate Training ad Testing Data
data_split <- splitTools::partition(
  y = opti.data[, ]$log.conc,
  p = c(train = 0.7, test = 0.3),
  type = "stratified",
  seed = seed
)

## Training data
train_x <- opti.data[data_split$train]

## Testing data
test_x <- opti.data[data_split$test]

## : TRAIN MODEL
rf.fungicides <-  ranger(log.conc ~ .,
                         data = train_x[ , .SD, .SDcols = best.pred],
                         num.trees = best.ntree, 
                         mtry = best.mtry,
                         importance = "permutation")


test_x[ , pred := predict(rf.fungicides, test_x)$predictions]
# Create residuals
test_x[, res := log.conc - pred]

## Accuracy plot
fig.model.acc <- 
  ggplot(test_x)+
  geom_density(aes(x = res), fill = "grey50", color = NA)+
  geom_vline(xintercept = c(-1, -0.5, 0, 0.5, 1),
             color = c("red", "orange", "green", "orange", "red"),
             linewidth = .1)+
  scale_y_continuous(expand = expansion(mult = c(0,.01)),
                     name = "Density")+
  xlab("Residuals [log10]")+
  ggtitle(capitalize(model.name))
fig.model.acc

# # Export
# ggsave(fig.model.acc,
#        file = paste0("data/PREDICTIONS/FINAL models/fig.", model.name, ".acc.png"),
#        width = 4,
#        height = 4,
#        dpi = 300)


fig.conc.tuning <- grid.arrange(fig.fun.tuning,
                                fig.tree,
                                fig.model.acc,
                                ncol = 3,
                                nrow = 1)

ggsave(fig.conc.tuning,
       file = paste0("data/PREDICTIONS/FINAL models/fig.", model.name, ".CONC.TUNING_det.png"),
       width = 14,
       height = 4.5,
       units = "cm")

# Calculate Accuracy estimates in OOM
oom05 <- round((test_x[res >= -0.5 & res <= 0.5, .N] / test_x[, .N])*100, 2)   # 87.46% of concentrations within .5 OOM
oom1 <- round((test_x[res >= -1 & res <= 1, .N] / test_x[, .N])*100, 2)        # 97.77 of concentrations within 1 OOM - excellent

### NULL vs FULL model
## calc MSE
full.mse <- mean((test_x$res)^2)
null.mse <- mean((test_x$log.conc - mean(test_x$log.conc))^2)    # null MSE == MSE of acutal vs. average conc
# relative Improvement over Null Model (RI)
dec.mse <- round(((null.mse - full.mse) / null.mse)*100, 2)      # RI = decreased MSE for full vs. null model

## Mean Absolute Error
null.mae <- sum(abs(test_x$log.conc - mean(test_x$log.conc)))/ test_x[, .N]
full.mae <- sum(abs(test_x$res))/ test_x[, .N]


## Calc Full R2 (train and test)
# Training R2
pred.r2 <- rf.fungicides$r.squared

# Testing R2
mean_observed <- mean(test_x$log.conc)                # Mean of observed values
ss_residual <- sum((test_x$log.conc - test_x$pred)^2) # SS_residual: Residual sum of squares
ss_total <- sum((test_x$log.conc - mean_observed)^2)  # SS_total: Total sum of squares
# R-squared calculation
test.r2 <- 1 - (ss_residual / ss_total) 


## :: EXPORT model with key metrics for later use
assign(paste0("export_", model.name, "_conc"), list("model.name" = model.name,    # model name
                                                   "null.mse" = null.mse,        # error of null model (MSE)
                                                   "full.mse" = full.mse,        # error of full model (MSE)
                                                   "dec.mse" = dec.mse,          # accuracy improvement (relative; RI)
                                                   "null.mae" =  null.mae,       # null mean absolute error (MAE)
                                                   "pred.r2" = pred.r2,          # R2 of training data
                                                   "test.r2" = test.r2,          # R2 of testing data
                                                   "data.n" = opti.data[, .N],   # total number of samples available for training and testing
                                                   "data.summary" = opti.data[, .N, .(cas)],   # simple data summary
                                                   "full.model" = get(paste0("rf.",model.name))))
# Final check
get(paste0("export_", model.name, "_conc"))

## :: FINAL EXPORT OF DATA
saveRDS(get(paste0("export_", model.name, "_conc")),
        file = paste0("data/PREDICTIONS/FINAL models/","export_", model.name, "_conc.rds"))




## 3.3 Insecticides conc ----

## :: 0.1 Model name
model.name <- "insecticides"

## :: DEFINE CHEMICALS
cur.cas <- aqua.data[!is.na(cas) & chemical_group == "insecticide"]$cas

# decide about data reduction
test.data[cas %in% cur.cas, .N] < 3*10^5 # IF TRUE, no reduction!
test.data[cas %in% cur.cas, .N]

# reduce to relevant data
opti.data <- test.data[cas %in% cur.cas,]


# Reduce to a max of 500.000 entries using stratified sampling approach
if (opti.data[, .N] > 300000) {
  
  # calc relative size by which the data needs to be split
  rel.size <- 300000 / opti.data[, .N]
  
  # derive 
  index <- createDataPartition(opti.data$cas, 
                               p = rel.size, 
                               list = FALSE)
  
  opti.data <- opti.data[index]
} else {}

# Sanity check
san.check <- merge(opti.data[, .N, cas],
                   test.data[cas %in% cur.cas, .N, cas],
                   by = "cas",
                   all.x = T)
san.check[, rel := N.x / N.y]
san.check   # perfect
rm(san.check)


## :: Packs

## : SEED
seed <- 1234
set.seed(1234)

## : CORES
ncores <- 8

### :: 2.0 Generate Training ad Testing Data
data_split <- splitTools::partition(
  y = opti.data[, ]$cas,
  p = c(train = 0.7, test = 0.3),
  type = "stratified",
  seed = seed
)

## Training data
train_x <- opti.data[data_split$train]
## Testing data
test_x <- opti.data[data_split$test]


### ::: 1.0 - PREDICTOR REMOVAL --- PREDICTION OPTIMIZATION

## :: Define  mtry and tree
ntree <- 150

## Create base model - leave mtry by default to allow for predictor complexity-dependent mtry setting (square root)
start <- Sys.time()
base.model <- ranger(log.conc ~ . ,
                     data = train_x,
                     num.tree = ntree,
                     importance = "permutation")
stop <- Sys.time()  # ca. 1 minute
stop-start


# view
base.model  #  baseline performace

## :: IMPORTANCE metrics
base.model.imp <- data.table(pred = names(base.model$variable.importance),
                             imp = unname(base.model$variable.importance))
setorder(base.model.imp, imp)
base.model.imp$pred <- factor(base.model.imp$pred , levels = base.model.imp$pred)
# Importance plot
ggplot(base.model.imp, aes(x = imp, y = pred)) + geom_point() + coord_cartesian(xlim = c(0, NA))


# Create list of number of predictor reductions
num.red <- seq(from = 2 , 
               to = nrow(base.model.imp)-1,
               by = 2)

# worst predictors - example for testing
worst.pred <- base.model.imp[1:num.red[1]]$pred               # worst
train_x[, setdiff(names(train_x), c(worst.pred)), with = F]   # visual check


# Optimization using k-fold (3) predictor removal
optim.list <- lapply(num.red, function(xx){
  
  # create worst predictor vector
  worst.pred <- base.model.imp[1:xx]$pred  # worst
  # create fold indices
  num.folds <- 3
  fold.indices <- createFolds(train_x$log.conc, k = num.folds)
  
  list.fold <- lapply(1:num.folds, function(yy){
    
    # build model
    rf.red <- ranger(log.conc ~ . ,
                     data = train_x[fold.indices[[yy]], setdiff(names(train_x), c(worst.pred)), with = F],
                     num.tree = ntree)
    
    # create secondary accuracy estiamte based on test data to further support decision process (
    # not really needed due to internal OOB estimates but whatever.. for the reviewers i guess)
    test_x[, pred := predict(rf.red, test_x)$predictions]
    ## calc MSE
    mse <- mean((test_x$log.conc - test_x$pred)^2)
    
    ## calc R2
    # Mean of observed values
    mean_observed <- mean(test_x$log.conc)
    # SS_residual: Residual sum of squares
    ss_residual <- sum((test_x$log.conc - test_x$pred)^2)
    # SS_total: Total sum of squares
    ss_total <- sum((test_x$log.conc - mean_observed)^2)
    # R-squared calculation
    r2 <- 1 - (ss_residual / ss_total) 
    
    # create results
    results <- data.table(run = xx,
                          fold = yy,
                          pred.mse = rf.red$prediction.error,
                          pred.r2 = rf.red$r.squared,
                          test.mse = mse,
                          test.r2 = r2)
    
  })
  export <- rbindlist(list.fold)  # bind fold results
  
  # print progress
  print(paste(((round(xx / length(num.red),2))/2)*100, "% completed"))
  # export
  export
})
optim.dt <- rbindlist(optim.list)

# see optim
optim.dt   
optim.dt[, .(mean(pred.mse), 
             mean(test.mse),
             mean(pred.r2), 
             mean(test.r2)), run][which.min(V1)]  # currently - test error even smaller than training = NICE
# Model choice based on lowest MSE
optim.dt[, mean(pred.mse), run][order(V1)]
# Quick check if this also applies to the highest R2 - DO NOT USE TEST SET ACCURACY FOR EVALUATION, risk of data leakage!
optim.dt[, mean(pred.r2), run][order(-V1)]   # same, great!
# Take best run
best.run <- optim.dt[, mean(pred.mse), run][which.min(V1)]$run

# best predictors
base.model.imp[(best.run+1) : .N]  # highly complex currently best for insecticides
best.pred <- base.model.imp[(best.run+1) : .N]$pred

# Define best predictors!
best.pred <- c("log.conc",
               as.vector(best.pred))

### ::: ------ NEW SELF_WRITTEN OPTIMIZER


# required for grid search and initialization of bayesian optimization
parameter_grid <- expand.grid(
  num.trees = c(150),   # previous analyses showed that num.tree already maxes out at 100-130. Iterating over it is nonsensical as higher ntree always improve predictions slightly without any overfits. Final model trained on ntree = 500 as overfits were never observed
  mtry = seq(from = 2,
             to = ifelse((length(best.pred)-1) < 12,
                         length(best.pred)-1,
                         12),               # allow a maximum mtry of 10 with 26 predictors
             by = 1)                        # After preliminary assessments there are only 2 valuable hyperparameters: ntree and mtry. ntree already amxes out at 100-130 without overfits in 500-1000, data not shown
  # min.node.size = c(1, 2, 4)              # preliminary assessment showed no real improvement with min node size, rather defaults are the best choices. No use in parameter grid searching it.
  # max.depth = seq(1, 3, 5, 9, NA)         # preliminary assessment showed that maximum tree depths always outcompetes without risk of overfit. No use in parameter grid searching this
  # sample.fraction = seq(0.5, 0.8, 0.3)    # preliminary assessment showed no improvement compared to default classifier values set by ranger package. No use in iterating over it
)
# SUMMARY: 
# ntree = 150 showed across contaminant groups to already reach very high predicitive power, or, alternatively, marginal gains if further increased. 
# Iteration over ntree only increases the total number of permutations, hence for fast grid searching only mtry requires optimization
# all other common hyperparamters did not outperform model defaults or 


gc()

## K-fold cross validation (3) for parameter grid tuning

tuning.list <- lapply(1:nrow(parameter_grid), function(ii){
  
  # dump current parameters
  num.trees <- parameter_grid[ii,]$num.trees
  mtry <- parameter_grid[ii,]$mtry
  sample.fraction <- parameter_grid[ii,]$sample.fraction
  
  ## 3 iterations to derive mean error / accuracy per parameter combination (similar to k-folding)
  k.list <- lapply(1:3, function(xx){
    
    cur.mod <- ranger(log.conc ~ .,
                      data = train_x[ , .SD, .SDcols = best.pred],
                      num.trees = num.trees, 
                      mtry = mtry,
                      verbose = F)
    
    # create secondary accuracy estiamte based on test data to further support decision process (not really needed due to internal OOB estimates but whatever.. for the reviewers i guess)
    test_x[, pred := predict(cur.mod, test_x)$predictions]
    
    ## calc MSE
    mse <- mean((test_x$log.conc - test_x$pred)^2)
    
    ## calc R2
    # Mean of observed values
    mean_observed <- mean(test_x$log.conc)
    # SS_residual: Residual sum of squares
    ss_residual <- sum((test_x$log.conc - test_x$pred)^2)
    # SS_total: Total sum of squares
    ss_total <- sum((test_x$log.conc - mean_observed)^2)
    # R-squared calculation
    r2 <- 1 - (ss_residual / ss_total) 
    
    results <- data.table(id = ii,
                          run = xx,
                          num.trees = num.trees,
                          mtry = mtry,
                          pred.mse = cur.mod$prediction.error,
                          pred.r2 = cur.mod$r.squared,
                          test.mse = mse,
                          test.r2 = r2)
    
  })
  # print progress
  print(paste0("Progress: ", round(ii / nrow(parameter_grid) *100,0), "% completed"))
  # print current grid
  print(parameter_grid[ii,])
  rbindlist(k.list)
  
})
tuning.list

tuning.dt <- rbindlist(tuning.list)   #  

# Assess accuracies - check comparability of pred and test error.
tuning.dt[, .(mean(pred.mse), 
              mean(test.mse),
              mean(pred.r2), 
              mean(test.r2)), .(id, num.trees, mtry)][order(V1)]   # again both internal and external (test_set) accuracies are near identical
# Model choice based on lowest MSE
tuning.dt[, mean(pred.mse), id][order(V1)]
# Quick check if this also applies to the highest R2 - DO NOT USE TEST SET ACCURACY FOR EVALUATION, risk of data leakage!
tuning.dt[, mean(pred.r2), id][order(-V1)]   # same, great!

# keep best mtry
best.mtry <- tuning.dt[, mean(pred.mse), .(id, num.trees, mtry)][order(V1)][1]$mtry
gc()

## Visualize results
fig.fun.tuning <-
  ggplot(tuning.dt)+
  geom_point(aes(x = mtry,
                 y = pred.mse))+
  coord_cartesian(ylim = c(NA,NA))+
  scale_y_continuous(expand = expansion(mult = c(.1,.1)))+
  ylab("Prediction error [%]")+
  ggtitle(label = model.name)
fig.fun.tuning

# ggsave(fig.fun.tuning,
#        file = paste0("data/PREDICTIONS/FINAL models/fig.fun.tuning.", model.name, "_conc.png"),
#        width = 4, height = 4, dpi = 300)

## NTREE testing outside grid search
# REASON: no overfitting with up to 1000 trees
# Inclusion in grid search would massively increase training time to days/weeks

list.ntree <- lapply(c(50,100,150,200,250,300,400), function(xx){
  
  # build model
  rf.red <- ranger(log.conc ~ . ,
                   data = train_x[ , .SD, .SDcols = best.pred],
                   num.tree = xx,
                   mtry = best.mtry)
  
  print(xx)
  # create results
  results <- data.table(ntree = xx,
                        pred.error = rf.red$prediction.error)
  
})
ntree.dt <- rbindlist(list.ntree)

## 0.5% rule, lowerst number of trees with least training 
# create 1% threshold
ntree.dt[, thresh := ntree.dt[which.min(pred.error)]$pred.error * 1.005]
best.ntree <- ntree.dt[pred.error < thresh][which.min(ntree)]$ntree

fig.tree <- 
  ggplot(ntree.dt)+
  geom_point(aes(x = ntree, y = pred.error))+
  geom_hline(yintercept = ntree.dt[1]$thresh, color ="green")+
  ggtitle(model.name)
fig.tree

# ggsave(fig.tree,
#        file = paste0("data/PREDICTIONS/FINAL models/fig.trees.", model.name, "_conc.png"),
#        width = 4, height = 4, dpi = 300)





### ::: 3.0 TRAIN FINAL MODEL AND EXPORT

## :: DEFINE CHEMICALS
# CONFIRM CURRENT CAS PRIOR TO TRAINING
cur.cas 

# reduce to relevant data
opti.data <- test.data[cas %in% cur.cas,]

## : SEED
seed <- 1234
set.seed(1234)

## : CORES
ncores <- 8

### :: 2.0 Generate Training ad Testing Data
data_split <- splitTools::partition(
  y = opti.data[, ]$log.conc,
  p = c(train = 0.7, test = 0.3),
  type = "stratified",
  seed = seed
)

## Training data
train_x <- opti.data[data_split$train]

## Testing data
test_x <- opti.data[data_split$test]

## : TRAIN MODEL
rf.insecticides <-  ranger(log.conc ~ .,
                         data = train_x[ , .SD, .SDcols = best.pred],
                         num.trees = best.ntree, 
                         mtry = best.mtry,
                         importance = "permutation")


test_x[ , pred := predict(rf.insecticides, test_x)$predictions]
# Create residuals
test_x[, res := log.conc - pred]

## Accuracy plot
fig.model.acc <- 
  ggplot(test_x)+
  geom_density(aes(x = res), fill = "grey50", color = NA)+
  geom_vline(xintercept = c(-1, -0.5, 0, 0.5, 1),
             color = c("red", "orange", "green", "orange", "red"),
             linewidth = .1)+
  scale_y_continuous(expand = expansion(mult = c(0,.01)),
                     name = "Density")+
  xlab("Residuals [log10]")+
  ggtitle(capitalize(model.name))
fig.model.acc

# # Export
# ggsave(fig.model.acc,
#        file = paste0("data/PREDICTIONS/FINAL models/fig.", model.name, ".acc.png"),
#        width = 4,
#        height = 4,
#        dpi = 300)


fig.conc.tuning <- grid.arrange(fig.fun.tuning,
                                fig.tree,
                                fig.model.acc,
                                ncol = 3,
                                nrow = 1)

ggsave(fig.conc.tuning,
       file = paste0("data/PREDICTIONS/FINAL models/fig.", model.name, ".CONC.TUNING_det.png"),
       width = 14,
       height = 4.5,
       units = "cm")



# Calculate Accuracy estimates in OOM
oom05 <- round((test_x[res >= -0.5 & res <= 0.5, .N] / test_x[, .N])*100, 2)   # 87.46% of concentrations within .5 OOM
oom1 <- round((test_x[res >= -1 & res <= 1, .N] / test_x[, .N])*100, 2)        # 97.77 of concentrations within 1 OOM - excellent

### NULL vs FULL model
## calc MSE
full.mse <- mean((test_x$res)^2)
null.mse <- mean((test_x$log.conc - mean(test_x$log.conc))^2)    # null MSE == MSE of acutal vs. average conc
# relative Improvement over Null Model (RI)
dec.mse <- round(((null.mse - full.mse) / null.mse)*100, 2)      # RI = decreased MSE for full vs. null model

## Mean Absolute Error
null.mae <- sum(abs(test_x$log.conc - mean(test_x$log.conc)))/ test_x[, .N]
full.mae <- sum(abs(test_x$res))/ test_x[, .N]


## Calc Full R2 (train and test)
# Training R2
pred.r2 <- rf.insecticides$r.squared

# Testing R2
mean_observed <- mean(test_x$log.conc)                # Mean of observed values
ss_residual <- sum((test_x$log.conc - test_x$pred)^2) # SS_residual: Residual sum of squares
ss_total <- sum((test_x$log.conc - mean_observed)^2)  # SS_total: Total sum of squares
# R-squared calculation
test.r2 <- 1 - (ss_residual / ss_total) 


## :: EXPORT model with key metrics for later use
assign(paste0("export_", model.name, "_conc"), list("model.name" = model.name,    # model name
                                                   "null.mse" = null.mse,        # error of null model (MSE)
                                                   "full.mse" = full.mse,        # error of full model (MSE)
                                                   "dec.mse" = dec.mse,          # accuracy improvement (relative; RI)
                                                   "null.mae" =  null.mae,       # null mean absolute error (MAE)
                                                   "pred.r2" = pred.r2,          # R2 of training data
                                                   "test.r2" = test.r2,          # R2 of testing data
                                                   "data.n" = opti.data[, .N],   # total number of samples available for training and testing
                                                   "data.summary" = opti.data[, .N, .(cas)],   # simple data summary
                                                   "full.model" = get(paste0("rf.",model.name))))
# Final check
get(paste0("export_", model.name, "_conc"))

## :: FINAL EXPORT OF DATA
saveRDS(get(paste0("export_", model.name, "_conc")),
        file = paste0("data/PREDICTIONS/FINAL models/","export_", model.name, "_conc.rds"))



## 3.4 Herbicides conc ----

## :: 0.1 Model name
model.name <- "herbicides"

## :: DEFINE CHEMICALS
cur.cas <- aqua.data[!is.na(cas) & chemical_group == "herbicide"]$cas

# decide about data reduction
test.data[cas %in% cur.cas, .N] < 3*10^5 # IF TRUE, no reduction!
test.data[cas %in% cur.cas, .N]

# reduce to relevant data
opti.data <- test.data[cas %in% cur.cas,]


# Reduce to a max of 500.000 entries using stratified sampling approach
if (opti.data[, .N] > 300000) {
  
  # calc relative size by which the data needs to be split
  rel.size <- 300000 / opti.data[, .N]
  
  # derive 
  index <- createDataPartition(opti.data$cas, 
                               p = rel.size, 
                               list = FALSE)
  
  opti.data <- opti.data[index]
} else {}

# Sanity check
san.check <- merge(opti.data[, .N, cas],
                   test.data[cas %in% cur.cas, .N, cas],
                   by = "cas",
                   all.x = T)
san.check[, rel := N.x / N.y]
san.check   # perfect
rm(san.check)


## :: Packs

## : SEED
seed <- 1234
set.seed(1234)

## : CORES
ncores <- 8

### :: 2.0 Generate Training ad Testing Data
data_split <- splitTools::partition(
  y = opti.data[, ]$cas,
  p = c(train = 0.7, test = 0.3),
  type = "stratified",
  seed = seed
)

## Training data
train_x <- opti.data[data_split$train]
## Testing data
test_x <- opti.data[data_split$test]


### ::: 1.0 - PREDICTOR REMOVAL --- PREDICTION OPTIMIZATION

## :: Define  mtry and tree
ntree <- 150

## Create base model - leave mtry by default to allow for predictor complexity-dependent mtry setting (square root)
start <- Sys.time()
base.model <- ranger(log.conc ~ . ,
                     data = train_x,
                     num.tree = ntree,
                     importance = "permutation")
stop <- Sys.time()  # ca. 1 minute
stop-start


# view
base.model  #  baseline performace

## :: IMPORTANCE metrics
base.model.imp <- data.table(pred = names(base.model$variable.importance),
                             imp = unname(base.model$variable.importance))
setorder(base.model.imp, imp)
base.model.imp$pred <- factor(base.model.imp$pred , levels = base.model.imp$pred)
# Importance plot
ggplot(base.model.imp, aes(x = imp, y = pred)) + geom_point() + coord_cartesian(xlim = c(0, NA))


# Create list of number of predictor reductions
num.red <- seq(from = 2 , 
               to = nrow(base.model.imp)-1,
               by = 2)

# worst predictors - example for testing
worst.pred <- base.model.imp[1:num.red[1]]$pred               # worst
train_x[, setdiff(names(train_x), c(worst.pred)), with = F]   # visual check


# Optimization using k-fold (3) predictor removal
optim.list <- lapply(num.red, function(xx){
  
  # create worst predictor vector
  worst.pred <- base.model.imp[1:xx]$pred  # worst
  # create fold indices
  num.folds <- 3
  fold.indices <- createFolds(train_x$log.conc, k = num.folds)
  
  list.fold <- lapply(1:num.folds, function(yy){
    
    # build model
    rf.red <- ranger(log.conc ~ . ,
                     data = train_x[fold.indices[[yy]], setdiff(names(train_x), c(worst.pred)), with = F],
                     num.tree = ntree)
    
    # create secondary accuracy estiamte based on test data to further support decision process (
    # not really needed due to internal OOB estimates but whatever.. for the reviewers i guess)
    test_x[, pred := predict(rf.red, test_x)$predictions]
    ## calc MSE
    mse <- mean((test_x$log.conc - test_x$pred)^2)
    
    ## calc R2
    # Mean of observed values
    mean_observed <- mean(test_x$log.conc)
    # SS_residual: Residual sum of squares
    ss_residual <- sum((test_x$log.conc - test_x$pred)^2)
    # SS_total: Total sum of squares
    ss_total <- sum((test_x$log.conc - mean_observed)^2)
    # R-squared calculation
    r2 <- 1 - (ss_residual / ss_total) 
    
    # create results
    results <- data.table(run = xx,
                          fold = yy,
                          pred.mse = rf.red$prediction.error,
                          pred.r2 = rf.red$r.squared,
                          test.mse = mse,
                          test.r2 = r2)
    
  })
  export <- rbindlist(list.fold)  # bind fold results
  
  # print progress
  print(paste(((round(xx / length(num.red),2))/2)*100, "% completed"))
  # export
  export
})
optim.dt <- rbindlist(optim.list)

# see optim
optim.dt   
optim.dt[, .(mean(pred.mse), 
             mean(test.mse),
             mean(pred.r2), 
             mean(test.r2)), run][which.min(V1)]  # currently - test error even smaller than training = NICE
# Model choice based on lowest MSE
optim.dt[, mean(pred.mse), run][order(V1)]
# Quick check if this also applies to the highest R2 - DO NOT USE TEST SET ACCURACY FOR EVALUATION, risk of data leakage!
optim.dt[, mean(pred.r2), run][order(-V1)]   # same, great!
# Take best run
best.run <- optim.dt[, mean(pred.mse), run][which.min(V1)]$run

# best predictors
base.model.imp[(best.run+1) : .N]  # highly complex currently best for herbicides
best.pred <- base.model.imp[(best.run+1) : .N]$pred

# Define best predictors!
best.pred <- c("log.conc",
               as.vector(best.pred))

### ::: ------ NEW SELF_WRITTEN OPTIMIZER


# required for grid search and initialization of bayesian optimization
parameter_grid <- expand.grid(
  num.trees = c(150),   # previous analyses showed that num.tree already maxes out at 100-130. Iterating over it is nonsensical as higher ntree always improve predictions slightly without any overfits. Final model trained on ntree = 500 as overfits were never observed
  mtry = seq(from = 2,
             to = ifelse((length(best.pred)-1) < 12,
                         length(best.pred)-1,
                         12),               # allow a maximum mtry of 10 with 26 predictors
             by = 1)                        # After preliminary assessments there are only 2 valuable hyperparameters: ntree and mtry. ntree already amxes out at 100-130 without overfits in 500-1000, data not shown
  # min.node.size = c(1, 2, 4)              # preliminary assessment showed no real improvement with min node size, rather defaults are the best choices. No use in parameter grid searching it.
  # max.depth = seq(1, 3, 5, 9, NA)         # preliminary assessment showed that maximum tree depths always outcompetes without risk of overfit. No use in parameter grid searching this
  # sample.fraction = seq(0.5, 0.8, 0.3)    # preliminary assessment showed no improvement compared to default classifier values set by ranger package. No use in iterating over it
)
# SUMMARY: 
# ntree = 150 showed across contaminant groups to already reach very high predicitive power, or, alternatively, marginal gains if further increased. 
# Iteration over ntree only increases the total number of permutations, hence for fast grid searching only mtry requires optimization
# all other common hyperparamters did not outperform model defaults or 


gc()

## K-fold cross validation (3) for parameter grid tuning

tuning.list <- lapply(1:nrow(parameter_grid), function(ii){
  
  # dump current parameters
  num.trees <- parameter_grid[ii,]$num.trees
  mtry <- parameter_grid[ii,]$mtry
  sample.fraction <- parameter_grid[ii,]$sample.fraction
  
  ## 3 iterations to derive mean error / accuracy per parameter combination (similar to k-folding)
  k.list <- lapply(1:3, function(xx){
    
    cur.mod <- ranger(log.conc ~ .,
                      data = train_x[ , .SD, .SDcols = best.pred],
                      num.trees = num.trees, 
                      mtry = mtry,
                      verbose = F)
    
    # create secondary accuracy estiamte based on test data to further support decision process (not really needed due to internal OOB estimates but whatever.. for the reviewers i guess)
    test_x[, pred := predict(cur.mod, test_x)$predictions]
    
    ## calc MSE
    mse <- mean((test_x$log.conc - test_x$pred)^2)
    
    ## calc R2
    # Mean of observed values
    mean_observed <- mean(test_x$log.conc)
    # SS_residual: Residual sum of squares
    ss_residual <- sum((test_x$log.conc - test_x$pred)^2)
    # SS_total: Total sum of squares
    ss_total <- sum((test_x$log.conc - mean_observed)^2)
    # R-squared calculation
    r2 <- 1 - (ss_residual / ss_total) 
    
    results <- data.table(id = ii,
                          run = xx,
                          num.trees = num.trees,
                          mtry = mtry,
                          pred.mse = cur.mod$prediction.error,
                          pred.r2 = cur.mod$r.squared,
                          test.mse = mse,
                          test.r2 = r2)
    
  })
  # print progress
  print(paste0("Progress: ", round(ii / nrow(parameter_grid) *100,0), "% completed"))
  # print current grid
  print(parameter_grid[ii,])
  rbindlist(k.list)
  
})
tuning.list

tuning.dt <- rbindlist(tuning.list)   #  

# Assess accuracies - check comparability of pred and test error.
tuning.dt[, .(mean(pred.mse), 
              mean(test.mse),
              mean(pred.r2), 
              mean(test.r2)), .(id, num.trees, mtry)][order(V1)]   # again both internal and external (test_set) accuracies are near identical
# Model choice based on lowest MSE
tuning.dt[, mean(pred.mse), id][order(V1)]
# Quick check if this also applies to the highest R2 - DO NOT USE TEST SET ACCURACY FOR EVALUATION, risk of data leakage!
tuning.dt[, mean(pred.r2), id][order(-V1)]   # same, great!

# keep best mtry
best.mtry <- tuning.dt[, mean(pred.mse), .(id, num.trees, mtry)][order(V1)][1]$mtry
gc()

## Visualize results
fig.fun.tuning <-
  ggplot(tuning.dt)+
  geom_point(aes(x = mtry,
                 y = pred.mse))+
  coord_cartesian(ylim = c(NA,NA))+
  scale_y_continuous(expand = expansion(mult = c(.1,.1)))+
  ylab("Prediction error [%]")+
  ggtitle(label = model.name)
fig.fun.tuning

# ggsave(fig.fun.tuning,
#        file = paste0("data/PREDICTIONS/FINAL models/fig.fun.tuning.", model.name, "_conc.png"),
#        width = 4, height = 4, dpi = 300)

## NTREE testing outside grid search
# REASON: no overfitting with up to 1000 trees
# Inclusion in grid search would massively increase training time to days/weeks

list.ntree <- lapply(c(50,100,150,200,250,300,400), function(xx){
  
  # build model
  rf.red <- ranger(log.conc ~ . ,
                   data = train_x[ , .SD, .SDcols = best.pred],
                   num.tree = xx,
                   mtry = best.mtry)
  
  print(xx)
  # create results
  results <- data.table(ntree = xx,
                        pred.error = rf.red$prediction.error)
  
})
ntree.dt <- rbindlist(list.ntree)

## 0.5% rule, lowerst number of trees with least training 
# create 1% threshold
ntree.dt[, thresh := ntree.dt[which.min(pred.error)]$pred.error * 1.005]
best.ntree <- ntree.dt[pred.error < thresh][which.min(ntree)]$ntree

fig.tree <- 
  ggplot(ntree.dt)+
  geom_point(aes(x = ntree, y = pred.error))+
  geom_hline(yintercept = ntree.dt[1]$thresh, color ="green")+
  ggtitle(model.name)
fig.tree

# ggsave(fig.tree,
#        file = paste0("data/PREDICTIONS/FINAL models/fig.trees.", model.name, "_conc.png"),
#        width = 4, height = 4, dpi = 300)



### ::: 3.0 TRAIN FINAL MODEL AND EXPORT

## :: DEFINE CHEMICALS
# CONFIRM CURRENT CAS PRIOR TO TRAINING
cur.cas 

# reduce to relevant data
opti.data <- test.data[cas %in% cur.cas,]

## : SEED
seed <- 1234
set.seed(1234)

## : CORES
ncores <- 8

### :: 2.0 Generate Training ad Testing Data
data_split <- splitTools::partition(
  y = opti.data[, ]$log.conc,
  p = c(train = 0.7, test = 0.3),
  type = "stratified",
  seed = seed
)

## Training data
train_x <- opti.data[data_split$train]

## Testing data
test_x <- opti.data[data_split$test]

## : TRAIN MODEL
rf.herbicides <-  ranger(log.conc ~ .,
                         data = train_x[ , .SD, .SDcols = best.pred],
                         num.trees = best.ntree, 
                         mtry = best.mtry,
                         importance = "permutation")


test_x[ , pred := predict(rf.herbicides, test_x)$predictions]
# Create residuals
test_x[, res := log.conc - pred]

## Accuracy plot
fig.model.acc <- 
  ggplot(test_x)+
  geom_density(aes(x = res), fill = "grey50", color = NA)+
  geom_vline(xintercept = c(-1, -0.5, 0, 0.5, 1),
             color = c("red", "orange", "green", "orange", "red"),
             linewidth = .1)+
  scale_y_continuous(expand = expansion(mult = c(0,.01)),
                     name = "Density")+
  xlab("Residuals [log10]")+
  ggtitle(capitalize(model.name))
fig.model.acc

# # Export
# ggsave(fig.model.acc,
#        file = paste0("data/PREDICTIONS/FINAL models/fig.", model.name, ".acc.png"),
#        width = 4,
#        height = 4,
#        dpi = 300)

fig.conc.tuning <- grid.arrange(fig.fun.tuning,
                                fig.tree,
                                fig.model.acc,
                                ncol = 3,
                                nrow = 1)

ggsave(fig.conc.tuning,
       file = paste0("data/PREDICTIONS/FINAL models/fig.", model.name, ".CONC.TUNING_det.png"),
       width = 14,
       height = 4.5,
       units = "cm")


# Calculate Accuracy estimates in OOM
oom05 <- round((test_x[res >= -0.5 & res <= 0.5, .N] / test_x[, .N])*100, 2)   # 87.46% of concentrations within .5 OOM
oom1 <- round((test_x[res >= -1 & res <= 1, .N] / test_x[, .N])*100, 2)        # 97.77 of concentrations within 1 OOM - excellent

### NULL vs FULL model
## calc MSE
full.mse <- mean((test_x$res)^2)
null.mse <- mean((test_x$log.conc - mean(test_x$log.conc))^2)    # null MSE == MSE of acutal vs. average conc
# relative Improvement over Null Model (RI)
dec.mse <- round(((null.mse - full.mse) / null.mse)*100, 2)      # RI = decreased MSE for full vs. null model

## Mean Absolute Error
null.mae <- sum(abs(test_x$log.conc - mean(test_x$log.conc)))/ test_x[, .N]
full.mae <- sum(abs(test_x$res))/ test_x[, .N]


## Calc Full R2 (train and test)
# Training R2
pred.r2 <- rf.herbicides$r.squared

# Testing R2
mean_observed <- mean(test_x$log.conc)                # Mean of observed values
ss_residual <- sum((test_x$log.conc - test_x$pred)^2) # SS_residual: Residual sum of squares
ss_total <- sum((test_x$log.conc - mean_observed)^2)  # SS_total: Total sum of squares
# R-squared calculation
test.r2 <- 1 - (ss_residual / ss_total) 


## :: EXPORT model with key metrics for later use
assign(paste0("export_", model.name, "_conc"), list("model.name" = model.name,    # model name
                                                   "null.mse" = null.mse,        # error of null model (MSE)
                                                   "full.mse" = full.mse,        # error of full model (MSE)
                                                   "dec.mse" = dec.mse,          # accuracy improvement (relative; RI)
                                                   "null.mae" =  null.mae,       # null mean absolute error (MAE)
                                                   "pred.r2" = pred.r2,          # R2 of training data
                                                   "test.r2" = test.r2,          # R2 of testing data
                                                   "data.n" = opti.data[, .N],   # total number of samples available for training and testing
                                                   "data.summary" = opti.data[, .N, .(cas)],   # simple data summary
                                                   "full.model" = get(paste0("rf.",model.name))))
# Final check
get(paste0("export_", model.name, "_conc"))

## :: FINAL EXPORT OF DATA
saveRDS(get(paste0("export_", model.name, "_conc")),
        file = paste0("data/PREDICTIONS/FINAL models/","export_", model.name, "_conc.rds"))



## 3.5 Metals conc ----


## :: 0.1 Model name
model.name <- "metals"

## :: DEFINE CHEMICALS
cur.cas <- aqua.data[!is.na(cas) & chemical_group == "metal"]$cas

# decide about data reduction
test.data[cas %in% cur.cas, .N] < 3*10^5 # IF TRUE, no reduction!
test.data[cas %in% cur.cas, .N]

# reduce to relevant data
opti.data <- test.data[cas %in% cur.cas,]


# Reduce to a max of 500.000 entries using stratified sampling approach
if (opti.data[, .N] > 300000) {
  
  # calc relative size by which the data needs to be split
  rel.size <- 300000 / opti.data[, .N]
  
  # derive 
  index <- createDataPartition(opti.data$cas, 
                               p = rel.size, 
                               list = FALSE)
  
  opti.data <- opti.data[index]
} else {}

# Sanity check
san.check <- merge(opti.data[, .N, cas],
                   test.data[cas %in% cur.cas, .N, cas],
                   by = "cas",
                   all.x = T)
san.check[, rel := N.x / N.y]
san.check   # perfect
rm(san.check)


## :: Packs

## : SEED
seed <- 1234
set.seed(1234)

## : CORES
ncores <- 8

### :: 2.0 Generate Training ad Testing Data
data_split <- splitTools::partition(
  y = opti.data[, ]$cas,
  p = c(train = 0.7, test = 0.3),
  type = "stratified",
  seed = seed
)

## Training data
train_x <- opti.data[data_split$train]
## Testing data
test_x <- opti.data[data_split$test]


### ::: 1.0 - PREDICTOR REMOVAL --- PREDICTION OPTIMIZATION

## :: Define  mtry and tree
ntree <- 150

## Create base model - leave mtry by default to allow for predictor complexity-dependent mtry setting (square root)
start <- Sys.time()
base.model <- ranger(log.conc ~ . ,
                     data = train_x,
                     num.tree = ntree,
                     importance = "permutation")
stop <- Sys.time()  # ca. 1 minute
stop-start


# view
base.model  #  baseline performace

## :: IMPORTANCE metrics
base.model.imp <- data.table(pred = names(base.model$variable.importance),
                             imp = unname(base.model$variable.importance))
setorder(base.model.imp, imp)
base.model.imp$pred <- factor(base.model.imp$pred , levels = base.model.imp$pred)
# Importance plot
ggplot(base.model.imp, aes(x = imp, y = pred)) + geom_point() + coord_cartesian(xlim = c(0, NA))


# Create list of number of predictor reductions
num.red <- seq(from = 2 , 
               to = nrow(base.model.imp)-1,
               by = 2)

# worst predictors - example for testing
worst.pred <- base.model.imp[1:num.red[1]]$pred               # worst
train_x[, setdiff(names(train_x), c(worst.pred)), with = F]   # visual check


# Optimization using k-fold (3) predictor removal
optim.list <- lapply(num.red, function(xx){
  
  # create worst predictor vector
  worst.pred <- base.model.imp[1:xx]$pred  # worst
  # create fold indices
  num.folds <- 3
  fold.indices <- createFolds(train_x$log.conc, k = num.folds)
  
  list.fold <- lapply(1:num.folds, function(yy){
    
    # build model
    rf.red <- ranger(log.conc ~ . ,
                     data = train_x[fold.indices[[yy]], setdiff(names(train_x), c(worst.pred)), with = F],
                     num.tree = ntree)
    
    # create secondary accuracy estiamte based on test data to further support decision process (
    # not really needed due to internal OOB estimates but whatever.. for the reviewers i guess)
    test_x[, pred := predict(rf.red, test_x)$predictions]
    ## calc MSE
    mse <- mean((test_x$log.conc - test_x$pred)^2)
    
    ## calc R2
    # Mean of observed values
    mean_observed <- mean(test_x$log.conc)
    # SS_residual: Residual sum of squares
    ss_residual <- sum((test_x$log.conc - test_x$pred)^2)
    # SS_total: Total sum of squares
    ss_total <- sum((test_x$log.conc - mean_observed)^2)
    # R-squared calculation
    r2 <- 1 - (ss_residual / ss_total) 
    
    # create results
    results <- data.table(run = xx,
                          fold = yy,
                          pred.mse = rf.red$prediction.error,
                          pred.r2 = rf.red$r.squared,
                          test.mse = mse,
                          test.r2 = r2)
    
  })
  export <- rbindlist(list.fold)  # bind fold results
  
  # print progress
  print(paste(((round(xx / length(num.red),2))/2)*100, "% completed"))
  # export
  export
})
optim.dt <- rbindlist(optim.list)

# see optim
optim.dt   
optim.dt[, .(mean(pred.mse), 
             mean(test.mse),
             mean(pred.r2), 
             mean(test.r2)), run][which.min(V1)]  # currently - test error even smaller than training = NICE
# Model choice based on lowest MSE
optim.dt[, mean(pred.mse), run][order(V1)]
# Quick check if this also applies to the highest R2 - DO NOT USE TEST SET ACCURACY FOR EVALUATION, risk of data leakage!
optim.dt[, mean(pred.r2), run][order(-V1)]   # same, great!
# Take best run
best.run <- optim.dt[, mean(pred.mse), run][which.min(V1)]$run

# best predictors
base.model.imp[(best.run+1) : .N]  # highly complex currently best for metals
best.pred <- base.model.imp[(best.run+1) : .N]$pred

# Define best predictors!
best.pred <- c("log.conc",
               as.vector(best.pred))

### ::: ------ NEW SELF_WRITTEN OPTIMIZER


# required for grid search and initialization of bayesian optimization
parameter_grid <- expand.grid(
  num.trees = c(150),   # previous analyses showed that num.tree already maxes out at 100-130. Iterating over it is nonsensical as higher ntree always improve predictions slightly without any overfits. Final model trained on ntree = 500 as overfits were never observed
  mtry = seq(from = 2,
             to = ifelse((length(best.pred)-1) < 12,
                         length(best.pred)-1,
                         12),               # allow a maximum mtry of 10 with 26 predictors
             by = 1)                        # After preliminary assessments there are only 2 valuable hyperparameters: ntree and mtry. ntree already amxes out at 100-130 without overfits in 500-1000, data not shown
  # min.node.size = c(1, 2, 4)              # preliminary assessment showed no real improvement with min node size, rather defaults are the best choices. No use in parameter grid searching it.
  # max.depth = seq(1, 3, 5, 9, NA)         # preliminary assessment showed that maximum tree depths always outcompetes without risk of overfit. No use in parameter grid searching this
  # sample.fraction = seq(0.5, 0.8, 0.3)    # preliminary assessment showed no improvement compared to default classifier values set by ranger package. No use in iterating over it
)
# SUMMARY: 
# ntree = 150 showed across contaminant groups to already reach very high predicitive power, or, alternatively, marginal gains if further increased. 
# Iteration over ntree only increases the total number of permutations, hence for fast grid searching only mtry requires optimization
# all other common hyperparamters did not outperform model defaults or 


gc()

## K-fold cross validation (3) for parameter grid tuning

tuning.list <- lapply(1:nrow(parameter_grid), function(ii){
  
  # dump current parameters
  num.trees <- parameter_grid[ii,]$num.trees
  mtry <- parameter_grid[ii,]$mtry
  sample.fraction <- parameter_grid[ii,]$sample.fraction
  
  ## 3 iterations to derive mean error / accuracy per parameter combination (similar to k-folding)
  k.list <- lapply(1:3, function(xx){
    
    cur.mod <- ranger(log.conc ~ .,
                      data = train_x[ , .SD, .SDcols = best.pred],
                      num.trees = num.trees, 
                      mtry = mtry,
                      verbose = F)
    
    # create secondary accuracy estiamte based on test data to further support decision process (not really needed due to internal OOB estimates but whatever.. for the reviewers i guess)
    test_x[, pred := predict(cur.mod, test_x)$predictions]
    
    ## calc MSE
    mse <- mean((test_x$log.conc - test_x$pred)^2)
    
    ## calc R2
    # Mean of observed values
    mean_observed <- mean(test_x$log.conc)
    # SS_residual: Residual sum of squares
    ss_residual <- sum((test_x$log.conc - test_x$pred)^2)
    # SS_total: Total sum of squares
    ss_total <- sum((test_x$log.conc - mean_observed)^2)
    # R-squared calculation
    r2 <- 1 - (ss_residual / ss_total) 
    
    results <- data.table(id = ii,
                          run = xx,
                          num.trees = num.trees,
                          mtry = mtry,
                          pred.mse = cur.mod$prediction.error,
                          pred.r2 = cur.mod$r.squared,
                          test.mse = mse,
                          test.r2 = r2)
    
  })
  # print progress
  print(paste0("Progress: ", round(ii / nrow(parameter_grid) *100,0), "% completed"))
  # print current grid
  print(parameter_grid[ii,])
  rbindlist(k.list)
  
})
tuning.list

tuning.dt <- rbindlist(tuning.list)   #  

# Assess accuracies - check comparability of pred and test error.
tuning.dt[, .(mean(pred.mse), 
              mean(test.mse),
              mean(pred.r2), 
              mean(test.r2)), .(id, num.trees, mtry)][order(V1)]   # again both internal and external (test_set) accuracies are near identical
# Model choice based on lowest MSE
tuning.dt[, mean(pred.mse), id][order(V1)]
# Quick check if this also applies to the highest R2 - DO NOT USE TEST SET ACCURACY FOR EVALUATION, risk of data leakage!
tuning.dt[, mean(pred.r2), id][order(-V1)]   # same, great!

# keep best mtry
best.mtry <- tuning.dt[, mean(pred.mse), .(id, num.trees, mtry)][order(V1)][1]$mtry
gc()

## Visualize results
fig.fun.tuning <-
  ggplot(tuning.dt)+
  geom_point(aes(x = mtry,
                 y = pred.mse))+
  coord_cartesian(ylim = c(NA,NA))+
  scale_y_continuous(expand = expansion(mult = c(.1,.1)))+
  ylab("Prediction error [%]")+
  ggtitle(label = model.name)
fig.fun.tuning

# ggsave(fig.fun.tuning,
#        file = paste0("data/PREDICTIONS/FINAL models/fig.fun.tuning.", model.name, "_conc.png"),
#        width = 4, height = 4, dpi = 300)

## NTREE testing outside grid search
# REASON: no overfitting with up to 1000 trees
# Inclusion in grid search would massively increase training time to days/weeks

list.ntree <- lapply(c(50,100,150,200,250,300,400), function(xx){
  
  # build model
  rf.red <- ranger(log.conc ~ . ,
                   data = train_x[ , .SD, .SDcols = best.pred],
                   num.tree = xx,
                   mtry = best.mtry)
  
  print(xx)
  # create results
  results <- data.table(ntree = xx,
                        pred.error = rf.red$prediction.error)
  
})
ntree.dt <- rbindlist(list.ntree)

## 0.5% rule, lowerst number of trees with least training 
# create 1% threshold
ntree.dt[, thresh := ntree.dt[which.min(pred.error)]$pred.error * 1.005]
best.ntree <- ntree.dt[pred.error < thresh][which.min(ntree)]$ntree

fig.tree <- 
  ggplot(ntree.dt)+
  geom_point(aes(x = ntree, y = pred.error))+
  geom_hline(yintercept = ntree.dt[1]$thresh, color ="green")+
  ggtitle(model.name)
fig.tree

# ggsave(fig.tree,
#        file = paste0("data/PREDICTIONS/FINAL models/fig.trees.", model.name, "_conc.png"),
#        width = 4, height = 4, dpi = 300)



### ::: 3.0 TRAIN FINAL MODEL AND EXPORT

## :: DEFINE CHEMICALS
# CONFIRM CURRENT CAS PRIOR TO TRAINING
cur.cas 

# reduce to relevant data
opti.data <- test.data[cas %in% cur.cas,]

## : SEED
seed <- 1234
set.seed(1234)

## : CORES
ncores <- 8

### :: 2.0 Generate Training ad Testing Data
data_split <- splitTools::partition(
  y = opti.data[, ]$log.conc,
  p = c(train = 0.7, test = 0.3),
  type = "stratified",
  seed = seed
)

## Training data
train_x <- opti.data[data_split$train]

## Testing data
test_x <- opti.data[data_split$test]

## Cleanup
rm(export_fungicides_det,
   export_herbicides_det,
   export_insecticides_det,
   rf.fungicides,
   rf.insecticides,
   rf.herbicides,
   test.data)
gc()

## : TRAIN MODEL
rf.metals <-  ranger(log.conc ~ .,
                         data = train_x[ , .SD, .SDcols = best.pred],
                         num.trees = best.ntree, 
                         mtry = best.mtry,
                         importance = "permutation")


test_x[ , pred := predict(rf.metals, test_x)$predictions]
# Create residuals
test_x[, res := log.conc - pred]

## Accuracy plot
fig.model.acc <- 
  ggplot(test_x)+
  geom_density(aes(x = res), fill = "grey50", color = NA)+
  geom_vline(xintercept = c(-1, -0.5, 0, 0.5, 1),
             color = c("red", "orange", "green", "orange", "red"),
             linewidth = .1)+
  scale_y_continuous(expand = expansion(mult = c(0,.01)),
                     name = "Density")+
  xlab("Residuals [log10]")+
  ggtitle(capitalize(model.name))
fig.model.acc

# # Export
# ggsave(fig.model.acc,
#        file = paste0("data/PREDICTIONS/FINAL models/fig.", model.name, ".acc.png"),
#        width = 4,
#        height = 4,
#        dpi = 300)

fig.conc.tuning <- grid.arrange(fig.fun.tuning,
                                fig.tree,
                                fig.model.acc,
                                ncol = 3,
                                nrow = 1)

ggsave(fig.conc.tuning,
       file = paste0("data/PREDICTIONS/FINAL models/fig.", model.name, ".CONC.TUNING_det.png"),
       width = 14,
       height = 4.5,
       units = "cm")


# Calculate Accuracy estimates in OOM
oom05 <- round((test_x[res >= -0.5 & res <= 0.5, .N] / test_x[, .N])*100, 2)   
oom1 <- round((test_x[res >= -1 & res <= 1, .N] / test_x[, .N])*100, 2)        

### NULL vs FULL model
## calc MSE
full.mse <- mean((test_x$res)^2)
null.mse <- mean((test_x$log.conc - mean(test_x$log.conc))^2)    # null MSE == MSE of acutal vs. average conc
# relative Improvement over Null Model (RI)
dec.mse <- round(((null.mse - full.mse) / null.mse)*100, 2)      # RI = decreased MSE for full vs. null model

## Mean Absolute Error
null.mae <- sum(abs(test_x$log.conc - mean(test_x$log.conc)))/ test_x[, .N]
full.mae <- sum(abs(test_x$res))/ test_x[, .N]


## Calc Full R2 (train and test)
# Training R2
pred.r2 <- rf.metals$r.squared

# Testing R2
mean_observed <- mean(test_x$log.conc)                # Mean of observed values
ss_residual <- sum((test_x$log.conc - test_x$pred)^2) # SS_residual: Residual sum of squares
ss_total <- sum((test_x$log.conc - mean_observed)^2)  # SS_total: Total sum of squares
# R-squared calculation
test.r2 <- 1 - (ss_residual / ss_total) 


## :: EXPORT model with key metrics for later use
assign(paste0("export_", model.name, "_conc"), list("model.name" = model.name,    # model name
                                                   "null.mse" = null.mse,        # error of null model (MSE)
                                                   "full.mse" = full.mse,        # error of full model (MSE)
                                                   "dec.mse" = dec.mse,          # accuracy improvement (relative; RI)
                                                   "null.mae" =  null.mae,       # null mean absolute error (MAE)
                                                   "pred.r2" = pred.r2,          # R2 of training data
                                                   "test.r2" = test.r2,          # R2 of testing data
                                                   "data.n" = opti.data[, .N],   # total number of samples available for training and testing
                                                   "data.summary" = opti.data[, .N, .(cas)],   # simple data summary
                                                   "full.model" = get(paste0("rf.",model.name))))
# Final check
get(paste0("export_", model.name, "_conc"))

## :: FINAL EXPORT OF DATA
saveRDS(get(paste0("export_", model.name, "_conc")),
        file = paste0("data/PREDICTIONS/FINAL models/","export_", model.name, "_conc.rds"))



# 4.0 Global predictions ----


## 4.1 Setup ----

## :: 0.0 :: TODOSs
# clear

## :: 1.0 CURRENT RULES FOR GLOBAL PREDICTION

## :: 1.1 TIME RANGE

### ::: DEFINE 
# 5 year coverage (2015 - 2019)
year.min <- 2015
year.max <- 2019

## :: 1.2 INTRA-YEAR RANGE

### ::: DEFINE 
# all 12 months
# always in the ides of each month
month.range <- 1:12

## :: 1.3 CHEMICALS  - CURRENTLY FIHM groups
model.names <- c("fungicide",
                 "insecticide",
                 "herbicide",
                 "metal")

aqua.data[chemical_group %in% model.names & !is.na(cas), .N]   # 39 
aqua.data[chemical_group %in% model.names & !is.na(cas)]$cas

eu.data[!duplicated(cas)][cas %in% aqua.data[chemical_group %in% model.names & !is.na(cas)]$cas, .N]  # 33/41 in EU
us.data[!duplicated(cas)][cas %in% aqua.data[chemical_group %in% model.names & !is.na(cas)]$cas, .N]  # 31/41 in US

# Get all CAS for which the model was trained
list.cas <- lapply(model.names, function(xx){
  
  dump <- rbind(eu.data[!duplicated(cas), .(cas)][cas %in% aqua.data[chemical_group %in% xx & !is.na(cas)]$cas],
                us.data[!duplicated(cas), .(cas)][cas %in% aqua.data[chemical_group %in% xx & !is.na(cas)]$cas])
  
  dump <- dump[!duplicated(cas)]
})
names(list.cas) <- model.names

## :: 1.4 Create TIME grid
time.grid <- as.data.table(expand.grid(year.min : year.max,
                                       month.range))
colnames(time.grid) <- c("year", "month")
setorder(time.grid, year)

# Cleanup
rm(eu.data, us.data)
gc()


## 4.2 Model selection + chemo grid ----

### ::: CHOOSE MODEL HERE
## :: Current model

## :: Current runtime ca. 3-4 days @12 physical cores / 20 threads (4 kept free for background tasks) - min 64GB RAM required.

# 1 fun 
# 2 herb
# 3 ins
# 4 metal
cur.model.name <- model.names[[2]]
cur.cas <- list.cas[[cur.model.name]]$cas


# make date-year string
time.grid[, m.y := paste(month, year)]

## :: 1.5 Expand TIME grid to CHEMO-TIME grid
chemo.grid <- as.data.table(expand.grid(time.grid$m.y, 
                                        cur.cas))
colnames(chemo.grid) <- c("m.y", "cas")
chemo.grid <- merge(chemo.grid,
                    time.grid,
                    by = "m.y",
                    all.x = T)
chemo.grid[, m.y := NULL]  # removal

# reorder cols
setcolorder(chemo.grid, c("cas", "month", "year"))
# set order 
setorderv(chemo.grid, c("cas", "year", "month"))


### ::: !!! REQUIRES GLOBAL MODELS
cur.mod_det <- readRDS(paste0("data/PREDICTIONS/FINAL models/Export_", cur.model.name, "s_det.rds"))       # OCCURrENCE
cur.mod_det <- cur.mod_det$full.model
cur.mod_conc <- readRDS(paste0("data/PREDICTIONS/FINAL models/Export_", cur.model.name, "s_conc.rds"))     # CONCENTRATIONS
cur.mod_conc <- cur.mod_conc$full.model

# Reduce chemo.grid to current model run
chemo.grid <- chemo.grid[cas %in% cur.cas]

## :: AUTOMATION
chemo.grid[, .N]            
gc()

# Create contintenal IDs for iterating
pnt.glob[, glob.id := substr(HYBAS_L12, 1,1)]
pnt.glob[, .N, glob.id]


## 4.3 Global prediction ----

glob.pred.list <- lapply(unique(chemo.grid$cas), function(xx){
  
  # iter.grid
  iter.grid <- chemo.grid[cas == xx]
  
  month.list <- lapply(1:iter.grid[, .N], function(yy){
    
    # prediction iterator
    pred.iter <- iter.grid[yy]
    
    # current example tree
    sub.data <- copy(pnt.glob)
    sub.data[, cas := pred.iter$cas]
    sub.data[, year := pred.iter$year]
    sub.data[, month := pred.iter$month]
    
    ## Prediction time - OCCURENCE
    # Predictions need to be split at least to 1/10th, otherweise whole populated model > 64GB
    start <- Sys.time()
    for (zz in unique(sub.data$glob.id)) {
      sub.data[glob.id == zz, pred :=  predict(cur.mod_det,
                                               sub.data[glob.id == zz])$predictions]
    }
    stop <- Sys.time()
    print(stop-start)          # currently ca. 2.5
    
    ## Prediction time - CONCENTRATIONS
    # CONC predictions also need to be split to conserve RAM, slowdown ca. 25%
    start <- Sys.time()
    for (zz in unique(sub.data$glob.id)) {
      sub.data[glob.id == zz & pred == "X",
               log.conc :=  predict(cur.mod_conc,
                                    sub.data[glob.id == zz & pred == "X"])$predictions]
    }
    stop <- Sys.time()
    print(stop-start)          # currently ca. 2.5
    
    ## :: reduce size
    sub.data <- sub.data[, .(HYBAS_L12, HYRIV_ID, cas, year, month, pred, log.conc)]
    
    ## aggregate on basin level - DETECTION frequency
    dump.det <- dcast(sub.data[, .N, .(HYBAS_L12, pred)],
                      HYBAS_L12 ~ pred, value.var = "N")
    dump.det[is.na(dump.det)] <- 0
    dump.det[, det.freq := X / (X + N)]
    
    ## aggregate on basin level - CONCETRATION // MEAN and MAX
    dump.conc <- sub.data[, .(mean.conc = mean(log.conc, na.rm = T),
                              max.conc = max(log.conc, na.rm = T)), HYBAS_L12]
    
    ## Prepare for join and export
    agg <- sub.data[!duplicated(HYBAS_L12)][, -c("pred", "log.conc", "HYRIV_ID")]
    
    ## Join summaries
    
    # DETECTIONS
    agg <- merge(agg, 
                 dump.det[, -c("N", "X")],
                 by = "HYBAS_L12",
                 all.x = T)
    
    # CONCENTRATIONS
    agg <- merge(agg, 
                 dump.conc,
                 by = "HYBAS_L12",
                 all.x = T)
    
    # cleanup
    rm(dump.det, dump.conc, sub.data)
    gc()
    # Progress
    print(paste0(which(chemo.grid[!duplicated(cas)]$cas == xx), "/", length(unique(chemo.grid$cas))))
    print(paste(yy, "/" , iter.grid[, .N]))
    ## :: EXPORT
    agg
    
  })
  
  # export
  export <- rbindlist(month.list)
  
  # Export raw data
  saveRDS(export,
          paste0("data/PREDICTIONS/FINAL models/outputs/glob.pred.dt_", cur.model.name, "_", xx, ".rds" ))
  
  glob.pred.agg <- export[, .(mean.conc.year = mean(mean.conc, na.rm = T),
                              max.conc.year = max(max.conc, na.rm = T),
                              mean.det.year = mean(det.freq, na.rm = T)), 
                          .(HYBAS_L12, cas)]
  
  # Export aggregated data
  saveRDS(glob.pred.agg,
          paste0("data/PREDICTIONS/FINAL models/outputs/glob.pred.agg_", cur.model.name, "_", xx, ".rds" ))
  
})



### ::: Combine aggregated results
files <- grep("glob.pred.agg", list.files("data/PREDICTIONS/FINAL models/outputs/"), value = T)

agg.list <- lapply(files, function(xx){
  
  dump <- readRDS(paste0("data/PREDICTIONS/FINAL models/outputs/", xx))
  
  
})
agg.dt <- rbindlist(agg.list)

## :: EXPORT
saveRDS(agg.dt, "data/PREDICTIONS/FINAL models/outputs/glob.pred.agg_COMBINED.rds")

36560674*5*12*8  # 17 billion detections with subequent max 17 billion concs - yikes.



# 5.0 Aggregation for hand-over ----

### ::: RULES:
## AGGREGATION of the 4 contaminant groups (M, F, H, I) in sum concentrations
## Aggregation should account for the probability of occurence to derive the mean concnetration, then sum

## :: Get files
files <- grep("glob.pred.agg", list.files("data/PREDICTIONS/FINAL models/outputs/"), value = T)
# remove glob.pred.agg_COMBINED.rds
files <- files[files != "glob.pred.agg_COMBINED.rds"]
# remove calcium!
files <- files[files != "glob.pred.agg_metal_7440-70-2.rds"]


# substance classes for aggregation
sub.classes <- c("metal", "herbicide", "fungicide", "insecticide")
files[grep(sub.classes[1], files)]  # example indexing

# get ATLAS attributes for post filtering of 
# (1) intermittent streams (Q < 0.1 m3/s) primarily in desert and arid environments
# (2) low agricultural areas (agricultural land use < 5%) // ONLY APPLIES TO PESTICIDES

atlas <-  fread("data/BasinATLAS_ATTRIBUTES.csv")
dis.ag <- atlas[, .(OBJECTID, HYBAS_ID, UP_AREA, dis_m3_pyr, crp_pc_sse, crp_pc_use)]


## Automation
list.sumconc <- lapply(sub.classes, function(xx){
  
  my.files <-  files[grep(xx, files)]
  print(my.files)
  
  # get files 
  list.files <- lapply(my.files, function(yy){
    
    dump <- readRDS(paste0("data/PREDICTIONS/FINAL models/outputs/", yy))
  })
  # bind files
  my.dt <- rbindlist(list.files)
  
  ## Start processing
  # NaN to NA
  my.dt[is.nan(mean.conc.year), mean.conc.year := NA]
  # convert back from log10
  my.dt[, conc.norm := 10^mean.conc.year]
  # account for detection freq
  my.dt[, conc.adj := conc.norm * mean.det.year]
  # NAs now to 0
  my.dt[is.na(conc.adj), conc.adj := 0]
  
  ## Aggregation (sum of contaminants)
  agg.dt <- my.dt[, .(sum.conc = sum(conc.adj, na.rm = T)), .(HYBAS_L12)]
  # also log10 transform
  agg.dt[, sum.log10 := log10(sum.conc)]
  # remove infinites
  agg.dt[is.infinite(sum.log10), sum.log10 := NA]
  # add cont type
  agg.dt[, cont.type := xx]
  
  ## :: Filtering logic
  
  ## Agricultural land use
  # 5% rule! (either in the subbasin or the entire contributing upstream catchment)
  have.agri <- dis.ag[crp_pc_sse >= 5 | crp_pc_use >= 5]$HYBAS_ID
  
  if (xx %in% c("herbicide", "fungicide", "insecticide")) {
    
    agg.dt[!HYBAS_L12 %in% have.agri, sum.conc := 0]
    agg.dt[!HYBAS_L12 %in% have.agri, sum.log10 := NA]
  }
  
  ## Intermittent or arid regions
  have.flow <- dis.ag[dis_m3_pyr >= 0.1]$HYBAS_ID
  # remove concentrations from low flow environments
  agg.dt[!HYBAS_L12 %in% have.flow, sum.conc := 0]
  agg.dt[!HYBAS_L12 %in% have.flow, sum.log10 := NA]
  
  # export
  agg.dt
  
  
})
dt.sumconc <- rbindlist(list.sumconc)




# 6.0 Model accuracy metrics ----

## :: get files
files <- grep("*.rds",list.files("data/PREDICTIONS/FINAL models/"), value = T)

acc.list <- lapply(files, function(xx){
  
  dump <- readRDS(paste0("data/PREDICTIONS/FINAL models/", xx))
  
  if (grepl("conc", dump$full.model$dependent.variable.name)) {
    
    dt <- data.table(model.name = dump$model.name,
                     null.mse = dump$null.mse,
                     full.mse = dump$full.mse,
                     dec.mse = dump$dec.mse,
                     null.mae = dump$null.mae,
                     pred.r2 = dump$pred.r2,
                     test.r2 = dump$test.r2,
                     data.n = dump$data.n,
                     type = "conc")
  } else {
    
    dt <- data.table(model.name = dump$model.name,
                     null.accuracy = dump$null.accuracy,
                     full.accuracy = dump$full.accuracy,
                     test.accuracy = dump$test.accuracy,
                     precision = dump$precision,
                     recall = dump$recall,
                     f1.score = dump$f1.score,
                     dec.error = dump$dec.error,
                     data.n = dump$data.n,
                     type = "det")
    
  }
  print(xx)
  dt
  
  
})

## Create 2 tables, 1 more conc and 1 for det
acc.conc.dt <- rbind(acc.list[[1]], 
                     acc.list[[3]],
                     acc.list[[5]],
                     acc.list[[7]])

acc.det.dt <- rbind(acc.list[[2]],
                    acc.list[[4]],
                    acc.list[[6]],
                    acc.list[[8]])

# ## :: EXPORT  - last export SEP2025
# write.xlsx(acc.conc.dt,
#            file = "data/PREDICTIONS/FINAL models/Table_Accuracy_models_CONC.xlsx")
# write.xlsx(acc.det.dt,
#            file = "data/PREDICTIONS/FINAL models/Table_Accuracy_models_DET.xlsx")

dump <- readRDS(paste0("data/PREDICTIONS/FINAL models/", files[[1]]))


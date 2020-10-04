# import libraries ####
# data wrangling

library(tidyverse)
library(reshape)
library(xlsx)
library(gridExtra)
library(grid)
library(chron)
library(devtools)
library(rscopus)
library(rlist)
library(rgeos)
library(future)
library(parallel)
library(doParallel)
library(feather)
library(Kendall) # Mann-Kendall trend test

# data visualization 

library(GGally)
library(RColorBrewer)
library(proj4)
library(leaflet)
library(leaflet.minicharts)
library(RColorBrewer)
library(mapview)
library(htmlwidgets)
library(corrplot)
library(mice)
library(VIM)
library(ggmosaic)
library(esquisse)
library(bibliometrix)
library(ggwordcloud)
library(colorspace)
library(rworldmap)
library(countrycode)
library(usethis)
library(pastecs)
library(officer) # make editable map
library(rvg) # make editable map
library(ggvis)
library(scales) # Make color gradient scales
library(dendextend) # make dendrogram plots
library(ggdendro) # make dendrogram plots
# Import datasets  ####

list_river_mlvised <- list.files(pattern = "*.csv")
river_ml_data <- lapply(list_river_mlvised, read_csv)
names(river_ml_data) <- str_split_fixed(list_river_mlvised,pattern = "\\.", n =2)[,1]
col_chosen <- colnames(river_ml_data$Manifold)[c(1, 3:5, 12, 13, 15, 17:19, 42:44, 46, 48)]
river_ml_data <- map(river_ml_data, subset, select = col_chosen)
river_ml_data <- rbindlist(river_ml_data, idcol = TRUE)

# identify duplicate rows

river_ml_dup <- river_ml_data[,-1]
sum(duplicated(river_ml_dup))
river_ml_data <- river_ml_data[!duplicated(river_ml_dup),]
rm(river_ml_dup)

river_ml_data$.id <- as.factor(river_ml_data$.id)
river_ml_data$`Document Type` <- as.factor(river_ml_data$`Document Type`)
river_ml_data$`Language of Original Document` <- as.factor(river_ml_data$`Language of Original Document`)
river_ml_data$`Abbreviated Source Title` <- as.factor(river_ml_data$`Abbreviated Source Title`)

# split affiliation ####

river_ml_data$Affi <- lapply(river_ml_data$Affiliations, function(x){str_split_fixed(x, "; ", n = 1+ str_count(x, "; "))})

river_ml_data$Country <- lapply(river_ml_data$Affi, function(y){
    y <- as_data_frame(as.matrix(str_split_fixed(y , ", ", n = 1 + str_count(y, ", "))))
    y <- split(y, seq(nrow(y)))
    y <- lapply(y, function(z){
        z <- z[,colSums(z != "") != 0]
        
        z <- z[,ncol(z)]
    })
    return(y)
})

river_ml_data$Country <- lapply(river_ml_data$Country, function(x){
    x <- lapply(x, function(y){
        y <- unlist(y)
        y <- unname(y)
    })
})

river_ml_data$Country <- lapply(river_ml_data$Country, unlist)

lat_long <- as.data.frame(gCentroid(getMap(resolution="high"), byid=TRUE))
lat_long$Country <- rownames(lat_long)
rownames(lat_long) <- c(1:nrow(lat_long))
lat_long <- bind_rows(lat_long, data.frame(x = 31.9522, y = 35.2332, Country = 'Palestine'))
lat_long <- bind_rows(lat_long, data.frame(x = 16.2650, y = 61.5510, Country = 'Guadeloupe'))
lat_long <- bind_rows(lat_long, data.frame(x = -112.461671, y = 45.679552, Country = 'United States'))
lat_long <- bind_rows(lat_long, data.frame(x = -112.461671, y = 45.679552, Country = 'USA'))
country_diff <- setdiff(levels(as.factor(unlist(river_ml_data$Country))), lat_long$Country)
#** country correction (NOT USE) ####
# country_correct <- c("Croatia", "United States of America", "Croatia", "Croatia", "Croatia",
#                      "United States of America", "Australia", "Russia", "Japan", "Japan",
#                      "United States of America", "Belgium", "China", "Canada", "United States of America", 
#                      "Australia", "Canada", "China", "China", "China",
#                      "China", "China", "Belgium", "United States of America", "Brazil",
#                      "Brazil", "Brunei", "United States of America", "Canada", "Canada",
#                      "Canada", "Canada", "United States of America", "Canada", "France",
#                      "Belgium", "Argentina", "Argentina", "United States of America", "China",
#                      "China", "China", "Democratic Republic of the Congo", "Argentina", "Ivory Coast",
#                      "Australia", "Japan", "Japan", "Romania", "Romania",
#                      "Germany", "Germany", "Germany", "Germany", "Democratic Republic of the Congo",
#                      "United Kingdom", "United Kingdom", "United Kingdom", "United Kingdom", "Ireland",
#                      "India", "Italy", "Italy", "United Kingdom", "United Kingdom", 
#                      "United Kingdom", "Russia", "France", "France", "United States of America", 
#                      "United States of America", "France", "France", "Russia", "Romania",
#                      "Romania", "Romania", "Hungary", "Iran", "Iran", 
#                      "Hong Kong S.A.R.", "Hong Kong S.A.R.", "United States of America", "France", "United States of America",
#                      "India", "India", "Brazil", "France", "Ireland", 
#                      "India", "Japan", "United States of America", "United States of America", "United States of America", 
#                      "Libya", "China", "China", "China", "China",
#                      "Macedonia", "China", "United States of America", "Brazil", "United Kingdom",
#                      "Czech Republic", "United States of America","United Kingdom", "Australia", "United States of America", 
#                      "Canada", "Canada", "Russia", "United Kingdom", "United Kingdom", 
#                      "United Kingdom", "United States of America", "Republic of Serbia", "South Africa", "Canada",
#                      "Syria", "Taiwan", "United Republic of Tanzania", "Iraq", "China",
#                      "Australia", "Nepal", "United States of America", "Brazil", "United Kingdom",
#                      "Spain", "United Kingdom", "Finland",  "United States of America",  
#                      "Netherlands", "United States of America", "Malaysia",  "United States of America",  "United States of America", 
#                      "United States of America",  "United States of America", "Russia", "Vietnam",  "United States of America", 
#                      "United States of America",  "United States of America",  "United States of America", "Germany", "Croatia"
# )
# 
# country_list <- tibble(`Country_wrong` = country_diff, `Country` =country_correct)

#** lat_long (NOT USE) ####
# lat_long2 <- left_join(country_list, lat_long, by = "Country")
# lat_long2 <- lat_long2 %>% select(-Country)
# colnames(lat_long2)[1] <- "Country"
# lat_long_final <- bind_rows(lat_long, lat_long2)
# write_feather(lat_long_final,"lat_long_final.feather")

# old <- c("11000 Belgrade", "2169 McCarty Hall", "41001 Zagreb", "52210 Rovinj", "58000 Split", "908", "Adelaide", "Adygei State University", "Aichi Institute of Technology", "Akita University School of Medicine.", "Alberta.", "and Molecular Biology", "Anhui Normal University", "as well as a faculty member of the University of Toronto's MIS fellowship training program. He can be reached by email at", "ASCE", "Auckland University of Technology", "B2Y 4A2", "Beijing China \\(e-mail: g20178747@xs.ustb.edu.cn\\)", "Beijing China \\(e-mail: long.wang@ieee.org\\)", "Beijing China \\(e-mail: s20170672@xs.ustb.edu.cn\\)", "Beijing China 100083 \\(e-mail: chao.huang@my.cityu.edu.hk\\)", "Beijing China 100083 \\(e-mail: xluo@ustb.edu.cn\\)", "Belg", "Black and Veatch Corporation", "Braz", "Brazil.", "Brunei Darussalam", "Burns & McDonnell Engineering Co", "CA 92134", "Can", "Canada A1B 3X5.", "Canada.", "Carnegie Mellon University", "CDM", "Cedex 9", "Center for Construction Methods and Materials", "Centro de Investigaciones del Mar y la Atm<f3>sfera<U+0096>CONICET/UBA", "Centro de Investigaciones del Mar y la Atm<f3>sfera<U+0096>CONICET/UBA", "ChevronTexaco", "Cold and Arid Region Environmental and Engineering Research Institute", "Collaborative Innovation Center on Forecast and Evaluation of Meteorological Disasters", "College of Information", "Congo", "CONICET", "Cote d'Ivoire", "CSIRO Land and Water", "CTI Eng. Co.", "D-7000", "D<e2>mbovita", "D<e2>mbovita", "DDR-1040", "DDR-1190", "DDR 1080", "DDR\\)", "Democratic Republic Congo", "Department of Computer Science", "Department of Electrical Engineering", "Department of Environmental Quality", "Department of Ocean Engineering", "Dublin Ireland", "Durgapur Institute of Advanced Technology and Management", "Ecologia e Conserva<e7><e3>o", "Ecologia e Conserva<e7><e3>o", "EH9 3JU", "EMBRAPA Pantanal", "Engl", "Expasoft Ltd", "F 54506", "Faculty of Forestry", "Fax 717-724-2525\\)", "Florida Atlantic University", "Fr", "France\\)", "Gazpromneft NTC Ltd.", "GeoEcoMar", "Geomatics and Forest Economics", "GIS", "Gödöllo and #x030B", "Gorgan University of Agricultural Sciences and Natural Resources", "Grogan University of Agriculture Science and Natural Resources", "Hong Kong", "Hong Kong Hong Kong \\(e-mail: abensous@cityu.edu.hk\\)", "IEEE", "IGN/SR-MATIS", "Inc.", "Indian Institute of Chemical Engineers", "Indian Institute of Engineers", "INPE", "INSU", "Irand", "Jadavpur University", "Jpn", "Khamid Alimjan str.", "L.S.S. International", "Laboratory of Dendrometry and Forest Productivity", "Libyan Arab Jamahiriya", "Macau", "Ministry of Agriculture", "Ministry of Labors and Social Affairs", "No. 1", "North Macedonia", "Northwestern Polytechnical University", "ORNL Distinguished Scientist Program", "Planning Engenharia e Consultoria", "Portsmouth", "Praha", "Pretoria Portland Cement", "Price Waterhouse", "QLD 4066", "Regis University", "research assistant at Humber River Hospital", "research coordinator at Humber River Hospital", "Russian Federation", "S-170 11", "S-739 92", "S-842 31", "Sale", "Serbia", "South Afr", "surgeon at Humber River Hospital", "Syrian Arab Republic", "Taiwan Taiwan \\(e-mail: jhwang@csie.ntut.edu.tw\\)", "Tanzania", "The Republic of Iraq", "Tianjin", "Townsville Port Authority", "Tribhuvan UniversityNPL", "U.S.A. \\(Tel. 717-724-3322", "UFMS", "UK", "Universidad Politécnica de Madrid", "University of Aberdeen", "University of Oulu", "University of Southern California", "University of Technology", "University of Tennessee", "University Putra Malaysia", "USA", "USA.", "USA. johnsok3@ohio.edu", "USGS/Biological Resources Division", "USSR", "Viet Nam", "Virginia Commonwealth University", "Virginia Commonwealth University\\;", "Virginia Department of Health", "W.C.I", "West Ger", "Yugoslavia")
# new <- as.vector(country_list$Country) 
# for(i in 1:nrow(river_ml_data)){
#     for(j in 1:length(river_ml_data$Country[[i]])){
#         for (k in 1:length(old)){
#             if(!is.null(river_ml_data$Country[[i]])){
#                 river_ml_data$Country[[i]][[j]] <- str_replace_all(river_ml_data$Country[[i]][[j]], old[k], new[k])
#             }
#         }
#     }
# }
#** add the names of the country as columns ####

add_namecolumn <- function(x){
    if (nrow(x) == 0){
        return(x)
    } else {
        b <- as.data.frame(matrix(data = NA, nrow = nrow(x), ncol = length(levels(as.factor(unlist(x$Country))))))
        colnames(b) <- levels(as.factor(unlist(x$Country)))
        y <- bind_cols(x, b)
        return(y)
    }
}
river_ml_data <- add_namecolumn(river_ml_data)

# remove wrong name column 

`%ni%` <- Negate(`%in%`)

river_ml_data <- subset(river_ml_data, select = names(river_ml_data) %ni% country_diff)

#** Making new lat and long columns ####

# remove the countries that are not in river_ml_data in lat_long

create_latlong <- function(x, y){
    z <- x %>% filter(Country %in% y)
    z <- z[order(z$Country),]
    return(z)
}
names_country <- colnames(river_ml_data)[19:ncol(river_ml_data)]

lat_long <- lat_long %>% filter(Country %in% names_country)
lat_long <- lat_long[order(lat_long$Country),]

# check match the order of names_country and lat_long$Country

all(diff(match(names_country, lat_long$Country))) > 0 # true means ok

# making new lat and long columns

# transfer country columns from logical to character
river_ml_data[, (which(colnames(river_ml_data) == 'Country')+1):ncol(river_ml_data)] <- lapply(river_ml_data[, (which(colnames(river_ml_data) == 'Country')+1):ncol(river_ml_data)], as.character)
river_ml_data_country <- river_ml_data[, (which(colnames(river_ml_data) == 'Country')+1):ncol(river_ml_data), drop = F]


for (i in seq_len(nrow(river_ml_data))){ 
    for (j in seq_along(river_ml_data$Country[[i]])){
        for (k in seq_along(river_ml_data_country)){
            if(colnames(river_ml_data_country)[k] == river_ml_data$Country[[i]][j]){
                river_ml_data_country[i,k] <- river_ml_data$Country[[i]][j]
            }
        }
    }
}

river_ml_data_lat <- river_ml_data_country
colnames(river_ml_data_lat) <- paste("lat",colnames(river_ml_data_country), sep = "_")
river_ml_data_long <- river_ml_data_country
colnames(river_ml_data_long) <- paste("long",colnames(river_ml_data_country), sep = "_")
river_ml_data_country <- as.data.frame(river_ml_data_country)

for(i in seq_along(river_ml_data_country)){
    for(j in seq_len(nrow(river_ml_data_country))){
        if(!is.na(river_ml_data_country[j,i])){
            river_ml_data_lat[j,i] <- lat_long$y[i]
            river_ml_data_long[j,i] <- lat_long$x[i]
        }
    }
}

river_ml_data[, (which(colnames(river_ml_data) == 'Country')+1):ncol(river_ml_data)] <- NULL
river_ml_data <- bind_cols(river_ml_data, river_ml_data_country, river_ml_data_lat, river_ml_data_long)


# Stick type of ML to the data frame ####

ml_type <- tribble(
    ~`.id`, ~`ML`,
    "Associate rule", "Unsupervised Learning",
    "Big_data", "Big Data",
    "Clustering", "Unsupervised Learning",
    "Decision trees", "Supervised Learning",
    "Deep_learning", "Deep Learning",
    "Discriminant_analysis", "Supervised Learning",
    "Ensemble", "Supervised Learning",
    "Feature selection", "Supervised Learning",
    "GPC", "Supervised Learning",
    "Human_interpretation", "Human interpretable information extraction",
    "knn", "Supervised Learning",
    "Linear models", "Supervised Learning",
    "Manifold", "Unsupervised Learning",
    "Matrix factorization", "Unsupervised Learning",
    "Multiclass", "Supervised Learning",
    "Naive Bayes", "Supervised Learning",
    "Neural network", "Unsupervised Learning",
    "Neural networks", "Supervised Learning",
    "Reinforcement_learning", "Reinforcement Learning",
    "SGD", "Supervised Learning",
    "SVM", "Supervised Learning"
)

river_ml_data <- left_join(river_ml_data,  ml_type, by = ".id")
river_ml_data<- river_ml_data %>% select(ML, everything())
colnames(river_ml_data)[2] <- "id"
# Before 1980s, 1980s, 1990s, 2000s, 2010s ####

river_ml_data$Period <- NA
river_ml_data$Period[river_ml_data$Year < 1980] <- "< 1980s"
river_ml_data$Period[between(river_ml_data$Year, lower = 1980, upper = 1989)] <- "1980s"
river_ml_data$Period[between(river_ml_data$Year, lower = 1990, upper = 1999)] <- "1990s"
river_ml_data$Period[between(river_ml_data$Year, lower = 2000, upper = 2009)] <- "2000s"
river_ml_data$Period[between(river_ml_data$Year, lower = 2010, upper = 2019)] <- "2010s"
river_ml_data$Period[between(river_ml_data$Year, lower = 2020, upper = 2021)] <- "2020"
river_ml_data$Period <- as.factor(river_ml_data$Period)
river_ml_data$Period <- relevel(river_ml_data$Period, "< 1980s")


# Categorize research to different research topics ####

topic_list <- c("Water Quality/Pollution", "Heavy Metal", "Climate Change", "Land use change", "Sediment", "Eutrophication", "Groundwater",
                "Hydrology", "Estuaries", "Hydropower and dams", "Biodiversity", "Antibiotic resistance", "Drinking water", "Fisheries", 
                "Management", "Aquatic environment", "Biogeochemistry", "Public health", "Movement", "Spatiotemporal trends", "Microbial")


river_ml_data[, topic_list] <- NA

river_ml_data$`Water Quality/Pollution`[str_detect(str_to_lower(river_ml_data$`Author Keywords`), 
                                                   pattern = "water quality|wqi|pollut*|contaminat*|wastewater|acidifi*|treatment")] <- 1
river_ml_data$`Heavy Metal`[str_detect(str_to_lower(river_ml_data$`Author Keywords`), 
                                       pattern = "heavy metal*|mercury|lead*|cadimum*|copper*|chromium*|nickel*|arsenic*|manganese*|
                                       cobalt*|zinc*|selenium*|silver*|antimony*|thallium*|metal*|metalloid*|radium*|bioaccumulat*|
                                       bioavailability|copper*|iron*")] <- 1

river_ml_data$`Climate Change`[str_detect(str_to_lower(river_ml_data$`Author Keywords`), pattern = "climate change*|global warming|
                                          climate warming*|kyoto protocol|paris agreement|palaeoclimat*|climate polic*|climate")] <- 1

river_ml_data$`Land use change`[str_detect(str_to_lower(river_ml_data$`Author Keywords`), pattern = "land use*|landscape|land-use|
                                           urban*|land cover|regulated river*")] <- 1

river_ml_data$Sediment[str_detect(str_to_lower(river_ml_data$`Author Keywords`), pattern = "sediment*")] <- 1

river_ml_data$Eutrophication[str_detect(str_to_lower(river_ml_data$`Author Keywords`), pattern = "eutrophic*|entrich*|nutrient*|nitrogen*|phoph*|nitrat*")] <- 1


river_ml_data$Groundwater[str_detect(str_to_lower(river_ml_data$`Author Keywords`), pattern = "ground water|groundwater*|underground")] <- 1

river_ml_data$Hydrology[str_detect(str_to_lower(river_ml_data$`Author Keywords`), pattern = "hydrolog*|flood|precipitation|
                                        streamflow|drought|runoff|run-off|surface water|rain*|landslide|floodplain|stream*|discharge|
                                        erosion*|riparian|flow|regime*")] <- 1

river_ml_data$Estuaries[str_detect(str_to_lower(river_ml_data$`Author Keywords`), pattern = "estuar*|salin*")] <- 1

river_ml_data$`Hydropower and dams`[str_detect(str_to_lower(river_ml_data$`Author Keywords`), pattern = "hydropower*|hydroelectric*|
                                               hydro power|dam*|weir*|embankment*|dike*|ditch*|wall*|barrier*|levee*|bank*")] <- 1

river_ml_data$Biodiversity[str_detect(str_to_lower(river_ml_data$`Author Keywords`), pattern = "biodivers*|diversit*|macroinvertebrate*|fish|zoo*|
                                      phyto*|commmunit*|diatom|species*|abundance*|macrophyte*|algae*|cyanobacteria|insect*|trout*|richness*|
                                      macrozoobethos*|chronomidae|otolith*")] <- 1

river_ml_data$`Antibiotic resistance`[str_detect(str_to_lower(river_ml_data$`Author Keywords`), pattern = "antibiotic*")] <- 1

river_ml_data$`Drinking water`[str_detect(str_to_lower(river_ml_data$`Author Keywords`), pattern = "drink*|tap|potable|human consum*")] <- 1

river_ml_data$Fisheries[str_detect(str_to_lower(river_ml_data$`Author Keywords`), pattern = "fisher*|aquacult*|aquafarm*|fish* yield|
                                   fish* harvest*|fish* sustainab*|fish* stock*")] <- 1

river_ml_data$Management[str_detect(str_to_lower(river_ml_data$`Author Keywords`), pattern = "decision|directive*|manage*|sustainable develop*|
                                   guidline*|strateg*|DPSIR|policymak*|decision-mak*|decision mak*|decisionmak*|polic*")] <- 1

river_ml_data$`Aquatic environment`[str_detect(str_to_lower(river_ml_data$`Author Keywords`), pattern = "dissolved oxygen|DO|temperature|
                                               environment|vegetration|turbid*|organic matter|cod|bod|doc|chlorophyll|escherichia coli|coliform*|
                                               ph|influencing factor*|physicochemical")] <- 1

river_ml_data$Biogeochemistry[str_detect(str_to_lower(river_ml_data$`Author Keywords`), pattern = "biogeochemi*|geochemi*|isotope|
                                         biogeograph*|holocene")] <- 1

river_ml_data$`Public health`[str_detect(str_to_lower(river_ml_data$`Author Keywords`), pattern = "disease*|illness*|sick*|human health|
                                         public health|health risk*|health care|physical health|mental|maternal|child*")] <- 1

river_ml_data$Movement[str_detect(str_to_lower(river_ml_data$`Author Keywords`), pattern ="move*|migrat")] <- 1
river_ml_data$`Spatiotemporal trends`[str_detect(str_to_lower(river_ml_data$`Author Keywords`), pattern = "temporal*|season*|spati*")] <- 1
river_ml_data$Microbial[str_detect(str_to_lower(river_ml_data$`Author Keywords`), pattern = "microbi|16s rdna|fluorescence")] <- 1

# Categorize research to different modelling techniques ####
# model validation, model evaluation, hyperparameter tuning, sensitivity analysis, uncertainty analysis, model selection, model optimization

model_list <- c("Model validation", "Model evaluation", "Model optimization", "Model selection", 
                "Hyperparameter tuning", "Sensitivity analysis", "Uncertainty analysis")


river_ml_data[, model_list] <- 0

river_ml_data$`Model validation`[str_detect(str_to_lower(river_ml_data$`Author Keywords`), pattern = "cross validation|cross-validation|model valid*|
                                            three-fold|three fold|3-fold|3 fold|k-fold|k fold|ten fold|ten-fold|10 fold|10-fold|leave one out|loo|
                                            loocv|leave one group out|lpo|lpocv|leave p groups out|lpgo|hold-out|hold out|shuffle split|time series split|bootstrap") | 
                                   str_detect(str_to_lower(river_ml_data$Abstract), pattern = "cross validation|cross-validation|model valid*|
                                            three-fold|three fold|3-fold|3 fold|k-fold|k fold|ten fold|ten-fold|10 fold|10-fold|leave one out|loo|
                                            loocv|leave one group out|lpo|lpocv|leave p groups out|lpgo|hold-out|hold out|shuffle split|time series split|bootstrap")] <- 1 

river_ml_data$`Model evaluation`[str_detect(str_to_lower(river_ml_data$`Author Keywords`), pattern = "learning curve*|confusion matri*|model accurac*|
                                            model precision|model recall|model specificity|model sensitivity|true positive rate*|tpr|true negative rate*|
                                            positive predictive value*|ppv|negative predictive value*|npv|false negative rate*|fnr|false positive rate*|
                                            fpr|matthews correlation coefficient*|mcc|fowlkes–mallows index*|fowlkes mallows index*|fm index|cohen’s kappa|kappa|
                                            correctly classified instances|cci|f1 score*|f-score*|f score*|f-measure*|jaccard|receiver operating characteristic|
                                            roc|area under the curve|auc|log loss|max error|mean absolute error|mae|mean squared error|mse|
                                            median absolute error|root mean squared error|rmse|root mean squared logarithmic error|rmsae|mdape|mape|gmrae|gmape|
                                            r-squared|r square|coefficient of determination|gini coefficient") | 
                                   str_detect(str_to_lower(river_ml_data$Abstract), pattern = "learning curve*|confusion matri*|model accurac*|
                                            model precision|model recall|model specificity|model sensitivity|true positive rate*|tpr|true negative rate*|
                                            positive predictive value*|ppv|negative predictive value*|npv|false negative rate*|fnr|false positive rate*|
                                            fpr|matthews correlation coefficient*|mcc|fowlkes–mallows index*|fowlkes mallows index*|fm index|cohen’s kappa|kappa|
                                            correctly classified instances|cci|f1 score*|f-score*|f score*|f-measure*|jaccard|receiver operating characteristic|
                                            roc|area under the curve|auc|log loss|max error|mean absolute error|mae|mean squared error|mse|
                                            median absolute error|root mean squared error|rmse|root mean squared logarithmic error|rmsae|mdape|mape|gmrae|gmape|
                                            r-squared|r square|coefficient of determination|gini coefficient")] <- 1
river_ml_data$`Model selection`[str_detect(str_to_lower(river_ml_data$`Author Keywords`), pattern = "step-backward|step-forward|step backward|step forward|
                                           bias variance trade-off*|stepwise regression|forward stepwise|backward stepwise|backward selection|forward selection|
                                           top-down|top down|step up|step-up|variable selection|likelihood ratio test|lrt|hypothesis testing|t-test|f*-test|
                                           information criteri*|aic|bic|dic|fic|kic|waic|ebic|efic|lagrange multiplier*|wald test*|score test*") |
                                  str_detect(str_to_lower(river_ml_data$Abstract), pattern = "step-backward|step-forward|step backward|step forward|
                                           bias variance trade-off*|stepwise regression|forward stepwise|backward stepwise|backward selection|forward selection|
                                           top-down|top down|step up|step-up|variable selection|likelihood ratio test|lrt|hypothesis testing|t-test|f*-test|
                                           information criteri*|aic|bic|dic|fic|kic|waic|ebic|efic|lagrange multiplier*|wald test*|score test*")] <- 1
river_ml_data$`Model optimization`[str_detect(str_to_lower(river_ml_data$`Author Keywords`), pattern = "model optimiz*|loss function minimization|
                                              loss function minimization|optimization method*|optimisation method*|multiobjective optimization|
                                              multiobjective optimization|optimization model*|optimization model*") | 
                                     str_detect(str_to_lower(river_ml_data$Abstract), pattern = "model optimiz*|loss function minimization|
                                              loss function minimization|optimization method*|optimisation method*|multiobjective optimization|
                                              multiobjective optimization|optimization model*|optimization model*")] <- 1 
river_ml_data$`Hyperparameter tuning`[str_detect(str_to_lower(river_ml_data$`Author Keywords`), pattern = "hyperparameter*|hyper-parameter*|
                                                 grid search|parameter optimization|parameter optimisation|random search|
                                                 bayesian optimization|Bayesian optimisation|gradient-based optimization|gradient-based optimisation|
                                                 evolutionary optimization|evolutionary optimisation|population based training|population-based training|PBT") | 
                                        str_detect(str_to_lower(river_ml_data$Abstract), pattern = "hyperparameter*|hyper-parameter*|
                                                 grid search|parameter optimization|parameter optimisation|random search|
                                                 bayesian optimization|Bayesian optimisation|gradient-based optimization|gradient-based optimisation|
                                                 evolutionary optimization|evolutionary optimisation|population based training|population-based training|PBT")] <- 1
river_ml_data$`Sensitivity analysis`[str_detect(str_to_lower(river_ml_data$`Author Keywords`), pattern = "sensitivity analys*|model sensitivity") |
                                       str_detect(str_to_lower(river_ml_data$Abstract), pattern = "sensitivity analys*|model sensitivity")] <- 1
river_ml_data$`Uncertainty analysis`[str_detect(str_to_lower(river_ml_data$`Author Keywords`), pattern = "uncertainty analys*|uncertainty propagat*|
                                                uncertainty assess*|uncertainty characteri*|uncertainty matri*|error propagat*|Monte Carlo|
                                                statistical uncertaint*|model uncertaint*|data uncertainty engine|due|output uncertaint*") |
                                       str_detect(str_to_lower(river_ml_data$Abstract), pattern = "uncertainty analys*|uncertainty propagat*|
                                                uncertainty assess*|uncertainty characteri*|uncertainty matri*|error propagat*|Monte Carlo|
                                                statistical uncertaint*|model uncertaint*|data uncertainty engine|due|output uncertaint*")] <-1

# save the file ####
river_ml_data_final <- river_ml_data %>% select(-Affi, -Country)
river_ml_data_final$id <- as.factor(river_ml_data_final$id)
river_ml_data_final$id <- factor(river_ml_data_final$id, labels = c("Associate rule", "Big data", "Clustering", "Decision trees", "Deep learning",
                                                    "Discriminant Analysis", "Ensemble methods", "Feature selection","Gaussian processes", 
                                                    "Human interpretable information extraction", "Nearest neighbors", "Linear models", 
                                                    "Manifold learning", "Matrix factorization","Multiclass and multilabel algorithms", "Naive Bayes", 
                                                    "Supervised neural networks", "Unsupervised neural networks", "Reinforcement learning", 
                                                    "Stochastic Gradient Descent", "Support Vector Machines"))


river_ml_short <- river_ml_data[,c(1:str_which(colnames(river_ml_data), "Country"), str_which(colnames(river_ml_data), "Period"):ncol(river_ml_data))]

write_csv(river_ml_data_final, "river_ml_data.csv")
write_feather(river_ml_data_final, "river_ml_data.feather")
rm(ml_type, river_ml_data_country, river_ml_data_lat, river_ml_data_long, list_river_mlvised)

# Temporal trends of research topics (NOT SO GOOD) #### 

river_research <- river_ml_short[,c(1,2,str_which(colnames(river_ml_short), "Period"):str_which(colnames(river_ml_short), "Microbial"))]
river_research[is.na(river_research)] <- 0
river_research$ML <- as.factor(river_research$ML)
river_research$ML <- factor(river_research$ML, 
                            levels = c("Supervised Learning", "Unsupervised Learning",
                                                              "Deep Learning", "Reinforcement learning",
                                                              "Human interpretable information extraction", "Big Data"),
                            labels = c("Supervised Learning", "Unsupervised Learning",
                                         "Deep Learning", "Reinforcement Learning",
                                         "Human interpretable info extraction", "Big Data"))

river_research$id <- as.factor(river_research$id)
research_total <- as_tibble(lapply(river_research[,4:ncol(river_research)], sum))

# Over period and id 

river_research_ML_id <- aggregate(data = river_research, .~Period+id+ML, sum) 
river_research_ML_id <- river_research_ML_id %>% pivot_longer(cols = -c(Period, id, ML), 
                                                              names_to = "Research Topics", 
                                                              values_to = "Number of publications")

ggsave("research_id_period.jpeg", ggplot(river_research_ML_id %>% 
                                           filter(Period != 2020), aes(x = Period, 
                                                                       y= `Number of publications`, 
                                                                       group = `Research Topics`)) +
    geom_point(aes(color = `Research Topics`)) +
    geom_line(aes(color = `Research Topics`)) +
    theme_bw() +
    ylab("Total number of publications") +
    facet_wrap(.~id, scales = "free_y") +
    # scale_color_brewer(palette = "Dark2") +
    theme(text=element_text(size=16),
          strip.text.x = element_text(size=14),
          axis.text = element_text(size=12),
          axis.title = element_text(size=14),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(size = 12))
    ,  units = 'cm', height = 20, width = 30, dpi = 300)


# over period and over ML types 

river_research_ML <- river_research %>% select(-id) 
river_research_ML <- aggregate(data = river_research_ML, .~Period+ML, sum) 
river_research_ML <- river_research_ML %>% pivot_longer(cols = -c(Period, ML), 
                                                        names_to = "Research Topics", 
                                                        values_to = "Number of publications")

ggsave("research_ML_period.jpeg", ggplot(river_research_ML %>% filter(Period != 2020), 
                                         aes(x = Period, y= `Number of publications`, group = `Research Topics`)) +
           geom_point(aes(color = `Research Topics`)) +
           geom_line(aes(color = `Research Topics`)) +
           theme_bw() +
           ylab("Total number of publications") +
           facet_wrap(.~ML, scales = "free_y") +
           # scale_color_brewer(palette = "Dark2") +
           theme(text=element_text(size=16),
                 strip.text.x = element_text(size=14),
                 axis.text = element_text(size=12),
                 axis.title = element_text(size=14),
                 legend.position = "bottom",
                 legend.title = element_blank(),
                 legend.text = element_text(size = 12))
       ,  units = 'cm', height = 20, width = 30, dpi = 300)

# over periods only
river_research_period <- river_research %>% select(-id, - ML) 
river_research_period <- aggregate(data = river_research_period, .~Period, sum)
river_research_period <- river_research_period %>% pivot_longer(cols = -c(Period), names_to = "Research Topics", values_to = "Number of publications")

ggsave("research_period.jpeg", ggplot(river_research_period %>% filter(Period != 2020), aes(x = Period, y= `Number of publications`, group = `Research Topics`)) +
           geom_point() +
           geom_line() +
           theme_bw() +
           ylab("Total number of publications") +
           xlab("Periods") +
           facet_wrap(.~`Research Topics`, scales = "free_y") +
           # scale_color_brewer(palette = "Dark2") +
           theme(text=element_text(size=16),
                 strip.text.x = element_text(size=12),
                 axis.text = element_text(size=8.5),
                 axis.title = element_text(size=14),
                 legend.position = "bottom",
                 legend.title = element_blank(),
                 legend.text = element_text(size = 12))
       ,  units = 'cm', height = 20, width = 30, dpi = 300)


#** Temporal trends of research rank (GOOD) ####

# make rank
river_research_rank <- river_research_period %>% 
    arrange(Period, -`Number of publications`) %>% 
    group_by(Period) %>% 
    mutate(Rank = rank(desc(`Number of publications`), ties.method = "min"))

ggsave("research_rank.jpeg", ggplot(river_research_rank %>% filter(Period != "< 1980s"), aes(x = Period, y= Rank, group = `Research Topics`)) +
           geom_point() +
           geom_line() +
           theme_bw() +
           ylab("Total number of publications") +
           facet_wrap(.~`Research Topics`, scales = "free_y") +
           scale_y_reverse() +
           # scale_color_brewer(palette = "Dark2") +
           theme(text=element_text(size=16),
                 strip.text.x = element_text(size=14),
                 axis.text = element_text(size=12),
                 axis.title = element_text(size=14),
                 legend.position = "bottom",
                 legend.title = element_blank(),
                 legend.text = element_text(size = 12))
       ,  units = 'cm', height = 20, width = 30, dpi = 300)

# divide into group  increase/decrease and stable
river_research_rank$Trends <- NA
for (i in seq_len(nlevels(as.factor(river_research_rank$`Research Topics`)))){
    if (max(river_research_rank$Rank[river_research_rank$`Research Topics` == river_research_rank$`Research Topics`[i]])-
        min(river_research_rank$Rank[river_research_rank$`Research Topics` == river_research_rank$`Research Topics`[i]]) < 3){
        river_research_rank$Trends[river_research_rank$`Research Topics` == river_research_rank$`Research Topics`[i]] <- "Stable"
    } else if (river_research_rank$Rank[river_research_rank$`Research Topics` == river_research_rank$`Research Topics`[i]][1]-
          river_research_rank$Rank[river_research_rank$`Research Topics` == river_research_rank$`Research Topics`[i]][5] > 2){
        river_research_rank$Trends[river_research_rank$`Research Topics` == river_research_rank$`Research Topics`[i]] <- "Increasing"
    } else if (river_research_rank$Rank[river_research_rank$`Research Topics` == river_research_rank$`Research Topics`[i]][1]-
          river_research_rank$Rank[river_research_rank$`Research Topics` == river_research_rank$`Research Topics`[i]][5] < -2){
        river_research_rank$Trends[river_research_rank$`Research Topics` == river_research_rank$`Research Topics`[i]] <- "Decreasing"
    } else if (river_research_rank$Rank[river_research_rank$`Research Topics` == river_research_rank$`Research Topics`[i]][3]-
               river_research_rank$Rank[river_research_rank$`Research Topics` == river_research_rank$`Research Topics`[i]][6] > 2){
        river_research_rank$Trends[river_research_rank$`Research Topics` == river_research_rank$`Research Topics`[i]] <- "Increasing"
    } else if (river_research_rank$Rank[river_research_rank$`Research Topics` == river_research_rank$`Research Topics`[i]][3]-
               river_research_rank$Rank[river_research_rank$`Research Topics` == river_research_rank$`Research Topics`[i]][6] < -2){
        river_research_rank$Trends[river_research_rank$`Research Topics` == river_research_rank$`Research Topics`[i]] <- "Decreasing"
    } else if (river_research_rank$Rank[river_research_rank$`Research Topics` == river_research_rank$`Research Topics`[i]][4]-
               river_research_rank$Rank[river_research_rank$`Research Topics` == river_research_rank$`Research Topics`[i]][6] > 2){
        river_research_rank$Trends[river_research_rank$`Research Topics` == river_research_rank$`Research Topics`[i]] <- "Increasing"
    } else {
        river_research_rank$Trends[river_research_rank$`Research Topics` == river_research_rank$`Research Topics`[i]] <- "Stable"
    }
}

river_research_rank$Trends[river_research_rank$`Research Topics` == "Management"] <- "Stable"
river_research_rank$Trends[river_research_rank$`Research Topics` == "Groundwater"] <- "Stable"

river_research_rank$Trends <- as.factor(river_research_rank$Trends)
river_research_rank$Trends <- relevel(river_research_rank$Trends, "Increasing")

# research topics groundwater and managment should be moved to stable 

river_research_rank$`Research Topics` <- as.factor(river_research_rank$`Research Topics`)
plot_rank <- ggplot(river_research_rank, aes(x = Period, y= Rank, group = `Research Topics`)) + # not so good 
    geom_point(aes(color = Trends, size = 1.01)) +
    geom_line(aes(color = Trends, size = 1.005)) +
    theme_bw() +
    ylab("Total number of publications") +
    facet_wrap(.~Trends, scales = "free_y") +
    scale_y_reverse() +
    scale_color_brewer(palette = "Dark2") +
    theme(text=element_text(size=16),
          strip.text.x = element_text(size=14),
          axis.text = element_text(size=12),
          axis.title = element_text(size=14),
          legend.position = c(0.8, 0.2),
          legend.title = element_blank(),
          legend.text = element_text(size = 12))
plot_rank # not nice
ggsave("research_rank_grouped.jpeg", plot_rank,  units = 'cm', height = 20, width = 35, dpi = 300)

rank_research <- tibble(`Research Topics` = levels(river_research_rank$`Research Topics`), nlevels = seq_len(nlevels(river_research_rank$`Research Topics`)))

river_research_rank <- left_join(river_research_rank, rank_research, by = "Research Topics")

cols <- c("Increasing" = "red", "Decreasing" = "blue", "Stable" = "violet")

plot_rank_group <- ggplot(river_research_rank, aes(x = Period, y= Rank, group = `Research Topics`)) + # Good graph 
    geom_point(aes(color = Trends, size = 1.01)) +
    geom_line(aes(color = Trends, size = 1.005, alpha = 0.8)) +
    theme_bw() +
    ylab("Rank of Research Topics") +
    xlab("Periods") +
    facet_wrap(.~Trends, ncol = 2) +
    scale_y_reverse() +
    # scale_colour_manual(values = cols, ) +
    scale_color_brewer(palette = "Reds", direction=-1) +
    theme(text=element_text(size=22),
          strip.text.x = element_text(size=20),
          axis.text = element_text(size=18),
          axis.title = element_text(size=20),
          legend.position = "none",
          legend.title = element_blank(),
          legend.text = element_text(size = 14))+ 
    geom_text(data = river_research_rank[river_research_rank$Period == "2020", ][1:9,], 
              aes(label = river_research_rank$Rank[river_research_rank$Period == "2020"][1:9]),
                            hjust = -1.05 ,vjust = 0.5, size = 6) +
    geom_text(data = river_research_rank[river_research_rank$Period == "2020", ][10:21,], 
              aes(label = river_research_rank$Rank[river_research_rank$Period == "2020"][10:21]),
              hjust = -0.5 ,vjust = 0.5, size = 6) 
plot_rank_group
ggsave("research_rank_grouped_color.jpeg", plot_rank_group
       ,  units = 'cm', height = 40, width = 30, dpi = 300)

plot_rank_group_editable <- dml(ggobj = plot_rank_group)
plot_rank_group_doc <- read_pptx()
plot_rank_group_doc <- add_slide(plot_rank_group_doc)
plot_rank_group_doc <- ph_with(x = plot_rank_group_doc, value = plot_rank_group_editable, location = ph_location_type(type = "body"))
print(plot_rank_group_doc, target = "research_rank_grouped.pptx")

#** Rank variability over decades (BETTER THAN YEARS) ####

river_trend_decade_err <- list(aggregate(data = river_research_rank, Rank ~ `Research Topics`, FUN = mean),
                              aggregate(data = river_research_rank, Rank ~ `Research Topics`, FUN = min),
                              aggregate(data = river_research_rank, Rank ~ `Research Topics`, FUN = max)) %>% 
    reduce(full_join, by = "Research Topics")

colnames(river_trend_decade_err)[2:4] <- c("Mean rank", "Min rank", "Max rank")

cc <- seq_gradient_pal("blue", "red", "Lab")(seq(0, 1, length.out=21))

# reorder research topic order based on mean rank

river_trend_decade_err$`Research Topics` <- reorder(river_trend_decade_err$`Research Topics`, -river_trend_decade_err$`Mean rank`)

river_trend_decade_err <- river_trend_decade_err %>% arrange(`Mean rank`)

plot_rank_decade <- ggplot(river_trend_decade_err, aes(y = `Research Topics`, x =`Mean rank`)) +
    geom_point(aes(size = 0.5, color = `Research Topics`)) + 
    geom_errorbar(aes(xmin=`Min rank`, xmax=`Max rank`, color = `Research Topics`), width=0,
                  position=position_dodge(0.05)) + 
    scale_x_continuous(position = "top", limits = c(0,21)
                       ,expand = c(0, 0)# remove the gap between axis and plot area
                       ) + 
    scale_colour_manual(values=cc) + 
    theme_classic() + 
    geom_text(data = river_trend_decade_err[1:12,], # by dividing this into smaller dataframes we can have it in different sides
              aes(label = `Research Topics`, x = `Max rank` +3),
              # hjust = -1.05 ,
              vjust = 0.25, size = 5) +
    geom_text(data = river_trend_decade_err[13:21,], # by dividing this into smaller dataframes we can have it in different sides
              aes(label = `Research Topics`, x = `Min rank` -3),
              # hjust = -1.05 ,
              vjust = 0.25, size = 5) +
    # geom_vline(xintercept = 10,  
    #            color = "purple", size=0.5)+
    xlab("Rank of Research Topics")+
    theme(text=element_text(size=14),
          # axis.line.x = element_line(size = 1),
          axis.ticks.x = element_line(size = 1),
          axis.ticks.length.x = unit(5, "pt"),
          axis.text = element_text(size=14),
          axis.title = element_text(size=20),
          axis.title.y = element_blank(),
          axis.line.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = "none")

plot_rank_decade

ggsave("research_rank_decade.jpeg", plot_rank_decade,  units = 'cm', height = 20, width = 20, dpi = 300)

plot_rank_decade_editable <- dml(ggobj = plot_rank_decade)
plot_rank_decade_doc <- read_pptx()
plot_rank_decade_doc <- add_slide(plot_rank_decade_doc)
plot_rank_decade_doc <- ph_with(x = plot_rank_decade_doc, value = plot_rank_decade_editable, location = ph_location_type(type = "body"))
print(plot_rank_decade_doc, target = "research_rank_decade.pptx")

#** Rank variability over years (NOT SO GOOD)####

river_rank_year <- river_ml_short[,c(1,2, str_which(colnames(river_ml_short), "Year"), (str_which(colnames(river_ml_short), "Period")+1):str_which(colnames(river_ml_short), "Microbial"))]
river_rank_year$ML <- as.factor(river_rank_year$ML)
river_rank_year$ML <- factor(river_rank_year$ML, levels = c("Supervised Learning", "Unsupervised Learning",
                                                          "Deep Learning", "Reinforcement learning",
                                                          "Human interpretable information extraction", "Big Data"),
                            labels = c("Supervised Learning", "Unsupervised Learning",
                                       "Deep Learning", "Reinforcement Learning",
                                       "Human interpretable info extraction", "Big Data"))
river_rank_year$id <- as.factor(river_rank_year$id)
rank_year_total <- as_tibble(lapply(river_rank_year[,4:ncol(river_rank_year)], sum))

river_research_year <- river_rank_year %>% select(-id, - ML) 
river_research_year[is.na(river_research_year)] <- 0
river_research_year <- aggregate(data = river_research_year, . ~ Year, FUN = sum, na.rm=TRUE) 
river_research_year <- river_research_year %>% filter(Year != "2021") %>% 
    pivot_longer(cols = -c(Year), names_to = "Research Topics", values_to = "Number of publications")

river_trend_year <- river_research_year %>% 
    arrange(Year, -`Number of publications`) %>%
    group_by(Year) %>% mutate(Rank = rank(desc(`Number of publications`), ties.method = "min"))

river_trend_year$`Research Topics` <- as.factor(river_trend_year$`Research Topics`)
# see the trends
ggsave("research_rank_year.jpeg", ggplot(river_trend_year %>% filter(Year > 1979), aes(x = Year, y= Rank, group = `Research Topics`)) +
    geom_point() +
    geom_line() +
    geom_smooth() +
    theme_bw() +
    ylab("Total number of publications") +
    facet_wrap(.~`Research Topics`, scales = "free_y") +
    scale_y_reverse() +
    # scale_color_brewer(palette = "Dark2") +
    theme(text=element_text(size=16),
          strip.text.x = element_text(size=14),
          axis.text = element_text(size=12),
          axis.title = element_text(size=14),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(size = 12))
     ,units = 'cm', height = 20, width = 30, dpi = 300)

.# making point + error bar
# make a new tibble 

river_trend_point_err <- list(aggregate(data = river_trend_year[river_trend_year$Year > 1979,], Rank ~ `Research Topics`, FUN = mean),
                               aggregate(data = river_trend_year[river_trend_year$Year > 1979,], Rank ~ `Research Topics`, FUN = min),
                               aggregate(data = river_trend_year[river_trend_year$Year > 1979,], Rank ~ `Research Topics`, FUN = max)) %>% 
    reduce(full_join, by = "Research Topics")

colnames(river_trend_point_err)[2:4] <- c("Mean rank", "Min rank", "Max rank")

cc <- seq_gradient_pal("blue", "red", "Lab")(seq(0,1,length.out=21))

plot_rank_year <- ggplot(river_trend_point_err, aes(y = `Research Topics`, x =`Mean rank`)) +
    geom_point(aes(size = 0.5, color = `Research Topics`)) + 
    geom_errorbar(aes(xmin=`Min rank`, xmax=`Max rank`, color = `Research Topics`), width=.2,
                  position=position_dodge(0.05)) + 
    scale_x_reverse(position = "top") + 
    scale_colour_manual(values=cc) + 
    theme_classic() + 
    geom_text(data = river_trend_point_err, 
              aes(label = `Research Topics`, x = `Min rank` -2),
              # hjust = -1.05 ,
              vjust = 0.5, size = 4) +
    theme(text=element_text(size=12),
          # axis.line.x = element_line(size = 1),
          axis.ticks.x = element_line(size = 1),
          axis.ticks.length.x = unit(5, "pt"),
          axis.text = element_text(size=14),
          axis.title = element_text(size=20),
          axis.title.y = element_blank(),
          axis.line.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = "none")

plot_rank_year
ggsave("rank_year.jpeg", plot_rank_year,  units = 'cm', height = 20, width = 30, dpi = 300)

# Temporal trends of ML types ####
#** ML types in different periods (NOT SO GOOD)####

river_mlt_period <- river_ml_short %>% filter(Period != "2020") %>% group_by(Period, id, ML) %>% summarise(n =n())
river_mlt_period$Period <- as.character(river_mlt_period$Period)
river_mlt_period$ML <- as.factor(river_mlt_period$ML)
river_mlt_period$ML <- factor(river_mlt_period$ML, levels = c("Supervised Learning", "Unsupervised Learning",
                                                              "Deep Learning", "Reinforcement learning",
                                                              "Human interpretable information extraction", "Big Data"),
                              labels = c("Supervised Learning", "Unsupervised Learning",
                                         "Deep Learning", "Reinforcement Learning",
                                         "Human interpretable info extraction", "Big Data"))

ggsave("river_mlt_periods.jpeg", ggplot(river_mlt_period, aes(x = Period, y= n, group = id)) +
           geom_point(aes(color = id)) +
           geom_line(aes(color = id)) +
           theme_bw() +
           ylab("Total number of publications") +
           facet_wrap(.~ML, scales = "free_y") +
           # scale_color_brewer(palette = "Dark2") +
           theme(text=element_text(size=16),
                 strip.text.x = element_text(size=14),
                 axis.text = element_text(size=12),
                 axis.title = element_text(size=14),
                 legend.position = "bottom",
                 legend.title = element_blank(),
                 legend.text = element_text(size = 12))
       ,  units = 'cm', height = 20, width = 30, dpi = 300)

ggsave("river_mlt_periods_grouped.jpeg", ggplot(river_mlt_period, aes(x = Period, y= n, group = id)) +
           geom_point(aes(color = ML)) +
           geom_line(aes(color = ML)) +
           theme_bw() +
           ylab("Total number of publications") +
           facet_wrap(.~ML, scales = "free_y") +
           scale_color_brewer(palette = "Dark2") +
           theme(text=element_text(size=16),
                 strip.text.x = element_text(size=14),
                 axis.text = element_text(size=12),
                 axis.title = element_text(size=14),
                 legend.position = "bottom",
                 legend.title = element_blank(),
                 legend.text = element_text(size = 12))
       ,  units = 'cm', height = 20, width = 30, dpi = 300)

#** ML ranks in different years (GOOD) ####

ml_topics <- river_ml_short %>% group_by(id, Year, ML) %>% summarise(n=n())
ml_topics$ML <- factor(ml_topics$ML, levels = c("Supervised Learning", "Unsupervised Learning",
                                                              "Deep Learning", "Reinforcement learning",
                                                              "Human interpretable information extraction", "Big Data"),
                              labels = c("Supervised Learning", "Unsupervised Learning",
                                         "Deep Learning", "Reinforcement Learning",
                                         "Human interpretable info extraction", "Big Data"))
ml_topics$id <- as.factor(ml_topics$id)
# ml_topics$id <- factor(ml_topics$id, labels = c("Matrix factorization", "Linear models", "Supervised neural networks",
#                                                  "Human interpretable information extraction", "Clustering", "Deep learning",
#                                                  "Ensemble methods", "Discriminant Analysis", "Unsupervised neural networks",
#                                                  "Decision trees", "Manifold learning", "Nearest neighbors", "Support Vector Machines",
#                                                  "Big data", "Stochastic Gradient Descent", "Gausian processes", "Feature selection",
#                                                  "Associate rule", "Naive Bayes", "Reinforcement learning", "Multiclass and multilabel algorithms"))

ml_topics$id <- factor(ml_topics$id, labels = c("Associate rule", "Big data", "Clustering", "Decision trees", "Deep learning",
                                                "Discriminant Analysis", "Ensemble methods", "Feature selection","Gaussian processes", 
                                                "Human interpretable information extraction", "Nearest neighbors", "Linear models", 
                                                "Manifold learning", "Matrix factorization","Multiclass and multilabel algorithms", "Naive Bayes", 
                                                "Unsupervised neural networks", "Supervised neural networks", "Reinforcement learning", 
                                                "Stochastic Gradient Descent", "Support Vector Machines"))

# make rank
ml_rank <- ml_topics %>% 
    arrange(Year, -n) %>% 
    group_by(Year) %>% 
    mutate(Rank = rank(desc(n), ties.method = "min"))

# see the trends (for data exploration)
ggplot(ml_rank %>% filter(Year > 1979), aes(x = Year, y= Rank, group = id)) +
    geom_point() +
    geom_line() +
    theme_bw() +
    ylab("Total number of publications") +
    facet_wrap(.~ id, scales = "free_y") +
    scale_y_reverse() +
    # scale_color_brewer(palette = "Dark2") +
    theme(text=element_text(size=16),
          strip.text.x = element_text(size=14),
          axis.text = element_text(size=12),
          axis.title = element_text(size=14),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(size = 12))

#** ML ranks in different period (BETTER THAN YEARS) ####

ml_topics_2 <- river_ml_short %>% group_by(id, Period, ML) %>% summarise(n=n())
ml_topics_2$ML <- factor(ml_topics_2$ML, levels = c("Supervised Learning", "Unsupervised Learning",
                                                "Deep Learning", "Reinforcement learning",
                                                "Human interpretable information extraction", "Big Data"),
                       labels = c("Supervised Learning", "Unsupervised Learning",
                                  "Deep Learning", "Reinforcement Learning",
                                  "Human interpretable info extraction", "Big Data"))
ml_topics_2$id <- as.factor(ml_topics_2$id)

# ml_topics_2$id <- factor(ml_topics_2$id, labels = c("Matrix factorization", "Linear models", "Supervised neural networks",
#                                                  "Human interpretable information extraction", "Clustering", "Deep learning",
#                                                  "Ensemble methods", "Discriminant Analysis", "Unsupervised neural networks",
#                                                  "Decision trees", "Manifold learning", "Nearest neighbors", "Support Vector Machines",
#                                                  "Big data", "Stochastic Gradient Descent", "Gausian processes", "Feature selection",
#                                                  "Associate rule", "Naive Bayes", "Reinforcement learning", "Multiclass and multilabel algorithms"))

ml_topics_2$id <- factor(ml_topics_2$id, labels = c("Associate rule", "Big data", "Clustering", "Decision trees", "Deep learning",
                                                "Discriminant Analysis", "Ensemble methods", "Feature selection","Gaussian processes", 
                                                "Human interpretable information extraction", "Nearest neighbors", "Linear models", 
                                                "Manifold learning", "Matrix factorization","Multiclass and multilabel algorithms", "Naive Bayes", 
                                                "Supervised neural networks", "Unsupervised neural networks", "Reinforcement learning", 
                                                "Stochastic Gradient Descent", "Support Vector Machines"))


# make rank
ml_rank_2 <- ml_topics_2 %>% 
    arrange(Period, -n) %>% 
    group_by(Period) %>% 
    mutate(Rank = rank(desc(n), ties.method = "min"))

# making point + error bar
# make a new tibble 

ml_rank_2_err <- list(aggregate(data = ml_rank_2, Rank ~ id, FUN = mean),
                    aggregate(data = ml_rank_2, Rank ~ id, FUN = min),
                    aggregate(data = ml_rank_2, Rank ~ id, FUN = max)) %>% reduce(full_join, by = "id")

colnames(ml_rank_2_err)[2:4] <- c("Mean rank", "Min rank", "Max rank")
cc_2 <- seq_gradient_pal("blue", "red", "Lab")(seq(0,1,length.out=21))

ml_rank_2_err$id <- reorder(ml_rank_2_err$id, -ml_rank_2_err$`Mean rank`)
ml_rank_2_err <- ml_rank_2_err %>% arrange(`Mean rank`)

plot_ml_rank_2_decade <- ggplot(ml_rank_2_err, aes(y = id, x =`Mean rank`)) +
    geom_point(aes(size = 0.5, color = id)) + 
    geom_errorbar(aes(xmin=`Min rank`, xmax=`Max rank`, color = id), width=0,
                  position=position_dodge(0.05)) + 
    scale_x_continuous(position = "top", limits = c(0,21)
                       ,expand = c(0, 0)# remove the gap between axis and plot area
    ) + 
    scale_colour_manual(values=cc_2) + 
    theme_classic() + 
    geom_text(data = ml_rank_2_err[1:2,], # by dividing this into smaller dataframes we can have it in different sides
              aes(label = id, x = `Max rank` +2),
              # hjust = -1.05 ,
              vjust = 0.25, size = 5) +
    geom_text(data = ml_rank_2_err[3,], # by dividing this into smaller dataframes we can have it in different sides
              aes(label = id, x = `Max rank` +3),
              # hjust = -1.05 ,
              vjust = 0.25, size = 5) +
    geom_text(data = ml_rank_2_err[4,], # by dividing this into smaller dataframes we can have it in different sides
              aes(label = id, x = `Max rank` +4.2),
              # hjust = -1.05 ,
              vjust = 0.25, size = 5) +
    geom_text(data = ml_rank_2_err[5:9,], # by dividing this into smaller dataframes we can have it in different sides
              aes(label = id, x = `Max rank` +2),
              # hjust = -1.05 ,
              vjust = 0.25, size = 5) +
    geom_text(data = ml_rank_2_err[10:11,], # by dividing this into smaller dataframes we can have it in different sides
              aes(label = id, x = `Max rank` +3),
              # hjust = -1.05 ,
              vjust = 0.25, size = 5) +
    geom_text(data = ml_rank_2_err[12,], # by dividing this into smaller dataframes we can have it in different sides
              aes(label = id, x = `Max rank` +2),
              # hjust = -1.05 ,
              vjust = 0.25, size = 5) +
    geom_text(data = ml_rank_2_err[13:14,], # by dividing this into smaller dataframes we can have it in different sides
              aes(label = id, x = `Min rank` -2),
              # hjust = -1.05 ,
              vjust = 0.25, size = 5) +
    geom_text(data = ml_rank_2_err[15,], # by dividing this into smaller dataframes we can have it in different sides
              aes(label = id, x = `Min rank` -3),
              # hjust = -1.05 ,
              vjust = 0.25, size = 5) +
    geom_text(data = ml_rank_2_err[16:20,], # by dividing this into smaller dataframes we can have it in different sides
              aes(label = id, x = `Min rank` -2),
              # hjust = -1.05 ,
              vjust = 0.25, size = 5) +
    geom_text(data = ml_rank_2_err[21,], # by dividing this into smaller dataframes we can have it in different sides
              aes(label = id, x = `Min rank` -3),
              # hjust = -1.05 ,
              vjust = 0.25, size = 5) +
    # geom_vline(xintercept = 10,  
    #            color = "purple", size=0.5)+
    xlab("Rank of Machine Learning")+
    theme(text=element_text(size=16),
          # axis.line.x = element_line(size = 1),
          axis.ticks.x = element_line(size = 1),
          axis.ticks.length.x = unit(5, "pt"),
          axis.text = element_text(size=14),
          axis.title = element_text(size=20),
          axis.title.y = element_blank(),
          axis.line.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = "none")

plot_ml_rank_2_decade
ggsave("ml_rank_period.jpeg", plot_ml_rank_2_decade, units = 'cm', height = 20, width = 30, dpi = 300)

plot_ml_rank_editable <- dml(ggobj = plot_ml_rank_2_decade)
plot_ml_rank_doc <- read_pptx()
plot_ml_rank_doc <- add_slide(plot_ml_rank_doc)
plot_ml_rank_doc <- ph_with(x = plot_ml_rank_doc, value = plot_ml_rank_editable, location = ph_location_type(type = "body"))
print(plot_ml_rank_doc, target = "research_ml_rank.pptx")

# Temporal trends of modeling methods trends #### 

river_modeling <- river_ml_short[,c(1,2,str_which(colnames(river_ml_short), "Year"), str_which(colnames(river_ml_short), "Period"),
                                    str_which(colnames(river_ml_short), "Model validation"):str_which(colnames(river_ml_short), "Uncertainty analysis"))]
river_modeling$ML <- as.factor(river_modeling$ML)
river_modeling$ML <- factor(river_modeling$ML, 
                            levels = c("Supervised Learning", "Unsupervised Learning",
                                       "Deep Learning", "Reinforcement learning",
                                       "Human interpretable information extraction", "Big Data"),
                            labels = c("Supervised Learning", "Unsupervised Learning",
                                       "Deep Learning", "Reinforcement Learning",
                                       "Human interpretable info extraction", "Big Data"))
river_modeling$id <- as.factor(river_modeling$id)
river_modeling_period <- river_modeling %>% select(-id, - ML, -Year) 
river_modeling_period <- aggregate(data = river_modeling_period, .~Period, sum)
river_modeling_period <- river_modeling_period %>% pivot_longer(cols = -c(Period), names_to = "Modeling methods", values_to = "Number of publications")
river_modeling_total <- river_ml_short %>% group_by(Period) %>% summarise(n=n())
river_modeling_period <- left_join(river_modeling_period, river_modeling_total, by= "Period") %>% mutate(Percentage = round(`Number of publications`*100/n, digits =2))
cc_model <- seq_gradient_pal("blue", "red", "Lab")(seq(0,1,length.out=21))


river_modeling_period$`Modeling methods` <- as.factor(river_modeling_period$`Modeling methods`)
river_modeling_period$`Modeling methods` <- factor(river_modeling_period$`Modeling methods`, 
                                                   levels = c("Model selection", "Model validation", "Uncertainty analysis", "Model evaluation",
                                                              "Sensitivity analysis", "Model optimization", "Hyperparameter tuning"))
river_modeling_period <- river_modeling_period[with(river_modeling_period, order(Period, Percentage)),]

ggsave("modeling_methods_period.jpeg", ggplot(river_modeling_period, 
                                              aes(x = Period, y= Percentage, group = `Modeling methods`)) +
         geom_point(aes(color = `Modeling methods`, size = 0.7)) +
         geom_line(aes(color = `Modeling methods`, size = 0.5)) +
         theme_bw() +
         ylab("Percentage of the publications") +
         xlab("Periods") +
         # facet_wrap(.~`Modeling methods`, scales = "free_y") +
         # scale_color_brewer(palette = "Dark2") +
         scale_colour_manual(values=cc_model[rev(seq(0,21,by = 3))]) + 
         # guides(size = FALSE) +
         geom_text(data = river_modeling_period[river_modeling_period$Period == "2020", ], 
                   aes(label = c(7:1)),
                   hjust = -1.5 ,
                   vjust = 0.5, size = 5) +
         theme(text=element_text(size=18),
               # strip.text.x = element_text(size=12),
               axis.text = element_text(size=14),
               axis.title = element_text(size=16),
               legend.position = "none",
               legend.title = element_text(size = 16),
               legend.text = element_text(size = 14))
       ,  units = 'cm', height = 30, width = 30, dpi = 300)
ggsave("modeling_methods_period2.jpeg", ggplot(river_modeling_period, 
                                               aes(x = Period, y= Percentage, group = `Modeling methods`)) +
         geom_point(aes(color = `Modeling methods`, size = 0.7)) +
         geom_line(aes(color = `Modeling methods`, size = 0.5)) +
         theme_bw() +
         ylab("Percentage of the publications (%)") +
         xlab("Periods") +
         # facet_wrap(.~`Modeling methods`, scales = "free_y") +
         scale_color_brewer(palette = "Paired",direction = -1) +
         # scale_colour_manual(values=cc_model[rev(seq(0,21,by = 3))]) + 
         guides(size = FALSE) +
         # geom_text(data = river_modeling_period[river_modeling_period$Period == "2020", ], 
         #           aes(label = c(7:1)),
         #           hjust = -1.5 ,
         #           vjust = 0.5, size = 5) +
         theme(text=element_text(size=20),
               # strip.text.x = element_text(size=12),
               axis.text = element_text(size=16),
               axis.title = element_text(size=18),
               legend.position = "bottom",
               legend.title =element_blank(),
               legend.text = element_text(size = 16))
       ,  units = 'cm', height = 20, width = 30, dpi = 300)


# World map (AFFILIATION) #### 

list_ml <- list.files(pattern = "*.txt")
list_ml_affi <- lapply(list_ml, read.delim, sep = "\t", stringsAsFactors = FALSE)
names(list_ml_affi) <- str_split_fixed(list_ml,pattern = "\\.", n =2)[,1]
ml_affi <- rbindlist(list_ml_affi, idcol = TRUE)
ml_affi$.id <- as.factor(ml_affi$.id)

# remove overlapped countries in one publication 

t1 <- data.frame()

for (i in 1:nlevels(ml_affi$.id)){
    t <- ml_affi[ml_affi$.id == levels(ml_affi$.id)[i]]
    t <-  t[!duplicated(t[,c("entry_number", "affiliation.country")]),]
    t1 <- rbind(t1, t)
}

ml_affi <- t1
rm(t1)

ml_affi <- ml_affi %>% select(.id, affilname, affiliation.city, affiliation.country)
colnames(ml_affi) <- c("id", "Institution", "City", "Country")
ml_affi <- ml_affi[-which(is.na(ml_affi[,2:4])), ] 

ml_affi_country <- ml_affi %>%
    group_by(Country) %>% 
    summarise(n = n()) %>% 
    mutate(Percentage = n*100/sum(n)) %>% 
    arrange(desc(n))
ml_affi_country$Percentage <- round(ml_affi_country$Percentage, digits = 0)

ml_affi_country$Code <- countrycode(ml_affi_country$Country, 'country.name', 'iso3c')
ml_affi_country <- ml_affi_country[complete.cases(ml_affi_country),]
ml_affi_map <- joinCountryData2Map(ml_affi_country, joinCode = "ISO3", nameJoinColumn = "Code") # name of some countries is not correct
  
jpeg("map_ml_affi.jpeg", units = 'px', height = 1500, width = 2500, res = 300, pointsize = 8)
mapParams <- mapCountryData(ml_affi_map, nameColumnToPlot = "n", 
                            addLegend=FALSE,
                            numCats = 10, mapRegion = "world", catMethod = "logFixedWidth",
                            colourPalette = brewer.pal(10,"PuBuGn"),
                            borderCol = "grey",
                            mapTitle = "",
                            missingCountryCol = "white")
do.call(addMapLegend, c(mapParams, 
                        legendWidth=0.5, 
                        legendMar = 3,
                        legendLabels ="all",
                        legendIntervals = "data"))
dev.off()

# Top keywords ####

KW_all <- function(x){
    if (nrow(x) == 0){
        return(x)
    } else {
        keyword <- strsplit(x$`Author Keywords`, "; ")
        for (i in 1:length(keyword)){
            keyword[i] <- as.data.frame(matrix(as.data.frame(keyword[i])))
        }
        keyword2 <- rbindlist(keyword)
        colnames(keyword2)[1]<- "keyword"
        keyword2<- keyword2[complete.cases(keyword2),]
        keyword2$keyword <- str_to_title(keyword2$keyword)
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'modeling', "modelling")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Lakes', "Lake")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Reservoirs', "Reservoir")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Ponds', "Pond")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Rivers', "River")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Wetlands', "Wetland")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Floods', "Flood")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Dams', "Dam")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Nutrients', "Nutrient")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Modelling', "Modeling")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Models', "Model")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Metals', "Metal")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Diatoms', "Diatom")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Macrophytes', "Macrophyte")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Sediments', "Sediment")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Levels', "Level")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Gases', "Gas")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Hiv', "Waterborne Disease")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Co2', "CO2")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Ch4', "CH4")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'N2o', "N2O")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'CO2', "Carbon Dioxide")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'CH4', "Methane")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'N2O', "Nitrous Oxide")
        keyword2$keyword <- str_replace_all(keyword2$keyword, '16s Rrna Gene', "16s Rrna")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'E. Coli', "Escherichia Coli")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Viruses', "Virus")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Zoonoses', "Zoonosis")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Genes', "Gene")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Mrsa', "MRSA")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Esbl', "ESBL")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Taphonomy', "Water Quality")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Microcystins', "Microcystin")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Fishery', "Fisheries")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'algorithms', "algorithm")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Machines', "Machine")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'machines', "machine")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Algorithms', "algorithm")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'methods', "method")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Methods', "method")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'rules', "rule")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Rules', "rule")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Networks', "Network")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Systems', "System")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Pca', "Principal Component Analysis")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'PCA', "Principal Component Analysis")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Principal Components Analysis (Pca)', "Principal Component Analysis")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Principal Component', "Principal Component Analysis")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Principal Components', "Principal Component Analysis")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Principal Components Analysis (Pca)', "Principal Component Analysis")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Principal Component Analysis Method', "Principal Component Analysis")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Analyses', "Analysis")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Cca', "Canonical Correspondence Analysis")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'CCA', "Canonical Correspondence Analysis")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Redundancy Analysis (Rda)', "Redundancy Analysis")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Neural Network', "Artificial Neural Network")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Artificial Neural Network \\(Ann\\)', "Artificial Neural Network")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Artificial Neural Networks \\(Ann\\)', "Artificial Neural Network")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Artificial Neural Networks \\(Anns\\)', "Artificial Neural Network")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Ann', "Artificial Neural Network")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'ANN', "Artificial Neural Network")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'ANNs', "Artificial Neural Network")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Anns', "Artificial Neural Network")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'ANN', "Artificial Neural Network")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Artificial Artificial Neural Network', "Artificial Neural Network")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Gis', "Geographic Information System")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'GIS', "Geographic Information System")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Geographic Information System \\(Gis\\)', "Geographic Information System")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Gis \\(Geographic Information System\\)', "Geographic Information System")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Analysis Analysis', "Analysis")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Svm', "Support Vector Machine")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Support Vector Machine \\(Svm\\)', "Support Vector Machine")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Support Vector Machines \\(Svms\\)', "Support Vector Machine")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Support Vector Machines \\(Svm\\)', "Support Vector Machine")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Support Vector Machine\\(Svm\\)', "Support Vector Machine")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Generalized', "Generalised")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Artificial Neural Network \\(Artificial Neural Network\\)', "Artificial Neural Network")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Support Vector Machine \\(Support Vector Machine\\)', "Support Vector Machine")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Principal Component Analysiss Analysis', "Principal Component Analysis")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Principal Component Analysis \\(Principal Component Analysis\\)', "Principal Component Analysis")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Forest \\(Rf\\)', "Forest")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'forests', "Forest")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'forest', "Forest")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Forests', "Forest")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Ground water', "Groundwater")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Estuaries', "Estuary")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Macroinvertebrates', "Macroinvertebrate")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Gam', "Generalised Additive Model")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Generalised Additive Model \\(Gam\\)', "Generalised Additive Model")
        keyword2$keyword <- str_replace_all(keyword2$keyword, ' Generalised Additive Model \\(Gams\\)', "Generalised Additive Model")
        keyword3 <- keyword2 %>%
            dplyr::group_by(keyword) %>% 
            dplyr::summarise(n=n()) %>% 
            dplyr::arrange(desc(n)) 
        return(keyword3)
    }
}

keyword_ml <- KW_all(river_ml_data_final)


# print the graphs

plot_kw <- function(x, y){
    if (nrow(x) == 0){
        return(x)
    } else {
        ggplot(x, aes(label = keyword, size =n ,color = rainbow_hcl(20))) +
            geom_text_wordcloud_area(shape = "star") +
            scale_size_area(max_size = 15) +
            theme_minimal()+
            ggtitle(paste(y))+
            theme(plot.title = element_text(hjust = 0.5, size = 18))
    }
}

plot_kw(keyword_ml[1:20,], "River Machine Learning")

# General info ####
# document types
save_DT <- function(x){
    y <- summary(x$`Document Type`)
    y <- rownames_to_column(as.data.frame(y), var = "rowname")
    z <- as.data.frame(y$y*100/sum(y$y))
    colnames(z) <- "Percentage"
    y <- bind_cols(y,z)
    return(y)
}

write_csv(save_DT(river_ml_short), "river_ml_DT.csv")

# language of the document 
save_language <- function(x){
    y <- summary(as.factor(x$`Language of Original Document`))
    y <- rownames_to_column(as.data.frame(y), var = "rowname")
    z <- as.data.frame(y$y*100/sum(y$y))
    colnames(z) <- "Percentage"
    y <- bind_cols(y,z)
}
write_csv(save_language(river_ml_short), "river_ml_lang.csv")

# Open access
save_access <- function(x){
    y <- summary(as.factor(x$`Access Type`))
    y <- rownames_to_column(as.data.frame(y), var = "rowname")
    z <- as.data.frame(y$y*100/sum(y$y))
    colnames(z) <- "Percentage"
    y <- bind_cols(y,z)
}

write_csv(save_access(river_ml_short), "river_ml_Access.csv")

# Citation
save_citation <- function(x, y){
    x <- x %>% 
        arrange(desc(`Cited by`)) %>% 
        slice(1:y)
}

write_csv(save_citation(river_ml_data_final, 100), "river_ml_citation.csv")

# total_citation <- function(x, z){
#     y <- sum(x$`Cited by`, na.rm = TRUE)
#     t <- data.frame(z, y)
#     colnames(t) <- c("SDG", "Citation")
#     return(t)
# }

# Publication years
save_pubyear <- function(x){
    y <- x %>% select(Year,`Document Type`, `Access Type`) %>% 
        dplyr::group_by(Year) %>% 
        dplyr::summarise(n=n()) %>% 
        dplyr::arrange(Year) %>% 
        dplyr::filter(Year < 2021)
    # z <- as.data.frame(ave(y$n, FUN = cumsum))
    # colnames(z) <- "cum"
    # y <- bind_cols(y, z)
    t <- ggplot(y, aes(x=Year, y=n))+
        geom_point(size = 2) +
        # geom_smooth(colour="gray20", size =0.5, method = "lm") +
        labs(x = "Years", y = "Cumulative publications", fill = NULL, title = NULL) +
        # scale_x_continuous(breaks = c(2008:2020))+
        theme_bw()+
        theme(text = element_text(size = 16))
}

ggsave(filename = "river_ml_pubyear.jpeg", save_pubyear(river_ml_short),  units = 'cm', height = 20, width = 20, dpi = 300)

# Top Journal 
save_topjournal <- function(x){
    y <- x %>% select(`Source title`) %>% 
        dplyr::group_by(`Source title`) %>% 
        dplyr::summarise(n=n()) %>% 
        dplyr::arrange(desc(n)) %>% 
        slice(1:20) %>% 
        ggplot(aes(x=reorder(`Source title`, n),y = n)) +
        geom_bar(stat = "identity",
                 position = position_stack(reverse = TRUE), 
                 fill = "tomato") +
        coord_flip() +
        theme_bw() +
        xlab("Journals") +
        ylab("Number of publications") +
        theme(text=element_text(family = "Arial")) +
        theme(axis.text.x = element_text(size = 14)) +
        theme(axis.text.y = element_text(size = 14)) +
        theme(axis.title = element_text(size = 14)) +
        theme(axis.title.y = element_blank())
}

ggsave(filename = "river_ml_journal.jpeg", save_topjournal(river_ml_short),  units = 'cm', height = 20, width = 40, dpi = 300)

# Dendorgram for ML ####

ml_dendogram <- river_ml_short[, c(which(colnames(river_ml_short) == "id"), (which(colnames(river_ml_short) == "Period")+1):str_which(colnames(river_ml_short), "Microbial"))]
ml_dendogram[is.na(ml_dendogram)] <- 0
ml_den <- aggregate(data = ml_dendogram, . ~ id, FUN = sum)
ml_den$id[2] <- "Big data"
ml_den$id[5] <- "Deep learning"
ml_den$id[6:7] <- c("Discriminant Analysis", "Ensemble methods")
ml_den$id[c(9:11,13, 15, 17:21)] <- c("Gaussian processes", "Human interpretable information extraction", "Nearest neighbors", "Manifold learning",
                                      "Multiclass and multilabel algorithms", "Supervised neural networks", "Unsupervised neural networks",
                                      "Reinforcement learning", "Stochastic Gradient Descent", "Support Vector Machines")
rownames(ml_den) <- ml_den$id
ml_den <- ml_den %>% select(-id)

# dendrogram base plots
# dd <- dist(scale(ml_den), method = "manhattan")
# hc <- hclust(dd, method = "ward.D")
# plot(hc)
# library("ape")
# colors <- seq_gradient_pal("blue", "red", "Lab")(seq(0,1,length.out=5))
# clus4 = cutree(hc, 5)
# plot(as.phylo(hc), tip.color = colors[clus4],
#      label.offset = 1, cex = 0.7)

# ggplot dendrogram 


dend <- ml_den %>%
    dist(method = "manhattan") %>%
    hclust(method = "ward.D") %>%
    as.dendrogram() %>% 
    set("branches_k_color", value = colors, k = 5)
# Rectangular lines
ddata <- as.ggdend(dend)

ml_dendogram_graph <- ggplot(ddata,horiz = TRUE, theme = NULL) +
    theme_void() +
    xlab("Height") +
    ylim(40000, -10000) +
    theme(text=element_text(size=16),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank())
ml_dendogram_graph
ggsave("ml_dendrogram.jpeg", ml_dendogram_graph , units = 'cm', height = 20, width = 40, dpi = 300)


ml_dendro_editable <- dml(ggobj = ml_dendogram_graph)
ml_dendro_doc <- read_pptx()
ml_dendro_doc <- add_slide(ml_dendro_doc)
ml_dendro_doc <- ph_with(x = ml_dendro_doc, value = ml_dendro_editable, location = ph_location_type(type = "body"))
print(ml_dendro_doc, target = "ml_dendro.pptx")

# Mann-Kendall trend test in research topics ####
# 1. Your data isn’t collected seasonally (e.g. only during the summer and winter months), because the test won’t work if alternating upward and downward trends exist in the data. Another test—the Seasonal Kendall Test—is generally used for seasonally collected data.
# 2. Your data does not have any covariates—other factors that could influence your data other than the ones you’re plotting. See Covariates for more information.
# 3. You have only one data point per time period. If you have multiple points, use the median value.
# yearly trend

river_topics <- split(river_trend_year, f = river_trend_year$`Research Topics`)
river_topics_MK <- lapply(river_topics, function(x){y <- MannKendall(x$Rank); return(y)})


# yearly trend but after 1980s

river_topics_aft_80s <- split(river_trend_year %>% filter(Year > 1979), f = river_trend_year$`Research Topics`)
river_topics_80s_MK <- lapply(river_topics_aft_80s, function(x){y <- MannKendall(x$Rank); return(y)})


# yearly trend but after 1990s

river_topics_aft_90s <- split(river_trend_year %>% filter(Year > 1989), f = river_trend_year$`Research Topics`)
river_topics_90s_MK <- lapply(river_topics_aft_90s, function(x){y <- MannKendall(x$Rank); return(y)})



# almost every research topics have either increasing or decreasing trend because of their initial rank which was almost the same in all topics

river_topics_period <- split(river_research_rank %>% filter(Period != "< 1980s"), f = river_research_rank$`Research Topics`)

river_topics_period_MK <- lapply(river_topics_period, function(x){y <- MannKendall(x$Rank); return(y)})


# almost every research topics have either increasing or decreasing trend because of their initial rank which was almost the same in all topics

river_topics_period_no2020 <- split(river_research_rank %>% filter(Period != "2020"), f = river_research_rank$`Research Topics`)

river_topics_period_no2020_MK <- lapply(river_topics_period_no2020, function(x){y <- MannKendall(x$Rank); return(y)})

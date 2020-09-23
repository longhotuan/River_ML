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
#** country correction ####
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

#** lat_long ####
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
# add the names of the country as columns ####

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
    "Reinforcement_learning", "Reinforcement learning",
    "SGD", "Supervised Learning",
    "SVM", "Supervised Learning"
)

river_ml_data <- left_join(river_ml_data,  ml_type, by = ".id")
river_ml_data<- river_ml_data %>% select(ML, everything())

river_ml_data_final <- river_ml_data %>% select(-Affi, -Country)
write_csv(river_ml_data_final, "river_ml_data.csv")
write_feather(river_ml_data_final, "river_ml_data.feather")


# Split into different periods ####
# before 1980s, 1980s, 1990s, 2000s, 2010s ####



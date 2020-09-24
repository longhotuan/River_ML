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
library(pastecs)

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
                "Management", "Aquatic environment", "Biogeochemistry", "Public health", "Movement", "Spatiotempral trends", "Microbial")
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
river_ml_data$`Spatiotempral trends`[str_detect(str_to_lower(river_ml_data$`Author Keywords`), pattern = "temporal*|season*|spati*")] <- 1
river_ml_data$Microbial[str_detect(str_to_lower(river_ml_data$`Author Keywords`), pattern = "microbi|16s rdna|fluorescence")] <- 1


river_ml_data_final <- river_ml_data %>% select(-Affi, -Country)
river_ml_short <- river_ml_data[,c(1:str_which(colnames(river_ml_data), "Country"), str_which(colnames(river_ml_data), "Period"):ncol(river_ml_data))]

write_csv(river_ml_data_final, "river_ml_data.csv")
write_feather(river_ml_data_final, "river_ml_data.feather")
rm(ml_type, river_ml_data_country, river_ml_data_lat, river_ml_data_long, list_river_mlvised)

#  ML types in different periods ####

river_mlt_period <- river_ml_short %>% filter(Period != "2020") %>% group_by(Period, id, ML) %>% summarise(n =n())
river_mlt_period$Period <- as.character(river_mlt_period$Period)
# river_mlt_period <- aggregate(data = river_mlt_period, Period ~ id + ML, FUN = "summarise")
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

# Different research topics #### 

river_research <- river_ml_short[,c(1,2,str_which(colnames(river_ml_short), "Period"):ncol(river_ml_short))]
river_research[is.na(river_research)] <- 0
river_research$ML <- as.factor(river_research$ML)
river_research$ML <- factor(river_research$ML, levels = c("Supervised Learning", "Unsupervised Learning",
                                                              "Deep Learning", "Reinforcement learning",
                                                              "Human interpretable information extraction", "Big Data"),
                              labels = c("Supervised Learning", "Unsupervised Learning",
                                         "Deep Learning", "Reinforcement Learning",
                                         "Human interpretable info extraction", "Big Data"))
river_research$id <- as.factor(river_research$id)

research_total <- as_tibble(lapply(river_research, function(x){sum(!is.na(x))}))

# Over period and id
river_research_ML_id <- aggregate(data = river_research, .~Period+id+ML, sum) 
river_research_ML_id <- river_research_ML_id %>% pivot_longer(cols = -c(Period, id, ML), names_to = "Research Topics", values_to = "Number of publications")

ggsave("research_id_period.jpeg", ggplot(river_research_ML_id %>% filter(Period != 2020), aes(x = Period, y= `Number of publications`, group = `Research Topics`)) +
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
river_research_ML <- river_research_ML %>% pivot_longer(cols = -c(Period, ML), names_to = "Research Topics", values_to = "Number of publications")

ggsave("research_ML_period.jpeg", ggplot(river_research_ML %>% filter(Period != 2020), aes(x = Period, y= `Number of publications`, group = `Research Topics`)) +
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
           facet_wrap(.~`Research Topics`, scales = "free_y") +
           # scale_color_brewer(palette = "Dark2") +
           theme(text=element_text(size=16),
                 strip.text.x = element_text(size=14),
                 axis.text = element_text(size=12),
                 axis.title = element_text(size=14),
                 legend.position = "bottom",
                 legend.title = element_blank(),
                 legend.text = element_text(size = 12))
       ,  units = 'cm', height = 20, width = 30, dpi = 300)


# split the keyword_ml at the begining into different periods and apply the function in each period (forget what is this for?)
river_period <- split(river_ml_data, f = river_ml_data$Period)

keyword_period <- map(river_period, KW_all)

sum(keyword_ml$n[str_detect(tolower(keyword_ml$keyword), 
                            pattern = "climate change*|global warming|
                                          climate warming*|kyoto protocol|paris agreement|palaeoclimat*|climate polic*")])

# Temporal trends of research rank ####

river_research_rank <- river_research_period %>% arrange(Period, -`Number of publications`) %>% group_by(Period) %>% mutate(Rank = rank(desc(`Number of publications`), ties.method = "min"))

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
river_research_rank$Trends <- as.factor(river_research_rank$Trends)
plot_rank <- ggplot(river_research_rank, aes(x = Period, y= Rank, group = `Research Topics`)) +
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
          legend.position = "none",
          legend.title = element_blank(),
          legend.text = element_text(size = 12))
ggsave("research_rank_grouped.jpeg", plot_rank, units = 'cm', height = 20, width = 35, dpi = 300)


plot_rank_group <- ggplot(river_research_rank, aes(x = Period, y= Rank, group = `Research Topics`)) +
    geom_point(aes(color = `Research Topics`, size = 1.01)) +
    geom_line(aes(color = `Research Topics`, size = 1.005)) +
    theme_bw() +
    ylab("Total number of publications") +
    facet_wrap(.~Trends, scales = "free_y") +
    scale_y_reverse() +
    # scale_color_brewer(palette = "Dark2") +
    theme(text=element_text(size=16),
          strip.text.x = element_text(size=14),
          axis.text = element_text(size=12),
          axis.title = element_text(size=14),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(size = 12))

ggsave("research_rank_grouped_color.jpeg", plot_rank_group
       ,  units = 'cm', height = 20, width = 35, dpi = 300)

plot_rank_group + geom_text(data = river_research_rank[river_research_rank$Period == "2020", ], aes(label = `Research Topics`), hjust = 0.7,vjust = 2)
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

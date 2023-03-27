# Psychological Subdisciplines and Study Method #

# Information is derived from the fields "Methodology" and "APA PsycInfo Classification Code"
# in PsycInfo / PSYNDEX.





# APA classification ----

# https://www.apa.org/pubs/databases/training/class-codes
# main category is in brackets
# e.g., "[3550]" -> "35* Educational Psychology"


# extract classification codes
# https://stackoverflow.com/a/57585772/11752986


# `data` is an export from OVID (a search using the DOIs in the final dataset)

codes <- vector(length = nrow(data))
for (i in 1:nrow(data)){
  temp <- stringr::str_match_all(data$classification_apa[i], "(?<!\\d)\\d{4}(?!\\d)")[[1]][, 1]
  temp <- substr(temp, 1, 4)
  codes[i] <- paste(temp, collapse = ";")
}
codes <- gsub("NA", NA, codes)


# assign subdiscipline to classification codes
# Codes retrieved from https://www.apa.org/pubs/databases/training/class-codes
# Subdisciplines assigned manually
# Assignment of Clinical Psychology according to 
# Richter et al. (2021), https://doi.org/10.1026/1616-3443/a000582
APA_Codes <- read.csv("./additional tables/APA_Codes.csv", header = TRUE)

code_replace <- function(x){
  
  if (!is.na(x)){
    x <- as.numeric(unlist(strsplit(x, ";")))
    x <- sapply(x, grep, APA_Codes$Code)
    x <- APA_Codes$Subdiscipline[x]
    x <- gsub(" ", "_", x)
    x <- unique(x)
    x <- paste(x, collapse = " ")
    return(x)
  } else {return(NA)}
  
}

subfields <- vector(length = length(codes))
for (i in 1:length(codes)){
  subfields[i] <- code_replace(codes[i])
}

head(codes)
head(subfields)




# Methodology ----

psyndex <- data$Database == "PSYNDEXplus Literature and Audiovisual Media"
psycinfo <- data$Database == "APA PsycInfo"

table(is.na(data$Methodology))

# preprocess database fields for tokenization
prep_field <- function(x){
  x <- gsub("&", "and", x)
  x <- gsub("; ", ";", x)
  x <- gsub(",", "", x)
  x <- gsub(" ", "_", x)
  x <- gsub(";", " ", x)
  return(x)
}

# preprocess methodology fields
methods <- tolower(data$Methodology)
methods <- gsub(" \\[\\s*(.*?)\\s*\\]", "", methods) # remove brackets
methods <- prep_field(methods)
methods <- gsub("\\/", "_", methods)
methods <- gsub("meta_analysis", "meta-analysis", methods)

# allign names from different databases
methods <- gsub("longitudinal_empirical", "longitudinal", methods)
methods <- gsub("qualitative_empirical", "qualitative", methods)

# In PsycInfo, "Empirical Study" is redundant
# e.g., "Empirical Study; Quantitative Study"
methods[psycinfo] <- gsub("empirical_study", "", methods[psycinfo])

# use categories as specific as possible:
# E.g., if interview, than the broader "qualitative study" is not needed.
methods <- ifelse(grepl("interview", methods) & grepl("qualitative_study", methods),
                  gsub("qualitative_study", "", methods), methods)
methods <- ifelse(grepl("focus_group", methods) & grepl("qualitative_study", methods),
                  gsub("qualitative_study", "", methods), methods)
methods <- ifelse(grepl("longitudinal_study", methods) & grepl("quantitative_study", methods),
                  gsub("quantitative_study", "", methods), methods)
methods <- ifelse(grepl("experimental_study", methods) & grepl("quantitative_study", methods),
                  gsub("quantitative_study", "", methods), methods)

# In PsycInfo, "Empirical Study" is "Quantitative Study"
methods[psyndex] <- gsub("empirical_study", "quantitative_study", methods[psyndex])





# Create data frame with additional metadata ----

subfields_methodology <- data.frame("DOI" = data$DOI,
                                "Subfield" = subfields,
                                "Methodology" = methods)

saveRDS(subfields_methodology, file = "./ESM/dataset/subfields_methodology.RDS")
write.csv(subfields_methodology, file = "./ESM/dataset/subfields_methodology.csv", row.names = FALSE)
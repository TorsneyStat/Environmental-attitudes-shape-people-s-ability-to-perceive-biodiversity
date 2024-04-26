
##Directory entered 
setwd("C:/Users/AndrewT/OneDrive - CAAS (Environmental Services) Ltd/Desktop/AndyPhD/Perception Study/Analysis")

##install.packages("qmethod")
library(qmethod)
library(dplyr)
library(ggplot2)


data <- read.csv("PerceptionData_Anon.csv", header=TRUE)
data <- data %>%  mutate(Participant.Name = paste0("ID_", row_number()))

##Get column names
#dput(names(data))


#write.csv(AllDataResults, "C:/Users/AndrewT/OneDrive - CAAS (Environmental Services) Ltd/Desktop/AndyPhD/Perception Study/Analysis/AllDataResults.csv", row.names = FALSE)



##############################################
##Assigning Value to photos according to rank - using the preference data.
## Loop through each number from 1 to 16
for (num in 1:16) {
  # Create a new variable 'PhotoXPref' in the 'data' data frame for the current number
  data[[paste0('PhotoBRank', num, 'Pref')]] <- NA
  ##top rank had to be the highest number
  # Loop through each row of the 'data' data frame
  for (i in 1:nrow(data)) {
    # Loop through each column of the 'data' data frame
    for (j in 1:16) {
      # Check if the current number is present in the current column 'PrefPosX'
      if (data[i, paste0('PrefPos', j)] == num) {
        # Assign the value based on the position
        if (j == 1) {
          data[[paste0('PhotoBRank', num, 'Pref')]][i] <- 1
        } else if (j %in% c(2, 3)) {
          data[[paste0('PhotoBRank', num, 'Pref')]][i] <- 2
        } else if (j %in% c(4, 5, 6)) {
          data[[paste0('PhotoBRank', num, 'Pref')]][i] <- 3
        } else if (j %in% c(7, 8, 9, 10)) {
          data[[paste0('PhotoBRank', num, 'Pref')]][i] <- 4
        } else if (j %in% c(11, 12, 13)) {
          data[[paste0('PhotoBRank', num, 'Pref')]][i] <- 5
        } else if (j %in% c(14, 15)) {
          data[[paste0('PhotoBRank', num, 'Pref')]][i] <- 6
        } else if (j == 16) {
          data[[paste0('PhotoBRank', num, 'Pref')]][i] <- 7
        }
        # Once the number is found, break the loop to avoid further checks for this row
        break
      }
    }
  }
}

##Assigning Value to photos according to rank - using the diversity data.
## Loop through each number from 1 to 16
for (num in 1:16) {
  ## Create a new variable 'PhotoXPref' in the 'data' data frame for the current number
  data[[paste0('PhotoBRank', num, 'Divers')]] <- NA
  ##top rank had to be the highest number
  ## Loop through each row of the 'data' data frame
  for (i in 1:nrow(data)) {
    ## Loop through each column of the 'data' data frame
    for (j in 1:16) {
      ## Check if the current number is present in the current column 'DiversPosX'
      if (data[i, paste0('DiversPos', j)] == num) {
        ## Assign the value based on the position
        if (j == 1) {
          data[[paste0('PhotoBRank', num, 'Divers')]][i] <- 1
        } else if (j %in% c(2, 3)) {
          data[[paste0('PhotoBRank', num, 'Divers')]][i] <- 2
        } else if (j %in% c(4, 5, 6)) {
          data[[paste0('PhotoBRank', num, 'Divers')]][i] <- 3
        } else if (j %in% c(7, 8, 9, 10)) {
          data[[paste0('PhotoBRank', num, 'Divers')]][i] <- 4
        } else if (j %in% c(11, 12, 13)) {
          data[[paste0('PhotoBRank', num, 'Divers')]][i] <- 5
        } else if (j %in% c(14, 15)) {
          data[[paste0('PhotoBRank', num, 'Divers')]][i] <- 6
        } else if (j == 16) {
          data[[paste0('PhotoBRank', num, 'Divers')]][i] <- 7
        }
        ## Once the number is found, break the loop to avoid further checks for this row
        break
      }
    }
  }
}

## Check the structure of the data - with no truncation in the output
#str(data, list.len = ncol(data))



###############################
## List of column names you want to select for PhotoBRank preferences and the diversity
PhotoBRankPreferenceMatrixList <- c("Participant.Name", "PhotoBRank1Pref", "PhotoBRank2Pref", "PhotoBRank3Pref",
                                    "PhotoBRank4Pref", "PhotoBRank5Pref", "PhotoBRank6Pref", "PhotoBRank7Pref",
                                    "PhotoBRank8Pref", "PhotoBRank9Pref",  "PhotoBRank10Pref", "PhotoBRank11Pref",
                                    "PhotoBRank12Pref", "PhotoBRank13Pref", "PhotoBRank14Pref", "PhotoBRank15Pref",
                                    "PhotoBRank16Pref")

PhotoBRankDiverserenceMatrixList <- c("Participant.Name", "PhotoBRank1Divers", "PhotoBRank2Divers", "PhotoBRank3Divers",
                                      "PhotoBRank4Divers", "PhotoBRank5Divers", "PhotoBRank6Divers", "PhotoBRank7Divers",
                                      "PhotoBRank8Divers", "PhotoBRank9Divers", "PhotoBRank10Divers", "PhotoBRank11Divers",
                                      "PhotoBRank12Divers", "PhotoBRank13Divers", "PhotoBRank14Divers", "PhotoBRank15Divers",
                                      "PhotoBRank16Divers")

BRankAllTogetherList <- c("Participant.Name", "PhotoBRank1Pref", "PhotoBRank2Pref", "PhotoBRank3Pref", "PhotoBRank4Pref", 
                          "PhotoBRank5Pref", "PhotoBRank6Pref", "PhotoBRank7Pref", "PhotoBRank8Pref", "PhotoBRank9Pref", 
                          "PhotoBRank10Pref", "PhotoBRank11Pref", "PhotoBRank12Pref", "PhotoBRank13Pref", "PhotoBRank14Pref", 
                          "PhotoBRank15Pref", "PhotoBRank16Pref", "PhotoBRank1Divers", "PhotoBRank2Divers", "PhotoBRank3Divers",
                          "PhotoBRank4Divers",  "PhotoBRank5Divers", "PhotoBRank6Divers", "PhotoBRank7Divers", "PhotoBRank8Divers",
                          "PhotoBRank9Divers",  "PhotoBRank10Divers", "PhotoBRank11Divers", "PhotoBRank12Divers", "PhotoBRank13Divers",
                          "PhotoBRank14Divers",  "PhotoBRank15Divers", "PhotoBRank16Divers")


## Create a new dataframe with selected columns for pref and then diversity
BRankdataforQPref <- data[, PhotoBRankPreferenceMatrixList]
BRankdataforQDivers <- data[, PhotoBRankDiverserenceMatrixList]
BRankAllTogether <- data[, BRankAllTogetherList]

##Transpose data so that it can be used in Qmethod.
BRankdataforQPref_T <- setNames(data.frame(t(BRankdataforQPref[ , - 1])), BRankdataforQPref[ , 1])
BRankdataforQDivers_T <- setNames(data.frame(t(BRankdataforQDivers[ , - 1])), BRankdataforQDivers[ , 1])          
BRankAllTogether_T <- setNames(data.frame(t(BRankAllTogether[ , - 1])), BRankAllTogether[ , 1])  

#######################################

#########################################
###########################################
## Making data numeric if needed
#dataforQPref <- dataforQPref %>% mutate_at(c("Photo1Pref", "Photo2Pref", "Photo3Pref", "Photo4Pref", "Photo5Pref", "Photo6Pref", "Photo7Pref", "Photo8Pref", "Photo9Pref", "Photo10Pref", "Photo11Pref", "Photo12Pref", "Photo13Pref", "Photo14Pref", "Photo15Pref", "Photo16Pref"), as.numeric)

BRankdataforQDivers_T$ActualDiversity <- c(4, 1, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5, 6, 6, 7)
BRankdataforQPref_T$ActualDiversity <- c(4, 1, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5, 6, 6, 7)



# Calculate correlations
BRankPrefcorrelations <- as.data.frame(cor(BRankdataforQPref_T, BRankdataforQPref_T$ActualDiversity, method = "spearman"))
BRankDiverscorrelations <- as.data.frame(cor(BRankdataforQDivers_T, BRankdataforQDivers_T$ActualDiversity, method = "spearman"))



BRankmerged_correlations <- cbind(Divers = BRankDiverscorrelations, Pref = BRankPrefcorrelations)
colnames(BRankmerged_correlations) <- c("BRankDivers_Cor", "BRankPref_Cor")
BRankmerged_correlations <- BRankmerged_correlations[1:(nrow(BRankmerged_correlations) - 1), ]


##############################################
##Assigning Value to photos according to rank - using the preference data.
## Loop through each number from 1 to 16
for (num in 1:16) {
  # Create a new variable 'PhotoXPref' in the 'data' data frame for the current number
  data[[paste0('Photo', num, 'Pref')]] <- NA
  
  # Loop through each row of the 'data' data frame
  for (i in 1:nrow(data)) {
    # Loop through each column of the 'data' data frame
    for (j in 1:16) {
      # Check if the current number is present in the current column 'PrefPosX'
      if (data[i, paste0('PrefPos', j)] == num) {
        # Assign the value based on the position
        if (j == 1) {
          data[[paste0('Photo', num, 'Pref')]][i] <- 3
        } else if (j %in% c(2, 3)) {
          data[[paste0('Photo', num, 'Pref')]][i] <- 2
        } else if (j %in% c(4, 5, 6)) {
          data[[paste0('Photo', num, 'Pref')]][i] <- 1
        } else if (j %in% c(7, 8, 9, 10)) {
          data[[paste0('Photo', num, 'Pref')]][i] <- 0
        } else if (j %in% c(11, 12, 13)) {
          data[[paste0('Photo', num, 'Pref')]][i] <- -1
        } else if (j %in% c(14, 15)) {
          data[[paste0('Photo', num, 'Pref')]][i] <- -2
        } else if (j == 16) {
          data[[paste0('Photo', num, 'Pref')]][i] <- -3
        }
        # Once the number is found, break the loop to avoid further checks for this row
        break
      }
    }
  }
}

##Assigning Value to photos according to rank - using the diversity data.
## Loop through each number from 1 to 16
for (num in 1:16) {
  ## Create a new variable 'PhotoXPref' in the 'data' data frame for the current number
  data[[paste0('Photo', num, 'Divers')]] <- NA
  
  ## Loop through each row of the 'data' data frame
  for (i in 1:nrow(data)) {
    ## Loop through each column of the 'data' data frame
    for (j in 1:16) {
      ## Check if the current number is present in the current column 'DiversPosX'
      if (data[i, paste0('DiversPos', j)] == num) {
        ## Assign the value based on the position
        if (j == 1) {
          data[[paste0('Photo', num, 'Divers')]][i] <- 3
        } else if (j %in% c(2, 3)) {
          data[[paste0('Photo', num, 'Divers')]][i] <- 2
        } else if (j %in% c(4, 5, 6)) {
          data[[paste0('Photo', num, 'Divers')]][i] <- 1
        } else if (j %in% c(7, 8, 9, 10)) {
          data[[paste0('Photo', num, 'Divers')]][i] <- 0
        } else if (j %in% c(11, 12, 13)) {
          data[[paste0('Photo', num, 'Divers')]][i] <- -1
        } else if (j %in% c(14, 15)) {
          data[[paste0('Photo', num, 'Divers')]][i] <- -2
        } else if (j == 16) {
          data[[paste0('Photo', num, 'Divers')]][i] <- -3
        }
        ## Once the number is found, break the loop to avoid further checks for this row
        break
      }
    }
  }
}

## Check the structure of the data - with no truncation in the output
#str(data, list.len = ncol(data))





###############################
## List of column names you want to select for photo preferences and the diversity
PhotoPreferenceMatrixList <- c("Participant.Name", "Photo1Pref", "Photo2Pref", "Photo3Pref", "Photo4Pref", 
                               "Photo5Pref", "Photo6Pref", "Photo7Pref", "Photo8Pref", "Photo9Pref", 
                               "Photo10Pref", "Photo11Pref", "Photo12Pref", "Photo13Pref", "Photo14Pref", 
                               "Photo15Pref", "Photo16Pref")

PhotoDiverserenceMatrixList <- c("Participant.Name", "Photo1Divers", "Photo2Divers", "Photo3Divers", "Photo4Divers", 
                                 "Photo5Divers", "Photo6Divers", "Photo7Divers", "Photo8Divers", "Photo9Divers", 
                                 "Photo10Divers", "Photo11Divers", "Photo12Divers", "Photo13Divers", "Photo14Divers", 
                                 "Photo15Divers", "Photo16Divers")
AllTogetherList <- c("Participant.Name", "Photo1Pref", "Photo2Pref", "Photo3Pref", "Photo4Pref", 
                     "Photo5Pref", "Photo6Pref", "Photo7Pref", "Photo8Pref", "Photo9Pref", 
                     "Photo10Pref", "Photo11Pref", "Photo12Pref", "Photo13Pref", "Photo14Pref", 
                     "Photo15Pref", "Photo16Pref", "Photo1Divers", "Photo2Divers", "Photo3Divers", "Photo4Divers", 
                     "Photo5Divers", "Photo6Divers", "Photo7Divers", "Photo8Divers", "Photo9Divers", 
                     "Photo10Divers", "Photo11Divers", "Photo12Divers", "Photo13Divers", "Photo14Divers", 
                     "Photo15Divers", "Photo16Divers")


## Create a new dataframe with selected columns for pref and then diversity
dataforQPref <- data[, PhotoPreferenceMatrixList]
dataforQDivers <- data[, PhotoDiverserenceMatrixList]
AllTogether <- data[, AllTogetherList]

##Transpose data so that it can be used in Qmethod.
dataforQPref_T <- setNames(data.frame(t(dataforQPref[ , - 1])), dataforQPref[ , 1])
dataforQDivers_T <- setNames(data.frame(t(dataforQDivers[ , - 1])), dataforQDivers[ , 1])          
AllTogether_T <- setNames(data.frame(t(AllTogether[ , - 1])), AllTogether[ , 1])  

#######################################


#########################################
###########################################
## Making data numeric if needed
#dataforQPref <- dataforQPref %>% mutate_at(c("Photo1Pref", "Photo2Pref", "Photo3Pref", "Photo4Pref", "Photo5Pref", "Photo6Pref", "Photo7Pref", "Photo8Pref", "Photo9Pref", "Photo10Pref", "Photo11Pref", "Photo12Pref", "Photo13Pref", "Photo14Pref", "Photo15Pref", "Photo16Pref"), as.numeric)


dataforQDivers_T$ActualDiversity <- c(0, 3, 2, 2, 1, 1, 1, 0, 0, 0, -1, -1, -1, -2, -2, -3)
dataforQPref_T$ActualDiversity <- c(0, 3, 2, 2, 1, 1, 1, 0, 0, 0, -1, -1, -1, -2, -2, -3)

# Calculate correlations
Prefcorrelations <- as.data.frame(cor(dataforQPref_T, dataforQPref_T$ActualDiversity, method = "spearman"))
Diverscorrelations <- as.data.frame(cor(dataforQDivers_T, dataforQDivers_T$ActualDiversity, method = "spearman"))


merged_correlations <- cbind(Divers = Diverscorrelations, Pref = Prefcorrelations)
colnames(merged_correlations) <- c("Divers_Cor", "Pref_Cor")
merged_correlations <- merged_correlations[1:(nrow(merged_correlations) - 1), ]








# 
# DataForCorrelation <- merge(dataforQPref, data[c("Participant.Name", "Group.1")], 
#                      by.x = "Participant.Name", by.y = "Participant.Name", all.x = TRUE)
# 
# ActualDiversity <- c(0, 3, 2, 2, 1, 1, 1, 0, 0, 0, -1, -1, -1, -2, -2, -3)
# 
# 
# actual_diversity_row <- data.frame(Participant.Name = "ActualDiversity", Photo1Pref = "0", Photo2Pref = "3", Photo3Pref = "2", Photo4Pref = "2", Photo5Pref = "1", Photo6Pref = "1", Photo7Pref = "1", Photo8Pref = "0", Photo9Pref = "0", Photo10Pref = "0", Photo11Pref = "-1", Photo12Pref = "-1", Photo13Pref = "-1", Photo14Pref = "-2", Photo15Pref = "-2", Photo16Pref = "-3", Group.1 = "On Its OWN")
# actual_diversity_row[, grep("^Photo\\d+Pref$", colnames(actual_diversity_row))] <- 
#   lapply(actual_diversity_row[, grep("^Photo\\d+Pref$", colnames(actual_diversity_row))], as.numeric)
# 
# 
# 
# DataForCorrelation <- rbind(DataForCorrelation, actual_diversity_row)
# 
# 
# # Select the columns of interest
# cols_of_interest <- c("Photo1Pref", "Photo2Pref", "Photo3Pref", "Photo4Pref", "Photo5Pref", "Photo6Pref", "Photo7Pref", "Photo8Pref", "Photo9Pref", "Photo10Pref", "Photo11Pref", "Photo12Pref", "Photo13Pref", "Photo14Pref", "Photo15Pref", "Photo16Pref")
# 
# # Filter the rows corresponding to "ActualDiversity" and remove them temporarily
# actual_diversity <- DataForCorrelation %>%
#   filter(Participant.Name == "ActualDiversity") %>%
#   select(all_of(cols_of_interest)) %>%
#   as.matrix() %>%
#   t()  # Transpose to have variables as rows
# 
# # Remove "ActualDiversity" rows from the dataset
# DataForCorrelation_filtered <- DataForCorrelation %>%
#   filter(Participant.Name != "ActualDiversity")
# 
# # Compute Spearman correlation for each row relative to "ActualDiversity"
# correlations <- DataForCorrelation %>%
#   filter(Participant.Name != "ActualDiversity") %>%
#   group_by(Group.1) %>%
#   summarize(Spearman_Correlation = cor(c_across(cols_of_interest), unlist(actual_diversity), method = "spearman"))
# 
# # Join the computed correlations back to the original dataset
# DataForCorrelation_filtered <- left_join(DataForCorrelation, correlations, by = "Group.1")
# 
# 
# 
# 
# # Add the correlation values to the DataForCorrelation dataset
# DataForCorrelation_filtered$Spearman_Correlation <- correlations
# 
# 
# print(DataForCorrelation_filtered$Spearman_Correlation)
# print(AllDataResults$Pref_Cor)






#####################################################
#####################################################
##Scoring the NEP Data
#####################################################
#####################################################

dput(names(data))
NEPList <- c("Participant.Name", "NEP1", "NEP2", "NEP3", "NEP4", "NEP5", "NEP6", "NEP7", "NEP8", 
             "NEP9", "NEP10", "NEP11", "NEP12", "NEP13", "NEP14", "NEP15")
NEPtoSum <- c("NEP1", "NEP2", "NEP3", "NEP4", "NEP5", "NEP6", "NEP7", "NEP8", 
              "NEP9", "NEP10", "NEP11", "NEP12", "NEP13", "NEP14", "NEP15")

NEP_Data <- data.frame(data[,NEPList])

new_column_name <- "NEP Score"

#scores were entered in backwards
NEP_Data$"NEP1" <- 7 - NEP_Data$"NEP1"
NEP_Data$"NEP2" <- 7 - NEP_Data$"NEP2"
NEP_Data$"NEP3" <- 7 - NEP_Data$"NEP3"
NEP_Data$"NEP5" <- 7 - NEP_Data$"NEP5"
NEP_Data$"NEP6" <- 7 - NEP_Data$"NEP6"
NEP_Data$"NEP7" <- 7 - NEP_Data$"NEP7"
NEP_Data$"NEP9" <- 7 - NEP_Data$"NEP9"
NEP_Data$"NEP11" <- 7 - NEP_Data$"NEP11"
NEP_Data$"NEP13" <- 7 - NEP_Data$"NEP13"
NEP_Data$"NEP15" <- 7 - NEP_Data$"NEP15"

# Add a new column with the sums of the specified columns
NEP_Data <- NEP_Data %>%
  mutate(!!new_column_name := rowSums(select(., all_of(NEPtoSum)), na.rm = TRUE))
##reverse scored questioned are these
NEPreverseScores <- c("NEP4", "NEP8", "NEP10", "NEP12", "NEP14")

NEP_Data <- NEP_Data %>%
  mutate(NEP_Category = case_when(
    `NEP Score` >= 75 ~ "Pro-Ecology",
    `NEP Score` >= 46 & `NEP Score` <= 74 ~ "Mid-Ecology",
    TRUE ~ "Anti-Ecology"
  ))
#names(NEP_Data)[names(NEP_Data) == "Participant.Name"] <- "Participant Name"





######################################################################
######################################################################
#####EcoConcern Scoring
#####Environmental Attitudes Confirmatory Factor Analysis (CFA)
######################################################################
######################################################################

#install.packages("lavaan")
#install.packages("foreign")
library(lavaan)
library(foreign)
AttitudeList <- c("Participant.Name", "Plants", "Marine.Life", 
                  "Birds", "Animals", "Me", "My.Lifestyle", "My.Health", "My.Future", 
                  "People.in.my.Country", "All.People", "Children", "Future.Generations")

Attitudes <- data.frame(data[,AttitudeList])
names(Attitudes)[names(Attitudes) == "Participant.Name"] <- "Participant Name"

modelspecs <- "Egocentric=~Me+My.Lifestyle+My.Health+My.Future
Altruistic=~People.in.my.Country+All.People+Children+Future.Generations
Biospheric=~Plants+Marine.Life+Birds+Animals"

cfa_results <- cfa(modelspecs, data=Attitudes)
summary(cfa_results, fit.measures=TRUE, standardized=TRUE)


##install.packages("psych")
library(psych)

Attitudes_T <- setNames(data.frame(t(Attitudes[ , - 1])), Attitudes[ , 1])
Attitudes_T[Attitudes_T == "N/A"] <- 0
Attitudes_T  <- Attitudes_T  %>% mutate_all(as.numeric)
efa_result <- fa(Attitudes_T, nfactors = 3, rotate = "varimax")
#str(Attitudes_T)
print(efa_result)



dput(names(data))
data[data == "N/A"] <- 0


# Sum the columns and create a new Bio_Score column
biolist <- c("Plants", "Marine.Life", "Birds", "Animals")
data <- data %>%
  mutate_at(vars(all_of(biolist)), as.numeric) %>%
  mutate(Bio_Score = rowSums(select(., all_of(biolist))))

egolist <- c("Me", "My.Lifestyle", "My.Health", "My.Future")
data <- data %>%
  mutate_at(vars(all_of(egolist)), as.numeric) %>%
  mutate(Ego_Score = rowSums(select(., all_of(egolist))))

Altrulist <- c("People.in.my.Country", "All.People", "Children", "Future.Generations")
data <- data %>%
  mutate_at(vars(all_of(Altrulist)), as.numeric) %>%
  mutate(Altru_Score = rowSums(select(., all_of(Altrulist))))

data <- data %>%
  mutate(Max_Score_Column = case_when(
    Bio_Score >= Ego_Score & Bio_Score >= Altru_Score ~ "Bio_Score",
    Ego_Score >= Bio_Score & Ego_Score >= Altru_Score ~ "Ego_Score",
    TRUE ~ "Altru_Score"
  ))

data <- data %>%
  mutate(Max_Score_Column = case_when(
    Bio_Score >= Ego_Score & Bio_Score >= Altru_Score ~ "Biocentric",
    Ego_Score >= Bio_Score & Ego_Score >= Altru_Score ~ "Egocentric",
    TRUE ~ "Altruistic"
  ))


merged_correlations$Participant.Name <- rownames(merged_correlations)

# Reset row names to NULL to remove them
rownames(merged_correlations) <- NULL
merged_correlations <- merge(merged_correlations, NEP_Data, by = "Participant.Name", all.x = TRUE)
merged_correlations <- merge(merged_correlations, data[, c("Participant.Name", "Max_Score_Column")], by = "Participant.Name", all.x = TRUE)
merged_correlations <- merge(merged_correlations, data[, c("Participant.Name", "Group.1")], by = "Participant.Name", all.x = TRUE)
merged_correlations <- merged_correlations[, !colnames(merged_correlations) %in% c("NEP1", "NEP2", "NEP3", "NEP4", "NEP5", "NEP6", "NEP7", "NEP8", "NEP9", "NEP10", "NEP11", "NEP12", "NEP13", "NEP14", "NEP15")]

merged_correlations$EcoConcern <- merged_correlations$Max_Score_Column
merged_correlations$Max_Score_Column <- NULL  # Remove the old column

merged_correlations$NEPcatagory <- merged_correlations$NEP_Category
merged_correlations$NEP_Category <- NULL  # Remove the old column

merged_correlations$NEPScore<- merged_correlations$`NEP Score`
merged_correlations$`NEP Score`<- NULL  # Remove the old column


merged_correlations$Group.2 <- gsub("Solo[0-9]+", "Solo", merged_correlations$Group.1)
merged_correlations$Group.2 <- gsub("^((?!Solo).)*$", "Group", merged_correlations$Group.2, perl = TRUE)

merged_correlations$Group.3 <- gsub("Solo[0-9]+", "Solo", merged_correlations$Group.1)
#dput(names(merged_correlations))


# Add the new row to the existing data frame
# merged_correlations$Participant.Name <- NEP_Data$`Participant Name`
# merged_correlations$NEPScore <- NEP_Data$`NEP Score`
# merged_correlations$NEPcatagory <- NEP_Data$NEP_Category
# merged_correlations$EcoConcern  <- data$Max_Score_Column
AllDataResults <- merged_correlations

AllDataResults$Group.1 <- factor(AllDataResults$Group.1)
AllDataResults$Group.2 <- factor(AllDataResults$Group.2)
AllDataResults$Group.3 <- factor(AllDataResults$Group.3)
AllDataResults$EcoConcern <- factor(AllDataResults$EcoConcern)
AllDataResults$NEPcatagory <- factor(AllDataResults$NEPcatagory)

#AllDataResults$Participant.Name <- rownames(AllDataResults)
AllDataResults <- merge(AllDataResults, data[, c("Participant.Name", "Group")], by = "Participant.Name", all.x = TRUE)
AllDataResults <- merge(AllDataResults, data[, c("Participant.Name", "Order")], by = "Participant.Name", all.x = TRUE)


AllDataResults






##################################Checking independence of observations. Group versus Solo
##################################Also Solo versus each individual group
##################################

##test to see if the correlation scores are influenced by doing it solo or in a group
t_test_result_Pref <- t.test(Pref_Cor ~ Group.2, data = AllDataResults)
print(t_test_result_Pref)

t_test_result_Divers <- t.test(Divers_Cor ~ Group.2, data = AllDataResults)
print(t_test_result_Divers)

# Perform Mann-Whitney U test
wilcox_test_result <- wilcox.test(Pref_Cor ~ Group.2, data = AllDataResults)
print(wilcox_test_result)

# Perform Kruskal-Wallis test
kruskal_test_result <- kruskal.test(Pref_Cor ~ Group.3, data = AllDataResults)

print(kruskal_test_result)


# Perform pairwise Wilcoxon test
pairwise_wilcox_result <- pairwise.wilcox.test(AllDataResults$Pref_Cor, AllDataResults$Group.3, p.adjust.method = "bonferroni")

print(pairwise_wilcox_result)

################
################


#AllDataResults$Participant.Name <- rownames(AllDataResults)
AllDataResults <- left_join(AllDataResults, 
                            data %>% 
                              select("Participant.Name", "Bio_Score", "Ego_Score", "Altru_Score"), 
                            by = "Participant.Name")


#Pref to correleation scores
correlation_scores <- vector("double")
participant_names <- character()

# Iterate through the columns in dataforQDivers_T and calculate correlations
for (col_name in colnames(dataforQDivers_T)) {
  # Extract the two vectors to be correlated
  vector1 <- dataforQDivers_T[, col_name]
  vector2 <- dataforQPref_T[, col_name]
  
  # Calculate Spearman correlation
  correlation <- cor(vector1, vector2, method = "spearman")
  
  # Append the correlation score to the vector
  correlation_scores <- c(correlation_scores, correlation)
  
  # Append the column name (participant name) to the vector
  participant_names <- c(participant_names, col_name)
}

# Create a data frame to store the results
correlation_results <- data.frame(Participant = participant_names, Correlation = correlation_scores)

# Print the results
DiversPrefcorrelations <- correlation_results

DiversPrefcorrelations <- DiversPrefcorrelations[1:(nrow(DiversPrefcorrelations) - 1), ]
DiversPrefcorrelations$EcoConcern <- data$Max_Score_Column


DiversPrefcorrelations <- na.omit(DiversPrefcorrelations)
DiversPrefcorrelations <- DiversPrefcorrelations %>%
  rename(Participant.Name = Participant)



###########################################################################
###########################################################################
##Mapping Groups Graphs
###########################################################################
###########################################################################


color_mapping <- c("Altruistic" = "#BDD7E7", "Biocentric" = "coral", "Egocentric" = "cyan4")
order_levels <- c("Altruistic", "Biocentric", "Egocentric")
order_levels_NEP <- c("Anti-Ecology","Mid-Ecology", "Pro-Ecology")
order_levels_NEP2 <- c("Pro-Ecology","Mid-Ecology", "Anti-Ecology")
color_mapping_NEP <- c("Anti-Ecology" = "#BDD7E7", "Mid-Ecology" = "cyan4", "Pro-Ecology" = "coral")
color_mapping_NEP2 <- c("Pro-Ecology" = "darkseagreen3", "Mid-Ecology" = "darkgoldenrod2","Anti-Ecology" = "indianred")

# Create a ggplot object
#install.packages("Hmisc")



library(cowplot)
library(gridExtra)
library(Hmisc)





#########################################
##3 graphgs together


# Calculate the mean and standard deviation for Divers_Cor
mean_divers <- mean(AllDataResults$Divers_Cor)
sd_divers <- sd(AllDataResults$Divers_Cor)

# Calculate the standard error of the mean for Divers_Cor
sem_divers <- sd_divers / sqrt(length(AllDataResults$Divers_Cor))

# Calculate the upper and lower bounds of the confidence interval for Divers_Cor
ci_upper_divers <- mean_divers + 1.96 * sem_divers  # Assuming a 95% confidence level
ci_lower_divers <- mean_divers - 1.96 * sem_divers

# Create the boxplot with error bars representing the confidence intervals around the mean for Divers_Cor
DiversityOverallPlot <- ggplot(AllDataResults, aes(x = 1, y = Divers_Cor)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position = position_jitter(width = 0.1), color = "black", size = 3, shape = 1) +
  geom_errorbar(aes(ymin = ci_lower_divers, ymax = ci_upper_divers), width = 0.5, color = "red", size = 1) +
  labs(x = "All Data", y = "Diversity Correlation\nbetween the selected diversity order\nand the calculated diversity", title = "") +
  theme(plot.title = element_blank(),
        legend.text = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_blank(),
        panel.background = element_blank(),  # Remove grey background
        panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),  # Remove minor gridlines
        axis.line = element_line(color = "black", size = 1),
        axis.text.y = element_text(size = 12)) +
  coord_cartesian(ylim = c(-0.6, 1))  # Set y-axis limits

# Calculate the mean and standard deviation for Pref_Cor
mean_pref <- mean(AllDataResults$Pref_Cor)
sd_pref <- sd(AllDataResults$Pref_Cor)

# Calculate the standard error of the mean for Pref_Cor
sem_pref <- sd_pref / sqrt(length(AllDataResults$Pref_Cor))

# Calculate the upper and lower bounds of the confidence interval for Pref_Cor
ci_upper_pref <- mean_pref + 1.96 * sem_pref  # Assuming a 95% confidence level
ci_lower_pref <- mean_pref - 1.96 * sem_pref

# Create the boxplot with error bars representing the confidence intervals around the mean for Pref_Cor
PrefereceOverallPlot <- ggplot(AllDataResults, aes(x = 1, y = Pref_Cor)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position = position_jitter(width = 0.1), color = "black", size = 3, shape = 1) +
  geom_errorbar(aes(ymin = ci_lower_pref, ymax = ci_upper_pref), width = 0.5, color = "red", size = 1) +
  labs(x = "All Data", y = "Preference Correlation\nbetween the selected preference order\nand the calculated diversity", title = "") +
  theme(plot.title = element_blank(),
        legend.text = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_blank(),
        panel.background = element_blank(),  # Remove grey background
        panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),  # Remove minor gridlines
        axis.line = element_line(color = "black", size = 1),
        axis.text.y = element_text(size = 12)) +
  coord_cartesian(ylim = c(-0.6, 1))  # Set y-axis limits

# Calculate the mean and standard deviation for Correlation
mean_correlation <- mean(DiversPrefcorrelations$Correlation)
sd_correlation <- sd(DiversPrefcorrelations$Correlation)

# Calculate the standard error of the mean for Correlation
sem_correlation <- sd_correlation / sqrt(length(DiversPrefcorrelations$Correlation))

# Calculate the upper and lower bounds of the confidence interval for Correlation
ci_upper_correlation <- mean_correlation + 1.96 * sem_correlation  # Assuming a 95% confidence level
ci_lower_correlation <- mean_correlation - 1.96 * sem_correlation

# Create the boxplot with error bars representing the confidence intervals around the mean for Correlation
WithinCorOverallPlot <- ggplot(DiversPrefcorrelations, aes(x = 1, y = Correlation)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position = position_jitter(width = 0.1), color = "black", size = 3, shape = 1) +
  geom_errorbar(aes(ymin = ci_lower_correlation, ymax = ci_upper_correlation), width = 0.5, color = "red", size = 1) +
  labs(x = "All Data", y = "Within Participant Preference Correlation\nbetween the selected preference order\nand the perceived diversity", title = "") +
  theme(plot.title = element_blank(),
        legend.text = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_blank(),
        panel.background = element_blank(),  # Remove grey background
        panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),  # Remove minor gridlines
        axis.line = element_line(color = "black", size = 1),  # Add x and y axis lines
        axis.text.y = element_text(size = 12)) +
  coord_cartesian(ylim = c(-0.6, 1))  # Set y-axis limits

# Add the letter label to each plot with adjusted coordinates
DiversityOverallPlot <- DiversityOverallPlot +
  annotate("text", x = Inf, y = Inf, label = "A", vjust = 1, hjust = 1,
           color = "red", size = 5, fontface = "bold")

PrefereceOverallPlot <- PrefereceOverallPlot +
  annotate("text", x = Inf, y = Inf, label = "B", vjust = 1, hjust = 1,
           color = "red", size = 5, fontface = "bold")

WithinCorOverallPlot <- WithinCorOverallPlot +
  annotate("text", x = Inf, y = Inf, label = "C", vjust = 1, hjust = 1,
           color = "red", size = 5, fontface = "bold")

# Combine the plots
combined_plot <- grid.arrange(DiversityOverallPlot, PrefereceOverallPlot, WithinCorOverallPlot, ncol = 3)



###############Preference plois together

NEPPrefPlot <- ggplot(AllDataResults, aes(x = factor(NEPcatagory, levels = order_levels_NEP), y = Pref_Cor)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(color = "black", size = 2, shape = 1) +  # Add data points
  labs(x = "New Ecological Paradigm", y = "Preference Correlation", 
       title = "Preference Correlation according to\nNew Ecological Paradigm Categories") +
  theme(plot.title = element_text(size = 14), 
        legend.position = "none",  # Remove legend
        axis.title.x = element_blank(),  # Remove x-axis title
        axis.title.y = element_text(size = 12),  # Update y-axis title
        axis.text.x = element_text(size = 12, color = "black", face = "bold"),  # Update x-axis labels size, color, and boldness
        axis.text.y = element_text(size = 12),
        panel.background = element_blank(),  # Remove grey background
        panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),  # Remove minor gridlines
        axis.line = element_line(color = "black", size = 1))  # Add x and y axis lines

PrefPlot <- ggplot(AllDataResults, aes(x = factor(EcoConcern, levels = order_levels), y = Pref_Cor)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(color = "black", size = 2, shape = 1) +  # Add data points
  labs(x = "Adapted Ecological Concern", y = "Preference Correlation",  # Modify labels here
       title = "Preference Correlation according to\nAdapted Ecological Concern Categories") +
  theme(plot.title = element_text(size = 14), 
        legend.position = "none",  # Remove legend
        axis.title.x = element_blank(),  # Remove x-axis title
        axis.title.y = element_text(size = 12),  # Update y-axis title
        axis.text.x = element_text(size = 12, color = "black", face = "bold"),  # Update x-axis labels size, color, and boldness
        axis.text.y = element_text(size = 12),
        panel.background = element_blank(),  # Remove grey background
        panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),  # Remove minor gridlines
        axis.line = element_line(color = "black", size = 1))  # Add x and y axis lines



# Combine plots
PrefCombinedPlot <- plot_grid(NEPPrefPlot + labs(y = "Preference Correlation\n (Compared to calculated diversity order)") + theme(axis.title.x = element_text(size = 12)), 
                              PrefPlot + labs(y = "") + theme(axis.title.x = element_text(size = 12)),
                          labels = c("A", "B"), label_colour = "red", ncol = 2, align = "h")

# Print combined plot
PrefCombinedPlot


###############Diversity plois together


NEPDiversPlot <- ggplot(AllDataResults, aes(x = factor(NEPcatagory, levels = order_levels_NEP), y = Divers_Cor)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(color = "black", size = 2, shape = 1) +  # Add data points
  labs(x = "New Ecological Paradigm", y = "Diversity Correlation", 
       title = "Diversity Correlation according to\nNew Ecological Paradigm Categories") +
  theme(plot.title = element_text(size = 14), 
        legend.position = "none",  # Remove legend
        axis.title.x = element_blank(),  # Remove x-axis title
        axis.title.y = element_text(size = 12),  # Update y-axis title
        axis.text.x = element_text(size = 12, color = "black", face = "bold"),  # Update x-axis labels size, color, and boldness
        axis.text.y = element_text(size = 12),
        panel.background = element_blank(),  # Remove grey background
        panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),  # Remove minor gridlines
        axis.line = element_line(color = "black", size = 1))  # Add x and y axis lines

DiversPlot <- ggplot(AllDataResults, aes(x = factor(EcoConcern, levels = order_levels), y = Divers_Cor)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(color = "black", size = 2, shape = 1) +  # Add data points
  labs(x = "Adapted Ecological Concern", y = "Diversity Correlation",  # Modify labels here
       title = "Diversity Correlation according to\nAdapted Ecological Concern Categories") +
  theme(plot.title = element_text(size = 14), 
        legend.position = "none",  # Remove legend
        axis.title.x = element_blank(),  # Remove x-axis title
        axis.title.y = element_text(size = 12),  # Update y-axis title
        axis.text.x = element_text(size = 12, color = "black", face = "bold"),  # Update x-axis labels size, color, and boldness
        axis.text.y = element_text(size = 12),
        panel.background = element_blank(),  # Remove grey background
        panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),  # Remove minor gridlines
        axis.line = element_line(color = "black", size = 1))  # Add x and y axis lines



# Combine plots
DiversCombinedPlot <- plot_grid(NEPDiversPlot + labs(y = "Diversity Correlation\n (Compared to calculated diversity order)") + theme(axis.title.x = element_text(size = 12)), 
                                DiversPlot + labs(y = "") + theme(axis.title.x = element_text(size = 12)),
                                labels = c("A", "B"), label_colour = "red", ncol = 2, align = "h")

# Print combined plot
DiversCombinedPlot



















AllDataResults <- AllDataResults[!is.na(AllDataResults$Divers_Cor), ]















NEPscoresforallparticipants <- ggplot(AllDataResults, aes(x = 1, y = NEPScore)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position = position_jitter(width = 0.1), color = "black", size = 3, shape = 1) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "crossbar", width = 0.5, color = "red", size = 1) +  # Use stat_summary to calculate mean and standard deviation
  labs(x = "All Data", y = "New Ecological Paradigm Score", title = "New Ecological Paradigm scores\nfor all participants") +
  theme(plot.title = element_text(size = 14), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 12),
        axis.title.x = element_blank(),  # No x-axis label
        axis.title.y = element_text(size = 12),
        axis.text.x = element_blank(),   # No x-axis ticks
        axis.text.y = element_text(size = 12)) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.5, color = "red", size = 1)  # Add manual calculation for confidence intervals

NEPscoresforallparticipants

bg_rects <- data.frame(
  xmin = -Inf, xmax = Inf, ymin = c(75, 45, -Inf), ymax = c(Inf, 75, 45),
  category = c("Pro-Ecology", "Mid-Ecology", "Anti-Ecology")
)

NEPscoresforallparticipants <- NEPscoresforallparticipants +
  geom_rect(data = bg_rects, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = category),
            alpha = c(0.2, 0.2, 0.2), inherit.aes = FALSE) +
  theme(legend.position = "right") +
  scale_fill_manual(breaks = c("Pro-Ecology","Mid-Ecology", "Anti-Ecology"), values = rev(color_mapping_NEP2)) +  # Reverse the order of colors
  guides(fill = guide_legend(override.aes = list(alpha = 0.2), title = "New Ecological Paradigm\nCategory Thresholds"))

NEPscoresforallparticipants





##################################################################
################################Avona Testing
#####################################################

# #ANOVA for testign groups
# model_Pref <- aov(Pref_Cor ~ EcoConcern, data = AllDataResults)
# # Perform the ANOVA
# anova_result_Pref  <- anova(model_Pref)
# # Print the ANOVA table
# print(anova_result_Pref)
# summary(anova_result_Pref)
# #Testing the assumptions
# residuals2 <- residuals(model_Pref)
# hist(residuals2, main="Histogram of Residuals Pref") #normal distributions
# qqnorm(residuals2) #straightish line is desired
# qqline(residuals2) #adds a reference line
# shapiro.test(residuals2) #not significant is good
# 
# 
# library(lme4)
# #install.packages("glmmTMB")
# library(glmmTMB)
# 
# response <- AllDataResults$Pref_Cor
# 
# # Truncate response variable to [-1, 1] range
# response_truncated <- pmin(1, pmax(-1, response))
# 
# # Fit linear mixed-effects model with truncated response variable
# model_truncated <- lmer(response_truncated ~ EcoConcern  + (1 | Group.3), 
#                         data = AllDataResults)
# 
# # Summary of the model
# summary(model_truncated)
# 
# 
# # Linearity: Residuals vs Fitted values plot
# plot(model_truncated, which = 1)
# 
# # Normality of Residuals: Q-Q plot
# qqnorm(residuals(model_truncated))
# qqline(residuals(model_truncated))
# shapiro.test(residuals(model_truncated))
# 
# # Homoscedasticity: Residuals vs Fitted values plot
# plot(model_truncated, which = 3)
# 
# # Independence of Residuals: Autocorrelation plot
# acf(residuals(model_truncated))
# 
# 
# 
# 
# #model_Pref_withgroup <- aov(Pref_Cor ~ EcoConcern + Group.3 + NEPcatagory, data = AllDataResults)
# 
# # Define a function to create Q-Q plot of residuals
# plot_residuals <- function(residuals) {
#   qqnorm(residuals)  # Q-Q plot
#   qqline(residuals)  # Adds a reference line
#   plot_title <- "Q-Q Plot of Residuals"  # Title for the plot
#   title(main = plot_title)  # Add title to the plot
#   return(invisible())  # Make the plot invisible
# }
# 
# # Call the function with your residuals
# plotres2 <- plot_residuals(residuals2)
# 
# 
# 
# model_Divers <- aov(Divers_Cor ~ EcoConcern, data = AllDataResults)
# 
# # Perform the ANOVA
# anova_result_Divers  <- anova(model_Divers)
# 
# # Print the ANOVA table
# print(anova_result_Divers)
# summary(anova_result_Divers)
# 
# #Testing the assumptions
# residuals <- residuals(model_Divers)
# hist(residuals, main="Histogram of Residuals Divers") #normal distributions
# qqnorm(residuals) #straightish line is desired
# qqline(residuals) #adds a reference line
# shapiro.test(residuals) #not significant is good
# 
# 
# 
# model_DiversPref <- aov(Correlation ~ EcoConcern, data = DiversPrefcorrelations)
# 
# # Perform the ANOVA
# anova_result_DiversPref  <- anova(model_DiversPref)
# 
# # Print the ANOVA table
# print(anova_result_DiversPref)
# summary(anova_result_DiversPref)
# 
# #Testing the assumptions
# residuals3 <- residuals(model_DiversPref)
# hist(residuals3, main="Histogram of Residuals DiversPref") #normal distributions
# qqnorm(residuals3) #straightish line is desired
# qqline(residuals3) #adds a reference line
# shapiro.test(residuals3) #not significant is good
# 
# 
# 
# anova_result <- aov(Divers_Cor ~ NEPcatagory, data = AllDataResults)
# summary(anova_result)
# plot(anova_result)
# 
# 
# # Print the result
# print(anova_result)
# 
# tukey_test <- TukeyHSD(anova_result)
# 
# # Print the post hoc results
# print(tukey_test)
# 
# 
# # Filter out rows with non-finite values in Divers_Cor and Pref_Cor
# AllDataResults <- AllDataResults[complete.cases(AllDataResults$Divers_Cor, AllDataResults$Pref_Cor, AllDataResults$EcoConcern), ]
# #AllDataResults$Participant.Name <- rownames(AllDataResults)
# 
# # Perform ANOVA
# anova_result <- aov(cbind(Divers_Cor, Pref_Cor) ~ EcoConcern, data = AllDataResults)
# 
# # Print the results
# print(summary(anova_result))
# 





##################################################################
################################Continuoous variables
#####################################################

# correlationTEST <- cor(AllDataResults$Divers_Cor, AllDataResults$NEPScore)
# 
# print(correlationTEST)
# 
# 
# correlation_matrix <- cor(AllDataResults[c("Bio_Score", "Ego_Score", "Altru_Score", "Divers_Cor")])
# print(correlation_matrix)
# 
# combined_explanatory <- cbind(AllDataResults$Bio_Score, AllDataResults$Ego_Score, AllDataResults$Altru_Score)
# print(combined_explanatory)
# 
# # Fit a linear model using the combined explanatory variable and Divers_Cor as the response variable
# Divers_Eco_Linearmodel <- lm(Divers_Cor ~ combined_explanatory, data = AllDataResults)
# 
# # Print the summary of the linear model
# summary(Divers_Eco_Linearmodel)
# 
# 
# # Obtain residuals from the linear model
# residuals <- residuals(Divers_Eco_Linearmodel)
# 
# # Plot the histogram of residuals
# hist(residuals, breaks = 20, main = "Histogram of Residuals", xlab = "Residuals")
# 
# # Add a normal density curve to the histogram
# lines(density(residuals), col = "red", lwd = 2)
# # Shapiro-Wilk test for normality
# shapiro_test <- shapiro.test(residuals(Divers_Eco_Linearmodel))
# 
# # Print the test results
# print(shapiro_test)





# 
# 
# # Fit a linear model using the combined explanatory variable and Divers_Cor as the response variable
# Pref_Eco_Linearmodel <- lm(Pref_Cor ~ combined_explanatory, data = AllDataResults)
# 
# # Print the summary of the linear model
# summary(Pref_Eco_Linearmodel)
# 

################################################
################################################
#####Getting data from -1 to 1 and making it 0 to 1 with the same distribution for transformation success
################################################
################################################



AllDataResults$NewDistDivers_Cor <- (AllDataResults$Divers_Cor +1)/2
AllDataResults$NewDistPref_Cor <- (AllDataResults$Pref_Cor +1)/2

par(mfrow = c(2, 2))
hist(AllDataResults$Divers_Cor, main = "Original Diversity Correlation")
hist(AllDataResults$NewDistDivers_Cor, main = "New Dist (Above 0) Diversity Correlation")
hist(AllDataResults$Pref_Cor, main = "Original Preference Correlation")
hist(AllDataResults$NewDistPref_Cor, main = "New Dist (Above 0) Preference Correlation")
par(mfrow = c(1, 1)) 


################################################
################################################
#####Function for Normality testing.
################################################
################################################

residual_diagnostics <- function(model) {
  par(mfrow = c(2, 2))  # Set up a 2x2 plotting layout
  
  # Residual diagnostics
  # Residuals vs Fitted values plot
  # Create ggplot objects for residual diagnostics
  
  qqnorm(residuals(model))
  qqline(residuals(model))
  hist(residuals(model))
  plot(residuals(model))
  plot(residuals(model) ~ fitted(model), main = "Residuals vs Fitted", xlab = "Fitted values", ylab = "Residuals")
  
  par(mfrow = c(1, 1))  # Reset plotting layout
}


################################################
################################################
#####Linear model
################################################
################################################
#install.packages("sjPlot")
#install.packages("lme4")
#install.packages("lmerTest")
#install.packages("car")
library(lme4)
library(lmerTest)
library(car)
library(dplyr)
library(emmeans)

library(MuMIn)


AllDataResults$EcoConcern <- relevel(AllDataResults$EcoConcern, ref = "Egocentric")
AllDataResults$NEPcatagory <- relevel(AllDataResults$NEPcatagory, ref = "Mid-Ecology")

Divers_Eco_RandEffmodel <- lmer(NewDistDivers_Cor ~ NEPcatagory + (1 | Group.3), data = AllDataResults)
# Print the summary of the linear model
summary(Divers_Eco_RandEffmodel)

residual_diagnostics(Divers_Eco_RandEffmodel)





# Pref_model1 <- lmer(NewDistPref_Cor ~ EcoConcern + NEPcatagory + (1 | Group.3), data = AllDataResults)
# 
# summary(Pref_model1)
# AIC(Pref_model1)
# residual_diagnostics(Pref_model1)
# shapiro.test(residuals(Pref_model1))



# Fit the Pref_model using the cleaned dataset
Pref_model2 <- lmer((NewDistPref_Cor^3) ~ EcoConcern + NEPcatagory + (1 | Group.3), data = AllDataResults)

summary(Pref_model2)
AIC(Pref_model2)
residual_diagnostics(Pref_model2)
shapiro.test(residuals(Pref_model2))
anova(Pref_model2)

r2_valuesPref_model2 <- r.squaredGLMM(Pref_model2)
# Print the Marginal and Conditional R² values
print(r2_valuesPref_model2)

Pref_model22 <- lmer((NewDistPref_Cor^3) ~ EcoConcern + NEPScore + (1 | Group.3), data = AllDataResults)
summary(Pref_model22)
AIC(Pref_model22)
residual_diagnostics(Pref_model22)
shapiro.test(residuals(Pref_model22))
anova(Pref_model22)


# Fit the Pref_model using the cleaned dataset
Pref_model3 <- lmer((NewDistPref_Cor^3) ~ EcoConcern + NEPcatagory + (1 | Group.3) + (1 | NewDistDivers_Cor), data = AllDataResults)

summary(Pref_model3)
AIC(Pref_model3)
residual_diagnostics(Pref_model3)
shapiro.test(residuals(Pref_model3))





Pref_model5 <- lmer((logit(NewDistPref_Cor)) ~ EcoConcern + NEPcatagory + (1 | Group.3), data = AllDataResults)

summary(Pref_model5)
AIC(Pref_model5)
residual_diagnostics(Pref_model5)
shapiro.test(residuals(Pref_model5))



Pref_model6 <- lmer((NewDistPref_Cor^2) ~ EcoConcern + NEPcatagory + (1 | Group.3), data = AllDataResults)

summary(Pref_model6)
AIC(Pref_model6)
residual_diagnostics(Pref_model6)
shapiro.test(residuals(Pref_model6))
anova(Pref_model6)

r2_valuesPref_model6 <- r.squaredGLMM(Pref_model6)
# Print the Marginal and Conditional R² values
print(r2_valuesPref_model6)






Pref_model7 <- lmer((log(NewDistPref_Cor)) ~ EcoConcern + NEPcatagory + (1 | Group.3) + (1 | NewDistDivers_Cor), data = AllDataResults)

summary(Pref_model7)
AIC(Pref_model7)
residual_diagnostics(Pref_model7)
shapiro.test(residuals(Pref_model7))













Divers_model1 <- lmer(NewDistDivers_Cor ~ EcoConcern + NEPcatagory + (1 | Group.3), data = AllDataResults)

summary(Divers_model1)

residual_diagnostics(Divers_model1)
shapiro.test(residuals(Divers_model1))




# Fit the Divers_model using the cleaned dataset
Divers_model2 <- lmer((NewDistDivers_Cor^3) ~ EcoConcern + NEPcatagory + (1 | Group.3), data = AllDataResults)

summary(Divers_model2)

residual_diagnostics(Divers_model2)
shapiro.test(residuals(Divers_model2))



# Fit the Divers_model using the cleaned dataset

Divers_model3 <- lmer((NewDistDivers_Cor^4) ~ EcoConcern + NEPcatagory + (1 | Group.3), data = AllDataResults)

summary(Divers_model3)

residual_diagnostics(Divers_model3)
shapiro.test(residuals(Divers_model3))

r2_values <- r.squaredGLMM(Divers_model3)
# Print the Marginal and Conditional R² values
print(r2_values)

#############
############ Check between group comparisons

# Obtain the estimated marginal means for EcoConcern
emm <- emmeans(Divers_model3, ~ EcoConcern)

# Conduct pairwise comparisons with Tukey adjustment for multiple comparisons
pairwise <- pairs(emm, adjust = "tukey")

# Summarize the pairwise comparisons
summary(pairwise)








# Fit the Divers_model using the cleaned dataset

Divers_model4 <- lmer((logit(NewDistDivers_Cor)) ~ EcoConcern + NEPcatagory + (1 | Group.3), data = AllDataResults)

summary(Divers_model4)

residual_diagnostics(Divers_model4)
shapiro.test(residuals(Divers_model4))

r2_values <- r.squaredGLMM(Divers_model4)
# Print the Marginal and Conditional R² values
print(r2_values)

#############
############ Check between group comparisons

# Obtain the estimated marginal means for EcoConcern
emm <- emmeans(Divers_model4, ~ EcoConcern)

# Conduct pairwise comparisons with Tukey adjustment for multiple comparisons
pairwise <- pairs(emm, adjust = "tukey")

# Summarize the pairwise comparisons
summary(pairwise)



# Fit the Divers_model using the cleaned dataset

Divers_model5 <- lmer((NewDistDivers_Cor^2) ~ EcoConcern + NEPcatagory + (1 | Group.3), data = AllDataResults)

summary(Divers_model5)

residual_diagnostics(Divers_model5)
shapiro.test(residuals(Divers_model5))









##Anchoring Test
# Perform the Wilcoxon rank-sum test for Divers_Cor between Group A and Group B
AnchoringTestDivers <- wilcox.test(Divers_Cor ~ Group, data = AllDataResults)
AnchoringTestDivers
# Perform the Wilcoxon rank-sum test for Pref_Cor between Group A and Group B
AnchoringTestPref <- wilcox.test(Pref_Cor ~ Group, data = AllDataResults)
AnchoringTestPref


##Testing associations Divers

kruskal.test(AllDataResults$Divers_Cor ~ AllDataResults$NEPcatagory)
kruskal.test(AllDataResults$Divers_Cor ~ AllDataResults$NEPScore)

kruskal.test(AllDataResults$Divers_Cor ~ AllDataResults$EcoConcern)


# #this next thing is wrong
# AllDataResults$Combined_Score <- factor(
# #   paste(AllDataResults$Bio_Score, AllDataResults$Ego_Score, AllDataResults$Altru_Score),
# #   levels = unique(paste(AllDataResults$Bio_Score, AllDataResults$Ego_Score, AllDataResults$Altru_Score))
# # )
# combined_explanatory <- cbind(AllDataResults$Bio_Score, AllDataResults$Ego_Score, AllDataResults$Altru_Score)
# 
# 
# # Perform Kruskal-Wallis test
# kruskal.test(Divers_Cor ~ combined_explanatory, data = AllDataResults)
# 
# # Fligner's test for homogeneity of variances
# fligner.test(Divers_Cor ~ Combined_Score, data = AllDataResults)
# 




##Testing associations Pref

kruskal.test(AllDataResults$Pref_Cor ~ AllDataResults$NEPcatagory)
kruskal.test(AllDataResults$Pref_Cor ~ AllDataResults$NEPScore)

kruskal.test(AllDataResults$Pref_Cor ~ AllDataResults$EcoConcern)



# # Perform Kruskal-Wallis test
# kruskal.test(Pref_Cor ~ Combined_Score, data = AllDataResults)
# 
# 
# # Fligner's test for homogeneity of variances
# fligner.test(Pref_Cor ~ Combined_Score, data = AllDataResults)








#############################################################################
##################Testing the Ecological Concern Method
#############################################################################


EC_Data <- read.csv("Ecological Concern Adaptation.csv", header=TRUE)
str(EC_Data)
dput(names(EC_Data))



# Selecting columns for Original group
Original <- EC_Data[, c("Plants", "Marine.Life", "Birds", "Animals",
                        "Me", "My.Lifestyle", "My.Health", "My.Future",
                        "People.in.my.country", "All.People", "Children",
                        "Future.Generations")]

# Selecting columns for Rank 12 group
Rank_12 <- EC_Data[, c("Plants.12", "Marine.Life..12", "Birds.12", "Animals.12",
                       "Me.12", "My.Lifestyle.12", "My.Health.12", "My.Future.12",
                       "People.in.my.country.12", "All.People.12", "Children.12",
                       "Future.Generations.12")]

# Selecting columns for Rank 7 group
Rank_7 <- EC_Data[, c("Plants.7", "Marine.Life.7", "Birds.7", "Animals.7",
                      "Me.7", "My.Lifestyle.7", "My.Health.7", "My.Future.7",
                      "People.in.my.country.7", "All.People.7", "Children.7",
                      "Future.Generations.7")]







# For Original group
# Biocentric: Plants, Marine Life, Birds, Animals
Biocentric_OG <- Original[, c("Plants", "Marine.Life", "Birds", "Animals")]
# Egocentric: Me, My.Lifestyle, My.Health, My.Future
Egocentric_OG <- Original[, c("Me", "My.Lifestyle", "My.Health", "My.Future")]
# Altruistic: People.in.my.country, All.People, Children, Future.Generations
Altruistic_OG <- Original[, c("People.in.my.country", "All.People", "Children", "Future.Generations")]

# For Rank 12 group
# Biocentric: Plants.12, Marine Life..12, Birds.12, Animals.12
Biocentric_Rank_12 <- Rank_12[, c("Plants.12", "Marine.Life..12", "Birds.12", "Animals.12")]
# Egocentric: Me.12, My.Lifestyle.12, My.Health.12, My.Future.12
Egocentric_Rank_12 <- Rank_12[, c("Me.12", "My.Lifestyle.12", "My.Health.12", "My.Future.12")]
# Altruistic: People.in.my.country.12, All.People.12, Children.12, Future.Generations.12
Altruistic_Rank_12 <- Rank_12[, c("People.in.my.country.12", "All.People.12", "Children.12", "Future.Generations.12")]

# For Rank 7 group
# Biocentric: Plants.7, Marine Life.7, Birds.7, Animals.7
Biocentric_Rank_7 <- Rank_7[, c("Plants.7", "Marine.Life.7", "Birds.7", "Animals.7")]
# Egocentric: Me.7, My.Lifestyle.7, My.Health.7, My.Future.7
Egocentric_Rank_7 <- Rank_7[, c("Me.7", "My.Lifestyle.7", "My.Health.7", "My.Future.7")]
# Altruistic: People.in.my.country.7, All.People.7, Children.7, Future.Generations.7
Altruistic_Rank_7 <- Rank_7[, c("People.in.my.country.7", "All.People.7", "Children.7", "Future.Generations.7")]





# Create Results dataset
Results <- data.frame(matrix(nrow = nrow(Biocentric_OG), ncol = 0))

# Calculate averages for Biocentric, Egocentric, and Altruistic groups in Original dataset
Results$Biocentric_OG_Average <- rowMeans(Biocentric_OG)
Results$Egocentric_OG_Average <- rowMeans(Egocentric_OG)
Results$Altruistic_OG_Average <- rowMeans(Altruistic_OG)

# Calculate averages for Biocentric, Egocentric, and Altruistic groups in Original dataset
Results$Biocentric_OG_Sum <- rowSums(Biocentric_OG)
Results$Egocentric_OG_Sum <- rowSums(Egocentric_OG)
Results$Altruistic_OG_Sum <- rowSums(Altruistic_OG)

# Calculate sums for Biocentric, Egocentric, and Altruistic groups in Rank 12 dataset
Results$Biocentric_12_Sum <- rowSums(Biocentric_Rank_12)
Results$Egocentric_12_Sum <- rowSums(Egocentric_Rank_12)
Results$Altruistic_12_Sum <- rowSums(Altruistic_Rank_12)

# Calculate sums for Biocentric, Egocentric, and Altruistic groups in Rank 7 dataset
Results$Biocentric_7_Sum <- rowSums(Biocentric_Rank_7)
Results$Egocentric_7_Sum <- rowSums(Egocentric_Rank_7)
Results$Altruistic_7_Sum <- rowSums(Altruistic_Rank_7)




Results$OG_Category <- apply(Results[, c("Biocentric_OG_Average", "Egocentric_OG_Average", "Altruistic_OG_Average")], 1,
                             function(x) {
                               categories <- c("Biocentric", "Egocentric", "Altruistic")
                               max_val <- max(x)
                               max_categories <- categories[x == max_val]
                               
                               if (length(max_categories) > 1) {
                                 return(paste("Equivalent:", paste(max_categories, collapse = ", ")))
                               } else {
                                 return(max_categories)
                               }
                             })
Results$OG_Category_Sum <- apply(Results[, c("Biocentric_OG_Sum", "Egocentric_OG_Sum", "Altruistic_OG_Sum")], 1,
                             function(x) {
                               categories <- c("Biocentric", "Egocentric", "Altruistic")
                               max_val <- max(x)
                               max_categories <- categories[x == max_val]
                               
                               if (length(max_categories) > 1) {
                                 return(paste("Equivalent:", paste(max_categories, collapse = ", ")))
                               } else {
                                 return(max_categories)
                               }
                             })

Results$Twelve_Category <- apply(Results[, c("Biocentric_12_Sum", "Egocentric_12_Sum", "Altruistic_12_Sum")], 1,
                                 function(x) {
                                   categories <- c("Biocentric", "Egocentric", "Altruistic")
                                   max_val <- max(x)
                                   max_categories <- categories[x == max_val]
                                   
                                   if (length(max_categories) > 1) {
                                     return(paste("Equivalent:", paste(max_categories, collapse = ", ")))
                                   } else {
                                     return(max_categories)
                                   }
                                 })


Results$Seven_Category <- apply(Results[, c("Biocentric_7_Sum", "Egocentric_7_Sum", "Altruistic_7_Sum")], 1,
                                function(x) {
                                  categories <- c("Biocentric", "Egocentric", "Altruistic")
                                  max_val <- max(x)
                                  max_categories <- categories[x == max_val]
                                  
                                  if (length(max_categories) > 1) {
                                    return(paste("Equivalent:", paste(max_categories, collapse = ", ")))
                                  } else {
                                    return(max_categories)
                                  }
                                })



dput(names(Results))

# Convert category columns to factors
Results$OG_Category <- as.factor(Results$OG_Category)
Results$OG_Category_Sum <- as.factor(Results$OG_Category_Sum)
Results$Twelve_Category <- as.factor(Results$Twelve_Category)
Results$Seven_Category <- as.factor(Results$Seven_Category)



# Count occurrences of "Equivalent" in each variable
og_count <- sum(grepl("Equivalent", Results$OG_Category))
og_count_Sum <- sum(grepl("Equivalent", Results$OG_Category_Sum))
twelve_count <- sum(grepl("Equivalent", Results$Twelve_Category))
seven_count <- sum(grepl("Equivalent", Results$Seven_Category))

# Print the counts
cat("Count of 'Equivalent' in OG_Category:", og_count, "\n")
cat("Count of 'Equivalent' in OG_Category SUM:", og_count_Sum, "\n")
cat("Count of 'Equivalent' in Twelve_Category:", twelve_count, "\n")
cat("Count of 'Equivalent' in Seven_Category:", seven_count, "\n")


# Define a function to determine equivalence
is_equivalent <- function(category1, category2) {
  if (category1 == category2) {
    return(TRUE)
  }
  if (grepl("Equivalent", category1)) {
    return(category2 %in% unlist(strsplit(sub("Equivalent: ", "", category1), ", ")))
  }
  if (grepl("Equivalent", category2)) {
    return(category1 %in% unlist(strsplit(sub("Equivalent: ", "", category2), ", ")))
  }
  return(FALSE)
}


# Compare OG_Category and Twelve_Category while recognizing equivalence
comparison_12_7 <- mapply(is_equivalent, Results$Twelve_Category, Results$Seven_Category)

# Calculate agreement considering equivalence
agreement_12_7 <- sum(comparison_12_7) / length(comparison_12_7)



# Compare OG_Category and Twelve_Category while recognizing equivalence
comparison_12andOG <- mapply(is_equivalent, Results$OG_Category, Results$Twelve_Category)

# Calculate agreement considering equivalence
agreement_12andOG <- sum(comparison_12andOG) / length(comparison_12andOG)



comparison_7andOG <- mapply(is_equivalent, Results$OG_Category, Results$Seven_Category)

# Calculate agreement considering equivalence
agreement_7andOG <- sum(comparison_7andOG) / length(comparison_7andOG)




comparison_7andOG_Sum <- mapply(is_equivalent, Results$OG_Category_Sum, Results$Seven_Category)

# Calculate agreement considering equivalence
agreement_7andOG_Sum <- sum(comparison_7andOG_Sum) / length(comparison_7andOG_Sum)



# Display the agreement
# Display the agreement
print(paste("Agreement between Twelve_Category and Seven_Category:", agreement_12_7))
print(paste("Agreement between OG_Category and Twelve_Category (considering equivalence):", agreement_12andOG))
print(paste("Agreement between OG_Category and Seven_Category (considering equivalence):", agreement_7andOG))
print(paste("Agreement between OG_Category SUM and Seven_Category (considering equivalence):", agreement_7andOG_Sum))



#write.csv(Results, "Results.csv", row.names = FALSE)

#install.packages("vcd")
library(vcd)

# Create a contingency table
contingency_table_OGand12 <- table(Results$OG_Category, Results$Twelve_Category)

# Perform Chi-Square Test for Independence
chi_square_test_OGand12 <- chisq.test(contingency_table_OGand12)

# Print the results
print(chi_square_test_OGand12)


# Create a contingency table
contingency_table_OGand7 <- table(Results$OG_Category, Results$Seven_Category)

# Perform Chi-Square Test for Independence
chi_square_test_OGand7 <- chisq.test(contingency_table_OGand7)

# Print the results
print(chi_square_test_OGand7)

print(chi_square_test_OGand12)
#############################################################################

#############################################################################

# Perform Chi-Squared Test of Association for OG_Category and Twelve_Category
chi_squared_test_association_OGand12 <- chisq.test(table(Results$OG_Category, Results$Twelve_Category))

# Print the results
print(chi_squared_test_association_OGand12)

# Perform Chi-Squared Test of Association for OG_Category and Seven_Category
chi_squared_test_association_OGand7 <- chisq.test(table(Results$OG_Category, Results$Seven_Category))

# Print the results
print(chi_squared_test_association_OGand7)





##########################

# Create a new column and initialize it with NA
Results$OG_Num_Ego <- 0
Results$OG_Num_Bio <- 0
Results$OG_Num_Altr <- 0

Results$Seven_Num_Ego <- 0
Results$Seven_Num_Bio <- 0
Results$Seven_Num_Altr <- 0



# Iterate through each row
for (i in 1:nrow(Results)) {
  
  if (grepl("Ego", Results$OG_Category[i])) {
    # Take the OG_Category value directly
    Results$OG_Num_Ego[i] <- 1
  }   
  if (grepl("Bio", Results$OG_Category[i])) {
    # Take the OG_Category value directly
    Results$OG_Num_Bio[i] <- 1
  }
  if (grepl("Alt", Results$OG_Category[i])) {
    # Take the OG_Category value directly
    Results$OG_Num_Altr[i] <- 1
  }
  if (grepl("Ego", Results$Seven_Category[i])) {
    # Take the OG_Category value directly
    Results$Seven_Num_Ego[i] <- 1
  }   
  if (grepl("Bio", Results$Seven_Category[i])) {
    # Take the OG_Category value directly
    Results$Seven_Num_Bio[i] <- 1
  }
  if (grepl("Alt", Results$Seven_Category[i])) {
    # Take the OG_Category value directly
    Results$Seven_Num_Altr[i] <- 1
  }
}


# Perform Chi-Squared Test of Association for OG_Category and Seven_Category
chi_squared_EgoOG_Ego7 <- chisq.test(table(Results$OG_Num_Ego, Results$Seven_Num_Ego))

chi_squared_AltrOG_Altr7 <- chisq.test(table(Results$OG_Num_Altr, Results$Seven_Num_Altr))

chi_squared_BioOG_Bio7 <- chisq.test(table(Results$OG_Num_Bio, Results$Seven_Num_Bio))

print(chi_squared_EgoOG_Ego7)
print(chi_squared_BioOG_Bio7)
print(chi_squared_AltrOG_Altr7)





#############This aligns Equivelant with the category for a true Chi Square test########



# Convert the new column to factor
Results$OG_NoEquiv_Seven <- as.factor(Results$OG_NoEquiv_Seven)





# Create a new column and initialize it with NA
Results$OG_NoEquiv_Seven <- NA

# Iterate through each row
for (i in 1:nrow(Results)) {
  # Split the OG_Category by ": " to check for "Equivalent"
  og_cats <- strsplit(as.character(Results$OG_Category[i]), ": ")[[1]]
  
  # If there's only one category or if "Equivalent" is not present
  if (length(og_cats) == 1 || !("Equivalent" %in% og_cats)) {
    # Take the OG_Category value directly
    Results$OG_NoEquiv_Seven[i] <- og_cats[1]
  } else {
    # Get the categories from Seven_Category
    seven_cat <- as.character(Results$Seven_Category[i])
    # Check if any of the categories in Seven_Category is present in OG_Category
    common_cat <- intersect(og_cats, seven_cat)
    if (length(common_cat) >= 1) {
      Results$OG_NoEquiv_Seven[i] <- common_cat[1]  # Select the first common category
    } else {
      # If there's no common category, select any category from Seven_Category
      Results$OG_NoEquiv_Seven[i] <- seven_cat[1]  # Select the first category from Seven_Category
    }
  }
}

# Convert the new column to factor
Results$OG_NoEquiv_Seven <- as.factor(Results$OG_NoEquiv_Seven)


Results$OG_NoEquiv_Twelve <- NA

# Iterate through each row
for (i in 1:nrow(Results)) {
  # Split the OG_Category by ": " to check for "Equivalent"
  og_cats <- strsplit(as.character(Results$OG_Category[i]), ": ")[[1]]
  
  # If there's only one category or if "Equivalent" is not present
  if (length(og_cats) == 1 || !("Equivalent" %in% og_cats)) {
    # Take the OG_Category value directly
    Results$OG_NoEquiv_Twelve[i] <- og_cats[1]
  } else {
    # Get the categories from Twelve_Category
    Twelve_cat <- as.character(Results$Twelve_Category[i])
    # Check if any of the categories in Twelve_Category is present in OG_Category
    common_cat <- intersect(og_cats, Twelve_cat)
    if (length(common_cat) >= 1) {
      Results$OG_NoEquiv_Twelve[i] <- common_cat[1]  # Select the first common category
    } else {
      # If there's no common category, select any category from Twelve_Category
      Results$OG_NoEquiv_Twelve[i] <- Twelve_cat[1]  # Select the first category from Twelve_Category
    }
  }
}

# Convert the new column to factor
Results$OG_NoEquiv_Twelve <- as.factor(Results$OG_NoEquiv_Twelve)





# Perform Chi-Squared Test of Association for OG_Category and Seven_Category
chi_squared_test_association_OG_NoEquiv_Sevenand7 <- chisq.test(table(Results$OG_NoEquiv_Seven, Results$Seven_Category))

# Print the results
print(chi_squared_test_association_OG_NoEquiv_Sevenand7)



# Perform Chi-Squared Test of Association for OG_Category and Twelve_Category
chi_squared_test_association_OG_NoEquiv_Twelveand12 <- chisq.test(table(Results$OG_NoEquiv_Twelve, Results$Twelve_Category))

# Print the results
print(chi_squared_test_association_OG_NoEquiv_Twelveand12)





########################Qualatative


biolist <- c("Plants", "Marine.Life", "Birds", "Animals")
egolist <- c("Me", "My.Lifestyle", "My.Health", "My.Future")
altrulist <- c("People.in.my.Country", "All.People", "Children", "Future.Generations")


#detach("package:lmerTest", unload = TRUE)
#detach("package:lme4", unload = TRUE)
#detach("package:Matrix", unload = TRUE)
#detach("package:quanteda", unload = TRUE)

#install.packages("lavaan", type = "source")
library(lavaan)

# Create a data frame with only the relevant variables
data_subset <- data[, c("Plants", "Marine.Life", "Birds", "Animals", 
                        "Me", "My.Lifestyle", "My.Health", "My.Future", 
                        "People.in.my.Country", "All.People", 
                        "Children", "Future.Generations")]

# Specify the CFA model
model <- '
    # Define latent factors
    biolatent =~ Plants + Marine.Life + Birds + Animals
    egolatent =~ Me + My.Lifestyle + My.Health + My.Future
    altrulatent =~ People.in.my.Country + All.People + Children + Future.Generations
'

# Fit the CFA model to the data
fit <- cfa(model, data = data_subset)

# Summary of the model
summary(fit)

# Additional fit indices
fitMeasures(fit)





library(psych)

# Prepare your data
# Assuming your data frame is named 'data' and contains the variables of interest
# Remove missing values if any
data <- na.omit(data)
# Select variables for factor analysis
variables <- data[, c("Plants", "Marine.Life", "Birds", "Animals", "Me", "My.Lifestyle", "My.Health", "My.Future", "People.in.my.Country", "All.People", "Children", "Future.Generations")]

# Conduct factor analysis
factor_analysis <- fa(variables, nfactors = 3, rotate = "varimax")

# View factor analysis results
summary(factor_analysis)


# Extract factor scores for each participant
#factor_scores <- as.data.frame(factorscores(factor_analysis))

# Add participant names/IDs if available
# Assuming your participant names/IDs are in a column named 'Participant.Name'
#factor_scores$Participant.Name <- data$Participant.Name

# View factor scores
#print(factor_scores)






# 
# ### the number of factors is determined by theory, - we propose that environmental attitude (ecoconcern = 3 categories) will be the distinguishing factor
# 
# Prefresults <- qmethod(dataforQPref_T, nfactors = 3)
# 
# Prefresults2 <- qmethod(dataforQPref_T, nfactors = 2)
# Prefresults4 <- qmethod(dataforQPref_T, nfactors = 4)
# 
# ##exact loading score on each factor
# QSort_Results_Pref <- data.frame(round(Prefresults$loa, digits = 2))
# 
# ##factor loading assigned
# QSort_FactorLoadings_Pref <- data.frame(Prefresults$flag)
# 
# 
# # ## Create a new column called Participant.Name
# # ## Remove the existing row names
# QSort_Results_Pref$Participant.Name <- rownames(QSort_Results_Pref)
# rownames(QSort_Results_Pref) <- NULL
# QSort_FactorLoadings_Pref$Participant.Name <- rownames(QSort_FactorLoadings_Pref)
# rownames(QSort_FactorLoadings_Pref) <- NULL
# 
# # Find the index of the row where Participant.Name is "ActualDiversity"
# index_to_drop <- which(QSort_FactorLoadings_Pref$Participant.Name == "ActualDiversity")
# # Drop the row
# QSort_FactorLoadings_Pref <- QSort_FactorLoadings_Pref[-index_to_drop, ]
# 
# 
# QSort_FactorLoadings_Pref <- merge(QSort_FactorLoadings_Pref, AllDataResults, by = "Participant.Name", all.x = FALSE)
# DropColumns <- c("Divers_Cor", "Pref_Cor", "NEPScore", "Group", "Order", "SQrDivers", "Divers_Cor_log", 
# "Divers_Cor_cbr", "Divers_Cor_sqrt", "Divers_Cor_reciprocal", "Divers_Cor_sqrtTWICE")
# QSort_FactorLoadings_Pref <- QSort_FactorLoadings_Pref[, -which(names(QSort_FactorLoadings_Pref) %in% DropColumns)]


# QSort_FactorLoadings_Pref_manova_model <- manova(cbind(flag_f1, flag_f2, flag_f3) ~ EcoConcern, data = QSort_FactorLoadings_Pref)
# 
# # Print the MANOVA results
# summary(QSort_FactorLoadings_Pref_manova_model, test = "Wilks")
# 
# QSort_FactorLoadings_Pref_manova_modelNEP <- manova(cbind(flag_f1, flag_f2, flag_f3) ~ NEPcatagory, data = QSort_FactorLoadings_Pref)
# 
# # Print the MANOVA results
# summary(QSort_FactorLoadings_Pref_manova_modelNEP, test = "Wilks")



Qdata <- read.csv("PrefeFactors.csv", header=TRUE)

dput(names(Qdata))
dput(names(AllDataResults))

Qdata2 <- merge(Qdata, AllDataResults[, c("Participant.Name", "EcoConcern")], by = "Participant.Name", all.x = TRUE)

str(Qdata2)
dput(names(Qdata2))
# Convert "Factor.Number" to a factor with specified levels
Qdata2$Factor.Number <- factor(Qdata2$Factor.Number, levels = c("Factor 1", "Factor 2", "Factor 3"))

# Convert "EcoConcern" to a factor with specified levels
Qdata2$EcoConcern <- factor(Qdata2$EcoConcern, levels = c("Biocentric", "Egocentric", "Altruistic"))

contingency_table <- table(Qdata2$EcoConcern, Qdata2$Factor.Number)

# Print the contingency table
print(contingency_table)

# Perform a chi-squared test of independence
chi_squared_test <- chisq.test(contingency_table)

# Print the chi-squared test results
print(chi_squared_test)

# 




subset_data_from_data <- data[c("Participant.Name", "Preference.Top", "Preference.Bottom", 
                                "Preference.Similar", "Preference.Observable.Difference", 
                                "Preference.Rank.Difficulty", "Preference.Comments",
                                "Diversity.Top", "Diversity.Bottom", "Diversity.Similar", 
                                "Diversity.Observable.Difference", "Diversity.Rank.Difficulty", 
                                "Diversity.Comments")]
# Subset Factor 1 from Qdata2
subset_data_F1 <- subset(Qdata2, Factor.Number == "Factor 1")

# Perform the merge
QMethodPref_Qual_F1 <- merge(subset_data_F1, subset_data_from_data, by = "Participant.Name", all.x = TRUE)

# Subset Factor 2 from Qdata2
subset_data_F2 <- subset(Qdata2, Factor.Number == "Factor 2")

# Perform the merge
QMethodPref_Qual_F2 <- merge(subset_data_F2, subset_data_from_data, by = "Participant.Name", all.x = TRUE)

# Subset Factor 3 from Qdata2
subset_data_F3 <- subset(Qdata2, Factor.Number == "Factor 3")

# Perform the merge
QMethodPref_Qual_F3 <- merge(subset_data_F3, subset_data_from_data, by = "Participant.Name", all.x = TRUE)



#install.packages("tm")
#install.packages("wordcloud")
library(tm)
library(wordcloud)
removeCustomWords <- function(x) gsub("looks", "", x)

# Prepare the text data for QMethodPref_Qual_F1
text_corpus_F1 <- Corpus(VectorSource(QMethodPref_Qual_F1$Preference.Top))
text_corpus_F1 <- tm_map(text_corpus_F1, content_transformer(tolower))
text_corpus_F1 <- tm_map(text_corpus_F1, removePunctuation)
text_corpus_F1 <- tm_map(text_corpus_F1, removeNumbers)
text_corpus_F1 <- tm_map(text_corpus_F1, removeWords, stopwords("en"))
text_corpus_F1 <- tm_map(text_corpus_F1, removeCustomWords) 
text_corpus_F1 <- tm_map(text_corpus_F1, stripWhitespace)

# Create a word cloud for QMethodPref_Qual_F1
wordcloud(words = text_corpus_F1, min.freq = 5, random.order = FALSE, colors = brewer.pal(8, "Dark2"))

# Generate a frequency table for QMethodPref_Qual_F1
word_freq_F1 <- table(unlist(strsplit(as.character(text_corpus_F1), "\\s+")))
word_freq_df_F1 <- data.frame(word = names(word_freq_F1), frequency = as.numeric(word_freq_F1), row.names = NULL)
word_freq_df_F1 <- word_freq_df_F1[order(-word_freq_df_F1$frequency), ]
head(word_freq_df_F1)

# Prepare the text data for QMethodPref_Qual_F2
text_corpus_F2 <- Corpus(VectorSource(QMethodPref_Qual_F2$Preference.Top))
text_corpus_F2 <- tm_map(text_corpus_F2, content_transformer(tolower))
text_corpus_F2 <- tm_map(text_corpus_F2, removePunctuation)
text_corpus_F2 <- tm_map(text_corpus_F2, removeNumbers)
text_corpus_F2 <- tm_map(text_corpus_F2, removeWords, stopwords("en"))
text_corpus_F2 <- tm_map(text_corpus_F2, removeCustomWords) 
text_corpus_F2 <- tm_map(text_corpus_F2, stripWhitespace)

# Create a word cloud for QMethodPref_Qual_F2
wordcloud(words = text_corpus_F2, min.freq = 5, random.order = FALSE, colors = brewer.pal(8, "Dark2"))

# Generate a frequency table for QMethodPref_Qual_F2
word_freq_F2 <- table(unlist(strsplit(as.character(text_corpus_F2), "\\s+")))
word_freq_df_F2 <- data.frame(word = names(word_freq_F2), frequency = as.numeric(word_freq_F2), row.names = NULL)
word_freq_df_F2 <- word_freq_df_F2[order(-word_freq_df_F2$frequency), ]
head(word_freq_df_F2)

# Prepare the text data for QMethodPref_Qual_F3
text_corpus_F3 <- Corpus(VectorSource(QMethodPref_Qual_F3$Preference.Top))
text_corpus_F3 <- tm_map(text_corpus_F3, content_transformer(tolower))
text_corpus_F3 <- tm_map(text_corpus_F3, removePunctuation)
text_corpus_F3 <- tm_map(text_corpus_F3, removeNumbers)
text_corpus_F3 <- tm_map(text_corpus_F3, removeWords, stopwords("en"))
text_corpus_F3 <- tm_map(text_corpus_F3, removeCustomWords) 
text_corpus_F3 <- tm_map(text_corpus_F3, stripWhitespace)

# Create a word cloud for QMethodPref_Qual_F3
wordcloud(words = text_corpus_F3, min.freq = 8, random.order = FALSE, colors = brewer.pal(8, "Dark2"))

# Generate a frequency table for QMethodPref_Qual_F3
word_freq_F3 <- table(unlist(strsplit(as.character(text_corpus_F3), "\\s+")))
word_freq_df_F3 <- data.frame(word = names(word_freq_F3), frequency = as.numeric(word_freq_F3), row.names = NULL)
word_freq_df_F3 <- word_freq_df_F3[order(-word_freq_df_F3$frequency), ]
head(word_freq_df_F3)



word_freq_all <- rbind(word_freq_df_F1, word_freq_df_F2, word_freq_df_F3)
word_freq_merged <- merge(merge(word_freq_df_F1, word_freq_df_F2, by = "word", all = TRUE), word_freq_df_F3, by = "word", all = TRUE)

# Replace NA values with 0
word_freq_merged[is.na(word_freq_merged)] <- 0

# Rename columns
colnames(word_freq_merged) <- c("word", "frequency_F1", "frequency_F2", "frequency_F3")

# Print the merged word frequency table
print(word_freq_merged)

# Create a word cloud for each factor
par(mfrow=c(1,3))  # Set up the layout for the plots
wordcloud(words = text_corpus_F1, min.freq = 6, random.order = FALSE, colors = brewer.pal(8, "Dark2"), main = "Factor 1")
wordcloud(words = text_corpus_F2, min.freq = 4, random.order = FALSE, colors = brewer.pal(8, "Dark2"), main = "Factor 2")
wordcloud(words = text_corpus_F3, min.freq = 2, random.order = FALSE, colors = brewer.pal(8, "Dark2"), main = "Factor 3")



#######bottom
# Prepare the text data for Preference.Bottom for Factor 1
text_corpus_F1_bottom <- Corpus(VectorSource(QMethodPref_Qual_F1$Preference.Bottom))
text_corpus_F1_bottom <- tm_map(text_corpus_F1_bottom, content_transformer(tolower))
text_corpus_F1_bottom <- tm_map(text_corpus_F1_bottom, removePunctuation)
text_corpus_F1_bottom <- tm_map(text_corpus_F1_bottom, removeNumbers)
text_corpus_F1_bottom <- tm_map(text_corpus_F1_bottom, removeWords, stopwords("en"))
text_corpus_F1_bottom <- tm_map(text_corpus_F1_bottom, stripWhitespace)

# Create a word cloud for Preference.Bottom for Factor 1
wordcloud(words = text_corpus_F1_bottom, min.freq = 5, random.order = FALSE, colors = brewer.pal(8, "Dark2"), main = "Factor 1 - Preference.Bottom")

# Generate a frequency table for Preference.Bottom for Factor 1
word_freq_F1_bottom <- table(unlist(strsplit(as.character(text_corpus_F1_bottom), "\\s+")))
word_freq_df_F1_bottom <- data.frame(word = names(word_freq_F1_bottom), frequency = as.numeric(word_freq_F1_bottom), row.names = NULL)
word_freq_df_F1_bottom <- word_freq_df_F1_bottom[order(-word_freq_df_F1_bottom$frequency), ]
head(word_freq_df_F1_bottom)

# Prepare the text data for Preference.Bottom for Factor 2
text_corpus_F2_bottom <- Corpus(VectorSource(QMethodPref_Qual_F2$Preference.Bottom))
text_corpus_F2_bottom <- tm_map(text_corpus_F2_bottom, content_transformer(tolower))
text_corpus_F2_bottom <- tm_map(text_corpus_F2_bottom, removePunctuation)
text_corpus_F2_bottom <- tm_map(text_corpus_F2_bottom, removeNumbers)
text_corpus_F2_bottom <- tm_map(text_corpus_F2_bottom, removeWords, stopwords("en"))
text_corpus_F2_bottom <- tm_map(text_corpus_F2_bottom, stripWhitespace)

# Create a word cloud for Preference.Bottom for Factor 2
wordcloud(words = text_corpus_F2_bottom, min.freq = 4, random.order = FALSE, colors = brewer.pal(8, "Dark2"), main = "Factor 2 - Preference.Bottom")

# Generate a frequency table for Preference.Bottom for Factor 2
word_freq_F2_bottom <- table(unlist(strsplit(as.character(text_corpus_F2_bottom), "\\s+")))
word_freq_df_F2_bottom <- data.frame(word = names(word_freq_F2_bottom), frequency = as.numeric(word_freq_F2_bottom), row.names = NULL)
word_freq_df_F2_bottom <- word_freq_df_F2_bottom[order(-word_freq_df_F2_bottom$frequency), ]
head(word_freq_df_F2_bottom)

# Prepare the text data for Preference.Bottom for Factor 3
text_corpus_F3_bottom <- Corpus(VectorSource(QMethodPref_Qual_F3$Preference.Bottom))
text_corpus_F3_bottom <- tm_map(text_corpus_F3_bottom, content_transformer(tolower))
text_corpus_F3_bottom <- tm_map(text_corpus_F3_bottom, removePunctuation)
text_corpus_F3_bottom <- tm_map(text_corpus_F3_bottom, removeNumbers)
text_corpus_F3_bottom <- tm_map(text_corpus_F3_bottom, removeWords, stopwords("en"))
text_corpus_F3_bottom <- tm_map(text_corpus_F3_bottom, stripWhitespace)

# Create a word cloud for Preference.Bottom for Factor 3
wordcloud(words = text_corpus_F3_bottom, min.freq = 2, random.order = FALSE, colors = brewer.pal(8, "Dark2"), main = "Factor 3 - Preference.Bottom")

# Generate a frequency table for Preference.Bottom for Factor 3
word_freq_F3_bottom <- table(unlist(strsplit(as.character(text_corpus_F3_bottom), "\\s+")))
word_freq_df_F3_bottom <- data.frame(word = names(word_freq_F3_bottom), frequency = as.numeric(word_freq_F3_bottom), row.names = NULL)
word_freq_df_F3_bottom <- word_freq_df_F3_bottom[order(-word_freq_df_F3_bottom$frequency), ]
head(word_freq_df_F3_bottom)

# Merge all frequency tables
word_freq_merged_bottom <- merge(merge(word_freq_df_F1_bottom, word_freq_df_F2_bottom, by = "word", all = TRUE), word_freq_df_F3_bottom, by = "word", all = TRUE)

# Replace NA values with 0
word_freq_merged_bottom[is.na(word_freq_merged_bottom)] <- 0

# Rename columns
colnames(word_freq_merged_bottom) <- c("word", "frequency_F1_bottom", "frequency_F2_bottom", "frequency_F3_bottom")

# Print the merged word frequency table for Preference.Bottom
print(word_freq_merged_bottom)



par(mfrow=c(1,1))










library(tm)
library(wordcloud)
library(RColorBrewer)

# Define the text columns
text_columns <- c("Preference.Top", "Preference.Bottom", "Diversity.Top", "Diversity.Bottom")

# Loop through each text column
for (col_name in text_columns) {
    # Extract text data
    text_data <- data[[col_name]]

    # Create a corpus
    corpus <- Corpus(VectorSource(text_data))

    # Preprocess the text data
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, removeWords, stopwords("english"))
    corpus <- tm_map(corpus, stripWhitespace)

    # Create term-document matrix
    tdm <- TermDocumentMatrix(corpus)
    matrix <- as.matrix(tdm)
    word_freq <- sort(rowSums(matrix), decreasing = TRUE)

    # Generate word cloud
    wordcloud(words = names(word_freq), freq = word_freq, min.freq = 5,
              random.order = FALSE, colors = brewer.pal(8, "Dark2"),
              main = col_name)

    # Save the word cloud as an image file
    png(filename = paste0(col_name, "_wordcloud.png"))
    wordcloud(words = names(word_freq), freq = word_freq, min.freq = 5,
              random.order = FALSE, colors = brewer.pal(8, "Dark2"),
              main = col_name)
    dev.off()
}







#Create a data frame for the results
top_terms_table <- data.frame(
  Preference_Top = names(top_terms_Preftop),
  Frequency_Preference_Top = as.numeric(top_terms_Preftop),
  Diversity_Top = names(top_terms_Diverstop),
  Frequency_Diversity_Top = as.numeric(top_terms_Diverstop),
  Diversity_Bottom = names(top_terms_Diversbottom),
  Frequency_Diversity_Bottom = as.numeric(top_terms_Diversbottom),
  Preference_Bottom = names(top_terms_Prefbottom),
  Frequency_Preference_Bottom = as.numeric(top_terms_Prefbottom)
)

# Print the table
print(top_terms_table)


# 
# 
# 
# 
# 










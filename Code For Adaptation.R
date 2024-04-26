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


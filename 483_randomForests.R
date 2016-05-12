 

library(ggplot2) # visualization
library(ggthemes) # visualizationbo
library(dplyr) # data manipulation
library(lubridate) # dates
library(rpart) # rpart for imputation
library(randomForest) # classification algorithm

#load the train set
outcome.train <- read.csv('train.csv', header = T, stringsAsFactors = F)

#load the test set
outcome.test <- read.csv('test.csv', header = T, stringsAsFactors = F)

# Convert the ID column in test set the char and change column name of AnimalID in train set to make them consistent with each other
outcome.test$ID = as.character(outcome.test$ID)
names(outcome.train)[1] <- 'ID'

# Combine the two data sets
outcomes = bind_rows(outcome.train, outcome.test)

# Clean the data set
outcome.train$Name = ifelse(nchar(outcome.train$Name)==0, 'Nameless', outcome.train$Name)

# create another column which indicates if an animal has a name or not, represented by either 0 or 1 and add to the data frame
hasName = ifelse(outcomes$Name == 'Nameless', 0, 1)
outcomes = data.frame(hasName, outcomes)

# extract time variables from date/time data using lubridate (credit: MeganRisdal)
outcomes$Hour = hour(outcomes$DateTime)
outcomes$Weekday = wday(outcomes$DateTime)
outcomes$Month = month(outcomes$DateTime)
outcomes$Year = year(outcomes$DateTime)

# I'm not sure how useful time of day is, from the prediction point of view, but we can see
outcomes$TimeofDay = ifelse(outcomes$Hour > 5 & outcomes$Hour < 11, 'morning', ifelse(outcomes$Hour > 11 & outcomes$Hour < 16, 'midday', ifelse (outcomes$Hour > 16 & outcomes$Hour < 20, 'evening', 'night')))

# Convert time of the day in factor levels
outcomes$TimeofDay = factor(outcomes$TimeofDay, levels  = c('morning', 'midday', 'evening', 'night'))

# AgeinOutcome column also has age data in different units. Convert all age in days (credit: MeganRisdal)

# get the time value
outcomes$TimeValue = sapply(outcomes$AgeuponOutcome, function(x) strsplit(x, split = ' ')[[1]][1])

# get unit of time
outcomes$UnitofTime = sapply(outcomes$AgeuponOutcome, function(x) strsplit(x, split = ' ')[[1]][2])

# Remove all plural forms of units from the UnitofTime column. (eg. 'years' - 's' = 'year')
outcomes$UnitofTime = gsub('s', '', outcomes$UnitofTime)

# Convert UnitOfTime in factor and TimeValue in numeric
outcomes$UnitofTime = as.factor(outcomes$UnitofTime)
outcomes$TimeValue = as.numeric(outcomes$TimeValue)

# calculate the age of the animal in days by converting TimeValue in days using the appropriate multiplier based on UnitofTime
multiplier = ifelse(outcomes$UnitofTime == 'day', 1, ifelse(outcomes$UnitofTime == 'week', 7, ifelse(outcomes$UnitofTime == 'month', 30, ifelse(outcomes$UnitofTime == 'year', 365, NA))))
outcomes$AgeinDays = multiplier * outcomes$TimeValue

# Replace blank sex with most common after finding the most common one
barplot(table(outcomes$SexuponOutcome))
outcomes$SexuponOutcome = ifelse(nchar(outcomes$SexuponOutcome) == 0, "Neutered Male", outcomes$SexuponOutcome)

# The factors (Breed, Color) have more many unique levels

# Create table showing frequency of each levels occurrence
Breed.table = data.frame(table(outcomes$Breed))
Color.table = data.frame(table(outcomes$Color))
# Order the table in descending order of frequency
Breed.table = Breed.table[order(-Breed.table$Freq),]
Color.table = Color.table[order(-Color.table$Freq),]

# As we want to use randomForest we choose the top 31 levels. So, we leave the top 31 levels unchanged|Get values of the top 31 occuring levels
noChange1 <- Breed.table$Var1[1:31]
noChange2 <- Color.table$Var1[1:31]
# we use 'Other' as factor to avoid overlap w/ other levels (ie if '32' was actually one of the levels). ifelse() checks to see if the factor level is in the list of the top 51 levels. If present it uses it as is, if not it changes it to 'Other'
# The factors (Breed, Color) have more many unique levels
# Modify Breed into 3 categories of sizes
outcomes.size = outcomes
outcomes.size$Breed[grep("Chihuahua",outcomes.size$Breed)] = "Small"
outcomes.size$Breed[grep("Dachshund",outcomes.size$Breed)] = "Small"
outcomes.size$Breed[grep("Miniature Poodle",outcomes.size$Breed)] = "Small"
outcomes.size$Breed[grep("Yorkshire",outcomes.size$Breed)] = "Small"
outcomes.size$Breed[grep("Jack Russell",outcomes.size$Breed)] = "Small"
outcomes.size$Breed[grep("Miniature Schnauzer",outcomes.size$Breed)] = "Small"
outcomes.size$Breed[grep("Rat Terrier",outcomes.size$Breed)] = "Small"
outcomes.size$Breed[grep("Shih Tzu",outcomes.size$Breed)] = "Small" 
outcomes.size$Breed[grep("Cairn Terrier",outcomes.size$Breed)] = "Small" 
outcomes.size$Breed[grep("Miniature Pinscher",outcomes.size$Breed)] = "Small"
outcomes.size$Breed[grep("Cardigan Welsh Corgi",outcomes.size$Breed)] = "Small"
outcomes.size$Breed[grep("Maltese",outcomes.size$Breed)] = "Small"
outcomes.size$Breed[grep("Pug",outcomes.size$Breed)] = "Small"
outcomes.size$Breed[grep("Lhasa Apso",outcomes.size$Breed)] = "Small"
outcomes.size$Breed[grep("Pomeranian",outcomes.size$Breed)] = "Small"
outcomes.size$Breed[grep("Basset Hound",outcomes.size$Breed)] = "Small"
outcomes.size$Breed[grep("Cocker Spaniel",outcomes.size$Breed)] = "Small"
outcomes.size$Breed[grep("Boston Terrier",outcomes.size$Breed)] = "Small"
outcomes.size$Breed[grep("Toy Poodle",outcomes.size$Breed)] = "Small"
outcomes.size$Breed[grep("Border Terrier",outcomes.size$Breed)] = "Small"
outcomes.size$Breed[grep("Pekingese",outcomes.size$Breed)] = "Small"
outcomes.size$Breed[grep("Norfolk Terrier",outcomes.size$Breed)] = "Small"
outcomes.size$Breed[grep("Bruss Griffon",outcomes.size$Breed)] = "Small"
outcomes.size$Breed[grep("Pembroke Welsh Corgi",outcomes.size$Breed)] = "Small" 
outcomes.size$Breed[grep("Parson Russell Terrier",outcomes.size$Breed)] = "Small"
outcomes.size$Breed[grep("Bichon Fris",outcomes.size$Breed)] = "Small"
outcomes.size$Breed[grep("Norwich Terrier",outcomes.size$Breed)] = "Small"
outcomes.size$Breed[grep("West Highland",outcomes.size$Breed)] = "Small"
outcomes.size$Breed[grep("Havanese",outcomes.size$Breed)] = "Small"
outcomes.size$Breed[grep("Wire Hair Fox Terrier",outcomes.size$Breed)] = "Small"
outcomes.size$Breed[grep("Italian Greyhound",outcomes.size$Breed)] = "Small"
outcomes.size$Breed[grep("Papillon",outcomes.size$Breed)] = "Small"
outcomes.size$Breed[grep("Bulldog",outcomes.size$Breed)] = "Small"
outcomes.size$Breed[grep("Cavalier Span",outcomes.size$Breed)] = "Small"
outcomes.size$Breed[grep("Pbgv",outcomes.size$Breed)] = "Small"
outcomes.size$Breed[grep("Tibetan Spaniel",outcomes.size$Breed)] = "Small"
outcomes.size$Breed[grep("Toy Fox Terrier",outcomes.size$Breed)] = "Small"
outcomes.size$Breed[grep("Schipperke",outcomes.size$Breed)] = "Small"
outcomes.size$Breed[grep("Silky Terrier",outcomes.size$Breed)] = "Small"
outcomes.size$Breed[grep("Chinese Crested",outcomes.size$Breed)] = "Small"
outcomes.size$Breed[grep("Podengo Pequeno",outcomes.size$Breed)] = "Small"
outcomes.size$Breed[grep("Scottish Terrier",outcomes.size$Breed)] = "Small"
outcomes.size$Breed[grep("Feist",outcomes.size$Breed)] = "Small"
outcomes.size$Breed[grep("Swedish Vallhund",outcomes.size$Breed)] = "Small"
outcomes.size$Breed[grep("Welsh Terrier",outcomes.size$Breed)] = "Small"
outcomes.size$Breed[grep("Affenpinscher",outcomes.size$Breed)] = "Small" 
outcomes.size$Breed[grep("Australian Terrier",outcomes.size$Breed)] = "Small"
outcomes.size$Breed[grep("Japanese Chin",outcomes.size$Breed)] = "Small"
outcomes.size$Breed[grep("Coton De Tulear",outcomes.size$Breed)] = "Small"
outcomes.size$Breed[grep("Dandie Dinmont",outcomes.size$Breed)] = "Small"
outcomes.size$Breed[grep("Eng Toy Spaniel",outcomes.size$Breed)] = "Small"
outcomes.size$Breed[grep("Lowchen",outcomes.size$Breed)] = "Small"
outcomes.size$Breed[grep("Sealyham Terr",outcomes.size$Breed)] = "Small"

outcomes.size$Breed[grep("Pit Bull",outcomes.size$Breed)] = "Medium"
outcomes.size$Breed[grep("Australian Cattle Dog",outcomes.size$Breed)] = "Medium"
outcomes.size$Breed[grep("Border Collie",outcomes.size$Breed)] = "Medium"
outcomes.size$Breed[grep("Siberian Husky",outcomes.size$Breed)] = "Medium"
outcomes.size$Breed[grep("Beagle",outcomes.size$Breed)] = "Medium"
outcomes.size$Breed[grep("Australian Kelpie",outcomes.size$Breed)] = "Medium"
outcomes.size$Breed[grep("Staffordshire",outcomes.size$Breed)] = "Medium"
outcomes.size$Breed[grep("American Staffordshire Terrier",outcomes.size$Breed)] = "Medium"
outcomes.size$Breed[grep("Pointer",outcomes.size$Breed)] = "Medium"
outcomes.size$Breed[grep("Black Mouth Cur",outcomes.size$Breed)] = "Medium"
outcomes.size$Breed[grep("Chow Chow",outcomes.size$Breed)] = "Medium"
outcomes.size$Breed[grep("Queensland Heeler",outcomes.size$Breed)] = "Medium"
outcomes.size$Breed[grep("Manchester Terrier",outcomes.size$Breed)] = "Medium" 
outcomes.size$Breed[grep("Blue Lacy",outcomes.size$Breed)] = "Medium"
outcomes.size$Breed[grep("Carolina Dog",outcomes.size$Breed)] = "Medium"
outcomes.size$Breed[grep("Bull Terrier",outcomes.size$Breed)] = "Medium"
outcomes.size$Breed[grep("Chinese Sharpei",outcomes.size$Breed)] = "Medium"
outcomes.size$Breed[grep("Collie Smooth",outcomes.size$Breed)] = "Medium"
outcomes.size$Breed[grep("English Bulldog",outcomes.size$Breed)] = "Medium"
outcomes.size$Breed[grep("Soft Coated Wheaten Terrier",outcomes.size$Breed)] = "Medium"
outcomes.size$Breed[grep("Whippet",outcomes.size$Breed)] = "Medium" 
outcomes.size$Breed[grep("Shetland Sheepdog",outcomes.size$Breed)] = "Medium"
outcomes.size$Breed[grep("Shiba Inu",outcomes.size$Breed)] = "Medium"
outcomes.size$Breed[grep("Harrier",outcomes.size$Breed)] = "Medium"
outcomes.size$Breed[grep("Smooth Fox Terrier",outcomes.size$Breed)] = "Medium"
outcomes.size$Breed[grep("American Eskimo",outcomes.size$Breed)] = "Medium"
outcomes.size$Breed[grep("Basenji",outcomes.size$Breed)] = "Medium"
outcomes.size$Breed[grep("Standard Schnauzer",outcomes.size$Breed)] = "Medium"
outcomes.size$Breed[grep("Treeing Walker Coonhound",outcomes.size$Breed)] = "Medium"
outcomes.size$Breed[grep("Collie Rough",outcomes.size$Breed)] = "Medium"
outcomes.size$Breed[grep("Dalmatian",outcomes.size$Breed)] = "Medium"
outcomes.size$Breed[grep("Finnish Spitz",outcomes.size$Breed)] = "Medium"
outcomes.size$Breed[grep("Brittany",outcomes.size$Breed)] = "Medium"
outcomes.size$Breed[grep("English Coonhound",outcomes.size$Breed)] = "Medium"
outcomes.size$Breed[grep("English Springer Spaniel",outcomes.size$Breed)] = "Medium"
outcomes.size$Breed[grep("Tibetan Terrier",outcomes.size$Breed)] = "Medium"
outcomes.size$Breed[grep("Irish Terrier",outcomes.size$Breed)] = "Medium"
outcomes.size$Breed[grep("Patterdale Terr",outcomes.size$Breed)] = "Medium"
outcomes.size$Breed[grep("Keeshond",outcomes.size$Breed)] = "Medium"
outcomes.size$Breed[grep("Treeing Cur",outcomes.size$Breed)] = "Medium"
outcomes.size$Breed[grep("Field Spaniel",outcomes.size$Breed)] = "Medium"
outcomes.size$Breed[grep("Samoyed",outcomes.size$Breed)] = "Medium"
outcomes.size$Breed[grep("Canaan Dog",outcomes.size$Breed)] = "Medium"
outcomes.size$Breed[grep("Glen Of Imaal",outcomes.size$Breed)] = "Medium"
outcomes.size$Breed[grep("Skye Terrier",outcomes.size$Breed)] = "Medium" 
outcomes.size$Breed[grep("Bearded Collie",outcomes.size$Breed)] = "Medium"
outcomes.size$Breed[grep("Bedlington Terr",outcomes.size$Breed)] = "Medium"
outcomes.size$Breed[grep("Boykin Span",outcomes.size$Breed)] = "Medium"
outcomes.size$Breed[grep("German Pinscher",outcomes.size$Breed)] = "Medium"
outcomes.size$Breed[grep("Jindo",outcomes.size$Breed)] = "Medium"
outcomes.size$Breed[grep("Nova Scotia Duck Tolling Retriever",outcomes.size$Breed)] = "Medium"
outcomes.size$Breed[grep("English Setter",outcomes.size$Breed)] = "Medium"
outcomes.size$Breed[grep("Mexican Hairless",outcomes.size$Breed)] = "Medium"
outcomes.size$Breed[grep("Spinone Italiano",outcomes.size$Breed)] = "Medium" 
outcomes.size$Breed[grep("Treeing Tennesse Brindle",outcomes.size$Breed)] = "Medium"
outcomes.size$Breed[grep("Welsh Springer Spaniel",outcomes.size$Breed)] = "Medium"

outcomes.size$Breed[grep("Australian Shepherd",outcomes.size$Breed)] = "Large"
outcomes.size$Breed[grep("Catahoula",outcomes.size$Breed)] = "Large"
outcomes.size$Breed[grep("Labrador",outcomes.size$Breed)] = "Large"
outcomes.size$Breed[grep("German Shepherd",outcomes.size$Breed)] = "Large"
outcomes.size$Breed[grep("Boxer",outcomes.size$Breed)] = "Large"
outcomes.size$Breed[grep("Rottweiler",outcomes.size$Breed)] = "Large"
outcomes.size$Breed[grep("American Bulldog",outcomes.size$Breed)] = "Large"
outcomes.size$Breed[grep("Great Pyrenees",outcomes.size$Breed)] = "Large"
outcomes.size$Breed[grep("Plott Hound",outcomes.size$Breed)] = "Large"
outcomes.size$Breed[grep("Anatol Shepherd",outcomes.size$Breed)] = "Large"
outcomes.size$Breed[grep("Golden Retriever",outcomes.size$Breed)] = "Large"
outcomes.size$Breed[grep("Doberman Pinsch",outcomes.size$Breed)] = "Large"
outcomes.size$Breed[grep("Flat Coat Retriever",outcomes.size$Breed)] = "Large"
outcomes.size$Breed[grep("Mastiff",outcomes.size$Breed)] = "Large" 
outcomes.size$Breed[grep("Rhod Ridgeback",outcomes.size$Breed)] = "Large"
outcomes.size$Breed[grep("Great Dane",outcomes.size$Breed)] = "Large"
outcomes.size$Breed[grep("Redbone Hound",outcomes.size$Breed)] = "Large"
outcomes.size$Breed[grep("Black/Tan Hound",outcomes.size$Breed)] = "Large"
outcomes.size$Breed[grep("Belgian Malinois",outcomes.size$Breed)] = "Large"
outcomes.size$Breed[grep("Weimaraner",outcomes.size$Breed)] = "Large"
outcomes.size$Breed[grep("Dogo Argentino",outcomes.size$Breed)] = "Large"
outcomes.size$Breed[grep("Akita",outcomes.size$Breed)] = "Large"
outcomes.size$Breed[grep("Pharaoh Hound",outcomes.size$Breed)] = "Large"
outcomes.size$Breed[grep("Alaskan Husky",outcomes.size$Breed)] = "Large"
outcomes.size$Breed[grep("Chesa Bay Retr",outcomes.size$Breed)] = "Large"
outcomes.size$Breed[grep("Vizsla",outcomes.size$Breed)] = "Large"
outcomes.size$Breed[grep("Beauceron",outcomes.size$Breed)] = "Large"
outcomes.size$Breed[grep("Greyhound",outcomes.size$Breed)] = "Large"
outcomes.size$Breed[grep("Standard Poodle",outcomes.size$Breed)] = "Large"
outcomes.size$Breed[grep("St. Bernard Smooth Coat",outcomes.size$Breed)] = "Large"
outcomes.size$Breed[grep("Bullmastiff",outcomes.size$Breed)] = "Large"
outcomes.size$Breed[grep("Dutch Shepherd",outcomes.size$Breed)] = "Large"
outcomes.size$Breed[grep("Leonberger",outcomes.size$Breed)] = "Large" 
outcomes.size$Breed[grep("St. Bernard Rough Coat",outcomes.size$Breed)] = "Large"
outcomes.size$Breed[grep("Bluetick Houn",outcomes.size$Breed)] = "Large"
outcomes.size$Breed[grep("Airedale Terrier",outcomes.size$Breed)] = "Large"
outcomes.size$Breed[grep("Bernese Mountain Dog",outcomes.size$Breed)] = "Large"
outcomes.size$Breed[grep("Boerboel",outcomes.size$Breed)] = "Large"
outcomes.size$Breed[grep("English Foxhound",outcomes.size$Breed)] = "Large"
outcomes.size$Breed[grep("Landseer",outcomes.size$Breed)] = "Large"
outcomes.size$Breed[grep("Newfoundland",outcomes.size$Breed)] = "Large"
outcomes.size$Breed[grep("Bloodhound",outcomes.size$Breed)] = "Large"
outcomes.size$Breed[grep("Dogue De Bordeaux",outcomes.size$Breed)] = "Large"
outcomes.size$Breed[grep("Alaskan Malamute",outcomes.size$Breed)] = "Large"
outcomes.size$Breed[grep("Afghan Hound",outcomes.size$Breed)] = "Large"
outcomes.size$Breed[grep("American Foxhound",outcomes.size$Breed)] = "Large"
outcomes.size$Breed[grep("Belgian Sheepdog",outcomes.size$Breed)] = "Large"
outcomes.size$Breed[grep("Cane Corso",outcomes.size$Breed)] = "Large"
outcomes.size$Breed[grep("Schnauzer Giant",outcomes.size$Breed)] = "Large"
outcomes.size$Breed[grep("Greater Swiss Mountain Dog",outcomes.size$Breed)] = "Large" 
outcomes.size$Breed[grep("Presa Canario",outcomes.size$Breed)] = "Large" 
outcomes.size$Breed[grep("Wirehaired Pointing Griffon",outcomes.size$Breed)] = "Large" 
outcomes.size$Breed[grep("Irish Wolfhound",outcomes.size$Breed)] = "Large"
outcomes.size$Breed[grep("Otterhound",outcomes.size$Breed)] = "Large"
outcomes.size$Breed[grep("Picardy Sheepdog",outcomes.size$Breed)] = "Large"
outcomes.size$Breed[grep("Saluki",outcomes.size$Breed)] = "Large"
outcomes.size$Breed[grep("Belgian Tervuren",outcomes.size$Breed)] = "Large"
outcomes.size$Breed[grep("English Shepherd",outcomes.size$Breed)] = "Large"
outcomes.size$Breed[grep("Ibizan Hound",outcomes.size$Breed)] = "Large"
outcomes.size$Breed[grep("Kuvasz",outcomes.size$Breed)] = "Large"
outcomes.size$Breed[grep("Old English Sheepdog",outcomes.size$Breed)] = "Large" 
outcomes.size$Breed[grep("Port Water Dog",outcomes.size$Breed)] = "Large"
outcomes.size$Breed[grep("English Shepherd",outcomes.size$Breed)] = "Large"
outcomes.size$Breed[grep("Entlebucher",outcomes.size$Breed)] = "Large"
outcomes.size$Breed[grep("Gordon Setter",outcomes.size$Breed)] = "Large"
outcomes.size$Breed[grep("Hovawart",outcomes.size$Breed)] = "Large"
outcomes.size$Breed[grep("Norwegian Elkhound",outcomes.size$Breed)] = "Large" 
outcomes.size$Breed[grep("Old English Sheepdog",outcomes.size$Breed)] = "Large"

outcomes.size$Breed[grep("Domestic Shorthair",outcomes.size$Breed)] = "Domestic Shorthair"
outcomes.size$Breed[grep("Domestic Longhair",outcomes.size$Breed)] = "Domestic Shorthair"
outcomes.size$Breed[grep("Domestic Medium Hair",outcomes.size$Breed)] = "Domestic Shorthair"
outcomes.size$Breed[grep("Siamese",outcomes.size$Breed)] = "Domestic Shorthair"
outcomes.size$Breed[grep("Snowshoe",outcomes.size$Breed)] = "Domestic Shorthair"

outcomes$newFactorBreed <- outcomes.size$Breed
outcomes$newFactorColor <- (ifelse(outcomes$Color %in% noChange2, outcomes$Color, "Other"))

# Impute missing age values by replacing the NAs with th mean age
outcomes$AgeinDays = ifelse(is.na(outcomes$AgeinDays), mean(outcomes$AgeinDays, na.rm = T), outcomes$AgeinDays)

# Replace all blank fields in the OutcomeSubType colum with 'Other'
outcomes$OutcomeSubtype = ifelse(nchar(outcomes$OutcomeSubtype) == 0, 'Other', outcomes$OutcomeSubtype)


# Factorize the data for Classification
factorize = c('OutcomeType', 'OutcomeSubtype', 'AnimalType', 'SexuponOutcome', 'AgeuponOutcome', 'newFactorBreed', 'newFactorColor', 'hasName')
outcomes[factorize] <- lapply(outcomes[factorize], function(x) as.factor(x))

# convert Hour into numeric
Hour = as.numeric(outcomes$Hour)

print("Starting Random Forest Classification")

# Start with Random Forest classification: split data set back into its original test and train set

outcome.train = outcomes[1:26729, ]
outcome.test = outcomes[26730:nrow(outcomes), ]

#randomForest  
rf.outcomes = randomForest(OutcomeType ~ 
                             +AnimalType+SexuponOutcome+Hour+Weekday+AgeinDays+newFactorBreed+newFactorColor, 
                           data = outcome.train, 
                           mtry = 3, n.trees = 600,
                           importance = T, node.size = 20)

print("Finished Randon Forest Classification")

rf.pred = predict(rf.outcomes, newdata = outcome.train, type = "vote")

# Plotting relative importance of the variables
plot(rf.outcomes)
varImpPlot(rf.outcomes)

# make predicion on animal outcome
rf.pred.test = predict(rf.outcomes, outcome.test, type = "vote")

# Output file for submission
sol = data.frame(ID = outcome.test$ID, rf.pred.test)
write.csv(sol, file = "submission.rf.csv", row.names = F)
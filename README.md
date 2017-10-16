Read Me for R_Assignment

##Part 1

#The first thing is to download The fang_et_al_genotypes.txt file and snp_position.txt file from the Buffalo repository and assign the file contents to a vector. This can be done with this code:
fang <- read.table("https://raw.githubusercontent.com/EEOB-BioData/BCB546X-Fall2017/master/UNIX_Assignment/fang_et_al_genotypes.txt", sep = "\t", header = TRUE, na.strings = "?/?", stringsAsFactors = FALSE)

###and for the SNP file
SNP <- read.table("https://raw.githubusercontent.com/EEOB-BioData/BCB546X-Fall2017/master/UNIX_Assignment/snp_position.txt", sep = "\t", header = TRUE, na.strings = "?/?", stringsAsFactors = FALSE)
###Make sure to have the arguments sep = "\t", headers = tru, na.strings = "?/?" and stringsAsFactors = FALSE. These will make the files be separated by Tab, keep the headers, change the "?/?" within the files to NA and keep your character from turning to factors.

###Next, we will make sure the dplyr, ggplot2 and reshape2 packages are loaded
library(dplyr)
library(ggplot2)
library(reshape2)
###these provide many of the functions I use later on and will be useful throughout the project.

###next, we will pull out the rows containing the specified groups for maize and teosinte. The pulled rows will be placed in a vector labeled accordingling.
maize <- filter(fang, Group== "ZMMMR" | Group== "ZMMLR" | Group== "ZMMIL")
teosinte <- filter(fang, Group== "ZMPBA" | Group== "ZMPIL" | Group== "ZMPJA")
###in this case, the "|" are considered "and"

###next, we will transpose the vectors that you just made.
transposed_maize <- t(maize)
transposed_teosinte <- t(teosinte)

###the transposition turned the information into a matrix! lets change them back into a dataframe quick.
dftransposed_teosinte <- as.data.frame(transposed_teosinte, stringsAsFactors = FALSE)
dftransposed_maize <- as.data.frame(transposed_maize, stringsAsFactors = FALSE)
###make sure this command doesn't change your data to factors by including the stringsasfactors = false argument.

###now we will merge the snp file to the separated fang vectors you just created.
mergedmaized <- merge.data.frame(SNP, dftransposed_maize, by.x = 1, by.y = 0)
mergedteosinte <- merge.data.frame(SNP, dftransposed_teosinte, by.x = 1, by.y = 0)

###next, we will pull out the columns 1,3,4 (SNP_ID, Chromosome and Position), and place them in that order on the far left side.
orderedmaize1 <- mergedmaized[,c(1,3,4,2,5)]
orderedmaize2 <- mergedmaized[,c(6:1588)]
orderedmaize <- cbind(orderedmaize1, orderedmaize2)

###and again with teosinte
orderedteosinte1 <- mergedteosinte[,c(1,3,4,2,5)]
orderedteosinte2 <- mergedteosinte[,c(6:990)]
orderedteosinte <- cbind(orderedteosinte1,orderedteosinte2)
###this will make it so all of the additional data are placed to the right hand side of the three identifying files.

###the next step will be to replace all of the NA's with either "?" or "-", then place them in arrange them in increasing and decreasing orders, perspectively. this is necessary for the final file set up.
questmaize <- replace(orderedmaize, is.na(orderedmaize), "?")
hyphmaize <- replace(orderedmaize, is.na(orderedmaize), "-")
questteosinte <- replace(orderedteosinte, is.na(orderedteosinte), "?")
hyphteosinte <- replace(orderedteosinte, is.na(orderedteosinte), "-")

###and this is how we are arranging them
hyphmaize$Position <- as.numeric(as.character(hyphmaize$Position))
arrangedhyphmaize <- arrange(hyphmaize, desc(Position))
arrangedhyphmaize$Position <- as.character(as.numeric(arrangedhyphmaize$Position))


questmaize$Position <- as.numeric(as.character(questmaize$Position))
arrangedquestmaize <- arrange(questmaize, Position)
arrangedquestmaize$Position <- as.character(as.numeric(arrangedquestmaize$Position))


questteosinte$Position <- as.numeric(as.character(questteosinte$Position))
arrangedquestteosinte <- arrange(questteosinte, Position)
arrangedquestteosinte$Position <- as.character(as.numeric(arrangedquestteosinte$Position))


hyphteosinte$Position <- as.numeric(as.character(hyphteosinte$Position))
arrangedhyphteosinte <- arrange(hyphteosinte, desc(Position))
arrangedhyphteosinte$Position <- as.character(as.numeric(arrangedhyphteosinte$Position))
###here we are changing the position column to numeric, arranging it either in increasing(no argument) or descending(desc) order. the hyphen files will be descending, and the question mark files will be increaing. we are creating a new vector with the newly arranged data frame, then changing the positions column back to characters.


###this next trick is pretty tricky. the goal is to separate the chromosomes based on numbers 1:10 and shunt them to new files. this will require us to split the data out based on the chromosome column to a new vector, applying a write table function on the different groups in the split vector, and ensuring that each group is comined in a new file.
###------- This will kick out a bunch of NULLs below, just scroll past or click the x ---here|-
Split_maizequest <- split(arrangedquestmaize, arrangedquestmaize$Chromosome)
lapply(names(Split_maizequest), function(x){write.table(Split_maizequest[[x]], file=paste("maizequestchrom",x), sep = "\t", row.names = FALSE)})


Split_teosintequest <- split(arrangedquestteosinte, arrangedquestteosinte$Chromosome)
lapply(names(Split_teosintequest), function(x){write.table(Split_teosintequest[[x]], file=paste("teosintequestchrom",x), sep = "\t", row.names = FALSE)})


Split_maizehyph <- split(arrangedhyphmaize, arrangedhyphmaize$Chromosome)
lapply(names(Split_maizehyph), function(x){write.table(Split_maizehyph[[x]], file=paste("maizehyphtchrom",x), sep = "\t", row.names = FALSE)})


Split_teosintehyph <- split(arrangedhyphteosinte, arrangedhyphteosinte$Chromosome)
lapply(names(Split_teosintehyph), function(x){write.table(Split_teosintehyph[[x]], file=paste("teosintehyphchrom",x), sep = "\t", row.names = FALSE)})
###the arguments in the function write.table say that for every group x, make a new file named by pasting the "name"to x. this file will be separated by tabs and will have no row names included.that function will be applied to all of the named groups in the split vector.

#this will be the end of part 1


##part 2

###to make plots of the number of snps for each chromosome, I will use these commands.
qm_Chrome <-as.data.frame(table(questmaize$Chromosome))
ggplot(qm_Chrome, aes(x = Var1, y = Freq)) + geom_bar(stat = "identity")

###and the next one...
qt_Chrome <-as.data.frame(table(questteosinte$Chromosome))
ggplot(qt_Chrome, aes(x = Var1, y = Freq)) + geom_bar(stat = "identity")
###I am taking the table of the chromosome column, converting it to a data frame and placing it in a new vector. then I am passing that vector to a ggplot function that makes a bar graph based on the designated x variable (Var1) and y variable (Freq). I use the argument stat = "identity" in the geom_bar function to ensure that the x values correspond with the number 1:10 specified in the Chromosome column.



or
melt_maize <- melt(questmaize)
qmmelt_Chrome <-as.data.frame(table(melt_maize$Chromosome))
ggplot(qmmelt_Chrome, aes(x = Var1, y = Freq)) + geom_bar(stat = "identity")


melt_teosinte <- melt(questteosinte)
qtmelt_Chrome <-as.data.frame(table(melt_teosinte$Chromosome))
ggplot(qtmelt_Chrome, aes(x = Var1, y = Freq)) + geom_bar(stat = "identity")
###I didn't use this command because I was not 100% sure what melt was doing. I just stuck with what was above.


### I also did not complete the "missing data and hetereozyogsity amount" part, Sorry

###I then made a new plot that shows which snps are present in which chromosome
qt_SNP <-as.data.frame(table(questteosinte$SNP_ID, questteosinte$Chromosome))
ggplot(qt_SNP, aes(x = Var1, y = Freq)) + geom_point(stat = "identity")
###its unreadable...

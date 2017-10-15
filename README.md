Read Me for R_Assignment

fang <- read.table("https://raw.githubusercontent.com/EEOB-BioData/BCB546X-Fall2017/master/UNIX_Assignment/fang_et_al_genotypes.txt", sep = "\t", header = TRUE, na.strings = "?/?", stringsAsFactors = FALSE)

SNP <- read.table("https://raw.githubusercontent.com/EEOB-BioData/BCB546X-Fall2017/master/UNIX_Assignment/snp_position.txt", sep = "\t", header = TRUE, na.strings = "?/?", stringsAsFactors = FALSE)

library(dplyr)

maize <- filter(fang, Group== "ZMMMR" | Group== "ZMMLR" | Group== "ZMMIL")

teosinte <- filter(fang, Group== "ZMPBA" | Group== "ZMPIL" | Group== "ZMPJA")

transposed_maize <- t(maize)

transposed_teosinte <- t(teosinte)

dftransposed_teosinte <- as.data.frame(transposed_teosinte, stringsAsFactors = FALSE)

dftransposed_maize <- as.data.frame(transposed_maize, stringsAsFactors = FALSE)

mergedmaized <- merge.data.frame(SNP, dftransposed_maize, by.x = 1, by.y = 0)

mergedteosinte <- merge.data.frame(SNP, dftransposed_teosinte, by.x = 1, by.y = 0)

orderedmaize1 <- mergedmaized[,c(1,3,4,2,5)]

orderedmaize2 <- mergedmaized[,c(6:1588)]

cbind(orderedmaize1, orderedmaize2)

orderedteosinte1 <- mergedteosinte[,c(1,3,4,2,5)]

orderedteosinte1 <- mergedteosinte[,c(1,3,4,2,5)]

orderedteosinte2 <- mergedteosinte[,c(6:990)]

cbind(orderedteosinte1,orderedteosinte2)

questmaize <- replace(orderedmaize, is.na(orderedmaize), "?")

hyphmaize <- replace(orderedmaize, is.na(orderedmaize), "-")

questteosinte <- replace(orderedteosinte, is.na(orderedteosinte), "?")

hyphteosinte <- replace(orderedteosinte, is.na(orderedteosinte), "-")

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

Split_maizequest <- split(arrangedquestmaize, arrangedquestmaize$Chromosome)
lapply(names(Split_maizequest), function(x){write.table(Split_maizequest[[x]], file=paste("maizequestchrom",x), sep = "\t", row.names = FALSE)})

Split_teosintequest <- split(arrangedquestteosinte, arrangedquestteosinte$Chromosome)
lapply(names(Split_teosintequest), function(x){write.table(Split_teosintequest[[x]], file=paste("teosintequestchrom",x), sep = "\t", row.names = FALSE)})

Split_maizehyph <- split(arrangedhyphmaize, arrangedhyphmaize$Chromosome)
lapply(names(Split_maizehyph), function(x){write.table(Split_maizehyph[[x]], file=paste("maizehyphtchrom",x), sep = "\t", row.names = FALSE)})

Split_teosintehyph <- split(arrangedhyphteosinte, arrangedhyphteosinte$Chromosome)
lapply(names(Split_teosintehyph), function(x){write.table(Split_teosintehyph[[x]], file=paste("teosintehyphchrom",x), sep = "\t", row.names = FALSE)})



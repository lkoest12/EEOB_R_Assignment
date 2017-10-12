Read Me for R_Assignment

fang <- read.table("https://raw.githubusercontent.com/EEOB-BioData/BCB546X-Fall2017/master/UNIX_Assignment/fang_et_al_genotypes.txt", sep = "\t", header = TRUE, na.strings = "?/?", stringsAsFactors = FALSE)

SNP <- read.table("https://raw.githubusercontent.com/EEOB-BioData/BCB546X-Fall2017/master/UNIX_Assignment/snp_position.txt", sep = "\t", header = TRUE, na.strings = "?/?", stringsAsFactors = FALSE)

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



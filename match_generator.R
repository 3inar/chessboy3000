players <- c(
 "Einar",
 "Edvard",
 "Lars A",
 "Vi",
 "Anders Sildnes",
 "Rigmor",
 "Maisha",
 "Solveig",
 "Binod",
 "Rafael",
 "Dominik",
 "Bjørn",
 "Mohammad",
 "Mohsen",
 "Adam",
 "Muhammad",
 "Nikita",
 "Per Niklas",
 "Pavitra",
 "Masoud",
 "Thomas",
 "Zulfiqar",
 "Ayoung",
 "Anders Søreide",
 "Morten",
 "Andrew",
 "Aakash",
 "Belal"
)

players <- unique(players)
matches <- plyr::raply(100, sample(players,2,replace=F))

for (i in 1:nrow(matches)) writeLines(paste(matches[i, 1], "-", matches[i, 2]))



cards <- read.table(file=file.choose(), header = TRUE, sep = ",")

campaign <- read.table(file=file.choose(), header = TRUE, sep = ",")

areValuesExisted(campaign$CardID, cards$CardID)

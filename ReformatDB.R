

DB=read.table(file="/home/tobal/RFiles/Shiny/DAS/DB/MetabolitesDB.txt", header=TRUE, sep="\t")
DB$shape=NULL
DF=data.frame("type"=c(as.character(unique(DB$type)), NA))
DF$shape=c(15, 16, 17, 18, 19, 25, 23, 4, 10, 13, 7, 1)
new=merge(x=DB, y=DF, by="type")

write.table(x=new, file="/home/tobal/RFiles/Shiny/DAS/DB/MetabolitesDB.txt", quote=FALSE, sep="\t", row.names=FALSE, col.names=TRUE)

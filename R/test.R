#PROBAR

dbGetQuery(con , "ALTER TABLE personas ADD vive int")
dbGetQuery(con , "ALTER TABLE personas ADD brca int")
dbGetQuery(con , "ALTER TABLE personas ADD p53 int")
dbGetQuery(con , "ALTER TABLE personas ADD cancer int")

dbGetQuery(con, "DROP TABLE personas;")
dbGetQuery(con, "DROP TABLE pedigrees;")

dbGetQuery(con, "CREATE TABLE personas
(
expediente varchar(255),
nombre varchar(255),
apellido varchar(255),
sexo varchar(255),
vive int,
padre_id int,
madre_id int,
pedigree_id int,
ref int,
FOREIGN KEY(pedigree_id) REFERENCES pedigrees(rowid)
);")

dbGetQuery(con, "CREATE TABLE pedigrees (
           probate_id int,
           FOREIGN KEY(probate_id) REFERENCES personas(rowid));")

nuevo_pedigree("expediente1")


dbGetQuery(con, "SELECT * FROM personas")
dbGetQuery(con, "SELECT * FROM pedigrees")

dbGetQuery(con, "UPDATE personas SET nombre = 'Juan', apellido='Perez', sexo='H' WHERE rowid=1 ")
dbGetQuery(con, "UPDATE personas SET vive = 0, brca=1, p53 = 0 WHERE rowid = 1 ")
dbGetQuery(con, "UPDATE personas SET nombre = 'Jose', apellido='Perez', sexo='H' WHERE rowid=2 ")
dbGetQuery(con, "UPDATE personas SET vive = 1 WHERE rowid = 2 ")
dbGetQuery(con, "UPDATE personas SET nombre = 'María', apellido='Gomez', sexo='M' WHERE rowid=3 ")
dbGetQuery(con, "UPDATE personas SET vive = 0 WHERE rowid = 3 ")



agrega_padre(2)
agrega_madre(2)

agrega_padre(4)
agrega_madre(4)

agrega_padre(3)
agrega_madre(3)

dbGetQuery(con, "SELECT * FROM personas")-> test.ped1
test.ped1 <- test.ped1[-16,]

test.ped1$sexo <- ifelse(test.ped1$sexo == "H", 1, 2)
test.ped1$vive <- 0
a<-rbinom(45, size=1, p=1/4)
dim(a)<-c(15,3)
colnames(a) <- c("BRCA","P53","CANCER")


ped <- kinship2:: pedigree(id = rownames(test.ped1), 
                           dadid = test.ped1$padre_id,
                           momid = test.ped1$madre_id,
                           sex = test.ped1$sexo,
                           famid = test.ped1$pedigree_id,
                           status = test.ped1$vive,
                           affected = a
                           )



plot(ped[1], col=c("red3", rep("black",dim(ped[1])-1)))
kinship2::pedigree.legend(ped, location="topright", radius=.3, cex=0.5)
all(is.na(c(NA,1)))

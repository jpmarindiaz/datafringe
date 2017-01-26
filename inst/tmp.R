library(datafringe)

df <- data_frame(`Nombre_Completo` = c("Pepito Perdomo",
                                       "Pepito Pérez",
                                       "Sutanito Perenzejo",
                                       "Cosiánfilo Gómez",
                                       "Lolita Ramírez"),
                 `Edad` = c(3,4,5,6,7),
                 `Sexo` = c("M", "M", "M", "M", "F"),
                 `País` = c("Colombia", "Colombia", "Colombia", "Colombia", "Colombia"))


names <- names(df)
names_to_change <- c("complete_name", "age", "gender", "country")

#Shuffle dic
names_to_change <- data_frame(label = names, id = names_to_change)
names(df) <- names_to_change$id

names_to_change <- sample_n(names_to_change, 4)

dic <- names_to_change
dic_ctype <- dic
dic_ctype$ctype <- datafringe::guessCtypes(df[,rev(dic$id)])


f1 <- fringe(df,dic = dic_ctype)



write_csv(df, "~/Desktop/data/hola_data.csv")
write_csv(dic, "~/Desktop/data/hola_dic.csv")
write_csv(dic_ctype, "~/Desktop/data/hola_dic_.csv")

#se copian los mismos archivos en la carpeta fringe sin _dic

write_csv(df, "~/Desktop/fringe/hola_data.csv")
write_csv(dic_ctype, "~/Desktop/fringe/hola_dic_.csv")

df_fringe <- data.frame(id = "hola", label = "Prueba", withDic = TRUE, exclude = FALSE)
write_csv(df_fringe, "~/Desktop/fringe/fringe_idx.csv")

###### Ya se tienen los datos ######

library(datafringe)

fringes_path <- "~/Desktop/fringe"

files <- list.files(path = fringes_path, recursive = FALSE, full.names = FALSE, pattern = "(.*)_data.csv")
write(gsub("\\_data.csv", "", files), "")

list_fringes(fringes_path)

frs <- load_fringes(fringes_path)

write_fpkg_sqlite(fringes_path, "~/Desktop/fringe/db.sqlite3", fringe_idx = "~/Desktop/fringe/fringe_idx.csv")

####### Se cargan los datos ########

db <- src_sqlite("~/Desktop/fringe/db.sqlite3")
src_tbls(db)
selectedTable <- "hola"
f0 <- readFringeSqlite(selectedTable,db)
data <- f0$data






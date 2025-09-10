library(parquetize)
library(arrow)


filesloc<-getwd()

files<-list.files(filesloc,
                  full.names = TRUE,
                  pattern=".csv",
                  recursive=TRUE)

for (i in seq_along(files)){
  csv<- files[i]
  parquet<- gsub(".csv",".parquet",csv)
  csv_to_parquet(csv,path_to_parquet = parquet)
}



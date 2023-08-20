library(pdftools)
library(tabulizer)
library(tidyverse)
library(stringr)
library(openxlsx)


#extract and store document
filename<-'Californina EPA Known Carcinogens.pdf'
extracted<-extract_tables(filename, output = 'character')
saveRDS(extracted, 'EPAKnownCarcinogens.RDS')

#retrieve tables and compile them
tables <- readRDS('EPAKnownCarcinogens.RDS')


number_of_columns<-4
merged_tables <- data.frame()
for (p in 1:length(tables)) {
  #for each page 
  temp_page<-unlist(tables[p])
  splitpage<-unlist(strsplit(temp_page,'\\t|\\n'))
  #Get number of rows based on 4 columns
  min_rows<-floor(length(splitpage)/number_of_columns)
  #for every row, set a counter 
  counter = 0
  temp_ds<-data.frame()
  for(k in 1:min_rows){
    for(column in 1:number_of_columns){
      counter = counter+1
      print(counter)
      temp_ds[k,column]<-gsub('\\r',' ',splitpage[counter])
    }
  }
  
  merged_tables <- bind_rows(merged_tables, temp_ds)
}

#Adding headers
headers<-c('Chemical', 'Type of Toxicity', 'CAS No.', 'Date Listed')
names(merged_tables)<-headers

#Cleaning up table
modified_df<-merged_tables

##removing empty lines
### Remove rows with only empty cells
modified_df1<-modified_df[!apply(modified_df == '',1,all),] 
### Remove rows with only NAs
modified_df2<-modified_df1[rowSums(is.na(modified_df1)) !=ncol(modified_df1),] 
##adding a temporary index column
modified_df2$Index<-1:nrow(modified_df2)

#Corrections 
modified_df3<-modified_df2
for( r in 1:nrow(modified_df2)){
  temp_row<-modified_df2[r,]
  
  #Multiple dates
  if(grepl('Date',modified_df2[r,'Chemical'])){
    print(r)
    temp_row$`Date Listed`[1]<-temp_row$Chemical[1]
    temp_row$Chemical[1]<-NA
  }
  modified_df3[r,]<-temp_row[1,]
}

#entries spanning on multiple lines
multi_name_index<-c()
multi_tox_index<-c()
multi_cas_index<-c()
multi_date_index<-c()

for(r in 1:nrow(modified_df3)){
  
}

modified_df3[which]


final_df<-modified_df
openxlsx::write.xlsx(final_df, 'California P65 EPA Known Carcinogens.xlsx')


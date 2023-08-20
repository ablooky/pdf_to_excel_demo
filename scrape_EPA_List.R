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
    temp_row$Chemical[1]<-''
    temp_row$`Type of Toxicity`[1]<- ''
    temp_row$`CAS No.`[1] <-''
  }
  modified_df3[r,]<-temp_row[1,]
}

#entries spanning on multiple lines
multi_name_index<-c()
multi_tox_index<-c()
multi_cas_index<-c()
multi_date_index<-c()

for(r in 1:nrow(modified_df3)){
  print(r)
  if(modified_df3$Chemical[r] != ''  && 
     modified_df3$`Type of Toxicity`[r] == '' &&
     modified_df3$`CAS No.`[r] == '' && 
     modified_df3$`Date Listed`[r]== ''){
    multi_name_index<-c(multi_name_index,r)
  }
  if(modified_df3$Chemical[r] == ''  && 
     modified_df3$`Type of Toxicity`[r] != '' &&
     modified_df3$`CAS No.`[r] == '' && 
     modified_df3$`Date Listed`[r]== ''){
    multi_tox_index<-c(multi_tox_index,r)
  }
  if(modified_df3$Chemical[r] == ''  && 
     modified_df3$`Type of Toxicity`[r] == '' &&
     modified_df3$`CAS No.`[r] != '' && 
     modified_df3$`Date Listed`[r] == ''){
    multi_cas_index<-c(multi_cas_index,r)
  }
  if(modified_df3$Chemical[r] == ''  && 
     modified_df3$`Type of Toxicity`[r] == '' &&
     modified_df3$`CAS No.`[r] == '' && 
     modified_df3$`Date Listed`[r] != ''){
    multi_date_index<-c( multi_date_index,r)
  }
}

#Hard coding indexes
multi_name_index<-c(169,586,588, multi_name_index)
multi_tox_index<-c(749,multi_tox_index)
multi_cas_index<-c(749,multi_cas_index)
multi_date_index<-c(749, multi_date_index)

#merging lines according to indexes

merged_index<-sort(unique(c(multi_cas_index,multi_date_index,multi_name_index,
               multi_tox_index)))

modified_df4<-data.frame()
for(r in 1:nrow(modified_df3)){
 
  if(r %in% merged_index){
    #skip
  } else {
    print(r)
    temp_row<-modified_df3[r,]
    if((r+1) %in% multi_name_index){
      temp_row$Chemical[1]<-paste(modified_df3[r,'Chemical'],
                                  modified_df3[r+1,'Chemical'],
                                  sep = ' ')
    } 
    if((r+1) %in% multi_tox_index){
      temp_row$`Type of Toxicity`[1]<-paste(modified_df3$`Type of Toxicity`[r],
                                            modified_df3[r+1,'Type of Toxicity'],
                                            sep = ' ')
    }
    if((r+1) %in% multi_cas_index){
      temp_row$`CAS No.`[1]<-paste(modified_df3[r,'CAS No.'],
                                   modified_df3[r+1,'CAS No.'],
                                   sep = '; ')
    }
    if((r+1) %in% multi_date_index){
      temp_row$`Date Listed`[1]<-paste(modified_df3[r,'Date Listed'],
                                       modified_df3[r+1,'Date Listed'],
                                       sep = '; ')
    }
    
    
    modified_df4<-bind_rows(modified_df4,temp_row)
    
  }
  
  
}


final_df<-modified_df4

#Exporting document
wb <- createWorkbook()
addWorksheet(wb, 'Chemicals List')
writeDataTable(wb, 1, x = final_df, 
               tableStyle = "TableStyleMedium7")

saveWorkbook(wb, file = "California P65 EPA Known Carcinogens.xlsx", overwrite = TRUE)

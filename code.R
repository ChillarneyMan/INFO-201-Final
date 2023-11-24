library('tidyverse')
library('janitor')
income <- read.csv('Census_Median_Income.csv')
school <- read.csv('Report_Card_Growth_from_2014-15_to_Current_Year_20231026.csv')
location <- read.csv('Washington_School_District_Boundaries.csv') #gotten from https://data-wutc.opendata.arcgis.com/datasets/7318c4e2a34f4515b721d11cdf91f08a/explore
zip_codes <- select(location, c('LEAName', 'PhysicalAddress'))
get_zip <- function(address){
  new_string <- substr(address, nchar(address) - 9, nchar(address)-5)
  return(as.integer(new_string))
}
get_zip_alt <- function(address){
  new_string_alt <- substr(address, nchar(address) - 4, nchar(address))
  return(as.integer(new_string_alt))
}

for (i in 1:length(zip_codes[, 'PhysicalAddress'])){
  zip_codes[i, 2] <- get_zip(location[i, 'PhysicalAddress'])
  if(is.na(zip_codes[i, 2])){
    zip_codes[i,2] <- get_zip_alt(location[i, 'PhysicalAddress'])
  }
}

income <- row_to_names(income, row_number = 1)
for (i in 1:length(income[,'Geographic Area Name'])){
  income[i, 2] <- get_zip_alt(income[i, 2])
}

district <- school[school$OrganizationLevel == 'District',]
district <- district[district$GradeLevel == 'All Grades', ]

update_school_year <- function(year){
  new_str <- substr(year, nchar(year) -1, nchar(year))
  return(as.integer(new_str) + 2000)
}

for (i in 1:length(district[,'SchoolYear'])){
  district[i,1] <- update_school_year(district[i,1])
}

district_zip <- left_join(district, zip_codes, c('DistrictName' = 'LEAName'))

income <- select(income, c('Geographic Area Name', Median_Household_Income = 'Estimate!!Median income (dollars)!!HOUSEHOLD INCOME BY RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!Households'))

income <- income[income$Median_Household_Income != '-',]

income[,'Median_Household_Income'] <- as.integer(income[,"Median_Household_Income"])

new_data_frame <- left_join(district_zip, income, c('PhysicalAddress' = 'Geographic Area Name'))
new_data_frame <- select(new_data_frame, -c('ESDName', 'ESDOrganizationID', 'DistrictOrganizationId', 'SchoolCode', 'SchoolName', 'SchoolOrganizationid', 'CurrentSchoolType', 'GradeLevel', 'Suppression', 'DataAsOf'))

summary(new_data_frame)

cleaned_data <- new_data_frame[!is.na(new_data_frame[,'Median_Household_Income']), ] #Our analysis is based on income, so lack of income data is useless to us

cleaned_data <- cleaned_data[cleaned_data$StudentGroup == 'All Students', ]
cleaned_data <- na.omit(cleaned_data)
summary(cleaned_data)

write.csv(cleaned_data, 'CleanedDataOfAllStudents.csv', row.names = FALSE)

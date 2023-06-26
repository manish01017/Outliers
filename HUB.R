#Importing Libraries
install.packages("dplyr")
install.packages("tidyverse")
install.packages("caret")
install.packages("janitor")
install.packages("mice")
install.packages("ggplot2")
install.packages("forcats")

library(forcats)

library(tidyverse)


library(dplyr)

library(caret)
library(janitor)
library(mice)
library(ggplot2)
# Reading the file


getwd()

setwd("/Users/sheetalsanwal/Downloads/")

na_strings <- c("", " ", "NA", "N/A", "missing")

pl_data<- read.csv("policy_data.csv",na.strings=na_strings,blank.lines.skip = FALSE)


#Checking data
nrow(pl_data)

summary(pl_data)


# Find and Remove duplicates

dups <- pl_data[duplicated(pl_data),]

#file containing duplicates
print(dups)

#No duplicates Found

# Check for missing values in the dataset
p<- function(x)  {sum(is.na(x))}
apply(pl_data,2,p)


#Renaming Columns
pl_data <- pl_data %>%
  rename(Customer_ID=Cust_ID,
         Policy_Id=Pol_polidx,
         Pol_st_date=Effective,
         Pol_exp_date=Expires,
         Pol_buy_date=Fwritten ,
         Pol_type=LOB,
         Pol_sub_type=Pol_type, 
         Postal_code=Postal ,
         Age_as_of_policy=Age_FirstWritten,
         Min_Continued_Ins=Min_Continued_Ins,
         Gender=Gender,
         Marital_status=Maritalsta,
         Driver_Count=Driver_Count,
         Vehicle_Count=Vehicle_Count,
         Years_Licenced=Years_Licensed_at_FW,
         Lic_susp_6yr=Licsusp6yr,
         Licsusp=Licsusp,
         Insurance_Cancellation_3yr=Inscancl3y,
         Misrepresented=Matmisrepr,
         FirstTerm_Premium=FirstTerm_Premium,
         SecondTerm_Premium=SecondTerm_Premium,
         ThirdTerm_Premium=ThirdTerm_Premium
        )




nrow(pl_data)


# making the Fwritten value (DD-MM) equal to the Effective date (MM-DD)values if they are not equal and dropping missing rows of Gender and Marital Status and dropping column Licsusp 
pl_data <- pl_data %>% 
  mutate ( Pol_buy_date=ifelse( substr(Pol_buy_date,start=1,stop=6)!=substr(Pol_st_date,start=1,stop=6) ,
                            paste0(substr(Pol_st_date, start = 1, stop = 6), substr(Pol_buy_date, start = 7, stop = nchar(Pol_buy_date))),Pol_buy_date)) %>% 
  mutate(Pol_buy_date=ifelse(is.na(Pol_buy_date),Pol_st_date,Pol_buy_date)) %>% drop_na(Gender) %>% drop_na(Marital_status) %>% select(-Licsusp)

###########


# Updating pol_type to Auto for this record from Property
#There is one record in the dataset with policytype as 'Property' but it's pol_sub_type was AUTC
#which was an error. Updating the pol_type to Property for this record.
pl_data <- pl_data %>% 
  mutate(Pol_type = ifelse(Pol_sub_type == "AUTC", "Auto", Pol_type))




#Removing Negative values for Age_as_of_policy and Years_Licenced
#filter Gender 'X' (1 row)
#Dropping Min_Continued_Ins column as it has 238261 missing values 
pl_data <- pl_data %>% filter(Gender %in% c('M','F')) %>% 
  filter (Years_Licenced >= 0 | is.na(Years_Licenced)) %>% filter(Age_as_of_policy>0 | is.na(Age_as_of_policy)) %>% 
  select(-Min_Continued_Ins)
##########################################
nrow(pl_data)
####################################
names(pl_data)




#Imputing NA  for where  Policy_Type = Property and values are present for Insurance_Cancellation_3yr
#Years_Licenced & Misrepresented & Vehicle Count and Driver Count and LIc_susp_6_yr as they are specifically for Auto Policy
# Dropping 1015 rows where polcy_type ="Auto and  NA values are present for Insurance cancellation_3yr or Misrepresented.


pl_data_tst <- pl_data %>%   filter(!(Pol_type=='Auto' &  (is.na(Insurance_Cancellation_3yr)| is.na(Misrepresented)))) %>% 
  mutate(Years_Licenced = ifelse(Pol_type == "Property" , NA, Years_Licenced),
         Insurance_Cancellation_3yr = ifelse(Pol_type == "Property", NA,Insurance_Cancellation_3yr),
         Misrepresented = ifelse(Pol_type == "Property", NA, Misrepresented),
         Vehicle_Count = if_else(Pol_type == "Property", NA, Vehicle_Count),
         Driver_Count = if_else(Pol_type == "Property", NA, Driver_Count),
         Years_Licenced = if_else(Pol_type == "Property", NA, Years_Licenced),
         Lic_susp_6yr = if_else(Pol_type == "Property", NA, Lic_susp_6yr)
         )
  
  nrow(pl_data_tst)
  


# Checking the dataset

summary(pl_data_tst)

p<- function(x)  {sum(is.na(x))}
apply(pl_data_tst,2,p)


print(pl_data_tst)

 sum(pl_data_tst$Pol_type == "Property", na.rm = TRUE) 
 #Sum of property records: 96785
 
 sum(pl_data_tst$Pol_type == "Auto", na.rm = TRUE) 
 #Sum of Auto records: 167686

 #  copying column Pol_buy_date to Pol_buy_date_orig
 pl_data_tst <- pl_data_tst %>% mutate(Pol_buy_date_orig=Pol_buy_date) 
 
 
 # Converting Policy date columns to date
 pl_data_tst$Pol_buy_date <- as.Date(pl_data_tst$Pol_buy_date, format = "%d-%b-%y")
 pl_data_tst$Pol_exp_date<- as.Date(pl_data_tst$Pol_exp_date, format = "%d-%b-%y")
 pl_data_tst$Pol_st_date <- as.Date(pl_data_tst$Pol_st_date, format = "%d-%b-%y")
 
 ## creating a new data frame where pol_buy_date is null (Since we equated the day and month of the both the dates (Effective and Pol_buy_date), For cases where Pol_buy_date
 # was in February month , it populated more than 28 days to those months and when we converted the column to date, it gave us null values)
 #Below code handles the 12 rows which became null due to this conversion and populates the correct no of days for Feb month)
 pl_data_tst2 <- pl_data_tst %>% filter(is.na(Pol_buy_date))
   
  

 ##updating dates where pol_buy_date is null by using leap year calculations
 #It's updating the feb month dates to 29 if the year is leap year and 28 if it's not a leap year.
 pl_data_tst2 <- pl_data_tst2 %>%
   mutate(
     day = substr(Pol_buy_date_orig, 1, 2),
     year = as.integer(substr(Pol_buy_date_orig, start = 8, stop = nchar(Pol_buy_date_orig))),
     new_date = ifelse(leap_year(year) & substr(Pol_buy_date_orig, 4, 6) == "Feb", 
                       paste0("29", substr(Pol_buy_date_orig, start = 3, stop = nchar(Pol_buy_date_orig))),
                       ifelse(substr(Pol_buy_date_orig, 4, 6) == "Feb",
                              paste0("28", substr(Pol_buy_date_orig, start = 3, stop = nchar(Pol_buy_date_orig))),
                              Pol_buy_date_orig)
     ),
     Pol_buy_date = as.Date(new_date, format = "%d-%b-%y")
   )
 
 #Creating new data frame to merge with the original data frame
 pl_data_tst2 <- pl_data_tst2 %>% select(Customer_ID,Policy_Id,Pol_st_date,new_date)
 
 #Merging Data frame,
pl_data_tst <- pl_data_tst %>% 
  left_join(pl_data_tst2, by = c("Customer_ID", "Policy_Id","Pol_st_date")) %>%
  mutate(Pol_buy_date = if_else(is.na(Pol_buy_date), as.Date(new_date, format = "%d-%b-%y"), Pol_buy_date))

#Checking the Final Dataset

str(pl_data_tst)

# removing columns Pol_buy_date_orig and new date from the merged data frame
pl_data_tst <- pl_data_tst %>% select (-Pol_buy_date_orig,-new_date)

str(pl_data_tst)

nrow(pl_data_tst)

#CLEAN VERSION 1 DATA
pl_data_tst

# checking missing values again
p<- function(x)  {sum(is.na(x))}
apply(pl_data_tst,2,p)

  
 
 # Populating Policy_Status column which gives an idea whether the customer is still with the firm or left (cancelled the policy)
 # Creating another column Policy_Active_Exp_Dt which will show the 2023-04-05 as date for the customers still having the policy
 
pl_data_tst <- pl_data_tst %>%
   mutate(Policy_Status = ifelse(Pol_exp_date <= as.Date("2023-04-05"), "Cancelled", "Active")) %>% 
   mutate(Policy_Active_Exp_Dt = if_else(Pol_exp_date >= as.Date("2023-04-05"), as.Date("2023-04-05"), Pol_exp_date))
 
pl_data_clean <-  pl_data_tst

str(pl_data_clean)
 

########################################################IN PROGRESS##########################

 #Caculation of Pro rata Premium Calculation
 library(lubridate) 
 
 
 #Create Data frame with the date range

  date_range <-  data.frame(pl_data_clean$Pol_buy_date,pl_data_clean$Policy_Active_Exp_Dt)
 


#Iterate over each row in the data frame
 diff_days <- numeric(nrow(date_range))  # Initialize diff_days vector
 
 for (i in 1:nrow(date_range)) {
   start_date <- date_range[i, "pl_data_clean.Pol_buy_date"]
   end_date <- date_range[i, "pl_data_clean.Policy_Active_Exp_Dt"]
   
   # Calculate the difference between dates in days
   diff_days[i] <- as.numeric(difftime(end_date, start_date, units = "days"))
   
   # Calculate the number of leap years within the date range
   leap_years <- sum(leap_year(seq(year(start_date), year(end_date))))
   
   # Add the number of leap years to the difference in days
   diff_days[i] <- diff_days[i] + leap_years
 }
 
 diff_days
 
 #Adding  No_of_days column to the main data frame
 pl_data_clean$No_of_days <- diff_days
 
 #Removing 125 rows where days are negative , error due to wrong dates inputted.
pl_data_clean <- pl_data_clean %>% filter(No_of_days>=0) 

#Final Clean data
nrow(pl_data_clean)


#Calculating No of Years by dividing no of days by 365
pl_data_clean <- pl_data_clean %>% mutate(policies=round(No_of_days/365,digits=2))

#Final data

str(pl_data_clean)



# Remove rows with NA in all three terms (Dropping 1308 rows)
  pl_data_clean <- pl_data_clean %>%
    filter(!(is.na(FirstTerm_Premium) & is.na(SecondTerm_Premium) & is.na(ThirdTerm_Premium)))
  
  
  #Put value of Third Term Premium to second Term Premium in case second Term Premium is blank
  pl_data_clean <- pl_data_clean %>%
    mutate(SecondTerm_Premium = if_else( (is.na(SecondTerm_Premium) & !is.na(ThirdTerm_Premium)), ThirdTerm_Premium, SecondTerm_Premium))
  
 
   #Put value of  Second  Term Premium to First Term Premium in case First Term Premium is blank
  pl_data_clean <- pl_data_clean %>%
    mutate(FirstTerm_Premium = if_else( (is.na(FirstTerm_Premium) & !is.na(SecondTerm_Premium)), SecondTerm_Premium, FirstTerm_Premium))
  
  
#Filter Negative Values of First Premium 11 values
  pl_data_clean <- pl_data_clean %>% filter ( FirstTerm_Premium>0)
  



#the final table
nrow(pl_data_clean)


#Dropping Postal Code 44 rows
pl_data_clean<- pl_data_clean %>% drop_na(Postal_code)

# Number of Missing Values
p<- function(x)  {sum(is.na(x))}
apply(pl_data_fin_tr,2,p)


#Using predictive mean matching of mice package to impute 1263 values in Driver Count and Vehicle Count and 9914 values for Auto Policies

imputed_data <- mice(pl_data_clean,m=1,method='pmm',maxit = 20)

print(imputed_data)

#analysing imputed data sets
imputed_data$imp$Vehicle_Count

imputed_data$imp$Driver_Count

imputed_data$imp$Years_Licenced


#using plots to validate the imputed sets.
densityplot(imputed_data,~Years_Licenced)


densityplot(imputed_data,~Vehicle_Count)

densityplot(imputed_data,~Driver_Count)


#Checking the imputed data wrt to the correlation between Years_Licenced and Age_as_of_policy
#Check out the red dots for the imputed data in the graph (It will take some time to load..)
xyplot(imputed_data , Years_Licenced~Age_as_of_policy)

#Selecting the third Dataset as it's close to the mean of the column
final_clean_ds <- complete(imputed_data)

type(final_clean_ds)





#Renaming columns to prepare for joining
final_clean_ds <- final_clean_ds  %>% select (Years_Licenced,Vehicle_Count,Driver_Count) %>% 
  rename(Licenced_Age=Years_Licenced,
         Count_Vehicle=Vehicle_Count,
         Count_Driver=Driver_Count
  )

#Merging imputed Dataset with final dataset on the basis of columns
final_data_set<- cbind(pl_data_clean,final_clean_ds)

rm(final_data_set )

<- data.frame(final_data_set)

str(final_data_set)

#Replacing the NA values in the data set for Years Licenced , Vehicle Count and Driver Count  with the imputed values where policy_type='Auto
final_data_set$Years_Licenced <- ifelse(is.na(final_data_set$Years_Licenced) & final_data_set$Pol_type == "Auto", final_data_set$Licenced_Age, final_data_set$Years_Licenced)
final_data_set$Vehicle_Count <- ifelse(is.na(final_data_set$Vehicle_Count) & final_data_set$Pol_type == "Auto", final_data_set$Count_Vehicle, final_data_set$Vehicle_Count)
final_data_set$Driver_Count <- ifelse(is.na(final_data_set$Driver_Count) & final_data_set$Pol_type == "Auto", final_data_set$Count_Driver, final_data_set$Driver_Count)

#Structure of Final Dataset
str(final_data_set)

#Removing redundant columns 
final_data_set <- final_data_set %>% select(-Count_Vehicle,-Count_Driver,-Licenced_Age)

#Checking NA values
p<- function(x)  {sum(is.na(x))}
apply(final_data_set,2,p)

#Merging External Data of Zipcodes to add the Country and State Details
# Reading zip code data
zip_data <- read.csv("Zip_codes.csv",na.strings=na_strings,blank.lines.skip = FALSE)
zip_missing <- read.csv("zip_missing.csv",na.strings=na_strings,blank.lines.skip = FALSE)


names(zip_missing)
names(zip_data)


# Renaming columns in one file so that merging can be done.
zip_data_rn <- zip_data %>% rename("Postal.Code"="ZIP",
                    "City"="city",
                    "State"="state",
                    "StateISO"="stateISO",
                     "Country"="country")



#Merging Zip files by rows
zip_total_data <- rbind(zip_data_rn,zip_missing)



#Removing StateISO column as it's not required
zip_total_data <- zip_total_data %>% select (-StateISO)

#Finding Duplicate rows by postal code in data
unique_combinations <- zip_total_data %>%
  group_by(Postal.Code) %>%
 summarise(count=n()) %>% 
  filter(count>1)

zip_total_data %>% filter(Postal.Code %in% (unique_combinations$Postal.Code))


# Removing Duplicates in Zip code data
zip_total_data <- distinct(zip_total_data)


#Correcting some postal codes which are incorrectly mapped.
zip_total_data <-  zip_total_data %>% filter(!((Postal.Code=='K2J'& City=='Nepean') | (Postal.Code=='K7V'& City=='Kingston')))

#Making the Postal Code to uppercase in the final dataset
final_data <- final_data %>% mutate(Postal_code=toupper(Postal_code))

# Left Joining final dataset with the zipcode file
joined_data<- left_join(final_data, zip_total_data, by =c("Postal_code"="Postal.Code"))

#Final data
final_data_clean <- joined_data

write.csv(final_data_clean,"insurance.csv")


#Checking NA values
p<- function(x)  {sum(is.na(x))}
apply(final_data_clean,2,p)

#Imputing Country name as Canada for NA values in country
final_data_clean$Country <- ifelse(is.na(final_data_clean$Country), 'Canada', final_data_clean$Country)



####### DISCOUNTED PREMIUM  CALCULATION#################################################
calculatePremium <- function(policies, FirstTerm_Premium, SecondTerm_Premium, ThirdTerm_Premium, pol_buy_date) {
  base_year <- 2014  # Base year for discounting
  n <- year(pol_buy_date) - base_year  # Difference in years
  r <- 0.1
  if (policies <= 1) {
    FirstTerm <- FirstTerm_Premium * policies / (1 + r) ^ n
    return(FirstTerm)
  } else if (policies <= 2) {
    FirstTerm <- FirstTerm_Premium / (1 + r) ^ n
    SecondTerm <- (SecondTerm_Premium * (policies - 1) / (1 + r) ^ (n + 1))
    return(FirstTerm+SecondTerm)
  } else if (policies <= 3) {
    FirstTerm <- FirstTerm_Premium / (1 + r) ^ n
    SecondTerm <- (SecondTerm_Premium / (1 + r) ^ (n + 1))
    ThirdTerm <- (ThirdTerm_Premium * (policies - 2) / (1 + r) ^ (n + 2))
    return(FirstTerm + SecondTerm + ThirdTerm)
  } else {
    average_change <- (((SecondTerm_Premium - FirstTerm_Premium) / FirstTerm_Premium) +
                         ((ThirdTerm_Premium - SecondTerm_Premium) / SecondTerm_Premium)) / 2
    current_premium <- ThirdTerm_Premium
    cum_premium <- FirstTerm_Premium / (1 + r) ^ n + SecondTerm_Premium / (1 + r) ^ (n + 1) + ThirdTerm_Premium / (1 + r) ^ (n + 2)
    premium <- 0
    t <- 0
    n <- n+3
    for (i in 4:floor(policies)) {
      if (floor(policies) < 4) {
        break
      }
      current_premium <- (current_premium * (1 + average_change)) 
      premium <- premium + (current_premium / (1 + r) ^ (n +t))
      t <- t + 1
    }
    n <- n+t
    premium <- cum_premium + premium + (current_premium * (policies - floor(policies)) * (1 + average_change)) /
      ((1 + r) ^ n)
    return(premium)
  }
}

#######################NEW CLTV Calculation################################################################
calculateCLTV <- function(policies, FirstTerm_Premium, SecondTerm_Premium, ThirdTerm_Premium, pol_buy_date) {
  base_year <- 2014  # Base year for discounting
  n <- year(pol_buy_date) - base_year  # Difference in years
  r <- 0.1
  if (policies <= 1) {
    FirstTerm <- FirstTerm_Premium * policies / (1 + r) ^ n
    return(FirstTerm * policies)
  } else if (policies <= 2) {
    FirstTerm <- FirstTerm_Premium / (1 + r) ^ n
    SecondTerm <- (SecondTerm_Premium * (policies - 1) / (1 + r) ^ (n + 1))
    return((FirstTerm+SecondTerm) * policies)
  } else if (policies <= 3) {
    FirstTerm <- FirstTerm_Premium / (1 + r) ^ n
    SecondTerm <- (SecondTerm_Premium / (1 + r) ^ (n + 1))
    ThirdTerm <- (ThirdTerm_Premium * (policies - 2) / (1 + r) ^ (n + 2))
    return((FirstTerm + SecondTerm + ThirdTerm) * policies)
  } else {
    average_change <- (((SecondTerm_Premium - FirstTerm_Premium) / FirstTerm_Premium) +
                         ((ThirdTerm_Premium - SecondTerm_Premium) / SecondTerm_Premium)) / 2
    current_premium <- ThirdTerm_Premium
    cum_premium <- FirstTerm_Premium / (1 + r) ^ n + SecondTerm_Premium / (1 + r) ^ (n + 1) + ThirdTerm_Premium / (1 + r) ^ (n + 2)
    premium <- 0
    t <- 0
    n <- n+3
    for (i in 4:floor(policies)) {
      if (floor(policies) < 4) {
        break
      }
      current_premium <- (current_premium * (1 + average_change)) 
      premium <- premium + (current_premium / (1 + r) ^ (n +t))
      t <- t + 1
    }
    n <- n+t
    premium <- cum_premium + premium + (current_premium * (policies - floor(policies)) * (1 + average_change)) /
      ((1 + r) ^ n)
    return(premium * policies)
  }
}

##################################################################################################



# Adding Premium and CLTV columns in the dataset by caling the functions calculatePremium & calculateCLTV
final_data_clean <- final_data_clean %>% 
  rowwise() %>% 
  mutate(
    premium = calculatePremium(policies, FirstTerm_Premium, SecondTerm_Premium, ThirdTerm_Premium, Pol_buy_date),
    CLTV = calculateCLTV(policies, FirstTerm_Premium, SecondTerm_Premium, ThirdTerm_Premium,Pol_buy_date))
  
#Converting to Dataframe
final_data_clean <-  data.frame(final_data_clean)



######################TESTING###########NEEDS TO BE EXCLUDED##################
year <- 2015

n <- year - 2014

n

year(final_data_st$Pol_buy_date)

year <- as.numeric(substr("2016-09-17", 1, 4))

year <- as.numeric(format('2016-09-17', "%Y"))

str(final_data_st)



###############################################################################
# Transforming pl_data_final

pl_data_fin_tr <- final_data_clean

str(pl_data_fin_tr)

#Changing categorical variables to factors
pl_data_fin_tr$Gender <- as.factor(pl_data_fin_tr$Gender)
pl_data_fin_tr$Marital_status <- as.factor(pl_data_fin_tr$Marital_status)
pl_data_fin_tr$Policy_Status <- as.factor(pl_data_fin_tr$Policy_Status)
pl_data_fin_tr$Pol_sub_type <- as.factor(pl_data_fin_tr$Pol_sub_type)


#dropping factors with no levels and grouping levels for Martial status


pl_data_fin_tr <- pl_data_fin_tr %>% mutate(Marital_status=fct_drop(Marital_status),
                                  Pol_sub_type=fct_drop(Pol_sub_type),
                                  Marital_status=fct_collapse(Marital_status,
                                                              other=c('C','N','P','R','W','Y')))




#Final data set
nrow(pl_data_fin_tr)

 

write.csv(pl_data_fin_tr,"insurance.csv")

names(pl_data_fin_tr)
 
summary(pl_data_fin_tr)

str(pl_data_fin_tr)

####EXploratory analysis######This code still needs to be build#########

#Dividing data sets into Auto and property insurance to calculate the correlation plots

pl_data_fin_tr_Auto <-  pl_data_fin_tr %>% filter(Pol_type=="Auto")

pl_data_fin_tr_Property <-  pl_data_fin_tr %>% filter(Pol_type=="Property") %>% select(-Driver_Count,-Vehicle_Count,-Years_Licenced,-Lic_susp_6yr,-Insurance_Cancellation_3yr,-Misrepresented)

#Correlation Plot For Auto Policy data
corrplot(corr=cor(pl_data_fin_tr_Auto[ ,c(9,12:17,24,28:29) ]))
# Can see positive correlation between Years Licenced and Age_as_of_Policy
#Vehicle Count and Driver Count also seems to have a positive correlation
#Insurance cancellation and Lic_Susp_yr has a strong relationship
# ClTV has just relationship with Premium which is obvious as the customer is paying more, his lifetime value is more


#Correlation plot of Property data
names(pl_data_fin_tr_Property) 
corrplot(corr=cor(pl_data_fin_tr_Property[ ,c(9,18,22,23) ]))


#Exploratory Analysis of Auto Policies
pl_data_fin_tr_Auto %>% filter(!is.na(Driver_Count) )%>% 
  ggplot(aes(Driver_Count))+
  geom_histogram(binwidth=1,fill="#97B3C6") +
  #coord_flip()
  theme_bw()+
  labs(x="Driver Count",y="Frequency",title="Driver Count") 
#Policy holder with Driver count 1 is more 

pl_data_fin_tr_Auto %>% filter(!is.na(Driver_Count) )%>% 
  ggplot(aes(Driver_Count,fill=Gender))+
  geom_histogram(binwidth=1,alpha=0.5) +
  #coord_flip()
  theme_bw()+
  labs(x="Driver Count",y="Frequency",title="Driver Count") +
facet_wrap(~Gender)
#Policy holder with Driver count 1 is more in both males and Females

pl_data_fin_tr_Auto %>% filter(!is.na(Driver_Count) )%>% 
  ggplot(aes(Vehicle_Count,fill=Gender))+
  geom_histogram(binwidth=1,alpha=0.5) +
  #coord_flip()
  theme_bw()+
  labs(x="Driver Count",y="Frequency",title="Driver_Count") +
  facet_grid(vars(Gender),vars(Marital_status))


pl_data_fin_tr_Auto %>% filter(!is.na(Vehicle_Count) )%>% 
  ggplot(aes(Vehicle_Count,fill=Gender))+
  geom_histogram(binwidth=1,alpha=0.5) +
  #coord_flip()
  theme_bw()+
  labs(x="Driver Count",y="Frequency",title="Vehicle Count") +
  facet_wrap(~Gender)
#Policy holder with Driver count 1 is more in both males and Females


pl_data_fin_tr_Auto %>% filter(!is.na(Vehicle_Count) )%>% 
  ggplot(aes(Vehicle_Count,fill=Gender))+
  geom_histogram(binwidth=1,alpha=0.5) +
  #coord_flip()
  theme_bw()+
  labs(x="Driver Count",y="Frequency",title="Vehicle Count") +
  facet_grid(vars(Gender),vars(Marital_status))



pl_data_fin_tr_Auto %>% filter(!is.na(premium) )%>% 
  ggplot(aes(premium,fill=Gender))+
  geom_histogram(binwidth=1,alpha=0.5) +
  #coord_flip()
  theme_bw()+
  labs(x="Driver Count",y="Frequency",title="Premium") +
  facet_grid(vars(Gender),vars(Pol_sub_type))+
  scale_x_log10()
  

pl_data_fin_tr %>% filter(!is.na(Vehicle_Count) )%>% 
  ggplot(aes(Vehicle_Count,fill=Gender))+
  geom_histogram(binwidth=1,alpha=0.5) +
  #coord_flip()
  theme_bw()+
  labs(x="Driver Count",y="Frequency",title="Driver Count") +
  facet_grid(vars(Marital_status),vars(Pol_sub_type))

pl_data_fin_tr %>% filter(!is.na(Vehicle_Count) )%>% 
  ggplot(aes(Vehicle_Count,fill=Gender))+
  geom_density(alpha=0.5) +
  #coord_flip()
  theme_bw()+
  labs(x="Driver Count",y="Frequency",title="Driver Count") +
  facet_grid(rows=vars(Marital_status))

pl_data_fin_tr %>% 
  ggplot(aes(x=Age_as_of_policy,y=premium,col=Gender))+
  geom_point(alpha=0.1)+
  #geom_smooth(se=0,method='lm')+
  #coord_flip()
  theme_bw()+
  labs(x="Age",y="CLTV",title="CLTV dependent on Age") +
  scale_y_log10()
  #facet_grid(rows=vars(Marital_status))

pl_data_fin_tr %>% 
  ggplot(aes(x=Years_Licenced,y=premium,col=Gender))+
  geom_point(alpha=0.1)+
  #geom_smooth(se=0,method='lm')+
  #coord_flip()
  theme_bw()+
  labs(x="Age",y="CLTV",title="CLTV dependent on Age") +
  scale_y_log10()

pl_data_fin_tr %>% 
  ggplot(aes(x=Misrepresented,y=premium,col=Gender))+
  geom_point(alpha=0.1)+
  #geom_smooth(se=0,method='lm')+
  #coord_flip()
  theme_bw()+
  labs(x="Age",y="CLTV",title="CLTV dependent on Age") +
  scale_y_log10()

str(pl_data_fin_tr)

boxplot(pl_data_fin_tr)

pl_data_fin_tr%>%
  ggplot(aes(CLTV))+
  geom_boxplot() +
  coord_flip() +
  theme_bw()

new <- pl_data_fin_tr %>% 
  group_by(Gender) %>% 
  summarise()

ggplot(final_data_set, aes(x = Gender, fill = Policy_Status)) +
  geom_bar(position = "dodge") +
  xlab("Gender") +
  ylab("Count")

ggplot(final_data_set, aes(x = Marital_status, fill = Gender)) +
  geom_bar(position = "dodge") +
  xlab("Gender") +
  ylab("Count")

ggplot(final_data_set, aes(x = Gender, fill = Policy_Status)) +
  geom_bar() +
  xlab("Variable 1") +
  ylab("Count")



ggplot(pl_data_fin_tr, aes(x=Gender,y=CLTV,col))+
  geom_col


pl_data_fin_tr %>% ggplot(aes(x=Gender,y=premim,col=Gender))+
  geom_boxplot()+
  scale_y_log10()

str(pl_data_fin_tr)
 

# Correlation Plots:

install.packages("corrplot")

library(corrplot)

names(pl_data_fin_tr)



#Boxplots

# Example dataset
data <- data.frame(
  Column1 = rnorm(100),
  Column2 = rnorm(100),
  Column3 = rnorm(100)
)

# Set the layout to display box plots in a grid
layout(matrix(1:3, nrow = 1))

# Create box plots for each column
for (col in names(data)) {
  boxplot(data[[col]], main = paste("Box Plot of", col))
}





 
#-------------------------------------------------------------------------------
# PROGRAMMING FOR DATA ANALYSIS - ASSIGNMENT EMPLOYEE ATTRITION
# MODULE CODE:    CT127-3-2-PFDA
# MODULE NAME:    PROGRAMMING FOR DATA ANALYSIS
# TITLE:          EMPLOYEE ATTRITION
# INTAKE:         APD2F2109CS(IS)
# Student Name:   Wong Horng Woei
# Student ID:     TP055241
# Lecturer:       Liew Yee Jing
#-------------------------------------------------------------------------------
#============================= 3.1 Install Packages ============================
install.packages("dplyr")
install.packages("ggplot2")
install.packages("magrittr")
install.packages("tibble")
install.packages("stringr")
install.packages("forcats")
install.packages("ggthemes")

#============================== 3.2 Load Libraries =============================
library(dplyr)
library(ggplot2)
library(magrittr)
library(tibble)
library(stringr)
library(forcats)
library(ggthemes)
#============================== 3.3 Import Data ================================
ds = read.csv("C:/Users/Horng Woei/Desktop/Programming for Data Analysis/Assignment/employee_attrition.csv")
str(ds)
summary(ds)
View(ds)

# ============================ 4.0 Data Exploration ============================
#Bar Chart for Employee Status by Year
ds%>%
  ggplot(aes(x = as.factor(STATUS_YEAR), fill = STATUS)) +
  geom_bar() +
  labs(title = "Employee Status by Year", x = "Year", y = "Employee Count") +
  geom_text(aes(label = ..count..), position=position_stack(vjust=0.5),
            stat = "count", colour = "white") +
  scale_fill_manual("STATUS", values = c("ACTIVE" = "cornflowerblue",
                                         "TERMINATED" = "sienna3"))

#Bar Chart for Length of Service
ds%>%
  ggplot(aes(x = as.factor(length_of_service), fill = ..count.. )) +
  geom_bar() +
  labs(title = "Bar Chart for Length of Service",
       x = "Length of Service", y = "Employee Count") +
  scale_fill_gradient("Employee Count", low = "sienna4", high = "sienna2") +
  geom_text(aes(label = ..count..), vjust=-0.15, stat = "count",
            colour = "black",size =3)

#Bar Chart for City Name
ds %>%
  ggplot(aes(y=city_name,  fill=..count..)) +
  geom_bar() + 
  labs(title = "Bar Chart for City Name",
       x = "Employee Count", y = "City Name") +
  scale_fill_gradient("Employee Count", low = "sienna4", high = "sienna1") +
  geom_text(aes(label = ..count..), hjust=-0.05, stat = "count",
            colour = "black", size=3)

#Bar Chart for Department
ds%>%
  ggplot(aes(y = department_name, fill = ..count.. )) +
  geom_bar() +
  xlim(0,10500) +
  labs(title = "Bar Chart for Department",
       x = "Employee Count", y = "Department Name") +
  scale_fill_gradient("Employee Count", low = "sienna4", high = "sienna1") +
  geom_text(aes(label = ..count..), vjust= 0.2, hjust= -0.05,
            stat = "count", colour = "black")

#Bar Chart for Job Title
ds %>%
  ggplot(aes(y = job_title,fill = ..count..)) +
  geom_bar() +
  labs(title="Bar Chart for Job Title of Employee",
       x="Employee Count", y="Job Title") +
  scale_fill_gradient("Employee Count", low = "sienna4", high = "sienna2") +
  geom_text(aes(label = ..count..), hjust=-0.05, stat = "count",
            colour = "black",size = 4)

#Pie Chart for Gender
ds%>%
  ggplot(aes(x="", fill = gender_full,stat = "count" )) +
  geom_bar(position = position_stack(),color="white",width=1, size = 2) +
  coord_polar("y", start=0) +
  theme_void()+
  labs(title = "Employee Count by Gender",
       x = "Gender", y = "Employee Count", fill = "Gender") +
  geom_text(aes(label = ..count..), position = position_stack(vjust=0.5),
            stat = "count", colour = "white", size=8)

#Pie Chart for Termination Type 
ds %>%
  filter(termtype_desc != "Not Applicable") %>%
  ggplot(aes(x="", fill = termtype_desc)) +
  geom_bar(position=position_stack(),width = 1, color = "white", size = 2) +
  coord_polar("y", start=0) +
  theme_void()+
  labs(title="Employee Count by Termination Type",
       x="Termination Type", y="Count", fill = "Termination Type") +
  geom_text(aes(label = ..count..), position = position_stack(vjust=0.5),
            stat = "count", colour = "white", size=8)

#Pie Chart for Termination Reason
ds %>%
  filter(termreason_desc != "Not Applicable") %>%
  ggplot( aes(x="", fill = termreason_desc)) +
  geom_bar(position=position_stack(),width = 1, color = "white", size = 2) +
  coord_polar("y", start=0) +
  theme_void()+
  labs(title="Employee Count by Termination Reason",
       x="Termination Reason", y="Count", fill = "Termination Reason") +
  geom_text(aes(label = ..count..), position = position_stack(vjust=0.5),
            stat = "count", colour = "white", size=5)

#============================ 5.0 Data Pre-processing ==========================
#check any "NA" 
sum(is.na(ds))

# Correct Spelling Errors
ds$termreason_desc <- str_replace(ds$termreason_desc, "Resignaton", "Resignation")
ds$job_title <- str_replace(ds$job_title, "CHief Information Officer", "Chief Information Officer")
ds$job_title <- str_replace(ds$job_title, "Accounts Receiveable Clerk", "Accounts Receivable Clerk")
ds$department_name <- str_replace(ds$department_name, "Accounts Receiveable", "Accounts Receivable")
View(ds)

# Remove Meaningless Attributes
ds$recorddate_key = NULL
ds$birthdate_key = NULL
ds$gender_short = NULL

#Rename the columns
rename_ds = ds%>%
  rename(HireDate          = orighiredate_key,
         TerminationDate   = terminationdate_key,
         Age               = age,
         ServiceDuration   = length_of_service,
         City              = city_name,
         Department        = department_name,
         JobTitle          = job_title,
         StoreNo           = store_name,
         Gender            = gender_full,
         TerminationReason = termreason_desc,
         TerminationType   = termtype_desc,
         Year              = STATUS_YEAR,
         Status            = STATUS,
         BusinessUnit      = BUSINESS_UNIT)

View(rename_ds)#Check renamed attributes

#============================ 6.0 Data Manipulation ============================
#Add new column for JobPosition 
convert_ds = rename_ds %>%
  mutate(JobPosition = case_when(
    JobTitle == "Accounting Clerk" ~ "Accounting",
    JobTitle == "Accounts Payable Clerk" ~ "Accounting",
    JobTitle == "Accounts Receivable Clerk" ~ "Accounting",
    JobTitle == "Director, Accounting" ~ "Director",
    JobTitle == "Director, Accounts Payable" ~ "Director",
    JobTitle == "Director, Accounts Receivable" ~ "Director",
    JobTitle == "Director, Audit" ~ "Director",
    JobTitle == "Director, Compensation" ~ "Director",
    JobTitle == "Director, Employee Records" ~ "Director",
    JobTitle == "Director, HR Technology" ~ "Director",
    JobTitle == "Director, Investments" ~ "Director",
    JobTitle == "Director, Labor Relations" ~ "Director",
    JobTitle == "Director, Recruitment" ~ "Director",
    JobTitle == "Director, Training" ~ "Director",
    JobTitle == "Exec Assistant, Finance" ~ "Exec Assistant",
    JobTitle == "Exec Assistant, Human Resources" ~ "Exec Assistant",
    JobTitle == "Exec Assistant, Legal Counsel" ~ "Exec Assistant",
    JobTitle == "Exec Assistant, VP Stores" ~ "Exec Assistant",
    JobTitle == "HRIS Analyst" ~ "Analyst",
    JobTitle == "Investment Analyst" ~ "Analyst",
    JobTitle == "Labor Relations Analyst" ~ "Analyst",
    JobTitle == "Systems Analyst" ~ "Analyst",
    JobTitle == "Compensation Analyst" ~ "Analyst",
    JobTitle == "Bakery Manager" ~ "Manager",
    JobTitle == "Customer Service Manager" ~ "Manager",
    JobTitle == "Dairy Manager" ~ "Manager",
    JobTitle == "Meats Manager" ~ "Manager",
    JobTitle == "Processed Foods Manager" ~ "Manager",
    JobTitle == "Produce Manager" ~ "Manager",
    JobTitle == "Store Manager" ~ "Manager",
    JobTitle == "Auditor" ~ "Auditor",
    JobTitle == "Baker" ~ "Baker",
    JobTitle == "Benefits Admin" ~ "Benefits Admin",
    JobTitle == "Cashier" ~ "Cashier",
    JobTitle == "CEO" ~ "CEO",
    JobTitle == "Chief Information Officer" ~ "Chief Information Officer",
    JobTitle == "Corporate Lawyer" ~ "Corporate Lawyer",
    JobTitle == "Dairy Person" ~ "Dairy Person",
    JobTitle == "Legal Counsel" ~ "Legal Counsel",
    JobTitle == "Meat Cutter" ~ "Meat Cutter",
    JobTitle == "Produce Clerk" ~ "Produce Clerk",
    JobTitle == "Recruiter" ~ "Recruiter",
    JobTitle == "Shelf Stocker" ~ "Shelf Stocker",
    JobTitle == "Trainer" ~ "Trainer",
    JobTitle == "VP Finance" ~ "VP Finance",
    JobTitle == "VP Human Resources" ~ "VP Human Resources",
    JobTitle == "VP Stores" ~ "VP Stores",
    TRUE ~ "Unknown"), .after = JobTitle)

#Convert HireDate and TerminationDate to date format
newcol_ds = convert_ds %>% 
  mutate(HireDate = as.Date(convert_ds$HireDate, format="%m/%d/%Y"),
         TerminationDate = as.Date(convert_ds$TerminationDate, format="%m/%d/%Y"))

str(newcol_ds) #Check changes on date format data types

#Add new column for HireYear, after HireDate column
new_ds = newcol_ds %>%
  add_column(HireYear = format(newcol_ds$HireDate, format="%Y"), .after = "HireDate")%>%
  add_column(TerminationYear = format(newcol_ds$TerminationDate, format="%Y"), .after = "TerminationDate")

View(new_ds) #Check new column added behind HireDate column

#Add Age Class
new_ds <- mutate(new_ds, AgeClass = cut(Age, c(18,24,34,44,54,64,70), 
                                        c("19-24", "25-34", "35-44", "45-54", "55-64", "65+")), .after = "Age")

View(new_ds) #Check new column added behind Age column

# Convert into Factor for Level Indication
new_ds$HireYear         = factor(new_ds$HireYear)
new_ds$TerminationYear  = factor(new_ds$TerminationYear)
new_ds$City             = factor(new_ds$City)
new_ds$Department       = factor(new_ds$Department)
new_ds$JobTitle         = factor(new_ds$JobTitle)
new_ds$JobPosition      = factor(new_ds$JobPosition)
new_ds$StoreNo          = factor(new_ds$StoreNo)
new_ds$Gender           = factor(new_ds$Gender)
new_ds$TerminationReason= factor(new_ds$TerminationReason)
new_ds$TerminationType  = factor(new_ds$TerminationType)
new_ds$Year             = factor(new_ds$Year)
new_ds$Status           = factor(new_ds$Status)
new_ds$BusinessUnit     = factor(new_ds$BusinessUnit)

View(new_ds) #check for newly added column

#Check Data Structures
str(new_ds) #check data types
summary(new_ds) #summarize the dataset

#======================= 7.0 Questions and Analysis ============================

#============================== 7.1 Question 1 =================================
#Why did the employees left the company?
#===============================================================================

#7.1.1 Analysis 1 - Determine the relationship between Year and Termination Reason
new_ds%>%
  filter(TerminationReason !="Not Applicable")%>%
  ggplot(aes(x = Year,fill = TerminationReason)) + 
  geom_bar() +
  theme_stata()+ #Extra Feature - 8.2 ggthemes
  #A theme from ggthemes library is applied for clearer visual of the graph.
  labs(title = "The Relationship between Year and Termination Reason",
       x = "Year", y = "Employee Count", fill = "Termination Reason") +
  geom_text(aes(label = ..count..), stat= "count",
            position = position_stack(vjust = 0.5), colour = "white")

#7.1.2 Analysis 2-Determine the relationship between Department and Termination Reason
new_ds%>%
  filter(Status=="TERMINATED")%>%
  ggplot(aes(x = Department,y = ..count.., fill = TerminationReason))+ 
  geom_bar()+
  theme_stata()+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
  labs(title = "The Relationship between Department and Termination Reason",
       x = "Department", y = "Employee Count", fill = "Termination Reason")

#7.1.3 Analysis 3 - Determine the Age when employees start to resign in Customer Service department
new_ds%>%
  filter(TerminationReason == "Resignation")%>%
  filter(Department == "Customer Service")%>%
  ggplot(aes(x = factor(Age)))+
  geom_bar(width = 0.9, fill = "sienna3")+
  theme_stata()+
  labs(title = "The age of employees that resign in Customer Service department",
       x = "Age", y = "Employee Count") +
  geom_text(aes(label = ..count..), stat= "count",
            vjust = -0.15, colour = "black")

#7.1.4 Analysis 4 - Determine the Duration of Service in Customer Service department with employees that are 21 years old.
new_ds%>%
  filter(TerminationReason == "Resignation")%>%
  filter(Department == "Customer Service")%>%
  filter(Age == "21")%>%
  ggplot(aes(x = factor(ServiceDuration)))+
  geom_bar(width = 0.3, fill = "sienna3")+
  theme_stata()+
  labs(title = "The Duration of Service in Customer Service department with employees that are 21 years old",
       x = "Service Duration(Year)", y = "Employee Count") +
  geom_text(aes(label = ..count..), stat= "count",  vjust = -0.15,
            colour = "black")

#7.1.5 Analysis 5 - Determine the Age when employees start to retire in Meats, and Produce department
new_ds%>%
  filter(TerminationReason == "Retirement")%>%
  filter(Department == "Meats" | Department == "Produce")%>%
  ggplot(aes(x = factor(Age)))+
  geom_bar(width = 0.9, fill = "sienna3")+
  theme_stata()+
  labs(title = "The age of employees that retire in Meats, and Produce department",
       x = "Age", y = "Employee Count") +
  geom_text(aes(label = ..count..), stat= "count",  vjust = -0.15,
            colour = "black")+
  facet_wrap(~Department)

#7.1.6 Analysis 6 - Determine the Duration of Service in Meats, and Product department with employees that over 60 years old.
new_ds%>%
  filter(TerminationReason == "Retirement")%>%
  filter(Department == "Meats" | Department == "Produce")%>%
  filter(Age == "60" | Age == "65")%>%
  ggplot(aes(x = factor(ServiceDuration)))+
  geom_bar(width = 0.5, fill = "sienna3")+
  theme_stata()+
  labs(title = "The Duration of Service in Meats, and Product department with employees that over 60 years old.",
       x = "Service Duration(Years)", y = "Employee Count") +
  geom_text(aes(label = ..count..), stat= "count",  vjust = -0.15,
            colour = "black")+
  facet_wrap(~Department)

#7.1.7 Analysis 7 - Determine the relationship between Age and Duration of Service
new_ds%>%
  ggplot(aes(x = ServiceDuration, y = Age, fill= Status)) +
  geom_boxplot() +
  theme_stata()+
  theme(legend.position="none")+
  scale_fill_manual("Status", values = c("ACTIVE" = "skyblue1",
                                         "TERMINATED" = "sienna3"))+
  labs(title = "The Relationship between Age and Service Duration(Years)",
       x = "Service Duration(Years)", y = "Age")+
  facet_grid(~Status)

#============================== 7.2 Question 2 =================================
#Which Age Class that getting the most layoff?
#===============================================================================

#7.2.1 Analysis 1 - Determine the relationship between Year and Termination Reason
new_ds%>%
  filter(TerminationReason =="Layoff")%>%
  ggplot(aes(x = Year,fill = TerminationReason)) + 
  geom_bar(width = 0.5) +
  theme_clean()+
  labs(title = "The Relationship between Year and Termination Reason", 
       x = "Year", y = "Employee Count", fill = "Termination Reason") +
  geom_text(aes(label = ..count..), stat= "count", 
            position = position_stack(vjust = 0.5), colour = "white", size = 7)
  
#7.2.2 Analysis 2 - Determine the Layoff in City
new_ds%>%
  filter(TerminationReason == "Layoff") %>%
  count(City)%>%
  mutate(City = fct_reorder(City,n))%>% #Extra Feature - 8.1 fct_reorder
  #The fct_reorder() functions help to reorder the highest value to lowest value
  #from top to bottom.
  ggplot(aes(y = City, x = n)) +
  geom_bar(stat = "identity", fill = "salmon") +
  theme_clean()+
  labs(title = "Layoff in City", 
       x = "Employee Count", y = "City", fill = "Employee Count") +
  geom_text(aes(label = n), vjust = 0.5,hjust = -0.25, colour = "black")


#7.2.3 Analysis 3 - Determine the relationship between Age Class and Employee Count
new_ds%>%
  filter(City == "Fort Nelson")%>%
  filter(TerminationReason =="Layoff")%>%
  ggplot(aes(x = AgeClass, fill = ..count..))+
  geom_bar(fill = "salmon")+
  ylim(0,15)+
  theme_clean()+
  labs(title = "The relationship between Age Class and Employee Count", 
       x = "Age Class", y = "Employee Count") +
  geom_text(aes(label = ..count..), stat= "count", vjust =-0.2, colour ="black")+
  facet_grid(~City)

#============================== 7.3 Question 3 =================================
# Which job are in high demand in a crowded city?
#===============================================================================

#7.3.1 Analysis 1 - Determine the Active Employee in each City
new_ds%>%
  filter(Status =="ACTIVE")%>%
  filter(Year =="2015")%>%
  distinct(EmployeeID, .keep_all = TRUE)%>%
  count(City)%>%
  mutate(City = fct_reorder(City, n))%>%
  ggplot(aes(y = City, x = n)) +
  geom_bar(stat="identity", fill = "sienna3") +
  theme_economist()+
  labs(title = "Active Employee in each City",
       x = "Employee Count", y = "City") +
  geom_text(aes(label = n), vjust = 0.5, hjust = -0.25, 
            colour = "black")

#7.3.2 Analysis 2 - Determine the Active Employee in the Store
new_ds%>%
  filter(Year =="2015")%>%
  filter(Status == "ACTIVE")%>%
  filter(City == "Vancouver")%>%
  distinct(EmployeeID, .keep_all = TRUE)%>%
  ggplot(aes(x = BusinessUnit))+
  geom_bar(width = 0.5, fill = "Sienna3") +
  theme_economist()+
  labs(title = "Active Employee in Vancouver by Business Unit",
       x = "Business Unit", y = "Employee Count") +
  geom_text(aes(label = ..count..), stat= "count", vjust =-0.2, colour ="black")

new_ds%>%
  filter(Year =="2015")%>%
  filter(Status =="ACTIVE")%>%
  filter(City == "Vancouver" )%>%
  filter(BusinessUnit == "STORES") %>%
  distinct(EmployeeID, .keep_all = TRUE)%>%
  ggplot(aes(x = factor(StoreNo)))+
  geom_bar(fill = "Sienna3")+
  theme_economist()+
  labs(title = "Active Employee in Vancouver by Stores",
       x = "Store Number", y = "Employee Count") +
  geom_text(aes(label = ..count..), stat= "count", vjust =-0.2, colour ="black")

#7.3.3 Analysis 3 - Determine the jobs that has the most employee in Vancouver
new_ds%>%
  filter(Year =="2015")%>%
  filter(Status =="ACTIVE")%>%
  filter(City == "Vancouver" )%>%
  filter(StoreNo == "42") %>%
  distinct(EmployeeID, .keep_all = TRUE)%>%
  count(JobTitle)%>%
  mutate(JobTitle = fct_reorder(JobTitle, n))%>%
  ggplot(aes(x = JobTitle, y = n))+
  geom_bar(stat = "identity", fill = "Sienna3")+
  theme_economist()+
  labs(title = "Job Title in Vancouver by Store Number 42",
       x = "Job Title", y = "Employee Count") +
  geom_text(aes(label = n), vjust =-0.2, colour ="black")

#============================== 7.4 Question 4 =================================
#Is there gender inequality in the organization?
#===============================================================================

#7.4.1 Analysis 1 - Determine the relationship between Year and Gender
new_ds%>%
  ggplot(aes(x = fct_rev(Year), fill = Gender))+
  geom_bar(data = subset(new_ds, Gender == "Female"))+
  geom_bar(data = subset(new_ds, Gender == "Male"),aes(y=- ..count..))+
  scale_y_continuous(breaks=seq(-2500,2500,500),labels=abs(seq(-2500,2500,500)))+
  theme_gdocs() +
  labs(title = "Population Pyramid for Year and Gender",
       x = "Year", y = "Employee Count")+
  coord_flip()

#7.4.2 Analysis 2 - Determine the relationship between HireYear and Gender
#Extra Feature - 8.3 Population Pyramid Chart
#This chart shows gender distribution of employee count which makes it easier
#and clearer to visualize the graph compare to bar chart.
new_ds%>%
  ggplot(aes(x = HireYear, fill = Gender))+
  geom_bar(data = subset(new_ds, Gender == "Female"))+
  geom_bar(data = subset(new_ds, Gender == "Male"),aes(y=- ..count..))+
  scale_y_continuous(breaks=seq(-1500,1500,250),labels=abs(seq(-1500,1500,250)))+
  theme_gdocs()+
  labs(title = "Population Pyramid for Year and Gender",
       x = "Hire Year", y = "Employee Count")+
  coord_flip()

#7.4.3 Analysis 3 - Determine the relationship between Status and Gender within 2011-2015
new_ds %>%
  filter(TerminationReason %in% c("Resignation", "Layoff")) %>%
  filter(Year %in% c("2015", "2014", 
                     "2013", "2011", "2012")) %>%
  filter(Status %in% "TERMINATED") %>%
  ggplot(aes(x = Status, fill = Year))+
  geom_bar()+
  ylim(0,300)+
  theme_stata()+
  labs(title = "Bar Chart for Status and Gender within 2011-2015",
       x = "Status", y = "Employee Count")+
  geom_text(aes(label = ..count..), stat="count", 
            position = position_stack(vjust = 0.5), colour ="black")+
  facet_wrap(~Gender)

#7.4.4 Analysis 4 - Determine the relationship between Department and Gender
new_ds%>%
  ggplot(aes(y = Department)) +
  geom_bar(fill = "sienna3") +
  theme_clean()+
  labs(title = "Bar Chart for Department and Gender",
       x = "Employee Count", y = "Department") +
  facet_wrap(~Gender)

#============================== 7.5 Question 5 =================================
# What is the organization of the store?
#===============================================================================

#7.5.1 Analysis 1 - Determine the relationship between Store Number and Department
new_ds %>%
  filter(Year == "2015") %>%
  filter(Status == "ACTIVE") %>%
  ggplot(aes(x = StoreNo, fill = Department)) +
  geom_bar()+
  ylim(0,500)+
  theme_economist()+
  labs(title = "Bar Chart for Store Number and Department",
       x = "Store Number", y = "Employee Count")

#7.5.2 Analysis 2 - Determine the Highest Employee Count in Store with Job Position
new_ds %>%
  filter(Year == "2015") %>%
  filter(Status == "ACTIVE") %>%
  filter(StoreNo == "46")%>%
  ggplot(aes(x = StoreNo, fill = JobPosition))+
  geom_bar(width = 0.3)+
  ylim(0,500)+
  theme_economist()+
  labs(title = "Bar Chart for Highest Employee Count in Store with Job Position",
       x = "Store Number", y = "Employee Count")+
  geom_text(aes(label = ..count..), stat= "count", 
            position = position_stack(vjust = 0.5), colour = "white", size = 5)+
  facet_grid(~City)


#7.5.3 Analysis 3 - Determine the Lowest Employee Count in Store with Job Position
new_ds %>%
  filter(Year %in% "2015") %>%
  filter(Status %in% "ACTIVE") %>%
  filter(StoreNo %in% c("3", "19", "35"))%>%
  ggplot(aes(x = StoreNo, fill = JobPosition))+
  geom_bar()+
  theme_economist()+
  labs(title = "Bar Chart for Lowest Employee Count in Store with Job Position",
       x = "Store Number", y = "Employee Count")+
  geom_text(aes(label = ..count..), stat= "count", 
            position = position_stack(vjust = 0.5), colour = "white", size = 5)+
  facet_grid(~City, scales="free")

#7.5.4 Analysis 4 - Determine the store that has Store Manager
new_ds %>%
  filter(Year %in% "2015") %>%
  filter(Status %in% "ACTIVE") %>%
  filter(StoreNo %in% c("17", "18", "32", "44"))%>%
  ggplot(aes(x = StoreNo, fill = JobPosition))+
  geom_bar()+
  theme_economist()+
  labs(title = "Bar Chart for Store that has Store Manager",
       x = "Store Number", y = "Employee Count")+
  facet_grid(~City, scales="free")

new_ds %>%
  filter(Year %in% "2015") %>%
  filter(Status %in% "ACTIVE") %>%
  filter(JobTitle%in% "Store Manager")%>%
  filter(StoreNo %in% c("17", "18", "32", "44"))%>%
  ggplot(aes(x = StoreNo, fill = JobTitle))+
  geom_bar(fill= "sienna3")+
  ylim(0,3)+
  theme_economist()+
  labs(title = "Bar Chart for Number of Store Manager in Store",
       x = "Store Number", y = "Employee Count")+
  facet_grid(~City, scales="free")


#==============================8.0 Extra Feature ===============================
#Extra Feature 1 - 8.1 fct_reorder()
#Located in Question 2, Analysis 2

#Extra Feature 2 - 8.2 ggthemes()
#Located in Question 1, Analysis 1

#Extra Feature 3 - 8.3 Population Pyramid Chart
#Located in Question 4, Analysis 2

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ END OF THE CODE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
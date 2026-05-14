#loading data
df <- read.csv("bank-additional-full.csv", header = TRUE, sep = ";")

#explore the dataset if needed
str(df)

#splitting 2 groups that did subcribed and did not
yes_group <- df[df$y == "yes",]
no_group  <- df[df$y == "no",]

#do the t-test with col age & duration 
#reason we use t-test its because 2 sample quantitative
t.test(yes_group$age, no_group$age, var.equal = FALSE)
t.test(yes_group$duration, no_group$duration, var.equal = FALSE)
t.test(yes_group$campaign, no_group$campaign, var.equal = FALSE)
t.test(yes_group$pdays, no_group$pdays, var.equal = FALSE)
t.test(yes_group$previous, no_group$previous, var.equal = FALSE)


#do the chi square test with column job, marital, education, default and housing
#create a table with column job and subcription status
job_table <- table(df$job, df$y)
#compute chi square for job
chisq.test(job_table)

#create a table with column marital and subcription status
marital_table <- table(df$marital, df$y)
#compute chi square for marital
chisq.test(marital_table)

#create a table with column education and subcription status
education_table <- table(df$education, df$y)
#compute chi square for education
chisq.test(education_table)

#create a table with column default and subcription status
default_table <- table(df$default, df$y)
#compute chi square for default
chisq.test(default_table)

#create a housing with column housing and subcription status
housing_table <- table(df$housing, df$y)
#compute chi square for housing
chisq.test(housing_table)

#create a table with column loan and subcription status
loan_table <- table(df$loan, df$y)
#compute chi square for loan
chisq.test(loan_table)

#create a table with column contact and subcription status
contact_table <- table(df$contact, df$y)
#compute chi square for contact
chisq.test(contact_table)

#create a table with column month and subcription status
month_table <- table(df$month, df$y)
#compute chi square for month
chisq.test(month_table)

#create a table with column poutcome and subcription status
poutcome_table <- table(df$poutcome, df$y)
#compute chi square for poutcome
chisq.test(poutcome_table)




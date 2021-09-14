#' Homework 01 - Tests R datastructures knowledge 
#' Due on: 09/06/2021 
#' Q1. Create two vectors yearsOfExperience	and annualSalary, using the below values: 

#' 1.1	39343.00
#' 1.3	46205.00
#' 1.5	37731.00
#' 2.0	43525.00
#' 2.2	39891.00
#' 2.9	56642.00
#' 3.0	60150.00
#' 3.2	54445.00
#' 3.2	64445.00
#' 3.7	57189.00
#' 3.9	63218.00
#' 4.0	55794.00
#' 4.0	56957.00
#' 4.1	57081.00
#' 4.5	61111.00
#' 4.9	67938.00
#' 5.1	66029.00
#' 5.3	83088.00
#' 5.9	81363.00
#' 6.0	93940.00
#' 6.8	91738.00
#' 7.1	98273.00
#' 7.9	101302.00
#' 8.2	113812.00
#' 8.7	109431.00
#' 9.0	105582.00
#' 9.5	116969.00
#' 9.6	112635.00
#' 10.3	122391.00
#' 10.5	121872.00

yearsOfExperience <- c(1.1, 1.3, 1.5, 2.0, 2.2, 2.9, 3.0, 3.2, 3.2, 3.7, 3.9, 4.0, 4.0, 4.1, 4.5, 4.9, 5.1, 5.3, 5.9, 6.0, 6.8, 7.1, 7.9, 8.2, 8.7, 9.0, 9.5, 9.6, 10.3, 10.5)
annualSalary <- c(39343.00, 46205.00, 37731.00, 43525.00, 39891.00, 56642.00, 60150.00, 54445.00, 64445.00, 57189.00, 63218.00, 55794.00, 56957.00, 57081.00, 61111.00, 67938.00, 66029.00, 83088.00, 81363.00, 93940.00, 91738.00, 98273.00, 101302.00, 113812.00, 109431.00, 105582.00, 116969.00, 112635.00, 122391.00, 121872.00)

#' Q2. Take a screenshot of the environment variables created.
#' (Include the screenshot in the word document you upload to moodle)
#' Please describe what is the view in which you can see these variables. 
#' 


#' Q3. Print type of each vector
print(typeof(yearsOfExperience))
print(typeof(annualSalary))

#' Q4. Create a dataframe as employees using the same two vectors created in Q1 and 
#' name the columns of the dataframe as yearsOfExperience	and annualSalary
employees <- data.frame(yearsOfExperience, annualSalary)

#' Q5. Create a new column perYearExperience to the dataframe. 
perYearExperience <- annualSalary/yearsOfExperience
employees <- cbind(employees, perYearExperience)
print(employees)
#' The value for this column should be annualSalary/yearsOfExperience


#' Q6. Create a logical vector extractRows of five elements with all the element values as TRUE 
extractRows <- c(1,2,3,4,5)
extractRows[c(TRUE, TRUE, TRUE, TRUE, TRUE)]


#' Q7. Extract first five rows of employees dataframe using the logical vector extractRows
#' Compare and validate the output using head function. Provide a screenshot in the word document. 
employees[extractRows]

#' Q8. Create an integer vector of values 1 to 30 using a sequence operator and name it as filterCriteria
filterCriteria <- c(1:30)

#' Q9. Create a logical vector of 30 elements with every 5th element as TRUE value and rest of the elements as FALSE. 
#' Name the vector the same as filterCriteria 
#' (Hint) filterCriteria can be created with a logical operation such as filterCriteria <- filterCriteria < 8 
#' With the above command, first seven elements are TRUE and rest all are FALSE. Use such an arithmatic operation 
#' to create filterCriteria. 
#' e.g: First 6 elements of the newly created filterCriteria vector should look like 
#' filterCriteria : FALSE FALSE FALSE FALSE TRUE FALSE 

filterCriteria <- filterCriteria *5

#' Q10. Create a new dataframe filteredEmployees from the original dataframe employees using the logical vector
#' filterCriteria created in Q9. 
filteredEmployees <- data.frame(employees)
#' Q11. Display the first 6 records of the dataframe filteredEmployees using head function 
head(filteredEmployees, n = 6)
#' 
#' Q12. Validate that the first record of filteredEmployees dataframe should be the fifth record of the original 
#' dataframe employees. It should be same as the record extracted using the filter criteria 
#' filteredEmployees[filteredEmployees$yearsOfExperience == 2.2,]
#' 

#' Q13. Display the structure of the prebuilt dataset diamonds 
install.packages('ggplot2')
library('ggplot2')
diamonds
str(diamonds)

#' Q14. Display the first 10 records of the diamonds dataset using head() function and override the parameters 
#' Provide explanation for what do I mean by overriding default parameters. 
#' Hint: Look for online help provided by RStudio 
head(diamonds, n = 10)

#' Q15. Create dataframe goodDiamonds from diamonds dataframe with each diamond cut being "Good" 
str(diamonds)
diamonds[c(1,2,3),]
diamonds[diamonds$cut == "Good",]

#' Q16. display unique values of cut colums of diamonds dataframe 
unique(diamonds$cut)

#' Q17. Assume that diamonds is a sales dataset. You would like to give discount as follow: 
#' 10%, 15%, 20% discount on price of Fair, Good, Very Good diamonds and 25% on Premium & Ideal diamonds 
#' Create a new dataframe column with the updated price 
discountedPrice <- function (category, price) {
  if (category == "Good") {
    price <- price - 0.15*price 
  } else if (category == "Fair") {
    price <- price - 0.10*price
  } else if (category == "Very Good")
    price <- price - 0.20*price
} else if (category == "Premium & Ideal diamonds")
    price <- price - 0.25*price


diamonds1$discountedPrice <- discountedPrice(diamonds1$cut, diamonds1$price)
#' Q18. Group by diamonds cut and display the count. Output should look like below: 
#' cut       countDiamonds
#' 
#' 1 Fair               1610
#' 2 Good               4906
#' 3 Very Good         12082
#' 4 Premium           13791
#' 5 Ideal             21551 

#' Q19. Only display data from diamonds that have a cut value of Fair or Good and a price at or under $600
specificDiamonds <- diamonds[discountedPrice]
#' Q20. Display the dimensions of preloaded mtcars dataset 
#' 

#' Q21. Use preloaded mtcars dataset and create a character vector of cars whose mileage is 21.0

#' Q22. Create a factor variable factorCyl using the cyl column of mtcars 
#' with labels as "Four-Cyl", "Six-Cyl", "Eight-Cyl"
mtcars$FactorCyl <- factor(mtcars$cyl,
                           level = c(4,6,8),
                           labels = c("Four-Cyl", "Six-Cyl", "Eight-Cyl"),
                           ordered = FALSE)
str(mtcars)

#' Q23. Create a factor variable factorCyl using the cyl column of mtcars 
#' with labels as "Four-Cyl", "Six-Cyl", "Eight-Cyl". Make the factor of order type 
mtcars$FactorCyl <- factor(mtcars$cyl,
                           level = c(4,6,8),
                           labels = c("Four-Cyl", "Six-Cyl", "Eight-Cyl"),
                           ordered = TRUE)
  
#' Q24. Display unique values of new column factorCyl and write what you observe 
unique(mtcars$FactorCyl)

#' Q25. Use subset function to extract automatic cars into autoCars and manual cars into manualCars dataframe
#' Use the am column of mtcars dataset to separate the records  
manual <- mtcars[mtcars$am == 1,]
manual
auto <- mtcars[mtcars$am == 0,]
auto
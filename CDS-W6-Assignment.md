CDS W6 Assignment
================
Johan Horsmans

# Cultural Data Science Assignemnt 6

I use the p\_load function from the pacman package to load and install
my packages. By using this function I can reduce the following chunk to
only two lines of code:

``` r
library(pacman)
p_load(gapminder, tidyverse)
```

**Assignment 1: Define a defensive function that calculates the Gross
Domestic Product of a nation from the data available in the gapminder
dataset. Using that function, calculate the GDP of Denmark in the
following years: 1967, 1977, 1987, 1997, 2007, and 2017.**

``` r
#I define the following function:
calcGDP <- function(dat, year=NULL, country=NULL) { #The function should take data, year and country as arguments)
  if(!is.null(year)) { #If specified year exists in dataset...
    dat <- dat[dat$year %in% year, ] #...take all the rows in the dataset where the value in the column "year" is equal to the specified year
  }
  if (!is.null(country)) { #If country exist in dataset...
    dat <- dat[dat$country %in% country,] #...take all the rows in the dataset, (from the last "if-statement"), where the value in the column "country" is equal to the specified country
  }
  gdp <- dat$pop * dat$gdpPercap #Multiply "gdpPercap" with "population" (in the dataset) and save the results in a list called "gdp"

  new <- cbind(dat, gdp=gdp) #create a new dataframe, called "new", by adding the "gdp"-list as a column, called "gdp" to the "dat" dataframe (which has been created throughout the function).
  return(new) #return the dataframe "new"
}

#Use the function to solve the assignment.
head(calcGDP(gapminder, c(1967, 1977, 1987, 1997, 2007, 2017) ,country="Mexico"),n=7)
```

    ##   country continent year lifeExp       pop gdpPercap          gdp
    ## 1  Mexico  Americas 1967  60.110  47995559  5754.734 2.762017e+11
    ## 2  Mexico  Americas 1977  65.032  63759976  7674.929 4.893533e+11
    ## 3  Mexico  Americas 1987  69.498  80122492  8688.156 6.961167e+11
    ## 4  Mexico  Americas 1997  73.670  95895146  9767.298 9.366364e+11
    ## 5  Mexico  Americas 2007  76.195 108700891 11977.575 1.301973e+12

The year 2017 is not documented for Mexico and, as such, it is not
returned by the function.

**Assignment 2: Write a script that loops over each country in the
gapminder dataset, tests whether the country starts with a ‘B’ , and
print out whether the life expectancy is smaller than 50, between 50 and
70, or greater than 70.**

``` r
fifty <- 50 #Define lower threshold
seventy <- 70 #Define upper threshold
b_countries <- grep("^B", unique(gapminder$country), value=TRUE) #Create a list with all the countries starting with a "B"

for(i in b_countries){ #Run through this loop for each element in the b_countries list
  life_Expectancy <- mean(gapminder$lifeExp[gapminder$country==i]) #Calculate the mean life expectancy for "i"-country and save it in object "life_Expectancy"
  if(life_Expectancy < fifty){ #If the mean is smaller than fifty
        cat("Average Life Expectancy in", i, "is less than", fifty, "\n") #...print the following sentence and end it with a line break
    }
    else if(life_Expectancy > fifty && life_Expectancy < seventy){ #If the mean is larger than fifty and smaller than seventy
        cat("Average Life Expectancy in", i, "is between", fifty, "and", seventy, "\n") #...print the following sentence and end it with a line break
    }
    else{
        cat("Average Life Expectancy in", i, "is greater than", seventy, "\n") #If the two above statements are not true print the following sentence and end it with a line break.
    }
    rm(life_Expectancy) #Clear the object "life_Expectancy" before running the loop again.
}
```

    ## Average Life Expectancy in Bahrain is between 50 and 70 
    ## Average Life Expectancy in Bangladesh is less than 50 
    ## Average Life Expectancy in Belgium is greater than 70 
    ## Average Life Expectancy in Benin is less than 50 
    ## Average Life Expectancy in Bolivia is between 50 and 70 
    ## Average Life Expectancy in Bosnia and Herzegovina is between 50 and 70 
    ## Average Life Expectancy in Botswana is between 50 and 70 
    ## Average Life Expectancy in Brazil is between 50 and 70 
    ## Average Life Expectancy in Bulgaria is between 50 and 70 
    ## Average Life Expectancy in Burkina Faso is less than 50 
    ## Average Life Expectancy in Burundi is less than 50

1+1

x<-5
y<-1

x+y

z<-x+y

simple_function<-function(number,anothernumber){number + anothernumber}

simple_function(1,1)

simple_function(x,y)

output_variable<-simple_function(x,y)

output_variable


another_simple_function<-function(number,anothernumber){added<-number + anothernumber}
another_simple_function(x,y)


another_simple_function<-function(number,anothernumber){added<-number + anothernumber
                                                        return(added)}
another_simple_function(x,y)

another_simple_function<-function(number,anothernumber){added<-number + anothernumber
                                                        multiplied<-added * number
                                                        return(multiplied)}
another_simple_function(x,y)

# Without variable assignment...
  (5+1) * 5
# Have to make multiple changes if one value changes
  (5+1) * 5
  (6+1) * 6
  (7+1) * 7

# Variable assignment simplifies it a bit, but not reproducable.
  x<-5 # Change x and re-run, easy
  (x+1) * x
  
# Function is much easier for re-use
  another_simple_function(5,1)
  another_simple_function(6,1)
  another_simple_function(7,1)
  

  
simple_function<-function(number,anothernumber){number + anothernumber}
  
nested_function<-function(number,anothernumber){added<-simple_function(number,anothernumber)
                                                multiplied<-added * number
                                                return(multiplied)}

nested_function(5,1)










# Challenge: add 1 to every number between 1 and 5.
1+1
1+2
1+3
1+4
1+5

simple_function(1,1)
simple_function(1,2)
simple_function(1,3)
simple_function(1,4)
simple_function(1,5)

for(i in 1:5){simple_function(1,i)}

for(i in 1:5){print(simple_function(1,i))}


# Make an empty vector: there's nothing there
numbers_added<-vector()

numbers_added

# Fill it with loop output

for(i in 1:5){numbers_added[i]<-simple_function(1,i)}

numbers_added


# Can easily change how many times you do this
numbers_added<-vector()
for(i in 1:50){numbers_added[i]<-simple_function(1,i)}
numbers_added


# Can easily change the numbers involved
numbers_added<-vector()
for(i in 1:50){numbers_added[i]<-simple_function(3,i)}
numbers_added

# Or swap out which function we use
numbers_added<-vector()
for(i in 1:50){numbers_added[i]<-nested_function(3,i)}
numbers_added











# Example script script
# E. Walsh
# Last edited: 16/01/2019
# ----------

# Use the hash to add comments, these will not run.
1+1
# 1+1

## Functions
    # Adds two numbers together
        simple_function<-function(number,anothernumber){number + anothernumber}
    
    # Takes two numbers, returns their sum multiplied by the first input.
        # Relies on: simple_function
        nested_function<-function(number,anothernumber){added<-simple_function(number,anothernumber)
        multiplied<-added * number
        return(multiplied)}

## Analysis
    nested_function(2,3)
    nested_function(5,6)








    
    
# Example script script
# E. Walsh
# Last edited: 16/01/2019
    
# ----------

## Packages
    library(psych) # psych_1.5.8 for descriptives
    
## Functions
    # Adds two numbers together
    simple_function<-function(number,anothernumber){number + anothernumber}
    
    # Takes two numbers, returns their sum multiplied by the first input.
    # Relies on: simple_function
    nested_function<-function(number,anothernumber){added<-simple_function(number,anothernumber)
    multiplied<-added * number
    return(multiplied)}
    
## Analysis
    
    # Make some example data with our functions and loops
      example_data<-vector()
      for(i in 1:50){example_data[i]<-nested_function(3,i)}
      example_data
      
    # Use the 'describe' function from the psych package to get the descriptive 
    # statistics associated with that data
      describe(example_data)
    
    
      source("example_script_containing_functions.R")
    



      
      
      
      
      
      
      
      
      
      
# Sensor data from house 10:25 PM to 8:02 AM 15/1/2019
  # Timestamp is ISO8601 formatted, samples every ~ 5 minutes.
  # Temperature is in Centigrade.
  # Humidity is in %.
sensornet_dat<-read.csv("sensornet-01__16_1_2019.csv")      
      head(sensornet_dat)
      
      
      sensornet_dat$temperature
      sensornet_dat[,"temperature"]
      
      describe(sensornet_dat$temperature)
      describe(sensornet_dat$humidity)
      
      plot(sensornet_dat$hour, sensornet_dat$temperature, type="l")
      plot(sensornet_dat$hour, sensornet_dat$humidity, type="l")

      
      plot(sensornet_dat$temperature, sensornet_dat$humidity)
      
      cor(sensornet_dat$temperature,sensornet_dat$humidity)


      
      
      
      
      
      
      
      
      
      
      
      # Sensor data from house 10:25 PM to 8:02 AM 15/1/2019
      # Timestamp is ISO8601 formatted, samples every ~ 5 minutes.
      # Temperature is in Centigrade.
      # Humidity is in %.
      sensornet_dat<-read.csv("sensornet-01__16_1_2019_new_version.csv")      
      head(sensornet_dat)
      
      
      sensornet_dat$temperature
      sensornet_dat[,"temperature"]
      
      describe(sensornet_dat$temperature)
      describe(sensornet_dat$humidity)
      
      plot(sensornet_dat$hour, sensornet_dat$temperature, type="l")
      plot(sensornet_dat$hour, sensornet_dat$humidity, type="l")
      
      
      plot(sensornet_dat$temperature, sensornet_dat$humidity)
      
      cor(sensornet_dat$temperature,sensornet_dat$humidity)
      
      
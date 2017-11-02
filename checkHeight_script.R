# Script for simple function that checks the difference in height from the sex-
# specific mean for each of the students in the given dataframe
# Date: 24.10.2017
# Author: Jann Goschenhofer

library(dplyr)

# Einlesen der Daten
age = c(19, 22, 21, 23, 22, 20, 28, 25)
weight = c(50, 75, 80, 56, 75, 58, 65, 82)
height = c(1.66, 1.78, 1.90, 1.72, 1.83, 1.68, 1.70, 1.85)
sex = c("F", "M", "M", "F", "M", "F", "F", "M")

# Kombinieren der Daten in einen data frame
students = data.frame(cbind(age, weight, height, sex))

# Transformation der Cols in das richtige Format
students = transform(students, age = as.numeric(as.character(age)))
students = transform(students, height = as.numeric(as.character(height)))
students = transform(students, weight = as.numeric(as.character(weight)))

# Benennung der einzelnen Namen
students$name = c("Maria", "Franz", "Peter", "Lisa", "Hans", "Eva", "Mia", "Karl")

# Funktion zur Berechnung der Groessenunterschiede (pro F / M)
checkHeight3 = function(students.input = students){
  heightVec = apply(students, MARGIN = 1, FUN = function(student){
    if (student["sex"] == "M") {
      mean(students$height[students$sex == "M"]) - as.numeric(student["height"])
    } else {
      mean(students$height[students$sex == "F"]) - as.numeric(student["height"])
    }
  })
  out = data.frame("name" = students$name, "diff" = heightVec)
  return(out)
}

# Ausfuehren der Fkt.
checkHeight3(students.input = students)


   

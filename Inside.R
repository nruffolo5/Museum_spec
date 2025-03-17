# Install necessary packages (if not installed)
# install.packages("readxl")
# install.packages("tidyverse")
# install.packages("tidylog")
# install.packages("pavo")

# Load required libraries
library(readxl)
library(tidyverse)
library(tidylog)
library(pavo)

# Read in the data from museum with shaded light
inside_small <- read_xlsx("Museum_indoors_small.xlsx", skip = 5)

# Removes sd columns
inside_small <- inside_small[,seq(1,175,1)]

# Print column names to identify relevant columns
print(colnames(inside_small))

# Identify columns matching the wavelength pattern
wavelength_cols <- grep("^\\d+\\.\\.\\.", colnames(inside_small), value = TRUE)

# Select relevant columns dynamically
inside_small <- inside_small %>% select(name...1, all_of(wavelength_cols))

# Extract the columns that match the wavelength pattern
print(colnames(inside_small))

# Ensure correct column selection
inside_small <- inside_small %>% select(name...1, all_of(wavelength_cols))

# Create an identifier for each specimen and separate metadata
inside_small <- inside_small %>%
  separate(name...1, into = c("id", "img.number", "date", "time"), sep = "_", extra = "merge", fill = "right")

# Check for duplicate IDs
if (sum(duplicated(inside_small$id)) > 0) {
  message("Warning: Duplicate IDs detected. Making them unique.")
  inside_small <- inside_small %>%
    mutate(id = make.unique(id))
}

# Remove existing row names before assigning new ones
rownames(inside_small) <- NULL  

# Set 'id' as row names
inside_small <- inside_small %>% column_to_rownames(var = "id")

# Remove unnecessary columns
inside_small <- inside_small %>% select(-c("img.number", "date", "time"))

# Transpose (flip) the data frame
inside_small <- t(inside_small)

# Turning inside_small into a base R dataframe
inside_small <- data.frame(inside_small)

# Checking that it has the correct structure
str(inside_small)

# Converting data into a percentage from a proportion
inside_small <- inside_small[,-1] * 100

# Add a column for the wavelengths extracted from perClass Mira
inside_small <- cbind(seq(350, 1002, 4), inside_small)
colnames(inside_small)[1] <- "wl"

# Convert the dataset into an 'rspec' object, which is required for 'pavo'
inside_small <- as.rspec(inside_small, interp = TRUE, lim = c(300,700))

# Convert negative reflectance values to 0
inside_small <- procspec(inside_small, fixneg = "zero")

# Plot the reflectance data
plot(inside_small)

# First plot: General reflectance
plot(inside_small, col = "white", ylim = c(0, max(inside_small[, -1])), main = "Inside: Small Birds Reflectance")
for (i in 2:length(colnames(inside_small))) {
  reflectance <- inside_small[, i]
  lines(x = 350:700, y = reflectance[-c(1:50)], col = spec2rgb(as.rspec(data.frame(wl = inside_small$wl, reflectance = inside_small[, i]))), lwd = 2)
}

# Second plot: Limited range
plot(inside_small, col = "white", ylim = c(0, 20), main = "Inside: Small Birds Reflectance", xlim = c(400, 700))
for (i in 2:length(colnames(inside_small))) {
  reflectance <- inside_small[, i]
  lines(x = 350:700, y = reflectance[-c(1:50)], col = spec2rgb(as.rspec(data.frame(wl = inside_small$wl, reflectance = inside_small[, i]))), lwd = 2)
}

# Third plot: Adjusted transparency
plot(inside_small, col = "white", ylim = c(0, 100), main = "Inside: Small Birds Reflectance", xlim = c(380, 700))
for (i in 2:length(colnames(inside_small))) {
  reflectance <- inside_small[, i]
  lines(x = 350:700, y = reflectance[-c(1:50)], col = adjustcolor(spec2rgb(as.rspec(data.frame(wl= inside_small$wl, reflectance = inside_small[, i]))), 0.5), lwd = 3)
}


###############################################################################################

# Inside big


# Read in the data from museum with shaded light
inside_big <- read_xlsx("Museum_indoors_big.xlsx", skip = 5)

# Removes sd columns
inside_big <- inside_big[,seq(1,175,1)]

# Print column names to identify relevant columns
print(colnames(inside_big))

# Identify columns matching the wavelength pattern
wavelength_cols <- grep("^\\d+\\.\\.\\.", colnames(inside_big), value = TRUE)

# Select relevant columns dynamically
inside_big <- inside_big %>% select(name...1, all_of(wavelength_cols))

# Extract the columns that match the wavelength pattern
print(colnames(inside_big))

# Ensure correct column selection
inside_big <- inside_big %>% select(name...1, all_of(wavelength_cols))

# Create an identifier for each specimen and separate metadata
inside_big <- inside_big %>%
  separate(name...1, into = c("id", "img.number", "date", "time"), sep = "_", extra = "merge", fill = "right")

# Check for duplicate IDs
if (sum(duplicated(inside_big$id)) > 0) {
  message("Warning: Duplicate IDs detected. Making them unique.")
  inside_big <- inside_big %>%
    mutate(id = make.unique(id))
}

# Remove existing row names before assigning new ones
rownames(inside_big) <- NULL  

# Set 'id' as row names
inside_big <- inside_big %>% column_to_rownames(var = "id")

# Remove unnecessary columns
inside_big <- inside_big %>% select(-c("img.number", "date", "time"))

# Transpose (flip) the data frame
inside_big <- t(inside_big)

# Turning inside_big into a base R dataframe
inside_big <- data.frame(inside_big)

# Checking that it has the correct structure
str(inside_big)

# Converting data into a percentage from a proportion
inside_big <- inside_big[,-1] * 100

# Add a column for the wavelengths extracted from perClass Mira
inside_big <- cbind(seq(350, 1002, 4), inside_big)
colnames(inside_big)[1] <- "wl"

# Convert the dataset into an 'rspec' object, which is required for 'pavo'
inside_big <- as.rspec(inside_big, interp = TRUE, lim = c(300,700))

# Convert negative reflectance values to 0
inside_big <- procspec(inside_big, fixneg = "zero")

# Plot the reflectance data
plot(inside_big)

# First plot: General reflectance
plot(inside_big, col = "white", ylim = c(0, max(inside_big[, -1])), main = "Inside: Large Birds Reflectance")
for (i in 2:length(colnames(inside_big))) {
  reflectance <- inside_big[, i]
  lines(x = 350:700, y = reflectance[-c(1:50)], col = spec2rgb(as.rspec(data.frame(wl = inside_big$wl, reflectance = inside_big[, i]))), lwd = 2)
}

# Second plot: Limited range
plot(inside_big, col = "white", ylim = c(0, 50), main = "Inside: Large Birds Reflectance", xlim = c(400, 700))
for (i in 2:length(colnames(inside_big))) {
  reflectance <- inside_big[, i]
  lines(x = 350:700, y = reflectance[-c(1:50)], col = spec2rgb(as.rspec(data.frame(wl = inside_big$wl, reflectance = inside_big[, i]))), lwd = 2)
}

# Third plot: Adjusted transparency
plot(inside_big, col = "white", ylim = c(0, 100), main = "Inside: Large Birds Reflectance", xlim = c(380, 700))
for (i in 2:length(colnames(inside_big))) {
  reflectance <- inside_big[, i]
  lines(x = 350:700, y = reflectance[-c(1:50)], col = adjustcolor(spec2rgb(as.rspec(data.frame(wl= inside_big$wl, reflectance = inside_big[, i]))), 0.5), lwd = 3)
}
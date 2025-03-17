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
shaded_small <- read_xlsx("Outside_shaded_small.xlsx", skip = 5)

# Removes sd columns
shaded_small <- shaded_small[,seq(1,175,1)]

# Print column names to identify relevant columns
print(colnames(shaded_small))

# Identify columns matching the wavelength pattern
wavelength_cols <- grep("^\\d+\\.\\.\\.", colnames(shaded_small), value = TRUE)

# Select relevant columns dynamically
shaded_small <- shaded_small %>% select(name...1, all_of(wavelength_cols))

# Extract the columns that match the wavelength pattern
print(colnames(shaded_small))

# Ensure correct column selection
shaded_small <- shaded_small %>% select(name...1, all_of(wavelength_cols))

# Create an identifier for each specimen and separate metadata
shaded_small <- shaded_small %>%
  separate(name...1, into = c("id", "img.number", "date", "time"), sep = "_", extra = "merge", fill = "right")

# Check for duplicate IDs
if (sum(duplicated(shaded_small$id)) > 0) {
  message("Warning: Duplicate IDs detected. Making them unique.")
  shaded_small <- shaded_small %>%
    mutate(id = make.unique(id))
}

# Remove existing row names before assigning new ones
rownames(shaded_small) <- NULL  

# Set 'id' as row names
shaded_small <- shaded_small %>% column_to_rownames(var = "id")

# Remove unnecessary columns
shaded_small <- shaded_small %>% select(-c("img.number", "date", "time"))

# Transpose (flip) the data frame
shaded_small <- t(shaded_small)

# Turning shaded_small into a base R dataframe
shaded_small <- data.frame(shaded_small)

# Checking that it has the correct structure
str(shaded_small)

# Converting data into a percentage from a proportion
shaded_small <- shaded_small[,-1] * 100

# Add a column for the wavelengths extracted from perClass Mira
shaded_small <- cbind(seq(350, 1002, 4), shaded_small)
colnames(shaded_small)[1] <- "wl"

# Convert the dataset into an 'rspec' object, which is required for 'pavo'
shaded_small <- as.rspec(shaded_small, interp = TRUE, lim = c(300,700))

# Convert negative reflectance values to 0
shaded_small <- procspec(shaded_small, fixneg = "zero")

# Plot the reflectance data
plot(shaded_small)

# First plot: General reflectance
plot(shaded_small, col = "white", ylim = c(0, max(shaded_small[, -1])), main = "Shaded: Small Birds")
for (i in 2:length(colnames(shaded_small))) {
  reflectance <- shaded_small[, i]
  lines(x = 350:700, y = reflectance[-c(1:50)], col = spec2rgb(as.rspec(data.frame(wl = shaded_small$wl, reflectance = shaded_small[, i]))), lwd = 2)
}

# Second plot: Limited range
plot(shaded_small, col = "white", ylim = c(0, 20), main = "Shaded: Small Birds Reflectance", xlim = c(400, 700))
for (i in 2:length(colnames(shaded_small))) {
  reflectance <- shaded_small[, i]
  lines(x = 350:700, y = reflectance[-c(1:50)], col = spec2rgb(as.rspec(data.frame(wl = shaded_small$wl, reflectance = shaded_small[, i]))), lwd = 2)
}

# Third plot: Adjusted transparency
plot(shaded_small, col = "white", ylim = c(0, 100), main = "Shaded: Small Birds Reflectance", xlim = c(380, 700))
for (i in 2:length(colnames(shaded_small))) {
  reflectance <- shaded_small[, i]
  lines(x = 350:700, y = reflectance[-c(1:50)], col = adjustcolor(spec2rgb(as.rspec(data.frame(wl= shaded_small$wl, reflectance = shaded_small[, i]))), 0.5), lwd = 3)
}


#########################################################################################################

# Shaded big

# Read in the data from museum with shaded light
shaded_big <- read_xlsx("Outside_shaded_big.xlsx", skip = 5)

# Removes sd columns
shaded_big <- shaded_big[,seq(1,175,1)]

# Print column names to identify relevant columns
print(colnames(shaded_big))

# Identify columns matching the wavelength pattern
wavelength_cols <- grep("^\\d+\\.\\.\\.", colnames(shaded_big), value = TRUE)

# Select relevant columns dynamically
shaded_big <- shaded_big %>% select(name...1, all_of(wavelength_cols))

# Extract the columns that match the wavelength pattern
print(colnames(shaded_big))

# Ensure correct column selection
shaded_big <- shaded_big %>% select(name...1, all_of(wavelength_cols))

# Create an identifier for each specimen and separate metadata
shaded_big <- shaded_big %>%
  separate(name...1, into = c("id", "img.number", "date", "time"), sep = "_", extra = "merge", fill = "right")

# Check for duplicate IDs
if (sum(duplicated(shaded_big$id)) > 0) {
  message("Warning: Duplicate IDs detected. Making them unique.")
  shaded_big <- shaded_big %>%
    mutate(id = make.unique(id))
}

# Remove existing row names before assigning new ones
rownames(shaded_big) <- NULL  

# Set 'id' as row names
shaded_big <- shaded_big %>% column_to_rownames(var = "id")

# Remove unnecessary columns
shaded_big <- shaded_big %>% select(-c("img.number", "date", "time"))

# Transpose (flip) the data frame
shaded_big <- t(shaded_big)

# Turning shaded_big into a base R dataframe
shaded_big <- data.frame(shaded_big)

# Checking that it has the correct structure
str(shaded_big)

# Converting data into a percentage from a proportion
shaded_big <- shaded_big[,-1] * 100

# Add a column for the wavelengths extracted from perClass Mira
shaded_big <- cbind(seq(350, 1002, 4), shaded_big)
colnames(shaded_big)[1] <- "wl"

# Convert the dataset into an 'rspec' object, which is required for 'pavo'
shaded_big <- as.rspec(shaded_big, interp = TRUE, lim = c(300,700))

# Convert negative reflectance values to 0
shaded_big <- procspec(shaded_big, fixneg = "zero")

# Plot the reflectance data
plot(shaded_big)

# First plot: General reflectance
plot(shaded_big, col = "white", ylim = c(0, max(shaded_big[, -1])), main = "Shaded: Large Birds Reflectance")
for (i in 2:length(colnames(shaded_big))) {
  reflectance <- shaded_big[, i]
  lines(x = 350:700, y = reflectance[-c(1:50)], col = spec2rgb(as.rspec(data.frame(wl = shaded_big$wl, reflectance = shaded_big[, i]))), lwd = 2)
}

# Second plot: Limited range
plot(shaded_big, col = "white", ylim = c(0, 50), main = "Shaded: Large Birds Reflectance", xlim = c(400, 700))
for (i in 2:length(colnames(shaded_big))) {
  reflectance <- shaded_big[, i]
  lines(x = 350:700, y = reflectance[-c(1:50)], col = spec2rgb(as.rspec(data.frame(wl = shaded_big$wl, reflectance = shaded_big[, i]))), lwd = 2)
}

# Third plot: Adjusted transparency
plot(shaded_big, col = "white", ylim = c(0, 100), main = "Shaded: Large Birds Reflectance", xlim = c(380, 700))
for (i in 2:length(colnames(shaded_big))) {
  reflectance <- shaded_big[, i]
  lines(x = 350:700, y = reflectance[-c(1:50)], col = adjustcolor(spec2rgb(as.rspec(data.frame(wl= shaded_big$wl, reflectance = shaded_big[, i]))), 0.5), lwd = 3)
}
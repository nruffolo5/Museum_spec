# Install necessary packages (if not installed)
install.packages("readxl")
install.packages("tidyverse")
install.packages("tidylog")
install.packages("pavo")

# Load required libraries
library(readxl)
library(tidyverse)
library(tidylog)
library(pavo)

# Read in the data from the Dickey Collection
outside_small <- read_xlsx("Outside_small.xlsx", skip = 5)

# Print column names to identify relevant columns
print(colnames(outside_small))

# Identify columns matching the wavelength pattern
wavelength_cols <- grep("^\\d+\\.\\.\\.", colnames(outside_small), value = TRUE)

# Select relevant columns dynamically
outside_small <- outside_small %>% select(name...1, all_of(wavelength_cols))

# Extract the columns that match the wavelength pattern
print(colnames(outside_small))

# Ensure correct column selection
outside_small <- outside_small %>% select(name...1, all_of(wavelength_cols))

# Create an identifier for each specimen and separate metadata
outside_small <- outside_small %>%
  separate(name...1, into = c("id", "img.number", "date", "time"), sep = "_", extra = "merge", fill = "right")

# Check for duplicate IDs
if (sum(duplicated(outside_small$id)) > 0) {
  message("Warning: Duplicate IDs detected. Making them unique.")
  outside_small <- outside_small %>%
    mutate(id = make.unique(id))
}

# Remove existing row names before assigning new ones
rownames(outside_small) <- NULL  

# Set 'id' as row names
outside_small <- outside_small %>% column_to_rownames(var = "id")

# Remove unnecessary columns
outside_small <- outside_small %>% select(-c("img.number", "date", "time"))

# Transpose (flip) the data frame
outside_small <- t(outside_small)

# Add a column for the wavelengths extracted from perClass Mira
outside_small <- cbind(seq(350, 702, 4), outside_small)
colnames(outside_small)[1] <- "wl"

# Convert the dataset into an 'rspec' object, which is required for 'pavo'
outside_small <- as.rspec(outside_small, interp = TRUE, lim = c(300,700))

# Convert negative reflectance values to 0
outside_small <- procspec(outside_small, fixneg = "zero")

# Plot the reflectance data
plot(outside_small)

# First plot: General reflectance
plot(outside_small, col = "white", ylim = c(0, max(outside_small[, -1] * 100)), main = "Grackle Throat Reflectance")
for (i in 2:length(colnames(outside_small))) {
  reflectance <- outside_small[, i]
  lines(x = 350:700, y = reflectance[-c(1:50)] * 100, col = spec2rgb(as.rspec(data.frame(wl = outside_small$wl, reflectance = outside_small[, i]))), lwd = 2)
}

# Second plot: Limited range
plot(outside_small, col = "white", ylim = c(0, 20), main = "Outside Small Birds Reflectance", xlim = c(400, 700))
for (i in 2:length(colnames(outside_small))) {
  reflectance <- outside_small[, i]
  lines(x = 350:700, y = reflectance[-c(1:50)] * 100, col = spec2rgb(as.rspec(data.frame(wl = outside_small$wl, reflectance = outside_small[, i]))), lwd = 2)
}

# Third plot: Adjusted transparency
plot(outside_small, col = "white", ylim = c(0, 100), main = "Outside Small Birds Reflectance", xlim = c(380, 700))
for (i in 2:length(colnames(outside_small))) {
  reflectance <- outside_small[, i]
  lines(x = 350:700, y = reflectance[-c(1:50)] * 100, col = adjustcolor(spec2rgb(as.rspec(data.frame(wl= outside_small$wl, reflectance = outside_small[, i]))), 0.5), lwd = 3)
}




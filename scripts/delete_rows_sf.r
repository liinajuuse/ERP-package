# Fake data frame
df <- data.frame('Electrode' = c(rep('FZ', 6), rep('P1', 4), rep('F7', 3)))
df$Sex <- c(rep('m', 6), rep('f', 7))
df$Group <- rep('ee', nrow(df))
df$T_1 <- c(100, -153, 143, -142, -222, 136, -109, 119, -190, 127, 259, -299, 0)
df$T_2 <- c(-625, -133, -163, 42, 122, -126, 301, 219, -195, -187, 159, -211, 100)
df$T_3 <- c(-117, -133, 163, 142, -212, 126, 1-19, -219, 195, -187, 59, 221, -121)

df # according to your preference, rows 1, 7 and 13 must be deleted

# get the only the columns that you want to apply the filters
df.sub <- df[, 4:ncol(df)] # the first "T_" column in the fake data frame is the fourth

# Replace the "undesired values" by NAs
df.sub[df.sub < -300] <- NA
df.sub[df.sub > 300] <- NA
df.sub[df.sub == 0] <- NA

# Now, bind the original data frame again
df <- cbind(df[, 1:4], df.sub)

# And then, delete all the rows containing NAs. The row will be deleted if it has a NA in any position (column) within the data frame
df <- df[complete.cases(df), ]

df # rows 1, 7 and 13 were deleted

rownames(df) <- NULL # I like to reset the row names every time I apply a filter to delete rows in a data frame, just to keep it more organised

df
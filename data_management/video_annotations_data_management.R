# Writing a script that pulls in the necessary bounding box and label information for each frame image from the video annotation JSON files
# Loading in packages
library(jsonlite)
library(stringr)
library(dplyr)
library(imager)


# The JSON files I am interested in are Video Annotations-April-27-2019, Video Annotations Post-Upgrade-May-24-2019, and 
# Video Annotations Auto-Save-September-08-2019. These are all structured slightly differently, so will have to be handled carefully.
# I would like to organize all of the data in a dataframe with the following columns:
annotations <- data.frame(video_name = character(),
                          annotation_number = integer(),
                          annotation_label = character(),
                          frame_number = integer(),
                          bb_upleft_coordx = numeric(),
                          bb_upleft_coordy = numeric(),
                          bb_width = numeric(),
                          bb_height = numeric(),
                          stringsAsFactors = FALSE)

# Reading in the Video Annotations-April-27-2019 JSON file                    
json1 <- fromJSON("Video Annotations-April-27-2019.json", flatten = TRUE)
# The extra label numbers (beyond 0 and 1) are from some annotation objects in the practice videos being numbered strangely.
# These should be treated as separate annotation objects and stacked onto the rest of the data.

# Stripping the video names in the `Labeled Data` column to just be the video name rather than the Dropbox URL
json1$`Labeled Data` <- sapply(str_split(json1$`Labeled Data`, "/"), function(x) x[[6]])
json1$`Labeled Data` <- sapply(str_split(json1$`Labeled Data`, ".mp4"), function(x) x[[1]])

# Renaming and reformatting some of the columns in JSON1
colnames(json1)[colnames(json1) == "Labeled Data"] <- "video_name"
json1$Frame <- as.integer(json1$Frame)
colnames(json1)[colnames(json1) == "Frame"] <- "frame_number"

# Removing the ID column in JSON1 as it is not necessary
json1$ID <- NULL

# Creating separate datasets for each unique annotation object (face) within a video
json1_0 <- subset(json1, !is.na(json1$Label.0.class))
null_columns <- c("Label.1.class", "Label.1.geometry", "Label.1.attributes.explicit", "Label.1.attributes.0", "Label.1.attributes.1", 
                  "Label.2.class", "Label.2.geometry", "Label.2.attributes.explicit", "Label.2.attributes.1", "Label.2.attributes.0", 
                  "Label.6.class", "Label.6.geometry", "Label.6.attributes.explicit", "Label.6.attributes.0", 
                  "Label.7.class", "Label.7.geometry", "Label.7.attributes.explicit", "Label.7.attributes.1", 
                  "Label.8.class", "Label.8.geometry", "Label.8.attributes.explicit", "Label.8.attributes.0", 
                  "Label.5.class", "Label.5.geometry", "Label.5.attributes.explicit", "Label.5.attributes.0")
json1_0[, colnames(json1_0) %in% null_columns] <- NULL

json1_1 <- subset(json1, !is.na(json1$Label.1.class))
null_columns <- c("Label.0.class", "Label.0.geometry", "Label.0.attributes.explicit", "Label.0.attributes.1", "Label.0.attributes.0", 
                  "Label.2.class", "Label.2.geometry", "Label.2.attributes.explicit", "Label.2.attributes.1", "Label.2.attributes.0", 
                  "Label.6.class", "Label.6.geometry", "Label.6.attributes.explicit", "Label.6.attributes.0", 
                  "Label.7.class", "Label.7.geometry", "Label.7.attributes.explicit", "Label.7.attributes.1", 
                  "Label.8.class", "Label.8.geometry", "Label.8.attributes.explicit", "Label.8.attributes.0", 
                  "Label.5.class", "Label.5.geometry", "Label.5.attributes.explicit", "Label.5.attributes.0")
json1_1[, colnames(json1_1) %in% null_columns] <- NULL

json1_2 <- subset(json1, !is.na(json1$Label.2.class))
null_columns <- c("Label.0.class", "Label.0.geometry", "Label.0.attributes.explicit", "Label.0.attributes.1", "Label.0.attributes.0", 
                  "Label.1.class", "Label.1.geometry", "Label.1.attributes.explicit", "Label.1.attributes.0", "Label.1.attributes.1", 
                  "Label.6.class", "Label.6.geometry", "Label.6.attributes.explicit", "Label.6.attributes.0", 
                  "Label.7.class", "Label.7.geometry", "Label.7.attributes.explicit", "Label.7.attributes.1", 
                  "Label.8.class", "Label.8.geometry", "Label.8.attributes.explicit", "Label.8.attributes.0", 
                  "Label.5.class", "Label.5.geometry", "Label.5.attributes.explicit", "Label.5.attributes.0")
json1_2[, colnames(json1_2) %in% null_columns] <- NULL

json1_5 <- subset(json1, !is.na(json1$Label.5.class))
null_columns <- c("Label.0.class", "Label.0.geometry", "Label.0.attributes.explicit", "Label.0.attributes.1", "Label.0.attributes.0", 
                  "Label.1.class", "Label.1.geometry", "Label.1.attributes.explicit", "Label.1.attributes.0", "Label.1.attributes.1",
                  "Label.2.class", "Label.2.geometry", "Label.2.attributes.explicit", "Label.2.attributes.1", "Label.2.attributes.0",
                  "Label.6.class", "Label.6.geometry", "Label.6.attributes.explicit", "Label.6.attributes.0", 
                  "Label.7.class", "Label.7.geometry", "Label.7.attributes.explicit", "Label.7.attributes.1", 
                  "Label.8.class", "Label.8.geometry", "Label.8.attributes.explicit", "Label.8.attributes.0")
json1_5[, colnames(json1_5) %in% null_columns] <- NULL

json1_6 <- subset(json1, !is.na(json1$Label.6.class))
null_columns <- c("Label.0.class", "Label.0.geometry", "Label.0.attributes.explicit", "Label.0.attributes.1", "Label.0.attributes.0", 
                  "Label.1.class", "Label.1.geometry", "Label.1.attributes.explicit", "Label.1.attributes.0", "Label.1.attributes.1",
                  "Label.2.class", "Label.2.geometry", "Label.2.attributes.explicit", "Label.2.attributes.1", "Label.2.attributes.0",
                  "Label.7.class", "Label.7.geometry", "Label.7.attributes.explicit", "Label.7.attributes.1", 
                  "Label.8.class", "Label.8.geometry", "Label.8.attributes.explicit", "Label.8.attributes.0",
                  "Label.5.class", "Label.5.geometry", "Label.5.attributes.explicit", "Label.5.attributes.0")
json1_6[, colnames(json1_6) %in% null_columns] <- NULL              

json1_7 <- subset(json1, !is.na(json1$Label.7.class))
null_columns <- c("Label.0.class", "Label.0.geometry", "Label.0.attributes.explicit", "Label.0.attributes.1", "Label.0.attributes.0", 
                  "Label.1.class", "Label.1.geometry", "Label.1.attributes.explicit", "Label.1.attributes.0", "Label.1.attributes.1",
                  "Label.2.class", "Label.2.geometry", "Label.2.attributes.explicit", "Label.2.attributes.1", "Label.2.attributes.0",
                  "Label.6.class", "Label.6.geometry", "Label.6.attributes.explicit", "Label.6.attributes.0", 
                  "Label.8.class", "Label.8.geometry", "Label.8.attributes.explicit", "Label.8.attributes.0",
                  "Label.5.class", "Label.5.geometry", "Label.5.attributes.explicit", "Label.5.attributes.0")
json1_7[, colnames(json1_7) %in% null_columns] <- NULL 

json1_8 <- subset(json1, !is.na(json1$Label.8.class))
null_columns <- c("Label.0.class", "Label.0.geometry", "Label.0.attributes.explicit", "Label.0.attributes.1", "Label.0.attributes.0", 
                  "Label.1.class", "Label.1.geometry", "Label.1.attributes.explicit", "Label.1.attributes.0", "Label.1.attributes.1",
                  "Label.2.class", "Label.2.geometry", "Label.2.attributes.explicit", "Label.2.attributes.1", "Label.2.attributes.0",
                  "Label.6.class", "Label.6.geometry", "Label.6.attributes.explicit", "Label.6.attributes.0", 
                  "Label.7.class", "Label.7.geometry", "Label.7.attributes.explicit", "Label.7.attributes.1",
                  "Label.5.class", "Label.5.geometry", "Label.5.attributes.explicit", "Label.5.attributes.0")
json1_8[, colnames(json1_8) %in% null_columns] <- NULL 

# Removing columns no longer needed in the separate datasets for each unique annotation object, and renaming some columns
json1_0$Label.0.class <- NULL
json1_0$Label.0.attributes.explicit <- NULL
json1_0$annotation_label[!is.na(json1_0$Label.0.attributes.0)] <- json1_0$Label.0.attributes.0[!is.na(json1_0$Label.0.attributes.0)]
json1_0$annotation_label[!is.na(json1_0$Label.0.attributes.1)] <- json1_0$Label.0.attributes.1[!is.na(json1_0$Label.0.attributes.1)]
json1_0$Label.0.attributes.0 <- NULL
json1_0$Label.0.attributes.1 <- NULL
colnames(json1_0)[colnames(json1_0) == "Label.0.geometry"] <- "bb_coords"
json1_0$annotation_number <- NA
json1_0 <- json1_0[c("video_name", "annotation_number", "annotation_label", "frame_number", "bb_coords")]

json1_1$Label.1.class <- NULL
json1_1$Label.1.attributes.explicit <- NULL
json1_1$annotation_label[!is.na(json1_1$Label.1.attributes.0)] <- json1_1$Label.1.attributes.0[!is.na(json1_1$Label.1.attributes.0)]
json1_1$annotation_label[!is.na(json1_1$Label.1.attributes.1)] <- json1_1$Label.1.attributes.1[!is.na(json1_1$Label.1.attributes.1)]
json1_1$Label.1.attributes.0 <- NULL
json1_1$Label.1.attributes.1 <- NULL
colnames(json1_1)[colnames(json1_1) == "Label.1.geometry"] <- "bb_coords"
json1_1$annotation_number <- NA
json1_1 <- json1_1[c("video_name", "annotation_number", "annotation_label", "frame_number", "bb_coords")]

json1_2$Label.2.class <- NULL
json1_2$Label.2.attributes.explicit <- NULL
json1_2$annotation_label[!is.na(json1_2$Label.2.attributes.0)] <- json1_2$Label.2.attributes.0[!is.na(json1_2$Label.2.attributes.0)]
json1_2$annotation_label[!is.na(json1_2$Label.2.attributes.1)] <- json1_2$Label.2.attributes.1[!is.na(json1_2$Label.2.attributes.1)]
json1_2$Label.2.attributes.0 <- NULL
json1_2$Label.2.attributes.1 <- NULL
colnames(json1_2)[colnames(json1_2) == "Label.2.geometry"] <- "bb_coords"
json1_2$annotation_number <- NA
json1_2 <- json1_2[c("video_name", "annotation_number", "annotation_label", "frame_number", "bb_coords")]

json1_5$Label.5.class <- NULL
json1_5$Label.5.attributes.explicit <- NULL
json1_5$annotation_label[!is.na(json1_5$Label.5.attributes.0)] <- json1_5$Label.5.attributes.0[!is.na(json1_5$Label.5.attributes.0)]
json1_5$Label.5.attributes.0 <- NULL
colnames(json1_5)[colnames(json1_5) == "Label.5.geometry"] <- "bb_coords"
json1_5$annotation_number <- NA
json1_5 <- json1_5[c("video_name", "annotation_number", "annotation_label", "frame_number", "bb_coords")]

json1_6$Label.6.class <- NULL
json1_6$Label.6.attributes.explicit <- NULL
json1_6$annotation_label[!is.na(json1_6$Label.6.attributes.0)] <- json1_6$Label.6.attributes.0[!is.na(json1_6$Label.6.attributes.0)]
json1_6$Label.6.attributes.0 <- NULL
colnames(json1_6)[colnames(json1_6) == "Label.6.geometry"] <- "bb_coords"
json1_6$annotation_number <- NA
json1_6 <- json1_6[c("video_name", "annotation_number", "annotation_label", "frame_number", "bb_coords")]

json1_7$Label.7.class <- NULL
json1_7$Label.7.attributes.explicit <- NULL
json1_7$annotation_label[!is.na(json1_7$Label.7.attributes.1)] <- json1_7$Label.7.attributes.1[!is.na(json1_7$Label.7.attributes.1)]
json1_7$Label.7.attributes.1 <- NULL
colnames(json1_7)[colnames(json1_7) == "Label.7.geometry"] <- "bb_coords"
json1_7$annotation_number <- NA
json1_7 <- json1_7[c("video_name", "annotation_number", "annotation_label", "frame_number", "bb_coords")]

json1_8$Label.8.class <- NULL
json1_8$Label.8.attributes.explicit <- NULL
json1_8$annotation_label[!is.na(json1_8$Label.8.attributes.0)] <- json1_8$Label.8.attributes.0[!is.na(json1_8$Label.8.attributes.0)]
json1_8$Label.8.attributes.0 <- NULL
colnames(json1_8)[colnames(json1_8) == "Label.8.geometry"] <- "bb_coords"
json1_8$annotation_number <- NA
json1_8 <- json1_8[c("video_name", "annotation_number", "annotation_label", "frame_number", "bb_coords")]

# Adding an annotation_number to each video based on how many unique annotation objects are present in a video
json1_0$annotation_number <- 1
json1_1$annotation_number <- 1
json1_1$annotation_number[json1_1$video_name %in% json1_0$video_name] <- json1_1$annotation_number[json1_1$video_name %in% json1_0$video_name] + 1
json1_2$annotation_number <- 1
json1_2$annotation_number[json1_2$video_name %in% json1_0$video_name] <- json1_2$annotation_number[json1_2$video_name %in% json1_0$video_name] + 1
json1_2$annotation_number[json1_2$video_name %in% json1_1$video_name] <- json1_2$annotation_number[json1_2$video_name %in% json1_1$video_name] + 1
json1_5$annotation_number <- 1
json1_5$annotation_number[json1_5$video_name %in% json1_0$video_name] <- json1_5$annotation_number[json1_5$video_name %in% json1_0$video_name] + 1
json1_5$annotation_number[json1_5$video_name %in% json1_1$video_name] <- json1_5$annotation_number[json1_5$video_name %in% json1_1$video_name] + 1
json1_5$annotation_number[json1_5$video_name %in% json1_2$video_name] <- json1_5$annotation_number[json1_5$video_name %in% json1_2$video_name] + 1
json1_6$annotation_number <- 1
json1_6$annotation_number[json1_6$video_name %in% json1_0$video_name] <- json1_6$annotation_number[json1_6$video_name %in% json1_0$video_name] + 1
json1_6$annotation_number[json1_6$video_name %in% json1_1$video_name] <- json1_6$annotation_number[json1_6$video_name %in% json1_1$video_name] + 1
json1_6$annotation_number[json1_6$video_name %in% json1_2$video_name] <- json1_6$annotation_number[json1_6$video_name %in% json1_2$video_name] + 1
json1_6$annotation_number[json1_6$video_name %in% json1_5$video_name] <- json1_6$annotation_number[json1_6$video_name %in% json1_5$video_name] + 1
json1_7$annotation_number <- 1
json1_7$annotation_number[json1_7$video_name %in% json1_0$video_name] <- json1_7$annotation_number[json1_7$video_name %in% json1_0$video_name] + 1
json1_7$annotation_number[json1_7$video_name %in% json1_1$video_name] <- json1_7$annotation_number[json1_7$video_name %in% json1_1$video_name] + 1
json1_7$annotation_number[json1_7$video_name %in% json1_2$video_name] <- json1_7$annotation_number[json1_7$video_name %in% json1_2$video_name] + 1
json1_7$annotation_number[json1_7$video_name %in% json1_5$video_name] <- json1_7$annotation_number[json1_7$video_name %in% json1_5$video_name] + 1
json1_7$annotation_number[json1_7$video_name %in% json1_6$video_name] <- json1_7$annotation_number[json1_7$video_name %in% json1_6$video_name] + 1
json1_8$annotation_number <- 1
json1_8$annotation_number[json1_8$video_name %in% json1_0$video_name] <- json1_8$annotation_number[json1_8$video_name %in% json1_0$video_name] + 1
json1_8$annotation_number[json1_8$video_name %in% json1_1$video_name] <- json1_8$annotation_number[json1_8$video_name %in% json1_1$video_name] + 1
json1_8$annotation_number[json1_8$video_name %in% json1_2$video_name] <- json1_8$annotation_number[json1_8$video_name %in% json1_2$video_name] + 1
json1_8$annotation_number[json1_8$video_name %in% json1_5$video_name] <- json1_8$annotation_number[json1_8$video_name %in% json1_5$video_name] + 1
json1_8$annotation_number[json1_8$video_name %in% json1_6$video_name] <- json1_8$annotation_number[json1_8$video_name %in% json1_6$video_name] + 1
json1_8$annotation_number[json1_8$video_name %in% json1_7$video_name] <- json1_8$annotation_number[json1_8$video_name %in% json1_7$video_name] + 1

# Stacking the separate datasets for each unique annotation object
json1 <- rbind(json1_0, json1_1, json1_2, json1_5, json1_6, json1_7, json1_8)
json1$annotation_number <- as.integer(json1$annotation_number)

# Extracting the bounding box coordinates to separate variables in JSON1
for (i in seq_along(json1$bb_coords)){
  json1$bb_upleft_coordx[i] <- json1$bb_coords[[i]][1]
}

for (i in seq_along(json1$bb_coords)){
  json1$bb_upleft_coordy[i] <- json1$bb_coords[[i]][2]
}

for (i in seq_along(json1$bb_coords)){
  json1$bb_width[i] <- json1$bb_coords[[i]][3]
}

for (i in seq_along(json1$bb_coords)){
  json1$bb_height[i] <- json1$bb_coords[[i]][4]
}

json1$bb_coords <- NULL

# Ordering JSON1 by the video_name, annotation_number, and frame_number
json1 <- json1[order(json1$video_name, json1$annotation_number, json1$frame_number) ,]


# Reading in the Video Annotations Post-Upgrade-May-24-2019 JSON file
json2 <- fromJSON("Video Annotations Post-Upgrade-May-24-2019.json", flatten = TRUE)

# Stripping the video names in the `Labeled Data` column to just be the video name rather than the Dropbox URL
json2$`Labeled Data` <- sapply(str_split(json2$`Labeled Data`, "/"), function(x) x[[6]])
json2$`Labeled Data` <- sapply(str_split(json2$`Labeled Data`, ".mp4"), function(x) x[[1]])

# Renaming and reformatting some of the columns in JSON2
colnames(json2)[colnames(json2) == "Labeled Data"] <- "video_name"
json2$Frame <- as.integer(json2$Frame)
colnames(json2)[colnames(json2) == "Frame"] <- "frame_number"

# Removing the ID column in JSON2 as it is not necessary
json2$ID <- NULL

# Creating separate datasets for each unique annotation object (face) within a video
json2_0 <- subset(json2, !is.na(json2$Label.0.class))
null_columns <- c("Label.1.class", "Label.1.geometry", "Label.1.attributes.explicit", "Label.1.attributes.implicit", "Label.1.attributes.1", "Label.1.attributes.0", 
                  "Label.2.class", "Label.2.geometry", "Label.2.attributes.explicit", "Label.2.attributes.implicit", "Label.2.attributes.0")
json2_0[, colnames(json2_0) %in% null_columns] <- NULL

json2_1 <- subset(json2, !is.na(json2$Label.1.class))
null_columns <- c("Label.0.class", "Label.0.geometry", "Label.0.attributes.explicit", "Label.0.attributes.1",
                  "Label.2.class", "Label.2.geometry", "Label.2.attributes.explicit", "Label.2.attributes.implicit", "Label.2.attributes.0")
json2_1[, colnames(json2_1) %in% null_columns] <- NULL

json2_2 <- subset(json2, !is.na(json2$Label.2.class))
null_columns <- c("Label.0.class", "Label.0.geometry", "Label.0.attributes.explicit", "Label.0.attributes.1", 
                  "Label.1.class", "Label.1.geometry", "Label.1.attributes.explicit", "Label.1.attributes.implicit", "Label.1.attributes.1", "Label.1.attributes.0")
json2_2[, colnames(json2_2) %in% null_columns] <- NULL

# Removing columns no longer needed in the separate datasets for each unique annotation object, and renaming some columns
json2_0$Label.0.class <- NULL
json2_0$Label.0.attributes.explicit <- NULL
json2_0$annotation_label[!is.na(json2_0$Label.0.attributes.1)] <- json2_0$Label.0.attributes.1[!is.na(json2_0$Label.0.attributes.1)]
json2_0$Label.0.attributes.1 <- NULL
colnames(json2_0)[colnames(json2_0) == "Label.0.geometry"] <- "bb_coords"
json2_0$annotation_number <- NA
json2_0 <- json2_0[c("video_name", "annotation_number", "annotation_label", "frame_number", "bb_coords")]

json2_1$Label.1.class <- NULL
json2_1$Label.1.attributes.explicit <- NULL
json2_1$Label.1.attributes.implicit <- NULL
json2_1$annotation_label[!is.na(json2_1$Label.1.attributes.0)] <- json2_1$Label.1.attributes.0[!is.na(json2_1$Label.1.attributes.0)]
json2_1$annotation_label[!is.na(json2_1$Label.1.attributes.1)] <- json2_1$Label.1.attributes.1[!is.na(json2_1$Label.1.attributes.1)]
json2_1$Label.1.attributes.0 <- NULL
json2_1$Label.1.attributes.1 <- NULL
colnames(json2_1)[colnames(json2_1) == "Label.1.geometry"] <- "bb_coords"
json2_1$annotation_number <- NA
json2_1 <- json2_1[c("video_name", "annotation_number", "annotation_label", "frame_number", "bb_coords")]

json2_2$Label.2.class <- NULL
json2_2$Label.2.attributes.explicit <- NULL
json2_2$Label.2.attributes.implicit <- NULL
json2_2$annotation_label[!is.na(json2_2$Label.2.attributes.0)] <- json2_2$Label.2.attributes.0[!is.na(json2_2$Label.2.attributes.0)]
json2_2$Label.2.attributes.0 <- NULL
colnames(json2_2)[colnames(json2_2) == "Label.2.geometry"] <- "bb_coords"
json2_2$annotation_number <- NA
json2_2 <- json2_2[c("video_name", "annotation_number", "annotation_label", "frame_number", "bb_coords")]

# Adding an annotation_number to each video based on how many unique annotation objects are present in a video
json2_0$annotation_number <- 1
json2_1$annotation_number <- 1
json2_1$annotation_number[json2_1$video_name %in% json2_0$video_name] <- json2_1$annotation_number[json2_1$video_name %in% json2_0$video_name] + 1
json2_2$annotation_number <- 1
json2_2$annotation_number[json2_2$video_name %in% json2_0$video_name] <- json2_2$annotation_number[json2_2$video_name %in% json2_0$video_name] + 1
json2_2$annotation_number[json2_2$video_name %in% json2_1$video_name] <- json2_2$annotation_number[json2_2$video_name %in% json2_1$video_name] + 1

# Stacking the separate datasets for each unique annotation object
json2 <- rbind(json2_0, json2_1, json2_2)
json2$annotation_number <- as.integer(json2$annotation_number)

# Extracting the bounding box coordinates to separate variables in JSON2
for (i in seq_along(json2$bb_coords)){
  json2$bb_upleft_coordx[i] <- json2$bb_coords[[i]][1]
}

for (i in seq_along(json2$bb_coords)){
  json2$bb_upleft_coordy[i] <- json2$bb_coords[[i]][2]
}

for (i in seq_along(json2$bb_coords)){
  json2$bb_width[i] <- json2$bb_coords[[i]][3]
}

for (i in seq_along(json2$bb_coords)){
  json2$bb_height[i] <- json2$bb_coords[[i]][4]
}

json2$bb_coords <- NULL

# Ordering JSON2 by the video_name, annotation_number, and frame_number
json2 <- json2[order(json2$video_name, json2$annotation_number, json2$frame_number) ,]


# Reading in the Video Annotations Auto-Save-September-08-2019 JSON file
json3 <- fromJSON("Video Annotations Auto-Save-September-08-2019.json", flatten = TRUE)

# Stripping the video names in the `Labeled Data` column to just be the video name rather than the Dropbox URL
json3$`Labeled Data` <- sapply(str_split(json3$`Labeled Data`, "/"), function(x) x[[6]])
json3$`Labeled Data` <- sapply(str_split(json3$`Labeled Data`, ".mp4"), function(x) x[[1]])

# Renaming and reformatting some of the columns in JSON3
colnames(json3)[colnames(json3) == "Labeled Data"] <- "video_name"
json3$Frame <- as.integer(json3$Frame)
colnames(json3)[colnames(json3) == "Frame"] <- "frame_number"

# Removing the ID column in JSON3 as it is not necessary
json3$ID <- NULL

# Creating separate datasets for each unique annotation object (face) within a video
json3_0 <- subset(json3, !is.na(json3$Label.0.class))
null_columns <- c("Label.1.class", "Label.1.geometry", "Label.1.attributes.explicit", "Label.1.attributes.implicit", "Label.1.attributes.0", 
                  "Label.2.class", "Label.2.geometry", "Label.2.attributes.explicit", "Label.2.attributes.implicit", "Label.2.attributes.0")
json3_0[, colnames(json3_0) %in% null_columns] <- NULL

json3_1 <- subset(json3, !is.na(json3$Label.1.class))
null_columns <- c("Label.0.class", "Label.0.geometry", "Label.0.attributes.explicit", "Label.0.attributes.implicit", "Label.0.attributes.0",
                  "Label.2.class", "Label.2.geometry", "Label.2.attributes.explicit", "Label.2.attributes.implicit", "Label.2.attributes.0")
json3_1[, colnames(json3_1) %in% null_columns] <- NULL

json3_2 <- subset(json3, !is.na(json3$Label.2.class))
null_columns <- c("Label.0.class", "Label.0.geometry", "Label.0.attributes.explicit", "Label.0.attributes.implicit", "Label.0.attributes.0", 
                  "Label.1.class", "Label.1.geometry", "Label.1.attributes.explicit", "Label.1.attributes.implicit", "Label.1.attributes.0")
json3_2[, colnames(json3_2) %in% null_columns] <- NULL

# Removing columns no longer needed in the separate datasets for each unique annotation object, and renaming some columns
json3_0$Label.0.class <- NULL
json3_0$Label.0.attributes.explicit <- NULL
json3_0$Label.0.attributes.implicit <- NULL
json3_0$annotation_label[!is.na(json3_0$Label.0.attributes.0)] <- json3_0$Label.0.attributes.0[!is.na(json3_0$Label.0.attributes.0)]
json3_0$Label.0.attributes.0 <- NULL
colnames(json3_0)[colnames(json3_0) == "Label.0.geometry"] <- "bb_coords"
json3_0$annotation_number <- NA
json3_0 <- json3_0[c("video_name", "annotation_number", "annotation_label", "frame_number", "bb_coords")]

json3_1$Label.1.class <- NULL
json3_1$Label.1.attributes.explicit <- NULL
json3_1$Label.1.attributes.implicit <- NULL
json3_1$annotation_label[!is.na(json3_1$Label.1.attributes.0)] <- json3_1$Label.1.attributes.0[!is.na(json3_1$Label.1.attributes.0)]
json3_1$Label.1.attributes.0 <- NULL
colnames(json3_1)[colnames(json3_1) == "Label.1.geometry"] <- "bb_coords"
json3_1$annotation_number <- NA
json3_1 <- json3_1[c("video_name", "annotation_number", "annotation_label", "frame_number", "bb_coords")]

json3_2$Label.2.class <- NULL
json3_2$Label.2.attributes.explicit <- NULL
json3_2$Label.2.attributes.implicit <- NULL
json3_2$annotation_label[!is.na(json3_2$Label.2.attributes.0)] <- json3_2$Label.2.attributes.0[!is.na(json3_2$Label.2.attributes.0)]
json3_2$Label.2.attributes.0 <- NULL
colnames(json3_2)[colnames(json3_2) == "Label.2.geometry"] <- "bb_coords"
json3_2$annotation_number <- NA
json3_2 <- json3_2[c("video_name", "annotation_number", "annotation_label", "frame_number", "bb_coords")]

# Adding an annotation_number to each video based on how many unique annotation objects are present in a video
json3_0$annotation_number <- 1
json3_1$annotation_number <- 1
json3_1$annotation_number[json3_1$video_name %in% json3_0$video_name] <- json3_1$annotation_number[json3_1$video_name %in% json3_0$video_name] + 1
json3_2$annotation_number <- 1
json3_2$annotation_number[json3_2$video_name %in% json3_0$video_name] <- json3_2$annotation_number[json3_2$video_name %in% json3_0$video_name] + 1
json3_2$annotation_number[json3_2$video_name %in% json3_1$video_name] <- json3_2$annotation_number[json3_2$video_name %in% json3_1$video_name] + 1

# Stacking the separate datasets for each unique annotation object
json3 <- rbind(json3_0, json3_1, json3_2)
json3$annotation_number <- as.integer(json3$annotation_number)

# Extracting the bounding box coordinates to separate variables in JSON3
for (i in seq_along(json3$bb_coords)){
  json3$bb_upleft_coordx[i] <- json3$bb_coords[[i]][1]
}

for (i in seq_along(json3$bb_coords)){
  json3$bb_upleft_coordy[i] <- json3$bb_coords[[i]][2]
}

for (i in seq_along(json3$bb_coords)){
  json3$bb_width[i] <- json3$bb_coords[[i]][3]
}

for (i in seq_along(json3$bb_coords)){
  json3$bb_height[i] <- json3$bb_coords[[i]][4]
}

json3$bb_coords <- NULL

# Ordering JSON3 by the video_name, annotation_number, and frame_number
json3 <- json3[order(json3$video_name, json3$annotation_number, json3$frame_number) ,]


# Stacking the three separate JSON files to one common annotations file: 470,882 frames total
annotations <- rbind(json1, json2, json3)

# Ordering ANNOTATIONS by the video_name, annotation_number, and frame_number
annotations <- annotations[order(annotations$video_name, annotations$annotation_number, annotations$frame_number) ,]


# Deleting practice_video_real1 and practice_video_real2 from ANNOTATIONS because the mp4 files of these got corrupted and did
# not slice up properly (video lags when you try to play it)
annotations <- subset(annotations, annotations$video_name != "practice_video_real1")
annotations <- subset(annotations, annotations$video_name != "practice_video_real2")

# Clay Sciences uses a 0-based counting scheme for frames, so I am adding 1 to all of the annotation frame numbers so they match the video frame slice frame numbers
annotations$frame_number <- annotations$frame_number + 1

# Deleting frame 353 from practice_video_fake1 because it is nearly identical to frame 352 and there were only 352 video frame slices
annotations <- annotations[!(annotations$video_name == "practice_video_fake1" & annotations$frame_number == 353) ,]

# Deleting frame 1119 from practice_video_fake2 because it is exactly the same as frame 1118 and there were only 1118 video frame slices
annotations <- annotations[!(annotations$video_name == "practice_video_fake2" & annotations$frame_number == 1119) ,]

# Deleting frame 7615 from fake_video20 because it is exactly the same as frame 7614 and there were only 7614 video frame slices
annotations <- annotations[!(annotations$video_name == "fake_video20" & annotations$frame_number == 7615) ,]

# Deleting frame 776 from fake_video21 because it is exactly the same as frame 775 and there were only 775 video frame slices
annotations <- annotations[!(annotations$video_name == "fake_video21" & annotations$frame_number == 776) ,]

# Deleting frame 3901 from fake_video23 because it is exactly the same as frame 3900 and there were only 3900 video frame slices
annotations <- annotations[!(annotations$video_name == "fake_video23" & annotations$frame_number == 3901) ,]

# Deleting frame 14695 from fake_video24 because it is exactly the same as frame 14694 and there were only 14694 video frame slices
annotations <- annotations[!(annotations$video_name == "fake_video24" & annotations$frame_number == 14695) ,]

# Deleting frame 8222 from fake_video25 because it is exactly the same as frame 8221 and there were only 8221 video frame slices
annotations <- annotations[!(annotations$video_name == "fake_video25" & annotations$frame_number == 8222) ,]

# Deleting frame 10285 from fake_video28 because it is exactly the same as frame 10284 and there were only 10284 video frame slices
annotations <- annotations[!(annotations$video_name == "fake_video28" & annotations$frame_number == 10285) ,]

# Deleting frame 1355 from fake_video30 because it is exactly the same as frame 1354 and there were only 1354 video frame slices
annotations <- annotations[!(annotations$video_name == "fake_video30" & annotations$frame_number == 1355) ,]

# Deleting frame 354 from fake_video31 because it is exactly the same as frame 353 and there were only 353 video frame slices
annotations <- annotations[!(annotations$video_name == "fake_video31" & annotations$frame_number == 354) ,]

# Deleting frame 2358 from fake_video32 because it is exactly the same as frame 2357 and there were only 2357 video frame slices
annotations <- annotations[!(annotations$video_name == "fake_video32" & annotations$frame_number == 2358) ,]

# Deleting frame 1868 from fake_video33 because it is exactly the same as frame 1867 and there were only 1867 video frame slices
annotations <- annotations[!(annotations$video_name == "fake_video33" & annotations$frame_number == 1868) ,]

# Deleting frame 868 from fake_video35 because it is exactly the same as frame 867 and there were only 867 video frame slices
annotations <- annotations[!(annotations$video_name == "fake_video35" & annotations$frame_number == 868) ,]

# Deleting frame 818 from fake_video37 because it is exactly the same as frame 817 and there were only 817 video frame slices
annotations <- annotations[!(annotations$video_name == "fake_video37" & annotations$frame_number == 818) ,]

# Deleting frame 5309 from fake_video40 because it is exactly the same as frame 5308 and there were only 5308 video frame slices
annotations <- annotations[!(annotations$video_name == "fake_video40" & annotations$frame_number == 5309) ,]

# Deleting frame 15742 from fake_video41 because it is exactly the same as frame 15741 and there were only 15741 video frame slices
annotations <- annotations[!(annotations$video_name == "fake_video41" & annotations$frame_number == 15742) ,]

# Deleting frame 12603 from fake_video43 because it is exactly the same as frame 12602 and there were only 12602 video frame slices
annotations <- annotations[!(annotations$video_name == "fake_video43" & annotations$frame_number == 12603) ,]

# Deleting frame 3602 from fake_video44 because it is exactly the same as frame 3601 and there were only 3601 video frame slices
annotations <- annotations[!(annotations$video_name == "fake_video44" & annotations$frame_number == 3602) ,]

# Deleting frames 1206-1207 from fake_video45 because they are nearly identical to frame 1205 and there were only 1205 video frame slices
annotations <- annotations[!(annotations$video_name == "fake_video45" & annotations$frame_number > 1205) ,]

# Deleting frame 1860 from fake_video49 because it is exactly the same as frame 1859 and there were only 1859 video frame slices
annotations <- annotations[!(annotations$video_name == "fake_video49" & annotations$frame_number == 1860) ,]

# Deleting frame 598 from fake_video50 because it is exactly the same as frame 597 and there were only 597 video frame slices
annotations <- annotations[!(annotations$video_name == "fake_video50" & annotations$frame_number == 598) ,]

# Deleting frame 1776 from fake_video51 because it is exactly the same as frame 1775 and there were only 1775 video frame slices
annotations <- annotations[!(annotations$video_name == "fake_video51" & annotations$frame_number == 1776) ,]

# Deleting frame 779 from fake_video52 because it is exactly the same as frame 778 and there were only 778 video frame slices
annotations <- annotations[!(annotations$video_name == "fake_video52" & annotations$frame_number == 779) ,]

# Deleting frame 1340 from fake_video54 because it is exactly the same as frame 1339 and there were only 1339 video frame slices
annotations <- annotations[!(annotations$video_name == "fake_video54" & annotations$frame_number == 1340) ,]

# Deleting frame 3263 from fake_video55 because it is exactly the same as frame 3262 and there were only 3262 video frame slices
annotations <- annotations[!(annotations$video_name == "fake_video55" & annotations$frame_number == 3263) ,]

# Deleting frame 141 from fake_video61 because it is exactly the same as frame 140 and there were only 140 video frame slices
annotations <- annotations[!(annotations$video_name == "fake_video61" & annotations$frame_number == 141) ,]

# Deleting frame 483 from fake_video62 because it is exactly the same as frame 482 and there were only 482 video frame slices
annotations <- annotations[!(annotations$video_name == "fake_video62" & annotations$frame_number == 483) ,]

# Deleting frame 603 from fake_video63 because it is exactly the same as frame 602 and there were only 602 video frame slices
annotations <- annotations[!(annotations$video_name == "fake_video63" & annotations$frame_number == 603) ,]

# Deleting frame 536 from fake_video64 because it is exactly the same as frame 535 and there were only 535 video frame slices
annotations <- annotations[!(annotations$video_name == "fake_video64" & annotations$frame_number == 536) ,]

# Deleting frame 318 from fake_video65 because it is exactly the same as frame 317 and there were only 317 video frame slices
annotations <- annotations[!(annotations$video_name == "fake_video65" & annotations$frame_number == 318) ,]

# Deleting frame 378 from fake_video66 because it is exactly the same as frame 377 and there were only 377 video frame slices
annotations <- annotations[!(annotations$video_name == "fake_video66" & annotations$frame_number == 378) ,]

# For the real_videos, the video metadata duration does not exactly match the length of the video stream. The annotations are based on the
# video metadata, while the frame extractions are based on the video stream. When these durations are discrepant, the annotations and
# extracted frames do not line up. In order to fix this, annotations need to be removed from the beginning of some of the real_videos.
# Some real_videos additionally need annotations to be removed from the end of the video.

# Deleting two annotation frames from the beginning of real_video1
annotations$frame_number[annotations$video_name == "real_video1"] <- annotations$frame_number[annotations$video_name == "real_video1"] - 2
annotations <- subset(annotations, annotations$frame_number > 0)

# Deleting two annotation frames from the beginning of real_video2
annotations$frame_number[annotations$video_name == "real_video2"] <- annotations$frame_number[annotations$video_name == "real_video2"] - 2
annotations <- subset(annotations, annotations$frame_number > 0)

# Reverse lagging the annotation frames of real_video3 by one (same process as deleting one annotation frame from the beginning)
annotations$frame_number[annotations$video_name == "real_video3"] <- annotations$frame_number[annotations$video_name == "real_video3"] - 1

# Reverse lagging the annotation frames of real_video4 by one (same process as deleting one annotation frame from the beginning)
annotations$frame_number[annotations$video_name == "real_video4"] <- annotations$frame_number[annotations$video_name == "real_video4"] - 1

# Reverse lagging the annotation frames of real_video5 by one (same process as deleting one annotation frame from the beginning)
annotations$frame_number[annotations$video_name == "real_video5"] <- annotations$frame_number[annotations$video_name == "real_video5"] - 1

# Deleting frames 14887 and 14888 from real_video6
annotations <- annotations[!(annotations$video_name == "real_video6" & annotations$frame_number == 14887) ,]
annotations <- annotations[!(annotations$video_name == "real_video6" & annotations$frame_number == 14888) ,]

# Deleting two annotation frames from the beginning of real_video7 and deleting three annotation frames from the end of real_video7
annotations$frame_number[annotations$video_name == "real_video7"] <- annotations$frame_number[annotations$video_name == "real_video7"] - 2
annotations <- subset(annotations, annotations$frame_number > 0)
annotations <- annotations[!(annotations$video_name == "real_video7" & annotations$frame_number > 10826) ,]

# Reverse lagging the annotation frames of real_video8 by two (same process as deleting two annotation frames from the beginning)
annotations$frame_number[annotations$video_name == "real_video8"] <- annotations$frame_number[annotations$video_name == "real_video8"] - 2

# Reverse lagging the annotation frames of real_video9 by two (same process as deleting two annotation frames from the beginning)
annotations$frame_number[annotations$video_name == "real_video9"] <- annotations$frame_number[annotations$video_name == "real_video9"] - 2

# Deleting two annotation frames from the beginning of real_video10 and deleting three annotation frames from the end of real_video10
annotations$frame_number[annotations$video_name == "real_video10"] <- annotations$frame_number[annotations$video_name == "real_video10"] - 2
annotations <- subset(annotations, annotations$frame_number > 0)
annotations <- annotations[!(annotations$video_name == "real_video10" & annotations$frame_number > 9601) ,]

# Deleting two annotation frames from the beginning of real_video11
annotations$frame_number[annotations$video_name == "real_video11"] <- annotations$frame_number[annotations$video_name == "real_video11"] - 2
annotations <- subset(annotations, annotations$frame_number > 0)

# Reverse lagging the annotation frames of real_video12 by one (same process as deleting one annotation frame from the beginning)
annotations$frame_number[annotations$video_name == "real_video12"] <- annotations$frame_number[annotations$video_name == "real_video12"] - 1

# Deleting one annotation frame from the beginning of real_video13 and deleting three annotation frames from the end of real_video13
annotations$frame_number[annotations$video_name == "real_video13"] <- annotations$frame_number[annotations$video_name == "real_video13"] - 1
annotations <- subset(annotations, annotations$frame_number > 0)
annotations <- annotations[!(annotations$video_name == "real_video13" & annotations$frame_number > 238) ,]

# Deleting one annotation frame from the beginning of real_video14
annotations$frame_number[annotations$video_name == "real_video14"] <- annotations$frame_number[annotations$video_name == "real_video14"] - 1
annotations <- subset(annotations, annotations$frame_number > 0)

# Deleting seven annotation frames from the end of real_video15
annotations <- annotations[!(annotations$video_name == "real_video15" & annotations$frame_number > 2753) ,]

# Deleting two annotation frames from the beginning of real_video16 and deleting one annotation frame from the end of real_video16
annotations$frame_number[annotations$video_name == "real_video16"] <- annotations$frame_number[annotations$video_name == "real_video16"] - 2
annotations <- subset(annotations, annotations$frame_number > 0)
annotations <- annotations[!(annotations$video_name == "real_video16" & annotations$frame_number > 896) ,]

# Deleting two annotation frames from the beginning of real_video17 and deleting one annotation frame from the end of real_video17
annotations$frame_number[annotations$video_name == "real_video17"] <- annotations$frame_number[annotations$video_name == "real_video17"] - 2
annotations <- subset(annotations, annotations$frame_number > 0)
annotations <- annotations[!(annotations$video_name == "real_video17" & annotations$frame_number > 2984) ,]

# Deleting two annotation frames from the beginning of real_video18
annotations$frame_number[annotations$video_name == "real_video18"] <- annotations$frame_number[annotations$video_name == "real_video18"] - 2
annotations <- subset(annotations, annotations$frame_number > 0)

# Deleting two annotation frames from the beginning of real_video19 and deleting two annotation frames from the end of real_video19
annotations$frame_number[annotations$video_name == "real_video19"] <- annotations$frame_number[annotations$video_name == "real_video19"] - 2
annotations <- subset(annotations, annotations$frame_number > 0)
annotations <- annotations[!(annotations$video_name == "real_video19" & annotations$frame_number > 5551) ,]

# Deleting one annotation frame from the beginning of real_video20 and deleting one annotation frame from the end of real_video20
annotations$frame_number[annotations$video_name == "real_video20"] <- annotations$frame_number[annotations$video_name == "real_video20"] - 1
annotations <- subset(annotations, annotations$frame_number > 0)
annotations <- annotations[!(annotations$video_name == "real_video20" & annotations$frame_number > 13993) ,]

# Deleting six annotation frames from the end of real_video21
annotations <- annotations[!(annotations$video_name == "real_video21" & annotations$frame_number > 1760) ,]

# The annotation frames of real_video22 should be kept as is

# Deleting two annotation frames from the beginning of real_video23 and deleting one annotation frame from the end of real_video23
annotations$frame_number[annotations$video_name == "real_video23"] <- annotations$frame_number[annotations$video_name == "real_video23"] - 2
annotations <- subset(annotations, annotations$frame_number > 0)
annotations <- annotations[!(annotations$video_name == "real_video23" & annotations$frame_number > 6174) ,]

# Deleting two annotation frames from the beginning of real_video24 and deleting one annotation frame from the end of real_video24
annotations$frame_number[annotations$video_name == "real_video24"] <- annotations$frame_number[annotations$video_name == "real_video24"] - 2
annotations <- subset(annotations, annotations$frame_number > 0)
annotations <- annotations[!(annotations$video_name == "real_video24" & annotations$frame_number > 16376) ,]

# Reverse lagging the annotation frames of real_video25 by two and deleting one annotation frame from the end of real_video25 
annotations$frame_number[annotations$video_name == "real_video25"] <- annotations$frame_number[annotations$video_name == "real_video25"] - 2
annotations <- annotations[!(annotations$video_name == "real_video25" & annotations$frame_number > 4997) ,]

# Deleting one annotation frame from the beginning of real_video26
annotations$frame_number[annotations$video_name == "real_video26"] <- annotations$frame_number[annotations$video_name == "real_video26"] - 1
annotations <- subset(annotations, annotations$frame_number > 0)

# Deleting two annotation frames from the beginning of real_video27
annotations$frame_number[annotations$video_name == "real_video27"] <- annotations$frame_number[annotations$video_name == "real_video27"] - 2
annotations <- subset(annotations, annotations$frame_number > 0)

# Deleting three annotation frames from the beginning of real_video28
annotations$frame_number[annotations$video_name == "real_video28"] <- annotations$frame_number[annotations$video_name == "real_video28"] - 3
annotations <- subset(annotations, annotations$frame_number > 0)

# Deleting four annotation frames from the end of real_video29
annotations <- annotations[!(annotations$video_name == "real_video29" & annotations$frame_number > 2698) ,]

# Reverse lagging the annotation frames of real_video30 by two (same process as deleting two annotation frames from the beginning)
annotations$frame_number[annotations$video_name == "real_video30"] <- annotations$frame_number[annotations$video_name == "real_video30"] - 2

# Deleting two annotation frames from the beginning of real_video31
annotations$frame_number[annotations$video_name == "real_video31"] <- annotations$frame_number[annotations$video_name == "real_video31"] - 2
annotations <- subset(annotations, annotations$frame_number > 0)

# The annotation frames of real_video32 should be kept as is

# Deleting two annotation frames from the beginning of real_video33
annotations$frame_number[annotations$video_name == "real_video33"] <- annotations$frame_number[annotations$video_name == "real_video33"] - 2
annotations <- subset(annotations, annotations$frame_number > 0)

# After removing all extra annotation frames, I am left with 469,141 frames total


# Fixing the videos that were exported with an incorrect rightward shift in the bounding box X coordinates
# fake_video13
annotations$bb_upleft_coordx[annotations$video_name == "fake_video13"] <- annotations$bb_upleft_coordx[annotations$video_name == "fake_video13"] - 15
annotations$bb_width[annotations$video_name == "fake_video13"] <- annotations$bb_width[annotations$video_name == "fake_video13"] / 1.1

# fake_video20
annotations$bb_upleft_coordx[annotations$video_name == "fake_video20" & annotations$annotation_number == 1] <- annotations$bb_upleft_coordx[annotations$video_name == "fake_video20" & annotations$annotation_number == 1] - 20
annotations$bb_upleft_coordx[annotations$video_name == "fake_video20" & annotations$annotation_number == 2] <- annotations$bb_upleft_coordx[annotations$video_name == "fake_video20" & annotations$annotation_number == 2] - 25

# fake_video51
annotations$bb_upleft_coordx[annotations$video_name == "fake_video51"] <- annotations$bb_upleft_coordx[annotations$video_name == "fake_video51"] - 40


# The resize code below is no longer necessary, but I'll keep it in for completeness #
# Resizing the extracted frame images to 320x240 pixels, and resizing the annotation bounding box coordinates to match the frame image resize
path_start <- "Videos\\"
width_resize <- 320
height_resize <- 240

image_resize <- function(image){
  frame_image <- load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image))
  frame_image_resize <- resize(frame_image, size_x = width_resize, size_y = height_resize)
  save.image(frame_image_resize, file = paste0(path_start, main_folder, "\\", sub_folder, "_resize\\", sapply(strsplit(image, ".jpg"), `[`, 1), "_resize.jpg"))
}

main_folder <- "practice_videos"
sub_folder <- "practice_video_fake1"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "practice_videos"
sub_folder <- "practice_video_fake2"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video1"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video2"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video3"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video4"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video5"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video6"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video7"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video8"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video9"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video10"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video11"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video12"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video13"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video14"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video15"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video16"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video17"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video18"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video19"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video20"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video21"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video22"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video23"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video24"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video25"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video26"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video27"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video28"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video29"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video30"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video31"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video32"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video33"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video34"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video35"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video36"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video37"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video38"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video39"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video40"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video41"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video42"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video43"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video44"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video45"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video46"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video47"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video48"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video49"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video50"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video51"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video52"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video53"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video54"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video55"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video56"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video57"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video58"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video59"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video60"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video61"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video62"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video63"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video64"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video65"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "fake_videos"
sub_folder <- "fake_video66"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "real_videos"
sub_folder <- "real_video1"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "real_videos"
sub_folder <- "real_video2"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "real_videos"
sub_folder <- "real_video3"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "real_videos"
sub_folder <- "real_video4"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "real_videos"
sub_folder <- "real_video5"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "real_videos"
sub_folder <- "real_video6"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "real_videos"
sub_folder <- "real_video7"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "real_videos"
sub_folder <- "real_video8"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "real_videos"
sub_folder <- "real_video9"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "real_videos"
sub_folder <- "real_video10"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "real_videos"
sub_folder <- "real_video11"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "real_videos"
sub_folder <- "real_video12"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "real_videos"
sub_folder <- "real_video13"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "real_videos"
sub_folder <- "real_video14"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "real_videos"
sub_folder <- "real_video15"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "real_videos"
sub_folder <- "real_video16"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "real_videos"
sub_folder <- "real_video17"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "real_videos"
sub_folder <- "real_video18"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "real_videos"
sub_folder <- "real_video19"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "real_videos"
sub_folder <- "real_video20"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "real_videos"
sub_folder <- "real_video21"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "real_videos"
sub_folder <- "real_video22"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "real_videos"
sub_folder <- "real_video23"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "real_videos"
sub_folder <- "real_video24"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "real_videos"
sub_folder <- "real_video25"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "real_videos"
sub_folder <- "real_video26"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "real_videos"
sub_folder <- "real_video27"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "real_videos"
sub_folder <- "real_video28"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "real_videos"
sub_folder <- "real_video29"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "real_videos"
sub_folder <- "real_video30"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "real_videos"
sub_folder <- "real_video31"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "real_videos"
sub_folder <- "real_video32"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))

main_folder <- "real_videos"
sub_folder <- "real_video33"
image_list <- as.list(list.files(paste0(path_start, main_folder, "\\", sub_folder)))
lapply(image_list, image_resize)
annotations$bb_upleft_coordx_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordx[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_upleft_coordy_resize[annotations$video_name == sub_folder] <- annotations$bb_upleft_coordy[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_width_resize[annotations$video_name == sub_folder] <- annotations$bb_width[annotations$video_name == sub_folder] * (width_resize / width(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))
annotations$bb_height_resize[annotations$video_name == sub_folder] <- annotations$bb_height[annotations$video_name == sub_folder] * (height_resize / height(load.image(paste0(path_start, main_folder, "\\", sub_folder, "\\", image_list[1]))))


# Creating a numeric variable for annotation label, where 1 = Real and 2 = Fake
annotations$annotation_label_num[annotations$annotation_label == "Real"] <- 1
annotations$annotation_label_num[annotations$annotation_label == "Fake"] <- 2

# Writing the ANNOTATIONS dataset out. To reload the dataset, use: annotations <- readRDS(file = "annotations.Rds")
saveRDS(annotations, file = "annotations.Rds")




# Checking the alignment of a randomly selected resized video frame per video and the corresponding resized bounding box coordinates
# This just helped to ensure that the resizing was done correctly
#random_frame <- sample_n(subset(annotations, annotations$video_name == "practice_video_fake1"), 1)
#image_list <- list.files(paste0(path_start, "practice_videos\\practice_video_fake1_resize"))
#frame_image <- load.image(paste0(path_start, "practice_videos\\practice_video_fake1_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "practice_video_fake2"), 1)
#image_list <- list.files(paste0(path_start, "practice_videos\\practice_video_fake2_resize"))
#frame_image <- load.image(paste0(path_start, "practice_videos\\practice_video_fake2_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video1"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video1_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video1_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video2"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video2_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video2_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video3"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video3_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video3_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video4"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video4_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video4_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video5"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video5_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video5_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video6"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video6_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video6_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video7"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video7_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video7_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video8"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video8_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video8_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video9"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video9_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video9_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video10"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video10_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video10_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video11"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video11_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video11_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video12"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video12_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video12_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video13"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video13_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video13_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video14"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video14_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video14_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video15"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video15_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video15_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video16"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video16_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video16_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video17"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video17_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video17_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video18"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video18_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video18_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video19"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video19_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video19_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video20"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video20_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video20_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video21"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video21_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video21_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video22"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video22_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video22_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video23"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video23_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video23_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video24"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video24_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video24_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video25"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video25_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video25_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video26"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video26_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video26_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video27"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video27_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video27_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video28"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video28_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video28_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video29"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video29_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video29_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video30"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video30_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video30_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video31"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video31_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video31_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video32"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video32_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video32_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video33"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video33_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video33_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video34"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video34_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video34_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video35"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video35_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video35_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video36"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video36_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video36_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video37"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video37_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video37_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video38"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video38_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video38_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video39"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video39_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video39_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video40"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video40_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video40_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video41"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video41_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video41_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video42"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video42_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video42_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video43"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video43_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video43_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video44"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video44_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video44_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video45"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video45_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video45_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video46"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video46_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video46_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video47"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video47_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video47_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video48"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video48_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video48_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video49"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video49_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video49_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video50"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video50_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video50_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video51"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video51_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video51_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video52"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video52_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video52_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video53"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video53_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video53_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video54"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video54_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video54_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video55"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video55_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video55_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video56"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video56_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video56_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video57"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video57_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video57_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video58"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video58_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video58_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video59"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video59_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video59_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video60"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video60_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video60_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video61"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video61_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video61_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video62"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video62_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video62_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video63"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video63_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video63_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video64"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video64_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video64_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video65"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video65_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video65_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "fake_video66"), 1)
#image_list <- list.files(paste0(path_start, "fake_videos\\fake_video66_resize"))
#frame_image <- load.image(paste0(path_start, "fake_videos\\fake_video66_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "real_video1"), 1)
#image_list <- list.files(paste0(path_start, "real_videos\\real_video1_resize"))
#frame_image <- load.image(paste0(path_start, "real_videos\\real_video1_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "real_video2"), 1)
#image_list <- list.files(paste0(path_start, "real_videos\\real_video2_resize"))
#frame_image <- load.image(paste0(path_start, "real_videos\\real_video2_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "real_video3"), 1)
#image_list <- list.files(paste0(path_start, "real_videos\\real_video3_resize"))
#frame_image <- load.image(paste0(path_start, "real_videos\\real_video3_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "real_video4"), 1)
#image_list <- list.files(paste0(path_start, "real_videos\\real_video4_resize"))
#frame_image <- load.image(paste0(path_start, "real_videos\\real_video4_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "real_video5"), 1)
#image_list <- list.files(paste0(path_start, "real_videos\\real_video5_resize"))
#frame_image <- load.image(paste0(path_start, "real_videos\\real_video5_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "real_video6"), 1)
#image_list <- list.files(paste0(path_start, "real_videos\\real_video6_resize"))
#frame_image <- load.image(paste0(path_start, "real_videos\\real_video6_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "real_video7"), 1)
#image_list <- list.files(paste0(path_start, "real_videos\\real_video7_resize"))
#frame_image <- load.image(paste0(path_start, "real_videos\\real_video7_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "real_video8"), 1)
#image_list <- list.files(paste0(path_start, "real_videos\\real_video8_resize"))
#frame_image <- load.image(paste0(path_start, "real_videos\\real_video8_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "real_video9"), 1)
#image_list <- list.files(paste0(path_start, "real_videos\\real_video9_resize"))
#frame_image <- load.image(paste0(path_start, "real_videos\\real_video9_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "real_video10"), 1)
#image_list <- list.files(paste0(path_start, "real_videos\\real_video10_resize"))
#frame_image <- load.image(paste0(path_start, "real_videos\\real_video10_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "real_video11"), 1)
#image_list <- list.files(paste0(path_start, "real_videos\\real_video11_resize"))
#frame_image <- load.image(paste0(path_start, "real_videos\\real_video11_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "real_video12"), 1)
#image_list <- list.files(paste0(path_start, "real_videos\\real_video12_resize"))
#frame_image <- load.image(paste0(path_start, "real_videos\\real_video12_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "real_video13"), 1)
#image_list <- list.files(paste0(path_start, "real_videos\\real_video13_resize"))
#frame_image <- load.image(paste0(path_start, "real_videos\\real_video13_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "real_video14"), 1)
#image_list <- list.files(paste0(path_start, "real_videos\\real_video14_resize"))
#frame_image <- load.image(paste0(path_start, "real_videos\\real_video14_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "real_video15"), 1)
#image_list <- list.files(paste0(path_start, "real_videos\\real_video15_resize"))
#frame_image <- load.image(paste0(path_start, "real_videos\\real_video15_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "real_video16"), 1)
#image_list <- list.files(paste0(path_start, "real_videos\\real_video16_resize"))
#frame_image <- load.image(paste0(path_start, "real_videos\\real_video16_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "real_video17"), 1)
#image_list <- list.files(paste0(path_start, "real_videos\\real_video17_resize"))
#frame_image <- load.image(paste0(path_start, "real_videos\\real_video17_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "real_video18"), 1)
#image_list <- list.files(paste0(path_start, "real_videos\\real_video18_resize"))
#frame_image <- load.image(paste0(path_start, "real_videos\\real_video18_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "real_video19"), 1)
#image_list <- list.files(paste0(path_start, "real_videos\\real_video19_resize"))
#frame_image <- load.image(paste0(path_start, "real_videos\\real_video19_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "real_video20"), 1)
#image_list <- list.files(paste0(path_start, "real_videos\\real_video20_resize"))
#frame_image <- load.image(paste0(path_start, "real_videos\\real_video20_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "real_video21"), 1)
#image_list <- list.files(paste0(path_start, "real_videos\\real_video21_resize"))
#frame_image <- load.image(paste0(path_start, "real_videos\\real_video21_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "real_video22"), 1)
#image_list <- list.files(paste0(path_start, "real_videos\\real_video22_resize"))
#frame_image <- load.image(paste0(path_start, "real_videos\\real_video22_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "real_video23"), 1)
#image_list <- list.files(paste0(path_start, "real_videos\\real_video23_resize"))
#frame_image <- load.image(paste0(path_start, "real_videos\\real_video23_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "real_video24"), 1)
#image_list <- list.files(paste0(path_start, "real_videos\\real_video24_resize"))
#frame_image <- load.image(paste0(path_start, "real_videos\\real_video24_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "real_video25"), 1)
#image_list <- list.files(paste0(path_start, "real_videos\\real_video25_resize"))
#frame_image <- load.image(paste0(path_start, "real_videos\\real_video25_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "real_video26"), 1)
#image_list <- list.files(paste0(path_start, "real_videos\\real_video26_resize"))
#frame_image <- load.image(paste0(path_start, "real_videos\\real_video26_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "real_video27"), 1)
#image_list <- list.files(paste0(path_start, "real_videos\\real_video27_resize"))
#frame_image <- load.image(paste0(path_start, "real_videos\\real_video27_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "real_video28"), 1)
#image_list <- list.files(paste0(path_start, "real_videos\\real_video28_resize"))
#frame_image <- load.image(paste0(path_start, "real_videos\\real_video28_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "real_video29"), 1)
#image_list <- list.files(paste0(path_start, "real_videos\\real_video29_resize"))
#frame_image <- load.image(paste0(path_start, "real_videos\\real_video29_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "real_video30"), 1)
#image_list <- list.files(paste0(path_start, "real_videos\\real_video30_resize"))
#frame_image <- load.image(paste0(path_start, "real_videos\\real_video30_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "real_video31"), 1)
#image_list <- list.files(paste0(path_start, "real_videos\\real_video31_resize"))
#frame_image <- load.image(paste0(path_start, "real_videos\\real_video31_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "real_video32"), 1)
#image_list <- list.files(paste0(path_start, "real_videos\\real_video32_resize"))
#frame_image <- load.image(paste0(path_start, "real_videos\\real_video32_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

#random_frame <- sample_n(subset(annotations, annotations$video_name == "real_video33"), 1)
#image_list <- list.files(paste0(path_start, "real_videos\\real_video33_resize"))
#frame_image <- load.image(paste0(path_start, "real_videos\\real_video33_resize\\", image_list[random_frame$frame_number]))
#draw_rect(frame_image, x0 = (random_frame$bb_upleft_coordx_resize), y0 = (random_frame$bb_upleft_coordy_resize + random_frame$bb_height_resize),
#          x1 = (random_frame$bb_upleft_coordx_resize + random_frame$bb_width_resize), y1 = (random_frame$bb_upleft_coordy_resize), 
#          color = "red", opacity = 0.25) %>% plot
#random_frame$annotation_label

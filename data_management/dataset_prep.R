# Program to prepare the datasets to be used in the SSD300 model

# Loading in the necessary packages
library(stringr)
library(dplyr)

# Reading in the cleaned ANNOTATIONS file
annotations <- readRDS(file = "annotations.Rds")

# Dropping some unneeded variables in the ANNOTATIONS file
annotations$bb_upleft_coordx_resize <- NULL
annotations$bb_upleft_coordy_resize <- NULL
annotations$bb_width_resize <- NULL
annotations$bb_height_resize <- NULL
annotations$annotation_number <- NULL
annotations$annotation_label <- NULL

# Renaming some of the variables in the ANNOTATIONS file
colnames(annotations)[colnames(annotations) == "video_name"] <- "frame"
colnames(annotations)[colnames(annotations) == "annotation_label_num"] <- "class_id"

# Padding the frame number variable with leading zeros so each frame number has five digits
# This will make it so the frame labels here match the JPG image names
annotations$frame_number <- as.character(annotations$frame_number)
annotations$frame_number <- str_pad(annotations$frame_number, width = 5, side = "left", pad = "0")

# Pasting the frame variable, the frame number variable, and a ".jpg" suffix
annotations$frame <- paste0(annotations$frame, "_", annotations$frame_number, ".jpg")
# Now the frame variable should match the JPG image names

# Dropping the frame number variable
annotations$frame_number <- NULL

# Sorting ANNOTATIONS by the frame variable
annotations <- annotations[order(annotations$frame) ,]


# Removing images from the all_videos folder that do not have corresponding annotations
`%notin%` <- Negate(`%in%`)

image_names <- list.files(path = "all_videos", pattern = ".jpg$")
to_delete <- image_names[image_names %notin% annotations$frame]
sapply(lapply(to_delete, function(x) paste("all_videos", x, sep = "/")), unlink)


# Transforming the coordinates in ANNOTATIONS to be xmin, xmax, ymin, ymax
# ('xmin', 'ymin') represents the absolute top left corner of the bounding box, and ('xmax', 'ymax') represents the 
# absolute bottom right corner of the bounding box (because the y-axis is traditionally listed in decreasing numerical
# order for images). Phrased another way, 'xmin' and 'xmax' are the left-most and right-most absolute horizontal 
# coordinates of the bounding box, and 'ymin' and 'ymax' are the top-most and bottom-most absolute vertical 
# coordinates of the bounding box.
# The variable bb_upleft_coordx is the same as xmin
annotations$xmin <- annotations$bb_upleft_coordx

# The variable bb_upleft_coordy is the same as ymin
annotations$ymin <- annotations$bb_upleft_coordy

# The variable bb_upleft_coordx plus the variable bb_width is xmax
annotations$xmax <- annotations$bb_upleft_coordx + annotations$bb_width

# The variable bb_upleft_coordy plus the variable bb_height is ymax
annotations$ymax <- annotations$bb_upleft_coordy + annotations$bb_height

# Dropping the old coordinate variables in ANNOTATIONS
annotations$bb_upleft_coordx <- NULL
annotations$bb_upleft_coordy <- NULL
annotations$bb_width <- NULL
annotations$bb_height <- NULL

# Reordering the variables in ANNOTATIONS
annotations <- annotations[c("frame", "xmin", "xmax", "ymin", "ymax", "class_id")]

# Rounding the pixel coordinate points in ANNOTATIONS to the nearest whole number
annotations$xmin <- round(annotations$xmin)
annotations$xmax <- round(annotations$xmax)
annotations$ymin <- round(annotations$ymin)
annotations$ymax <- round(annotations$ymax)

# Setting a handful of negative ymin pixel coordinates to be equal to zero (maximum negative ymin coordinate is -2)
annotations$ymin[annotations$ymin < 0] <- 0


# Splitting ANNOTATIONS out into train, validation, and test datasets, and writing the datasets out as CSV files.
# Will use a 90/10 split between train and test. Then will further split train out into train and validation,
# using an 80/20 split. During the splitting, I will preserve the video groups (i.e. no video will appear in both
# the train and test datasets). The videos can be thought of as longitudinal data, where each video is an independent
# unit. Preserving the video groups ensures that model evaluation (test) is not biased to overperform, as a simple
# random sample would result in the model evaluating data it already has some information about.
# First adding a temporary variable for just the video name
annotations$video_name <- gsub("_[0-9]{5}.jpg$", "", annotations$frame)

# Creating a dataset of the unique video names, and creating an indicator to distinguish between real and fake videos
videos <- as.data.frame(unique(annotations$video_name))
colnames(videos) <- "video_name"
videos <- subset(videos, videos$video_name != "practice_video_fake1" & videos$video_name != "practice_video_fake2")
videos$r1_f2[str_detect(videos$video_name, "^real") == TRUE] <- 1
videos$r1_f2[str_detect(videos$video_name, "^fake") == TRUE] <- 2

# Splitting out the trainval dataset using practice_video_fake1 and 90% of the fake_videos and 90% of the real_videos
set.seed(15)
trainval_videos <- videos %>% group_by(r1_f2) %>% sample_frac(0.9, replace = FALSE)
trainval_videos$video_name <- as.character(trainval_videos$video_name)
trainval_videos$r1_f2 <- NULL
trainval_videos <- data.frame(trainval_videos)
practice_video_fake1_df <- data.frame("practice_video_fake1")
colnames(practice_video_fake1_df) <- "video_name"
trainval_videos <- rbind(trainval_videos, practice_video_fake1_df)
labels_trainval <- subset(annotations, annotations$video_name %in% trainval_videos$video_name)

# Splitting out the test dataset using videos that do not appear in the trainval dataset
labels_test <- subset(annotations, annotations$video_name %notin% trainval_videos$video_name)

# Splitting out the train dataset using practice_video_fake1 and 80% of the fake_videos and 80% of the real_videos
# First creating an indicator to distinguish between real and fake videos in trainval_videos
trainval_videos <- subset(trainval_videos, trainval_videos$video_name != "practice_video_fake1")
trainval_videos$r1_f2[str_detect(trainval_videos$video_name, "^real") == TRUE] <- 1
trainval_videos$r1_f2[str_detect(trainval_videos$video_name, "^fake") == TRUE] <- 2
# Now making the split
set.seed(5)
train_videos <- trainval_videos %>% group_by(r1_f2) %>% sample_frac(0.8, replace = FALSE)
train_videos$video_name <- as.character(train_videos$video_name)
train_videos$r1_f2 <- NULL
train_videos <- data.frame(train_videos)
practice_video_fake1_df <- data.frame("practice_video_fake1")
colnames(practice_video_fake1_df) <- "video_name"
train_videos <- rbind(train_videos, practice_video_fake1_df)
labels_train <- subset(annotations, annotations$video_name %in% train_videos$video_name)

# Splitting out the validation dataset using trainval videos that do not appear in the train dataset
labels_validation <- subset(labels_trainval, labels_trainval$video_name %notin% train_videos$video_name)

# Removing the video_name variable across the train, validation, and test datasets
labels_trainval$video_name <- NULL
labels_train$video_name <- NULL
labels_validation$video_name <- NULL
labels_test$video_name <- NULL

# Writing the train, validation, and test datasets out as CSV files
write.csv(labels_trainval, file = "labels_trainval.csv", row.names = FALSE)
write.csv(labels_train, file = "labels_train.csv", row.names = FALSE)
write.csv(labels_validation, file = "labels_validation.csv", row.names = FALSE)
write.csv(labels_test, file = "labels_test.csv", row.names = FALSE)

# Writing out a practice_train and practice_validation dataset to ensure that code works correctly before
# feeding the full dataset into the SSD300 model
labels_practice_train <- subset(annotations, annotations$video_name == "practice_video_fake2")
labels_practice_validation <- subset(annotations, annotations$video_name == "practice_video_fake1")
labels_practice_train$video_name <- NULL
labels_practice_validation$video_name <- NULL
write.csv(labels_practice_train, file = "labels_practice_train.csv", row.names = FALSE)
write.csv(labels_practice_validation, file = "labels_practice_validation.csv", row.names = FALSE)


# Reading in all of the image files to find the per-channel (RGB) mean of the images in the dataset
# I am processing each source video separately, rather than applying through a list, because although the separate processing looks a 
# lot messier, it was way faster than trying to compute and store everything in memory
#library(imager)
#practice_video_fake1_image_names <- list.files(path = "all_videos", pattern = "practice_video_fake1_[0-9]{5}.jpg")
#practice_video_fake2_image_names <- list.files(path = "all_videos", pattern = "practice_video_fake2_[0-9]{5}.jpg")
#fake_video1_image_names <- list.files(path = "all_videos", pattern = "fake_video1_[0-9]{5}.jpg")
#fake_video2_image_names <- list.files(path = "all_videos", pattern = "fake_video2_[0-9]{5}.jpg")
#fake_video3_image_names <- list.files(path = "all_videos", pattern = "fake_video3_[0-9]{5}.jpg")
#fake_video4_image_names <- list.files(path = "all_videos", pattern = "fake_video4_[0-9]{5}.jpg")
#fake_video5_image_names <- list.files(path = "all_videos", pattern = "fake_video5_[0-9]{5}.jpg")
#fake_video6_image_names <- list.files(path = "all_videos", pattern = "fake_video6_[0-9]{5}.jpg")
#fake_video7_image_names <- list.files(path = "all_videos", pattern = "fake_video7_[0-9]{5}.jpg")
#fake_video8_image_names <- list.files(path = "all_videos", pattern = "fake_video8_[0-9]{5}.jpg")
#fake_video9_image_names <- list.files(path = "all_videos", pattern = "fake_video9_[0-9]{5}.jpg")
#fake_video10_image_names <- list.files(path = "all_videos", pattern = "fake_video10_[0-9]{5}.jpg")
#fake_video11_image_names <- list.files(path = "all_videos", pattern = "fake_video11_[0-9]{5}.jpg")
#fake_video12_image_names <- list.files(path = "all_videos", pattern = "fake_video12_[0-9]{5}.jpg")
#fake_video13_image_names <- list.files(path = "all_videos", pattern = "fake_video13_[0-9]{5}.jpg")
#fake_video14_image_names <- list.files(path = "all_videos", pattern = "fake_video14_[0-9]{5}.jpg")
#fake_video15_image_names <- list.files(path = "all_videos", pattern = "fake_video15_[0-9]{5}.jpg")
#fake_video16_image_names <- list.files(path = "all_videos", pattern = "fake_video16_[0-9]{5}.jpg")
#fake_video17_image_names <- list.files(path = "all_videos", pattern = "fake_video17_[0-9]{5}.jpg")
#fake_video18_image_names <- list.files(path = "all_videos", pattern = "fake_video18_[0-9]{5}.jpg")
#fake_video19_image_names <- list.files(path = "all_videos", pattern = "fake_video19_[0-9]{5}.jpg")
#fake_video20_image_names <- list.files(path = "all_videos", pattern = "fake_video20_[0-9]{5}.jpg")
#fake_video21_image_names <- list.files(path = "all_videos", pattern = "fake_video21_[0-9]{5}.jpg")
#fake_video22_image_names <- list.files(path = "all_videos", pattern = "fake_video22_[0-9]{5}.jpg")
#fake_video23_image_names <- list.files(path = "all_videos", pattern = "fake_video23_[0-9]{5}.jpg")
#fake_video24_image_names <- list.files(path = "all_videos", pattern = "fake_video24_[0-9]{5}.jpg")
#fake_video25_image_names <- list.files(path = "all_videos", pattern = "fake_video25_[0-9]{5}.jpg")
#fake_video26_image_names <- list.files(path = "all_videos", pattern = "fake_video26_[0-9]{5}.jpg")
#fake_video27_image_names <- list.files(path = "all_videos", pattern = "fake_video27_[0-9]{5}.jpg")
#fake_video28_image_names <- list.files(path = "all_videos", pattern = "fake_video28_[0-9]{5}.jpg")
#fake_video29_image_names <- list.files(path = "all_videos", pattern = "fake_video29_[0-9]{5}.jpg")
#fake_video30_image_names <- list.files(path = "all_videos", pattern = "fake_video30_[0-9]{5}.jpg")
#fake_video31_image_names <- list.files(path = "all_videos", pattern = "fake_video31_[0-9]{5}.jpg")
#fake_video32_image_names <- list.files(path = "all_videos", pattern = "fake_video32_[0-9]{5}.jpg")
#fake_video33_image_names <- list.files(path = "all_videos", pattern = "fake_video33_[0-9]{5}.jpg")
#fake_video34_image_names <- list.files(path = "all_videos", pattern = "fake_video34_[0-9]{5}.jpg")
#fake_video35_image_names <- list.files(path = "all_videos", pattern = "fake_video35_[0-9]{5}.jpg")
#fake_video36_image_names <- list.files(path = "all_videos", pattern = "fake_video36_[0-9]{5}.jpg")
#fake_video37_image_names <- list.files(path = "all_videos", pattern = "fake_video37_[0-9]{5}.jpg")
#fake_video38_image_names <- list.files(path = "all_videos", pattern = "fake_video38_[0-9]{5}.jpg")
#fake_video39_image_names <- list.files(path = "all_videos", pattern = "fake_video39_[0-9]{5}.jpg")
#fake_video40_image_names <- list.files(path = "all_videos", pattern = "fake_video40_[0-9]{5}.jpg")
#fake_video41_image_names <- list.files(path = "all_videos", pattern = "fake_video41_[0-9]{5}.jpg")
#fake_video42_image_names <- list.files(path = "all_videos", pattern = "fake_video42_[0-9]{5}.jpg")
#fake_video43_image_names <- list.files(path = "all_videos", pattern = "fake_video43_[0-9]{5}.jpg")
#fake_video44_image_names <- list.files(path = "all_videos", pattern = "fake_video44_[0-9]{5}.jpg")
#fake_video45_image_names <- list.files(path = "all_videos", pattern = "fake_video45_[0-9]{5}.jpg")
#fake_video46_image_names <- list.files(path = "all_videos", pattern = "fake_video46_[0-9]{5}.jpg")
#fake_video47_image_names <- list.files(path = "all_videos", pattern = "fake_video47_[0-9]{5}.jpg")
#fake_video48_image_names <- list.files(path = "all_videos", pattern = "fake_video48_[0-9]{5}.jpg")
#fake_video49_image_names <- list.files(path = "all_videos", pattern = "fake_video49_[0-9]{5}.jpg")
#fake_video50_image_names <- list.files(path = "all_videos", pattern = "fake_video50_[0-9]{5}.jpg")
#fake_video51_image_names <- list.files(path = "all_videos", pattern = "fake_video51_[0-9]{5}.jpg")
#fake_video52_image_names <- list.files(path = "all_videos", pattern = "fake_video52_[0-9]{5}.jpg")
#fake_video53_image_names <- list.files(path = "all_videos", pattern = "fake_video53_[0-9]{5}.jpg")
#fake_video54_image_names <- list.files(path = "all_videos", pattern = "fake_video54_[0-9]{5}.jpg")
#fake_video55_image_names <- list.files(path = "all_videos", pattern = "fake_video55_[0-9]{5}.jpg")
#fake_video56_image_names <- list.files(path = "all_videos", pattern = "fake_video56_[0-9]{5}.jpg")
#fake_video57_image_names <- list.files(path = "all_videos", pattern = "fake_video57_[0-9]{5}.jpg")
#fake_video58_image_names <- list.files(path = "all_videos", pattern = "fake_video58_[0-9]{5}.jpg")
#fake_video59_image_names <- list.files(path = "all_videos", pattern = "fake_video59_[0-9]{5}.jpg")
#fake_video60_image_names <- list.files(path = "all_videos", pattern = "fake_video60_[0-9]{5}.jpg")
#fake_video61_image_names <- list.files(path = "all_videos", pattern = "fake_video61_[0-9]{5}.jpg")
#fake_video62_image_names <- list.files(path = "all_videos", pattern = "fake_video62_[0-9]{5}.jpg")
#fake_video63_image_names <- list.files(path = "all_videos", pattern = "fake_video63_[0-9]{5}.jpg")
#fake_video64_image_names <- list.files(path = "all_videos", pattern = "fake_video64_[0-9]{5}.jpg")
#fake_video65_image_names <- list.files(path = "all_videos", pattern = "fake_video65_[0-9]{5}.jpg")
#fake_video66_image_names <- list.files(path = "all_videos", pattern = "fake_video66_[0-9]{5}.jpg")
#real_video1_image_names <- list.files(path = "all_videos", pattern = "real_video1_[0-9]{5}.jpg")
#real_video2_image_names <- list.files(path = "all_videos", pattern = "real_video2_[0-9]{5}.jpg")
#real_video3_image_names <- list.files(path = "all_videos", pattern = "real_video3_[0-9]{5}.jpg")
#real_video4_image_names <- list.files(path = "all_videos", pattern = "real_video4_[0-9]{5}.jpg")
#real_video5_image_names <- list.files(path = "all_videos", pattern = "real_video5_[0-9]{5}.jpg")
#real_video6_image_names <- list.files(path = "all_videos", pattern = "real_video6_[0-9]{5}.jpg")
#real_video7_image_names <- list.files(path = "all_videos", pattern = "real_video7_[0-9]{5}.jpg")
#real_video8_image_names <- list.files(path = "all_videos", pattern = "real_video8_[0-9]{5}.jpg")
#real_video9_image_names <- list.files(path = "all_videos", pattern = "real_video9_[0-9]{5}.jpg")
#real_video10_image_names <- list.files(path = "all_videos", pattern = "real_video10_[0-9]{5}.jpg")
#real_video11_image_names <- list.files(path = "all_videos", pattern = "real_video11_[0-9]{5}.jpg")
#real_video12_image_names <- list.files(path = "all_videos", pattern = "real_video12_[0-9]{5}.jpg")
#real_video13_image_names <- list.files(path = "all_videos", pattern = "real_video13_[0-9]{5}.jpg")
#real_video14_image_names <- list.files(path = "all_videos", pattern = "real_video14_[0-9]{5}.jpg")
#real_video15_image_names <- list.files(path = "all_videos", pattern = "real_video15_[0-9]{5}.jpg")
#real_video16_image_names <- list.files(path = "all_videos", pattern = "real_video16_[0-9]{5}.jpg")
#real_video17_image_names <- list.files(path = "all_videos", pattern = "real_video17_[0-9]{5}.jpg")
#real_video18_image_names <- list.files(path = "all_videos", pattern = "real_video18_[0-9]{5}.jpg")
#real_video19_image_names <- list.files(path = "all_videos", pattern = "real_video19_[0-9]{5}.jpg")
#real_video20_image_names <- list.files(path = "all_videos", pattern = "real_video20_[0-9]{5}.jpg")
#real_video21_image_names <- list.files(path = "all_videos", pattern = "real_video21_[0-9]{5}.jpg")
#real_video22_image_names <- list.files(path = "all_videos", pattern = "real_video22_[0-9]{5}.jpg")
#real_video23_image_names <- list.files(path = "all_videos", pattern = "real_video23_[0-9]{5}.jpg")
#real_video24_image_names <- list.files(path = "all_videos", pattern = "real_video24_[0-9]{5}.jpg")
#real_video25_image_names <- list.files(path = "all_videos", pattern = "real_video25_[0-9]{5}.jpg")
#real_video26_image_names <- list.files(path = "all_videos", pattern = "real_video26_[0-9]{5}.jpg")
#real_video27_image_names <- list.files(path = "all_videos", pattern = "real_video27_[0-9]{5}.jpg")
#real_video28_image_names <- list.files(path = "all_videos", pattern = "real_video28_[0-9]{5}.jpg")
#real_video29_image_names <- list.files(path = "all_videos", pattern = "real_video29_[0-9]{5}.jpg")
#real_video30_image_names <- list.files(path = "all_videos", pattern = "real_video30_[0-9]{5}.jpg")
#real_video31_image_names <- list.files(path = "all_videos", pattern = "real_video31_[0-9]{5}.jpg")
#real_video32_image_names <- list.files(path = "all_videos", pattern = "real_video32_[0-9]{5}.jpg")
#real_video33_image_names <- list.files(path = "all_videos", pattern = "real_video33_[0-9]{5}.jpg")

#image_mean <- function(x){
#  as.data.frame(load.image(paste0("all_videos/", x))) %>%
#    group_by(cc) %>%
#    # Imager reports pixel values in a 0-1 range, while the input for SSD300 wants the pixel values in a 0-255 range
#    summarise(mean = mean(value * 255), N = length(cc))
#}

# Outputting the mean of the color channels and number of observations per frame per video for a weighted average later
#practice_video_fake1_images_df_list <- lapply(practice_video_fake1_image_names, image_mean)
#saveRDS(practice_video_fake1_images_df_list, "temp\\practice_video_fake1_images_df_list.rds")
#rm(practice_video_fake1_image_names)
#practice_video_fake2_images_df_list <- lapply(practice_video_fake2_image_names, image_mean)
#saveRDS(practice_video_fake2_images_df_list, "temp\\practice_video_fake2_images_df_list.rds")
#rm(practice_video_fake2_image_names)
#fake_video1_images_df_list <- lapply(fake_video1_image_names, image_mean)
#saveRDS(fake_video1_images_df_list, "temp\\fake_video1_images_df_list.rds")
#rm(fake_video1_image_names)
#fake_video2_images_df_list <- lapply(fake_video2_image_names, image_mean)
#saveRDS(fake_video2_images_df_list, "temp\\fake_video2_images_df_list.rds")
#rm(fake_video2_image_names)
#fake_video3_images_df_list <- lapply(fake_video3_image_names, image_mean)
#saveRDS(fake_video3_images_df_list, "temp\\fake_video3_images_df_list.rds")
#rm(fake_video3_image_names)
#fake_video4_images_df_list <- lapply(fake_video4_image_names, image_mean)
#saveRDS(fake_video4_images_df_list, "temp\\fake_video4_images_df_list.rds")
#rm(fake_video4_image_names)
#fake_video5_images_df_list <- lapply(fake_video5_image_names, image_mean)
#saveRDS(fake_video5_images_df_list, "temp\\fake_video5_images_df_list.rds")
#rm(fake_video5_image_names)
#fake_video6_images_df_list <- lapply(fake_video6_image_names, image_mean)
#saveRDS(fake_video6_images_df_list, "temp\\fake_video6_images_df_list.rds")
#rm(fake_video6_image_names)
#fake_video7_images_df_list <- lapply(fake_video7_image_names, image_mean)
#saveRDS(fake_video7_images_df_list, "temp\\fake_video7_images_df_list.rds")
#rm(fake_video7_image_names)
#fake_video8_images_df_list <- lapply(fake_video8_image_names, image_mean)
#saveRDS(fake_video8_images_df_list, "temp\\fake_video8_images_df_list.rds")
#rm(fake_video8_image_names)
#fake_video9_images_df_list <- lapply(fake_video9_image_names, image_mean)
#saveRDS(fake_video9_images_df_list, "temp\\fake_video9_images_df_list.rds")
#rm(fake_video9_image_names)
#fake_video10_images_df_list <- lapply(fake_video10_image_names, image_mean)
#saveRDS(fake_video10_images_df_list, "temp\\fake_video10_images_df_list.rds")
#rm(fake_video10_image_names)
#fake_video11_images_df_list <- lapply(fake_video11_image_names, image_mean)
#saveRDS(fake_video11_images_df_list, "temp\\fake_video11_images_df_list.rds")
#rm(fake_video11_image_names)
#fake_video12_images_df_list <- lapply(fake_video12_image_names, image_mean)
#saveRDS(fake_video12_images_df_list, "temp\\fake_video12_images_df_list.rds")
#rm(fake_video12_image_names)
#fake_video13_images_df_list <- lapply(fake_video13_image_names, image_mean)
#saveRDS(fake_video13_images_df_list, "temp\\fake_video13_images_df_list.rds")
#rm(fake_video13_image_names)
#fake_video14_images_df_list <- lapply(fake_video14_image_names, image_mean)
#saveRDS(fake_video14_images_df_list, "temp\\fake_video14_images_df_list.rds")
#rm(fake_video14_image_names)
#fake_video15_images_df_list <- lapply(fake_video15_image_names, image_mean)
#saveRDS(fake_video15_images_df_list, "temp\\fake_video15_images_df_list.rds")
#rm(fake_video15_image_names)
#fake_video16_images_df_list <- lapply(fake_video16_image_names, image_mean)
#saveRDS(fake_video16_images_df_list, "temp\\fake_video16_images_df_list.rds")
#rm(fake_video16_image_names)
#fake_video17_images_df_list <- lapply(fake_video17_image_names, image_mean)
#saveRDS(fake_video17_images_df_list, "temp\\fake_video17_images_df_list.rds")
#rm(fake_video17_image_names)
#fake_video18_images_df_list <- lapply(fake_video18_image_names, image_mean)
#saveRDS(fake_video18_images_df_list, "temp\\fake_video18_images_df_list.rds")
#rm(fake_video18_image_names)
#fake_video19_images_df_list <- lapply(fake_video19_image_names, image_mean)
#saveRDS(fake_video19_images_df_list, "temp\\fake_video19_images_df_list.rds")
#rm(fake_video19_image_names)
#fake_video20_images_df_list <- lapply(fake_video20_image_names, image_mean)
#saveRDS(fake_video20_images_df_list, "temp\\fake_video20_images_df_list.rds")
#rm(fake_video20_image_names)
#fake_video21_images_df_list <- lapply(fake_video21_image_names, image_mean)
#saveRDS(fake_video21_images_df_list, "temp\\fake_video21_images_df_list.rds")
#rm(fake_video21_image_names)
#fake_video22_images_df_list <- lapply(fake_video22_image_names, image_mean)
#saveRDS(fake_video22_images_df_list, "temp\\fake_video22_images_df_list.rds")
#rm(fake_video22_image_names)
#fake_video23_images_df_list <- lapply(fake_video23_image_names, image_mean)
#saveRDS(fake_video23_images_df_list, "temp\\fake_video23_images_df_list.rds")
#rm(fake_video23_image_names)
#fake_video24_images_df_list <- lapply(fake_video24_image_names, image_mean)
#saveRDS(fake_video24_images_df_list, "temp\\fake_video24_images_df_list.rds")
#rm(fake_video24_image_names)
#fake_video25_images_df_list <- lapply(fake_video25_image_names, image_mean)
#saveRDS(fake_video25_images_df_list, "temp\\fake_video25_images_df_list.rds")
#rm(fake_video25_image_names)
#fake_video26_images_df_list <- lapply(fake_video26_image_names, image_mean)
#saveRDS(fake_video26_images_df_list, "temp\\fake_video26_images_df_list.rds")
#rm(fake_video26_image_names)
#fake_video27_images_df_list <- lapply(fake_video27_image_names, image_mean)
#saveRDS(fake_video27_images_df_list, "temp\\fake_video27_images_df_list.rds")
#rm(fake_video27_image_names)
#fake_video28_images_df_list <- lapply(fake_video28_image_names, image_mean)
#saveRDS(fake_video28_images_df_list, "temp\\fake_video28_images_df_list.rds")
#rm(fake_video28_image_names)
#fake_video29_images_df_list <- lapply(fake_video29_image_names, image_mean)
#saveRDS(fake_video29_images_df_list, "temp\\fake_video29_images_df_list.rds")
#rm(fake_video29_image_names)
#fake_video30_images_df_list <- lapply(fake_video30_image_names, image_mean)
#saveRDS(fake_video30_images_df_list, "temp\\fake_video30_images_df_list.rds")
#rm(fake_video30_image_names)
#fake_video31_images_df_list <- lapply(fake_video31_image_names, image_mean)
#saveRDS(fake_video31_images_df_list, "temp\\fake_video31_images_df_list.rds")
#rm(fake_video31_image_names)
#fake_video32_images_df_list <- lapply(fake_video32_image_names, image_mean)
#saveRDS(fake_video32_images_df_list, "temp\\fake_video32_images_df_list.rds")
#rm(fake_video32_image_names)
#fake_video33_images_df_list <- lapply(fake_video33_image_names, image_mean)
#saveRDS(fake_video33_images_df_list, "temp\\fake_video33_images_df_list.rds")
#rm(fake_video33_image_names)
#fake_video34_images_df_list <- lapply(fake_video34_image_names, image_mean)
#saveRDS(fake_video34_images_df_list, "temp\\fake_video34_images_df_list.rds")
#rm(fake_video34_image_names)
#fake_video35_images_df_list <- lapply(fake_video35_image_names, image_mean)
#saveRDS(fake_video35_images_df_list, "temp\\fake_video35_images_df_list.rds")
#rm(fake_video35_image_names)
#fake_video36_images_df_list <- lapply(fake_video36_image_names, image_mean)
#saveRDS(fake_video36_images_df_list, "temp\\fake_video36_images_df_list.rds")
#rm(fake_video36_image_names)
#fake_video37_images_df_list <- lapply(fake_video37_image_names, image_mean)
#saveRDS(fake_video37_images_df_list, "temp\\fake_video37_images_df_list.rds")
#rm(fake_video37_image_names)
#fake_video38_images_df_list <- lapply(fake_video38_image_names, image_mean)
#saveRDS(fake_video38_images_df_list, "temp\\fake_video38_images_df_list.rds")
#rm(fake_video38_image_names)
#fake_video39_images_df_list <- lapply(fake_video39_image_names, image_mean)
#saveRDS(fake_video39_images_df_list, "temp\\fake_video39_images_df_list.rds")
#rm(fake_video39_image_names)
#fake_video40_images_df_list <- lapply(fake_video40_image_names, image_mean)
#saveRDS(fake_video40_images_df_list, "temp\\fake_video40_images_df_list.rds")
#rm(fake_video40_image_names)
#fake_video41_images_df_list <- lapply(fake_video41_image_names, image_mean)
#saveRDS(fake_video41_images_df_list, "temp\\fake_video41_images_df_list.rds")
#rm(fake_video41_image_names)
#fake_video42_images_df_list <- lapply(fake_video42_image_names, image_mean)
#saveRDS(fake_video42_images_df_list, "temp\\fake_video42_images_df_list.rds")
#rm(fake_video42_image_names)
#fake_video43_images_df_list <- lapply(fake_video43_image_names, image_mean)
#saveRDS(fake_video43_images_df_list, "temp\\fake_video43_images_df_list.rds")
#rm(fake_video43_image_names)
#fake_video44_images_df_list <- lapply(fake_video44_image_names, image_mean)
#saveRDS(fake_video44_images_df_list, "temp\\fake_video44_images_df_list.rds")
#rm(fake_video44_image_names)
#fake_video45_images_df_list <- lapply(fake_video45_image_names, image_mean)
#saveRDS(fake_video45_images_df_list, "temp\\fake_video45_images_df_list.rds")
#rm(fake_video45_image_names)
#fake_video46_images_df_list <- lapply(fake_video46_image_names, image_mean)
#saveRDS(fake_video46_images_df_list, "temp\\fake_video46_images_df_list.rds")
#rm(fake_video46_image_names)
#fake_video47_images_df_list <- lapply(fake_video47_image_names, image_mean)
#saveRDS(fake_video47_images_df_list, "temp\\fake_video47_images_df_list.rds")
#rm(fake_video47_image_names)
#fake_video48_images_df_list <- lapply(fake_video48_image_names, image_mean)
#saveRDS(fake_video48_images_df_list, "temp\\fake_video48_images_df_list.rds")
#rm(fake_video48_image_names)
#fake_video49_images_df_list <- lapply(fake_video49_image_names, image_mean)
#saveRDS(fake_video49_images_df_list, "temp\\fake_video49_images_df_list.rds")
#rm(fake_video49_image_names)
#fake_video50_images_df_list <- lapply(fake_video50_image_names, image_mean)
#saveRDS(fake_video50_images_df_list, "temp\\fake_video50_images_df_list.rds")
#rm(fake_video50_image_names)
#fake_video51_images_df_list <- lapply(fake_video51_image_names, image_mean)
#saveRDS(fake_video51_images_df_list, "temp\\fake_video51_images_df_list.rds")
#rm(fake_video51_image_names)
#fake_video52_images_df_list <- lapply(fake_video52_image_names, image_mean)
#saveRDS(fake_video52_images_df_list, "temp\\fake_video52_images_df_list.rds")
#rm(fake_video52_image_names)
#fake_video53_images_df_list <- lapply(fake_video53_image_names, image_mean)
#saveRDS(fake_video53_images_df_list, "temp\\fake_video53_images_df_list.rds")
#rm(fake_video53_image_names)
#fake_video54_images_df_list <- lapply(fake_video54_image_names, image_mean)
#saveRDS(fake_video54_images_df_list, "temp\\fake_video54_images_df_list.rds")
#rm(fake_video54_image_names)
#fake_video55_images_df_list <- lapply(fake_video55_image_names, image_mean)
#saveRDS(fake_video55_images_df_list, "temp\\fake_video55_images_df_list.rds")
#rm(fake_video55_image_names)
#fake_video56_images_df_list <- lapply(fake_video56_image_names, image_mean)
#saveRDS(fake_video56_images_df_list, "temp\\fake_video56_images_df_list.rds")
#rm(fake_video56_image_names)
#fake_video57_images_df_list <- lapply(fake_video57_image_names, image_mean)
#saveRDS(fake_video57_images_df_list, "temp\\fake_video57_images_df_list.rds")
#rm(fake_video57_image_names)
#fake_video58_images_df_list <- lapply(fake_video58_image_names, image_mean)
#saveRDS(fake_video58_images_df_list, "temp\\fake_video58_images_df_list.rds")
#rm(fake_video58_image_names)
#fake_video59_images_df_list <- lapply(fake_video59_image_names, image_mean)
#saveRDS(fake_video59_images_df_list, "temp\\fake_video59_images_df_list.rds")
#rm(fake_video59_image_names)
#fake_video60_images_df_list <- lapply(fake_video60_image_names, image_mean)
#saveRDS(fake_video60_images_df_list, "temp\\fake_video60_images_df_list.rds")
#rm(fake_video60_image_names)
#fake_video61_images_df_list <- lapply(fake_video61_image_names, image_mean)
#saveRDS(fake_video61_images_df_list, "temp\\fake_video61_images_df_list.rds")
#rm(fake_video61_image_names)
#fake_video62_images_df_list <- lapply(fake_video62_image_names, image_mean)
#saveRDS(fake_video62_images_df_list, "temp\\fake_video62_images_df_list.rds")
#rm(fake_video62_image_names)
#fake_video63_images_df_list <- lapply(fake_video63_image_names, image_mean)
#saveRDS(fake_video63_images_df_list, "temp\\fake_video63_images_df_list.rds")
#rm(fake_video63_image_names)
#fake_video64_images_df_list <- lapply(fake_video64_image_names, image_mean)
#saveRDS(fake_video64_images_df_list, "temp\\fake_video64_images_df_list.rds")
#rm(fake_video64_image_names)
#fake_video65_images_df_list <- lapply(fake_video65_image_names, image_mean)
#saveRDS(fake_video65_images_df_list, "temp\\fake_video65_images_df_list.rds")
#rm(fake_video65_image_names)
#fake_video66_images_df_list <- lapply(fake_video66_image_names, image_mean)
#saveRDS(fake_video66_images_df_list, "temp\\fake_video66_images_df_list.rds")
#rm(fake_video66_image_names)
#real_video1_images_df_list <- lapply(real_video1_image_names, image_mean)
#saveRDS(real_video1_images_df_list, "temp\\real_video1_images_df_list.rds")
#rm(real_video1_image_names)
#real_video2_images_df_list <- lapply(real_video2_image_names, image_mean)
#saveRDS(real_video2_images_df_list, "temp\\real_video2_images_df_list.rds")
#rm(real_video2_image_names)
#real_video3_images_df_list <- lapply(real_video3_image_names, image_mean)
#saveRDS(real_video3_images_df_list, "temp\\real_video3_images_df_list.rds")
#rm(real_video3_image_names)
#real_video4_images_df_list <- lapply(real_video4_image_names, image_mean)
#saveRDS(real_video4_images_df_list, "temp\\real_video4_images_df_list.rds")
#rm(real_video4_image_names)
#real_video5_images_df_list <- lapply(real_video5_image_names, image_mean)
#saveRDS(real_video5_images_df_list, "temp\\real_video5_images_df_list.rds")
#rm(real_video5_image_names)
#real_video6_images_df_list <- lapply(real_video6_image_names, image_mean)
#saveRDS(real_video6_images_df_list, "temp\\real_video6_images_df_list.rds")
#rm(real_video6_image_names)
#real_video7_images_df_list <- lapply(real_video7_image_names, image_mean)
#saveRDS(real_video7_images_df_list, "temp\\real_video7_images_df_list.rds")
#rm(real_video7_image_names)
#real_video8_images_df_list <- lapply(real_video8_image_names, image_mean)
#saveRDS(real_video8_images_df_list, "temp\\real_video8_images_df_list.rds")
#rm(real_video8_image_names)
#real_video9_images_df_list <- lapply(real_video9_image_names, image_mean)
#saveRDS(real_video9_images_df_list, "temp\\real_video9_images_df_list.rds")
#rm(real_video9_image_names)
#real_video10_images_df_list <- lapply(real_video10_image_names, image_mean)
#saveRDS(real_video10_images_df_list, "temp\\real_video10_images_df_list.rds")
#rm(real_video10_image_names)
#real_video11_images_df_list <- lapply(real_video11_image_names, image_mean)
#saveRDS(real_video11_images_df_list, "temp\\real_video11_images_df_list.rds")
#rm(real_video11_image_names)
#real_video12_images_df_list <- lapply(real_video12_image_names, image_mean)
#saveRDS(real_video12_images_df_list, "temp\\real_video12_images_df_list.rds")
#rm(real_video12_image_names)
#real_video13_images_df_list <- lapply(real_video13_image_names, image_mean)
#saveRDS(real_video13_images_df_list, "temp\\real_video13_images_df_list.rds")
#rm(real_video13_image_names)
#real_video14_images_df_list <- lapply(real_video14_image_names, image_mean)
#saveRDS(real_video14_images_df_list, "temp\\real_video14_images_df_list.rds")
#rm(real_video14_image_names)
#real_video15_images_df_list <- lapply(real_video15_image_names, image_mean)
#saveRDS(real_video15_images_df_list, "temp\\real_video15_images_df_list.rds")
#rm(real_video15_image_names)
#real_video16_images_df_list <- lapply(real_video16_image_names, image_mean)
#saveRDS(real_video16_images_df_list, "temp\\real_video16_images_df_list.rds")
#rm(real_video16_image_names)
#real_video17_images_df_list <- lapply(real_video17_image_names, image_mean)
#saveRDS(real_video17_images_df_list, "temp\\real_video17_images_df_list.rds")
#rm(real_video17_image_names)
#real_video18_images_df_list <- lapply(real_video18_image_names, image_mean)
#saveRDS(real_video18_images_df_list, "temp\\real_video18_images_df_list.rds")
#rm(real_video18_image_names)
#real_video19_images_df_list <- lapply(real_video19_image_names, image_mean)
#saveRDS(real_video19_images_df_list, "temp\\real_video19_images_df_list.rds")
#rm(real_video19_image_names)
#real_video20_images_df_list <- lapply(real_video20_image_names, image_mean)
#saveRDS(real_video20_images_df_list, "temp\\real_video20_images_df_list.rds")
#rm(real_video20_image_names)
#real_video21_images_df_list <- lapply(real_video21_image_names, image_mean)
#saveRDS(real_video21_images_df_list, "temp\\real_video21_images_df_list.rds")
#rm(real_video21_image_names)
#real_video22_images_df_list <- lapply(real_video22_image_names, image_mean)
#saveRDS(real_video22_images_df_list, "temp\\real_video22_images_df_list.rds")
#rm(real_video22_image_names)
#real_video23_images_df_list <- lapply(real_video23_image_names, image_mean)
#saveRDS(real_video23_images_df_list, "temp\\real_video23_images_df_list.rds")
#rm(real_video23_image_names)
#real_video24_images_df_list <- lapply(real_video24_image_names, image_mean)
#saveRDS(real_video24_images_df_list, "temp\\real_video24_images_df_list.rds")
#rm(real_video24_image_names)
#real_video25_images_df_list <- lapply(real_video25_image_names, image_mean)
#saveRDS(real_video25_images_df_list, "temp\\real_video25_images_df_list.rds")
#rm(real_video25_image_names)
#real_video26_images_df_list <- lapply(real_video26_image_names, image_mean)
#saveRDS(real_video26_images_df_list, "temp\\real_video26_images_df_list.rds")
#rm(real_video26_image_names)
#real_video27_images_df_list <- lapply(real_video27_image_names, image_mean)
#saveRDS(real_video27_images_df_list, "temp\\real_video27_images_df_list.rds")
#rm(real_video27_image_names)
#real_video28_images_df_list <- lapply(real_video28_image_names, image_mean)
#saveRDS(real_video28_images_df_list, "temp\\real_video28_images_df_list.rds")
#rm(real_video28_image_names)
#real_video29_images_df_list <- lapply(real_video29_image_names, image_mean)
#saveRDS(real_video29_images_df_list, "temp\\real_video29_images_df_list.rds")
#rm(real_video29_image_names)
#real_video30_images_df_list <- lapply(real_video30_image_names, image_mean)
#saveRDS(real_video30_images_df_list, "temp\\real_video30_images_df_list.rds")
#rm(real_video30_image_names)
#real_video31_images_df_list <- lapply(real_video31_image_names, image_mean)
#saveRDS(real_video31_images_df_list, "temp\\real_video31_images_df_list.rds")
#rm(real_video31_image_names)
#real_video32_images_df_list <- lapply(real_video32_image_names, image_mean)
#saveRDS(real_video32_images_df_list, "temp\\real_video32_images_df_list.rds")
#rm(real_video32_image_names)
#real_video33_images_df_list <- lapply(real_video33_image_names, image_mean)
#saveRDS(real_video33_images_df_list, "temp\\real_video33_images_df_list.rds")
#rm(real_video33_image_names)

#images_df <- bind_rows(practice_video_fake1_images_df_list, practice_video_fake2_images_df_list, fake_video1_images_df_list, 
#fake_video2_images_df_list, fake_video3_images_df_list, fake_video4_images_df_list, fake_video5_images_df_list, 
#fake_video6_images_df_list, fake_video7_images_df_list, fake_video8_images_df_list, fake_video9_images_df_list, 
#fake_video10_images_df_list, fake_video11_images_df_list, fake_video12_images_df_list, fake_video13_images_df_list, 
#fake_video14_images_df_list, fake_video15_images_df_list, fake_video16_images_df_list, fake_video17_images_df_list, 
#fake_video18_images_df_list, fake_video19_images_df_list, fake_video20_images_df_list, fake_video21_images_df_list, 
#fake_video22_images_df_list, fake_video23_images_df_list, fake_video24_images_df_list, fake_video25_images_df_list, 
#fake_video26_images_df_list, fake_video27_images_df_list, fake_video28_images_df_list, fake_video29_images_df_list, 
#fake_video30_images_df_list, fake_video31_images_df_list, fake_video32_images_df_list, fake_video33_images_df_list, 
#fake_video34_images_df_list, fake_video35_images_df_list, fake_video36_images_df_list, fake_video37_images_df_list, 
#fake_video38_images_df_list, fake_video39_images_df_list, fake_video40_images_df_list, fake_video41_images_df_list, 
#fake_video42_images_df_list, fake_video43_images_df_list, fake_video44_images_df_list, fake_video45_images_df_list, 
#fake_video46_images_df_list, fake_video47_images_df_list, fake_video48_images_df_list, fake_video49_images_df_list, 
#fake_video50_images_df_list, fake_video51_images_df_list, fake_video52_images_df_list, fake_video53_images_df_list, 
#fake_video54_images_df_list, fake_video55_images_df_list, fake_video56_images_df_list, fake_video57_images_df_list, 
#fake_video58_images_df_list, fake_video59_images_df_list, fake_video60_images_df_list, fake_video61_images_df_list, 
#fake_video62_images_df_list, fake_video63_images_df_list, fake_video64_images_df_list, fake_video65_images_df_list, 
#fake_video66_images_df_list, real_video1_images_df_list, real_video2_images_df_list, real_video3_images_df_list, 
#real_video4_images_df_list, real_video5_images_df_list, real_video6_images_df_list, real_video7_images_df_list, 
#real_video8_images_df_list, real_video9_images_df_list, real_video10_images_df_list, real_video11_images_df_list, 
#real_video12_images_df_list, real_video13_images_df_list, real_video14_images_df_list, real_video15_images_df_list, 
#real_video16_images_df_list, real_video17_images_df_list, real_video18_images_df_list, real_video19_images_df_list, 
#real_video20_images_df_list, real_video21_images_df_list, real_video22_images_df_list, real_video23_images_df_list, 
#real_video24_images_df_list, real_video25_images_df_list, real_video26_images_df_list, real_video27_images_df_list, 
#real_video28_images_df_list, real_video29_images_df_list, real_video30_images_df_list, real_video31_images_df_list, 
#real_video32_images_df_list, real_video33_images_df_list)

# Taking the weighted average
#images_mean <- group_by(images_df, cc) %>% summarise(mean_wavg = round(sum((N / sum(N)) * mean)))
#images_mean$cc[images_mean$cc == 1] <- "R"
#images_mean$cc[images_mean$cc == 2] <- "G"
#images_mean$cc[images_mean$cc == 3] <- "B"

# Per-channel average of the images in the dataset:
# R = 139
# G = 121
# B = 113

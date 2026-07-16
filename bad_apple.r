install.packages(c("imager", "av"))
library(imager)
library(av)

FPS = 10
source_file = "~/bad_apple.mp4"
temp_frame_folder = "./frames"

# Kung wala ang folder, himo-a; kondili, way mahitabo
if (!dir.exists(temp_frame_folder)) {
  paste("Gihimo na ang", temp_frame_folder)
  dir.create(temp_frame_folder)
}

# Ipa-convert gikan bidyo paadto hulagway
av_video_images(
  source_file,
  destdir = temp_frame_folder,
  fps = FPS
)

# Kuhaa ang path sa mga hulahulagway
frames_path <- list.files(path=temp_frame_folder, full.names = TRUE)[20:25]

# Temp Listahan
list_of_dfs = list()

for (i in seq_along(frames_path)) {
  # Ipa-grayscale para siguradong BW ang dekolor
  img <- grayscale(load.image(frames_path[[i]]))

  # Pangitaa ang mga ngilbit; dili saktong kilid kay aproksima ra
  edges <- cannyEdges(img)
  
  # Ipa-dataframe
  edge_df <- as.data.frame(edges)
  
  # Baliktara ang y para sa pagplot
  edge_df$y <- max(edge_df$y) - edge_df$y
  
  # Frame number
  edge_df$ika_ <- i
  
  # Tangala ang di kailangan
  edge_df <- subset(edge_df, select = -c(z, cc))
  
  # Ipalista
  list_of_dfs[[length(list_of_dfs) + 1]] = edge_df
  
  paste("Giproseso na ang ika-", i, "na frame")
}


# Final edge_df -- patong-patonga ang tanang listahan
edge_df <- do.call(rbind, list_of_dfs)
edge_df


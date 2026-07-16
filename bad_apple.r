install.packages(c("imager", "av"))
library(imager)
library(av)

FPS = 10
SPF = 1 / FPS
source_file = "~/bad_apple.mp4"
temp_frame_folder = "./frames"

# Kung wala ang folder, himo-a; kondili, way mahitabo
if (!dir.exists(temp_frame_folder)) {
  print(paste("Gihimo na ang", temp_frame_folder))
  dir.create(temp_frame_folder)
}

# Ipa-convert gikan bidyo paadto hulagway
av_video_images(
  source_file,
  destdir = temp_frame_folder,
  fps = FPS
)

# Kuhaa ang path sa mga hulahulagway
frames_path <- list.files(path=temp_frame_folder, full.names = TRUE)[1:100]

# Temp Listahan
list_of_dfs = list()

# Usa-usahon ang mga hulagway
for (i in seq_along(frames_path)) {
  # Ipa-grayscale para siguradong naka-gray ang dekolor
  img <- grayscale(load.image(frames_path[[i]]))
  
  # Ipa-BW
  img <- (img > 0.5) * 1

  print(paste0("Giproseso na ang ika-", i, " na frame"))
  # Pangitaa ang mga ngilbit; dili saktong kilid kay aproksima ra
  if (length(unique(img)) > 1) {
    edges <- cannyEdges(img, t1 = 0.01, t2 = 0.05)
    
    # Ipa-dataframe
    edge_df <- as.data.frame(edges)
    
    # Tangala ang di kailangan
    edge_df <- subset(edge_df, select = -c(z, cc))
    
    # Baliktara ang y para sa pagplot
    edge_df$y <- max(edge_df$y) - edge_df$y
  } else {
    # Pangblangko (dili makit-an sa pangplot)
    pngblangko = -67

    edge_df <- data.frame(x = c(pngblangko), y = c(pngblangko))
  }
  
  # Frame number
  edge_df$ika_ <- i
  
  # Ipalista
  list_of_dfs[[length(list_of_dfs) + 1]] = edge_df
  
  
}


# Final edge_df -- patong-patonga ang tanang listahan
edge_df <- do.call(rbind, list_of_dfs)
edge_df

# I-save in case mawala
write.csv(edge_df, "./edges_bad_apple.csv", row.names = FALSE)

# Itanggal ang naka-temp
unlink(temp_frame_folder, recursive = TRUE, force = TRUE)

print(paste("Napatong na tanan ang edge_df, nabutang sa .csv para sigurado,", 
            "ug gitanggal ang naka-temp na folder"))


for (frame in seq_along(frames_path)) {
  
  ipakita <- edge_df[edge_df$ika_ == frame, ]
  
  plot(ipakita$x, ipakita$y, xlim=c(0, width(edges)), ylim=c(0, height(edges)))
  
  rect(-40, -40, 500, 500, col="#EBEBEB", border=NA)
  grid(NULL, NULL, lty=1, lwd=1, col="#FFFFFF")
  
  points(ipakita$x, ipakita$y, xlim=c(0, width(edges)), ylim=c(0, height(edges)))
  
  Sys.sleep(SPF)
}


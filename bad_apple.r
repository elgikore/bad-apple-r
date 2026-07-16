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
  
  # Ipa-BW para duha lang ka kolor
  img <- (img > 0.5) * 1

  print(paste0("Giproseso na ang ika-", i, " na frame"))
  
  
  # Kung naay duha ka kulay (itim ug puti), ipa-Canny
  if (length(unique(img)) > 1) {
    # Pangitaa ang mga ngilbit; dili saktong kilid kay aproksima ra
    edges <- cannyEdges(img, t1 = 0.01, t2 = 0.05)
    
    # Ipa-dataframe
    edge_df <- as.data.frame(edges)
    
    # Tangala ang di kailangan
    edge_df <- subset(edge_df, select = -c(z, cc))
    
    # Baliktara ang y para sa pagplot
    edge_df$y <- max(edge_df$y) - edge_df$y
  } else {
    # Kondili, ipablangko
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

wlen = width(edges)
hlen = height(edges)
major_gap_x = 50
major_gap_y = 25
tuldok_style = 21
x_ticks = seq(-major_gap_x, wlen + major_gap_x, major_gap_x)
y_ticks = seq(-major_gap_y, hlen + major_gap_y, major_gap_y)

for (frame in seq_along(frames_path)) {
  
  ipakita <- edge_df[edge_df$ika_ == frame, ]
  
  plot(ipakita$x, ipakita$y,
       xlab="Kalapdon",
       ylab="Katas-on",
       xlim=c(0, width(edges)), 
       ylim=c(0, height(edges)),
       axes=FALSE)
  
  rect(-40, -40, 500, 500, col="#EBEBEB", border=NA)
  grid(NULL, NULL, lty=1, lwd=1, col="#FFFFFF")
  
  points(ipakita$x, ipakita$y,
         pch=tuldok_style,
         xlim=c(0, wlen), 
         ylim=c(0, hlen))

  axis(1, at=x_ticks, lwd.ticks=0.5)
  axis(2, at=y_ticks, lwd.ticks=0.5, las=2)
  axis(3, at=x_ticks, lwd.ticks=0.5, tcl=0, labels=FALSE)
  axis(4, at=y_ticks, lwd.ticks=0.5, tcl=0, labels=FALSE)
  
  title(
    main="Hulag 1. Binad Apple",
    adj=0,
    line=0.8,
    cex.main = 1.5
  )
  
  text(x = 240, y = 389, "(In the Style of Bad Apple)", xpd=NA, font=3, 
       col="darkgray")
  
  legend("topright", c("Tuldok"), 
         inset=c(0, -0.08),
         pch=tuldok_style, 
         bty="o",
         xpd=TRUE)
  
  Sys.sleep(SPF)
}


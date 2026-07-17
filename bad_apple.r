# install.packages(c("imager", "av"))
library(imager)
library(av)

FPS <- 10
SPF <- 1 / FPS
source_file <- "~/bad_apple.mp4"
temp_frame_folder <- "./frames"

# Kung wala ang folder, himo-a; kondili, way mahitabo
if (!dir.exists(temp_frame_folder)) {
  print(paste("Gihimo na ang", temp_frame_folder))
  dir.create(temp_frame_folder)
}

# Ipa-convert gikan bidyo paadto hulagway
av_video_images(
  source_file,
  destdir=temp_frame_folder,
  fps=FPS
)

# Kuhaa ang path sa mga hulahulagway
frames_path <- list.files(path=temp_frame_folder, full.names=TRUE)

# Temp Listahan
list_of_dfs <- list()

# Usa-usahon ang mga hulagway
for (i in seq_along(frames_path)) {
  # Ipa-grayscale para siguradong naka-gray ang dekolor
  img <- grayscale(load.image(frames_path[[i]]))
  
  # Ipa-BW para duha lang ka kolor
  img <- (img > 0.5) * 1

  print(paste0("Ginaproseso ron ang ika-", i, " na frame"))
  
  # Kung naay duha ka kulay (itim ug puti), ipa-Canny
  if (length(unique(img)) > 1) {
    # Pangitaa ang mga ngilbit; dili saktong kilid kay aproksima ra
    # t1=0.01, t2=0.05 para naay allowance, ug dili mag-kmeans
    edges <- cannyEdges(img, t1=0.01, t2=0.05)
    
    # Ipa-dataframe
    edge_df <- as.data.frame(edges)
    
    # Tangala ang di kailangan
    edge_df <- subset(edge_df, select = -c(z, cc))
    
    # Baliktara ang y para sa pagplot
    h_imager <- dim(img)[2] # Pinakataas
    edge_df$y <- h_imager - edge_df$y
  } else {
    # Kondili, ipablangko
    # Pangblangko (dili makit-an sa pangplot)
    pngblangko <- -67

    edge_df <- data.frame(x = c(pngblangko), y = c(pngblangko))
  }
  
  # Frame number
  edge_df$ika_ <- i
  
  # Ipalista
  list_of_dfs[[length(list_of_dfs) + 1]] <- edge_df
}


# Final edge_df -- patong-patonga ang tanang listahan
edge_df <- do.call(rbind, list_of_dfs)
edge_df

# Itanggal ang naka-temp
unlink(temp_frame_folder, recursive = TRUE, force = TRUE)

print("Napatong na tanan ang edge_df ug gitanggal ang naka-temp na folder")

# Setting sa Plot
## Panguna
wlen <- width(edges)
hlen <- height(edges)
major_gap_x <- 50
major_gap_y <- 25
tuldok_style <- 21
x_ticks <- seq(-major_gap_x, wlen + major_gap_x, major_gap_x)
y_ticks <- seq(-major_gap_y, hlen + major_gap_y, major_gap_y)

## Histograms
bin_width <- 25
hist_gap_x <- 50
hist_gap_y <- 100
hist_ylim <- 700
hist_xlim_x <- wlen + bin_width
hist_xlim_y <- hlen + bin_width
hist_x_ticks <- seq(-hist_gap_x, wlen + (hist_gap_x * 2), hist_gap_x) 
hist_y_ticks <- seq(-hist_gap_x, 400, hist_gap_x)
hist_ihap_ticks <- seq(-hist_gap_y, hist_ylim + 100, hist_gap_y)
hulag_2 <- "Hulag 2. Distribusyon sa X"
hulag_3 <- "Hulag 3. Distribusyon sa Y"
hist_y_label <- "Ihap"
hulag_2_x_label <- "X"
hulag_3_x_label <- "Y"

## Tanggala ang naulahi ug ipa-concat sa blangkong string para parehas
## ang katas-on sa vector sa hist_y_ticks
hist_y_labels <- c(hist_y_ticks[-length(hist_y_ticks)], "")


for (i in 1:5) {
  print(paste("Magsugod na in", i))
  Sys.sleep(1)
}

print("Gipasalida na ang Binad Apple.")


for (frame in seq_along(frames_path)) {
  # Tuldokanan sa Frame
  ipakita <- edge_df[edge_df$ika_ == frame, ]

  # Kanbas
  layout(
    matrix(c(1, 2,
             1, 3),
           nrow=2,
           byrow=TRUE),  # byrow=TRUE kay mali ang pagkabutang sa FALSE
    widths  = c(2, 1),   # 2x wala, 1x tuo
    heights = c(1, 1)    # 1x taas-ubos
  )

  # Panguna
  plot(ipakita$x, ipakita$y,
       xlab="Kalapdon",
       ylab="Katas-on",
       xlim=c(0, wlen),
       ylim=c(0, hlen),
       axes=FALSE)

  # BG (pina-ggplot) sa Panguna
  rect(-40, -40, 500, 500, col="#EBEBEB", border=NA)
  grid(NULL, NULL, lty=1, lwd=1, col="#FFFFFF")

  # Mga Tinuldokan
  points(ipakita$x, ipakita$y,
         pch=tuldok_style,
         xlim=c(0, wlen),
         ylim=c(0, hlen))

  # Mano-mano sa axis
  axis(1, at=x_ticks, lwd.ticks=0.5)
  axis(2, at=y_ticks, lwd.ticks=0.5, las=2)
  axis(3, at=x_ticks, lwd.ticks=0.5, tcl=0, labels=FALSE)
  axis(4, at=y_ticks, lwd.ticks=0.5, tcl=0, labels=FALSE)

  # Pangalan, subtitle, legend
  title(
    main="Hulag 1. Binad Apple",
    adj=0,
    line=0.8,
    cex.main = 1.5
  )

  # I-adjust lang ang panid sa plot para tama ang pagkakita sa title ug 
  # subtitle
  text(x = 216, y = 387, "(In the Style of Bad Apple)", xpd=NA, cex=1.5)

  legend("topright", c("Tuldok"),
         inset=c(0, -0.07),
         pch=tuldok_style,
         bty="o",
         xpd=TRUE)

  if(all(ipakita$x == pngblangko & ipakita$y == pngblangko)) {
    # Blangko na Histogram
    plot(
      NA, NA,
      xlim=c(0, hist_xlim_x),
      ylim=c(0, hist_ylim),
      xlab=hulag_2_x_label,
      ylab=hist_y_label,
      axes=FALSE
    )
    
    title(
      main=hulag_2,
      adj=0,
      line=0.8
    )
    
    # Mano-mano nasad sa grid ug axes
    rect(-40, -40, hist_ylim + 100, hist_ylim + 100, col="#EBEBEB", border=NA)
    grid(NULL, NULL, lty=1, lwd=1, col="#FFFFFF")
    axis(1, at=hist_x_ticks, lwd.ticks=0.5)
    axis(2, at=hist_ihap_ticks, lwd.ticks=0.5, las=2)
    axis(3, at=hist_x_ticks, lwd.ticks=0.5, tcl=0, labels=FALSE)
    axis(4, at=hist_ihap_ticks, lwd.ticks=0.5, tcl=0, labels=FALSE)

    plot(
      NA, NA,
      xlim=c(0, hist_xlim_y),
      ylim=c(0, hist_ylim),
      xlab=hulag_3_x_label,
      ylab=hist_y_label,
      axes=FALSE
    )
    
    title(
      main=hulag_3,
      adj=0,
      line=0.8
    )
    
    # Mano-mano nasad sa grid ug axes
    rect(-40, -40, hist_ylim + 100, hist_ylim + 100, col="#EBEBEB", border=NA)
    grid(NULL, NULL, lty=1, lwd=1, col="#FFFFFF")
    axis(1, at=hist_y_ticks, lwd.ticks=0.5, labels=hist_y_labels)
    axis(2, at=hist_ihap_ticks, lwd.ticks=0.5, las=2)
    axis(3, at=hist_y_ticks, lwd.ticks=0.5, tcl=0, labels=FALSE)
    axis(4, at=hist_ihap_ticks, lwd.ticks=0.5, tcl=0, labels=FALSE)

    next
  }

  # Histogram sa X
  hist(ipakita$x,
       breaks = seq(0, hist_xlim_x, bin_width),
       col="#619CFF", # Pina-ggplot na kolor
       main="",
       xlim=c(0, hist_xlim_x),
       ylim=c(0, hist_ylim),
       xlab=hulag_2_x_label,
       ylab=hist_y_label,

       # Pre-render
       panel.first={
         rect(-40, -40, hist_ylim + 100, hist_ylim + 100, col="#EBEBEB", border=NA)
         grid(NULL, NULL, lty=1, lwd=1, col="#FFFFFF")
       },
       axes=FALSE)
  
  title(
    main=hulag_2,
    adj=0,
    line=0.8
  )

  # Mano-mano sa axis
  axis(1, at=hist_x_ticks, lwd.ticks=0.5)
  axis(2, at=hist_ihap_ticks, lwd.ticks=0.5, las=2)
  axis(3, at=hist_x_ticks, lwd.ticks=0.5, tcl=0, labels=FALSE)
  axis(4, at=hist_ihap_ticks, lwd.ticks=0.5, tcl=0, labels=FALSE)


  # Histogram sa Y
  hist(ipakita$y,
       breaks = seq(0, hist_xlim_y, bin_width),
       col="#00BA38", # Pina-ggplot na kolor
       main="",
       xlim=c(0, hist_xlim_y),
       ylim=c(0, hist_ylim),
       xlab=hulag_3_x_label,
       ylab=hist_y_label,

       # Pre-render
       panel.first={
         rect(-40, -40, hist_ylim + 100, hist_ylim + 100, col="#EBEBEB", border=NA)
         grid(NULL, NULL, lty=1, lwd=1, col="#FFFFFF")
       },
       axes=FALSE)
  
  title(
    main=hulag_3,
    adj=0,
    line=0.8
  )

  # Mano-mano sa axis
  axis(1, at=hist_y_ticks, lwd.ticks=0.5, labels=hist_y_labels)
  axis(2, at=hist_ihap_ticks, lwd.ticks=0.5, las=2)
  axis(3, at=hist_y_ticks, lwd.ticks=0.5, tcl=0, labels=FALSE)
  axis(4, at=hist_ihap_ticks, lwd.ticks=0.5, tcl=0, labels=FALSE)
  
  # Pangdelay
  Sys.sleep(SPF)
}


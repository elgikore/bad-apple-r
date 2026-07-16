install.packages(c("imager", "av"))
library(imager)
library(av)

FPS = 10
source_file = "~/bad_apple.mp4"
temp_frame_folder = "./frames"

if (!dir.exists(temp_frame_folder)) {
  paste("Created", temp_frame_folder)
  dir.create(temp_frame_folder)
}

av_video_images(
  source_file,
  destdir = temp_frame_folder,
  fps = FPS
)
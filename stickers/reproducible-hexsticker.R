#install.packages("hexSticker")
if (Sys.info()["sysname"] == "Darwin") install.packages("sysfonts")
library(hexSticker)
library(showtext)
library(sysfonts)

## Loading Google fonts (http://www.google.com/fonts)
font_add_google("Bree Serif", "bree")
## Automatically use showtext to render text for future devices
showtext_auto()

imgurl <- "https://cdn2.iconfinder.com/data/icons/synchronize-1/24/synchronize-hexagon-1-512.png"

sticker(imgurl, package = "reproducible",
        h_color = "#006dff", h_fill = "#cccccc",
        p_color = "#006dff", p_family = "bree", p_size = 18, p_x = 0.85, p_y = 1.3,
        s_x = 1.1, s_y = 1, s_width = 0.8, s_height = 0.8,
        url = "http://reproducible.predictiveecology.org", u_color = "#006dff", u_size = 3.8,
        filename = "stickers/hexsticker.png", spotlight = FALSE)

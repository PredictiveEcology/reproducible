#install.packages("hexSticker")
if (Sys.info()["sysname"] == "Darwin") install.packages("sysfonts")
library(hexSticker)
library(showtext)
library(sysfonts)

## Loading Google fonts (http://www.google.com/fonts)
font_add_google("Bree Serif", "bree")
## Automatically use showtext to render text for future devices
showtext_auto()

imgurl <- "https://www.google.com/url?sa=i&source=images&cd=&ved=2ahUKEwjmzIeE_5DlAhXPIDQIHRWdDk0QjRx6BAgBEAQ&url=https%3A%2F%2Fwww.iconfinder.com%2Ficons%2F2651766%2Farrows_hexagon_loop_reload_reverse_synchronize_icon&psig=AOvVaw0ZHyd4NP6-LCL0iNRaxbk8&ust=1570772782869727"

dir.create("inst/figures")

sticker(imgurl, package = "reproducible",
        h_color = "#006dff", h_fill = "#cccccc",
        p_color = "#006dff", p_family = "bree", p_size = 18, p_x = 0.85, p_y = 1.3,
        s_x = 1.1, s_y = 1, s_width = 0.8, s_height = 0.8,
        url = "http://reproducible.predictiveecology.org", u_color = "#006dff", u_size = 3.8,
        filename = "~/GitHub/PredictiveEcology/reproducible/inst/figures/hexsticker.png", spotlight = FALSE)

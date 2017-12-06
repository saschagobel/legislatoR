# ---------------------------------------------------------------------------------------
# legislatoR
# Sascha Göbel and Simon Munzert
# Script: logo
# December 2017
# ---------------------------------------------------------------------------------------

library(hexSticker)
library(showtext)

setwd("D:/Sascha/projects/legislatoR/images")


font_add_google("Cabin Sketch", "cabin")



sticker(subplot = "angrymanfist2.png",
        package = "legislatoR",
        p_x = 1,
        p_y = 1.45,
        p_color = "black",
        p_size = 20,
        p_family = "cabin",
        h_size = 1.5,
        h_fill = "white",
        h_color = "black",
        s_width = 0.57,
        s_height = 0.3,
        s_x = 0.89,
        s_y = 0.65,
        spotlight = FALSE,
        url = "https://github.com/saschagobel/legislatoR",
        u_size = 3,
        u_y = 0.06,
        filename = "sticker.jpg"
        )

# ---------------------------------------------------------------------------------------
# legislatoR
# Sascha Göbel and Simon Munzert
# Script: logo
# December 2017
# ---------------------------------------------------------------------------------------

library(hexSticker)
library(showtext)

setwd("D:/Sascha/projects/legislatoR/images")


font_add_google("IM Fell French Canon SC", "political")



sticker(subplot = "new_logo.png",
        package = "legislatoR",
        p_x = 1,
        p_y = 1.45,
        p_color = "black",
        p_size = 70,
        p_family = "political",
        h_size = 1.5,
        h_fill = "white",
        h_color = "black",
        asp = 8,
        s_x = 1,
        s_y = 0.7,
        spotlight = FALSE,
        filename = "sticker.jpg",
        white_around_sticker = TRUE,
        dpi = 1000
        )

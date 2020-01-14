library(hexSticker)
library(magick)
library(sysfonts)

# import logo -----------------------------------------------------------------

img <- image_read("~/Documents/github/hexLogoGerke/other-logos/unicoRns/unicorn_Rlogo.png")

# create hex sticker ----------------------------------------------------------

sticker(img, package="unicoRns are real", p_size=5, s_x=1, s_y=0.9,
        s_width = 0.9, s_height = 0.9, h_fill = "black", h_color = "deeppink",
        url = "www.gerkelab.com", u_color = "white", u_size = 1.3,
        filename="~/Documents/github/hexLogoGerke/other-logos/unicoRns/unicorn_hex.png")


library(hexSticker) # Create Hexagon Sticker in R
library(showtext)   # Using Fonts More Easily in R Graphs

sticker(
  subplot = "factorial_cube.png",
  s_x = 1,
  s_y = 1.1,
  s_width = 0.55,
  asp  = 1,
  package = "doeR",                 
  p_size = 40,                       
  p_y = 0.4,
  p_color =  "#000000", 
  h_fill = "#ffc425",               
  h_color = "#000000",        
  dpi=600,                         
  filename="logo.png"
)

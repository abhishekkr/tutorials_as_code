#!/usr/bin/env python

"""
## Python Pillow Tutorial - Examples & Programs

> [source](https://pythonexamples.org/python-pillow/)

* install `pip install Pillow`

* import image handler package, read image, show it & print its size

"""


from PIL import Image, ImageEnhance


path_to_image = "images/dream-cloud.jpg"

img = Image.open(path_to_image)

img.show()

print("size: ", img.size)
print("width: ", img.size[0])
print("height: ", img.size[1])
print("size: ", img.size)


"""
resampling filters:
+ `PIL.Image.NEAREST`, use nearest neighbor (default)
+ `PIL.Image.BOX`, 
+ `PIL.Image.BILINEAR`, linear interpolation in a 2x2 environment
+ `PIL.Image.HAMMING`, 
+ `PIL.Image.BICUBIC`, cubic spline interpolation in a 4x4 environment
+ `PIL.Image.LANCZOS`

* resize, rotate & flip image vertically/horizontally
"""


new_width = 600
new_height = 400
new_size = (new_width, new_height)


"""
syntax: Image.resize(size, resample=0, box=None)
+ box to be used when providing rectangle to use
"""


resized_image = img.resize(new_size)
resized_filtered_image = img.resize(new_size, resample=Image.LANCZOS)
resized_box_image = img.resize(new_size, box=(100,100, 750, 550))

resized_filtered_image.show()
resized_box_image.show()

"""
syntax: Image.rotate(angle, resample=0, expand=0, center=None, translate=None, fillcolor=None)
+ 'expand', if True enlarges rotated image to hold entire image, False assume rotation around center w/o translation
+ 'center'. optional center of rotation as a Tuple
+ 'translate', optional post-rotate translation as a Tuple
+ 'fillcolor', optional post-rotate filling of color for empty area
"""


rotated_img_90 = img.rotate(90)
rotated_img_45 = img.rotate(45, expand=True)

rotated_img_90.show()
rotated_img_45.show()

"""
syntax: Image.transpose(method)
+ methods: Image.FLIP_LEFT_RIGHT, Image.FLIP_TOP_BOTTOM,
+          Image,ROTATE_90, Image.ROTATE_180, Image.ROTATE_270
+          Image.TRANSPOSE, Image.TRANSVERSE
"""


vertical_axis_flipped_img = img.transpose(Image.FLIP_LEFT_RIGHT)
horizontal_axis_flipped_img = img.transpose(Image.FLIP_TOP_BOTTOM)

vertical_axis_flipped_img.show()
horizontal_axis_flipped_img.show()


"""
* adjust image brightness, contrast & sharpness
+ brighten image with a factor of 1, giving original
+ contrast with 1 gives original, lesser gives grayer
+ sharpness with 1 gives original, lesser gives blur
"""

brightness_img = ImageEnhance.Brightness(img)
darken_img = brightness_img.enhance(0.5)
brighten_img = brightness_img.enhance(1.5)

contrast_img = ImageEnhance.Contrast(img)
locontrast_img = contrast_img.enhance(0.5)
hicontrast_img = contrast_img.enhance(1.5)

sharpness_img = ImageEnhance.Sharpness(img)
blurred_img = sharpness_img.enhance(0.5)
sharper_img = sharpness_img.enhance(1.5)

darken_img.show()
brighten_img.show()
locontrast_img.show()
hicontrast_img.show()
blurred_img.show()
sharper_img.show()


# closing all

img.close()
resized_filtered_image.close()
resized_box_image.close()
rotated_img_90.close()
rotated_img_45.close()
vertical_axis_flipped_img.close()
horizontal_axis_flipped_img.close()
darken_img.close()
brighten_img.close()
locontrast_img.close()
hicontrast_img.close()
blurred_img.close()
sharper_img.close()

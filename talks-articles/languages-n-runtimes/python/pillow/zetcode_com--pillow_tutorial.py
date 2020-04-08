#!/usr/bin/env python
# source: http://zetcode.com/python/pillow/
# source: https://www.pythonforbeginners.com/gui/how-to-use-pillow
#
# portions not covered in ./corey-schafer-image-manipulation-with-pillow.py

# Filters in Pillow: BLUR, CONTOUR, DETAIL, EDGE_ENHANCE,
# EDGE_ENHANCE_MORE, EMBOSS, FIND_EDGES, SMOOTH, SMOOTH_MORE, SHARPEN

from PIL import Image, ImageDraw, ImageFilter, ImageFont
import sys


source_bg_image = 'images/dream-cloud.jpg'
tmp_bg_image = '/tmp/dream-cloud.png'
font_freesans_path = '/usr/share/fonts/gnu-free/FreeSans.ttf'


def open_n_blur(img_path):
    try:
        img = Image.open(img_path)
        return img.filter(ImageFilter.BLUR)
    except Exception as e:
        print("failed to load image: %s" % (source_bg_image))
        sys.exit(1)


def image_info(img):
    print("Format: {0}\nSize: {1}\nMode: {2}".format(
        img.format, img.size, img.mode))


def draw_new_image():
    img = Image.new('RGBA', (800, 600), 'black')
    draw = ImageDraw.Draw(img)
    draw.rectangle((50, 50, 750, 550), fill='lightblue')
    del draw
    return img


def add_text(img, text, font_path):
    draw = ImageDraw.Draw(img)
    font = ImageFont.truetype(font_path, size=32)
    draw.text((25, 25), text, font=font)
    del draw
    return img


blurred = open_n_blur(source_bg_image)
image_info(blurred)
blurred.show()

new_img = draw_new_image()
new_img_with_txt = add_text(new_img, "Watermark or something...", font_freesans_path)
new_img_with_txt.save(tmp_bg_image)
contoured_img = new_img_with_txt.filter(ImageFilter.CONTOUR)
new_img_with_txt.show()
contoured_img.show()
new_img_with_txt.close()
contoured_img.close()
new_img.close()

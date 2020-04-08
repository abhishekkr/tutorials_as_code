#!/usr/bin/env python
# source: https://www.youtube.com/watch?v=6Qs3wObeWwc

from PIL import Image, ImageFilter


source_bg_image = 'images/dream-cloud.jpg'
tmp_bg_image = '/tmp/dream-cloud.png'
thumbnail_image = '/tmp/dream-cloud-thumbnail.png'
thumbnail_size = (800, 600)

# open image handler
jpg_dream_cloud = Image.open(source_bg_image)
# to save this PNG from JPEG
jpg_dream_cloud.save(tmp_bg_image)
# close this image handler
jpg_dream_cloud.close()


png_dream_cloud = Image.open(tmp_bg_image)

# resize image
png_dream_cloud.thumbnail(thumbnail_size)

# rotate 90degrees
rotate_dream_cloud = png_dream_cloud.rotate(90)

# convert mode, for BnW use 'L'
bnw_dream_cloud = rotate_dream_cloud.convert(mode='L')

# blur filter
blurred_dream_cloud = bnw_dream_cloud.filter(ImageFilter.GaussianBlur(3))

# cropping
cropped_dream_cloud = blurred_dream_cloud.crop((100, 100, 250, 250))

cropped_dream_cloud.save(thumbnail_image)

thumbnail = Image.open(thumbnail_image)
thumbnail.show()

#!python3
"""
This python script could be used as below to use Stable Diffusion for Text-to-Image

$ python3 stable-diffusion-simple.py "${IMAGE}.png" "${PROMPT_TEXT}"


But, install libraries below first..

$ pip install diffusers==0.10.0
$ pip install transformers scipy ftfy accelerate
$ pip install "ipywidgets>=7,<8"
"""

import torch
from diffusers import StableDiffusionPipeline
import sys


def sd_model():
    torch_device = "cuda" if torch.cuda.is_available() else "cpu"
    torch_dtype = torch.float32 if torch_device == "cpu" else torch.float16
    model = StableDiffusionPipeline.from_pretrained(
              "CompVis/stable-diffusion-v1-4",
              revision="fp16",
              torch_dtype=torch_dtype
            )
    return model.to(torch_device) #this box don't have cuda


def gen_img(model, prompt, imgname):
    images = model(prompt).images  # image here is in PIL format
    images[0].save(imgname)


if __name__ == "__main__":
    if len(sys.argv) < 3:
        print(f"usage: {sys.argv[0]} img-to-save-as.png 'prompt explaining what image to generate'")
        sys.exit(123)
    imgname = sys.argv[1]
    prompt = " ".join(sys.argv[2:])
    model = sd_model()
    gen_img(model, prompt, imgname)

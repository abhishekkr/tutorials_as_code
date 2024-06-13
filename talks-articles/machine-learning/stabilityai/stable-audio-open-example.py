"""
if you get , CUDA OOM.. try below env exports
PYTORCH_CUDA_ALLOC_CONF="max_split_size_mb:64"
CUDA_VISIBLE_DEVICES="2, 3"


Create Music With AI (Stability AI's Stable Audio Open Transformer Model, trained on public domain music samples)

Stable Audio: https://stability.ai/stable-audio

Model is available at HuggingFace for StabilityAI repo.
https://huggingface.co/stabilityai/stable-audio-open-1.0

It's been trained on public domain music, can generate audio.. sound effects & music sample upto 47sec at 44.1KHz.

It requires STABLE-AUDIO-TOOLS; Python Package's Github:
https://github.com/Stability-AI/stable-audio-tools

License allows NON-COMMERCIAL CREATIVE USAGE. Can't wrap and sell model as a product.

Text-To-Audio User Guide: https://www.stableaudio.com/user-guide/text-to-audio

Prompt Structure User Guide: https://www.stableaudio.com/user-guide/prompt-structure
"""

import torch
import torchaudio
from einops import rearrange
from stable_audio_tools import get_pretrained_model
from stable_audio_tools.inference.generation import generate_diffusion_cond
from stable_audio_tools.models.factory import create_model_from_config
from stable_audio_tools.models.utils import load_ckpt_state_dict

import os
import gc
import json


def load_local_model(model_path, model_config_path):
    model_config = None
    with open(model_config_path) as f:
        model_config = json.load(f)
    model = create_model_from_config(model_config)
    model.load_state_dict(
        load_ckpt_state_dict(model_path)
    )
    return model, model_config


def gen_audio(model, model_config, prompt, wavfile):
    sample_rate = model_config["sample_rate"]
    sample_size = model_config["sample_size"]

    # Set up text and timing conditioning
    conditioning = [{
        "prompt": prompt,
        "seconds_start": 0, 
        "seconds_total": 30
    }]

    output = generate_diffusion_cond(
        model,
        steps=100,
        cfg_scale=7,
        conditioning=conditioning,
        sample_size=sample_size,
        sigma_min=0.3,
        sigma_max=500,
        sampler_type="dpmpp-3m-sde",
        device=device
    )

    # Rearrange audio batch to a single sequence
    output = rearrange(output, "b d n -> d (b n)")
    # Peak normalize, clip, convert to int16, and save to file
    output = output.to(torch.float32).div(torch.max(torch.abs(output))).clamp(-1, 1).mul(32767).to(torch.int16).cpu()
    torchaudio.save(wavfile, output, sample_rate)


if __name__ == "__main__":
    gc.collect
    torch.cuda.empty_cache()
    model_path = "%s/model.safetensors" % (os.environ['MODEL_DIR'])
    model_cfg_path = "%s/model_config.json" % (os.environ['MODEL_DIR'])
    device = "cuda" if torch.cuda.is_available() else "cpu"

    model, model_config = load_local_model(model_path, model_cfg_path)
    model = model.to(device)

    prompt = "Format: Band | Genre: Bluegrass | Instruments: Acoustic Guitar, Piano | Moods: Cinematic, Happy"  # "128 BPM tech house drum loop"
    wavfile = "sample-bluegrass.wav"
    gen_audio(model, model_config, prompt, wavfile)

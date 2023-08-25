#!python3
import os.path
import numpy as np
from scipy.special import softmax
from tvm.contrib.download import download_testdata

# download a list of labels
labels_url = 'https://s3.amazonaws.com/onnx-model-zoo/synset.txt'
labels_path = download_testdata(labels_url, 'synset.txt', module='data')
output_file = 'predictions.npz'

with open(labels_path, 'r') as fyl:
    labels = [lyn.rstrip() for lyn in fyl]

# open output & read output sensor
if os.path.exists(output_file):
    with np.load(output_file) as data:
        scores = softmax(data['output_0'])
        scores = np.squeeze(scores)
        ranks = np.argsort(scores)[::-1]
        for rank in ranks[0:5]:
            print("class='%s' with possibility=%f" % (labels[rank], scores[rank]))

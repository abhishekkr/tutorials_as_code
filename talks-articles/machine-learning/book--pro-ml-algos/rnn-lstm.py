from keras.preprocessing.text import one_hot
from keras.preprocessing.sequence import pad_sequences
from keras.models import Sequential
from keras.layers import Dense, Flatten, SimpleRNN, Embedding, LSTM
import numpy as np
import pandas as pd
from collections import Counter


docs = ["very good", "very bad"]  # define documents
labels = [1, 0]  # define class labels

# one-hot-encode documents
counts = Counter()
for i, review in enumerate(docs):
    counts.update(review.split())
words = sorted(counts, key=counts.get, reverse=True)
vocab_size = len(words)
word_to_int = {word: i for i, word in enumerate(words, 1)}
encoded_docs = []
for doc in docs:
    encoded_docs.append([word_to_int[word] for word in doc.split()])
encoded_docs

# pad documents to max of 2 words
max_length = 2
padded_docs = pad_sequences(encoded_docs, maxlen=max_length, padding='pre')
print(padded_docs)

# build model
model = Sequential()
model.add(
    LSTM(1,
         activation='tanh',
         return_sequences=False,
         recurrent_initializer='Zeros',
         recurrent_activation='sigmoid',
         input_shape=(2, 1),
         unroll=True)
)
model.add(Dense(1, activation='sigmoid'))
model.compile(optimizer='adam', loss='binary_crossentropy', metrics=['acc'])
print(model.summary())

model.fit(padded_docs.reshape(2, 2, 1),
          np.array(labels).reshape(max_length, 1),
          epochs=500)
print("Layer0 Weights:", model.layers[0].get_weights())
print("Layer0 Trainable Weights:", model.layers[0].get_weights())

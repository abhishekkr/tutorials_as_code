from keras.preprocessing.sequence import pad_sequences
from keras.models import Sequential
from keras.layers import Dense
from keras.layers import SimpleRNN
import numpy as np


# ## Init doc & Encode words corresponding to doc
docs = ['very good', 'very bad']
# define class labels
labels = [1, 0]

from collections import Counter
counts = Counter()
for i, review in enumerate(docs):
    counts.update(review.split())
words = sorted(counts, key=counts.get, reverse=True)
vocab_size = len(words)
word_to_int = {word: i for i, word in enumerate(words, 1)}
encoded_docs = []
for doc in docs:
    encoded_docs.append([word_to_int[word] for word in doc.split()])

# pad documents to a max length of 2 words, to mantain same size for all doc
max_length = 2
padded_docs = pad_sequences(encoded_docs, maxlen=max_length, padding='pre')
print(padded_docs)


# ## Compiling Model
embed_length = 1
max_length = 2
model = Sequential()
model.add(
    SimpleRNN(1,
              activation='tanh',
              return_sequences=False,
              recurrent_initializer='Zeros',
              input_shape=(max_length, embed_length),
              unroll=True)
)
model.add(Dense(1, activation='sigmoid'))
# compile the model
model.compile(optimizer='adam', loss='binary_crossentropy', metrics=['acc'])
# summarize the model
print(model.summary())

model.fit(padded_docs.reshape(2, 2, 1),
          np.array(labels).reshape(max_length, 1),
          epochs=500)

print(model.layers)
print(model.get_weights())

print(
    padded_docs[0],
    ": ",
    model.predict(padded_docs[0].reshape(1, 2, 1))
)
print(
    padded_docs[1],
    ": ",
    model.predict(padded_docs[1].reshape(1, 2, 1))
)

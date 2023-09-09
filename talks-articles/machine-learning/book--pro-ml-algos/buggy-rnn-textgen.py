from keras.models import Sequential
from keras.layers import Dense,Activation
from keras.layers import SimpleRNN
import numpy as np
from urllib.request import urlretrieve
import re
from collections import Counter


# fetch data
url = ("https://www.gutenberg.org/cache/epub/11/pg11.txt")
filename = "/tmp/alice.txt"
urlretrieve(url, filename)


lines=[]
with open(filename, encoding='utf-8-sig') as fyl:
    for line in fyl:
        line = line.strip().lower()
        line = line.encode('utf-8-sig').decode("ascii", "ignore")
        if(len(line)==0):
            continue
        lines.append(line)
text = " ".join(lines)

print("\ndataset:", text[:100], "..")


# normalize text: to smallcase, remove punctuations
text = text.lower()
text = re.sub('[^0-9a-zA-Z]+', ' ', text)

# one-hot-encode the words
counts = Counter()
counts.update(text.split())
words = sorted(counts, key=counts.get, reverse=True)
chars = words
total_chars = len(set(chars))
nb_chars = len(text.split())
char2index = {word: i for i, word in enumerate(chars)}
index2char = {i: word for i, word in enumerate(chars)}

# create input & target dataset
SEQLEN = 10
STEP = 1
input_chars = []
label_chars = []
text2 = text.split()
for i in range(0, (nb_chars - SEQLEN), STEP):
    x = text2[i:(i+SEQLEN)]
    y = text2[i+SEQLEN]
    input_chars.append(x)
    label_chars.append(y)
print("====================================")
print("input char count:", len(input_chars), " | 0idx:", input_chars[0])
print("label char count:", len(label_chars), " | 0idx:", label_chars[0])

# encode input & output datasets
X = np.zeros((len(input_chars), SEQLEN, total_chars), dtype=bool)
y = np.zeros((len(input_chars), total_chars), dtype=bool)
for i, input_char in enumerate(input_chars):
    for j, ch in enumerate(input_char):
        X[i, j, char2index[ch]] = 1
    y[i, char2index[label_chars[i]]] = 1
print("X shape:", X.shape)
print("y shape:", y.shape)

# build model
HIDDEN_SIZE = 128
BATCH_SIZE = 128
NUM_ITERATIONS = 100
NUM_EPOCHS_PER_ITERATION = 1
NUM_PREDS_PER_EPOCH = 100
model = Sequential()
model.add(
    SimpleRNN(HIDDEN_SIZE,
              return_sequences=False,
              input_shape=(SEQLEN, total_chars),
              unroll=True)
)
model.add(Dense(nb_chars, activation='sigmoid'))
model.compile(optimizer='rmsprop', loss='categorical_crossentropy')
model.summary()

# running model: randomly generate seed text & try predict next word given the seed
for iteration in range(150):
    print("=" * 50)
    print("Iteration #: %d" % (iteration))
    # Fitting the values
    model.fit(X, y, batch_size=BATCH_SIZE, epochs=NUM_EPOCHS_PER_ITERATION)
    # Time to see how our predictions fare
    # We are creating a test set from a random location in our dataset
    # In the code below, we are selecting a random input as our seed value of words
    test_idx = np.random.randint(len(input_chars))
    test_chars = input_chars[test_idx]
    print("Generating from seed: %s" % (test_chars))
    print(test_chars)
    # From the seed words, we are tasked to predict the next words
    # In the code below, we are predicting the next 100 words (NUM_PREDS_PER_EPOCH) after the seed words
    for i in range(NUM_PREDS_PER_EPOCH):
        # Pre processing the input data, just like the way we did before training the model
        Xtest = np.zeros((1, SEQLEN, total_chars))
        for i, ch in enumerate(test_chars):
            Xtest[0, i, char2index[ch]] = 1
        # Predict the next word
        pred = model.predict(Xtest, verbose=0)[0]
        # Given that, the predictions are probability values,
        # we take the argmax to fetch the location of highest probability
        # Extract the word belonging to argmax
        ypred = index2char[np.argmax(pred)]
        print(ypred,end=' ')
        # move forward with test_chars + ypred so that we use the
        # original 9 words + prediction for the next prediction
        test_chars = test_chars[1:] + [ypred]

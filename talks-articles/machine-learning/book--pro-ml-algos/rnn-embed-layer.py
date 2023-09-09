from keras.layers import Dense, Activation, SimpleRNN, Embedding
from keras.models import Sequential
from keras.utils import to_categorical
from sklearn.model_selection import train_test_split
import numpy as np
import nltk
from nltk.corpus import stopwords
import re
import pandas as pd
from collections import Counter

nltk.download('stopwords')
eng_stopwords = stopwords.words('english')
ps = nltk.stem.PorterStemmer()

# Ready the dataset
filename = "sentiment-review.csv"
revu_col = 'text'
svc_snti_col = 'svc_sentiment'
snti_col = 'sentiment'
"""
Structure of datset would be like

   | svc_sentiment | text
 0 | positive      | @SvcName thanks for smooth refund
 1 | negative      | @SvcName's lack of printable bills is frustrating
 ...
"""
t = pd.read_csv(filename)
print(t.head())
t[snti_col] = np.where((t[svc_snti_col] == "positive"), 1, 0)


# Preparing text; common process of lowercase-ing & punctuation removal
def prepare_text(text):
    text = text.lower()
    text = re.sub('[^0-9a-zA-Z]+', ' ', text)
    words = text.split()
    word_s = [w for w in words if w not in eng_stopwords]
    wor_ds = [ps.stem(w) for w in word_s]
    if len(wor_ds) == 0:  # reviews with only emoji or such
        return 'NA'
    return ' '.join(wor_ds)

t[revu_col] = t[revu_col].apply(prepare_text)


# Convert each word to an index value
counts = Counter()
for i, review in enumerate(t[revu_col]):
    counts.update(review.split())
words = sorted(counts, key=counts.get, reverse=True)
print("first 10 words:", words[:10])
## chars = words
## nb_chars = len(words)
word_to_int = {word: idx for idx, word in enumerate(words, 1)}
int_to_word = {idx: word for idx, word in enumerate(words, 1)}

int_for_x = word_to_int['app']
word_for_int_for_x = int_to_word[int_for_x]
print("app =>", int_for_x, "|", int_for_x, "=>", word_for_int_for_x)


# Map each word in a review to its respective index
mapped_reviews = []
for revu in t[revu_col]:
    mapped_reviews.append(
        [word_to_int[word] for word in revu.split()]
    )
print("First 3 Texts:\n", t.loc[0:2][revu_col])
print("Mapped Reviews:\n", mapped_reviews[0:2])


# If all reviews have less than 200 words; choosing 200 Sequence Length for 0s
# and pad starting indices with 0s for empty lengths
SEQLEN = 200
sequences = np.zeros((len(mapped_reviews), SEQLEN), dtype=int)
for idx, row in enumerate(mapped_reviews):
    print(idx)
    revu_arr = np.array(row)
    sequences[idx, -len(row):] = revu_arr[-SEQLEN:]


# Split dataset to train & test
y = t[snti_col].values
X_train, X_test, y_train, y_test = train_test_split(sequences,
                                                    y,
                                                    test_size=0.3,
                                                    random_state=10)
y_train_cat = to_categorical(y_train)
y_test_cat = to_categorical(y_test)


# Create model. Embedding as Fn take total uniq word count,
# reduced dim & word count in input.
top_words = 12679
embedding_vec_length = 32
max_review_length = SEQLEN
model = Sequential()
model.add(
    Embedding(top_words, embedding_vec_length, input_length=max_review_length)
)
model.add(
    SimpleRNN(1, return_sequences=False, unroll=True)
)
model.add(
    Dense(2, activation='softmax')
)
model.compile(loss='categorical_crossentropy',
              optimizer='adam',
              metrics=['accuracy'])
print("Summary:\n", model.summary())
model.fit(X_train,
          y_train_cat,
          validation_data=(X_test, y_test_cat),
          epochs=50,
          batch_size=1024)

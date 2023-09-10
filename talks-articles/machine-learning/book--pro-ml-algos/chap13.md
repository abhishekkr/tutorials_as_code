
## Chapter.13 Recommender System

> Scenario like predicting a User's rating for an item using *Collaborative Filtering* and *Matrix Factorization*.

### Understanding k-nN

* `nearest Neighbor` is a data point closes to entity.

```
 {User,KgWeight} => (A,60), (B,62), (C,90)
 Have A & B similar when compared to C.

 Adding, Age var
 {User,KgWeight,Age} => (A,60,30), (B,62,35), (C,90,30)

 So, distance between A & B can be measured as sqrt((62-60)^2 + (35-30)^2)
```

* Scale of variables can cause issues, so normalizing variables is important. Ways to normalize are

> * Dividing variables with their max value, keeping all values between -1 to 1.
> * Find Z-score `((data point value - variable mean) / (standard deviation of var))` of each data point.
> * Divide each var by `(max - min)` value of var; doing `min max scaling`.

* `k` in `k-nearest neighbors` is count of nearest neighbors to consider while voting on its likability.


### Working Details of User-Based Collaborative Filtering (UBCF)

* UBCF is filtering out certain users due to their similarity. So likability for one user could be determined based on similar users action. Similarity can be seen in 2 common ways `Euclidean Distance` & `Cosine Similarity`.

#### Euclidean Distance

* Distance of each user from a User for each rating being calculated, then calculating overall distance for each user. Gaining user with least distance to the User.

> Major Issue is some taking extreme stance & other softer with same experience, while rating. So rating might be different with similar choices.

* Normalize would help for extreme stances. By taking average rating of a user across all reviews, then taking difference of each review to average as actual value.

* Considering a single similar user doesn't give good coverage. Choosing k most similar users with weights associated to how near they are. **Cosine Similarity** help with this.

#### Cosine Similarity

* Cosine Similarity between A&B vectors is defined as `sumOf(Ai * Bi) / ( sumOf(Ai^2) + sumOf(Bi^2) )`. Can now assign high similarity to users directinally correlated, not necessarily in magnitude.

> Should normalize values, then calculate Cosine Similarity for a user. Giving similarity value between -1 to 1.

* Weighted Avg Rating Calculation

> * Say A is most similar to B & C with `0.47 & 0.56` similarity respectively. For ProductX, B & C had normalized rating as `-0.5 & 0.17` respectively.
> * So Weighted Avg Rating would be `(0.47*-0.5 + 0.56*0.17) / (0.47+0.56) = -0.14`.
> Meaning average rating of A, could be predicted as `A's avg rating + (-0.14)`.

* Choosing Right Approach: No fixed way. 'Train, validate, test' to identify optimal k similar user, optimal common data points, correct weighted approach.

* Calculating Error: MSE of all predictions made on test dataset. MSE might not be suited to measure model's perf when running with business outcome. In that case, need a more business oriented metric as increase in items consumed by the user.

#### Issues with UBCF

* Every user need to be compared to every other user, suitable for small datasets (not if dealing with million+ users and/or million+ products). IBCF is more suitable in such cases.


### Item-Based Collaborative Filtering (IBCF)

* Uses hypothesis that two items are similar if they get similar ratings from same user.

* Data points considered are of Product not of users, so computation is still high just better than UBCF.


### Implementing Collaborative Filtering

```
import pandas as pd
import numpy as np

t = pd.read_csv('recommender_ratings_small.csv')

t2 = pd.pivot_table(t, values='rating', index='userId', columns='movieId')
# reset index
t3 = t2.reset_index()
t3 = t3.drop(['userId'], axis=1)

# Normalize Dataset
t4 = t3.subtract(np.mean(t3, axis=1), axis=0)

# Drop rows that don't have desired Movie Id
movie_id_var = 10
t5 = t4.loc[t4[movie_id_var].dropna(axis=0).index]
t6 = t5.reset_index()
t7 = t6.drop(['index'], axis=1)

# Calculate distance of every other user to UserA
x = []
for i in range(t7.shape[0]):
    x.append(np.mean(np.square(t4.loc[0]-t7.loc[i])))
t6.loc[np.argmin(x)][movie_id_var]

# Calculate predicted rating
print(
    np.mean(t3.loc[0]) * (1+(t6.loc[np.argmin(x)][movie_id_var]/np.mean(t3.loc[3])))
)
```


### Working Details of Matrix Factorization

* UBCF & IBCF are intuitive, though Matrix Factorization help discover latent interactions between users & items.

> * U users represented in K columns, with UxK matrix. And D items in K columns giving DxK matrix.
> Assigning random values to columns to begin with.
> * MatMul of UxK matix on transpose of Item matrix giving UxD matrix. U users may have rated some of items.
> * Calculating error using MSE, iterate to minimize Overall MSE.
> In UxK matrix Users with similar weights could be considered similar. Similarly in KxD matrix, items with minimal distance would be more similar.


### Implementing Matrix Factorization

> Python code blocks

* Imports

```
import pandas as pd
import keras.backend as K
from keras.layers import Input, Embedding, Dense, Dropout, merge, Flatten
from keras.models import Model
from keras import optimizers
```

* Read ratings `ratings = pd.read_csv('dataset_mat_fac.csv')`

* Extract unique users `users = ratings.User.unique()` & unique items `articles = ratings.Movies.unique()`.

* Index each user `userid2idx = {o:i for i,o in enumerate(users)}` & item `articlesid2idx = {o:i for i,o in enumerate(articles)}`.

* Apply index to original dataset for user `ratings.User = ratings.User.apply(lambda x: userid2idx[x])` and items `ratings.Movies = ratings.Movies.apply(lambda x: articlesid2idx[x])`.

* Extract unique count of users `n_users = ratings.User.nunique()` and items `n_articles = ratings.Movies.nunique()`.

* Define error metric

```
def rmse(y_true,y_pred):
    return K.sqrt(K.mean(K.pow(y_true - y_pred, 2)))
```

* Create initialization of P (UxK) & Q (KxD) matrix

```
def embedding_input(name,n_in,n_out):
    inp = Input(shape=(1,),dtype='int64',name=name)
    return inp, Embedding(n_in,n_out,input_length=1)(inp)

n_factors = 2
user_in, u = embedding_input('user_in', n_users, n_factors)
article_in, a = embedding_input('article_in', n_articles, n_factors)
# Initialize the dot product between user matrix and movie matrix
x = merge.dot([u,a],axes=2)
x=Flatten()(x)
```

* Build model

```
model = Model([user_in,article_in],x)
sgd = optimizers.SGD(lr=0.01)
model.compile(sgd,loss='mse',metrics=[rmse])
model.summary()
model.fit([ratings.User,ratings.Movies], ratings.Actual, nb_epoch=1000, batch_size=13)
```

---


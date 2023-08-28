#!python3

import mlflow
from sklearn.model_selection import train_test_split
from sklearn.datasets import load_diabetes

# If store script generated artifact with
# Run ID: 94ce1d2710fd4948a38439545173acdc
run_id = '94ce1d2710fd4948a38439545173acdc'

db = load_diabetes()
X_train, X_test, y_train, y_test = train_test_split(db.data, db.target)

model = mlflow.sklearn.load_model("runs:/{}/model".format(run_id))
predictions = model.predict(X_test)
print(predictions)

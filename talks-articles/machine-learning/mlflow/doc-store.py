#!python3

import mlflow
from mlflow.models import infer_signature
from sklearn.model_selection import train_test_split
from sklearn.datasets import load_diabetes
from sklearn.ensemble import RandomForestRegressor

with mlflow.start_run() as run:
    db = load_diabetes()
    X_train, X_test, y_train, y_test = train_test_split(db.data, db.target)

    rf = RandomForestRegressor(n_estimators=100, max_depth=6, max_features=3)
    rf.fit(X_train, y_train)

    predictions = rf.predict(X_test)
    print(predictions)

    signature = infer_signature(X_test, predictions)
    # for flavor: sklearn; log_model stores files in 'artifacts' dir
    # * built-in flavors: https://mlflow.org/docs/latest/models.html#models-built-in-model-flavors
    mlflow.sklearn.log_model(rf, 'model', signature=signature)

    print("Run ID: {}".format(run.info.run_id))

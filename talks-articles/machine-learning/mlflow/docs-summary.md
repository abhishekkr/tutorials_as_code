
## MLflow

> version 2.6.0

### What is MLflow?

* Extensible OSS to manage ML workflows & artifacts. Can write plugins to support for anything new.

* Has 5 components: Tracking, Models, Registry, Projects & Recipes

> * Tracking: API to log params, code versions, metrics, model env dep & model artifacts when running ML code. Has UI to review runs.
> * Models: Model packaging format & tools to deploy (from any ML lib) trained model on platforms as Docker, Spark, Databricks. Azure ML, AWS SageMaker.
> * Model Registry: Centrial store, APIs & UI for approval, QA & deploy.
> * Projects: Standard format for packaging reusable data science code, usable with different configs.
> * Recipes: Predefined templates to develop models for common tasks (like classification, regression).

### Quickstart: Install, Track, Store & Load

* Install via `pip install mlflow`. Also available in R & Java.

* Add MLflow tracking to code: for many ML libs (scikit-learn, keras, gluon, xgboost, lightgbm, statsmodels, spark, fastai, pytorch) calling `mlflow.autolog()` [auto logs](https://mlflow.org/docs/latest/tracking.html#automatic-logging) params, metrics & artifacts of run; [sample code for scikit-learn](./doc-autolog.py)

> * If lib doesn't support `autolog()`, can use key-val pairs `mlflow.{log_param, log_params, log_metric, log_artifacts, log_image, log_text}`.
> * Can disable autolog via `mlflow.autolog(disable=True)`.
> * Logging API: [details](https://mlflow.org/docs/latest/tracking.html#tracking-logging-functions)

* View MLflow runs & experiments: `mlflow ui` & `open http://localhost:5000` shows Default experiment with tracking data for your run. Local data/artifacts persist in `./mlruns/`, used to visualize.

* Share MLflow runs & experiments: With data stored remotely (SQLAlchemy compatible or Databricks workspace), by running a tracking http server using `mlflow server` & configuring in script via `mlflow.set_tracking_uri("http://MY_TRACK_IP:5000")`. Can also set env `MLFLOW_TRACKING_URI=http://MY_TRACK_IP:5000`. Track server has [options](https://mlflow.org/docs/latest/tracking.html#tracking-server).

> * If using Databricks, need to `databrick configure` for tokens & in python script side use `'databricks'` instead of uri value alongwith `mlflow.set_experiment(f"/Users/{user_name}/{experiment_name}")` to specify target. [Details](https://docs.databricks.com/mlflow/index.html).

* Store a model: autolog logs files cretaed or can do manually via `mlflow.{library_module_name}.log_model`. To load model soon, easy to stdout run's ID (by using `mlflow.ActiveRun` object by wrapping all logging code in `with mlflow.start_run()`). Code: [doc-store.py](./doc-store.py).

> MLflow model is a dir packaging
> * MLModel yaml file (spec of model's flavor, deps, signature, other metadata),
> * Serialized py object files (required by yaml file to init model),
> * Files to recreate runtime env,
> * & optionally and input example.

* Load a model for inference: Load & run using `mlflow.{library_module_name}.load_model` using store run's ID (can also be found in tracking). Code: [doc-load.py](./doc-load.py).

> To `load_model`, either recreate run env using files as `conda.yaml` in artifacts. Or `mlflow models serve` auto recreates env, also except `--env-manager` option. [Details](https://mlflow.org/docs/latest/quickstart_drilldown.html#quickstart-drilldown-log-and-load-model).


### Quickstart: Compare runs, choose model, deploy to REST API

* SetUp: The above Quickstart for env. Clone [mlflow](https://github.com/mlflow/mlflow) repo for sample. Run `mlflow server`.

* Run hyperparameter sweep: In repo path `mlflow/examples/hyperparam`, example tries to optimize RMSE metric of DeepLearning model on wine quality dataset. Has 2 hyperparameters to optimize: learning-rate & momentum.

```
export MLFLOW_TRACKING_URI=http://localhost:5000
mlflow run -e hyperopt .

## default uses pyenv with virtualenv; another option is conda
## to use present setup woth custom virtualenv, install deps from python_env.yaml
## --env-manager=local
## would also need to alter 'examples/hyperparam/search_hyperopt.py'
## call to 'mlflow.projects.run(...)' would need param 'env_manager="local"'
```

> * Uses *MLflow Projects format* defining multiple entry point, with `hyperopt` being one, in `MLproject` file.
> * Uses `Hyperopt` library to run hyperparameter sweep over `train` entry point. Sets different values to 2 parameters & records results in MLflow. Defaults to 12 runs of 32 epochs apiece.


* Compare results: UI would have a new nested list of runs. Can enable columns `Metrics.test_rmse, Parameters.{lr, momentum}`. Can select *Chart View* & configure *Parallel Coordinates* with same fields (here Red graphs poorly run).

* Register best model: Selecting best run & registering as model in `Table View | {best run} | Run Detail | Artifacts | Register Model`, enter name as `wine-quality`. Now available in `Models` page, can transition to different stages as `Staging`.

* Serve model locally: Can serve by any run/model version via `mlflow models serve -m "models:/wine-quality/Staging" --port 5002`. Could also use `runs:/..` uri.

> * Schema for request input/output can be found at UI in `Artifacts|Model`. Schema is available due to `infer_signature`.
> * To test the model, can `curl`. Response is `{"predictions": [{..},..]}`

```
curl -d '{"dataframe_split": {"columns": ["fixed acidity","volatile acidity","citric acid","residual sugar","chlorides","free sulfur dioxide","total sulfur dioxide","density","pH","sulphates","alcohol"], "data": [[7,0.27,0.36,20.7,0.045,45,170,1.001,3,0.45,8.8]]}}' -H 'Content-Type: application/json' -X POST http://localhost:5002/invocations
```

* Build container image for model: Build docker using `mlflow models build-docker --model-uri "models:/wine-quality/1" --name "qs_mlops"`. Here `1` in model-uri specify version number (can also use lifecycle stage as `/Staging`).

> * Once build can run as any `docker run -p 5002:8080 qs_mlops`,
> * Use it with `curl` above but to port `:8080`.

* Deploy to Cloud: All support docker images. Azure, Databricks, SageMaker & GCP have MLflow support.

---

* [Tutorials & Examples](https://mlflow.org/docs/latest/tutorials-and-examples/index.html#tutorials-and-examples)

---



## Machine Learning Operations

### Intro to MLOps

> [source: YT/Sokratis Kartakis](https://www.youtube.com/@sokratis.kartakis/videos)
>
> At the cross section of ML, Data Engineering & DevOps.

#### MLOps Maturity Model

* (Initial) Time to PoC: Standardize env & processes; access; track experiments

* (Repeatable) Model deployment time: Standardize SCM, automate model build & deployment, automate governed access, centralize management of models

* (Reliable) ML Defect Rate: Introduce auto testing, monitoring & lineage tracking. Standardize CI/CD & multi-account deployment.

* (Scalable) ML Lifecycle Time: ML Lifecycle reproducible via templates across teams. Standardize infra & team onboarding.

#### MLOps on AWS

* Self-service secure Infra deployment for ML use cases: AWS Sagemaker Projects, AWS Service Catalogs, IaC

* Auditability: Sagemaker Experiments, Model Registry & Model Monitor, Model Dashboard & Cards

* Increased Collboration among Data Scientists: Sagemaker Studio

* Enable sustanability: Sagemaker Processor & Pipelines

* Embedded QA & Automated Testing: Sagemaker Model Registry, CodePipeline & CodeBuild

* Data Preparation: Sagemaker (Spark) Processors

* Explainability & Bias Reporting: Sagemaker Clarify & Model Monitor

* Production-ready ML Workflows: Sagemaker Pipelines

#### MLOps KPI Metrics

* Time to Value (Inception to Production)

* Time to productionize existing ML use cases

* Percent of Template Driven Development

* Time to init new MLOps infra & ML Projects

* Execute ML solutions w/o internet access in Private Cloud

* Reduce Infra Costs

### ML Lifecycle: research to production

```
 [Data     ]---->[Data Curation, Quality]--->[Data prep, pipeline]-->
 [Ingestion]     [ & Cataloging         ]    [ & sharing         ]  |
 ,________________________________________________________________<-|
 |
 |           _______________________________________________________
 |         ,/                                                       \
 | ETL  [Data sampling]    [New feature]    [Model Build]    [Model Eval]
 |----->[& exploration]--->[engineering]--->[/Fine Tune ]--->[ & PoV    ]
 |
 |           __________________________________________________________
 | ML      ,/                                                          \
 '----->[Auto re-train]--->[Model version]--->[Model deployment]-->[Model  ]
        [at Scale     ]    [& auditing   ]    [& serve at scale]   [Monitor]
```

* Separation of Concern: Platform Administration, Data, {Experimentation, Model Build, Model Test, Model Deployment}, ML Governance

> * Experimentation: Notebooks
> * Model Pipeline: Pre-Migration Notebooks moved to a standard structure project; CI/CD to run-test-debug each execution. Standardize repo structure for ML build & deploy phases.
> * Standardize data storage & versioning based on ML Pipelines

### AWS Sagemaker Guides, Samples & Tools

> * [github: Amazon Sagemaker Examples](https://github.com/aws/amazon-sagemaker-examples)
> * [github: Amazon Sagemaker 101 Workshop](https://github.com/aws-samples/sagemaker-101-workshop)
> * [github: Amazon Sagemaker MLOps Workshop](https://github.com/aws-samples/amazon-sagemaker-mlops-workshop)
> * [github: Amazon Sagemaker Secure MLOps](https://github.com/aws-samples/amazon-sagemaker-secure-mlops)
> * [github: Amazon Sagemaker Build-Train-Deploy Sample](https://github.com/aws-samples/amazon-sagemaker-build-train-deploy)
> * [github: Amazon Sagemaker Notebook Instance Lifecycle Config Sample](https://github.com/aws-samples/amazon-sagemaker-notebook-instance-lifecycle-config-samples)
> * [github: Amazon Sagemaker Safe Deployment Pipeline Sample](https://github.com/aws-samples/amazon-sagemaker-safe-deployment-pipeline)
> * [github: Amazon Sagemaker MLFlow Fargate Sample](https://github.com/aws-samples/amazon-sagemaker-mlflow-fargate)
> * [github: Amazon Sagemaker GenerativeAI Sample](https://github.com/aws-samples/amazon-sagemaker-generativeai)
> * [github: Amazon Sagemaker Distributed Training Workshop](https://github.com/aws-samples/sagemaker-distributed-training-workshop)

* Amazon Sagemaker Experiments: track parameters & metrics across ML experiments

* Sagemaker Pipelines: Pre-process, Train, Evaluation & Register Models

* Sagemaker Model Registry: Store, Version & Trigger model promotion

* Sagemaker Projects: manager repo & CI/CD per project; organize ML Lifecycle under one namespace

* Sagemaker Model Monitor: Auto detection of data & model quality drifts

* Sagemaker Lineage Tracking: track workflow steps; model & data lineage; establish model governance & audit

* ML Governance - Model Cards: create single source of truth for model information; visibility with Sagemaker Model Dashboard

* Sagemaker RT Endpoints, BatchTransform, Shadow Testing for model testing & deployment

* Sagemaker Custom Projects Templates & AWS Service Catalog for better env init

* Sagemaker Data Wrangler, Sagemaker Feature Store, AWS LakeFormation, AWS Glue, AWS EMR for auto data flow

---


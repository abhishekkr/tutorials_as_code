
## Deploy docker to GCP with Kubernetes

### Target your gcloud to specific cluster and zone

```
alias gcloud="gcloud --project ${MY_GCP_PROJECT} --zone ${_GCP_ZONE}"
```

### Create a cluster

```
gcloud container clusters create scotch-cluster --disk-size=50 --num-nodes=10
```

### Upgrade version of existing cluster

```
gcloud container clusters upgrade scotch-cluster
```

### Setting default cluster

```
gcloud config set container/cluster ${_CLUSTER_ID}
```

### Connecting to cluster, Fetch credentials for a running cluster

```
gcloud container clusters get-credentials ${_CLUSTER_ID}
```

### Start proxy to K8s control plane, serve on 127.0.0.1:8001

```
kubectl proxy
```

### Goto dir with your Dockerfile for your service

```
_DOCKER_IMAGE_NAME="gcr.io/${MY_GCP_PROJECT}/${_APP_NAME}:${_APP_TAG}"
docker build  -t  ${_DOCKER_IMAGE_NAME} --file Dockerfile .
gcloud docker -- push ${_DOCKER_IMAGE_NAME}
```

### Goto dir with deployment.yml

```
kubectl create -f deployment.yml

curl -X POST -H 'Content-Type: application/yaml' --data '
apiVersion: apps/v1beta1
kind: Deployment
metadata:
  name: deployment-example
spec:
  replicas: 3
  revisionHistoryLimit: 10
  template:
    metadata:
      labels:
        app: nginx
    spec:
      containers:
      - name: nginx
        image: nginx:1.10
        ports:
        - containerPort: 80
' http://127.0.0.1:8001/apis/apps/v1beta1/namespaces/default/deployments
```

---
---

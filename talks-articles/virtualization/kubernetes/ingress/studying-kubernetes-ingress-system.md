
## Studying Kubernetes Ingress System

[source](https://www.joyfulbikeshedding.com/blog/2018-03-26-studying-the-kubernetes-ingress-system.html)

> setup an autoscaled nginx cluster that reverse proxies to pods in multiple deployments

* by default pods in Kubernetes are not supposed to be reachable from outside the cluster

* can be made reachable by associating with `service` of right type (NodePort or LoadBalancer); or defining an Ingress

* `ingress` by itself does nothing, need to deploy an `ingress controller`
> Google Cloud's Kubernetes Engine deploys `Google Cloud Controller`, by default responds to ingress resources and provisions a Google Cloud LB
> `kubernetes.io/ingress.class` annotation tells Google Cloud controller to ignore that ingress resource


#### Installing Nginx ingress controller

* create ingress

```
apiVersion: extensions/v1beta1
kind: Ingress
metadata:
  name: nginx-ingress
  annotations:
    nginx.ingress.kubernetes.io/rewrite-target: /
    # Avoid the Google Compute Engine controller from processing this Ingress.
    kubernetes.io/ingress.class: "nginx"
spec:
  rules:
  - http:
      paths:
      - path: /
        backend:
          serviceName: hello-minikube
          servicePort: 80
```

* create ingress controller, [how to for different setups](https://github.com/kubernetes/ingress-nginx/blob/master/deploy/README.md)

```
## for minikube
minikube addons enable ingress        ## will setup resources and open a port for it
minikube service nginx-ingress --url  ## helps get the usable url
```


#### The anatomy of an Nginx ingress controller installation

Can check [minikube-addons](https://github.com/kubernetes/minikube/blob/master/deploy/addons/ingress/) section.

RBAC get managed by a separate service-account, binded with a role at system-namespace and cluster level for multiple resource actions.

One deployment for nginx-ingress-controller and another for default backend which manages any traffic not managed by nginx-ingress-controller. Configmap and Service resource for these to be usable.

---

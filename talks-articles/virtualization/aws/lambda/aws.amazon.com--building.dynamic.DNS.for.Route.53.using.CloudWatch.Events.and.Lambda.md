
## Building a Dynamic DNS for Route 53 using CloudWatch Events and Lambda

[source](https://aws.amazon.com/blogs/compute/building-a-dynamic-dns-for-route-53-using-cloudwatch-events-and-lambda/)

> Besides creating `A records`, this solution allows you to create alias, i.e. `CNAME records`. Not treating as cattles though, but for when you need it.

With [Cloudwatch events](https://aws.amazon.com/cloudwatch), get near real-time information of AWS resource changing states.
Combining it with [Route 53](https://aws.amazon.com/route53) and [AWS Lambda](https://aws.amazon.com/lambda), you can mimic Dynamic DNS.
It persists these actions detail in [DynamoDB](https://aws.amazon.com/dynamodb), so it can be cleaned later.


#### Route53 Hosted Zones

Allows instances within your VPC to resolve the names of resources that run within your AWS environment.
A private hosted zone is basically a container holding information about how to route traffic for a domain and its subdomains within one or more VPCs.

Also lets clients on Internet resolve names of public-facing resources.
A public hosted zone is a container that holds information how to route traffic from Internet.


#### Choosing between VPC DNS or Route53 Private Hosted Zones

VPC DNS will provide name resolution for all the hosts within a VPC's CIDR range.
Unless you create a DHCP option set with a custom domain name and disable hostnames at VPC, you can't change domain suffix. All instances are either ec2.internal or _region_.compute.internal domain suffix. You can't create aliases or other resource record types.

Private hosted zones help you overcome these challenges.

---

### DDNS//Lambda Example

> pre-req
> * get [AWS CLI](http://docs.aws.amazon.com/cli/latest/userguide/cli-chap-getting-set-up.html) working
> * create a new VPC configured with a [private and public subnet](http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Scenario2.html)
> * VPC shall have DNS resolution and DNS hostnames set to 'yes'

* [CODE can be found at Github](https://github.com/awslabs/aws-lambda-ddns-function)

* More details and commands to be run at the `source` link above

---
---

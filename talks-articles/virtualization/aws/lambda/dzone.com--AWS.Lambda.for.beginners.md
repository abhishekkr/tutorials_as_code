
## AWS Lambda for Beginners

[source](https://dzone.com/articles/aws-lambda-for-beginners)

* [AWS Lambda](http://docs.aws.amazon.com/lambda/latest/dg/welcome.html) is a compute service from Amazon.

* Programming model for lambdas consist of **Handler, Context Object, Logging and Exceptions**. [here](http://docs.aws.amazon.com/lambda/latest/dg/programming-model-v2.html)

* Lamdas need to be stateless, persistence need be managed outside there lifecycle.

---

The most basic python AWS Lambda might would look like

```helloLambda.py
def lambda_handler(event, context):
  return "Hello Lambda!"
```

then create a file to authorize our lambda execution

```trust.json
{
  "Version": "2012-10-17",
    "Statement": [{
      "Sid": "",
      "Effect": "Allow",
      "Principal": {
        "Service": "lambda.amazonaws.com"
      },
      "Action": "sts:AssumeRole"
    }]
}
```

can deploy it using

```
#!/bin/bash

### Create the lambda package

zip -j helloLambda.zip *.py

### Create the role for the lambda to assume

role="helloLambda_exec_role"

trust="trust.json"

aws iam create-role --role-name $role --assume-role-policy-document file://$trust

aws iam update-assume-role-policy --role-name $role --policy-document file://$trust

### Create the lambda function

function_name="helloLambda"
handler_name="helloLambda.lambda_handler"
package_file=helloLambda.zip
runtime=python2.7

aws lambda create-function \
  --function-name $function_name \
  --handler $handler_name \
  --runtime $runtime \
  --memory 512 \
  --timeout 60 \
  --role arn:aws:iam::${AWS_ACCOUNT_ID}:role/$role \
  --zip-file fileb://$package_file
```


can invoke lambda using

```
aws lambda invoke --invocation-type RequestResponse --function-name helloLambda --payload '[""]' output.txt
```


clean-up after you

```
#!/bin/bash
role="helloLambda_exec_role"
function_name="helloLambda"
aws lambda delete-function --function-name $function_name
aws iam delete-role --role-name $role
```

---
---

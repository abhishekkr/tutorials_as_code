
## Quickstart Guide to FastAPI

```
https://fastapi.tiangolo.com/

  /|       =;==               __    __,    ,
 /.'-,     /  '        _/_    / \   /  )   |)
  | /   ,-/== ,_,  @   /     /--/  /--'    /
  |/   (_/   (_/|_/_)_<__   /  (_ /     ,_/ \_

```

> High-Performance Modern Web Framework for Python 3.8+

```
python3 -m venv .venv && source .venv/bin/activate

pip install fastapi
pip install "uvicorn[standard]"
```

* Quickstart

```sample.py
from fastapi import FastAPI
import os

app = FastAPI()


@app.get("/")
def read_root():
    return {"virtualenv": os.environ.get('VIRTUAL_ENV')}
```

* Run with

```
uvicorn sample:app
```

---

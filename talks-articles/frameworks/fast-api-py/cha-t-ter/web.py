#!uvicorn web:app

from fastapi import FastAPI
from lib.models import User
from lib.fakedb import FakeDB

app = FastAPI()


@app.get("/user/{name}")
def get_user(name: str):
    user_x = FakeDB.get_user_by_name(name)
    if user_x is None:
        return {"error": "User Not Found"}
    return {"value": user_x}


@app.put("/user")
def put_user(user: User):
    status = FakeDB.add_user(user)
    return {"status": status}


@app.get("/usernames")
def get_usernames():
    usernames = FakeDB.get_usernames()
    return {"usernames": usernames}


@app.get("/health")
def get_health():
    return {"status": "ok"}

#!uvicorn web:app

from fastapi import FastAPI, Depends
from sqlalchemy.orm import Session
from lib.models import User
from lib.dbmgr import DB

app = FastAPI()


@app.get("/user/{name}")
def get_user(name: str, db: Session = Depends(DB.get_db)):
    user_x = DB.get_user_by_name(db, name)
    if user_x is None:
        return {"error": f"Issues fetching user: {name}"}
    return {"user": user_x}


@app.put("/user")
def put_user(user: User, db: Session = Depends(DB.get_db)):
    user_id = DB.add_user(db, user)
    if user_id is None:
        return {"error": f"Issues adding user: {user.name}"}
    return {"user_id": user_id}


@app.get("/usernames")
def get_usernames(db: Session = Depends(DB.get_db)):
    usernames = DB.get_usernames(db)
    if usernames is None:
        return {"error": "Issues fetching usernames."}
    return {"usernames": usernames}


@app.get("/health")
def get_health():
    return {"status": "ok"}

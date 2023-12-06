#!uvicorn web:app

from fastapi import FastAPI, Depends, Response, status
from sqlalchemy.orm import Session
from lib.models import User
from lib.dbmgr import DB

app = FastAPI()


@app.get("/user/{name}", status_code=200)
def get_user(name: str, resp: Response, db: Session = Depends(DB.get_db)):
    user_x = DB.get_user_by_name(db, name)
    if user_x is None:
        resp.status_code = status.HTTP_500_INTERNAL_SERVER_ERROR
        return {"error": f"Issues fetching user: {name}"}
    return {"user": user_x}


@app.put("/user", status_code=200)
def put_user(user: User, resp: Response, db: Session = Depends(DB.get_db)):
    user_id = DB.add_user(db, user)
    if user_id is None:
        resp.status_code = status.HTTP_500_INTERNAL_SERVER_ERROR
        return {"error": f"Issues adding user: {user.name}"}
    return {"user_id": user_id}


@app.get("/usernames", status_code=200)
def get_usernames(resp: Response, db: Session = Depends(DB.get_db)):
    usernames = DB.get_usernames(db)
    if usernames is None:
        resp.status_code = status.HTTP_500_INTERNAL_SERVER_ERROR
        return {"error": "Issues fetching usernames."}
    return {"usernames": usernames}


@app.get("/health")
def get_health():
    return {"status": "ok"}

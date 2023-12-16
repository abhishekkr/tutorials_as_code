from fastapi import FastAPI, Depends
from fastapi import Response, status
from sqlalchemy.orm import Session
from .dbmgr import DB
from .models import User

api_user = FastAPI()


@api_user.get("/user/{name}", status_code=200)
def get_user(name: str, resp: Response, db: Session = Depends(DB.get_db)):
    user_x = DB.get_user_by_name(db, name)
    if user_x is None:
        resp.status_code = status.HTTP_500_INTERNAL_SERVER_ERROR
        return {"error": f"Issues fetching user: {name}"}
    return {"user": user_x}


@api_user.put("/user", status_code=200)
def put_user(user: User, resp: Response, db: Session = Depends(DB.get_db)):
    user_id = DB.add_user(db, user)
    if user_id is None:
        resp.status_code = status.HTTP_500_INTERNAL_SERVER_ERROR
        return {"error": f"Issues adding user: {user.name}"}
    return {"user_id": user_id}


@api_user.get("/user", status_code=200)
def get_user(resp: Response, name: bool = False, db: Session = Depends(DB.get_db)):
    users = None
    if name:
        users = DB.get_usernames(db)
    else:
        users = DB.get_users(db)
    if users is None:
        resp.status_code = status.HTTP_500_INTERNAL_SERVER_ERROR
        return {"error": "Issues fetching users."}
    return {"users": users}

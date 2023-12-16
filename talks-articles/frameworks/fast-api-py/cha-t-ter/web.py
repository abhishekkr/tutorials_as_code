#!uvicorn web:app

from fastapi import FastAPI
from lib.api_user import api_user
from lib.frontend import app_fe

app = FastAPI()


@app.get("/health")
def get_health():
    return {"status": "ok"}


app.mount("/user", api_user)
app.mount("/", app_fe)

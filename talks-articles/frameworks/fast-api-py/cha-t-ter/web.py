#!uvicorn web:app --reload

from fastapi import FastAPI, BackgroundTasks
from fastapi.staticfiles import StaticFiles
from fastapi import WebSocket
from starlette.responses import RedirectResponse
from starlette import status

from lib.fake_db import FakeDB
from lib.models import User

app = FastAPI()


@app.get("/user")
async def read_users():
    usernames = await FakeDB.get_users()
    return {"status": True, "users": usernames}


@app.put("/user")
async def update_user(user: User, background_tasks: BackgroundTasks):
    user_id = int(await FakeDB.new_users_id())
    user_x = User(id=user_id, name=user.name, subscribed=user.subscribed)
    background_tasks.add_task(FakeDB.add_user, user_x)
    return {"status": "queued user"}


@app.get("/user/{user_id}")
async def read_user(user_id: str, by_name: bool = False):
    user_x = None
    if by_name:
        if not isinstance(user_id, str):
            return {"status": "Failed. Try removing by_name=true get param."}
        user_x = await FakeDB.get_user_by_name(user_id)
    else:
        if not isinstance(user_id, int):
            return {"status": "Failed. Try using by_name=true get param."}
        user_x = await FakeDB.get_user(int(user_id))
    if user_x is None:
        return {"status": "User Not Found"}
    return user_x.model_dump(mode='json')


@app.get("/status")
def get_status():
    return {"status": "ok"}


@app.get("/")
async def get_root():
    return RedirectResponse(url='/index.html',
                            status_code=status.HTTP_302_FOUND)


@app.websocket("/chat")
async def ws_chat(websocket: WebSocket):
    await websocket.accept()
    try:
        while True:
            data = await websocket.receive_text()
            await websocket.send_text(f"Received: {data}")
    except Exception as e:
        print(e)
    await websocket.send_text("Adios!")
    await websocket.close()


app.mount("/", StaticFiles(directory="static"), name="static")

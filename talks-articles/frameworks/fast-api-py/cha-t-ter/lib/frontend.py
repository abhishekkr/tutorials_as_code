from fastapi import FastAPI, Request
from fastapi.staticfiles import StaticFiles
from fastapi.responses import HTMLResponse
from fastapi.templating import Jinja2Templates
from starlette import status
from starlette.responses import RedirectResponse


templates = Jinja2Templates(directory="templates")
app_fe = FastAPI(title="Chatter")


@app_fe.get("/", response_class=HTMLResponse)
async def get_root():
    return RedirectResponse(url='/index.html',
                            status_code=status.HTTP_302_FOUND)


@app_fe.get("/{fyl}.html", response_class=HTMLResponse)
async def get_html(request: Request, fyl: str):
    return templates.TemplateResponse(f"{fyl}.html", {"request": request})


app_fe.mount("/css/", StaticFiles(directory="static/css"), name="static/css")
app_fe.mount("/js/", StaticFiles(directory="static/js"), name="static/js")

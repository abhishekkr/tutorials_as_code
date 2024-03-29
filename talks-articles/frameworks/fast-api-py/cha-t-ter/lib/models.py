from pydantic import BaseModel
from typing import Union


class User(BaseModel):
    id: Union[int, None] = None
    name: str
    subscribed: Union[bool, None] = None

from pydantic import BaseModel
from typing import Union
"""
This is to manage Pydantic Models used for routes in Chatter.
"""


class User(BaseModel):
    id: Union[int, None] = None
    name: str
    subscribed: Union[bool, None] = None

import time
from .models import User
"""
We are adding time.sleep to several operations,
just to introduce negligible delay for single queries.

But to add up at Fake Integration Point, if heavy load is put through.

FakeDB here specifically provides dummy calls specific to usecase of Chatter.
"""


class FakeDB:
    app_users = []

    def add_user(new_user: User):
        retval = False
        time.sleep(0.05)
        all_user_ids = []
        all_usernames = []
        if len(FakeDB.app_users) > 0:
            all_user_ids = [u.id for u in FakeDB.app_users]
            all_usernames = [u.name for u in FakeDB.app_users]
        if new_user.id in all_user_ids:
            print(f"Failed Adding User: {new_user.name}. Try again.")
        elif new_user.name in all_usernames:
            print(f"Failed Adding User: {new_user.name}. Try another name.")
        else:
            FakeDB.app_users.extend([new_user])
            print(f"Added User: {new_user.name}")
            retval = True
        return retval

    async def get_users():
        time.sleep(0.10)
        usernames = [u.name for u in FakeDB.app_users]
        print(f"Usernames: {', '.join(usernames)}")
        return usernames

    async def get_user(id):
        time.sleep(0.005)
        user_x = None
        for u in FakeDB.app_users:
            if u.id == id:
                user_x = u
        if user_x is not None:
            print(f"Get User#{id}: {str(user_x)}")
        return user_x

    async def get_user_by_name(name):
        time.sleep(0.005)
        user_x = None
        for u in FakeDB.app_users:
            if u.name == name:
                user_x = u
        if user_x is not None:
            print(f"Get User#{user_x.id}: {str(user_x)}")
        return user_x

    async def new_users_id():
        return len(FakeDB.app_users) + 1

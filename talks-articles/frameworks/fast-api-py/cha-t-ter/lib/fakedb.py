from .models import User

class FakeDB:
    users = []

    def add_user(user: User):
        FakeDB.users.extend([user])
        return True

    def get_usernames():
        return [u.name for u in FakeDB.users]

    def get_user_by_name(uname):
        for u in FakeDB.users:
            if u.name == uname:
                return u
        return None

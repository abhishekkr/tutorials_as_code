from sqlalchemy import create_engine, select
from sqlalchemy.dialects.sqlite import *
from sqlalchemy.orm import sessionmaker, Session
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy import Column, Integer, String, Boolean

from .models import User
from .config import settings


SQLALCHEMY_DATABASE_URL = settings.sqlite_db_path
ENGINE = create_engine(SQLALCHEMY_DATABASE_URL,
                       connect_args={"check_same_thread": False})
SESSION = sessionmaker(autocommit=settings.db_autocommit,
                       autoflush=settings.db_autoflush,
                       bind=ENGINE)
BASE = declarative_base()


class Users(BASE):
   __tablename__ = 'users'
   id = Column(Integer, primary_key=True, autoincrement=True)
   name = Column(String(50), unique=True, nullable=False)
   email = Column(String(255), unique=True, nullable=False)
   subscribed = Column(Boolean)


class DB:
    def get_db():
        d = SESSION()
        try:
            yield d
        finally:
            d.close()

    def add_user(db: Session, user: User):
        try:
            u = Users(id=user.id, name=user.name, subscribed=user.subscribed)
            db.add(u)
            db.commit()
            db.refresh(u)
            return u.id
        except Exception as e:
            print(e)
            return None

    def get_users(db: Session):
        try:
            users = db.query(Users).all()
            return [{'name': u.name, 'id': u.id} for u in users]
        except Exception as e:
            print(e)
            return None

    def get_usernames(db: Session):
        try:
            users = db.query(Users).all()
            return [u.name for u in users]
        except Exception as e:
            print(e)
            return None

    def get_user_by_name(db: Session, name: str):
        try:
            stmt = select(Users).where(Users.name == name)
            return db.scalars(stmt).one()
        except Exception as e:
            print(e)
            return None

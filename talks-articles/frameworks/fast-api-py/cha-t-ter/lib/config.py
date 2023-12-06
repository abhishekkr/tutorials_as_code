from pydantic_settings import BaseSettings, SettingsConfigDict
import os


env_file = os.getenv("CHATTER_ENV_FILE")


class Settings(BaseSettings):
    sqlite_db_path: str = "sqlite:///./test.db"
    db_autocommit: bool = False
    db_autoflush: bool = False

    if env_file is not None:
        model_config = SettingsConfigDict(env_file=env_file)


settings = Settings()

import os, times


proc atTime*(): int = getTime().toUnix().int
proc toTime*(u: int): Time = u.fromUnix()


proc getConfig(envVar, defaultVal: string): string =
  result = getEnv(envVar)
  if result == "": result = defaultVal


proc cfgDbPath*(): string =
  result = getConfig("TWIT_SQLITE_PATH", "/tmp/twit.sqlite3")


proc cfgMigrationsPath*(): string =
  result = getConfig("TWIT_MIGRATIONS_PATH", "src" / "migrations")

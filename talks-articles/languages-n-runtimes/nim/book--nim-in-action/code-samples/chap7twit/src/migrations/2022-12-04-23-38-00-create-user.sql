CREATE TABLE IF NOT EXISTS user(
  id VARCHAR(36) PRIMARY KEY,
  username VARCHAR(36) NOT NULL,
  created_at INTEGER NOT NULL
);
CREATE INDEX idx_username
  ON message(username);

CREATE TABLE IF NOT EXISTS message(
  id VARCHAR(280) PRIMARY KEY,
  msg VARCHAR(280) NOT NULL,
  user_id VARCHAR(36) NOT NULL,
  created_at INTEGER NOT NULL,
  FOREIGN KEY (user_id) REFERENCES user(id)
);
CREATE INDEX idx_user_id
  ON message(user_id);

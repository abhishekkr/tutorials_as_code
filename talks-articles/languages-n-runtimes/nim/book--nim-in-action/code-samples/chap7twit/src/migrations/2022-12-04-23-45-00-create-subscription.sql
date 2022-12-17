CREATE TABLE IF NOT EXISTS subscription(
  user_id varchar(36) NOT NULL,
  subscribed_to varchar(36) NOT NULL,
  created_at INTEGER NOT NULL,
  PRIMARY KEY (user_id, subscribed_to),
  FOREIGN KEY (user_id) REFERENCES user(id),
  FOREIGN KEY (subscribed_to) REFERENCES user(id)
);
CREATE INDEX idx_user_id
  ON subscription(user_id);
CREATE INDEX idx_subscribed_to
  ON subscription(subscribed_to);

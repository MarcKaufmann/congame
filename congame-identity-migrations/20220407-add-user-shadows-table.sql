#lang north

-- @revision: 61636e72ecf7dea19fc4512c71f21136
-- @parent: 088b84e63536c0871c5973a53ee37cf7
-- @description: Adds the user_shadows table.
-- @up {
CREATE TABLE user_shadows(
  user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
  server_id INTEGER NOT NULL REFERENCES congame_servers(id) ON DELETE SET NULL,
  instance_id INTEGER NOT NULL,
  display_name TEXT UNIQUE NOT NULL,
  api_key TEXT UNIQUE NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,

  CONSTRAINT user_shadows_pk PRIMARY KEY (user_id, server_id, instance_id)
);
-- }

-- @up {
ALTER TABLE users
  DROP COLUMN display_name,
  DROP COLUMN api_key;
-- }

-- @down {
NOGOINGBACK;
-- }

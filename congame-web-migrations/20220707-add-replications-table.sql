#lang north

-- @revision: ec7a793b466e7f80c29424f473e591b0
-- @parent: fa16a3ecaddad12055735a1f806f3ffe
-- @description: Adds the replications table.
-- @up {
CREATE TABLE replications(
  id SERIAL PRIMARY KEY,
  slug TEXT NOT NULL UNIQUE,
  git_sha TEXT NOT NULL,
  instance_ids INTEGER[] NOT NULL,
  docker_container_port INTEGER NOT NULL UNIQUE,
  docker_container_id TEXT NOT NULL UNIQUE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP
);
-- }

-- @down {
DROP TABLE replications;
-- }

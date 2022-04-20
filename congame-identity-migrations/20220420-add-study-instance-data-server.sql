#lang north

-- @revision: 1da271c6c6f55ee5802f759191793855
-- @parent: 61636e72ecf7dea19fc4512c71f21136
-- @description: Adds the server_id column to study_instance_data.
-- @up {
TRUNCATE study_instance_data;
-- }
-- @up {
ALTER TABLE study_instance_data
  DROP COLUMN congame_url,
  ADD COLUMN server_id INTEGER NOT NULL REFERENCES congame_servers(id) ON DELETE CASCADE,
  DROP CONSTRAINT study_instance_data_pkey,
  ADD CONSTRAINT study_instance_data_pkey PRIMARY KEY (user_id, server_id, instance_id, study_stack, key);
-- }

-- @down {
NOGOINGBACK;
-- }

#lang north

-- @revision: 22b1560b64ab31aa35b56085ff1ad9ff
-- @parent: 372de2a1ce9524f635fdd62547a44e95
-- @description: Adds the study_instance_data table.
-- @up {
CREATE TABLE study_instance_data(
  user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
  instance_id INTEGER NOT NULL,
  study_stack TEXT[] NOT NULL,
  key TEXT NOT NULL,
  value JSONB NOT NULL,
  last_put_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
  first_put_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,

  CONSTRAINT study_instance_data_pkey PRIMARY KEY (user_id, instance_id, study_stack, key)
);
-- }

-- @down {
DROP TABLE study_instance_data;
-- }

#lang north

-- @revision: df9963a6446f05973c82f8ad13d710d4
-- @parent: 65d731fa82074e6046e6d1c1703d03ec
-- @description: Adds the csv_export_templates table.
-- @up {
CREATE TABLE csv_export_templates(
  id SERIAL PRIMARY KEY,
  study_instance_id INTEGER NOT NULL REFERENCES study_instances(id) ON DELETE CASCADE,
  name TEXT NOT NULL,
  fields JSONB NOT NULL DEFAULT '[]',
  created_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP
);
-- }

-- @down {
DROP TABLE csv_export_templates;
-- }

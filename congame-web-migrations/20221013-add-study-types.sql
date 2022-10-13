#lang north

-- @revision: 5fcee61162180b96881c46f7c00cda8d
-- @parent: fcac3927c8bf77800673bffab0f9ab9a
-- @description: Alters studies to add types for DSL studies.
-- @up {
ALTER TABLE studies
  ADD COLUMN type TEXT NOT NULL DEFAULT 'racket',
  ADD COLUMN dsl_source TEXT NOT NULL DEFAULT '',
  ADD CONSTRAINT studies_type_chk CHECK (type IN ('racket', 'dsl'));
-- }

-- @down {
ALTER TABLE studies
  DROP COLUMN type,
  DROP COLUMN dsl_source;
-- }

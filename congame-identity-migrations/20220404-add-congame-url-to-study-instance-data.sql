#lang north

-- @revision: 088b84e63536c0871c5973a53ee37cf7
-- @parent: 22b1560b64ab31aa35b56085ff1ad9ff
-- @description: adds the url of the congame server putting the study instance data
-- @up {
alter table study_instance_data add column congame_url TEXT not null default 'EMPTY';
-- }

-- @down {
alter table study_instance_data drop column congame_url;
-- }

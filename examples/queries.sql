--
-- Get all users from database
-- :get_all_users
SELECT *
FROM users

-- Just some description here
-- name: get_user_by_id
SELECT * FROM
users WHERE id = ?

-- another comment
-- name: get_all_schema_users
SELECT *
FROM :schema.users

--
-- name: accept_type_casts
select '[{"a":"foo"},{"b":"bar"},{"c":"baz"}]'::json->2


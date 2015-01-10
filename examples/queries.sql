-- Get all users from database
-- :get_all_users
SELECT *
FROM users

-- Just some description here
-- :get_user_by_id
SELECT *
FROM users
WHERE id = ?

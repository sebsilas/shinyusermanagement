SELECT username
FROM users
WHERE enabled = 'y'
  AND username = ?username
  AND password = ?password;

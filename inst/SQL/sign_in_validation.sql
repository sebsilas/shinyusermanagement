SELECT username
FROM user_management
WHERE is_enabled = 'y'
  AND username = ?username
  AND password = ?password;

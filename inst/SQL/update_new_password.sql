UPDATE users
SET password = ?password
WHERE username = ?username
  AND enabled = 'y';

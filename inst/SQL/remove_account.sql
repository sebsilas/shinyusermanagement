UPDATE users
SET enabled = NULL
WHERE username = ?username
  AND enabled = 'y';

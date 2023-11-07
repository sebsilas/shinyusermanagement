UPDATE users
SET color = ?color
WHERE username = ?username
  AND enabled = 'y';

UPDATE user_management
SET is_enabled = NULL
WHERE username = ?username
  AND is_enabled = 'y';

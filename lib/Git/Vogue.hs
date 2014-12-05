module Git.Vogue where

-- | Command string to insert into pre-commit hooks.
preCommitCommand :: String
preCommitCommand = "git-vogue check"

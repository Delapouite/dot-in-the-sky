function zg --description 'Zoxide scoped to current project .git/'
	set --local directory (command zoxide query --base-dir (git rev-parse --show-toplevel) $argv)
	and __zoxide_cd $directory
end

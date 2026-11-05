function zz --description 'Zoxide scoped to pwd'
	set --local directory (command zoxide query --base-dir (pwd) $argv)
	and __zoxide_cd $directory
end

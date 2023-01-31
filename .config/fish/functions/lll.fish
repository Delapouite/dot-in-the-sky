function lll --description 'verbose ls -la'
	exa --all --long --git --links --classify --octal-permissions $argv
end

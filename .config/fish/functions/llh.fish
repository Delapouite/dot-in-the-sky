function llh --description 'list all files verbosely'
	exa --long --git --links --classify --octal-permissions --all $argv
end

# see also ll, lh

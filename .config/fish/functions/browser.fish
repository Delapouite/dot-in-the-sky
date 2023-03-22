function browser --description 'browser: bookmarks, search-engines, tabs'
	set --function source "$argv[1]"
	set --function sources \
		bookmarks \
		search-engines \
		tabs

	if test -z "$source"
		set --function source (printf '%s\n' $sources | fzf --prompt 'browser ❯ ' --info inline --reverse --no-separator)
	end
	# pressing escape/enter key
	if test -z "$source"
		exit
	end

	switch $source

	case bookmarks
		fz browser-bookmarks

	case search-engines
		browser-search

	case tabs
		fz browser-tabs

	end
end

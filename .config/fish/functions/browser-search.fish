function browser-search --description 'browser search with various engines'
	set --function engine "$argv[1]"
	set --function query "$argv[1..-1]"

	set --function browser firefox-developer-edition
	set --function engines \
		arch \
		arch-wiki \
		bitbucket \
		bugzilla \
		duckduckgo \
		gitlab \
		github \
		google \
		hacker-news \
		linguee \
		lobsters \
		mdn \
		musicbrainz \
		npm \
		repology \
		stackoverflow \
		wikipedia-en \
		wikipedia-fr \
		wiktionary-en \
		wiktionary-fr \
		wordnik

	if test -z "$engine"
		set --function engine (printf '%s\n' $engines | fzf --prompt 'engine> ' --info inline --reverse --no-separator)
	end
	# pressing escape/enter key
	if test -z "$engine"
		exit
	end

	if test -z "$query"
		read --prompt-str "$engine> " query
	end
	# pressing enter key
	if test -z "$query"
		exit
	end

	switch $engine
	case arch
		$browser "https://archlinux.org/packages/?q=$query"

	case arch-wiki
		$browser "https://wiki.archlinux.org/index.php?search=$query"

	case bitbucket bb
		$browser "https://bitbucket.org/repo/all/?name=$query"

	case bugzilla
		$browser "https://bugzilla.mozilla.org/buglist.cgi?quicksearch=$query"

	case duckduckgo ddg
		$browser "https://duckduckgo.com/?q=$query"

	case github gh
		$browser "https://github.com/search?q=$query"

	case gitlab gl
		$browser "https://gitlab.com/search?search=$query"

	case google goo
		$browser "https://www.google.com/search?q=$query"

	case hacker-news hn
		$browser "https://hn.algolia.com/?q=$query"

	case linguee
		$browser "https://www.linguee.com/english-french/search?query=$query"

	case lobsters
		$browser "https://lobste.rs/search?q=$query"

	case mdn
		$browser "https://developer.mozilla.org/en-US/search?q=$query"

	case musicbrainz mb
		$browser "https://musicbrainz.org/search?type=artist&query=$query"

	case npm
		$browser "https://www.npmjs.com/search?q=$query"

	case repology repo
		$browser "https://repology.org/projects/?search=$query"

	case stackoverflow stack
		$browser "https://stackoverflow.com/search?q=$query"

	case wikipedia-en wiki
		$browser "https://en.wikipedia.org/wiki/Special:Search?search=$query"

	case wikipedia-fr wikifr
		$browser "https://fr.wikipedia.org/wiki/Special:Search?search=$query"

	case wiktionary-en dico
		$browser "https://en.wiktionary.org/w/index.php?search=$query"

	case wiktionary-fr dicofr
		$browser "https://fr.wiktionary.org/w/index.php?search=$query"

	case wordnik
		$browser "https://www.wordnik.com/words/$query"

	case '*'
		$browser "https://duckduckgo.com/?q=$query"

	end
end

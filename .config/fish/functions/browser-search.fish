function browser-search --description 'browser search with various engines'
	set --function engine "$argv[1]"
	set --function query "$argv[1..-1]"

	set --function browser xdg-open
	set --function engines \
		# default first
		duckduckgo \
		arch \
		arch-wiki \
		bitbucket \
		bugzilla \
		deepl \
		devdocs \
		gitlab \
		github \
		goodreads \
		google \
		hacker-news \
		hacker-news-url \
		jsr \
		larousse \
		linguee \
		lobsters \
		mdn \
		merriam-webster \
		musicbrainz \
		npm \
		oxford-english-dictionary \
		petit-robert \
		repology \
		stackoverflow \
		wikipedia-en \
		wikipedia-fr \
		wiktionary-en \
		wiktionary-fr \
		wordnik

	if test -z "$engine"
		set --function engine (printf '%s\n' $engines | fzf --prompt 'browser-search ❯ ' --info inline --reverse --no-separator)
	end
	# pressing escape/enter key
	if test -z "$engine"
		exit
	end

	if test -z "$query"
		set --function query (printf "" | fzf --prompt "$engine ❯ " --info hidden --reverse --no-separator --print-query)
	end
	# pressing escape/enter key
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

	case deepl
		$browser "https://www.deepl.com/translator#en/fr/$query"

	case devdocs
		$browser "https://devdocs.io/?q=$query"

	case duckduckgo ddg
		# https://duckduckgo.com/settings
		# https://duckduckgo.com/duckduckgo-help-pages/settings/params/
		# - kav infinite scroll
		# - kp safe search
		# - kaj metric system
		# - kak, kax install ddg
		# - kaq, kap newsletter
		# - kao privacy tips
		# - kau help improve
		# - km center
		# - kae theme
		$browser "https://duckduckgo.com/?kav=1&kp=-2&kaj=m&kak=-1&kax=-1&kaq=-1&kap=-1&kao=-1&kau=-1&km=m&kae=d&q=$query"

	case github gh
		$browser "https://github.com/search?q=$query"

	case gitlab gl
		$browser "https://gitlab.com/search?search=$query"

	case goodreads
		$browser "https://www.goodreads.com/search?utf8=%E2%9C%93&query=$query"

	case google goo
		$browser "https://www.google.com/search?q=$query"

	case hacker-news hn
		$browser "https://hn.algolia.com/?q=$query"

	case hacker-news-url hn
		$browser "https://hn.algolia.com/api/v1/search?restrictSearchableAttributes=url&query=$query"

	case jsr
		$browser "https://jsr.io/packages?search=$query"

	case larousse
		$browser "https://www.larousse.fr/dictionnaires/francais/$query"

	case linguee
		$browser "https://www.linguee.com/english-french/search?query=$query"

	case lobsters
		$browser "https://lobste.rs/search?q=$query"

	case mdn
		$browser "https://developer.mozilla.org/en-US/search?q=$query"

	case merriam-webster
		$browser "https://www.merriam-webster.com/dictionary/$query"

	case musicbrainz mb
		$browser "https://musicbrainz.org/search?type=artist&query=$query"

	case npm
		$browser "https://www.npmjs.com/search?q=$query"

	case repology repo
		$browser "https://repology.org/projects/?search=$query"

	case oxford-english-dictionary oed
		$browser "https://www.oed.com/search/dictionary/?scope=Entries&q=$query"

	case petit-robert
		$browser "https://dictionnaire.lerobert.com/definition/$query"

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
		$browser "https://duckduckgo.com/?kav=1&kp=-2&kaj=m&kak=-1&kax=-1&kaq=-1&kap=-1&kao=-1&kau=-1&km=m&kae=d&q=$query"

	end

	sleep 0.2
end


function lh --description 'list only hidden files'
	eza \
		--all \
		--classify \
		--color-scale all \
		--color-scale-mode fixed \
		--git \
		--links \
		--list-dirs \
		--long \
		--octal-permissions \
		.*
end

# see also ll and llh


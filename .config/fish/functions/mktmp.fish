function mktmp --argument template
	if test -n "$template"
		set template "$template.XXXXXX"
	else
		set template "tmp.XXXXXX"
	end
	cd (mktemp --directory --tmpdir "$template")
end

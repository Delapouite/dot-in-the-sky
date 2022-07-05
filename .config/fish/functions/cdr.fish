function cdr --description 'cd to git repo root'
	cd "$(git rev-parse --show-toplevel)"
end

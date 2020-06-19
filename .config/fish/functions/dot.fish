function dot --description 'interact with dot in the sky'
	set -x GIT_COMMITTER_NAME 'Delapouite'
	set -x GIT_COMMITTER_EMAIL 'delapouite@gmail.com'
	set -x GIT_AUTHOR_NAME 'Delapouite'
	set -x GIT_AUTHOR_EMAIL 'delapouite@gmail.com'

	/usr/bin/git --git-dir=$HOME/.dot-in-the-sky --work-tree=$HOME $argv
end

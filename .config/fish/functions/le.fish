function le --description 'print sorted and colorful environment variables'
	set --local hide 0

	if test -n "$argv[1]"
		set hide 1
	end

	set --local direnv 'DIRENV_DIFF|DIRENV_DIR|DIRENV_FILE|DIRENV_WATCHES'
	set --local kitty 'KITTY_INSTALLATION_DIR|KITTY_PID|KITTY_PUBLIC_KEY|KITTY_WINDOW_ID'
	set --local ssh 'SSH_AGENT_PID|SSH_AUTH_SOCK'
	set --local starship 'STARSHIP_SESSION_KEY|STARSHIP_SHELL'
	set --local systemd 'DBUS_SESSION_BUS_ADDRESS|INVOCATION_ID|SYSTEMD_EXEC_PID'
	set --local term 'COLORTERM|TERM|TERMINFO'
	set --local tool 'ASDF_DIR|FZF_DEFAULT_OPTS|I3SOCK'
	set --local wellknown 'EDITOR|HOME|LANG|LOGNAME|MAIL|MOTD_SHOWN|PATH|PWD|SHELL|SHLVL|USER|VISUAL'
	set --local xdg 'DESKTOP_STARTUP_ID|XDG_RUNTIME_DIR|XDG_SEAT|XDG_SESSION_CLASS|XDG_SESSION_ID|XDG_SESSION_TYPE|XDG_VTNR'
	set --local xorg 'DISPLAY|WINDOWID|WINDOWPATH|XAUTHORITY'
	set --local masked "$direnv|$kitty|$ssh|$starship|$systemd|$term|$tool|$wellknown|$xdg|$xorg"

	env -0 | sort -z | tr '\0' '\n' | awk "\
	match(\$0, /^($masked)=(.*)/, m) { \
		if ($hide == 0) { printf \"\x1b[38;2;98;114;164m%s\x1b[m \x1b[38;2;173;178;203m%s\x1b[m\n\", m[1], m[2]; } \
		next; \
	} \
	match(\$0, /([^=]*)=(.*)/, m) { \
		printf \"\x1b[36m%s\x1b[m %s\n\", m[1], m[2]; \
	}"
end


function dot-versions --description 'list commom tools versions'
	type -q bat && echo "bat $(bat --version | awk '{ print $2 }')" || begin; set_color red; echo 'bat'; set_color normal; end;
	type -q broot && broot --version || begin; set_color red; echo 'broot'; set_color normal; end;
	type -q caddy && echo "caddy $(caddy version)" || begin; set_color red; echo 'caddy'; set_color normal; end;
	type -q chars && chars --version || begin; set_color red; echo 'chars'; set_color normal; end;
	type -q dog && echo "dog $(dog --version | head -n 2 | tail -n 1)" || begin; set_color red; echo 'dog'; set_color normal; end;
	type -q entr && echo "entr $(entr 2>&1 | head -n 1 | awk '{ print $2 }')" || begin; set_color red; echo 'entr'; set_color normal; end;
	type -q exa && echo "exa $(exa --version | head -n 2 | tail -n 1 | awk '{ print $1 }')" || begin; set_color red; echo 'exa'; set_color normal; end;
	type -q fd && fd --version || begin; set_color red; echo 'fd'; set_color normal; end;
	type -q fish && echo "fish $(fish --version | awk '{ print $3 }')" || begin; set_color red; echo 'fish'; set_color normal; end;
	type -q flameshot && echo "flameshot $(flameshot --version | head -n 1 | awk '{ print $2 }')" || begin; set_color red; echo 'flameshot'; set_color normal; end;
	type -q fzf && echo "fzf $(fzf --version)" || begin; set_color red; echo 'fzf'; set_color normal; end;
	type -q git && echo "git $(git --version | awk '{ print $3 }')" || begin; set_color red; echo 'git'; set_color normal; end;
	type -q gitui && gitui --version || begin; set_color red; echo 'gitui'; set_color normal; end;
	type -q grex && grex --version || begin; set_color red; echo 'grex'; set_color normal; end;
	type -q htop && echo "htop $(htop --version | head -n 1 | awk '{ print $2 }')" || begin; set_color red; echo 'htop'; set_color normal; end;
	type -q hyperfine && hyperfine --version || begin; set_color red; echo 'hyperfine'; set_color normal; end;
	type -q i3 && echo "i3 $(i3 --version | awk '{ print $3 }')" || begin; set_color red; echo 'i3'; set_color normal; end;
	type -q jo && jo -v || begin; set_color red; echo 'jo'; set_color normal; end;
	type -q jq && jq --version | tr '-' ' ' || begin; set_color red; echo 'jq'; set_color normal; end;
	type -q jwt && jwt --version | head -n 1 || begin; set_color red; echo 'jwt'; set_color normal; end;
	type -q kak && kak -version | tr '[:upper:]' '[:lower:]' || begin; set_color red; echo 'kak'; set_color normal; end;
	type -q kitty && kitty --version | awk '{ print $1" "$2 }' || begin; set_color red; echo 'kitty'; set_color normal; end;
	type -q lazydocker && echo "lazydocker $(lazydocker --version | head -n 1 | awk '{ print $2 }')" || begin; set_color red; echo 'lazydocker'; set_color normal; end;
	type -q lf && echo "lf $(lf --version)" || begin; set_color red; echo 'lf'; set_color normal; end;
	type -q meld && meld --version || begin; set_color red; echo 'meld'; set_color normal; end;
	type -q mons && mons -v | head -n 1 | tr '[:upper:]' '[:lower:]' || begin; set_color red; echo 'mons'; set_color normal; end;
	type -q mpd && echo "mpd $(mpd --version | head -n 1 | awk '{ print $4 }')" || begin; set_color red; echo 'mpd'; set_color normal; end;
	type -q mpv && mpv --version | head -n 1 | awk '{ print $1" "$2 }' || begin; set_color red; echo 'mpv'; set_color normal; end;
	type -q ncmpcpp && ncmpcpp --version | head -n 1 || begin; set_color red; echo 'ncmpcpp'; set_color normal; end;
	type -q nvim && nvim --version | head -n 1 | tr '[:upper:]' '[:lower:]' || begin; set_color red; echo 'nvim'; set_color normal; end;
	type -q nu && echo "nu $(nu --version)" || begin; set_color red; echo 'nu'; set_color normal; end;
	type -q pastel && pastel --version || begin; set_color red; echo 'pastel'; set_color normal; end;
	type -q rg && rg --version | head -n 1 || begin; set_color red; echo 'rg'; set_color normal; end;
	type -q rofi && echo "rofi $(rofi -v | awk '{ print $2 }')" || begin; set_color red; echo 'rofi'; set_color normal; end;
	type -q rofimoji && rofimoji --version || begin; set_color red; echo 'rofimoji'; set_color normal; end;
	type -q sad && sad --version || begin; set_color red; echo 'sad'; set_color normal; end;
	type -q semgrep && echo "semgrep $(semgrep --version)" || begin; set_color red; echo 'semgrep'; set_color normal; end;
	type -q starship && starship --version | head -n 1 || begin; set_color red; echo 'starship'; set_color normal; end;
	type -q sxiv && sxiv -v || begin; set_color red; echo 'sxiv'; set_color normal; end;
	type -q tldr && tldr --version || begin; set_color red; echo 'tldr'; set_color normal; end;
	type -q tig && tig --version | head -n 1 | awk '{ print $1" "$3 }' || begin; set_color red; echo 'tig'; set_color normal; end;
	type -q tokei && tokei --version | awk '{ print $1" "$2}' || begin; set_color red; echo 'tokei'; set_color normal; end;
	type -q units && units --version | head -n 1 || begin; set_color red; echo 'units'; set_color normal; end;
	type -q xsv && echo "xsv $(xsv --version)" || begin; set_color red; echo 'xsv'; set_color normal; end;
	type -q zola && zola --version || begin; set_color red; echo 'zola'; set_color normal; end;
	type -q websocketd && websocketd --version | awk '{ print $1" "$2 }' || begin; set_color red; echo 'websocketd'; set_color normal; end;
end

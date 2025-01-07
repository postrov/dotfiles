set -g -x fish_greeting ''

set -gx PATH $PATH ~/bin

alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'
alias lg=lazygit
alias hx=helix
alias t=todo.sh

begin
    set --local AUTOJUMP_PATH $HOME/.autojump/share/autojump/autojump.fish
    if test -e $AUTOJUMP_PATH
        source $AUTOJUMP_PATH
    end
end
if [ -f $HOME/.config/fish/alias.fish ]
    source $HOME/.config/fish/alias.fish
end
starship init fish |source
zoxide init fish | source
mise activate fish | source

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/home/pasza/git-installs/google-cloud-sdk/path.fish.inc' ]; . '/home/pasza/git-installs/google-cloud-sdk/path.fish.inc'; end

# Created by `pipx` on 2024-05-14 12:24:27
set PATH $PATH /home/pasza/.local/bin

function yy
	set tmp (mktemp -t "yazi-cwd.XXXXXX")
	yazi $argv --cwd-file="$tmp"
	if set cwd (cat -- "$tmp"); and [ -n "$cwd" ]; and [ "$cwd" != "$PWD" ]
		cd -- "$cwd"
	end
	rm -f -- "$tmp"
end


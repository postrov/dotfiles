function fzv -a path
    if [ -n "$path" -a -d "$path" ]
        fd --type f --hidden --exclude .git --print0 . "$path"| fzf-tmux -p --read0 --print0 --exit-0 | xargs -r -0 nvim
    else
        fd --type f --hidden --exclude .git --print0 | fzf-tmux -p --read0 --print0 --exit-0 | xargs -r -0 nvim
    end
end

function fzc -a path
    if [ -n "$path" -a -d "$path" ]
        set __TMP_CD (fd --type d --hidden --exclude .git --print0 . "$path"| fzf-tmux -p --read0 --print0 --exit-0)
    else
        set __TMP_CD (fd --type d --hidden --exclude .git --print0 | fzf-tmux -p --read0 --print0 --exit-0)
    end
    if [ -n "$__TMP_CD" ]
        cd "$__TMP_CD"
    end
end

# Dotfiles

A collection of my dotfiles, based on [this solution](https://www.ackama.com/blog/posts/the-best-way-to-store-your-dotfiles-a-bare-git-repository-explained)

# Initial setup on first machine:
1. ```git init --bare $HOME/.cfg```
2. ```alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'```
3. ```echo "alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'" >> $HOME/.config/fish/config.fish```
4. ```config config --local status.showUntrackedFiles no```
5. ```config add .vimrc + config commit -m "add .vimrc" + set up a remote repository on GitHub or your Git server of choice + config push ```

# Restoring on new machine:

1. ```echo ".cfg" >> .gitignore```
2. ```git clone --bare <remote-git-repo-url> $HOME/.cfg```
3. ```alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'```
4. ```config config --local status.showUntrackedFiles no```
5. ```config checkout```

# After restoring
1. Install oh my fish ```curl -L https://get.oh-my.fish | fish```
2. Install tmux plugin manager: ```git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm```



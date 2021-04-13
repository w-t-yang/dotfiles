# dotfiles

#### Clone to home dir

```
git clone git@github.com:winterock/dotfiles.git ./
```

#### Ignore untracked files
```
git config --local status.showUntrackedFiles no
```

#### Install Purcell's emacs config
```
git clone https://github.com/purcell/emacs.d.git ~/.emacs.d
```

#### Install emacs
```
brew install --cask emacs
brew reinstall --cask emacs --no-quarantine
```

#### Using vmd to preview markdown files
```
npm install -g vmd
```

#### Dependencies
- https://github.com/arcticicestudio/nord-dircolors.git
- https://github.com/arcticicestudio/nord-tmux.git
- https://github.com/arcticicestudio/nord-terminal-app.git
- https://github.com/adobe-fonts/source-code-pro.git

```
brew install coreutils
curl -LO https://github.com/adobe-fonts/source-code-pro/archive/release.zip
unzip release.zip
cp -a source-code-pro-release/TTF/* ~/Library/Fonts
```

#### Miscennanious
- ~/.emacs.d/custom.el
- Map 'Capslock' to 'Control'
- For terminal.app, go to 'Preference->Profiles->Keyboard', check "Using Option as Meta"
- Install Dash, then integrate with Alfred and Emacs

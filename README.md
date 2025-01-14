# dotfiles

## Brew
### Installation
```
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

### Trouble Shooting
When experiencing network issues
- Download the install script, and run it manually

When fail to run install.sh
- open the script and find `ohai "Downloading and installing Homebrew..."`
- find out where it fails
- comment out the lines that has been executed
- then re-run the script


## Apply Dotfiles in This Repo
Clone to home dir
```
git clone git@github.com:winterock/dotfiles.git
```

Move dotfiles to home folder
```
mv dotfiles ~/.wy
```

Copy emacs and zsh example config to home folder
```
cp ~/.wy/examples/.emacs ~/.emacs
cp ~/.wy/examples/.zshrc ~/.zshrc
```


## Emacs

### Install with brew
```
brew install --cask emacs

# When having troubles and need to reinstall, try
brew reinstall --cask emacs --no-quarantine
```

### Install with binary [Preferred]
https://emacsformacosx.com/


### Purcell's emacs config
```
git clone https://github.com/purcell/emacs.d.git ~/.emacs.d
```

### Trouble shooting
When init emacs for the first time, it may take a long time to install all the
packages. And some may timeout or run into other issues. Restart emacs multiple
times to make sure everything is properly installed.

### Tips
- Delete `~/.emacs.d/custom.el` to re-init customization
- Use `~/.emacs.d/eshell/alias` to set alias in eshel
- [ALWAYS] Map 'Capslock' to 'Control'

## Programming Language Env Setup
### Python
https://github.com/pyenv/pyenv
```
brew install pyenv
# restart shell
```

### Ruby
https://github.com/rbenv/rbenv
```
brew install rbenv ruby-build
# restart shell
```

### Node
```
brew install node
# or brew upgrade node
```

pnpm - https://github.com/pnpm/pnpm
```
brew install pnpm
# add alias to .zshrc -> alias npm="pnpm"
```

#### Mermaid

Install mermaid https://github.com/mermaid-js/mermaid
```
pnpm install -g mermaid
```

Install mermaid-cli https://github.com/mermaid-js/mermaid-cli
```
pnpm install -g @mermaid-js/mermaid-cli
```

Integrate Mermaid with Org-mode https://github.com/arnm/ob-mermaid

## Misc
### Terminal
Preference->Profiles->Keyboard > check "Using Option as Meta"

Nord terminal App
- https://github.com/arcticicestudio/nord-terminal-app.git

Nord dircolors
- https://github.com/arcticicestudio/nord-dircolors.git

Nord tmux
- https://github.com/arcticicestudio/nord-tmux.git

### CoreUtils
```
brew install coreutils
```

### Font
Source Code Pro https://github.com/adobe-fonts/source-code-pro.git
```
curl -LO https://github.com/adobe-fonts/source-code-pro/archive/release.zip
unzip release.zip
cp -a source-code-pro-release/TTF/* ~/Library/Fonts
```

### Themes
Tomorrow Theme
- https://github.com/chriskempson/tomorrow-theme

Solarized Theme
- https://github.com/altercation/solarized


# dotfiles

### Brew
##### Installation
```
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```
##### Trouble Shooting
When experiencing network issues
- Download the install script, and run it manually

When fail to run install.sh
- Open the script and find `ohai "Downloading and installing Homebrew..."`
- Unstand where it fails, and run it manually
- Comment out the lines that has been executed, then re-run the script

### Terminal

Preference->Profiles->Keyboard > check "Using Option as Meta"

##### Dependencies
```
brew install coreutils
```

Source Code Pro https://github.com/adobe-fonts/source-code-pro.git
```
curl -LO https://github.com/adobe-fonts/source-code-pro/archive/release.zip
unzip release.zip
cp -a source-code-pro-release/TTF/* ~/Library/Fonts
```

Nord terminal App
- https://github.com/arcticicestudio/nord-terminal-app.git

Nord dircolors
- https://github.com/arcticicestudio/nord-dircolors.git

Nord tmux
- https://github.com/arcticicestudio/nord-tmux.git

Tomorrow Theme
- https://github.com/chriskempson/tomorrow-theme

##### DOTFILES

Clone to home dir
```
git clone git@github.com:winterock/dotfiles.git
mv dotfiles/.* ~/
```

Ignore untracked files
```
git config --local status.showUntrackedFiles no
```

### Emacs

##### Install with brew
```
brew install --cask emacs

# When having troubles and need to reinstall, try
brew reinstall --cask emacs --no-quarantine
```

##### Install with binary
https://emacsformacosx.com/


##### Purcell's emacs config
```
git clone https://github.com/purcell/emacs.d.git ~/.emacs.d
```

##### Trouble shooting
When init emacs for the first time, it may take a long time
to install all the packages. And some may timeout or run into other issues.

Restart emacs multiple times to make sure everything is properly installed.

##### [Optional]Using vmd to preview markdown files
```
npm install -g vmd
```

##### Miscennanious
- ~/.emacs.d/custom.el
- ~/.emacs.d/eshell/alias
- Map 'Capslock' to 'Control'
- [Optional]Install Dash, then integrate with Alfred and Emacs

# Install Step

## 1. Install emacs

##### Using emacs-plus

```
brew tap d12frosted/emacs-plus
# to install Emacs 26
brew install emacs-plus
# or to install Emacs 27
brew install emacs-plus@27 --with-spacemacs-icon
# or to install Emacs 28
brew install emacs-plus@28 --with-spacemacs-icon
brew link emacs-plus
```

##### Using emacs-mac

```
brew tap railwaycat/emacsmacport
brew install emacs-mac
brew link emacs-mac
```

##### Using cask

Homebrew now recommends to use the cask version with the following message:
"Please try the Cask for a better-supported Cocoa version". To install the cask
version:

```
brew cask install emacs
```

## 2. Install base spacemacs config

This assumes you don't have an existing Emacs setup and want to run Spacemacs as
your config. If you do have one, look at
the [full installation instructions](#install) for other options.

* For stable releases:
  ```shell
  git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
  ```

* For development updates and participation:
  ```shell
  git clone -b develop https://github.com/syl20bnr/spacemacs ~/.emacs.d
  ```

## 3. Install customized spacemacs config

1. remove spacemacs default .spacemacs
   ```shell
   rm ~/.spacemacs
   ```

2. add customized .spacemacs.d folder
   ```shell
   git clone git@github.com:adispring/spacemacsDotD.git ~/.spacemacs.d
   ```

## 2. Using Emacs

1. kill emacs
   ```shell
   emacsclient -e "(kill-emacs)"
   ```

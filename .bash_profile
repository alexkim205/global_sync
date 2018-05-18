
# Setting PATH for Python 3.6
PYTHONHOME=/usr/local/Cellar/python3/3.6.5/Frameworks/Python.framework/Versions/3.6
PYTHONPATH=$PYTHONHOME/bin
export PYTHONHOME
export PYTHONPATH

# iterm2 shell integration
source ~/.iterm2/.iterm2_shell_integration.bash

# automatically git sync emacs configurations

## git pull changes when terminal starts
git pull -q
echo "Bash settings up to date!"

## git add changes when terminal exits
gitadd() {
    git add -f .bash_profile .bashrc .emacs .tmux.conf .emacs.d/ .ssh/ .tmux/
    git commit -m "terminal startup sync"
    git push    
}

trap gitadd EXIT

if [ -f ~/.bashrc ]; then
    source ~/.bashrc
fi

test -e "${HOME}/.iterm2_shell_integration.bash" && source "${HOME}/.iterm2_shell_integration.bash"

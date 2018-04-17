
# Setting PATH for Python 3.6
# The original version is saved in .bash_profile.pysave
PATH="/System/Library/Frameworks/Python.framework/Versions/2.7/bin:${PATH}"
export PATH

export PYTHONPATH=$PYTHONPATH:/usr/local/lib/python2.7/site-packages

# iterm2 shell integration
source ~/.iterm2/.iterm2_shell_integration.bash

# automatically git sync emacs configurations

## git pull changes when terminal starts
git pull -q
echo "Bash settings up to date!"

## git add changes when terminal exits
gitadd() {
    git add -f .bash_profile .bashrc .emacs .tmux.conf .emacs.d/ .ssh/ .tmux/ .iterm2/ ~/Library/Preferences/com.googlecode.iterm2.plist
    git commit -m "terminal startup sync"
    git push    
}

trap gitadd EXIT

if [ -f ~/.bashrc ]; then
    source ~/.bashrc
fi

test -e "${HOME}/.iterm2_shell_integration.bash" && source "${HOME}/.iterm2_shell_integration.bash"

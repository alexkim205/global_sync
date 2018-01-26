
# Setting PATH for Python 3.6
# The original version is saved in .bash_profile.pysave
PATH="/Library/Frameworks/Python.framework/Versions/3.6/bin:${PATH}"
export PATH
PATH="/System/Library/Frameworks/Python.framework/Versions/2.7/bin/python"
export PATH

# automatically git sync emacs configurations

## git pull changes when terminal starts
git pull -q
echo "Bash settings up to date!"

## git add changes when terminal exits
gitadd() {
    git add .bash_profile .bashrc .emacs .emacs.d/ .ssh/
    git commit -m "terminal startup sync"
    git push    
}

trap gitadd EXIT

if [ -f ~/.bashrc ]; then
    source ~/.bashrc
fi


# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

source ~/.alias

export ALTERNATE_EDITOR=emacs EDITOR=emacsclient VISUAL=emacsclient

PATH=$PATH:/sbin/
export PATH

ANDROID_HOME="$HOME/Android/Sdk/"
export ANDROID_HOME

##Change the prompt
export PS1='[\w]$ '

## http://jeroenjanssens.com/2013/08/16/quickly-navigate-your-filesystem-from-the-command-line.html
export MARKPATH=$HOME/.marks
function jump {
    cd -P "$MARKPATH/$1" 2>/dev/null || echo "No such mark: $1"
}
function mark {
    mkdir -p "$MARKPATH"; ln -s "$(pwd)" "$MARKPATH/$1"
}
function unmark {
    rm -i "$MARKPATH/$1"
}
function marks {
    ls -l "$MARKPATH" | sed 's/ / /g' | cut -d' ' -f9- | sed 's/ -/\t-/g' && echo
}

_completemarks() {
    local curw=${COMP_WORDS[COMP_CWORD]}
    local wordlist=$(find $MARKPATH -type l -printf "%f\n")
    COMPREPLY=($(compgen -W '${wordlist[@]}' -- "$curw"))
    return 0
}

complete -F _completemarks jump unmark

## For going to backward directory. (bd home when your pwd is /home/sibi/Documents/java)
function bd () {
    OLDPWD=`pwd`
    NEWPWD=`echo $OLDPWD | sed 's|\(.*/'$1'[^/]*/\).*|\1|'`
    index=`echo $NEWPWD | awk '{ print index($1,"/'$1'"); }'`
    if [ $index -eq 0 ] ; then
	echo "No such occurrence."
    else
	echo $NEWPWD
	cd "$NEWPWD"
    fi
}

_completebd() {
    local curw=${COMP_WORDS[COMP_CWORD]}
    local wordlist=$(pwd | sed 's/\//\n/g')
    COMPREPLY=($(compgen -W '${wordlist[@]}' -- "$curw"))
    return 0
}

complete -F _completebd bd

## Similar to bd, but the argument is a number
function up () {
    if [[ $# -eq 1 && "$1" -gt 0 ]] ; then
	local i d
        for (( i = 0; i < $1; i++ )) ; do d="../$d" ; done
	cd $d
    else
	echo "Usage: up N"
    fi
}

# mkdir, cd into it
function mkcd () {
    mkdir -p "$*"
    cd "$*"
}

#create symlinks to executables in ~/.local/bin/

function mkx () {
    if [[ $# -gt 2 ]] ; then
	echo "Usage: mkx Target Name <Symlink Name>"
    else
	if [[ $# -eq 1 ]] ; then
	    ln -s $(pwd)/$1 ~/.local/bin/
	else
	    if [[ $# -eq 2 ]] ; then
		ln -s $(pwd)/$1 ~/.local/bin/$2
	    fi
	fi
    fi
}

function extract () {
    if [ -f "$1" ] ; then
	case $1 in
	    *.tar.bz2) tar xvjf "$1" ;;
	    *.tar.gz) tar xvzf "$1" ;;
	    *.bz2) bunzip2 "$1" ;;
	    *.rar) unrar x "$1" ;;
	    *.gz) gunzip "$1" ;;
	    *.tar) tar xvf "$1" ;;
	    *.tbz2) tar xvjf "$1" ;;
	    *.tgz) tar xvzf "$1" ;;
	    *.zip) unzip "$1" ;;
	    *.Z) uncompress "$1" ;;
	    *.7z) 7z x "$1" ;;
	    *) echo "don't know how to extract '$1'..." ;;
	esac
    else
	echo "'$1' is not a valid file"
    fi
}

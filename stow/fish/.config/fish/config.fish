set fish_greeting

# variables
# set language to english but give german dates/measurements/currency 
set -x LANG en_US.UTF-8
set -x LC_ALL de_DE.UTF-8
set -gx GOPATH $HOME/.local/share/go
set -gx GOMODCACHE $HOME/.cache/go

fish_add_path $GOPATH/bin   

# $PATHs
test -d ~/.local/bin; and set PATH ~/.local/bin $PATH
test -d ~/bin; and set ~/bin $PATH

# set aliases via abbreviations (expand to command)
# remove latex temp files
abbr texrm 'rm -f *.aux *.toc *.blg *.bbl *.log *.out *.synctex.gz'
# show public ip
abbr pubip 'curl ifconfig.me'
# star trek federation ship ambient noise
abbr warpcorebg0 'play -n synth whitenoise band -n 100 24 band -n 300 100 gain +20'
abbr warpcorebg1 'play -n -c1 synth whitenoise lowpass -1 120 lowpass -1 120 lowpass -1 120 gain +8'

# bun
set --export BUN_INSTALL "$HOME/.bun"
set --export PATH $BUN_INSTALL/bin $PATH

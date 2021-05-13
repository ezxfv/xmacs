#!/bin/sh
curl -L https://github.com/hbin/top-programming-fonts/raw/master/install.sh | bash

if [ -f $HOME/.spacemacs ]; then
	mv $HOME/.spacemacs $HOME/.spacemacs.bak
fi

ln -s `pwd`/.spacemacs $HOME/.spacemacs


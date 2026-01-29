function ranger --description 'alias ranger=ranger --choosedir=$HOME/lastrangerdir.txt; cd (cat $HOME/lastrangerdir.txt);'
    command ranger --choosedir=$HOME/lastrangerdir.txt; cd (cat $HOME/lastrangerdir.txt);
end

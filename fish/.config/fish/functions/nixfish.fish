function nixfish --wraps='nix develop -c fish' --description 'alias nixfish=nix develop -c fish'
    nix develop -c fish $argv
end

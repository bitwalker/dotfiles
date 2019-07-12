if test -f ~/.nix_profile/etc/profile.d/nix.fish
    source ~/.nix_profile/etc/profile.d/nix.fish
else if test -f ~/.nix_profile/etc/profile.d/nix.sh
    bass source ~/.nix_profile/etc/profile.d/nix.sh
end

# Dotfiles and Configs

Dotfiles managed via [stow](https://www.gnu.org/software/stow/).

Pull the repository, and then create the symbolic links via stow.


## Link Dotfiles via Stow

Link all:

```
cd stow/
./setup.sh
```

Link a single folder:

```
cd stow/
stow -v --target=$HOME insert-folder/
```

## Additional config

### Syncthing

Start syncthing via systemd

```
systemctl --user enable syncthing.service
systemctl --user start syncthing.service
```

### Trackball config

Hwdb remapping configs for Kensington Expert Trackball and Elecom Huge Trackball are in `etc/`.

After stowing, run:

```
install-trackball-config
```

Reboot to apply changes.

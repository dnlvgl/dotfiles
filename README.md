# Dotfiles and Configs

Dotfiles managed via [stow](https://www.gnu.org/software/stow/).

Pull the repository, and then create the symbolic links via stow.


## Ansible

Setup a Fedora Worksation via Ansible

Installs all packeges in `ansible/packages.yml` expects a standard Gnome Desktop.

```
cd ansible/
./setup.sh
```


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

TODO: add script in etc/

save `90-kensington-expert-trackball-remap.hwdb` under `/etc/udev/hwdb.d/90-kensington-expert-trackball-remap.hwdb`

run `sudo systemd-hwdb update` and reboot
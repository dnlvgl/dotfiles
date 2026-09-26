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

After stowing, enable and start the user service:

```
systemctl --user enable --now syncthing.service
```

### OCR scripts

`ocr-screen` (in `stow/scripts`) OCRs a screen region into the clipboard: drag a rectangle, release to capture. Run it from rofi's run mode. It reads German and English by default; pass a tesseract language (`ocr-screen eng`) to force one.

Dependencies (Fedora):

```
sudo dnf install grim slurp tesseract tesseract-langpack-eng tesseract-langpack-deu
```

### Power menu

`power-menu` (in `stow/scripts`) is a rofi menu for lock, suspend, logout, reboot and shutdown. Run it from rofi's run mode. Logout, reboot and shutdown ask for confirmation.

### Trackball config

Hwdb remapping configs for Kensington Expert Trackball and Elecom Huge Trackball are in `etc/`.

After stowing, run:

```
install-trackball-config
```

Reboot to apply changes.


## TODO: Ansible playbook

Set up an Ansible playbook that installs everything the niri setup needs on a fresh Fedora machine, stows the dotfiles and enables the services. Package names are Fedora 44 as installed today.

### Repositories

- COPR `scottames/ghostty` for `ghostty`. Everything else is in the stock Fedora repos.

### Packages

Session and desktop (started or bound from `stow/niri/.config/niri/config.kdl`):

| Package | Used for |
| --- | --- |
| `niri` | compositor |
| `waybar` | status bar |
| `SwayNotificationCenter` | notifications (`swaync`, `swaync-client`) |
| `swayidle` | idle lock, lock before suspend |
| `swaylock` | lock screen |
| `swaybg` | wallpaper |
| `rofi` | launcher, run mode for the scripts |
| `ghostty` | terminal (`Mod+T`) |
| `qt6ct` | Qt theming (`QT_QPA_PLATFORMTHEME` in the niri config) |

Key bindings and waybar modules:

| Package | Used for |
| --- | --- |
| `wireplumber` (`wpctl`), `pipewire` | volume and mic mute keys, waybar volume module |
| `playerctl` | media keys |
| `brightnessctl` | brightness keys |
| `orca` | screen reader toggle (`Super+Alt+S`) |
| `pavucontrol` | waybar volume, right click |
| `nm-connection-editor` | waybar network, click |
| `blueman` | waybar bluetooth, click |
| `syncthing` | sync, waybar status module and the user service in `stow/syncthing` |
| `curl`, `jq` | `syncthing-status.sh` |
| `xdg-utils` | `xdg-open` for the syncthing web UI |

Scripts in `stow/scripts` and `stow/waybar/.config/waybar/scripts`:

| Package | Used for |
| --- | --- |
| `grim`, `slurp` | `ocr-screen` capture and selection |
| `tesseract`, `tesseract-langpack-eng`, `tesseract-langpack-deu` | `ocr-screen` text recognition |
| `wl-clipboard` | `wl-copy` for `ocr-screen`, also the tmux copy command |
| `libnotify` | `notify-send` for `ocr-screen` |
| `glib2` | `gsettings` in the theme toggle |
| `adw-gtk3-theme` | GTK3 light and dark theme switched by the theme toggle |
| `systemd` | `systemctl` and `loginctl` for `power-menu` and the idle lock |

Theming and fonts:

| Package | Used for |
| --- | --- |
| `fontawesome-6-free-fonts` | waybar icons |
| `adwaita-icon-theme` | rofi icon theme |
| `google-noto-sans-vf-fonts` | Fedora's default `sans-serif`, used by rofi, swaylock and swaync |

Installed on the current machine but not referenced by any file in this repo (niri relies on them for X11 apps, screen sharing and secrets, so verify before dropping them):

`xwayland-satellite`, `xdg-desktop-portal`, `xdg-desktop-portal-gnome`, `xdg-desktop-portal-gtk`, `gnome-keyring`

Tooling for the dotfiles themselves: `stow`.

### Playbook tasks

- [ ] Enable the `scottames/ghostty` COPR
- [ ] Install the packages above with `ansible.builtin.dnf`
- [ ] Clone this repository and run `stow/setup.sh`
- [ ] Enable the Syncthing user service (`systemctl --user enable --now syncthing.service`)
- [ ] Run `install-trackball-config` (needs sudo, copies hwdb files to `/etc/udev/hwdb.d/`)
- [ ] Create `~/.gitconfig.local` from `stow/git/.gitconfig.local.template`
- [ ] Check whether the fish, emacs and tmux tooling (fisher, nvm, vterm build dependencies and so on) should be part of the same playbook

# Dotfiles and Configs

Workstation setup for Fedora with niri. Ansible installs the packages, stows the dotfiles from `stow/` with [stow](https://www.gnu.org/software/stow/), copies system files from `system/` and enables services.

## Setup

On a fresh machine run:

```
sudo dnf install -y git
git clone https://github.com/dnlvgl/dotfiles.git ~/Projects/code/dotfiles
~/Projects/code/dotfiles/bootstrap.sh workstation
```

The same script updates an existing machine. It installs missing packages, stows packages that aren't linked yet and copies changed system files. If a real file is in the way of a stow link, the run stops and lists the conflicts; nothing is overwritten.

The first argument is the profile, a playbook in `ansible/playbooks/` (default `workstation`). Extra arguments go to `ansible-playbook`:

```
./bootstrap.sh workstation --check --diff   # dry run
./bootstrap.sh workstation --tags dotfiles  # only stow
./bootstrap.sh workstation --tags fisher    # update fish plugins (never runs by default)
```

Tags: `packages`, `dotfiles`, `system`, `shell`, `services`.

### Layout

| Path | Content |
| --- | --- |
| `stow/` | one folder per stow package, linked into `$HOME` |
| `system/` | files copied as root, mirroring their destination (`system/etc/udev/hwdb.d/x.hwdb` goes to `/etc/udev/hwdb.d/x.hwdb`) |
| `ansible/inventory/group_vars/workstations.yml` | packages, COPRs, user services, what each package is used for |
| `ansible/playbooks/` | profiles, e.g. `workstation.yml` |
| `ansible/machines/<hostname>.yml` | optional per-machine extras, e.g. `host_packages` |
| `ansible/roles/` | `packages`, `dotfiles`, `system_files`, `shell`, `services` |

New folders in `stow/` and new files in `system/` are picked up automatically. If a system file needs a reload after changing, add a task next to `Update hwdb` in `ansible/roles/system_files/tasks/main.yml`.

Workstation profiles always configure the machine they run on (`localhost`), so a new machine needs no inventory entry. Homelab servers can be added later as inventory groups with real hostnames and their own profile, e.g. `playbooks/homelab.yml` with `hosts: homelab`.

## Manual stow

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

### OCR scripts

`ocr-screen` (in `stow/scripts`) OCRs a screen region into the clipboard: drag a rectangle, release to capture. Run it from rofi's run mode. It reads German and English by default; pass a tesseract language (`ocr-screen eng`) to force one.

Dependencies (Fedora):

```
sudo dnf install grim slurp tesseract tesseract-langpack-eng tesseract-langpack-deu
```

### Power menu

`power-menu` (in `stow/scripts`) is a rofi menu for lock, suspend, logout, reboot and shutdown. Run it from rofi's run mode. Logout, reboot and shutdown ask for confirmation.

### Trackball config

Hwdb remapping configs for Kensington Expert Trackball and Elecom Huge Trackball are in `system/etc/udev/hwdb.d/`. Ansible copies them and runs `systemd-hwdb update`. Reboot to apply changes.

### Git

Machine-specific git settings go in `~/.gitconfig.local`, which is not tracked. Create it from the template:

```
cp ~/.gitconfig.local.template ~/.gitconfig.local
```

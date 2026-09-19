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

### Trackball config

Hwdb remapping configs for Kensington Expert Trackball and Elecom Huge Trackball are in `etc/`.

After stowing, run:

```
install-trackball-config
```

Reboot to apply changes.

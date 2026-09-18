#!/bin/sh
set -e

waybar_dir="$HOME/.config/waybar"
fuzzel_dir="$HOME/.config/fuzzel"

case "$(readlink "$waybar_dir/colors.css")" in
    *light*) new=dark ;;
    *) new=light ;;
esac

ln -sf "colors-$new.css" "$waybar_dir/colors.css"
ln -sf "colors-$new.ini" "$fuzzel_dir/colors.ini"
ln -sf "colors-$new.css" "$HOME/.config/swaync/colors.css"
ln -sf "colors-$new.css" "$HOME/.config/gtklock/colors.css"

# swaync doesn't watch its stylesheet; it's a no-op if the daemon isn't running.
swaync-client -rs >/dev/null 2>&1 || true

gsettings set org.gnome.desktop.interface color-scheme "prefer-$new"

# GTK3 ignores color-scheme and follows the theme name instead.
case "$new" in
    light) gtk3_theme=adw-gtk3 ;;
    *) gtk3_theme=adw-gtk3-dark ;;
esac
gsettings set org.gnome.desktop.interface gtk-theme "$gtk3_theme"

#!/bin/bash
# Set up or update this workstation with Ansible.
# Extra arguments go to ansible-playbook, e.g. ./bootstrap.sh --tags dotfiles
set -euo pipefail

REPO="https://github.com/dnlvgl/dotfiles.git"
DOTFILES_DIR="$HOME/Projects/code/dotfiles"

if ! command -v ansible-playbook >/dev/null || ! command -v git >/dev/null; then
    sudo dnf install -y ansible git
fi

if [[ ! -d "$DOTFILES_DIR" ]]; then
    git clone "$REPO" "$DOTFILES_DIR"
fi

cd "$DOTFILES_DIR/ansible"
ansible-galaxy collection install -r requirements.yml >/dev/null
ansible-playbook site.yml --ask-become-pass --limit "$(hostname -s)" "$@"

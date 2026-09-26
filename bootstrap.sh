#!/bin/bash
# Set up or update this machine with an Ansible profile (a playbook in ansible/playbooks).
# Usage: ./bootstrap.sh [profile] [ansible-playbook args], e.g. ./bootstrap.sh workstation --tags dotfiles
set -euo pipefail

REPO="https://github.com/dnlvgl/dotfiles.git"
DOTFILES_DIR="$HOME/Projects/code/dotfiles"

PROFILE="workstation"
if [[ $# -gt 0 && $1 != -* ]]; then
    PROFILE="$1"
    shift
fi

if ! command -v ansible-playbook >/dev/null || ! command -v git >/dev/null; then
    sudo dnf install -y ansible git
fi

if [[ ! -d "$DOTFILES_DIR" ]]; then
    git clone "$REPO" "$DOTFILES_DIR"
fi

cd "$DOTFILES_DIR/ansible"
PLAYBOOK="playbooks/$PROFILE.yml"
if [[ ! -f "$PLAYBOOK" ]]; then
    echo "Unknown profile: $PROFILE"
    echo "Available: $(ls playbooks | sed 's/\.yml$//' | tr '\n' ' ')"
    exit 1
fi

ansible-galaxy collection install -r requirements.yml >/dev/null
ansible-playbook "$PLAYBOOK" --ask-become-pass "$@"

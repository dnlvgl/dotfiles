#!/bin/bash
# Set up or update this machine with an Ansible profile (a playbook in ansible/playbooks).
# Usage: ./bootstrap.sh [profile] [ansible-playbook args], e.g. ./bootstrap.sh workstation --tags dotfiles
set -euo pipefail

PROFILE="workstation"
if [[ $# -gt 0 && $1 != -* ]]; then
    PROFILE="$1"
    shift
fi

if ! command -v ansible-playbook >/dev/null; then
    sudo dnf install -y ansible
fi

cd "$(dirname "${BASH_SOURCE[0]}")/ansible"
PLAYBOOK="playbooks/$PROFILE.yml"
if [[ ! -f "$PLAYBOOK" ]]; then
    echo "Unknown profile: $PROFILE"
    echo "Available: $(ls playbooks | sed 's/\.yml$//' | tr '\n' ' ')"
    exit 1
fi

ansible-galaxy collection install -r requirements.yml >/dev/null
ansible-playbook "$PLAYBOOK" --ask-become-pass "$@"

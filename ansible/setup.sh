# Script to install ansible and then run playbook
# run via wget
# bash wget -qO- https://urltorepo/run.sh | bash

echo -e "*\n* install ansible via dnf\n* enter password\n*"
sudo dnf install ansible

echo -e "*\n* run packages playbook\n* enter password\n*"
ansible-playbook -K packages.yml

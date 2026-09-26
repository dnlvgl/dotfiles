Optional per-machine vars, loaded by workstation profiles from `<hostname>.yml`
(`hostname -s`). They override `inventory/group_vars`, e.g.

```yaml
host_packages:
  - steam
```

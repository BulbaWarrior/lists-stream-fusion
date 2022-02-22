# stream-fusion

## How to profile

```sh
stack build && stack exec -- stream-fusion-exe +RTS -h && hp2ps -i+ -M -m200 -c stream-fusion-exe.hp && open stream-fusion-exe.ps
```

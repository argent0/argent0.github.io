---
title: Android mic
---

1. Start murmur:
```
$ murmurd &
```

2. launch mumble:
```
mumble mumble://localhost
```

3. Set a pulse audio monitor

```
pactl load-module module-null-sink sink_name=my-input-monitor
pactl load-module module-null-sink sink_name=my-output-monitor
```

4. set the mumble output to the new monitor in pavucontrol
5. set the new monitor as the default input
```
pacmd "set-default-source my-output-monitor.monitor"
```
6. connect your phone to the murmur instance and use its mic.

7. use a sox filter

```
sox -d -d highpass 160
```

```
set -e
murmurd &
sleep 1
mumble mumble://localhost &
sleep 5
pactl load-module module-null-sink sink_name=my-input-monitor
pacmd "set-default-source my-input-monitor.monitor"
pavucontrol &

# use
# pacmd unload-module module-null-sink
# to remove the sink
```

#!/usr/bin/env bash
set -euo pipefail

state=$(gdbus call --system --dest org.bluez --object-path /org/bluez/hci0 --method org.freedesktop.DBus.Properties.Get "org.bluez.Adapter1" "Powered")
case ${state} in
    "(<false>,)")
	new="<true>"
	;;
    "(<true>,)")
	new="<false>"
	;;
    *)
	exit 1
esac
gdbus call --system --dest org.bluez --object-path /org/bluez/hci0 --method org.freedesktop.DBus.Properties.Set "org.bluez.Adapter1" "Powered" $new >/dev/null

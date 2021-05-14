#!/usr/bin/python3

import psutil
import subprocess
import sys

interval = 2

def get_lines():
    global interval

    line1 = f"CPU: {psutil.cpu_percent(interval):.0f}% "
    line1 += f"MEM: {psutil.virtual_memory().percent:.0f}% "

    line2 = f"DSK: {psutil.disk_usage('/').percent:.0f}%"

    battery = psutil.sensors_battery()
    line2 = f"BAT: {battery.percent:.0f}%"
    line2 += " (C)" if battery.power_plugged else ""

    nmcli_cmd = ["nmcli", "c", "show", "--active"]
    line3 = "VPN Status: "
    if ("vpn0" in subprocess.run(nmcli_cmd, stdout=subprocess.PIPE).stdout.decode('utf-8')):
        line3 += "ON"
    else:
        line3 += "OFF"

    return ("-1", line1, "-2", line2, "-3", line3)

while True:
    keyb_update = ["apex7tkl", "text"]
    keyb_update.extend(get_lines())
    try:
        subprocess.check_call(keyb_update)
    except:
        print("No keyb, so dying...")
        sys.exit(1)

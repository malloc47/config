#!/bin/bash
while(true) ; do if iwconfig 2>/dev/null | grep "Access Point: Not-Associated" ; then sudo iwconfig wlan0 essid "$1" ; fi ; sleep 5 ; done

#!/bin/bash
iw wlp3s0 info | grep ssid | awk '{print $2}'

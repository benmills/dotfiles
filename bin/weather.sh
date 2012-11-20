#!/bin/bash
# Prints the current weather in Celsius, Fahrenheits or lord Kelvins. The forecast is cached and updated with a period of $update_period.

# You location. Find a string that works for you by Googling on "weather in <location-string>"
location="chicago"

# Can be any of {c,f,k}.
unit="f"

tmp_file="/tmp/tmux-powerline_weather.txt"

read_tmp_file() {
	if [ ! -f "$tmp_file" ]; then
		return
	fi
	IFS_bak="$IFS"
	IFS=$'\n'
	lines=($(cat ${tmp_file}))
	IFS="$IFS_bak"
	degrees="${lines[0]}"
	conditions="${lines[1]}"
}

degrees=""
if [ -f "$tmp_file" ]; then
	if [ "$PLATFORM" == "mac" ]; then
		last_update=$(stat -f "%m" ${tmp_file})
	else
		last_update=$(stat -c "%Y" ${tmp_file})
	fi
	time_now=$(date +%s)
	update_period=600

	up_to_date=$(echo "(${time_now}-${last_update}) < ${update_period}" | bc)
	if [ "$up_to_date" -eq 1 ]; then
		read_tmp_file
	fi
fi

if [ -z "$degrees" ]; then
	if [ "$unit" == "k" ]; then
		search_unit="c"
	else
		search_unit="$unit"
	fi
	if [ "$PLATFORM" == "mac" ]; then
		search_location=$(echo "$location" | sed -e 's/[ ]/%20/g')
	else
		search_location=$(echo "$location" | sed -e 's/\s/%20/g')
	fi

	weather_data=$(curl --max-time 2 -s "http://www.google.com/ig/api?weather=${search_location}")
	if [ "$?" -eq "0" ]; then
		error=$(echo "$weather_data" | grep "problem_cause\|DOCTYPE");
		if [ -n "$error" ]; then
			echo "error"
			exit 1
		fi
		degrees=$(echo "$weather_data" | sed "s|.*<temp_${search_unit} data=\"\([^\"]*\)\"/>.*|\1|")
		conditions=$(echo "$weather_data" | grep -PZo "<current_conditions>(\\n|.)*</current_conditions>" | grep -PZo "(?<=<condition\sdata=\")([^\"]*)")
		echo "$degrees" > $tmp_file
		echo "$conditions" >> $tmp_file
	elif [ -f "$tmp_file" ]; then
		read_tmp_file
	fi
fi

if [ -n "$degrees" ]; then
	if [ "$unit" == "k" ]; then
		degrees=$(echo "${degrees} + 273.15" | bc)
	fi
	unit_upper=$(echo "$unit" | tr '[cfk]' '[CFK]')
	echo "${degrees} ${unit_upper}"
fi

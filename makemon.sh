#!/bin/sh
while inotifywait -r -q -e modify src; do
	clear
	make
	if [ $? -eq 0 ]; then
		clear
		./fileTest.sh
		#firefox result/file.c.html
	fi
done

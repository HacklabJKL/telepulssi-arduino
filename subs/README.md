# Subtitles

This directory contains subtitles for Assembly 2017 compo.

To combine them to Matroska file, I have used the following one-liner:

	mkvmerge -o TARGET.mkv SOURCE.mkv --language 0:eng --track-name 0:English telepulssi-en.ass --language 0:fin --track-name 0:Finnish telepulssi-fi.ass

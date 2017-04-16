# Helper functions for text rendering

# Add strips of 3 pixels after every LED segment so that every LED
# segment line represents exactly one byte in the resulting data.
byte_padding_magick="-background white -gravity west 
		     -splice 3x0+40+0 -splice 3x0+35+0 -splice 3x0+30+0
		     -splice 3x0+25+0 -splice 3x0+20+0 -splice 3x0+15+0
		     -splice 3x0+10+0 -splice 3x0+5+0"

# Render using ImageMagick and pango to a frame of 40x7 pixels using
# given threshold (darkness) in the font. Defaults to 50% which is
# quite OK.  Outputs raw binary bitmap.
render_text() {
    convert -gravity center +antialias +dither \
	    -font Ubuntu -pointsize 6 -define pango:markup=false \
	    "pango:$1" \
	    -gravity north -crop '40x7+1+1!' -threshold ${2-50}% \
	    $byte_padding_magick mono:-
}

send_to_screen() {
    # Set serial port parameters if outputting to a char device
    test -c "$1" && stty -F "$1" raw 115200

    {
	# Synchronize screen
	echo -n S
	# Passthrough
	cat
    } >"$1"
}
